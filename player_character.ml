open Chara_status.Open
open Phi_map.Open

type list_select_type = Get | Use | Unequip | Spell

type command_st = 
    Normal
  | List_select of list_select_type
  | Cast of (Spell.t * int)

type t = 
    <get_name : string;
  get_status_view : Chara_status.view;
  get_item_list : (Item.t * Chara.equip_flag option) list;
  get_chara_id : Chara_id.t;
  get_spell_list : Spell.t list;
  sight_change : Chara.sight_change_type -> Event.t list;
  sight_update : Event.t list;
  turn : dir:Phi_map.direction -> Event.t list;
  go : dir:Phi_map.direction -> Event.t list;
  do_action : Event.t list;
  hit : Event.t list;
  defense : combat:Combat.t -> achid:Chara_id.t -> Event.t list;
  resolve_attack_result : result_list:Combat.result list -> dchid:Chara_id.t -> Event.t list;
     (* throw an exception if there is no such item *)
  item_get : item:Item.t -> Event.t list;
  dead : Event.t list;
  say : msg:string -> Event.t list;
  listen : msg:string -> achid:Chara_id.t -> Event.t list;
  use_item : item:Item.t -> Event.t list;
  unequip_item : item:Item.t -> Event.t list;
  cast : spell:Spell.t -> Event.t list;

  set_command_st : st:command_st -> unit;
  get_command_st : command_st;
  get_phirc : string>


let ($) f g x = f (g x);;

let client_map_center = (3, 4);;


let create ~phirc ~cid ~chid = 
  match Player_character_db.load ~phirc with
      None -> None
    | Some (name, pos, adir, status, item_list, spell_list) ->

  let chara = object (self)
    val cid = cid
    val phirc = phirc
    val chid = chid
    val mutable status =
      Chara_status.create ~view:status
    val mutable item_list = List.map (fun item -> (item, None)) item_list
    val mutable command_st = Normal
    val mutable spell_list = spell_list

    method get_name = name
    method get_phirc = phirc
    method get_chara_id = chid
    method get_spell_list = spell_list

    method sight_change = function
        Chara.Appear_chara (_, _) ->
        ignore (self#sight_update);
          []
      | Chara.Move_chara (_, (_, _)) ->
        ignore (self#sight_update);
        []
      | Chara.Disappear_chara (_, _) ->
        ignore (self#sight_update);
        []

    method cast ~spell =
      command_st <- Cast (spell, (Spell.get_view ~spell).Spell.cast_time);
      self#send_message (Dm_message.make (Dm_message.Cast_now self#get_name));
      []

    method turn ~dir =
      let adir =
        (match dir with
            Phi_map_data.Absolute_direction adir -> adir
          | Phi_map_data.Relative_direction rdir ->
            let adir = Phi_map.get_chara_absolute_direction ~chara_id:chid in
            Phi_map.turn_absolute_direction ~adir ~rdir
        )
      in
      Phi_map.set_chara_direction ~chara_id:chid ~adir;
      ignore (self#sight_update);
      []

    method go ~dir =
      let adir =
        match dir with
            Phi_map_data.Absolute_direction adir -> adir
          | Phi_map_data.Relative_direction rdir ->
            let adir = Phi_map.get_chara_absolute_direction ~chara_id:chid in
            Phi_map.turn_absolute_direction ~adir ~rdir
      in
      let pos = Phi_map.get_chara_position ~chara_id:chid in
      match Phi_map.get_neighbor_position ~pos ~adir with
          None -> 
            self#send_message (Dm_message.make Dm_message.Go_no);
            []
        | Some next_pos ->
          if Phi_map.is_enterable ~pos:next_pos
          then 
            (let old_pos = Phi_map.get_chara_position ~chara_id:chid in
            let event = Event.Position_change (chid, (Some old_pos, Some next_pos)) in
            Phi_map.set_chara_position ~chara_id:chid ~pos:next_pos;
            ignore (self#sight_update);
            [event])
          else
            (self#send_message (Dm_message.make Dm_message.Go_no);
            [])


    method do_action =
      match command_st with
          Cast (spell, count) ->
            if count > 0
            then (command_st <- Cast (spell, count-1); [])
            else (self#complete_spell spell)
        | List_select _ | Normal -> []

    method private complete_spell spell =
      let spell_name = Spell.get_name ~spell in
      self#send_message (name ^ " > " ^ spell_name);
      command_st <- Normal;
      let pos = Phi_map.get_chara_position ~chara_id:chid in
      let adir = Phi_map.get_chara_absolute_direction ~chara_id:chid in
      match Phi_map.get_neighbor_position ~pos ~adir with
          None -> []
        | Some front_pos -> 
          (match Phi_map.get_neighbor_position ~pos:front_pos ~adir with
              None ->
              [Event.Attack_to (chid, (front_pos,
                                       Combat.create ~attack_status:status ~attack_name:spell_name))]
            | Some next_pos ->
              [Event.Attack_to (chid, (front_pos,
                                       Combat.create ~attack_status:status ~attack_name:spell_name));
              Event.Attack_to (chid, (next_pos,
                                       Combat.create ~attack_status:status ~attack_name:spell_name))])

    method listen ~msg ~achid =
      let name =
        match Chara_name_cache.get_name ~chid:achid with
            None -> "????"
          | Some n -> n
      in
      self#send_message (name ^ " > " ^ msg);
      []

    method say ~msg =      
      [Event.Say (chid, msg)]

    method use_item ~item =
      let view = Item.get_view ~item in
      (match view.Item.item_type with
          Item.Weapon _ ->
            item_list <-
              List.map 
              (fun (i, flag) ->
                if i == item
                then (i, Some Chara.Wpn)
                else
                  (if flag = Some Chara.Wpn
                   then (i, None)
                   else (i, flag)))
              item_list;
            self#send_message
              (Dm_message.make (Dm_message.Use(self#get_name, Item.get_name ~item)));
            self#send_message
             (Dm_message.make(Dm_message.Equip(self#get_name, Item.get_name ~item)));
            []
        | Item.Food power ->
          self#send_message
            (Dm_message.make (Dm_message.Eatfood(self#get_name, Item.get_name ~item, power)));
          let cure_point =
            min power ((Chara_status.get_view ~status).mhp - (Chara_status.get_view ~status).hp)
          in
          status <- Chara_status.add_hp ~hp:cure_point ~status;
          []
      )

    method unequip_item ~item =
      item_list <-
        List.map 
        (fun (i, flag) ->
          if i == item
          then (i, None)
          else (i, flag))
        item_list;
      self#send_message
        (Dm_message.make(Dm_message.Unequip(self#get_name, Item.get_name ~item)));
      []

    method hit =
      let pos = Phi_map.get_chara_position ~chara_id:chid in
      let adir = Phi_map.get_chara_absolute_direction ~chara_id:chid in
      match Phi_map.get_neighbor_position ~pos ~adir with
          None -> []
        | Some front_pos -> [Event.Attack_to (chid, (front_pos, Combat.create ~attack_status:status ~attack_name:"knuckle"))]

    method private send_attack_messages aname dname result_list =
      let result_to_message = function
          Combat.Hp_damage (value, attack_name) ->
          Dm_message.make (Dm_message.Attack_hp (aname, dname, attack_name, value))
        | Combat.Kill ->
          Dm_message.make (Dm_message.Kill_by (aname, dname))
      in
      ignore (List.map (self#send_message $ result_to_message) result_list);      

    method defense ~combat ~achid =
      let (new_status, result_list) = combat status in
      status <- new_status;
      let dname = self#get_name in
      let aname =
        match Chara_name_cache.get_name ~chid:achid with
            None -> "????"
          | Some n -> n
      in
      self#send_attack_messages aname dname result_list;
      if Chara_status.is_dead ~status
      then [Event.Attack_result ((achid, chid), result_list); Event.Dead chid]
      else [Event.Attack_result ((achid, chid), result_list)]

    method dead =
      self#send_message (Dm_message.make Dm_message.Dead);
      self#send_message (Dm_message.make Dm_message.Try_again);
      self#send_message (Dm_message.make Dm_message.Savedata);
      self#send_message " "; (*dummy*)
      let status_view = Chara_status.get_view ~status in
      status <- Chara_status.add_hp ~status ~hp:(1 - status_view.hp);
      []

    method resolve_attack_result ~result_list ~dchid =
      let aname = self#get_name in
      let dname =
        match Chara_name_cache.get_name ~chid:dchid with
            None -> "????"
          | Some n -> n
      in
      self#send_attack_messages aname dname result_list;
      []

    method item_get ~item =
      self#send_message (Dm_message.make (Dm_message.Get (self#get_name, (Item.get_name ~item))));
      item_list <- (item, None) :: item_list;
      Phi_map.delete_item ~item ~pos:(Phi_map.get_chara_position ~chara_id:chid);
      []

    method get_status_view =
      Chara_status.get_view ~status

    method get_item_list =
      item_list

    method set_command_st ~st =
      command_st <- st

    method get_command_st =
      command_st
    
    method private send_message msg = Client_manager.send_message ~cid ~msg
    method sight_update =
      self#send_message (Protocol.encode_server_protocol (Protocol.M57_map (Phi_map.get_mapview ~chara_id:chid)));
      let chara_id_list = Phi_map.get_chara_in_sight_list ~chara_id:chid in
      let chara_name_list =
        List.map
          (fun (id, vp, rdir) -> (
            (match (Chara_name_cache.get_name ~chid:id) with None -> "????" | Some name -> name),
            vp,
            rdir)
          )
          chara_id_list
      in
      ignore (List.map
        (fun (name, vp, rdir) ->
          self#send_message
            (Protocol.encode_server_protocol
               (Protocol.M57_obj 
                  (Protocol.C_obj,
                   ((fst client_map_center) + vp.Phi_map_data.x),
                   ((snd client_map_center) + vp.Phi_map_data.y),
                   rdir,
                   name))
            )
        )
        chara_name_list);
      self#send_message (Protocol.encode_server_protocol Protocol.M57_end);
      []
  end in
  Phi_map.set_chara_position ~chara_id:chid ~pos;
  Phi_map.set_chara_direction ~chara_id:chid ~adir;
  Chara_name_cache.set_name ~chid ~name:chara#get_name;
  Some chara
;;
