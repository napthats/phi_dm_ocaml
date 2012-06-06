open Chara_status.Open


let ($) f g x = f (g x);;

let client_map_center = (3, 4);;

let create ~phirc ~cid ~chid = 
  let chara = object (self)
    val cid = cid
    val phirc = phirc
    val chid = chid
    val mutable status =
      Chara_status.create ~view:{hp = 5000; mhp = 5200; mp = 5100; mmp = 5500;
                                 flv = 120; wlv = 1; mlv = 2; clv = 3;
                                 state = Chara_status.Command; condition = []}
    val mutable item_list = []

    method get_name = phirc
    method sight_change = function
        Chara.Appear_chara (chara, vpos) ->
          self#send_message
            (String.concat ""
               ["appear "; chara#get_name;
                " on "; string_of_int vpos.Phi_map.x; ":"; string_of_int vpos.Phi_map.y]);
          ignore (self#sight_update);
          []
      | Chara.Move_chara (chara, (ovpos, nvpos)) ->
        self#send_message
          (String.concat ""
             ["move "; chara#get_name;
              " on "; string_of_int ovpos.Phi_map.x; ":"; string_of_int ovpos.Phi_map.y;
              " to "; string_of_int nvpos.Phi_map.x; ":"; string_of_int nvpos.Phi_map.y]);
          ignore (self#sight_update);
        []
      | Chara.Disappear_chara (chara, vpos) ->
        self#send_message
          (String.concat ""
             ["dipappear "; chara#get_name;
              " on "; string_of_int vpos.Phi_map.x; ":"; string_of_int vpos.Phi_map.y]);
          ignore (self#sight_update);
        []

    method turn ~dir =
      let adir =
        (match dir with
            Phi_map.Absolute_direction adir -> adir
          | Phi_map.Relative_direction rdir ->
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
            Phi_map.Absolute_direction adir -> adir
          | Phi_map.Relative_direction rdir ->
            let adir = Phi_map.get_chara_absolute_direction ~chara_id:chid in
            Phi_map.turn_absolute_direction ~adir ~rdir
      in
      let pos = Phi_map.get_chara_position ~chara_id:chid in
      match Phi_map.get_neighbor_position ~pos ~adir with
          None -> 
            self#send_message (Dm_message.make Dm_message.Go_no);
            []
        | Some next_pos ->
          let old_pos = Phi_map.get_chara_position ~chara_id:chid in
          let event = Event.Position_change (chid, (Some old_pos, Some next_pos)) in
          Phi_map.set_chara_position ~chara_id:chid ~pos:next_pos;
          ignore (self#sight_update);
          [event]

    method do_action =
      []

    method hit =
      let pos = Phi_map.get_chara_position ~chara_id:chid in
      let adir = Phi_map.get_chara_absolute_direction ~chara_id:chid in
      match Phi_map.get_neighbor_position ~pos ~adir with
          None -> []
        | Some front_pos -> [Event.Attack_to (chid, (front_pos, Combat.create ~attack_status:status))]

    method defense ~combat:_ ~achid:_ =
      []

    method resolve_attack_result ~result_list ~dchid =
      let aname = self#get_name in
      let dname =
        match Chara_name_cache.get_name ~chid:dchid with
            None -> "????"
          | Some n -> n
      in
      let result_to_message = function
          Combat.Hp_damage value ->
          Dm_message.make (Dm_message.Attack_hp (aname, dname, "knuckle", value))
        | Combat.Kill ->
          Dm_message.make (Dm_message.Kill_by (aname, dname))
      in
      ignore (List.map (self#send_message $ result_to_message) result_list);
      []

    method get_item ~item =
      self#send_message (Dm_message.make (Dm_message.Get (self#get_name, (Item.get_name ~item))));
      item_list <- item :: item_list;
      Phi_map.delete_item ~item ~pos:(Phi_map.get_chara_position ~chara_id:chid);
      []

    method get_status_view =
      Chara_status.get_view ~status

    method get_item_list =
      item_list
    
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
                   ((fst client_map_center) + vp.Phi_map.x),
                   ((snd client_map_center) + vp.Phi_map.y),
                   rdir,
                   name))
            )
        )
        chara_name_list);
      self#send_message (Protocol.encode_server_protocol Protocol.M57_end);
      []
  end in
  let pos = Phi_map.get_default_position in (* tentative *)
  let adir = Phi_map.North in (* tentative *)
  Phi_map.set_chara_position ~chara_id:chid ~pos;
  Phi_map.set_chara_direction ~chara_id:chid ~adir;
  chara
;;
