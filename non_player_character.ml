open Chara_status.Open


type t = 
    <get_name : string;
  get_status_view : Chara_status.view;
  get_item_list : Item.t list;
  get_chara_id : Chara_id.t;
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
  dead : Event.t list>


let adirs = [|Phi_map.North; Phi_map.East; Phi_map.West; Phi_map.South|];;

let create ~chid =
  let chara = object (self)
    val chid = chid
    val mutable status =
      Chara_status.create ~view:{hp = 100; mhp = 1200; mp = 100; mmp = 500;
                                 flv = 2000; wlv = 1; mlv = 2; clv = 3;
                                 state = Chara_status.Command; condition = []}
    val mutable item_list = []

    method get_name = "npc " ^ (string_of_int (Chara_id.to_num ~id:chid))
    method sight_change _ = []
    method get_chara_id = chid

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
      []

    method go ~dir =
      let adir =
        match dir with
            Phi_map.Absolute_direction adir -> adir
          | Phi_map.Relative_direction rdir ->
            let adir =Phi_map.get_chara_absolute_direction ~chara_id:chid in
            Phi_map.turn_absolute_direction ~adir ~rdir
      in
      let pos = Phi_map.get_chara_position ~chara_id:chid in
      match Phi_map.get_neighbor_position ~pos ~adir with
          None -> 
            []
        | Some next_pos ->
          let old_pos = Phi_map.get_chara_position ~chara_id:chid in
          let event = Event.Position_change (chid, (Some old_pos, Some next_pos)) in
          Phi_map.set_chara_position ~chara_id:chid ~pos:next_pos;
          [event]

    method do_action =
      if Random.int 10 < 9
      then []
      else
      if Random.int 10 < 5
      then
          let random_adir = adirs.(Random.int 4) in
          self#go ~dir:(Phi_map.Absolute_direction random_adir)
      else
          self#hit

    method hit =
      let pos = Phi_map.get_chara_position ~chara_id:chid in
      let adir = Phi_map.get_chara_absolute_direction ~chara_id:chid in
      match Phi_map.get_neighbor_position ~pos ~adir with
          None -> []
        | Some front_pos -> [Event.Attack_to (chid, (front_pos, Combat.create ~attack_status:status))]

    method defense ~combat ~achid =
      let (new_status, result_list) = combat status in
      status <- new_status;
      if Chara_status.is_dead ~status
      then [Event.Attack_result ((achid, chid), result_list); Event.Dead chid]
      else [Event.Attack_result ((achid, chid), result_list)]

    method resolve_attack_result ~result_list:_ ~dchid:_ =
      []

    method item_get ~item:_ =
      []

    method sight_update =
      []

    method dead =
      []

    method get_status_view =
      Chara_status.get_view ~status

    method get_item_list =
      item_list

  end in
  let pos = Phi_map.get_default_position in (* tentative *)
  let adir = Phi_map.North in (* tentative *)
  Phi_map.set_chara_position ~chara_id:chid ~pos;
  Phi_map.set_chara_direction ~chara_id:chid ~adir;
  Chara_name_cache.set_name ~chid ~name:chara#get_name;
  chara
;;
