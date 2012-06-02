let adirs = [|Phi_map.North; Phi_map.East; Phi_map.West; Phi_map.South|];;

let create ~chid =
  let chara = object (self)
    val chid = chid

    method get_name = "npc"
    method sight_change _ = []

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
      let random_adir = adirs.(Random.int 4) in
      self#go ~dir:(Phi_map.Absolute_direction random_adir)
  end in
  let pos = Phi_map.get_default_position in (* tentative *)
  let adir = Phi_map.North in (* tentative *)
  Phi_map.set_chara_position ~chara_id:chid ~pos;
  Phi_map.set_chara_direction ~chara_id:chid ~adir;
  chara
;;
