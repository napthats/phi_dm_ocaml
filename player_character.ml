let create ~phirc ~cid ~chid : Chara.chara = 
  let chara = object (self)
    val cid = cid
    val phirc = phirc
    val chid = chid

    method get_name = phirc
    method sight_change = function
        Chara.Appear_chara (chara, vpos) ->
          self#send_message
            (String.concat ""
               ["appear "; chara#get_name;
                " on "; string_of_int vpos.Phi_map.x; ":"; string_of_int vpos.Phi_map.y]);
          self#mapview_update;
          []
      | Chara.Move_chara (chara, (ovpos, nvpos)) ->
        self#send_message
          (String.concat ""
             ["move "; chara#get_name;
              " on "; string_of_int ovpos.Phi_map.x; ":"; string_of_int ovpos.Phi_map.y;
              " to "; string_of_int nvpos.Phi_map.x; ":"; string_of_int nvpos.Phi_map.y]);
        []
      | Chara.Disappear_chara (chara, vpos) ->
        self#send_message
          (String.concat ""
             ["dipappear "; chara#get_name;
              " on "; string_of_int vpos.Phi_map.x; ":"; string_of_int vpos.Phi_map.y]);
        []

    method turn ~dir =
      let adir =
        (match dir with
            Phi_map.Absolute_direction adir -> adir
          | Phi_map.Relative_direction rdir ->
            let adir =Phi_map.get_chara_absolute_direction ~chara_id:chid in
            Phi_map.turn_absolute_direction ~adir ~rdir
        )
      in
      Phi_map.set_chara_direction ~chara_id:chid ~adir;
      self#mapview_update;
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
          None -> []
        | Some next_pos ->
          Phi_map.set_chara_position ~chara_id:chid ~pos:next_pos;
          self#mapview_update;
          []

    method private send_message msg = Client_manager.send_message ~cid ~msg
    method mapview_update =
      self#send_message (Protocol.encode_server_protocol (Protocol.M57Map (Phi_map.get_mapview ~chara_id:chid)))
  end in
  let pos = Phi_map.get_default_position in (* tentative *)
  let adir = Phi_map.North in (* tentative *)
  Phi_map.set_chara_position ~chara_id:chid ~pos;
  Phi_map.set_chara_direction ~chara_id:chid ~adir;
  chara#mapview_update;
  chara
;;
