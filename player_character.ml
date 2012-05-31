let create ~phirc ~cid ~chid : Chara.chara = 
  let pos = Phi_map.get_default_position in
  let dir = Phi_map.North in (* tentative *)
  let chara = object (self)
    val cid = cid
    val phirc = phirc
    val chid = chid
    val pos = pos

    method get_name = phirc
    method get_position = pos
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

    method private send_message msg = Client_manager.send_message ~cid ~msg
    method private mapview_update =
      self#send_message (Protocol.encode_server_protocol (Protocol.M57Map (Phi_map.get_mapview ~chara_id:chid)))
  end in
  Phi_map.set_chara_position ~chara_id:chid ~pos;
  Phi_map.set_chara_direction ~chara_id:chid ~dir:(Phi_map.Absolute_direction dir);
  chara
;;
