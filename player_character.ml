let create ~phirc ~cid : Chara.chara = 
  object (self)
    val cid = cid
    val phirc = phirc

    method get_name = phirc
    method get_position = Phi_map.get_default_position
    method sight_change = function
        Chara.Appear_chara (chara, vpos) ->
          self#send_message
            (String.concat ""
               ["appear "; chara#get_name;
                " on "; string_of_int vpos.Phi_map.x; ":"; string_of_int vpos.Phi_map.y]);
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
  end
;;
