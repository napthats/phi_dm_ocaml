type event =
(*    Client_message of (Client_manager.client_id * Protocol.client_protocol) *)
    Client_message of (Tcp_server.client_id * string)
(*  | Position_change of (Chara_manager.chara_id * (Phi_map.position option * Phi_map.position option))
  | Status_view_change of (Chara_manager.chara_id * Phi_map.position
                      * (Chara_manager.chara_status_view * Chara_manager.chara_status_view))
  | Time_tick of int *)
;;

