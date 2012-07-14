type t =
    Client_message of (Tcp_server.client_id * Protocol.client_protocol) 
  | Position_change of (Chara_id.t * (Phi_map.position option * Phi_map.position option))
  | Npc_appear
  | Tick
  | Attack_to of (Chara_id.t * (Phi_map.position * Combat.t))
  | Attack_result of ((Chara_id.t * Chara_id.t) * Combat.result list)
  | Dead of Chara_id.t
  | Say of (Chara_id.t * string)
(*  | Status_view_change of (Chara_manager.chara_id * Phi_map.position
                      * (Chara_manager.chara_status_view * Chara_manager.chara_status_view))
  | Time_tick of int *)

