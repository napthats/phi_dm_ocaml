class phi_dm =
object (self)
  val client_manager = new Client_manager.client_manager
  val chara_manager = new Chara_manager.chara_manager
  val phi_map = new Phi_map.phi_map

  method main =
    self#main_loop []

  method private main_loop ~event_list =
    let client_message_event_list = client_manager#fetch_messages in
    let event_list =
      self#event_dispatch ~event_list:(List.append event_list client_message_event_list) in
    main_loop ~event_list

  method private event_dispatch ~event_list =
    chara_manager#event_dispatch ~event_list ~phi_map
end;;

type event =
    Client_message of (Client_message.client_id * Protocol.client_protocol)
(*  | Position_change of (Chara_manager.chara_id * (Phi_map.position option * Phi_map.position option))
  | Status_change of (Chara_manager.chara_id * Phi_map.position
                      * (Chara_manager.chara_status_view * Chara_manager.chara_status_view))
  | Time_tick of int *)
