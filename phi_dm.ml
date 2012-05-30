let rec main_loop event_list =
  Thread.delay 1.0;
  let client_message_event_list = Client_manager.fetch_messages () in
  let event_list =
    Chara_manager.event_dispatch ~event_list:(List.append event_list client_message_event_list) in
  main_loop event_list
;;

let _ = main_loop []
;;
