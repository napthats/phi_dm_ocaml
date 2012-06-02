let rec main_loop event_list =
  Thread.delay 1.0;
  let clock_event_list = Clock.tick () in
  let client_message_event_list = Client_manager.fetch_messages () in
  let event_list =
    Chara_manager.event_dispatch
      ~event_list:(List.concat [clock_event_list; event_list; client_message_event_list])
  in
  main_loop event_list
;;

let _ =
  Random.self_init ();
  main_loop []
;;
