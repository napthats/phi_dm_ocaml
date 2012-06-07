let rec main_loop event_list prev_time =
  let clock_event_list = Clock.tick () in
  let client_message_event_list = Client_manager.fetch_messages () in
  let event_list =
    Chara_manager.event_dispatch
      ~event_list:(List.concat [event_list; client_message_event_list; clock_event_list])
  in
  Thread.delay (max 0.0 (0.1 -. ((Sys.time ()) -. prev_time)));
  print_endline "-----------------";
  print_endline (string_of_float (max 0.0 (0.1 -. ((Sys.time ()) -. prev_time))));
  print_endline (string_of_float (Sys.time ()));
  main_loop event_list (Sys.time ())
;;

let _ =
  Random.self_init ();
  main_loop [] (Sys.time ())
;;
