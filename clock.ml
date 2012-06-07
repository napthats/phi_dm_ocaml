let current_tick = ref 0;;

let tick () =
  current_tick := !current_tick + 1;
  let event_list =
    if !current_tick mod 200 = 1
    then [Event.Npc_appear]
    else []
  in
  Event.Tick :: event_list
;;
