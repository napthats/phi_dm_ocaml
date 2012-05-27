let server = new Tcp_server.tcp_server 20017;;
let _ = server#listen;;

let fetch_messages () =
  print_endline "hi";
(*  List.map (fun msg -> Event.Client_message msg) server#get_each_client_messages; *)
  List.map (fun msg -> match msg with (_, m) -> print_endline m; Event.Client_message msg) server#get_each_client_messages;
;;

let send_message ~cid ~msg = ignore (server#send_message_to ~cid ~msg);;
