let server = new Tcp_server.tcp_server 20017
let _ = server#listen

let fetch_messages () =
  List.map
    (fun (cid, msg) -> Event.Client_message (cid, Protocol.decode_client_protocol msg))
    server#get_each_client_messages

let send_message ~cid ~msg = server#send_message_to ~cid ~msg:(msg ^ "\n")

let disconnect ~cid = server#disconnect_client ~cid
