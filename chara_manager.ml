let event_dispatch ~event_list =
  let cmsg_list = List.map (fun (Event.Client_message cmsg) -> cmsg) event_list in
  ignore (List.map (fun (cid, msg) -> Client_manager.send_message ~cid ~msg) cmsg_list);
  []
;;
