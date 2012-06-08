val fetch_messages : unit -> Event.t list
val send_message : cid:Tcp_server.client_id -> msg:string -> unit
val disconnect : cid:Tcp_server.client_id -> unit
