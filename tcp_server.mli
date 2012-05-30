type client_id
class tcp_server :
  int ->
  object
    method get_each_client_messages : (client_id * string) list
    method listen : unit
    method send_message_to : cid:client_id -> msg:string -> unit
  end

