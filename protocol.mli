type raw_client_protocol =
    Raw_message of string
  | Go of Phi_map.direction
  | Turn of Phi_map.direction option
  | Hit
  | Get of string option
  | Check
  | Exit

type sharp_client_protocol =
    Open of string
  | Unknown

type client_protocol =
    Raw_client_protocol of raw_client_protocol
  | Sharp_client_protocol of sharp_client_protocol

val decode_client_protocol : string -> client_protocol

type object_type = B_obj | C_obj | F_obj;;

type server_protocol =
    M57_map of (Phi_map.absolute_direction * ((Phi_map.mapchip_view list) list))
  | M57_obj of (object_type * int * int * Phi_map.relative_direction * string)
  | M57_end

val encode_server_protocol : server_protocol -> string
