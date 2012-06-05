type raw_client_protocol =
    Raw_message of string
  | Go of Phi_map.direction
  | Turn of Phi_map.direction option
  | Hit
  | Get of string option
  | Check

type sharp_client_protocol =
    Open of string
  | Unknown

type client_protocol =
    Raw_client_protocol of raw_client_protocol
  | Sharp_client_protocol of sharp_client_protocol

val decode_client_protocol : string -> client_protocol

type server_protocol =
    M57Map of (Phi_map.absolute_direction * ((Phi_map.mapchip_view list) list))

val encode_server_protocol : server_protocol -> string
