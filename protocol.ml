open ExtString

type raw_client_protocol =
    Raw_message of string
;;

type sharp_client_protocol =
    Open of string
  | Unknown
;;

type client_protocol =
    Raw_client_protocol of raw_client_protocol
  | Sharp_client_protocol of sharp_client_protocol
;;

let decode_sharp_client_protocol protocol =
  match String.nsplit protocol " " with
      ["open"; phirc] -> Open phirc
    | _ -> Unknown
;;


let decode_raw_client_protocol protocol = Raw_message protocol;;

let decode_client_protocol protocol =
  if protocol.[0] = '#'
  then Sharp_client_protocol (decode_sharp_client_protocol (String.lchop protocol))
  else Raw_client_protocol (decode_raw_client_protocol protocol)
;;
