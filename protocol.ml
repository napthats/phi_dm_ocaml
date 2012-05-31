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

type server_protocol =
    M57Map of (Phi_map.absolute_direction * ((Phi_map.mapchip_view list) list))
;;

let mapchip_view_to_string = function
    Phi_map.Bars -> "I"
  | Phi_map.Door -> "["
  | Phi_map.Dummy -> "o"
  | Phi_map.Flower -> "+"
  | Phi_map.Glass -> "="
  | Phi_map.Grass -> ":"
  | Phi_map.Mist -> "/"
  | Phi_map.Mwall -> "H"
  | Phi_map.Pcircle -> "x"
  | Phi_map.Road -> " "
  | Phi_map.Rock -> "@"
  | Phi_map.Tgate -> ">"
  | Phi_map.Unknown -> "?"
  | Phi_map.Water -> "_"
  | Phi_map.Window -> "|"
  | Phi_map.Wood -> "T"
  | Phi_map.Wwall -> "#"
  | Phi_map.Door_lock -> "{"
  | Phi_map.Pcircle_lock -> "#"
;;

let absolute_direction_to_string = function
    Phi_map.North -> "N"
  | Phi_map.East -> "E"
  | Phi_map.West -> "W"
  | Phi_map.South -> "S"
;;

(* for telnet *)
let encode_server_protocol = function
    M57Map (adir, view_list) ->
      absolute_direction_to_string adir
      ^ (List.fold_left
           (fun line_acc line ->
             line_acc
             ^ List.fold_left
               (fun acc chip -> acc ^ mapchip_view_to_string chip ^ " ")
               "\n"
               line
           )
           ""
           view_list
      )
      ^ "\n"
;;
