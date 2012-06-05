open ExtString


type raw_client_protocol =
    Raw_message of string
  | Go of Phi_map.direction
  | Turn of Phi_map.direction option
  | Hit
  | Get of string option
  | Check
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

let string_to_direction = function
    "" -> None
  | str ->
    match str.[0] with
        'n' -> Some (Phi_map.Absolute_direction Phi_map.North)
      | 'e' -> Some (Phi_map.Absolute_direction Phi_map.East)
      | 'w' -> Some (Phi_map.Absolute_direction Phi_map.West)
      | 's' -> Some (Phi_map.Absolute_direction Phi_map.South)
      | 'f' -> Some (Phi_map.Relative_direction Phi_map.Forth)
      | 'r' -> Some (Phi_map.Relative_direction Phi_map.Right)
      | 'l' -> Some (Phi_map.Relative_direction Phi_map.Left)
      | 'b' -> Some (Phi_map.Relative_direction Phi_map.Back)
      | _ -> None
;;

let decode_raw_client_protocol protocol =
  match String.nsplit protocol " " with
      ["go"] -> Go (Phi_map.Relative_direction Phi_map.Forth)
    | ["go"; dir] ->
      (match string_to_direction dir with
          None -> Go (Phi_map.Relative_direction Phi_map.Forth)
        | Some d -> Go d)
    | ["turn"] -> Turn None
    | ["turn"; dir] -> Turn (string_to_direction dir)
    | ["hit"] -> Hit
    | ["get"] -> Get None
    | "get" :: name_list -> Get (Some (String.concat " " name_list))
    | ["check"] -> Check
    | _ -> Raw_message protocol
;;


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
(*      absolute_direction_to_string adir
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
      ^ "\n"*)
      "#m57 M " ^ (absolute_direction_to_string adir) ^ "        0:"
      ^ (List.fold_left
           (fun line_acc line ->
             line_acc
             ^ List.fold_left
               (fun acc chip -> acc ^ mapchip_view_to_string chip ^ (String.make 1 (Char.chr 128)))
               ""
               line
           )
           ""
           view_list
      )
;;
