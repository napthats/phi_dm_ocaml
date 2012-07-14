open ExtString


type raw_client_protocol =
    Raw_message of string
  | Go of Phi_map_data.direction
  | Turn of Phi_map_data.direction option
  | Hit
  | Get of string option
  | Use of string option
  | Unequip
  | Check
  | Exit
  | Cast

type sharp_client_protocol =
    Open of string
  | Unknown

type client_protocol =
    Raw_client_protocol of raw_client_protocol
  | Sharp_client_protocol of sharp_client_protocol


let decode_sharp_client_protocol protocol =
  match String.nsplit protocol " " with
      ["open"; phirc] -> Open phirc
    | _ -> Unknown

let string_to_direction = function
    "" -> None
  | str ->
    match str.[0] with
        'n' -> Some (Phi_map_data.Absolute_direction Phi_map_data.North)
      | 'e' -> Some (Phi_map_data.Absolute_direction Phi_map_data.East)
      | 'w' -> Some (Phi_map_data.Absolute_direction Phi_map_data.West)
      | 's' -> Some (Phi_map_data.Absolute_direction Phi_map_data.South)
      | 'f' -> Some (Phi_map_data.Relative_direction Phi_map_data.Forth)
      | 'r' -> Some (Phi_map_data.Relative_direction Phi_map_data.Right)
      | 'l' -> Some (Phi_map_data.Relative_direction Phi_map_data.Left)
      | 'b' -> Some (Phi_map_data.Relative_direction Phi_map_data.Back)
      | _ -> None

let decode_raw_client_protocol protocol =
  match String.nsplit protocol " " with
      ["go"] -> Go (Phi_map_data.Relative_direction Phi_map_data.Forth)
    | ["go"; dir] ->
      (match string_to_direction dir with
          None -> Go (Phi_map_data.Relative_direction Phi_map_data.Forth)
        | Some d -> Go d)
    | ["turn"] -> Turn None
    | ["turn"; dir] -> Turn (string_to_direction dir)
    | ["hit"] -> Hit
    | ["get"] -> Get None
    | "get" :: name_list -> Get (Some (String.concat " " name_list))
    | ["use"] -> Use None
    | "use" :: name_list -> Use (Some (String.concat " " name_list))
    | ["check"] -> Check
    | ["exit"] -> Exit
    | ["unequip"] -> Unequip
    | ["cast"] -> Cast
    | _ -> Raw_message protocol


let decode_client_protocol protocol =
  if protocol.[0] = '#'
  then Sharp_client_protocol (decode_sharp_client_protocol (String.lchop protocol))
  else Raw_client_protocol (decode_raw_client_protocol protocol)


type object_type = B_obj | C_obj | F_obj;;

type server_protocol =
    M57_map of (Phi_map_data.absolute_direction * ((Phi_map_data.view list) list))
  | M57_obj of (object_type * int * int * Phi_map_data.relative_direction * string)
  | M57_end

let mapchip_view_to_string = function
    Phi_map_data.Bars -> "I"
  | Phi_map_data.Door -> "["
  | Phi_map_data.Dummy -> "o"
  | Phi_map_data.Flower -> "+"
  | Phi_map_data.Glass -> "="
  | Phi_map_data.Grass -> ":"
  | Phi_map_data.Mist -> "/"
  | Phi_map_data.Mwall -> "H"
  | Phi_map_data.Pcircle -> "x"
  | Phi_map_data.Road -> " "
  | Phi_map_data.Rock -> "@"
  | Phi_map_data.Tgate -> ">"
  | Phi_map_data.Unknown -> "?"
  | Phi_map_data.Water -> "_"
  | Phi_map_data.Window -> "|"
  | Phi_map_data.Wood -> "T"
  | Phi_map_data.Wwall -> "#"
  | Phi_map_data.Door_lock -> "{"
  | Phi_map_data.Pcircle_lock -> "#"

let absolute_direction_to_string = function
    Phi_map_data.North -> "N"
  | Phi_map_data.East -> "E"
  | Phi_map_data.West -> "W"
  | Phi_map_data.South -> "S"

let encode_server_protocol = function
    M57_end -> "#m57 ."
  | M57_obj (otype, x, y, rdir, name) ->
    let otype_string =
      (match otype with
          B_obj -> "B"
        | C_obj -> "C"
        | F_obj -> "F")
    in
    let dir_string =
      (match rdir with
          Phi_map_data.Forth -> "F"
        | Phi_map_data.Right -> "R"
        | Phi_map_data.Left -> "L"
        | Phi_map_data.Back -> "B")
    in
    Printf.sprintf
      "#m57 O %s0000:%d %d %s %-31s 00                 # 00"
      otype_string x y dir_string name
  | M57_map (adir, view_list) ->
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
               (fun acc chip ->
                 (match chip.Phi_map_data.item with
                     None -> 
                     acc ^ mapchip_view_to_string chip.Phi_map_data.chip ^ (String.make 1 (Char.chr 128))
                   | Some Phi_map_data.Normal ->
                     acc ^ mapchip_view_to_string chip.Phi_map_data.chip ^ (String.make 1 (Char.chr 224))))
               ""
               line
           )
           ""
           view_list
      )
