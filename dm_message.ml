open Chara_status


type msg_type =
    Go_no
  | Turn_bad
  | Attack_hp of (string * string * string * int)
  | Kill_by of (string * string)
  | No_item_here
  | Get_bad
  | Get_no
  | Get of (string * string)
;;

let make = function
    Go_no -> "DM > Can not go."
  | Turn_bad -> "DM > Which direction? Type 'turn ????'."
  | Attack_hp (aname, dname, weapon, value) ->
    "DM > " ^ aname ^ " attacked to " ^ dname ^ " by " ^ weapon
    ^ ". [/*color=-hp*/" ^ (string_of_int value) ^ "/*.*/ hp]"
  | Kill_by (aname, dname) ->
    "DM > " ^ dname ^ " is killed by " ^ aname ^ "."
  | No_item_here -> "DM > Here is no item."
  | Get_bad -> "DM > Can not get such a thing."
  | Get_no -> "DM > Can not get."
  | Get (chara_name, item_name) -> "DM > " ^ chara_name ^ " gets " ^ item_name ^ "."
;;
