type msg_type =
    Go_no
  | Turn_bad
  | Attack_hp of (string * string * string * int)
  | Kill_by of (string * string)
;;

let make = function
    Go_no -> "DM > Can not go."
  | Turn_bad -> "DM > Which direction? Type 'turn ????'."
  | Attack_hp (aname, dname, weapon, value) ->
    "DM > " ^ aname ^ " attacked to " ^ dname ^ " by " ^ weapon
    ^ ". [/*color=-hp*/" ^ (string_of_int value) ^ "/*.*/ hp]"
  | Kill_by (aname, dname) ->
    "DM > " ^ dname ^ " is killed by " ^ aname ^ "."
;;
