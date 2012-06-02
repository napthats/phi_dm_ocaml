type msg_type =
    Go_no
  | Turn_bad
  | Attack_hp of (string * string * string * int)
;;

let make = function
    Go_no -> "DM > Can not go."
  | Turn_bad -> "DM > Which direction? Type 'turn ????'."
  | Attack_hp (name, vsname, weapon, value) ->
    "DM > " ^ name ^ " attacked to " ^ vsname ^ " by " ^ weapon
    ^ ". [/*color=-hp*/" ^ (string_of_int value) ^ "/*.*/ hp]"
;;
