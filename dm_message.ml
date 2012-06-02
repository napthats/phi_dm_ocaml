type msg_type =
    Go_no
  | Turn_bad
;;

let make = function
    Go_no -> "DM > Can not go."
  | Turn_bad -> "DM > Which direction? Type 'turn ????'."
;;
