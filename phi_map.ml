type position = {px : int; py : int};;

type view_position = {x : int; y : int};;

let charaid_list = ref [];;

let get_default_position = {px = 0; py = 0};;

let get_cansee_chara_list ~pos:_ =
  List.map (fun chid -> (chid, {x = 0; y = 0})) !charaid_list
;;

let set_chara_position ~chara_id ~pos:_ =
  charaid_list := chara_id :: !charaid_list
;;
