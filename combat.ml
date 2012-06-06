open Chara_status.Open


type result =
    Hp_damage of int
  | Kill
;;

type t = Chara_status.t -> (Chara_status.t * result list);;


let create ~attack_status defense_status =
  let aview = Chara_status.get_view ~status:attack_status in
  let _ = Chara_status.get_view ~status:defense_status in
  let damage = aview.flv in
  let defense_status = Chara_status.add_hp ~hp:(-damage) ~status:defense_status in
  let result_list =
    if Chara_status.is_dead ~status:defense_status
    then [Hp_damage damage; Kill]
    else [Hp_damage damage]
  in
  (defense_status, result_list)
;;
