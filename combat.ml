open Chara_status


type result =
    Hp_damage of int
;;

type t = Chara_status.t -> (Chara_status.t * result list);;


let create ~attack_status defense_status =
  let aview = Chara_status.get_view ~status:attack_status in
  let _ = Chara_status.get_view ~status:defense_status in
  let damage = aview.flv in
  (Chara_status.add_hp ~hp:damage ~status:defense_status, [Hp_damage damage])
;;
