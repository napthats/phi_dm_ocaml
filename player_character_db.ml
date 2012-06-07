open Item.Open
open Chara_status.Open


let db = Hashtbl.create 100;;
let _ = Hashtbl.replace db "guest1"
  ("guest 1",
   Phi_map.get_default_position,
   Phi_map.North,
   {hp = 5000; mhp = 5200; mp = 5100; mmp = 5500;
    flv = 120; wlv = 1; mlv = 2; clv = 3;
    state = Chara_status.Command; condition = []},
   [Item.create ~view:({name = "sword"; attack_range = Item.Forth; material = Item.Steel; weapon_type = Item.Sword; atp = 10; item_type =Item.Weapon {element = Item.Fire; er = 30; effect = Item.EFNone; special_effect = Item.SENone}})])
;;
let _ = Hashtbl.replace db "guest2"
  ("guest 2",
   Phi_map.get_default_position,
   Phi_map.South,
   {hp = 100; mhp = 5200; mp = 5100; mmp = 5500;
    flv = 1200; wlv = 1; mlv = 2; clv = 3;
    state = Chara_status.Command; condition = []},
   [Item.create ~view:({name = "sword2"; attack_range = Item.Forth; material = Item.Steel; weapon_type = Item.Sword; atp = 10; item_type =Item.Weapon {element = Item.Fire; er = 30; effect = Item.EFNone; special_effect = Item.SENone}})])
;;


let load ~phirc = 
  if Hashtbl.find_all db phirc = []
  then None
  else
    Some (Hashtbl.find db phirc)
;;

let save ~phirc ~pos ~adir ~name ~status ~item_list =
  Hashtbl.replace db phirc (name, pos, adir, status, item_list)
;;
