open Chara_status.Open


let db = Hashtbl.create 100;;
let _ = Hashtbl.replace db "guest1"
  ("guest 1",
   Phi_map.get_default_position,
   Phi_map.North,
   {hp = 5000; mhp = 5200; mp = 5100; mmp = 5500;
    flv = 120; wlv = 1; mlv = 2; clv = 3;
    state = Chara_status.Command; condition = []},
   [Item.create ~view:({Item.name = "sword"; Item.attack_range = Item.Forth; Item.material = Item.Steel; weapon_type = Item.Sword; Item.atp = 10; Item.item_type =Item.Weapon {Item.element = Item.Fire; Item.er = 30; Item.effect = Item.EFNone; Item.special_effect = Item.SENone}})],
   [Spell.create ~view:{Spell.name = "fire"; Spell.effect = Spell.Hp_pierce; Spell.power = 100; Spell.cast_time = 10}])
;;
let _ = Hashtbl.replace db "guest2"
  ("guest 2",
   Phi_map.get_default_position,
   Phi_map.South,
   {hp = 100; mhp = 5200; mp = 5100; mmp = 5500;
    flv = 1200; wlv = 1; mlv = 2; clv = 3;
    state = Chara_status.Command; condition = []},
   [(Item.create ~view:({Item.name = "sword2"; Item.attack_range = Item.Forth; Item.material = Item.Steel; Item.weapon_type = Item.Sword; Item.atp = 10; Item.item_type =Item.Weapon {Item.element = Item.Fire; Item.er = 30; Item.effect = Item.EFNone; Item.special_effect = Item.SENone}}));
   (Item.create ~view:({Item.name = "Nuts"; Item.attack_range = Item.Forth; Item.material = Item.Wood; Item.weapon_type = Item.Sword; Item.atp = 10; Item.item_type = (Item.Food 250)}))],
   [Spell.create ~view:{Spell.name = "fire"; Spell.effect = Spell.Hp_pierce; Spell.power = 100; Spell.cast_time = 10}])
;;


let load ~phirc = 
  if Hashtbl.find_all db phirc = []
  then None
  else
    Some (Hashtbl.find db phirc)
;;

let save ~phirc ~pos ~adir ~name ~status ~item_list ~spell_list =
  Hashtbl.replace db phirc (name, pos, adir, status, item_list, spell_list)
;;
