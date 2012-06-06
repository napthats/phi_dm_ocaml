type attack_range = Forth;;

type material = Leather | Wood | Stone | Steel | Silver | Mythril | Auric | Adamantite | Magical;;

type weapon_type =
    Fist | Sword | Blade | Stinger | Spear | Axe | Striker | Whip | Fang | Wide | Around
;;

type element = Earth | Light | Air | Spirit | Water | Fire | ELNone;;

type effect = EFNone;;

type special_effect = SENone;;


module Open =
struct
  type weapon_view = {element : element; er : int; effect : effect; special_effect : special_effect}

  type item_type = Weapon of weapon_view

  type view =
      {name : string; attack_range : attack_range; material : material; weapon_type : weapon_type;
       atp : int; item_type : item_type}
end;;

include Open;;


type t = view;;


let create ~view = view;;

let get_view ~item = item;;

let get_name ~item = item.name;;
