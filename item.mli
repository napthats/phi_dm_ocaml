type t

type attack_range = Forth

type material = Leather | Wood | Stone | Steel | Silver | Mythril | Auric | Adamantite | Magical

type weapon_type = Fist | Sword | Blade | Stinger | Spear | Axe | Striker | Whip | Fang | Wide | Around

type element = Earth | Light | Air | Spirit | Water | Fire | ELNone

type effect = EFNone

type special_effect = SENone

type weapon_view = {element : element; er : int; effect : effect; special_effect : special_effect}

type item_type = Weapon of weapon_view | Food of int

type view =
      {name : string; attack_range : attack_range; material : material; weapon_type : weapon_type;
       atp : int; item_type : item_type}

val create : view:view -> t

val get_view : item:t -> view

val get_name : item:t -> string
