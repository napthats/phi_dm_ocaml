type position

type view_position = {x : int; y : int}

type absolute_direction = North | East | West | South

type mapchip_view = Bars | Door | Dummy | Flower | Glass | Grass | Mist | Mwall | Pcircle | Road | Rock | Tgate | Unknown | Water | Window | Wood | Wwall | Door_lock | Pcircle_lock

val get_cansee_chara_list : pos:position -> (Chara_id.chara_id * view_position) list

val get_default_position : position

val set_chara_position : chara_id:Chara_id.chara_id -> pos:position -> unit

val set_chara_direction : chara_id:Chara_id.chara_id -> dir:absolute_direction -> unit

val get_mapview : chara_id:Chara_id.chara_id -> (absolute_direction * (mapchip_view list) list)
