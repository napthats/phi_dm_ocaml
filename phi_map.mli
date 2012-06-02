type position

type view_position = {x : int; y : int}

type absolute_direction = North | East | West | South

type relative_direction = Forth | Right | Left | Back

type direction = Absolute_direction of absolute_direction | Relative_direction of relative_direction

type mapchip_view = Bars | Door | Dummy | Flower | Glass | Grass | Mist | Mwall | Pcircle | Road | Rock | Tgate | Unknown | Water | Window | Wood | Wwall | Door_lock | Pcircle_lock

val get_cansee_chara_list : pos:position -> (Chara_id.chara_id * view_position) list

val get_default_position : position

val get_neighbor_position : pos:position -> adir:absolute_direction -> (position option)

val set_chara_position : chara_id:Chara_id.chara_id -> pos:position -> unit

val set_chara_direction : chara_id:Chara_id.chara_id -> adir:absolute_direction -> unit

(* throw exeption if there is no chara with chara_id *)
val get_chara_position : chara_id:Chara_id.chara_id -> position

(* throw exeption if there is no chara with chara_id *)
val get_chara_absolute_direction : chara_id:Chara_id.chara_id -> absolute_direction

(* throw exeption if there is no chara with chara_id *)
val get_mapview : chara_id:Chara_id.chara_id -> (absolute_direction * (mapchip_view list) list)

val turn_absolute_direction : adir:absolute_direction -> rdir:relative_direction -> absolute_direction
