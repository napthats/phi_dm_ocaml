type position

type view_position = {x : int; y : int}

val get_cansee_chara_list : pos:position -> (Chara_id.chara_id * view_position) list

val get_default_position : position

val set_chara_position : chara_id:Chara_id.chara_id -> pos:position -> unit
