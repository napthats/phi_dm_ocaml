val get_chara : Chara_id.t -> Chara.t option
val get_pc_by_cid : Tcp_server.client_id -> Player_character.t option
val get_pc_by_phirc : string -> Player_character.t option
val get_chara_id_by_cid : Tcp_server.client_id -> Chara_id.t option
val create_npc : unit -> Event.t list
val create_pc : Tcp_server.client_id -> string -> (Event.t list) option
val remove_chara : Chara_id.t -> Event.t list
val map : (Chara.t -> 'a) -> 'a list
