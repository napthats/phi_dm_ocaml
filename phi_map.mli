
type view_position = Phi_map_data.view_position

type absolute_direction = Phi_map_data.absolute_direction

type relative_direction = Phi_map_data.relative_direction

type direction = Phi_map_data.direction

type mapchip = Phi_map_data.mapchip

type flooritem = Phi_map_data.flooritem

type view = Phi_map_data.view

type position

val get_cansee_chara_list : pos:position -> (Chara_id.t * view_position) list

val get_default_position : position

val get_neighbor_position : pos:position -> adir:absolute_direction -> (position option)

val set_chara_position : chara_id:Chara_id.t -> pos:position -> unit

val set_chara_direction : chara_id:Chara_id.t -> adir:absolute_direction -> unit

(* throw exeption if there is no chara with chara_id *)
val get_chara_position : chara_id:Chara_id.t -> position

(* throw exeption if there is no chara with chara_id *)
val get_chara_absolute_direction : chara_id:Chara_id.t -> absolute_direction

(* throw exeption if there is no chara with chara_id *)
val get_mapview : chara_id:Chara_id.t -> (absolute_direction * (view list) list)

val turn_absolute_direction : adir:absolute_direction -> rdir:relative_direction -> absolute_direction

val get_chara_list_with_position : pos:position -> Chara_id.t list

val get_item_list_with_position : pos:position -> Item.t list

(* throw exeption if there is no chara with chara_id *)
val delete_chara : chara_id:Chara_id.t -> unit

(* delete first item on position *)
(* throw exeption if there is no such item *)
val delete_item : pos:position -> item:Item.t -> unit

val add_item : pos:position -> item:Item.t -> unit

(* throw exeption if there is no chara with chara_id *)
val get_chara_in_sight_list : chara_id:Chara_id.t -> (Chara_id.t * view_position * relative_direction) list

val is_enterable : pos:position -> bool

module Open : sig
module Event : sig
type t =
    Client_message of (Tcp_server.client_id * Protocol.client_protocol) 
  | Position_change of (Chara_id.t * (position option * position option))
  | Npc_appear
  | Tick
  | Attack_to of (Chara_id.t * (position * Combat.t))
  | Attack_result of ((Chara_id.t * Chara_id.t) * Combat.result list)
  | Dead of Chara_id.t
  | Say of (Chara_id.t * string)
  | Switch_move of (Chara_id.t * position)
  | Switch_list of (Chara_id.t * string list)
  | Switch_select_done of (Chara_id.t * int * position)
(*  | Status_view_change of (Chara_manager.chara_id * Phi_map.position
                      * (Chara_manager.chara_status_view * Chara_manager.chara_status_view))
  | Time_tick of int *)
end
end

val event_dispatch : event_list:Open.Event.t list -> Open.Event.t list
