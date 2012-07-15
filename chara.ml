open Phi_map.Open

type equip_flag = Wpn | Arm | Acr

type sight_change_type =
    Appear_chara of (t * Phi_map_data.view_position)
  | Move_chara of (t * (Phi_map_data.view_position * Phi_map_data.view_position))
  | Disappear_chara of (t * Phi_map_data.view_position)
and
t = <get_name : string;
     get_status_view : Chara_status.view;
     get_item_list : (Item.t * equip_flag option) list;
     get_chara_id : Chara_id.t;
     get_spell_list : Spell.t list;
     sight_change : sight_change_type -> Event.t list;
     sight_update : Event.t list;
     turn : dir:Phi_map_data.direction -> Event.t list;
     go : dir:Phi_map_data.direction -> Event.t list;
     do_action : Event.t list;
     hit : Event.t list;
     defense : combat:Combat.t -> achid:Chara_id.t -> Event.t list;
     resolve_attack_result : result_list:Combat.result list -> dchid:Chara_id.t -> Event.t list;
     (* throw an exception if there is no such item *)
     item_get : item:Item.t -> Event.t list;
     dead : Event.t list;
     say : msg:string -> Event.t list;
     listen : msg:string -> achid:Chara_id.t -> Event.t list;
     use_item : item:Item.t -> Event.t list;
     unequip_item : item:Item.t -> Event.t list;
     move : pos:Phi_map.position -> Event.t list;
     select_list : list:string list -> Event.t list;
     cast : spell:Spell.t -> Event.t list>
