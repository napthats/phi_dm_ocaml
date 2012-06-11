type sight_change_type =
    Appear_chara of (t * Phi_map.view_position)
  | Move_chara of (t * (Phi_map.view_position * Phi_map.view_position))
  | Disappear_chara of (t * Phi_map.view_position)
and
t = <get_name : string;
     get_status_view : Chara_status.view;
     get_item_list : Item.t list;
     sight_change : sight_change_type -> Event.t list;
     sight_update : Event.t list;
     turn : dir:Phi_map.direction -> Event.t list;
     go : dir:Phi_map.direction -> Event.t list;
     do_action : Event.t list;
     hit : Event.t list;
     defense : combat:Combat.t -> achid:Chara_id.t -> Event.t list;
     resolve_attack_result : result_list:Combat.result list -> dchid:Chara_id.t -> Event.t list;
     (* throw an exception if there is no such item *)
     item_get : item:Item.t -> Event.t list;
     get_phirc : string option>
;;
