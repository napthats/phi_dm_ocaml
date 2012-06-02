type sight_change_type =
    Appear_chara of (t * Phi_map.view_position)
  | Move_chara of (t * (Phi_map.view_position * Phi_map.view_position))
  | Disappear_chara of (t * Phi_map.view_position)
and
t = 
    <get_name : string;
     sight_change : sight_change_type -> Event.t list;
     turn : dir:Phi_map.direction -> Event.t list;
     go : dir:Phi_map.direction -> Event.t list;
     do_action : Event.t list>
;;
