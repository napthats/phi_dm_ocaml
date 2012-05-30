type sight_change_type =
    Appear_chara of (chara * Phi_map.view_position)
  | Move_chara of (chara * (Phi_map.view_position * Phi_map.view_position))
  | Disappear_chara of (chara * Phi_map.view_position)
and
chara = 
    <get_position : Phi_map.position;
     get_name : string;
     sight_change : sight_change_type -> Event.event list>
