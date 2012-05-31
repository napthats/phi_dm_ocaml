type sight_change_type =
    Appear_chara of (chara * Phi_map.view_position)
  | Move_chara of (chara * (Phi_map.view_position * Phi_map.view_position))
  | Disappear_chara of (chara * Phi_map.view_position)
and
chara = 
    <get_name : string;
     sight_change : sight_change_type -> Event.event list;
     turn : dir:Phi_map.direction -> Event.event list;
     go : dir:Phi_map.direction -> Event.event list;
     mapview_update : unit>
