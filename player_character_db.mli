val load :
  phirc:string -> (string * Phi_map.position * Phi_map.absolute_direction * Chara_status.view * Item.t list * Spell.t list) option
val save :
  phirc:string -> pos:Phi_map.position -> adir:Phi_map.absolute_direction -> name:string -> status:Chara_status.view -> item_list:Item.t list -> spell_list:Spell.t list -> unit
