type t

type spell_effect = Hp_pierce

type view = {name : string; effect : spell_effect; power : int; cast_time : int}

val create : view:view -> t

val get_view : spell:t -> view

val get_name : spell:t -> string
