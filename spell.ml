type spell_effect = Hp_pierce

type view = {name : string; effect : spell_effect; power : int; cast_time : int}

type t = view

let create ~view = view

let get_view ~spell = spell

let get_name ~spell = spell.name
