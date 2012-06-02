type result = Hp_damage of int

type t = Chara_status.t -> (Chara_status.t * result list)

val create : attack_status:Chara_status.t -> t
