type result =
    Hp_damage of (int * string)
  | Kill

type t = Chara_status.t -> (Chara_status.t * result list)

val create : attack_status:Chara_status.t -> attack_name:string -> t
