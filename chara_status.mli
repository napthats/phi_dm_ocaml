type t

type state = Command

type condition = Dummy


module Open :
sig
  type view = 
      {hp : int; mhp : int; mp : int; mmp : int;
       flv : int; wlv : int; mlv : int; clv : int;
       state : state; condition : condition list}
end

type view = Open.view

(* type view_diff = 
    {dhp : int; dmhp : int; dmp : int; dmmp : int;;
     dflv : int; dwlv : int; dmlv : int; dclv : int;
     state_change : bool;
     deleting_condition : condition list; adding_condition : condition list} *)

(* use it only when chara create *)
val create : view:view -> t

val get_view : status:t -> view

(* hp(mp) can exceed max hp(mp) *)

val add_hp : hp:int -> status:t -> t
val add_mp : mp:int -> status:t -> t

val is_dead : status:t -> bool

(*val get_status_diff_view : old_status:t -> new_status:t -> view_diff*)
