type state = Command;;

type condition = Dummy;;

type view = 
    {hp : int; mhp : int; mp : int; mmp : int;
     flv : int; wlv : int; mlv : int; clv : int;
     state : state; condition : condition list}
;;

type view_diff = 
    {dhp : int; dmhp : int; dmp : int; dmmp : int;
     dflv : int; dwlv : int; dmlv : int; dclv : int;
     state_change : bool;
     deleting_condition : condition list; adding_condition : condition list}
;;

type t = view;;


let create ~view = view;;

let get_view ~status = status;;

let add_hp ~hp:old_hp ~status =
  {status with hp = status.hp + old_hp}
;;

let is_dead ~status = status.hp <= 0 || status.mp <= 0;;

let get_status_diff_view ~old_status ~new_status = 
  let oview = get_view ~status:old_status in
  let nview = get_view ~status:new_status in
  {dhp = nview.hp - oview.hp; dmhp = nview.mhp - oview.mhp;
   dmp = nview.mp - oview.mp; dmmp = nview.mmp - oview.mmp;
   dflv = nview.flv - oview.flv; dwlv = nview.wlv - oview.wlv;
   dmlv = nview.mlv - oview.mlv; dclv = nview.clv - oview.clv;
   state_change = false;
   deleting_condition = []; adding_condition = []}
;;
