type t = int;;

let current_chara_id = ref 0;;

let get_next_chara_id () =
  let cid = !current_chara_id in
  current_chara_id := !current_chara_id + 1;
  cid
;;
