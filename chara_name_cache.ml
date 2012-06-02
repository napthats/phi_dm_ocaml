(* chara_id, string *)
(* data is kept in some frames *)
let name_tbl : (Chara_id.t, string) Hashtbl.t = Hashtbl.create 100;;

let get_name ~chid =
  match Hashtbl.find_all name_tbl chid with
      [name] -> Some name
    | _ -> None
;;

let set_name ~chid ~name = Hashtbl.replace name_tbl chid name;;
