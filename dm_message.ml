open Chara_status


type msg_type =
    Go_no
  | Turn_bad
  | Attack_hp of (string * string * string * int)
  | Kill_by of (string * string)
  | No_item_here
  | Get_bad
  | Get_no
  | Get of (string * string)
  | Pc_status of (string * Chara_status.view * string list)
;;

let make = function
    Go_no -> "DM > Can not go."
  | Turn_bad -> "DM > Which direction? Type 'turn ????'."
  | Attack_hp (aname, dname, weapon, value) ->
    "DM > " ^ aname ^ " attacked to " ^ dname ^ " by " ^ weapon
    ^ ". [/*color=-hp*/" ^ (string_of_int value) ^ "/*.*/ hp]"
  | Kill_by (aname, dname) ->
    "DM > " ^ dname ^ " is killed by " ^ aname ^ "."
  | No_item_here -> "DM > Here is no item."
  | Get_bad -> "DM > Can not get such a thing."
  | Get_no -> "DM > Can not get."
  | Get (chara_name, item_name) -> "DM > " ^ chara_name ^ " gets " ^ item_name ^ "."
  | Pc_status (pc_name, v, item_name_list) ->
    let status_string = Printf.sprintf " /*color=+mp*/Name/*.*/  : %s\n   /*color=cyan*/Hp/*.*/  : /*color=+hp*/%4d/*.*/ / %4d       /*color=cyan*/Mp/*.*/: /*color=+mp*/%4d/*.*/ / %4d\n   /*color=cyan*/Exp/*.*/ : %-8d          /*color=cyan*/Gp/*.*/: %-4d\n   /*color=cyan*/Land/*.*/: %s\n   /*color=chan*/Area/*.*/: %s\n /*color=+mp*/Level/*.*/ :\n    /*color=cyan*/Fighter/*.*/  %4d    /*color=cyan*/Wizard/*.*/   %4d\n    /*color=cyan*/Merchant/*.*/ %4d    /*color=cyan*/Creater/*.*/  %4d\n /*color=+mp*/Items :\n" pc_name v.hp v.mhp v.mp v.mmp 0 0 "dummy land" "dummy area" v.flv v.wlv v.mlv v.clv in
    let (_, item_string) =
      List.fold_left
        (fun (ord, acc) item_name -> (ord + 1, acc ^
          Printf.sprintf "     [/*color=cyan*/%2d/*.*/] %-30s Wp  :%2d\n" ord item_name 0
         ))
        (1, "")
        item_name_list
    in
    status_string ^ item_string
;;
