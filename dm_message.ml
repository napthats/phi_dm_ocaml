open Chara_status.Open
open ExtString


type msg_type =
    Go_no
  | Turn_bad
  | Attack_hp of (string * string * string * int)
  | Kill_by of (string * string)
  | No_item_here
  | Get_bad
  | Get_no
  | Cancel_list_select
  | Get of (string * string)
  | Get_select
  | Item_list of string list
  | Item_list_with_equip of (string * Chara.equip_flag option) list
  | Use_select
  | Use of (string * string)
  | Equip of (string * string)
  | Unequip_no
  | Unequip_select
  | Unequip of (string * string)
  | Put_bad
  | Use_bad
  | Use_no
  | No_item_investory
  | No_character
  | Pc_status of
      (string * Chara_status.view * (string * Chara.equip_flag option) list)
  | Access_already
  | Change_client_fail
  | Savedata
  | Seeyou
  | Try_again
  | Dead


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
  | Use_no -> "DM > Can not use."
  | Cancel_list_select -> "DM > Cancel list selecting."
  | Get (chara_name, item_name) -> "DM > " ^ chara_name ^ " gets " ^ item_name ^ "."
  | No_character -> " Your character is not here."
  | Access_already -> " You accessed already."
  | Change_client_fail -> " Changing client is failed."
  | Dead -> "DM > You are dead... \n"
  | Try_again -> "  Try again.  \n"
  | Savedata -> "  Saving data..  "
  | Seeyou -> "  See you next time.  "
  | Get_select -> "DM > Input item number to get."
  | Use_select -> "DM > Input item number to use."
  | Put_bad -> "DM > You do not have such a thing."
  | Use_bad -> "DM > You do not have such a thing."
  | No_item_investory -> "DM > you do not have any items."
  | Use (name, item_name) -> "DM > " ^ name ^ " used " ^ item_name ^ "."
  | Equip (name, item_name) -> "DM > " ^ name ^ " equip " ^ item_name ^ "."
  | Unequip (name, item_name) -> "DM > " ^ name ^ " unequiped " ^ item_name ^ "."
  | Unequip_no -> "DM > Can not unequip."
  | Unequip_select -> "DM > Input item number to unequip."
  | Item_list name_list ->
    String.rchop 
      (snd 
         (List.fold_left
            (fun (ord, acc) name ->
              (ord+1, acc ^ (Printf.sprintf "[%2d] %s" ord name) ^ "\n"))
            (1, "")
            name_list))
  | Item_list_with_equip name_list ->
    let equip_flag_to_string = function
        Some Chara.Wpn -> " [/*color=red*/Wpn/*.*/]"
      | Some Chara.Arm -> " [/*color=+hp*/Arm/*.*/]"
      | Some Chara.Acr -> " [/*color=blue*/Acr/*.*/]"
      | None -> ""
    in
    let (_, item_string) =
      List.fold_left
        (fun (ord, acc) (item_name, eflag) -> (ord + 1, acc ^
          Printf.sprintf "     [/*color=cyan*/%2d/*.*/] %-30s Wp  :%2d%s\n" ord item_name 0 (equip_flag_to_string eflag)
         ))
        (1, "")
        name_list
    in
    item_string
  | Pc_status (pc_name, v, item_name_list) ->
    let equip_flag_to_string = function
        Some Chara.Wpn -> " [/*color=red*/Wpn/*.*/]"
      | Some Chara.Arm -> " [/*color=+hp*/Arm/*.*/]"
      | Some Chara.Acr -> " [/*color=blue*/Acr/*.*/]"
      | None -> ""
    in
    let status_string = Printf.sprintf " /*color=+mp*/Name/*.*/  : %s\n   /*color=cyan*/Hp/*.*/  : /*color=+hp*/%4d/*.*/ / %4d       /*color=cyan*/Mp/*.*/: /*color=+mp*/%4d/*.*/ / %4d\n   /*color=cyan*/Exp/*.*/ : %-8d          /*color=cyan*/Gp/*.*/: %-4d\n   /*color=cyan*/Land/*.*/: %s\n   /*color=chan*/Area/*.*/: %s\n /*color=+mp*/Level/*.*/ :\n    /*color=cyan*/Fighter/*.*/  %4d    /*color=cyan*/Wizard/*.*/   %4d\n    /*color=cyan*/Merchant/*.*/ %4d    /*color=cyan*/Creater/*.*/  %4d\n /*color=+mp*/Items :\n" pc_name v.hp v.mhp v.mp v.mmp 0 0 "dummy land" "dummy area" v.flv v.wlv v.mlv v.clv in
    let (_, item_string) =
      List.fold_left
        (fun (ord, acc) (item_name, eflag) -> (ord + 1, acc ^
          Printf.sprintf "     [/*color=cyan*/%2d/*.*/] %-30s Wp  :%2d%s\n" ord item_name 0 (equip_flag_to_string eflag)
         ))
        (1, "")
        item_name_list
    in
    status_string ^ item_string
;;
