open ExtList

let ($) f g x = f (g x);;

let pc_tbl = Hashtbl.create 100;;

let client_table = Hashtbl.create 100;;

let execute_event = function
    (Event.Client_message (cid, Protocol.Raw_client_protocol (Protocol.Raw_message msg))) ->
(* tentative *)
    Client_manager.send_message ~cid ~msg;
    []
  | (Event.Client_message (cid, Protocol.Sharp_client_protocol (Protocol.Open phirc))) ->
    let pc = Player_character.create ~phirc ~cid in
    let chid = Chara_id.get_next_chara_id () in
    Hashtbl.add pc_tbl chid pc;
    Hashtbl.add client_table cid chid;
    Phi_map.set_chara_position ~chara_id:chid ~pos:(pc#get_position);
    [Event.Position_change (chid, (None, Some pc#get_position))]
  | (Event.Client_message (_, Protocol.Sharp_client_protocol (Protocol.Unknown))) ->
    [] (* tentative *)
  | (Event.Position_change (chid, (maybe_old_pos, maybe_new_pos))) ->
    let target_chara = Hashtbl.find pc_tbl chid in
    let cansee_old_chara_list =
      match maybe_old_pos with
          Some old_pos -> Phi_map.get_cansee_chara_list ~pos:old_pos
        | None -> []
    in
    let cansee_new_chara_list =
      match maybe_new_pos with
          Some new_pos -> Phi_map.get_cansee_chara_list ~pos:new_pos
        | None -> []
    in
    let chara_list_appear =
      List.filter (fun (chid_new, _) -> not (List.exists (fun (chid_old, _) -> chid_new = chid_old) cansee_old_chara_list)) cansee_new_chara_list
    in
    let chara_list_move =
      List.filter_map
        (fun (chid_new, pos_new) ->
          (if List.exists (fun (chid_old, _) -> chid_new = chid_old) cansee_old_chara_list
           then Some (List.find_map
                        (fun (_, pos_old) -> Some (chid_new, (pos_old, pos_new)))
                        cansee_old_chara_list)
           else None)
        )
        cansee_new_chara_list
    in
    let chara_list_disappear =
      List.filter (fun (chid_old, _) -> not (List.exists (fun (chid_new, _) -> chid_new = chid_old) cansee_new_chara_list)) cansee_old_chara_list
    in
    List.concat 
      ((List.map (fun (chid, view_pos) -> (Hashtbl.find pc_tbl chid)#sight_change (Chara.Appear_chara (target_chara, view_pos))) chara_list_appear)
      @ (List.map (fun (chid, view_pos_pair) -> (Hashtbl.find pc_tbl chid)#sight_change (Chara.Move_chara (target_chara, view_pos_pair))) chara_list_move)
      @ (List.map (fun (chid, view_pos) -> (Hashtbl.find pc_tbl chid)#sight_change (Chara.Disappear_chara (target_chara, view_pos))) chara_list_disappear))
;;

let event_dispatch ~event_list =
  List.concat (List.map execute_event event_list)
;;

