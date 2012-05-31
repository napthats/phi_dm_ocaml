open ExtList

let ($) f g x = f (g x);;

let pc_tbl = Hashtbl.create 100;;

let client_table = Hashtbl.create 100;;

let execute_event = function
    (Event.Client_message (cid, Protocol.Raw_client_protocol (Protocol.Raw_message msg))) ->
(* tentative *)
    Client_manager.send_message ~cid ~msg;
    []
  | (Event.Client_message (cid, Protocol.Raw_client_protocol (Protocol.Go dir))) ->
    (match Hashtbl.find_all client_table cid with
        [chara_id] ->
          let chara = Hashtbl.find pc_tbl chara_id in
          chara#go ~dir
      | _ -> []
    )
  | (Event.Client_message (cid, Protocol.Raw_client_protocol (Protocol.Turn maybe_dir))) ->
    (match Hashtbl.find_all client_table cid with
        [chara_id] ->
          (match maybe_dir with
              None -> []
            | Some dir ->
              let chara = Hashtbl.find pc_tbl chara_id in
              chara#turn ~dir
          )
      | _ -> []
    )

  | (Event.Client_message (cid, Protocol.Sharp_client_protocol (Protocol.Open phirc))) ->
    let chid = Chara_id.get_next_chara_id () in
    Hashtbl.replace client_table cid chid;
    let pc = Player_character.create ~phirc ~cid ~chid in
    Hashtbl.replace pc_tbl chid pc;
    [Event.Position_change (chid, (None, Some (Phi_map.get_chara_position ~chara_id:chid)))]
  | (Event.Client_message (_, Protocol.Sharp_client_protocol (Protocol.Unknown))) ->
    [] (* tentative *)

  | (Event.Position_change (chid, (maybe_old_pos, maybe_new_pos))) ->
    let target_chara = Hashtbl.find pc_tbl chid in
    let cansee_old_chara_list =
      match maybe_old_pos with
          Some old_pos ->
            List.filter (fun (id, _) -> chid <> id) (Phi_map.get_cansee_chara_list ~pos:old_pos)
        | None -> []
    in
    let cansee_new_chara_list =
      match maybe_new_pos with
          Some new_pos -> 
            List.filter (fun (id, _) -> chid <> id) (Phi_map.get_cansee_chara_list ~pos:new_pos)
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

