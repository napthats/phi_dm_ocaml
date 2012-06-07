open ExtList
open ExtHashtbl
open Item.Open


let ($) f g x = f (g x);;

(* chara_id, chara *)
let chara_tbl = Hashtbl.create 100;;

(* client_id, chara_id *)
let client_tbl = Hashtbl.create 100;;


let execute_event = function
    Event.Client_message (cid, Protocol.Raw_client_protocol (Protocol.Raw_message msg)) ->
    (* tentative *)
    Client_manager.send_message ~cid ~msg;
    []
  | Event.Client_message (cid, Protocol.Raw_client_protocol (Protocol.Go dir)) ->
    (match Hashtbl.find_all client_tbl cid with
        [chara_id] ->
          let chara = Hashtbl.find chara_tbl chara_id in
          chara#go ~dir
      | _ -> []
    )
  | Event.Client_message (cid, Protocol.Raw_client_protocol (Protocol.Turn maybe_dir)) ->
    (match Hashtbl.find_all client_tbl cid with
        [chara_id] ->
          (match maybe_dir with
              None -> 
                Client_manager.send_message ~cid ~msg:(Dm_message.make Dm_message.Turn_bad);
                []
            | Some dir ->
              let chara = Hashtbl.find chara_tbl chara_id in
              chara#turn ~dir
          )
      | _ -> []
    )
  | Event.Client_message (cid, Protocol.Raw_client_protocol Protocol.Hit) ->
    (match Hashtbl.find_all client_tbl cid with
        [chara_id] ->
          let chara = Hashtbl.find chara_tbl chara_id in
          chara#hit
      | _ -> []
    )     
  | Event.Client_message (cid, Protocol.Raw_client_protocol Protocol.Check) ->
    (match Hashtbl.find_all client_tbl cid with
        [chara_id] ->
          let chara = Hashtbl.find chara_tbl chara_id in
          Client_manager.send_message ~cid
            ~msg:(Dm_message.make (Dm_message.Pc_status(chara#get_name, chara#get_status_view, List.map (fun item -> Item.get_name ~item) chara#get_item_list)));
          chara#sight_update
      | _ -> []
    )    
  | Event.Client_message (cid, Protocol.Raw_client_protocol (Protocol.Get item)) ->
    (match Hashtbl.find_all client_tbl cid with
        [chara_id] ->
(*          let chara = Hashtbl.find chara_tbl chara_id in *)
          (* tentative *)
          let pos = Phi_map.get_chara_position ~chara_id in
          (match item with
              None ->
                (match Phi_map.get_item_list_with_position ~pos with
                    [] ->
                      Client_manager.send_message ~cid ~msg:(Dm_message.make Dm_message.No_item_here);
                      []
                  | item_list ->
                    ignore (List.map
                              (fun item -> Client_manager.send_message ~cid
                                ~msg:((Item.get_view ~item).name ^ ", "))
                              item_list);
                    []
                )
            | Some target_name ->
                (match Phi_map.get_item_list_with_position ~pos with
                    [] ->
                      Client_manager.send_message ~cid ~msg:(Dm_message.make Dm_message.Get_bad);
                      []
                  | item_list ->
                    (match
                        List.find_all (fun item -> (Item.get_view ~item).name = target_name) item_list
                     with
                         [] ->
                         Client_manager.send_message ~cid ~msg:(Dm_message.make Dm_message.Get_bad);
                         []
                       | item :: _ ->
                         let pc = Hashtbl.find chara_tbl chara_id in
                         pc#get_item ~item
                    )
                )
          )
      | _ -> []
    )    

  (* tentative: ignore duplicate open now *)
  | (Event.Client_message (cid, Protocol.Sharp_client_protocol (Protocol.Open phirc))) ->
    let chid = Chara_id.get_next_chara_id () in
    Hashtbl.replace client_tbl cid chid;
    (match Player_character.create ~phirc ~cid ~chid with
        None ->
        Client_manager.send_message ~cid ~msg:(Dm_message.make Dm_message.No_character);
        []
      | Some pc ->
        Hashtbl.replace chara_tbl chid pc;
        Chara_name_cache.set_name ~chid ~name:pc#get_name;
        Event.Position_change (chid, (None, Some (Phi_map.get_chara_position ~chara_id:chid)))
        :: pc#sight_update)
  | (Event.Client_message (_, Protocol.Sharp_client_protocol (Protocol.Unknown))) ->
    []

  | Event.Tick ->
    List.concat (List.map (fun chara -> chara#do_action) (List.of_enum (Hashtbl.values chara_tbl)))

  | Event.Npc_appear ->
    let chid = Chara_id.get_next_chara_id () in
    let npc = Non_player_character.create ~chid in
    Hashtbl.replace chara_tbl chid npc;
    Chara_name_cache.set_name ~chid ~name:npc#get_name;
    [Event.Position_change (chid, (None, Some (Phi_map.get_chara_position ~chara_id:chid)))]

  | Event.Attack_to (achid, (pos, combat)) ->
    let defense_chara_list = Phi_map.get_chara_list_with_position ~pos in
    List.concat
      (List.map (fun dchid-> (Hashtbl.find chara_tbl dchid)#defense ~combat ~achid) defense_chara_list)
  | Event.Attack_result ((achid, dchid), result_list) ->
    (match Hashtbl.find_all chara_tbl achid with
        [chara] ->
          chara#resolve_attack_result ~result_list ~dchid
      | _ -> []
    )

  | Event.Dead chid ->
    Hashtbl.remove chara_tbl chid;
    Phi_map.delete_chara ~chara_id:chid;
    []

  | (Event.Position_change (chid, (maybe_old_pos, maybe_new_pos))) ->
    if Hashtbl.find_all chara_tbl chid = []
    then []
    else
    let target_chara = Hashtbl.find chara_tbl chid in
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
      ((List.map (fun (chid, view_pos) -> (Hashtbl.find chara_tbl chid)#sight_change (Chara.Appear_chara (target_chara, view_pos))) chara_list_appear)
      @ (List.map (fun (chid, view_pos_pair) -> (Hashtbl.find chara_tbl chid)#sight_change (Chara.Move_chara (target_chara, view_pos_pair))) chara_list_move)
      @ (List.map (fun (chid, view_pos) -> (Hashtbl.find chara_tbl chid)#sight_change (Chara.Disappear_chara (target_chara, view_pos))) chara_list_disappear))
;;

let event_dispatch ~event_list =
  List.concat (List.map execute_event event_list)
;;

