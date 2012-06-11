open ExtList
open ExtHashtbl
open Item.Open


let ($) f g x = f (g x);;


module Chara_data :
sig
  val get_chara : Chara_id.t -> Chara.t option
  val get_pc_by_cid : Tcp_server.client_id -> Player_character.t option
  val get_pc_by_phirc : string -> Player_character.t option
  val get_chara_id_by_cid : Tcp_server.client_id -> Chara_id.t option
  val create_npc : unit -> Event.t list
  val create_pc : Tcp_server.client_id -> string -> (Event.t list) option
  val remove_chara : Chara_id.t -> Event.t list
  val map : (Chara.t -> 'a) -> 'a list
end
=
struct
  (* chara_id, chara *)
  let npc_tbl = Hashtbl.create 100
  let pc_tbl = Hashtbl.create 100

  (* client_id, chara_id *)
  let client_tbl = Hashtbl.create 100
  let client_rev_tbl = Hashtbl.create 100

  let map f =
    let pc_list =
      List.map
        (fun (_, pc) -> (pc : Player_character.t :> Chara.t))
        (List.of_enum (Hashtbl.enum pc_tbl))
    in
    let npc_list =
      List.map
        (fun (_, npc) -> (npc : Non_player_character.t :> Chara.t))
        (List.of_enum (Hashtbl.enum npc_tbl))
    in
    List.map f (List.append pc_list npc_list)


  let get_chara_id_by_cid cid =
    (match Hashtbl.find_all client_tbl cid with
        [chara_id] -> Some chara_id
        | _ -> None)

  let get_chara chid =
    (match Hashtbl.find_all npc_tbl chid with
        [npc] -> Some npc
      | _ ->
          (match Hashtbl.find_all pc_tbl chid with
              [pc] -> Some (pc : Player_character.t :> Chara.t)
            | _ -> None
          )
    )

  let get_pc_by_cid cid =
    (match Hashtbl.find_all client_tbl cid with
        [chid] ->
        Some (Hashtbl.find pc_tbl chid)
      | _ ->
        None
    )

  let get_pc_by_phirc phirc =
    (match List.filter
        (fun pc -> pc#get_phirc = phirc)
        (List.map snd (List.of_enum (Hashtbl.enum pc_tbl)))
     with
         [] -> None
       | pc :: _ -> Some pc
    )

  let create_npc () = 
    let chid = Chara_id.get_next_chara_id () in
    let npc = Non_player_character.create ~chid in
    Hashtbl.replace npc_tbl chid npc;
    Event.Position_change (chid, (None, Some (Phi_map.get_chara_position
                                                ~chara_id:chid)))
    :: npc#sight_update


  let create_pc cid phirc =
    let chid = Chara_id.get_next_chara_id () in
    (match Player_character.create ~phirc ~cid ~chid with
        None -> None
      | Some pc ->
        Hashtbl.replace pc_tbl chid pc;
        Hashtbl.replace client_tbl cid chid;
        Hashtbl.replace client_rev_tbl chid cid;
        Chara_name_cache.set_name ~chid ~name:pc#get_name;
        Some (
          Event.Position_change (chid, (None, Some (Phi_map.get_chara_position
                                                      ~chara_id:chid)))
          :: pc#sight_update
        )
    )

  let remove_chara chid =
    (match Hashtbl.find_all npc_tbl chid with
        [_] ->
        Hashtbl.remove npc_tbl chid;
        let pos = Phi_map.get_chara_position ~chara_id:chid in
        Phi_map.delete_chara ~chara_id:chid;
        [Event.Position_change (chid, (Some pos, None))]
      | _ ->
        (match Hashtbl.find_all client_rev_tbl chid with
            [cid] ->
              let pc = Hashtbl.find pc_tbl chid in
              let pos = Phi_map.get_chara_position ~chara_id:chid in
              let adir = Phi_map.get_chara_absolute_direction ~chara_id:chid in
              Player_character_db.save 
                ~name:pc#get_name
                ~phirc:pc#get_phirc
                ~pos
                ~adir
                ~status:pc#get_status_view
                ~item_list:pc#get_item_list;
              Phi_map.delete_chara ~chara_id:chid;
              Client_manager.disconnect ~cid;
              Hashtbl.remove client_tbl cid;
              Hashtbl.remove client_rev_tbl chid;
              Hashtbl.remove pc_tbl chid;
              [Event.Position_change (chid, (Some pos, None))]
          | _ -> []
        )
    )
end


let execute_event = function
    Event.Client_message (cid, Protocol.Raw_client_protocol (Protocol.Raw_message msg)) ->
    (* tentative *)
    Client_manager.send_message ~cid ~msg;
    []
  | Event.Client_message (cid, Protocol.Raw_client_protocol Protocol.Exit) ->
    (match Chara_data.get_chara_id_by_cid cid with
        None -> []
      | Some chara_id ->
        Client_manager.send_message ~cid ~msg:
          (Dm_message.make Dm_message.Savedata);
        Client_manager.send_message ~cid ~msg:
          (Dm_message.make Dm_message.Seeyou);
        Chara_data.remove_chara chara_id
    )
  | Event.Client_message (cid, Protocol.Raw_client_protocol (Protocol.Go dir)) ->
    (match Chara_data.get_pc_by_cid cid with
        None -> []
      | Some pc -> pc#go ~dir
    )
  | Event.Client_message (cid, Protocol.Raw_client_protocol (Protocol.Turn maybe_dir)) ->
    (match Chara_data.get_pc_by_cid cid with
        None -> []
      | Some pc ->
        (match maybe_dir with
            None -> 
              Client_manager.send_message ~cid
                ~msg:(Dm_message.make Dm_message.Turn_bad);
              []
          | Some dir ->
            pc#turn ~dir
        )
    )
  | Event.Client_message (cid, Protocol.Raw_client_protocol Protocol.Hit) ->
    (match Chara_data.get_pc_by_cid cid with
        None -> []
      | Some pc -> pc#hit
    )
  | Event.Client_message (cid, Protocol.Raw_client_protocol Protocol.Check) ->
    (match Chara_data.get_pc_by_cid cid with
        None -> []
      | Some pc ->
        Client_manager.send_message ~cid
          ~msg:(
            Dm_message.make (
              Dm_message.Pc_status(
                pc#get_name,
                pc#get_status_view,
                List.map (fun item -> Item.get_name ~item) pc#get_item_list
              )
            )
          );
        pc#sight_update
    )
  | Event.Client_message (cid, Protocol.Raw_client_protocol (Protocol.Get item)) ->
    (match Chara_data.get_chara_id_by_cid cid with
        None -> []
      | Some chara_id ->
        (*          let chara = Hashtbl.find chara_tbl chara_id in *)
        (* tentative *)
        let pos = Phi_map.get_chara_position ~chara_id in
        (match item with
            None ->
              (match Phi_map.get_item_list_with_position ~pos with
                  [] ->
                    Client_manager.send_message ~cid ~msg:
                      (Dm_message.make Dm_message.No_item_here);
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
                  Client_manager.send_message ~cid ~msg:
                    (Dm_message.make Dm_message.Get_bad);
                  []
              | item_list ->
                (match
                    List.find_all 
                      (fun item -> (Item.get_view ~item).name = target_name)
                      item_list
                 with
                     [] ->
                       Client_manager.send_message ~cid ~msg:
                         (Dm_message.make Dm_message.Get_bad);
                       []
                   | item :: _ ->
                     (match Chara_data.get_pc_by_cid cid with
                         None -> []
                       | Some pc -> pc#item_get ~item
                     )
                )
            )
        )
    )

  (* tentative: ignore duplicate open now *)
  | (Event.Client_message (cid, Protocol.Sharp_client_protocol (Protocol.Open phirc))) ->
    (match Chara_data.get_pc_by_phirc phirc with
        Some _ ->
          Client_manager.send_message ~cid ~msg:
            (Dm_message.make Dm_message.Access_already);
          Client_manager.send_message ~cid ~msg:
            (Dm_message.make Dm_message.Change_client_fail);  
          []
      | None ->
        (match Chara_data.create_pc cid phirc with
            None ->
              Client_manager.send_message ~cid ~msg:(Dm_message.make Dm_message.No_character);
              []
          | Some event_list ->
            event_list
        )
    )
  | (Event.Client_message (_, Protocol.Sharp_client_protocol (Protocol.Unknown))) ->
    []

  | Event.Tick ->
    List.concat (Chara_data.map (fun chara -> chara#do_action))

  | Event.Npc_appear ->
    Chara_data.create_npc ()

  | Event.Attack_to (achid, (pos, combat)) ->
    let defense_chara_id_list = Phi_map.get_chara_list_with_position ~pos in
    let defense_chara_list =
      List.filter_map Chara_data.get_chara defense_chara_id_list
    in
    List.concat (
      List.map 
        (fun chara -> chara#defense ~combat ~achid)
        defense_chara_list
    )

  | Event.Attack_result ((achid, dchid), result_list) ->
    (match Chara_data.get_chara achid with
        None -> []
      | Some chara -> chara#resolve_attack_result ~result_list ~dchid
    )

  | Event.Dead chid ->
    (match Chara_data.get_chara chid with
        None -> []
      | Some chara ->
        let dead_event_list = chara#dead in
        List.append dead_event_list (Chara_data.remove_chara chid)
    )

  | (Event.Position_change (chid, (maybe_old_pos, maybe_new_pos))) ->
    (match Chara_data.get_chara chid with
        None -> []
      | Some target_chara ->
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
    let chara_id_list_appear =
      List.filter (fun (chid_new, _) -> not (List.exists (fun (chid_old, _) -> chid_new = chid_old) cansee_old_chara_list)) cansee_new_chara_list
    in
    let chara_id_list_move =
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
    let chara_id_list_disappear =
      List.filter (fun (chid_old, _) -> not (List.exists (fun (chid_new, _) -> chid_new = chid_old) cansee_new_chara_list)) cansee_old_chara_list
    in
    let chara_list_appear =
      List.filter_map
        (fun (chid, x) ->
          match Chara_data.get_chara chid with
              None -> None
            | Some chara -> Some (chara, x))
        chara_id_list_appear
    in
    let chara_list_move =
      List.filter_map
        (fun (chid, x) ->
          match Chara_data.get_chara chid with
              None -> None
            | Some chara -> Some (chara, x))
        chara_id_list_move
    in
    let chara_list_disappear =
      List.filter_map
        (fun (chid, x) ->
          match Chara_data.get_chara chid with
              None -> None
            | Some chara -> Some (chara, x))
        chara_id_list_disappear
    in
    List.concat 
      ((List.map (fun (chara, view_pos) -> chara#sight_change (Chara.Appear_chara (target_chara, view_pos))) chara_list_appear)
      @ (List.map (fun (chara, view_pos_pair) -> chara#sight_change (Chara.Move_chara (target_chara, view_pos_pair))) chara_list_move)
      @ (List.map (fun (chara, view_pos) -> chara#sight_change (Chara.Disappear_chara (target_chara, view_pos))) chara_list_disappear))
    )

let event_dispatch ~event_list =
  List.concat (List.map execute_event event_list)
;;

