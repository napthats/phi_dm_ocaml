open ExtList
open ExtHashtbl


let ($) f g x = f (g x);;


let execute_raw_client_protocol (cid, pc, protocol) =
  (match pc#get_command_st with
      Player_character.List_select select_type ->
      (match protocol with
          Protocol.Raw_message msg ->
          (try
             let f list succeed_action fail_msg =
               let ord = (int_of_string msg) - 1 in
               if ord >= 0 && ord < (List.length list)
               then succeed_action (List.nth list ord)
               else (Client_manager.send_message ~cid ~msg:fail_msg; [])
             in
             pc#set_command_st ~st:Player_character.Normal;
             (match select_type with
                 Player_character.Get ->
                 let pos = Phi_map.get_chara_position ~chara_id:pc#get_chara_id in
                 f (Phi_map.get_item_list_with_position ~pos)
                   (fun item -> pc#item_get ~item) (Dm_message.make Dm_message.Get_no)
               | Player_character.Use ->
                 f (List.map fst pc#get_item_list)
                   (fun item -> pc#use_item ~item) (Dm_message.make Dm_message.Use_no)
               | Player_character.Unequip ->
                 f (List.filter_map 
                      (fun (item, eflag) -> match eflag with
                          None -> None
                        | Some _ -> Some item) 
                      pc#get_item_list)
                   (fun item -> pc#unequip_item ~item) (Dm_message.make Dm_message.Unequip_no)
               | Player_character.Spell ->
                 f pc#get_spell_list
                   (fun spell -> pc#cast ~spell) (Dm_message.make Dm_message.Spell_no))
          with
              Failure "int_of_string" ->
                pc#set_command_st ~st:Player_character.Normal;
                Client_manager.send_message ~cid
                  ~msg:(Dm_message.make Dm_message.Cancel_list_select);
                []
          )
        | Protocol.Go _ | Protocol.Turn _ | Protocol.Hit
        | Protocol.Get _ | Protocol.Check | Protocol.Exit 
        | Protocol.Use _ | Protocol.Unequip | Protocol.Cast ->
          pc#set_command_st ~st:Player_character.Normal;
          Client_manager.send_message ~cid
            ~msg:(Dm_message.make Dm_message.Cancel_list_select);
          [])

    | Player_character.Cast _ ->
      Client_manager.send_message ~cid
        ~msg:(Dm_message.make Dm_message.Cast_stop);
      pc#set_command_st ~st:Player_character.Normal;
      []
      
    | Player_character.Normal ->
      (match protocol with
          Protocol.Raw_message msg ->
            pc#say ~msg
        | Protocol.Exit ->
          Client_manager.send_message ~cid ~msg:
            (Dm_message.make Dm_message.Savedata);
          Client_manager.send_message ~cid ~msg:
            (Dm_message.make Dm_message.Seeyou);
          Client_manager.send_message ~cid ~msg:" "; (* dummy *)
          Chara_data.remove_chara pc#get_chara_id
        | Protocol.Go dir ->
          pc#go ~dir
        | Protocol.Turn maybe_dir ->
          (match maybe_dir with
              None -> 
                Client_manager.send_message ~cid
                  ~msg:(Dm_message.make Dm_message.Turn_bad);
                []
            | Some dir ->
              pc#turn ~dir)
        | Protocol.Hit ->
          pc#hit
        | Protocol.Check ->
          Client_manager.send_message ~cid
            ~msg:(Dm_message.make (Dm_message.Pc_status(
              pc#get_name,
              pc#get_status_view,
              List.map
                (fun (item, equip_flag) -> (Item.get_name ~item, equip_flag))
                pc#get_item_list)));
          pc#sight_update

        | Protocol.Unequip ->
          (match List.filter
              (fun (_, eflag) ->
                match eflag with
                    None -> false
                  | Some _ -> true)
              pc#get_item_list
           with
              [] ->
                Client_manager.send_message ~cid ~msg:
                  (Dm_message.make Dm_message.Unequip_no);
                []
            | item_list ->
              Client_manager.send_message ~cid ~msg:
                (Dm_message.make (Dm_message.Item_list_with_equip
                                    (List.map 
                                       (fun (item,e)->((Item.get_view ~item).Item.name,e))
                                       item_list)));
              Client_manager.send_message ~cid ~msg:
                (Dm_message.make Dm_message.Unequip_select);
              pc#set_command_st
                ~st:(Player_character.List_select Player_character.Unequip);
              []
          )

        | Protocol.Cast ->
          (match pc#get_spell_list with
              [] ->
                Client_manager.send_message ~cid ~msg:
                  (Dm_message.make Dm_message.Spell_no);
                []
            | spell_list ->
              Client_manager.send_message ~cid ~msg:
                (Dm_message.make (Dm_message.Spell_list (List.map (fun spell -> Spell.get_name ~spell) spell_list)));
              Client_manager.send_message ~cid ~msg:
                (Dm_message.make Dm_message.Cast_select);
              pc#set_command_st
                ~st:(Player_character.List_select Player_character.Spell);
              [])

        | Protocol.Use item ->
          (match item with
              None ->
                (match (List.map fst pc#get_item_list) with
                    [] ->
                      Client_manager.send_message ~cid ~msg:
                        (Dm_message.make Dm_message.No_item_investory);
                      []
                  | item_list ->
                    Client_manager.send_message ~cid ~msg:
                      (Dm_message.make (Dm_message.Item_list 
                                          (List.map 
                                             (fun item -> (Item.get_view ~item).Item.name)
                                             item_list)));
                    Client_manager.send_message ~cid ~msg:
                      (Dm_message.make Dm_message.Use_select);
                    pc#set_command_st
                      ~st:(Player_character.List_select Player_character.Use);
                    []
                )                
            | Some target_name ->
              (match (List.map fst pc#get_item_list) with
                  [] ->
                    Client_manager.send_message ~cid ~msg:
                      (Dm_message.make Dm_message.No_item_investory);
                    []
                | item_list ->
                  (match
                      List.find_all 
                        (fun item -> (Item.get_view ~item).Item.name = target_name)
                        item_list
                   with
                       [] ->
                         Client_manager.send_message ~cid ~msg:
                           (Dm_message.make Dm_message.Use_bad);
                         []
                     | item :: _ ->
                       pc#use_item ~item
                  )
              )
          )

        | Protocol.Get item ->
          let chara_id = pc#get_chara_id in
          let pos = Phi_map.get_chara_position ~chara_id in
          (match item with
              None ->
                (match Phi_map.get_item_list_with_position ~pos with
                    [] ->
                      Client_manager.send_message ~cid ~msg:
                        (Dm_message.make Dm_message.No_item_here);
                      []
                  | item_list ->
                    Client_manager.send_message ~cid ~msg:
                      (Dm_message.make (Dm_message.Item_list 
                                          (List.map 
                                             (fun item -> (Item.get_view ~item).Item.name)
                                             item_list)));
                    Client_manager.send_message ~cid ~msg:
                      (Dm_message.make Dm_message.Get_select);
                    pc#set_command_st
                      ~st:(Player_character.List_select Player_character.Get);
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
                        (fun item -> (Item.get_view ~item).Item.name = target_name)
                        item_list
                   with
                       [] ->
                         Client_manager.send_message ~cid ~msg:
                           (Dm_message.make Dm_message.Get_bad);
                         []
                     | item :: _ ->
                       pc#item_get ~item
                  )
              )
          )
      )
  )

let execute_sharp_client_protocol (cid, protocol) = match protocol with
   (* tentative: ignore duplicate open now *)
  Protocol.Open phirc ->
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
              Client_manager.send_message ~cid
                ~msg:(Dm_message.make Dm_message.No_character);
              []
          | Some event_list ->
            event_list
        )
    )
  | Protocol.Unknown ->
    []

let execute_client_message (cid, client_message) =
  (match client_message with
      Protocol.Raw_client_protocol protocol ->
        (match Chara_data.get_pc_by_cid cid with
            None -> []
          | Some pc ->
            execute_raw_client_protocol (cid, pc, protocol))
    | Protocol.Sharp_client_protocol protocol ->
      execute_sharp_client_protocol (cid, protocol))

let execute_event = function
    Event.Client_message (cid, client_message) ->
      execute_client_message (cid, client_message)

  | Event.Tick ->
    List.concat (Chara_data.map (fun chara -> chara#do_action))

  | Event.Say (achid, msg) ->
    ignore 
      (List.map
         (fun (chid, _, _) -> 
           match Chara_data.get_chara chid with
               None -> assert false
             | Some chara -> chara#listen ~achid ~msg)
         (Phi_map.get_chara_in_sight_list ~chara_id:achid));
    []

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

