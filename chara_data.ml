open ExtList
open ExtHashtbl
open Item.Open


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
    
