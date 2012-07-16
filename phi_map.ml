open ExtHashtbl
open ExtList
open ExtString
open Phi_map_data




let ($) f g x = f (g x)

type position = {px : int; py : int}

type view_position = Phi_map_data.view_position

type absolute_direction = Phi_map_data.absolute_direction

type relative_direction = Phi_map_data.relative_direction

type direction = Phi_map_data.direction

type mapchip = Phi_map_data.mapchip

type flooritem = Phi_map_data.flooritem

type view = Phi_map_data.view

module Open = struct
module Event = struct
type t =
    Client_message of (Tcp_server.client_id * Protocol.client_protocol) 
  | Position_change of (Chara_id.t * (position option * position option))
  | Npc_appear
  | Tick
  | Attack_to of (Chara_id.t * (position * Combat.t))
  | Attack_result of ((Chara_id.t * Chara_id.t) * Combat.result list)
  | Dead of Chara_id.t
  | Say of (Chara_id.t * string)
  | Switch_move of (Chara_id.t * position)
  | Switch_list of (Chara_id.t * string list)
  | Switch_select_done of (Chara_id.t * int * position)
(*  | Status_view_change of (Chara_manager.chara_id * Phi_map.position
                      * (Chara_manager.chara_status_view * Chara_manager.chara_status_view))
  | Time_tick of int *)
end
end

include Open


module Switch =
struct
  type condition = List_select_done of int
  type result = Move of position | List_select of string list
  type t = (condition list * result)
end


(* all length of arrays are same *)
module Map_loader :
sig
  val load : unit -> mapchip array array
end
=
struct
  let char_to_mapchip = function
      ' ' -> Road
    | 'o' -> Dummy
    | ':' -> Grass
    | '+' -> Flower
    | '_' -> Water
    | 'x' -> Pcircle
    | '/' -> Mist
    | '>' -> Tgate
    | '[' -> Door
    | 'I' -> Bars
    | '%' -> Pcircle_lock
    | '|' -> Window
    | 'T' -> Wood
    | '=' -> Glass
    | 'H' -> Mwall
    | '{' -> Door_lock
    | '#' -> Mwall
    | '@' -> Rock
    | '?' -> Unknown      
    | x -> failwith ("phi.dd: illigal mapchip " ^ (Char.escaped x))

  let load () =
    let file_chan = open_in "phi.dd" in
    let file = IO.input_channel file_chan in
    let data = IO.read_all file in
    let get_odd_list list =
      fst (List.fold_left (fun (acc, cond) elem -> 
        if cond 
        then (List.append acc [elem], not cond) 
        else (acc, not cond)
      ) ([], true) list)
    in
    let split_data = String.nsplit data "\n" in
    let char_list = List.map String.explode split_data in
    let char_list_odd = List.map get_odd_list char_list in
    let chip_list = List.map (List.map char_to_mapchip) char_list_odd in
    close_in file_chan;
    Array.of_list (List.map Array.of_list chip_list)
end


(* throw exception if posistion is invalid *)
module Map_data :
sig
  val get_chip_view : position -> mapchip
  val set_chip_view : position -> mapchip -> unit
  val get_chara_list : position -> Chara_id.t list
  val set_chara_list : position -> Chara_id.t list -> unit
  val get_item_list : position -> Item.t list
  val set_item_list : position -> Item.t list -> unit
  val get_switch_list : position -> Switch.t list
  val set_switch_list : position -> Switch.t list -> unit
  val is_valid_pos : position -> bool
end
=
struct
  let item_debug =
      Item.create ~view:(
        {Item.name = "test item 1"; Item.attack_range = Item.Forth; Item.material = Item.Steel; Item.weapon_type = Item.Sword; Item.atp = 10; Item.item_type =Item.Weapon {Item.element = Item.Fire; Item.er = 30; Item.effect = Item.EFNone; Item.special_effect = Item.SENone}})

  let switch_debug_move = ([], (Switch.Move {px=4;py=6}))
  let switch_debug_list = ([], (Switch.List_select ["test gate 1"; "test gate 2"; "re-select"]))
  let switch_debug_select1 = ([Switch.List_select_done 1], (Switch.Move {px=5;py=5}))
  let switch_debug_select2 = ([Switch.List_select_done 2], (Switch.Move {px=0;py=6}))
  let switch_debug_select3 = ([Switch.List_select_done 3], (Switch.List_select ["test gate 1"; "test gate 2"; "re-select"]))
    
  let data = Array.map (Array.map (fun chip -> (chip, ([], [], [])))) (Map_loader.load ())

  let get_chip_view pos = fst data.(pos.py).(pos.px)
  let set_chip_view pos chip_view = 
    let (_, lists) = data.(pos.py).(pos.px) in
    data.(pos.py).(pos.px) <- (chip_view, lists)
  
  let get_chara_list pos =
    match data.(pos.py).(pos.px) with
        (_, (chara_list, _, _)) -> chara_list
  let set_chara_list pos chara_list = 
    let (cv, (_, item_list, switch_list)) = data.(pos.py).(pos.px) in
    data.(pos.py).(pos.px) <- (cv, (chara_list, item_list, switch_list))
  
  let get_item_list pos =
    match data.(pos.py).(pos.px) with
        (_, (_, item_list, _)) -> item_list
  let set_item_list pos item_list = 
    let (cv, (chara_list, _, switch_list)) = data.(pos.py).(pos.px) in
    data.(pos.py).(pos.px) <- (cv, (chara_list, item_list, switch_list))

  let get_switch_list pos =
    match data.(pos.py).(pos.px) with
        (_, (_, _, switch_list)) -> switch_list
  let set_switch_list pos switch_list = 
    let (cv, (chara_list, item_list, _)) = data.(pos.py).(pos.px) in
    data.(pos.py).(pos.px) <- (cv, (chara_list, item_list, switch_list))

  let is_valid_pos pos =
    pos.px >= 0 && pos.px < Array.length data.(0) && pos.py >= 0 && pos.py < Array.length data

  (* tentative: initialize mapdata *)
  let _ =
    set_switch_list {px=0;py=0} [switch_debug_list; switch_debug_select1; switch_debug_select2; switch_debug_select3];
    set_switch_list {px=2;py=6} [switch_debug_move];
    set_item_list {px=3;py=4} [item_debug]
end


let charaid_pos_tbl = Hashtbl.create 100

let charaid_dir_tbl = Hashtbl.create 100


let get_default_position = {px = 0; py = 0}

let set_chara_position ~chara_id ~pos =
  (match Hashtbl.find_all charaid_pos_tbl chara_id with
      [] -> ()
    | old_pos :: _ ->
      let chara_list = Map_data.get_chara_list old_pos in
      Map_data.set_chara_list old_pos (List.remove chara_list chara_id));
  let chara_list = Map_data.get_chara_list pos in
  Map_data.set_chara_list pos (chara_id :: chara_list);
  Hashtbl.replace charaid_pos_tbl chara_id pos
;;

let turn_absolute_direction ~adir ~rdir =
  match (adir, rdir) with
      (adir, Forth) -> adir
    | (North, Right) -> East
    | (North, Left) -> West
    | (North, Back) -> South
    | (East, Right) -> South
    | (East, Left) -> North
    | (East, Back) -> West
    | (West, Right) -> North
    | (West, Left) -> South
    | (West, Back) -> East
    | (South, Right) -> West
    | (South, Left) -> East
    | (South, Back) -> North


let set_chara_direction ~chara_id ~adir =
  Hashtbl.replace charaid_dir_tbl chara_id adir


let adir_to_offset = function
    North -> (0, -1)
  | East -> (1, 0)
  | West -> (-1, 0)
  | South -> (0, 1)



let get_neighbor_position ~pos ~adir =
  let offset = adir_to_offset adir in
  let new_pos = {px = pos.px + (fst offset); py = pos.py + (snd offset)} in
  if Map_data.is_valid_pos new_pos
  then Some new_pos
  else None


let get_chara_position ~chara_id =
  match Hashtbl.find_all charaid_pos_tbl chara_id with
      [] -> assert false
    | [pos] -> pos
    | _ -> assert false


let get_chara_absolute_direction ~chara_id =
  match Hashtbl.find_all charaid_dir_tbl chara_id with
      [] -> assert false
    | [dir] -> dir
    | _ -> assert false


let normal_sight_offset =
  [[(-3, -4); (-2, -4); (-1, -4); (0, -4); (1, -4); (2, -4); (3, -4)]
  ;[(-3, -3); (-2, -3); (-1, -3); (0, -3); (1, -3); (2, -3); (3, -3)]
  ;[(-3, -2); (-2, -2); (-1, -2); (0, -2); (1, -2); (2, -2); (3, -2)]
  ;[(-3, -1); (-2, -1); (-1, -1); (0, -1); (1, -1); (2, -1); (3, -1)]
  ;[(-3, 0); (-2, 0); (-1, 0); (0, 0); (1, 0); (2, 0); (3, 0)]
  ;[(-3, 1); (-2, 1); (-1, 1); (0, 1); (1, 1); (2, 1); (3, 1)]
  ;[(-3, 2); (-2, 2); (-1, 2); (0, 2); (1, 2); (2, 2); (3, 2)]]


let get_normal_sight_offset = function
    North -> normal_sight_offset
  | East -> List.map (List.rev $ (List.map (fun (x, y) -> (-y, -x)))) normal_sight_offset
  | West -> List.map (List.rev $ (List.map (fun (x, y) -> (y, x)))) normal_sight_offset
  | South -> List.map (List.map (fun (x, y) -> (-x, -y))) normal_sight_offset

let is_transparent = function
   Tgate | Pcircle | Road | Bars | Door | Dummy | Flower | Glass | Grass | Water | Window | Wood | Pcircle_lock -> true
  | Mist | Mwall | Rock | Unknown | Wwall | Door_lock -> false

let cansee_check_ord = [(0, 0); (0, -1); (1, 0); (0, 1); (-1, 0); (0, -2); (1, -1); (2, 0); (1, 1); (0, 2); (-1, 1); (-2, 0); (-1, -1); (0, -3); (1, -2); (2, -1); (3, 0); (2, 1); (1, 2); (-1, 2); (-2, 1); (-3, 0); (-2, -1); (-1, -2); (0, -4); (1, -3); (2, -2); (3, -1); (3, 1); (2, 2); (-2, 2); (-3, 1); (-3, -1); (-2, -2); (-1, -3); (1, -4); (2, -3); (3, -2); (3, 2); (-3, 2); (-3, -2); (-2, -3); (-1, -4); (2, -4); (3, -3); (-3, -3); (-2, -4); (3, -4); (-3, -4)]

let get_mapview ~chara_id =
  let dir = Hashtbl.find charaid_dir_tbl chara_id in
  let pos = Hashtbl.find charaid_pos_tbl chara_id in
  let all_view : view list list =
    List.map
      (List.map (fun (ox, oy) ->
        let p = {px = pos.px + ox; py = pos.py + oy} in
        if Map_data.is_valid_pos p
        then 
          {chip=Map_data.get_chip_view p;
           item = (match Map_data.get_item_list p with
               [] -> None
             | _ -> Some Normal)}
        else {chip=Unknown; item=None}))
      (get_normal_sight_offset dir)
  in
  let view : view list list =
    List.fold_left
      (fun (view : view list list) (ox, oy) ->
        let nnth list x y = List.nth (List.nth list (y+4)) (x+3) in
        let set_nnth list x y elem =
          let list_y = List.concat (List.take 1 (List.drop (y+4) list)) in
          List.append
            (List.take (y+4) list)
            (List.append
               ((List.append
                  (List.take (x+3) list_y)
                  (List.append
                     [elem]
                     (List.drop (x+3+1) list_y))) :: [])
               (List.drop (y+4+1) list))
        in
        let dist = abs ox + abs oy in
        if dist <= 1
        then view
        else
          (match (ox, oy) with
              (0, 0) -> view
            | (0, _) ->
                if is_transparent (nnth view 0 (oy - (oy / abs oy))).chip
                then view
                else set_nnth view 0 oy {chip=Unknown; item=None}
            | (_, 0) ->
                if is_transparent (nnth view (ox - (ox / abs ox)) 0).chip
                then view
                else set_nnth view ox 0 {chip=Unknown; item=None}
            | _ ->
              if (dist <> 2) &&
                (not (is_transparent (nnth view (ox-(ox/abs ox)) (oy-(oy/abs oy))).chip))
              then
                set_nnth view ox oy {chip=Unknown; item=None}
              else
                (
                  if (dist <> 3) &&
                    (not (is_transparent (nnth view ox (oy-(oy/abs oy))).chip)) &&
                    (not (is_transparent (nnth view (ox-(ox/abs ox)) oy).chip))
                  then
                    set_nnth view ox oy {chip=Unknown; item=None}
                  else
                    view
                )
          )
      )
      all_view
      cansee_check_ord
  in
  (dir, view)

let get_cansee_chara_list ~pos =
  let pos_to_sight_viewpos dir pos =
    List.map
      (fun ((ox, oy), (vx, vy)) -> ({px = pos.px + ox; py = pos.py + oy}, {x = vx; y = vy}))
      (List.combine (List.concat (get_normal_sight_offset dir)) (List.concat normal_sight_offset))
  in
  let chara_sight_viewpos_list =
    List.map
      (fun (chid, pos) -> (chid, pos_to_sight_viewpos (Hashtbl.find charaid_dir_tbl chid) pos))
      (List.of_enum (Hashtbl.enum charaid_pos_tbl))
  in
  List.filter_map
    (fun (chid, pos_viewpos_list) ->
      match List.find_all ((=) pos $ fst) pos_viewpos_list with
          [] -> None
        | pos_viewpos :: _ -> Some (chid, snd pos_viewpos)
    )
    chara_sight_viewpos_list
;;

let get_chara_list_with_position ~pos =
  Map_data.get_chara_list pos
;;

let delete_chara ~chara_id:chid =
  let pos = Hashtbl.find charaid_pos_tbl chid in
  let chara_list = Map_data.get_chara_list pos in
  Map_data.set_chara_list pos (List.remove chara_list chid);
  Hashtbl.remove charaid_pos_tbl chid;
  Hashtbl.remove charaid_dir_tbl chid;
;;

let delete_item ~pos ~item =
  let item_list = Map_data.get_item_list pos in
  Map_data.set_item_list pos (List.remove item_list item)

let add_item ~pos ~item =
  let item_list = Map_data.get_item_list pos in
  Map_data.set_item_list pos (item :: item_list)


let get_item_list_with_position ~pos =
  Map_data.get_item_list pos
;;

let get_rdir_between_adir = function
    (North, North) -> Back
  | (North, East) -> Right
  | (North, West) -> Left
  | (North, South) -> Forth
  | (East, North) -> Left
  | (East, East) -> Back
  | (East, West) -> Forth
  | (East, South) -> Right
  | (West, North) -> Right
  | (West, East) -> Forth
  | (West, West) -> Back
  | (West, South) -> Left
  | (South, North) -> Forth
  | (South, East) -> Left
  | (South, West) -> Right
  | (South, South) -> Back

let get_chara_in_sight_list ~chara_id:chid =
  let pos = Hashtbl.find charaid_pos_tbl chid in
  let dir = Hashtbl.find charaid_dir_tbl chid in
  let sight_offset_list = List.concat (get_normal_sight_offset dir) in
  let sight_pos_list =
    List.map (fun (ox, oy) -> {px = pos.px + ox; py = pos.py + oy}) sight_offset_list
  in
  let (_, mapview) = get_mapview ~chara_id:chid in
  List.concat (List.map
    (fun (pos, (vx, vy)) ->
      if Map_data.is_valid_pos pos &&
        (List.nth (List.nth mapview (vy+4)) (vx+3)).chip <> Unknown
      then
        List.map
          (fun chara_id ->
            (chara_id,
             {x = vx; y= vy},
             get_rdir_between_adir (dir, (Hashtbl.find charaid_dir_tbl chara_id)))
          )
          (Map_data.get_chara_list pos)
      else []
    )
    (List.combine sight_pos_list (List.concat normal_sight_offset)))


let is_enterable ~pos =
  match Map_data.get_chip_view pos with
      Bars | Glass | Mwall | Rock | Unknown | Window | Wood | Wwall | Door_lock | Pcircle_lock -> false
    | Door | Dummy | Flower | Grass | Mist | Pcircle | Road | Tgate | Water -> true

let execute_switch_position chid (cond_list, result) =
  match cond_list with
      [] ->
      (match result with
          Switch.Move pos -> [Event.Switch_move (chid, pos)]
        | Switch.List_select list -> [Event.Switch_list (chid, list)]
      )
    | _ -> [] (* tentative: cannot put conditions to switch for Position_change events *)

let execute_switch_select_done chid ord (cond_list, result) =
  match cond_list with
      [Switch.List_select_done cond_ord] when ord = cond_ord ->
      (match result with
          Switch.Move pos -> [Event.Switch_move (chid, pos)]
        | Switch.List_select list -> [Event.Switch_list (chid, list)]
      )
    | _ -> []

let execute_event event = match event with
    Event.Position_change (chid, (_, Some pos)) ->
    (match Map_data.get_switch_list pos with
        [] -> []
      | switch_list ->
        List.concat (List.map (execute_switch_position chid) switch_list)
    )
  | Event.Switch_select_done (chid, ord, pos) ->
    (match Map_data.get_switch_list pos with
        [] -> []
      | switch_list ->
        List.concat (List.map (execute_switch_select_done chid ord) switch_list)
    )
  | Event.Client_message _ | Event.Position_change _ | Event.Npc_appear | Event.Tick | Event.Attack_to _ | Event.Attack_result _ | Event.Dead _ | Event.Say _ | Event.Switch_move _ | Event.Switch_list _ -> []

let event_dispatch ~event_list =
  List.concat (List.map execute_event event_list)
