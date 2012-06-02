open ExtHashtbl
open ExtList


let ($) f g x = f (g x);;

type position = {px : int; py : int};;

type view_position = {x : int; y : int};;

type absolute_direction = North | East | West | South;;

type relative_direction = Forth | Right | Left | Back;;

type direction = Absolute_direction of absolute_direction | Relative_direction of relative_direction;;

type mapchip_view = Bars | Door | Dummy | Flower | Glass | Grass | Mist | Mwall | Pcircle | Road | Rock | Tgate | Unknown | Water | Window | Wood | Wwall | Door_lock | Pcircle_lock


let phi_map =
  ([|[|(Bars, []); (Door, []); (Flower, []); (Glass, []); (Grass, []); (Mist, []); (Mwall, [])|];
     [|(Bars, []); (Door, []); (Flower, []); (Glass, []); (Grass, []); (Mist, []); (Mwall, [])|];
     [|(Bars, []); (Door, []); (Flower, []); (Glass, []); (Grass, []); (Mist, []); (Mwall, [])|];
     [|(Bars, []); (Door, []); (Flower, []); (Glass, []); (Grass, []); (Mist, []); (Mwall, [])|];
     [|(Bars, []); (Door, []); (Flower, []); (Glass, []); (Grass, []); (Mist, []); (Mwall, [])|];
     [|(Bars, []); (Door, []); (Flower, []); (Glass, []); (Grass, []); (Mist, []); (Mwall, [])|];
     [|(Bars, []); (Door, []); (Flower, []); (Glass, []); (Grass, []); (Mist, []); (Mwall, [])|];|])
;;

let chip_outer = (Unknown, []);;

let phi_map_width = 7;;

let phi_map_height = 7;;

let charaid_pos_tbl = Hashtbl.create 100;;

let charaid_dir_tbl = Hashtbl.create 100;;


let get_default_position = {px = 0; py = 0};;

let set_chara_position ~chara_id ~pos =
  (match Hashtbl.find_all charaid_pos_tbl chara_id with
      [] -> ()
    | old_pos :: _ ->
      let (ct, chara_list) = phi_map.(old_pos.py).(old_pos.px) in
      phi_map.(old_pos.py).(old_pos.px) <- (ct, List.remove chara_list chara_id));
  (match phi_map.(pos.py).(pos.px) with
      (ct, chara_list) ->
        phi_map.(pos.py).(pos.px) <- (ct, chara_id :: chara_list));
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
;;

let set_chara_direction ~chara_id ~adir =
  Hashtbl.replace charaid_dir_tbl chara_id adir
;;

let adir_to_offset = function
    North -> (0, -1)
  | East -> (1, 0)
  | West -> (-1, 0)
  | South -> (0, 1)
;;

let is_position_valid pos =
  pos.px >= 0 && pos.px < phi_map_width && pos.py >= 0 && pos.py < phi_map_height
;;

let get_neighbor_position ~pos ~adir =
  let offset = adir_to_offset adir in
  let new_pos = {px = pos.px + (fst offset); py = pos.py + (snd offset)} in
  if is_position_valid new_pos
  then Some new_pos
  else None
;;

let get_chara_position ~chara_id =
  match Hashtbl.find_all charaid_pos_tbl chara_id with
      [] -> assert false
    | [pos] -> pos
    | _ -> assert false
;;

let get_chara_absolute_direction ~chara_id =
  match Hashtbl.find_all charaid_dir_tbl chara_id with
      [] -> assert false
    | [dir] -> dir
    | _ -> assert false
;;


let get_chip_with_outer pos =
  if is_position_valid pos
  then phi_map.(pos.py).(pos.px)
  else chip_outer
;;

let normal_sight_offset =
  [[(-3, -4); (-2, -4); (-1, -4); (0, -4); (1, -4); (2, -4); (3, -4)]
  ;[(-3, -3); (-2, -3); (-1, -3); (0, -3); (1, -3); (2, -3); (3, -3)]
  ;[(-3, -2); (-2, -2); (-1, -2); (0, -2); (1, -2); (2, -2); (3, -2)]
  ;[(-3, -1); (-2, -1); (-1, -1); (0, -1); (1, -1); (2, -1); (3, -1)]
  ;[(-3, 0); (-2, 0); (-1, 0); (0, 0); (1, 0); (2, 0); (3, 0)]
  ;[(-3, 1); (-2, 1); (-1, 1); (0, 1); (1, 1); (2, 1); (3, 1)]
  ;[(-3, 2); (-2, 2); (-1, 2); (0, 2); (1, 2); (2, 2); (3, 2)]]
;;

let get_normal_sight_offset = function
    North -> normal_sight_offset
  | East -> List.map (List.rev $ (List.map (fun (x, y) -> (-y, -x)))) normal_sight_offset
  | West -> List.map (List.rev $ (List.map (fun (x, y) -> (y, x)))) normal_sight_offset
  | South -> List.map (List.map (fun (x, y) -> (-x, -y))) normal_sight_offset
;;

let get_mapview ~chara_id =
  let dir = Hashtbl.find charaid_dir_tbl chara_id in
  let pos = Hashtbl.find charaid_pos_tbl chara_id in
  let view =
    List.map
      (List.map (fun (ox, oy) ->  fst (get_chip_with_outer ({px = pos.px + ox; py = pos.py + oy}))))
      (get_normal_sight_offset dir)
  in
  (dir, view)
;;

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
  snd phi_map.(pos.py).(pos.px)
;;

let delete_chara ~chara_id:chid =
  let pos = Hashtbl.find charaid_pos_tbl chid in
  let map_chip = phi_map.(pos.py).(pos.px) in
  phi_map.(pos.py).(pos.px) <- (fst map_chip, List.filter ((<>) chid) (snd map_chip));
  Hashtbl.remove charaid_pos_tbl chid;
  Hashtbl.remove charaid_dir_tbl chid
;;
