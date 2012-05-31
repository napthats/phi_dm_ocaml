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

let get_cansee_chara_list ~pos:_ =
  List.map (fun chid -> (chid, {x = 0; y = 0})) (List.of_enum (Hashtbl.keys charaid_pos_tbl))
;;

let set_chara_position ~chara_id ~pos =
  (match Hashtbl.find_all charaid_pos_tbl chara_id with
      [] -> ()
    | old_pos :: _ ->
      let (ct, chara_list) = phi_map.(old_pos.py).(old_pos.px) in
      phi_map.(old_pos.py).(old_pos.px) <- (ct, List.remove chara_list chara_id));
  (match phi_map.(pos.px).(pos.py) with
      (ct, chara_list) ->
        phi_map.(pos.px).(pos.py) <- (ct, chara_id :: chara_list));
  Hashtbl.replace charaid_pos_tbl chara_id pos
;;

let turn_absolute_direction = function
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

let set_chara_direction ~chara_id ~dir =
  match dir with
      Absolute_direction adir ->
        Hashtbl.replace charaid_dir_tbl chara_id adir
    | Relative_direction rdir ->
        let base_adir = Hashtbl.find charaid_dir_tbl chara_id in
        Hashtbl.replace charaid_dir_tbl chara_id (turn_absolute_direction (base_adir, rdir))
;;


let get_chip_with_outer pos =
  if (pos.px < 0 || pos.px >= phi_map_width || pos.py < 0 || pos.py >= phi_map_height)
  then chip_outer
  else phi_map.(pos.py).(pos.px)
;;

let sight_offset_for_client_north =
  [[(-3, -4); (-2, -4); (-1, -4); (0, -4); (1, -4); (2, -4); (3, -4)]
  ;[(-3, -3); (-2, -3); (-1, -3); (0, -3); (1, -3); (2, -3); (3, -3)]
  ;[(-3, -2); (-2, -2); (-1, -2); (0, -2); (1, -2); (2, -2); (3, -2)]
  ;[(-3, -1); (-2, -1); (-1, -1); (0, -1); (1, -1); (2, -1); (3, -1)]
  ;[(-3, 0); (-2, 0); (-1, 0); (0, 0); (1, 0); (2, 0); (3, 0)]
  ;[(-3, 1); (-2, 1); (-1, 1); (0, 1); (1, 1); (2, 1); (3, 1)]
  ;[(-3, 2); (-2, 2); (-1, 2); (0, 2); (1, 2); (2, 2); (3, 2)]]
;;

let get_sight_offset_for_client = function
    North -> sight_offset_for_client_north
  | East -> List.map (List.rev $ (List.map (fun (x, y) -> (-y, -x)))) sight_offset_for_client_north
  | West -> List.map (List.rev $ (List.map (fun (x, y) -> (y, x)))) sight_offset_for_client_north
  | South -> List.map (List.map (fun (x, y) -> (-x, -y))) sight_offset_for_client_north
;;

let get_mapview ~chara_id =
  let dir = Hashtbl.find charaid_dir_tbl chara_id in
  let pos = Hashtbl.find charaid_pos_tbl chara_id in
  let view =
    List.map
      (List.map (fun (ox, oy) ->  fst (get_chip_with_outer ({px = pos.px + ox; py = pos.py + oy}))))
      (get_sight_offset_for_client dir)
  in
  (dir, view)
;;
