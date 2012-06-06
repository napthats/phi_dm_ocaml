open ExtHashtbl
open ExtList
open Item.Open


let ($) f g x = f (g x);;

type position = {px : int; py : int};;

type view_position = {x : int; y : int};;

type absolute_direction = North | East | West | South;;

type relative_direction = Forth | Right | Left | Back;;

type direction = Absolute_direction of absolute_direction | Relative_direction of relative_direction;;

type mapchip_view = Bars | Door | Dummy | Flower | Glass | Grass | Mist | Mwall | Pcircle | Road | Rock | Tgate | Unknown | Water | Window | Wood | Wwall | Door_lock | Pcircle_lock


(* throw exception if posistion isn't valid *)
module Map_data :
sig
  val get_chip_view : position -> mapchip_view
  val set_chip_view : position -> mapchip_view -> unit
  val get_chara_list : position -> Chara_id.t list
  val set_chara_list : position -> Chara_id.t list -> unit
  val get_item_list : position -> Item.t list
  val set_item_list : position -> Item.t list -> unit
  val is_valid_pos : position -> bool
end
=
struct
  let item_debug =
      Item.create ~view:(
        {name = "test item 1"; attack_range = Item.Forth; material = Item.Steel; weapon_type = Item.Sword; atp = 10; item_type =Item.Weapon {element = Item.Fire; er = 30; effect = Item.EFNone; special_effect = Item.SENone}})
    ;;
  let data =
    ([|[|(Bars, ([], [])); (Door, ([], [item_debug])); (Flower, ([], [])); (Glass, ([], [])); (Grass, ([], [])); (Mist, ([], [])); (Mwall, ([], []))|];
       [|(Bars, ([], [])); (Door, ([], [])); (Flower, ([], [])); (Glass, ([], [])); (Grass, ([], [])); (Mist, ([], [])); (Mwall, ([], []))|];
       [|(Bars, ([], [])); (Door, ([], [])); (Flower, ([], [])); (Glass, ([], [])); (Grass, ([], [])); (Mist, ([], [])); (Mwall, ([], []))|];
       [|(Bars, ([], [])); (Door, ([], [])); (Flower, ([], [])); (Glass, ([], [])); (Grass, ([], [])); (Mist, ([], [])); (Mwall, ([], []))|];
       [|(Bars, ([], [])); (Door, ([], [])); (Flower, ([], [])); (Glass, ([], [])); (Grass, ([], [])); (Mist, ([], [])); (Mwall, ([], []))|];
       [|(Bars, ([], [])); (Door, ([], [])); (Flower, ([], [])); (Glass, ([], [])); (Grass, ([], [])); (Mist, ([], [])); (Mwall, ([], []))|];
       [|(Bars, ([], [])); (Door, ([], [])); (Flower, ([], [])); (Glass, ([], [])); (Grass, ([], [])); (Mist, ([], [])); (Mwall, ([], []))|];|])
  ;;
  let get_chip_view pos = fst data.(pos.py).(pos.px);;
  let set_chip_view pos chip_view = 
    let (_, lists) = data.(pos.py).(pos.px) in
    data.(pos.py).(pos.px) <- (chip_view, lists)
  ;;
  let get_chara_list pos = fst (snd data.(pos.py).(pos.px));;
  let set_chara_list pos chara_list = 
    let (cv, (_, item_list)) = data.(pos.py).(pos.px) in
    data.(pos.py).(pos.px) <- (cv, (chara_list, item_list))
  ;;
  let get_item_list pos = snd (snd data.(pos.py).(pos.px));;
  let set_item_list pos item_list = 
    let (cv, (chara_list, _)) = data.(pos.py).(pos.px) in
    data.(pos.py).(pos.px) <- (cv, (chara_list, item_list))
  ;;

  let data_width = 7;;

  let data_height = 7;;

  let is_valid_pos pos =
    pos.px >= 0 && pos.px < data_width && pos.py >= 0 && pos.py < data_height
  ;;
end
;;


let charaid_pos_tbl = Hashtbl.create 100;;

let charaid_dir_tbl = Hashtbl.create 100;;


let get_default_position = {px = 0; py = 0};;

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


let get_neighbor_position ~pos ~adir =
  let offset = adir_to_offset adir in
  let new_pos = {px = pos.px + (fst offset); py = pos.py + (snd offset)} in
  if Map_data.is_valid_pos new_pos
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
      (List.map (fun (ox, oy) ->
        let p = {px = pos.px + ox; py = pos.py + oy} in
        if Map_data.is_valid_pos p
        then Map_data.get_chip_view p
        else Unknown))
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
  Map_data.get_chara_list pos
;;

let delete_chara ~chara_id:chid =
  let pos = Hashtbl.find charaid_pos_tbl chid in
  let chara_list = Map_data.get_chara_list pos in
  Map_data.set_chara_list pos (List.remove chara_list chid);
  Hashtbl.remove charaid_pos_tbl chid;
  Hashtbl.remove charaid_dir_tbl chid
;;

let delete_item ~pos ~item =
  let item_list = Map_data.get_item_list pos in
  Map_data.set_item_list pos (List.remove item_list item)
;;

let get_item_list_with_position ~pos =
  Map_data.get_item_list pos
;;
