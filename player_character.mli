type list_select_type = Get | Use

type command_st = 
    Normal
  | List_select of list_select_type

type t = 
    <get_name : string;
  get_status_view : Chara_status.view;
  get_item_list : (Item.t * Chara.equip_flag option) list;
  get_chara_id : Chara_id.t;
  sight_change : Chara.sight_change_type -> Event.t list;
  sight_update : Event.t list;
  turn : dir:Phi_map.direction -> Event.t list;
  go : dir:Phi_map.direction -> Event.t list;
  do_action : Event.t list;
  hit : Event.t list;
  defense : combat:Combat.t -> achid:Chara_id.t -> Event.t list;
  resolve_attack_result : result_list:Combat.result list -> dchid:Chara_id.t -> Event.t list;
     (* throw an exception if there is no such item *)
  item_get : item:Item.t -> Event.t list;
  dead : Event.t list;
  say : msg:string -> Event.t list;
  listen : msg:string -> achid:Chara_id.t -> Event.t list;
  use_item : item:Item.t -> Event.t list;

  get_command_st : command_st;
  set_command_st : st:command_st -> unit;
  get_phirc : string>

val create :
  phirc : string ->
  cid : Tcp_server.client_id ->
  chid : Chara_id.t ->
  t option
