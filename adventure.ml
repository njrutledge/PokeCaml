open Yojson.Basic.Util

type room_id = string
type exit_name = string
type item_name = string
exception UnknownRoom of room_id
exception UnknownExit of exit_name
exception LockedExit of exit_name
exception UnknownItem of string


(* [exit] defines an exit of a [room]. *)
type exit = {
  name : exit_name;
  is_locked : bool;
  exit_keys : item_name list;
  exit_room : room_id;
}

type dynamic_desc = {
  room_items : item_name list;
  player_items : item_name list;
  dyn_description : string;
}

(** [room] defines a room in the game. *)
type room = {
  id : room_id;
  default_desc : string;
  dynamic_desc : dynamic_desc list;
  score : int;
  doots : string;  
  exits : exit list;
}

type item = {
  item_name : item_name;
  cur_room : room_id;
  score : int;
}

type t_room = {
  t_id : room_id;
  needed_items : item_name list;
}

type win = {
  min_score : int;
  win_message : string;
}

type t = {
  rooms : room list;
  start : room_id;
  items : item list;
  treasure_room : t_room;
  win_msgs : win list;

}

(** [json_exit j] is the adventure room exit that [j] represents. 
    Requires: [j] is a valid JSON adventure exit representation. *)
let json_exit j_exit = {
  name = 
    j_exit 
    |> member "name" 
    |> to_string;
  is_locked = 
    j_exit
    |> member "is locked"
    |> to_bool;
  (*ex_msg = j_exit|> member "msg"|> to_string;does_exit =j_exit|> member "exit?"|> to_bool; ex_score = j_exit|> member "score"|> to_int;*)
  exit_keys =
    j_exit
    |> member "keys"
    |> to_list
    |> List.map to_string;
  exit_room = 
    j_exit 
    |> member "room id" 
    |> to_string;
}

let json_dynamic_desc j_dy_desc = {
  room_items = 
    j_dy_desc
    |> member "room items"
    |> to_list
    |> List.map to_string;
  player_items = 
    j_dy_desc
    |> member "player items"
    |> to_list
    |> List.map to_string;
  dyn_description = 
    j_dy_desc
    |> member "description"
    |> to_string;
}

(** [json_room j] is the adventure room that [j] represents. 
    Requires: [j] is a valid JSON adventure room representation. *)
let json_room j_room = {
  id = 
    j_room 
    |> member "id" 
    |> to_string;
  default_desc = 
    j_room 
    |> member "description" 
    |> to_string;
  dynamic_desc = 
    j_room
    |> member "dynamic descriptions"
    |> to_list
    |> List.map json_dynamic_desc;
  doots = 
    j_room
    |> member "doot level" 
    |> to_string;
  score = 
    j_room
    |> member "score"
    |> to_int;
  exits = 
    j_room 
    |> member "exits" 
    |> to_list 
    |> List.map json_exit;
}

let json_item j_item = {
  item_name = 
    j_item
    |> member "name"
    |> to_string;
  cur_room = 
    j_item
    |> member "start room"
    |> to_string;
  score = 
    j_item
    |> member "score"
    |> to_int;
}

let json_t_room j_t_room= {
  t_id = 
    j_t_room
    |> member "room id"
    |> to_string;
  needed_items = 
    j_t_room
    |> member "needed items"
    |> to_list
    |> List.map to_string;
}

let json_win j_win = {
  min_score = 
    j_win 
    |> member "min score"
    |> to_int;
  win_message = 
    j_win
    |> member "message"
    |> to_string;
}

let from_json json = 
  {
    rooms = 
      json 
      |> member "rooms" 
      |> to_list 
      |> List.map json_room;
    start = 
      json 
      |> member "start room"  
      |> to_string;
    items = 
      json
      |> member "adventure items"
      |> to_list
      |> List.map json_item;
    treasure_room = 
      json
      |> member "treasure room"
      |> json_t_room;
    win_msgs = 
      json
      |> member "win messages"
      |> to_list
      |> List.map json_win;
  }

let start_room adv =
  adv.start

let room_ids adv = 
  adv.rooms
  |> List.map(fun room -> room.id)
  |> List.sort_uniq compare 

(** [find_room r room_lst] is the [room] in [room_lst] with room_id [r]. 
    Raises [UnknownRoom r] if [r] is not a room identifier in [room_lst]. *)
let find_room room rooms = 
  match List.find_opt (fun x-> x.id=room) rooms with 
  | Some r -> r
  | None -> raise (UnknownRoom room)

let room_items adv room = 
  let rec find_items = function 
    | [] -> []
    |h::t -> if h.cur_room = room then h.item_name::find_items t else
        find_items t
  in find_items adv.items 

(** [has_needed_items dyn a r] is true if the room [r] in adventure [a] 
    and the player have the necessary items to show the dynamic description. *)
let has_needed_items dyn_desc adv room = 
  List.fold_left 
    (fun x y -> x && List.mem y (room_items adv room))
    true
    dyn_desc.room_items &&
  List.fold_left 
    (fun x y -> x && List.mem y (room_items adv "INV"))
    true
    (dyn_desc.player_items)

(** [match_dyn_desc adv room] is the description of room [r] in adventure [a], 
    accounting for dynamic descriptions. If the player and [r] have the 
    necessary items, then a dynamic description is returned. Otherwise, the 
    default message for the room is returned.  *)
let rec match_dyn_desc adv room = function
  | [] -> room.default_desc
  | h::t -> if has_needed_items h adv room.id then h.dyn_description else 
      match_dyn_desc adv room t

(** [get_dyn_desc adv room] gets the correct description 
    of room [room] in adventure [adv]*)
let get_dyn_desc adv room = 
  match_dyn_desc adv room room.dynamic_desc

let description adv room =
  (adv.rooms |> find_room room)|>
  get_dyn_desc adv 

let room_score adv room = 
  (adv.rooms |> find_room room).score

let exits adv room = 
  (adv.rooms |> find_room room).exits
  |> List.map (fun exit -> exit.name)
  |> List.sort_uniq compare

(** [find_exit ex exits] is the exit associated with exit name [ex]. 
    Raises [UnknownExit ex] if [ex] is not a name of an exit in [exits]. *)
let rec find_exit ex exits= 
  match exits with 
  | [] -> raise (UnknownExit ex)
  | h::t -> if h.name = ex then h else find_exit ex t

(** [get_exit_room ex] is the [exit_room] associated with [ex]. 
    If [ex] does not exit, returns [room]. *)
let get_exit_room room ex = 
  if ex.is_locked then raise (LockedExit ex.name) else ex.exit_room (*else room*)

let next_room adv room ex =
  (adv.rooms |> find_room room).exits
  |> find_exit ex
  |> get_exit_room room 

let next_rooms adv room =
  (adv.rooms |> find_room room).exits
  |> List.map (fun exit -> exit.exit_room)
  |> List.sort_uniq compare

(** [remove_item it items] removes the item [it] from the item list [items]. 
    Raises [UnknownItem it] if [items] does not contain [it]. *)
let rec remove_item it = function
  | [] -> raise (UnknownItem it)
  | h::t -> if h = it then t else h::remove_item it t 

(** [modify_items dest room it items] modifies the [cur_room] of [it] in the 
    list of items [item]. room_id "INV" represents the adventurer's inventory 
    Raises [UnknownItem it] if [it] is not in the list [items]. *)
let rec modify_items dest room it = function
  | [] -> raise (UnknownItem it)
  | h::t -> if h.item_name = it then
      if h.cur_room = room then {h with cur_room = dest}::t 
      else raise (UnknownItem it)
    else h::modify_items dest room it t

let take_item adv room it = { 
  adv with items = modify_items "INV" room it adv.items
}

let drop_item adv room it = {
  adv with items = modify_items room "INV" it adv.items
}

let treasure_room_id adv = 
  adv.treasure_room.t_id

let treasure_room_needed_items adv = 
  adv.treasure_room.needed_items

let win_msg adv score = 
  let rec check_win = function
    | [] -> "you win!"
    | h::t -> if h.min_score <= score then h.win_message else check_win t
  in check_win adv.win_msgs

let item_score adv item = 
  let rec find_item = function
    | [] -> raise (UnknownItem item)
    | h::t -> if h.item_name = item then h.score else find_item t
  in find_item adv.items

let keys adv room ex = 
  ((adv.rooms |> find_room room).exits |> find_exit ex).exit_keys


(** [lock_exit ex] locks exit [ex]. *)
let lock_exit ex = {
  ex with is_locked = true
}
(** [unlock_exit ex] unlocks exit [ex]. *)
let unlock_exit ex = {
  ex with is_locked = false
}
(** [modify_exit f r e] modifies exit [ex] of the exits of room [r] by
    applying [f] to [e]. 
    Raises [UnknownExit e] if [e] is not an exit of room [r]. *)
let modify_exit mod_f room ex = {
  room with exits = begin 
    let rec find_exit = function
      | [] -> raise (UnknownExit ex)
      | h::t -> if h.name = ex then mod_f h :: t else h :: find_exit t
    in find_exit room.exits
  end 
}
(** [modify_room_exit f a r e] modifies the exit with name [e] of room with 
    name [r] of adventure [a] by applying [f] to the exit. 
    Raises [UnknownRoom r] if [r] is not a room identifier in [a]. 
    Raises [UnknownExit e] if [e] is not an exit identifier of room [r]. *)
let modify_room_exit mod_f adv room ex = 
  let rec find_room = function
    | [] -> raise (UnknownRoom room)
    | h::t -> if h.id = room then modify_exit mod_f h ex :: t else 
        h :: find_room t
  in find_room adv.rooms

let unlock adv room ex = {
  adv with rooms = modify_room_exit (unlock_exit) adv room ex
}

let lock adv room ex = {
  adv with rooms = modify_room_exit (lock_exit) adv room ex
}

let doot adv room = 
  (adv.rooms |> find_room room ).doots