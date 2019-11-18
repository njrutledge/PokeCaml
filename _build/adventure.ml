open Yojson.Basic.Util
open Pokemon
module PM = Pokemon

type town_id = string
type exit_name = string
type item_name = string
exception UnknownTown of town_id
exception UnknownExit of exit_name
exception LockedExit of exit_name
exception UnknownItem of string


(* [exit] defines an exit of a [town]. *)
type exit = {
  name : exit_name;
  exit_keys : item_name list;
  exit_town : town_id;
}

type dynamic_desc = {
  town_items : item_name list;
  player_items : item_name list;
  dyn_description : string;
}

(** [town] defines a town in the game. *)
type town = {
  id : town_id;
  default_desc : string;
  (*dynamic_desc : dynamic_desc list;*)
  exits : exit list;
}

type item = {
  item_name : item_name;
}

type t_town = {
  t_id : town_id;
  needed_items : item_name list;
}

type win = {
  win_message : string;
}

type bat = 
  | Wild 
  | Trainer of string

type route = {
  route_name : string;
  battles : bat list;
  wilds : ((string * float) * int * int) list;
}

type t = {
  towns : town list;
  routes : route list;
  start : town_id;
  poke_file : string;
  trainers : (string * PM.t list) list
  (*items : item list;*)
  (*treasure_town : t_town;*)
  (*win_msgs : win list;*)
}

(** [get_ranges acc prev lst] is the list of wild pokemon taken from [lst] 
    in a tuple with their start and end ranges for randomizing. 
    Requires: [prev] starts at 0 and [acc] starts empty.  *)
let rec get_ranges acc prev = function
  | [] -> acc
  | (name,lvl , chance) :: t -> let next = prev + 1 + chance in 
    get_ranges (((name,lvl), prev + 1, next) :: acc) next t

(** [json_exit j] is the adventure town exit that [j] represents. 
    Requires: [j] is a valid JSON adventure exit representation. *)
let json_exit j_exit = {
  name = 
    j_exit 
    |> member "name" 
    |> to_string;
  exit_keys =
    j_exit
    |> member "keys"
    |> to_list
    |> List.map to_string;
  exit_town = 
    j_exit 
    |> member "place" 
    |> to_string;
}

let json_dynamic_desc j_dy_desc = {
  town_items = 
    j_dy_desc
    |> member "town items"
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

(** [json_town j] is the adventure town that [j] represents. 
    Requires: [j] is a valid JSON adventure town representation. *)
let json_town j_town = {
  id = 
    j_town 
    |> member "id" 
    |> to_string;
  default_desc = 
    j_town 
    |> member "description" 
    |> to_string;
  (*dynamic_desc = 
    j_town
    |> member "dynamic descriptions"
    |> to_list
    |> List.map json_dynamic_desc;*)
  exits = 
    j_town 
    |> member "exits" 
    |> to_list 
    |> List.map json_exit;
}

let json_item j_item = {
  item_name = 
    j_item
    |> member "name"
    |> to_string;
}

let json_wilds j_item = 
  let name = 
    j_item 
    |> member "name"
    |> to_string in
  let lvl = 
    j_item
    |> member "lvl"
    |> to_float in
  let chance = 
    j_item
    |> member "chance"
    |> to_int in
  (name, lvl, chance)

let json_t_pokemon j_item = 
  let name = 
    j_item 
    |> member "name"
    |> to_string in
  let lvl = 
    j_item
    |> member "lvl"
    |> to_float in
  (PM.create_pokemon name lvl)

let json_trainers j_item = 
  let name = 
    j_item 
    |> member "name"
    |> to_string in
  let pokemon_list = 
    j_item
    |> member "pokemon"
    |> to_list 
    |> List.map json_t_pokemon in
  (name, pokemon_list)

let rec make_bats acc = function 
  | [] ->  acc
  | h :: t -> if h = "wild" then make_bats (Wild :: acc) t 
    else make_bats (Trainer h :: acc) t

let json_route j_route = {
  route_name = 
    j_route 
    |> member "name"
    |> to_string;
  battles = 
    j_route 
    |> member "battles" 
    |> to_list 
    |> List.map to_string 
    |> make_bats [];
  wilds =
    j_route
    |> member "wilds"
    |> to_list
    |> List.map json_wilds
    |> get_ranges [] 0;
}

let from_json json = {
  towns = 
    json 
    |> member "places" 
    |> to_list 
    |> List.map json_town;
  start = 
    json 
    |> member "start town"  
    |> to_string;
  routes = 
    json
    |> member "route_file" 
    |> to_string
    |> Yojson.Basic.from_file
    |> member "routes"
    |> to_list
    |> List.map json_route;
  poke_file = 
    json
    |> member "pokemon_file" 
    |> to_string;
  trainers =
    json
    |> member "route_file" 
    |> to_string
    |> Yojson.Basic.from_file
    |> member "trainers"
    |> to_list
    |> List.map json_trainers
    (*items = 
      json
      |> member "adventure items"
      |> to_list
      |> List.map json_item;*)
}

let start_town adv =
  adv.start

let town_ids adv = 
  adv.towns
  |> List.map(fun town -> town.id)
  |> List.sort_uniq compare 

(** [find_town r town_lst] is the [town] in [town_lst] with town_id [r]. 
    Raises [UnknownTown r] if [r] is not a town identifier in [town_lst]. *)
let find_town town towns = 
  match List.find_opt (fun x-> x.id=town) towns with 
  | Some r -> r
  | None -> raise (UnknownTown town)

let description adv town =
  (adv.towns |> find_town town).default_desc

let exits adv town = 
  (adv.towns |> find_town town).exits
  |> List.map (fun exit -> exit.name)
  |> List.sort_uniq compare

(** [find_exit ex exits] is the exit associated with exit name [ex]. 
    Raises [UnknownExit ex] if [ex] is not a name of an exit in [exits]. *)
let rec find_exit ex exits= 
  match exits with 
  | [] -> raise (UnknownExit ex)
  | h::t -> if h.name = ex then h else find_exit ex t

(** [get_exit_town ex] is the [exit_town] associated with [ex]. 
    If [ex] *)
let get_exit_town town ex = 
  ex.exit_town 

let next_town adv town ex =
  (adv.towns |> find_town town).exits
  |> find_exit ex
  |> get_exit_town town 

let next_towns adv town =
  (adv.towns |> find_town town).exits
  |> List.map (fun exit -> exit.exit_town)
  |> List.sort_uniq compare

(** [remove_item it items] removes the item [it] from the item list [items]. 
    Raises [UnknownItem it] if [items] does not contain [it]. *)
let rec remove_item it = function
  | [] -> raise (UnknownItem it)
  | h::t -> if h = it then t else h::remove_item it t 

let win_msg adv score = 
  let rec check_win = function
    | [] -> "you win!"
    | h::t -> "you win!"
  in check_win []

let keys adv town ex = 
  ((adv.towns |> find_town town).exits |> find_exit ex).exit_keys

(** [modify_exit f r e] modifies exit [ex] of the exits of town [r] by
    applying [f] to [e]. 
    Raises [UnknownExit e] if [e] is not an exit of town [r]. *)
let modify_exit mod_f town ex = {
  town with exits = begin 
    let rec find_exit = function
      | [] -> raise (UnknownExit ex)
      | h::t -> if h.name = ex then mod_f h :: t else h :: find_exit t
    in find_exit town.exits
  end 
}
(** [modify_town_exit f a r e] modifies the exit with name [e] of town with 
    name [r] of adventure [a] by applying [f] to the exit. 
    Raises [UnknownTown r] if [r] is not a town identifier in [a]. 
    Raises [UnknownExit e] if [e] is not an exit identifier of town [r]. *)
let modify_town_exit mod_f adv town ex = 
  let rec find_town = function
    | [] -> raise (UnknownTown town)
    | h::t -> if h.id = town then modify_exit mod_f h ex :: t else 
        h :: find_town t
  in find_town adv.towns

(** [find_route adv town r] is the route associated with route_name [r].
    Raises [UnknownTown town] if [town] is not a town identifier in [a]. 
    Raises [UnknownExit e] if [e] is not an exit identifier of town [r], or if 
    [r] is not a valid route for the current town. *)
let get_battles adv town r = 
  if (adv.towns |> find_town town).exits 
     |> List.map (fun x -> x.name) 
     |> List.mem r then 
    adv.routes 
    |> List.find (fun x -> x.route_name = r) 
    |> (fun x -> x.battles) 
  else raise(UnknownExit r)

let take_route adv town r = 
  let next = next_town adv town r in 
  (get_battles adv town r, next)

let get_wild adv route = 
  let wilds = (adv.routes |> List.find (fun x -> x.route_name = route)).wilds in
  Random.self_init (); let rand = Random.int 100 in
  let rec find_wild = function 
    | [] -> failwith "bad math (aka wild random error)"
    | ((name,lvl), st, nd) :: t -> 
      if st <= rand && rand <= nd then [PM.create_pokemon name lvl] 
      else find_wild t
  in find_wild wilds

let get_t_mons adv name = 
  match (List.assoc_opt name adv.trainers) with 
  | None -> failwith "trainer does not exist"
  | Some v -> v