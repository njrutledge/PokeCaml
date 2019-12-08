open Yojson.Basic.Util
open Pokemon
open Moves
module PM = Pokemon

type town_id = string
type exit_name = string
type badge_name = string
exception UnknownTown of town_id
exception UnknownExit of exit_name
exception LockedExit of exit_name
exception UnknownBadge of string


(** [exit] defines an exit of a [town]. *)
type exit = {
  name : exit_name;
  exit_badge : badge_name;
  exit_town : town_id;
}

(** [town] defines a town in the game. *)
type town = {
  id : town_id;
  default_desc : string;
  exits : exit list;
}

type bat = 
  | Wild 
  | Trainer of string

(** [route] defines the type of a route. *)
type route = {
  route_name : string;
  badge: string;
  battles : bat list;
  wilds : ((string * int * string list) * int * int) list;
}

type t = {
  towns : town list;
  routes : route list;
  start : town_id;
  poke_file : string;
  trainers : (string * (int * bool * PM.t array)) list
}

(** [get_ranges acc prev lst] is the list of wild pokemon taken from [lst] 
    in a tuple with their start and end ranges for randomizing. 
    Requires: [prev] starts at 0 and [acc] starts empty.  *)
let rec get_ranges acc prev = function
  | [] -> acc
  | (name, lvl, chance, moves) :: t -> let next = prev + 1 + chance in 
    get_ranges (((name, lvl, moves), prev + 1, next) :: acc) next t

(** [json_exit j] is the adventure town exit that [j] represents. 
    Requires: [j] is a valid JSON adventure exit representation. *)
let json_exit j_exit = {
  name = 
    j_exit 
    |> member "name" 
    |> to_string;
  exit_badge =
    j_exit
    |> member "badge"
    |> to_string;
  exit_town = 
    j_exit 
    |> member "place" 
    |> to_string;
}

(** [json_town j] is the adventure town that [j] represents. 
    Requires: [j] is a valid JSON adventure town representation. *)
let json_town j_town = 
  try 
    {
      id = 
        j_town 
        |> member "id" 
        |> to_string;
      default_desc = 
        j_town 
        |> member "description" 
        |> to_string;
      exits = 
        j_town 
        |> member "exits" 
        |> to_list 
        |> List.map json_exit;
    }
  with _ -> failwith "bad town"

(** [json_wilds j] is the wild pokemon that [j] represents. 
    Requires: [j] is a valid JSON wild representation. *)
let json_wilds j_item = 
  let name = 
    j_item 
    |> member "name"
    |> to_string in
  let lvl = 
    j_item
    |> member "lvl"
    |> to_int in
  let chance = 
    j_item
    |> member "chance"
    |> to_int in
  let moves = 
    j_item
    |> member "moves"
    |> to_list
    |>List.map to_string in 
  (name, lvl, chance, moves)

let json_t_pokemon j_item = 
  try
    let name = 
      j_item 
      |> member "name"
      |> to_string in
    let lvl = 
      j_item
      |> member "lvl"
      |> to_int in
    let moves = 
      j_item
      |> member "moves"
      |> to_list
      |> List.map (fun x -> x|> to_string |> Moves.create_move) in 
    PM.create_pokemon name lvl moves
  with Failure e -> failwith (e ^ ": error in json_t_pokemon")

(** [json_trainers j] is the trainer that [j] represents. 
    Requires: [j] is a valid JSON trainer representation. *)
let json_trainers j_item = 
  let name = 
    j_item 
    |> member "name"
    |> to_string in
  try 
    let pokemon_list = 
      j_item
      |> member "pokemon"
      |> to_list 
      |> List.map json_t_pokemon in
    let money =
      j_item 
      |> member "money"
      |> to_int in
    (name, (money, false, Array.of_list pokemon_list))
  with Failure e -> failwith (e ^ ": trainer " ^ name)

(** [make_bats acc names] is the list of battles agains the mons or trainers
    in string list [names]. [acc] is the accumulator. *)
let rec make_bats acc = function 
  | [] ->  acc
  | h :: t -> if h = "wild" then make_bats (Wild :: acc) t 
    else make_bats (Trainer h :: acc) t

(** [json_route j] is the adventure route that [j] represents. 
    Requires: [j] is a valid JSON adventure route representation. *)
let json_route j_route = 
  try {
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
    badge = 
      j_route
      |> member "badge"
      |> to_string;
  }
  with _ -> failwith "bad route"

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

let req_badge adv town route = 
  ((adv.towns |> find_town town).exits |> find_exit route).exit_badge

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
  let is_member =  ((adv.towns |> find_town town).exits 
                    |> List.map (fun x -> x.name) 
                    |> List.mem r )
  in 
  if is_member then 
    adv.routes 
    |> List.find (fun x -> x.route_name = r) 
    |> (fun x -> x.battles) 
    |> List.rev
  else raise(UnknownExit r)

let take_route adv town r = 
  get_battles adv town r

let get_wild adv route = 
  let wilds = (adv.routes |> List.find (fun x -> x.route_name = route)).wilds in
  Random.self_init (); let rand = Random.int 100 + 1 in
  let rec find_wild = function 
    | [] -> failwith "bad math (aka wild random error)"
    | ((name, lvl, str_moves), st, nd) :: t ->
      let moves = List.map (fun x -> Moves.create_move x) str_moves in  
      if st <= rand && rand <= nd then [|PM.create_pokemon name lvl moves|] 
      else find_wild t
  in 
  find_wild wilds

let get_t_mons adv name = 
  match (List.assoc_opt name adv.trainers) with 
  | None -> failwith "trainer does not exist"
  | Some (money, false, v) -> v
  | Some (_, true, _ ) -> [||]

let get_defeat adv name = 
  match (List.assoc_opt name adv.trainers) with 
  | None -> failwith "trainer does not exist"
  | Some (_, d, _)  -> d

let defeat_trainers adv names =
  {
    adv with 
    trainers = List.map 
        (fun (n, (m, d, pms)) -> if List.mem n names then (n,(0,true,pms)) 
          else (n, (m, d, pms))) adv.trainers
  }

let get_trainer_money adv name = 
  match (List.assoc_opt name adv.trainers) with 
  | None -> failwith "trainer does not exist"
  | Some (m, _, _)  -> m

let get_badge adv route = 
  (adv.routes |> List.find (fun x -> x.route_name = route)).badge