open Pokemon
open Moves
open Yojson.Basic
open Yojson.Basic.Util
module PM = Pokemon

exception ItemNotFound of Adventure.badge_name
exception BadgeNotFound

type t = {
  cur_town : Adventure.town_id;
  bag : (Item.t * int ref) list;
  money : int ref;
  party: PM.t array;
  defeated_trainers: string list;
  defeated_trainers_full: string list;
  badges: string list;
}

let init_state adv = {
  cur_town = Adventure.start_town adv;
  bag = [(Potion, ref 5); (HyperPotion, ref 1);
         (PokeBall, ref 5); (GreatBall, ref 1)];
  money = ref 1000;
  party = [|(PM.create_pokemon "Pikachu" 5 [Moves.create_move "thundershock";
                                            Moves.create_move "thunder wave";
                                            Moves.create_move "rest"]);
            (PM.create_pokemon "Charmander" 5 [Moves.create_move "scratch"]);
            (PM.create_pokemon "Squirtle" 5 [Moves.create_move "tackle";]);|];
  defeated_trainers = [];
  defeated_trainers_full = [];
  badges = []
}

let current_town_id st =
  st.cur_town

type result = Legal of t | Illegal of string

let go ex adv st =
  try 
    Legal{
      cur_town = Adventure.next_town adv st.cur_town ex;
      bag = st.bag;
      money = st.money;
      party = st.party;
      defeated_trainers = st.defeated_trainers;
      defeated_trainers_full = st.defeated_trainers_full;
      badges = st.badges
    }
  with 
  | Adventure.UnknownExit ex -> Illegal ("\nExit \"" ^ ex ^ "\" does not exist.\n")
  | Adventure.LockedExit ex -> Illegal ("\nIt's locked.\n")

let rec run_battles route adv st = function 
  | [] -> begin 
      if (String.split_on_char ' ' route |> List.hd) =  "gym" then 
        go route adv 
          {st with badges = Adventure.get_badge adv route :: st.badges}  
      else begin 
        print_endline(st.cur_town);
        go route adv st
      end 
    end 
  | Adventure.Wild :: t -> make_battle route adv st "wild" (Adventure.get_wild adv route) t
  | Adventure.Trainer tr :: t ->  
    let t_mons = Adventure.get_t_mons adv tr in 
    if t_mons <> [||] then 
      make_battle route adv st tr t_mons t
    else run_battles route adv st t

and make_battle route adv st cpu_name cpu_mons bats = 
  let cpu_money = if cpu_name = "wild" then 0 else 
      Adventure.get_trainer_money adv cpu_name in 
  let (p, b, m, keep_going) = Battle.main
      (st.party, 
       st.bag, 
       st.money, 
       (cpu_mons),
       cpu_name,
       cpu_money,
       bats = [])
  in 
  let dt = if cpu_name <> "wild" 
    then cpu_name :: st.defeated_trainers 
    else st.defeated_trainers in 
  let dt_full = if cpu_name <> "wild" 
    then cpu_name :: st.defeated_trainers_full
    else st.defeated_trainers_full in 
  let st' = {st with
             party = p;
             bag = b;
             money = m;
             defeated_trainers = dt;
             defeated_trainers_full = dt_full} in 
  if keep_going then run_battles route adv st' bats
  else begin 
    Legal st' end 

let route r adv st = 
  try 
    let bats = Adventure.take_route adv st.cur_town r in 
    run_battles r adv st bats
  with 
  | Adventure.UnknownExit ex -> Illegal ("\nExit \"" ^ ex ^ "\" does not exist.\n")
  | Adventure.LockedExit ex -> Illegal ("\nIt's locked.\n")

(** [add_item st item new_amt] adds [new_amt] of [item] into the player's bag
    [st.bag]. *)
let add_item st item new_amt =
  match List.assoc_opt item st.bag with 
  | Some amt -> amt := (!amt + new_amt); st
  | None -> {st with bag = (item, ref new_amt) :: st.bag}

(** [remove_item it items] is the list [items] without element [it]. 
    Raises [UnknownItem it] if [it] is not in [items]. *)
let rec remove_item it = function
  | [] -> raise (ItemNotFound it)
  | h :: t -> if h = it then t else h::remove_item it t

let drop_item st it = failwith "drop unimplemented" 
(*{
    st with bag = remove_item it st.bag
  }*)

let bag st = st.bag

let get_party st = st.party

(** [has_badge st badges] is true if the player currently has an item of
    [badges] in their bag in [st]. *)
let rec has_badge st = function 
  | [] -> false
  | h :: t -> if (List.mem h st.bag) then true else has_badge st t

let get_def_tr st = st.defeated_trainers

let get_money st = st.money

let get_badges st = st.badges

let clear_def_trs st = { st with defeated_trainers = []}

(** [get_moves_list m] is the list of moves for pokemon [m] formated
    for JSON. *)
let get_moves_list mon = 
  PM.get_moves mon
  |> Array.to_list
  |> List.map (fun move -> `String (Moves.name move)) 

(** [form_party acc mons] is [mons] formated for JSON, using [acc]
    as an accumulator. *)
let rec form_party acc = function
  | [] -> ("party", `List (List.rev acc))
  | h::t -> 
    form_party (`Assoc [("name", `String (PM.get_name h));
                        ("hp", `Float(PM.get_hp h)); 
                        ("lvl", `Int (PM.get_lvl h));
                        ("xp", `Float (PM.get_xp h));
                        ("moves", `List (get_moves_list h))
                       ] :: acc) t

let save st = 
  let town = ("cur_town", `String (st.cur_town)) in 
  let bag_lst = 
    List.map (fun (name, amt) -> 
        `Assoc[("item_name", `String (Item.string_of_item name));
               ("ammount", `Int (!amt))]) 
      st.bag in 
  let bag = ("bag", `List bag_lst) in 
  let money = ("money",`Int !(st.money)) in 
  let party = form_party [] (st.party |> Array.to_list) in
  let defeated_trainers = 
    let lst = List.map (fun x -> `String x) st.defeated_trainers_full in 
    ("defeated trainers", `List lst)
  in 
  let badges = 
    let lst = List.map (fun x -> `String x) st.badges in 
    ("badges", `List lst) 
  in 
  let save = `Assoc[town; bag; money; party; defeated_trainers; badges] in 
  to_file "save.json" save

(** [json_item j] constructs a bag item entry by parsing [j]. *)
let json_item j = 
  let item = 
    j 
    |> member "item_name"
    |> to_string
    |> Item.item_of_string in 
  let amt = 
    j 
    |> member "ammount"
    |> to_int
  in 
  (item, ref amt)

let load_pokemon j = 
  let mon = Adventure.json_t_pokemon j in 
  let xp = j |> member "xp" |> to_float in 
  let hp = j |> member "hp" |> to_float in 
  PM.set_hp mon hp;
  PM.set_xp mon xp;
  mon


let load () = 
  let json = Yojson.Basic.from_file "save.json" in 
  {
    cur_town = 
      json 
      |> member "cur_town"
      |> to_string;
    bag = 
      json
      |> member "bag"
      |> to_list
      |> List.map json_item;
    money = 
      json |> member "money" |> to_int|> ref;
    party = 
      json 
      |> member "party" 
      |> to_list 
      |> List.map load_pokemon
      |> Array.of_list;
    defeated_trainers = [];
    defeated_trainers_full = 
      json
      |> member "defeated trainers"
      |> to_list
      |> List.map to_string;
    badges = 
      json
      |> member "badges"
      |> to_list
      |> List.map to_string;
  }
