open Pokemon
open Moves
module PM = Pokemon

exception ItemNotFound of Adventure.badge_name
exception BadgeNotFound

type t = {
  cur_town : Adventure.town_id;
  last_town : Adventure.town_id;
  bag : (Item.t * int ref) list;
  money : int ref;
  party: PM.t array;
  defeated_trainers: string list;
  badges: string list;
}

let init_state adv = {
  cur_town = Adventure.start_town adv;
  last_town = Adventure.start_town adv;
  bag = [(Potion, ref 5); (HyperPotion, ref 1);
         (PokeBall, ref 5); (GreatBall, ref 1)];
  money = ref 1000;
  party = [|(PM.create_pokemon "Pikachu" 5 [Moves.create_move "thundershock";
                                            Moves.create_move "thunder wave";
                                            Moves.create_move "rest"]);
            (PM.create_pokemon "Charmander" 5 [Moves.create_move "scratch"]);
            (PM.create_pokemon "Squirtle" 5 [Moves.create_move "tackle";]);|];
  defeated_trainers = [];
  badges = []
}

let current_town_id st =
  st.cur_town

let last st =
  st.last_town

type result = Legal of t | Illegal of string

let go ex adv st =
  try 
    Legal{
      cur_town = Adventure.next_town adv st.cur_town ex;
      last_town = Adventure.next_town adv st.cur_town ex;
      bag = st.bag;
      money = st.money;
      party = st.party;
      defeated_trainers = st.defeated_trainers;
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
  let st' = {st with party = p; bag = b; money = m; defeated_trainers = dt} in 
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