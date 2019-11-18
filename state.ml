open Pokemon
module PM = Pokemon

let () = PM.set_file "testmons.json"

exception ItemNotFound of Adventure.item_name
exception KeyNotFound

type t = {
  cur_town : Adventure.town_id;
  last_town : Adventure.town_id;
  bag : (Item.t * int ref) list;
  money : int;
  party: PM.t ref list
}

let init_state adv = {
  cur_town = Adventure.start_town adv;
  last_town = Adventure.start_town adv;
  bag = [(Potion, ref 5); (PokeBall, ref 3)];
  money = 500;
  party = [ref (PM.create_pokemon "Mon1" 1.); ref (PM.create_pokemon "Mon2" 2.);
           ref (PM.create_pokemon "Mon3" 3.;)]
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
    }
  with 
  | Adventure.UnknownExit ex -> Illegal ("\nExit \"" ^ ex ^ "\" does not exist.\n")
  | Adventure.LockedExit ex -> Illegal ("\nIt's locked.\n")

let rec run_battles route adv st = function 
  | [] -> go route adv st
  | Adventure.Wild :: t -> make_battle route adv st "wild" (Adventure.get_wild adv route) t
  | Adventure.Trainer tr :: t -> 
    let t_mons = Adventure.get_t_mons adv tr in 
    make_battle route adv st tr t_mons t

and make_battle route adv st cpu_name cpu_mons bats = 
  let (p, b, m, keep_going) = Battle.main
      (st.party, 
       st.bag, 
       st.money, 
       (cpu_mons),
       cpu_name) 
  in let st' = {st with party = p; bag = b; money = m} in 
  if keep_going then run_battles route adv st' bats
  else 
    Legal st'

let route r adv st = 
  try 
    let bats = Adventure.take_route adv st.cur_town r in 
    run_battles r adv st bats
  with 
  | Adventure.UnknownExit ex -> Illegal ("\nExit \"" ^ ex ^ "\" does not exist.\n")
  | Adventure.LockedExit ex -> Illegal ("\nIt's locked.\n")

let add_item st it = 
  failwith ("Unimplemented: must check money first")
(*  {
    st with bag = it::st.bag
    }*)

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

(** [has_key st keys] is true if the adventurer currently has an item of [keys]
    in their bag in [st]. *)
let rec has_key st = function 
  | [] -> false
  | h :: t -> if (List.mem h st.bag) then true else has_key st t
