open Pokemon
module PM = Pokemon

let () = PM.set_file "testmons.json"

exception ItemNotFound of Adventure.item_name
exception KeyNotFound

type t = {
  cur_town : Adventure.town_id;
  last_town : Adventure.town_id;
  bag : string list;
  money : int;
  party: PM.t list
}

let init_state adv = {
  cur_town = Adventure.start_town adv;
  last_town = Adventure.start_town adv;
  bag = [];
  money = 500;
  party = [PM.create_pokemon "Mon1" 1.];
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

let route ex adv st = 
  try 
    let (bats, next) = Adventure.take_route adv st.cur_town ex in 
    let rec run_battles st' = function 
      | [] -> go ex adv st
      | h :: t -> let (p, b, m, keep_going) = Battle.main
                      (st.party, 
                       st.bag, 
                       st.money, 
                       [PM.create_pokemon "Mon4" 1.]) 
        in let st'' = {st with party = p; bag = b; money = m} in 
        if keep_going then run_battles st'' t 
        else 
          Legal st''

    in run_battles st bats
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
  | h::t -> if h = it then t else h::remove_item it t

let drop_item st it = {
  st with bag = remove_item it st.bag
}

let bag st = 
  st.bag

let get_party st = st.party

(** [has_key st keys] is true if the adventurer currently has an item of [keys]
    in their bag in [st]. *)
let rec has_key st = function 
  | [] -> false
  | h::t -> if (List.mem h st.bag) then true else has_key st t
