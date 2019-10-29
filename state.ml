exception ItemNotFound of Adventure.item_name
exception KeyNotFound

type t = {
  cur_town : Adventure.town_id;
  visited_towns : Adventure.town_id list;
  bag : string list;
}

let init_state adv = {
  cur_town = Adventure.start_town adv;
  visited_towns = [Adventure.start_town adv];
  bag = [];
}

let current_town_id st =
  st.cur_town

let visited st =
  st.visited_towns

type result = Legal of t | Illegal of string

let go ex adv st =
  try 
    Legal{
      cur_town = Adventure.next_town adv st.cur_town ex;
      visited_towns = List.sort_uniq compare
          (Adventure.next_town adv st.cur_town ex :: st.visited_towns);
      bag = st.bag;
    }
  with 
  | Adventure.UnknownExit ex -> Illegal ("\nExit \"" ^ ex ^ "\" does not exist.\n")
  | Adventure.LockedExit ex -> Illegal ("\nIt's locked.\n")

let calc_item_score adv = 
  Adventure.town_items adv (Adventure.treasure_town_id adv)
  |> List.fold_left (fun acc item -> acc + Adventure.item_score adv item) 0   

let calc_score adv st = 
  visited st 
  |> List.map (Adventure.town_score adv)
  |> List.fold_left (+) 0
  |> (+) (calc_item_score adv)

let add_item st it = {
  st with bag = it::st.bag
}

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

(** [has_key st keys] is true if the adventurer currently has an item of [keys]
    in their bag in [st]. *)
let rec has_key st = function 
  | [] -> false
  | h::t -> if (List.mem h st.bag) then true else has_key st t

let lock_door adv st ex = 
  if has_key st (Adventure.keys adv st.cur_town ex) then 
    Adventure.lock adv st.cur_town ex else
    raise KeyNotFound

let unlock_door adv st ex = 
  if has_key st (Adventure.keys adv st.cur_town ex) then
    Adventure.unlock adv st.cur_town ex else
    raise KeyNotFound