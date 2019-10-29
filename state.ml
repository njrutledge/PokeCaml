exception ItemNotFound of Adventure.item_name
exception KeyNotFound

type t = {
  cur_room : Adventure.room_id;
  visited_rooms : Adventure.room_id list;
  inventory : string list;
}

let init_state adv = {
  cur_room = Adventure.start_room adv;
  visited_rooms = [Adventure.start_room adv];
  inventory = [];
}

let current_room_id st =
  st.cur_room

let visited st =
  st.visited_rooms

type result = Legal of t | Illegal of string

let go ex adv st =
  try 
    Legal{
      cur_room = Adventure.next_room adv st.cur_room ex;
      visited_rooms = List.sort_uniq compare
          (Adventure.next_room adv st.cur_room ex :: st.visited_rooms);
      inventory = st.inventory;
    }
  with 
  | Adventure.UnknownExit ex -> Illegal ("\nExit \"" ^ ex ^ "\" does not exist.\n")
  | Adventure.LockedExit ex -> Illegal ("\nIt's locked.\n")

let calc_item_score adv = 
  Adventure.room_items adv (Adventure.treasure_room_id adv)
  |> List.fold_left (fun acc item -> acc + Adventure.item_score adv item) 0   

let calc_score adv st = 
  visited st 
  |> List.map (Adventure.room_score adv)
  |> List.fold_left (+) 0
  |> (+) (calc_item_score adv)

let add_item st it = {
  st with inventory = it::st.inventory
}

(** [remove_item it items] is the list [items] without element [it]. 
    Raises [UnknownItem it] if [it] is not in [items]. *)
let rec remove_item it = function
  | [] -> raise (ItemNotFound it)
  | h::t -> if h = it then t else h::remove_item it t

let drop_item st it = {
  st with inventory = remove_item it st.inventory
}

let inventory st = 
  st.inventory

(** [has_key st keys] is true if the adventurer currently has an item of [keys]
    in their inventory in [st]. *)
let rec has_key st = function 
  | [] -> false
  | h::t -> if (List.mem h st.inventory) then true else has_key st t

let lock_door adv st ex = 
  if has_key st (Adventure.keys adv st.cur_room ex) then 
    Adventure.lock adv st.cur_room ex else
    raise KeyNotFound

let unlock_door adv st ex = 
  if has_key st (Adventure.keys adv st.cur_room ex) then
    Adventure.unlock adv st.cur_room ex else
    raise KeyNotFound