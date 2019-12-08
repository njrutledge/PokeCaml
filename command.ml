(* Note: You may introduce new code anywhere in this file. *) 

type object_phrase = string list

type command = 
  | Buy of object_phrase
  | Moves of object_phrase
  | Go of object_phrase
  | GoRoute of object_phrase
  | Shop
  | Badges
  | Bag
  | Heal
  | Party
  | Map
  | Quit
  | Save
  | TGM
  | PC
  | Info of object_phrase
  | Switch of object_phrase
  | Swap of object_phrase

exception Empty

exception Malformed

(** [rem_spaces lst] removes any empty string elements of [lst]. *)
let rec rem_empty = function
  | [] -> []
  | h::t -> if h = "" then rem_empty t else h::rem_empty t

(** [make_command verb phrase] creates a command of type [verb] with 
    [object_phrase] [phrase]. Raises Malformed if incorrect command. *)
let make_command verb phrase = 
  if phrase = [] then 
    match verb with  
    | "quit" -> Quit  
    | "bag" -> Bag  
    | "party" -> Party 
    | "heal" -> Heal 
    | "map" -> Map 
    | "badges" -> Badges
    | "save" -> Save
    | "shop" -> Shop
    | "tgm" -> TGM
    | "pc" -> PC
    | _ -> raise Malformed
  else
  if verb = "go" then begin 
    match phrase with 
    | [] -> raise Malformed
    | h :: t -> if h = "route" || h = "gym" then GoRoute phrase else Go phrase
  end 
  else 
  if verb = "buy" then Buy phrase else
  if verb = "moves" then Moves phrase else
  if verb = "info" then Info phrase else 
  if verb = "switch" then Switch phrase else
  if verb = "swap" then Swap phrase else 
    raise Malformed 

let parse str =
  String.split_on_char ' ' str 
  |> rem_empty
  |> function
  | [] -> raise Empty
  | h::t -> make_command h t
