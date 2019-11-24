(* Note: You may introduce new code anywhere in this file. *) 

type object_phrase = string list

type command = 
  | Go of object_phrase
  | Take of object_phrase
  | Bag
  | Party
  | GoRoute of object_phrase
  | Heal 
  | Buy of object_phrase
  | Moves of object_phrase
  | Map 
  | Quit

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
    if verb = "quit" then Quit else 
    if verb = "bag" then Bag else 
    if verb = "party" then Party else
    if verb = "heal" then Heal else
    if verb = "map" then Map else
      raise Malformed 
  else
  if verb = "go" then begin 
    match phrase with 
    | [] -> raise Malformed
    | h :: t -> if h = "route" || h = "gym" then GoRoute phrase else Go phrase
  end 
  else 
  if verb = "route" then GoRoute phrase else
  if verb = "take" then Take phrase else 
  if verb = "buy" then Buy phrase else
  if verb = "moves" then Moves phrase else
    raise Malformed 

let parse str =
  String.split_on_char ' ' str 
  |> rem_empty
  |> function
  | [] -> raise Empty
  | h::t -> make_command h t
