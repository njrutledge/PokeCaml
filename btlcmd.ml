(* Note: You may introduce new code anywhere in this file. *) 

type object_phrase = string list

type command = 
  | Attack of object_phrase
  | Item of object_phrase
  | MovesInfo
  | Party
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
    if verb = "moves" then MovesInfo else
    if verb = "party" then Party else 
      raise Malformed 
  else
  if verb = "attack" then Attack phrase else
  if verb = "item" then Item phrase else 
    raise Malformed 

let parse str =
  String.split_on_char ' ' str 
  |> rem_empty
  |> function
  | [] -> raise Empty
  | h :: t -> make_command h t
