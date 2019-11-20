(* Note: You may introduce new code anywhere in this file. *) 

type object_phrase = string list

type command = 
  | Attack of int
  | Item of object_phrase
  | Bag
  | MovesInfo
  | Party
  | Run
  | Switch
  | Quit

exception Empty

exception Malformed

(** [rem_spaces lst] removes any empty string elements of [lst]. *)
let rec rem_empty = function
  | [] -> []
  | h :: t -> if h = "" then rem_empty t else h :: rem_empty t

(** [make_command verb phrase] creates a command of type [verb] with 
    [object_phrase] [phrase]. Raises Malformed if incorrect command. *)
let make_command verb phrase = 
  if phrase = [] then 
    if verb = "quit" then Quit else
    if verb = "moves" then MovesInfo else
    if verb = "party" then Party else 
    if verb = "run" then Run else 
    if verb = "switch" then Switch else 
    if verb = "bag" then Bag else
      raise Malformed 
  else
  if verb = "attack" then begin
    match phrase with 
    | h :: [] -> begin
        try Attack (int_of_string h)
        with _ -> raise Malformed 
      end
    | _ -> raise Malformed end
  else
  if verb = "item" then Item phrase else 
    raise Malformed 

let parse str =
  String.split_on_char ' ' str 
  |> rem_empty
  |> function
  | [] -> raise Empty
  | h :: t -> make_command h t
