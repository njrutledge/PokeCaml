(**
   Parsing of player commands.
*)

(** The type [object_phrase] represents the object phrase that can be part of a 
    player command.  Each element of the list represents a word of the object 
    phrase, where a {i word} is defined as a consecutive sequence of non-space 
    characters.  Thus, no element of the list should contain any leading,
    internal, or trailing spaces.  The list is in the same order as the words 
    in the original player command.  For example:
    - If the player command is ["go clock tower"], then the object phrase is 
      [["clock"; "tower"]].
    - If the player command is ["go clock     tower"], then the object phrase is
      again [["clock"; "tower"]]. 

    An [object_phrase] is not permitted to be the empty list. *)
type object_phrase = string list

(** The type [command] represents a player command that is decomposed
    into a verb and possibly an object phrase. *)
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

(** Raised when an empty command is parsed. *)
exception Empty

(** Raised when a malformed command is encountered. *)
exception Malformed

(** [parse str] parses a player's input into a [command], as follows. The first
    word (i.e., consecutive sequence of non-space characters) of [str] becomes 
    the verb. The rest of the words, if any, become the object phrase.
    Examples: 
    - [parse "    go   route   3110   "] is [Go ["route"; "3110"]]
    - [parse "quit"] is [Quit]. 

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space 
    characters (only ASCII character code 32; not tabs or newlines, etc.).

    Raises: [Empty] if [str] is the empty string or contains only spaces. 

    Raises: [Malformed] if the command is malformed. A command
    is {i malformed}:
      if the verb is "shop" and there is a non-empty object phrase,
      or if the verb is "badges" and there is a non-empty object phrase,
      or if the verb is "bag" and there is a non-empty object phrase,
      or if the verb is "heal" and there is a non-empty object phrase,
      or if the verb is "party" and there is a non-empty object phrase,
      or if the verb is "map" and there is a non-empty object phrase,
      or if the verb is "quit" and there is a non-empty object phrase,
      or if the verb is "save" and there is a non-empty object phrase
      or if the verb is "pc" and there is a non-empty object phrase,
      or if the verb is "buy" and there is an empty object phrase,
      or if the verb is "moves" and there is an empty object phrase,
      or if the verb is "go" and there is an empty object phrase,
      or if the verb is "info" and there is an empty object phrase,
      or if the verb is "switch" and there is an empty object phrase,
      or if the verb is "swap" and there is an empty object phrase,
      or if the verb is anything else. *)
val parse : string -> command