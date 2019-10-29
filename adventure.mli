(** 
   Representation of static adventure data.

   This module represents the data stored in adventure files, including
   the rooms and exits.  It handles loading of that data from JSON as well
   as querying the data.
*)
(** The type of item names. *)
type item_name = string

(** Raised when an unkown item is encountered. *)
exception UnknownItem of item_name

(**********************************************************************
 * DO NOT CHANGE THIS CODE
 * It is part of the interface the course staff will use to test your 
 * submission.
*)

(** The abstract type of values representing adventures. *)
type t

(** The type of room identifiers. *)
type room_id = string

(** The type of exit names. *)
type exit_name = string

(** Raised when an unknown room is encountered. *)
exception UnknownRoom of room_id

(** Raised when an unknown exit is encountered. *)
exception UnknownExit of exit_name

(** Raised when the exit used is locked. *)
exception LockedExit of exit_name

(** [from_json j] is the adventure that [j] represents.
    Requires: [j] is a valid JSON adventure representation. *)
val from_json : Yojson.Basic.t -> t

(** [start_room a] is the identifier of the starting room in adventure 
    [a]. *)
val start_room : t -> room_id

(** [room_ids a] is a set-like list of all of the room identifiers in 
    adventure [a]. *)
val room_ids : t -> room_id list

(** [description a r] is the description of room [r] in adventure [a]. 
    Raises [UnknownRoom r] if [r] is not a room identifier in [a]. *)
val description : t -> room_id -> string

(** [exits a r] is a set-like list of all exit names from room [r] in 
    adventure [a].
    Raises [UnknownRoom r] if [r] is not a room identifier in [a]. *)
val exits : t -> room_id -> exit_name list

(** [next_room a r e] is the room to which [e] exits from room [r] in
    adventure [a].  
    Raises [UnknownRoom r] if [r] is not a room identifier in [a].
    Raises [UnknownExit e] if [e] is not an exit from room [r] in [a]. *)
val next_room : t -> room_id -> exit_name -> room_id

(** [next_rooms a r] is a set-like list of all rooms to which there is an exit 
    from [r] in adventure [a]. 
    Raises [UnknownRoom r] if [r] is not a room identifier in [a].*)
val next_rooms : t -> room_id -> room_id list

(* END DO NOT CHANGE
 **********************************************************************)

(** [score a r] is the score of room [r] in adventure [a]. 
    Raises [UnkownRoom r] if [r] is not a room identifier in [a]. *)
val room_score : t -> room_id -> int

(*(** [msg a r ex] is the [msg] of the exit [ex] of room [r] in adventure [a]. 
    Raises [UnkownRoom r] if [r] is not a room identifier in [a]. 
    Raises [UnknownExit ex ] if [ex] is not an exit from room [r] in [a]. *)
  val exit_msg : t -> room_id -> exit_name -> string

  (** [ex_score a r ex] is the score of using exit [ex] of 
    room [r] in adventure [a]. 
    Raises [UnkownRoom r] if [r] is not a room identifier in [a]. *)
  val exit_score : t -> room_id -> exit_name -> int*)

(** [take_item a r i] removes the item [i] in room [r] in adventure [a].
    Raises [UnknownItem i] if [i] is not an item identifier in [a], or if 
    [r] does not match the current room of the item. *)
val take_item : t -> room_id -> item_name -> t

(** [drop_item a r i] adds the item [i] to the room [r] in adventure [a]. 
    Raises [UnknownItem i] if [i] is not an item identifier in [a], or if
    the adventurer does not currently have [i]. *)
val drop_item : t -> room_id -> item_name -> t

(** [room_items a r] is a list of all items in room [r] of adventure [a]. *)
val room_items : t -> room_id -> item_name list

(** [treasure_room_id a] is the room identifier of the treasure room of 
    adventure [a]. *)
val treasure_room_id : t -> room_id

(** [treasure_room_needed_items a] is the list of items the adventurer needs
    to place in the treasure room of adventure [a] in order to win. *)
val treasure_room_needed_items : t -> item_name list

(** [win_msg a sc] is the message displayed when the adventurer 
    wins adventure [a] with score [sc]. *)
val win_msg : t -> int -> string

(** [item_score a i] is the score of the item [i] in adventure [a].
    Raises [UnknownItem i] if [i] is not an item identifier in [a]. *)
val item_score : t -> item_name -> int

(** [keys a r e] is the list of all items than can unlock exit [e] of room [r]
    of adventure [r]. 
    Raises [UnknownRoom r] if [r] is not a room identifier in [a].
    Raises [UnknownExit e] if [e] is not an exit from room [r] in [a]. *)
val keys : t -> room_id -> exit_name -> item_name list

(** [unlock a r e] unlocks exit [e] in room [r] of adventure [a]. 
    Requires adventurer has the correct key in their inventory. 
    Raises [UnknownRoom r] if [r] is not a room identifier in [a].
    Raises [UnknownExit e] if [e] is not an exit from room [r] in [a]. *)
val unlock : t -> room_id -> exit_name -> t

(** [lock a r e] locks exit [e] in room [r] of adventure [a]. 
    Requires adventurer has the correct key in their inventory. 
    Raises [UnknownRoom r] if [r] is not a room identifier in [a].
    Raises [UnknownExit e] if [e] is not an exit from room [r] in [a]. *)
val lock : t -> room_id -> exit_name -> t

(** [doot a r] is the secret doot ammount of the room [r] in adventure [a]. *)
val doot : t -> room_id -> string