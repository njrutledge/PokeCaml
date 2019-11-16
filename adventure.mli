open Pokemon
module PM = Pokemon
(** 
   Representation of static adventure data.

   This module represents the data stored in adventure files, including
   the towns and exits.  It handles loading of that data from JSON as well
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

type bat = 
  | Wild
  | Trainer of string
  (** The abstract type of values representing adventures. *)
type t

(** The type of town identifiers. *)
type town_id = string

(** The type of exit names. *)
type exit_name = string

(** Raised when an unknown town is encountered. *)
exception UnknownTown of town_id

(** Raised when an unknown exit is encountered. *)
exception UnknownExit of exit_name

(** Raised when the exit used is locked. *)
exception LockedExit of exit_name

(** [from_json j] is the adventure that [j] represents.
    Requires: [j] is a valid JSON adventure representation. *)
val from_json : Yojson.Basic.t -> t

(** [start_town a] is the identifier of the starting town in adventure 
    [a]. *)
val start_town : t -> town_id

(** [town_ids a] is a set-like list of all of the town identifiers in 
    adventure [a]. *)
val town_ids : t -> town_id list

(** [description a r] is the description of town [r] in adventure [a]. 
    Raises [UnknownTown r] if [r] is not a town identifier in [a]. *)
val description : t -> town_id -> string

(** [exits a r] is a set-like list of all exit names from town [r] in 
    adventure [a].
    Raises [UnknownTown r] if [r] is not a town identifier in [a]. *)
val exits : t -> town_id -> exit_name list

(** [next_town a r e] is the town to which [e] exits from town [r] in
    adventure [a].  
    Raises [UnknownTown r] if [r] is not a town identifier in [a].
    Raises [UnknownExit e] if [e] is not an exit from town [r] in [a]. *)
val next_town : t -> town_id -> exit_name -> town_id

(** [next_towns a r] is a set-like list of all towns to which there is an exit 
    from [r] in adventure [a]. 
    Raises [UnknownTown r] if [r] is not a town identifier in [a].*)
val next_towns : t -> town_id -> town_id list

(* END DO NOT CHANGE
 **********************************************************************)

(*(** [msg a r ex] is the [msg] of the exit [ex] of town [r] in adventure [a]. 
    Raises [Unkowntown r] if [r] is not a town identifier in [a]. 
    Raises [UnknownExit ex ] if [ex] is not an exit from town [r] in [a]. *)
  val exit_msg : t -> town_id -> exit_name -> string

  (** [ex_score a r ex] is the score of using exit [ex] of 
    town [r] in adventure [a]. 
    Raises [Unkowntown r] if [r] is not a town identifier in [a]. *)
  val exit_score : t -> town_id -> exit_name -> int*)

(** [win_msg a sc] is the message displayed when the adventurer 
    wins adventure [a] with score [sc]. *)
val win_msg : t -> int -> string


(** [keys a r e] is the list of all items than can unlock exit [e] of town [r]
    of adventure [r]. 
    Raises [UnknownTown r] if [r] is not a town identifier in [a].
    Raises [UnknownExit e] if [e] is not an exit from town [r] in [a]. *)
val keys : t -> town_id -> exit_name -> item_name list

val take_route : t -> town_id -> exit_name -> bat list * town_id

(** [get_wild a r] is a randomly selected pokemon from the route [r]'s wild 
    pokmemon list in adventure [a]. *)
val get_wild : t-> exit_name -> PM.t list