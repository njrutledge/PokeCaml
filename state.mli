(** 
   Representation of dynamic adventure state.

   This module represents the state of an adventure as it is being played,
   including the adventurer's current town, the towns that have been visited,
   and functions that cause the state to change.
*)
open Pokemon
module PM = Pokemon
(** Raised when an item is not found in the adventurer's bag. *)
exception ItemNotFound of Adventure.item_name

(** Raised when the adventurer tries to lock or unlock a door without a valid
    key in their bag. *)
exception KeyNotFound
(**********************************************************************
 * DO NOT CHANGE THIS CODE
 * It is part of the interface the course staff will use to test your 
 * submission.
*)

(** The abstract type of values representing the game state. *)
type t 

(** [init_state a] is the initial state of the game when playing adventure [a]. 
    In that state the adventurer is currently located in the starting town,
    and they have visited only that town. *)
val init_state : Adventure.t -> t

(** [current_town_id st] is the identifier of the town in which the adventurer
    currently is located in state [st]. *)
val current_town_id : t -> string

(** [visited st] is a set-like list of the town identifiers the adventurer has 
    visited in state [st]. The adventurer has visited a town [rm] if their
    current town location is or has ever been [rm]. *)
val visited : t -> string list

(** The type representing the result of an attempted movement. *)
type result = Legal of t | Illegal of string

(** [go exit adv st] is [r] if attempting to go through exit [exit] in state 
    [st] and adventure [adv] results in [r].  If [exit] is an exit from the 
    adventurer's current town, then [r] is [Legal st'], where in [st'] the 
    adventurer is now located in the town to which [exit] leads.  Otherwise, 
    the result is [Illegal]. 
    Effects: none.  [go] is not permitted to do any printing. *)
val go : Adventure.exit_name -> Adventure.t -> t -> result

(* END DO NOT CHANGE
 **********************************************************************)


(*(** [exit_msg st] is message from the last exit the adventurer has gone through
    in state [st]. *)
  val exit_msg : t -> string*)

(** [add_item st it] adds [it] to the adventurer's bag in [st]. *)
val add_item : t -> Adventure.item_name -> t

(** [drop_item st it] removes [it] from the adventurer's bag in [st]. 
    Raises [ItemNotFound it] if [it] is not in the adventurer's bag. *)
val drop_item : t -> Adventure.item_name -> t

(** [bag st] is the bag of state [st]. *)
val bag : t -> Adventure.item_name list

(** [get_party st] is the current party (Pokemon.t list) of the player. *)
val get_party : t -> PM.t list
