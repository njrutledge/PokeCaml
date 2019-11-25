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
    badge in their bag. *)
exception BadgeNotFound
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

(** [last st] is the town identifier of the last town the adventurer has 
    visited in state [st]. *)
val last : t -> string

(** The type representing the result of an attempted movement. *)
type result = Legal of t | Illegal of string

(** [go exit adv st] is [r] if attempting to go through exit [exit] in state 
    [st] and adventure [adv] results in [r].  If [exit] is an exit from the 
    adventurer's current town, then [r] is [Legal st'], where in [st'] the 
    adventurer is now located in the town to which [exit] leads.  Otherwise, 
    the result is [Illegal]. 
    Effects: none.  [go] is not permitted to do any printing. *)
val go : Adventure.exit_name -> Adventure.t -> t -> result

val route : Adventure.exit_name -> Adventure.t -> t -> result

(* END DO NOT CHANGE
 **********************************************************************)

(*(** [exit_msg st] is message from the last exit the trainer has gone through
    in state [st]. *)
  val exit_msg : t -> string*)

(** [add_item st it n] adds [n] items [it] to the trainer's bag in [st],
    and returns the new state. *)
val add_item : t -> Item.t -> int -> t

(** [drop_item st it] removes [it] from the trainer's bag in [st]. 
    Raises [ItemNotFound it] if [it] is not in the trainer's bag. *)
val drop_item : t -> Adventure.item_name -> t

(** [bag st] is the bag of state [st]. *)
val bag : t -> (Item.t * int ref) list

(** [get_party st] is the current party (Pokemon.t list) of the player. *)
val get_party : t -> PM.t array

(** [get_def_tr st] returns the defeated trainers in state [st]. *)
val get_def_tr: t -> string list

(** [get_money st] returns the current money in state [st]. *)
val get_money: t -> int ref

(** [get_badges st] returns the badges currently obtained in state [st]. *)
val get_badges: t -> string list 