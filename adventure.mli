open Pokemon
(** [PM] is an instance of the Pokemon module. *)
module PM = Pokemon
(** 
   Representation of static adventure data.

   This module represents the data stored in adventure files, including
   the towns, exits, routes, battles, and badges.
   It handles loading of that data from JSON as well as querying the data.
*)

(** The type of item names. *)
type badge_name = string

(** [UnknownBadge] is raised when an invalid badge name is found. *)
exception UnknownBadge of badge_name

(** [bat] defines the type of the battle, i.e. against a trainer or a wild 
    pokemon. *)
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

(** [json_t_pokemon j] is the pokemon that [j] represents.
    Requires: [j] is a valid JSON pokemon representation. *)
val json_t_pokemon: Yojson.Basic.t -> PM.t

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

(** [req_badge a t r] is the badge than can unlock route [r] 
    of town [t] of adventure [a]. 
    Raises [UnknownTown t] if [t] is not a town identifier in [a].
    Raises [UnknownExit r] if [r] is not an exit from town [r] in [a]. *)
val req_badge : t -> town_id -> exit_name -> badge_name 

(** [take_route adv t route] is the list of battles needed to fight in order
    to successfuly taking the route [r] in town [t] of adventure [adv]. *)
val take_route : t -> town_id -> exit_name -> bat list

(** [get_wild a r] is a randomly selected pokemon from the route [r]'s wild 
    pokmemon list in adventure [a]. *)
val get_wild : t-> exit_name -> PM.t array

(** [get_t_mons a tr] is the pokemon list associated with trainer [tr] in 
    adventure [a]. *)
val get_t_mons : t -> string -> PM.t array

(** [get_defeat a tr] is whether trainer [tr] has been defeated. *)
val get_defeat : t -> string -> bool

(** [defeat_trainers a trs] is the adventure [a] with 
    trainers [trs] set as defeated.*)
val defeat_trainers : t -> string list -> t

(** [get_trainer_money adv s] -> is the money earned after beating trainer
    with name [s] in adventure [adv]. *)
val get_trainer_money : t -> string -> int

(** [get_badge adv route] is the badge in [adv] on [route]. *)
val get_badge : t -> string -> string