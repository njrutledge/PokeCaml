(** Representation of a move.

    This module represents the data stored in a move, including
    the name, pp, and stats of the move. *)

(** [MoveSig] defines a Pokemon's move. *)
module type MoveSig = sig
  (** [t] is the type of a move. *) 
  type t = {
    move_name: string;
    description: string;
    power : float;
    is_special: bool;
    accuracy: float;
    el_type: Types.t;
    status: string list;
    mutable pp : int;
    max_pp : int;

  }
  (** [create_move m] is the move created by parsing moves.json, 
      grabbing all data for move with name [m]. *)
  val create_move: string -> t

  (** [move_name t] is the name of move [t]. *)
  val name: t -> string

  (** [get_pp move] is the pp left for [move]. *)
  val get_pp: t -> int

  (** [get_max_pp move] is the max pp for [move]. *)
  val get_max_pp: t -> int

  (** [get_accuracy move] is the accuracy for [move]. *)
  val get_acc: t -> float

  (** [get_is_special move] is whether or not a move is a special move. If
      false, it is a physical move. *)
  val get_is_special: t -> bool

  (** [get_status move] is the list of status effects a move might cause. *)
  val get_status: t -> string list

  (** [set_pp move pp] sets the pp of [move] to [pp]. *)
  val set_pp: t -> int -> unit

  (** [decr_pp move] decrements the pp of [move] by 1. *)
  val decr_pp: t -> unit

  (** [to_string_name t] is the string representation of the name and pp of [t].
  *)
  val to_string_name : t -> string

  (** [to_string t] is the string representation of move [t]. *)
  val to_string: t -> string

end 
(** [Moves] is the module handing move creation and getting stats
    and discriptions about moves. *)
module Moves : MoveSig