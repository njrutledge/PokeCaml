(** [MoveSig] defines a Pokemon's move. *)
module type MoveSig = sig
  (** [t] is the type of a move. *) 
  type t = {
    move_name: string;
    description: string;
    power : float;
    accuracy: float;
    el_type: Types.t
  }
  (** [create_move m] is the move created by parsing moves.json, 
      grabbing all data for move with name [m]. *)
  val create_move: string -> t

  (** [move_name t] is the name of move [t]. *)
  val name: t -> string

  (** [to_string t] is the string representation of move [t]. *)
  val to_string: t -> string

end 
(** TODO *)
module Moves : MoveSig