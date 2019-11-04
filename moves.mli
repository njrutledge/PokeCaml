(** TODO *)
module type MoveSig = sig 
  type t = {
    move_name: string;
    description: string;
    power : float;
    accuracy: float;
    el_type: Types.t list
  }
  val create_move: string -> t

end 
(** TODO *)
module Moves : MoveSig