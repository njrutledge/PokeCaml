(** [t] is the type of pokemon types. *)
type t = string 

(** [UnknownType t] is raised when a pokemon or move tries to use a move of 
    a non-known type. *)
exception UnknownType of t

(** [type_matrix_and_hash j] is the tuple of the type_matrix created by parsing 
    [j] and the hashing function that when given a type [t], returns the 
    index of the row/column associated with [t]. The structure of the
    type matrix means the row index [t] equals col index of [t]. *)
val type_matrix_and_hash: Yojson.Basic.t -> float array array * (t -> int) 