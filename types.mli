
(** [UnknownType t] is raised when a pokemon or move tries to use a move of 
    a non-known type. *)
exception UnknownType of string

(** [hash lst] is a function that creates the hash function for converting types
    of pokemon as string into integers. The returned function will 
    raise [UnknownType t] when applied to [t] if that type is not valid. *)
val make_hash: string list -> (string-> int)

(** [type_matrix j] is TODO *)
val type_matrix: Yojson.Basic.t -> float array array * (string -> int) 