(** Representation of an item.

    This module represents the data stored in an item, including
    the the type and cost of the item. *)

(** [InvalidItem s] is raised if an invalid item [s] is attempted to be
    accessed, created, etc. (should never really happen). *)
exception InvalidItem of string

(** [t] is the type of an item, and the variant names are self explanatory. *)
type t =
  | Potion 
  | SuperPotion
  | HyperPotion
  | FullRestore
  | PokeBall
  | GreatBall
  | UltraBall
  | MasterBall
  | Antidote
  | ParalyzeHeal
  | Awakening
  | IceHeal
  | BurnHeal
  | FullHeal

(** [string_of_item i] is the string representation of item [i]. *)
val string_of_item : t -> string

(** [item_of_string s] is the item corresponding to string [s]. *)
val item_of_string : string -> t

(** [cost_of_item itm] is the cost of the string [itm]. *)
val cost_of_item : string -> int

(** [format_items itm_lst] returns a formatted string of the items in [itm_lst]
    and their costs. *)
val format_items : string list -> string