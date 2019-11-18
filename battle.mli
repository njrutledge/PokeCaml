open Pokemon
module PM = Pokemon

(** [main party bag money enemy_mons] takes in the party, bag, and money of
    the player as well as the enemy pokemon and returns the updated party, bag,
    and amount of the money the player has. *)
val main: PM.t ref list * (Item.t * int ref) list * int * PM.t ref list *string 
  -> PM.t ref list * (Item.t * int ref) list * int * bool