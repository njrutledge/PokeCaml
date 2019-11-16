open Pokemon
module PM = Pokemon

(** [main party bag money enemy_mons] takes in the party, bag, and money of
    the player as well as the enemy pokemon and returns the updated party, bag,
    and amount of the money the player has. *)
val main: PM.t list * 'a list * int * PM.t list -> PM.t list * 'a list * int * bool