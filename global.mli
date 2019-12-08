(** [change_speed ()] changes the sleep speed of the game. *)
val change_speed : unit -> unit 

(** [execute_quit ()] quits the game. *)
val execute_quit : unit -> 'a

(** [get_sleep_speed ()] is the current sleep speed for the game. *)
val get_sleep_speed : unit -> float

(** [get_y_n ()] is true if the user inputs an affirmative, false if negative, 
    quits the application if inputs quit, and loops otherwise. *)
val get_y_n : unit -> bool