open Moves

exception UnknownMove of string

module type StatsSig = sig
  type t = Yojson.Basic.t
  val mon: string ref
  val get_data: string -> Yojson.Basic.t
end

module Stats : StatsSig

module type PokeSig = sig
  exception UnknownMove of string
  type t_type = string 
  type t_hp = float
  type t_attack = float
  type t_defense = float
  type t_speed = float 
  type t_moves = Moves.t array
  type t_lvl = int
  type t_xp = float
  type t

  (** [set_file s] sets the json file containing all the pokemon, and 
      then returns unit. *)
  val set_file : string -> unit

  (** [lvl_up mon] levels up mon *)
  val lvl_up: t -> bool

  (** [create_pokemon name lvl] is the representation of the pokemon species 
      [name] and level [lvl]. *)
  val create_pokemon: string -> int -> Moves.t list -> t

  (** [get_max_hp mon] is the maximum hp of [mon]. *)
  val get_max_hp : t -> t_hp

  (** [change_hp mon hp] modifies the hp of [mon] by [hp].*)
  val change_hp : t -> t_hp -> unit

  (** incr_stats mon] increases the stats of [mon] based on its level. *)
  val incr_stats : t -> unit

  (** [fainted mon] is whether the hp of [mon] is 0. *)
  val fainted : t -> bool 

  (** [get_name mon] is the name of [mon]. *)
  val get_name : t -> string

  (** [get_type mon] is a list of the type(s) of [mon]. *)
  val get_type : t -> t_type list

  (** [get_moves mon] is a list of the moves for [mon]. *)
  val get_moves : t -> Moves.t array

  (** [get_hp mon] is the current hp of [mon]. *)
  val get_hp : t -> t_hp

  (** [get_attack mon is_spec] is the base attack of [mon]. *)
  val get_attack : t -> bool -> t_attack

  (** [get_defense mon is_spec] is the base defense of [mon]. *)
  val get_defense : t -> bool -> t_defense

  (** [get_speed mon] is the base speed of [mon]. *)
  val get_speed : t -> t_speed

  (** [get_move mon move] is the move with name [move] in [mon]'s moveset. *)
  val get_move : t -> int -> Moves.t

  (** [get_lvl mon] is the level of [mon]. *)
  val get_lvl : t -> t_lvl

  (** [get_xp mon is the current experience of [mon]. *)
  val get_xp : t -> float

  (** [get_status mon] is the current non-volatile status effect of [mon]. *)
  val get_status: t -> string

  (** [get_acc mon] is the accuracy of [mon]. *)
  val get_accuracy: t -> float

  (** [get_confusion mon] is the confusion state for [mon]. *)
  val get_confusion: t -> bool * int

  (** [get_sleep_counter mon] is the current sleep counter of [mon]. *)
  val get_sleep_counter: t -> int

  (** [set_hp mon hp] sets the hp of [mon] to [hp].*)
  val set_hp : t -> t_hp -> unit

  (** [set_xp mon xp] sets the xp of [mon] to [xp]. *)
  val set_xp : t -> t_xp -> unit

  (** [set_status mon st] sets the status [st] to the current status of [mon],
      if [mon] does not currently have a status. Otherwise, does nothing. *)
  val set_status : t -> string -> unit

  (** [set_confusion mon conf] sets the current confusion state of [mon] to 
      [conf]. *)
  val set_confusion: t -> bool * int -> unit

  (** [set_sleep_counter mon c] sets the sleep counter of [mon] to [c]. *)
  val set_sleep_counter: t -> int -> unit

  (** [rem_status mon] removes the status of [mon]. *)
  val rem_status : t -> unit

  (** [change_stage mon st add] changes the stage represented by name [st]
      for pokemon [mon] by the ammount [add]. 
      Valid strings for [st] are:
      [attack], [special attack], [defense], [special defense], [speed],
      [accuracy], and [evasion]. *)
  val change_stage: t -> string -> int -> unit

  (**[reset_stages_all mon] resets the stat changes for [mon]. *)
  val reset_stages: t -> unit

  (** [format_stages mon] is the string representation of the current
      stages of [mon]. *)
  val format_stages: t -> string

  (** [format_moves_names mon] is a formatted output of names of [mon]'s  moves.*)
  val format_moves_names : t -> string

  (** [format_moves_all mon] is a formatted output of all moves info for [mon]'s
      moveset. *)
  val format_moves_all: t -> string

  (** [retreat party] is true if all mons in [party] are fainted. *)
  val retreat: t array -> bool

  (** [alive_pmons mon_lst] returns a list of the pokemon in the party that 
      are alive (not fainted). *)
  val alive_pmons: t array -> t array

  (** [hp_string m] is a string representation of pokemon [m]'s 
      health and max health. *)
  val hp_string: t -> string 

  (** [string_of_mon mon] is the string representation of pokemon [mon]. *)
  val string_of_mon: t -> string 

  (** [string_of_mons mons] is the string representation of the list of 
      pokemons [mons]*)
  val string_of_mons: t array -> string 

  (** [restore mons] restores all mons to full health*)
  val restore_mons: t array -> unit

  (** [give_xp mon cpu_lvl wild] gives [mon] the correct ammount of experience
      after defeating the pokemon of lvl [cpu_lvl]. [wild] is true 
      if this pokemon was a wild pokemon, and false otherwise. *)
  val give_xp: t -> t_lvl -> bool -> unit

  (** [add_mon mons m] is the array resulting from adding [m] to array [mons].
      Requires: length of [mons] is less than 6. *)
  val add_mon: t array -> t -> t array 

  (** [get_new_move mon lvl] is [Some m] if the pokemon can learn move [m]
      at level [lvl], and is None if no move can be learned at this level. *)
  val get_new_move: t -> t_lvl -> Moves.t option

  (** [add_move mon i mov] adds move [mov] to [mon]'s move list at spot [i]. *)
  val add_move: t-> int -> Moves.t -> unit

  (** [evolve m] is the evolution of pokemon [m] if [m] is at a 
      high enough level. If not at a high enough level or cannot evolve,
      returns [m]. *)
  val evolve: t -> t * bool

end

module Pokemon : PokeSig