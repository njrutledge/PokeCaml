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
  type t (*= {
             el_type: t_type list;
             mutable name : string;
             mutable max_hp : t_hp;
             mutable hp: t_hp;
             mutable lvl: float;
             mutable attack: t_attack;
             mutable defense: t_defense;
             mutable speed: t_speed;
             mutable moves: t_moves;
             evolution: string;
             }*)

  (** [set_file s] sets the json file containing all the pokemon, and 
      then returns unit. *)
  val set_file : string -> unit

  (** [lvl_up mon] levels up mon *)
  val lvl_up: t -> bool

  (** [create_pokemon name lvl] is the representation of the pokemon species 
      [name] and level [lvl]. *)
  val create_pokemon: string -> float -> t

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

  (** [get_attack mon] is the base attack of [mon]. *)
  val get_attack : t -> t_attack

  (** [get_defense mon] is the base defense of [mon]. *)
  val get_defense : t -> t_defense

  (** [get_speed mon] is the base speed of [mon]. *)
  val get_speed : t -> t_speed

  (** [get_move mon move] is the move with name [move] in [mon]'s moveset. *)
  val get_move : t -> int -> Moves.t

  (** [get_lvl mon] is the level of [mon]. *)
  val get_lvl : t -> float

  (** [get_xp mon is the current experience of [mon]. *)
  val get_xp : t -> float

  (** [set_hp mon hp] sets the hp of [mon] to [hp].*)
  val set_hp : t -> t_hp -> unit

  (** [format_moves_names mon] is a formatted output of names of [mon]'s  moves.*)
  val format_moves_names : t -> string

  (** [format_moves_all mon] is a formatted output of all moves info for [mon]'s
      moveset. *)
  val format_moves_all: t -> string

  (** [retreat party] is true if all mons in [party] are fainted. *)
  val retreat: t ref list -> bool

  (** [alive_pmons mon_lst] returns a list of the pokemon in the party that 
      are alive (not fainted). *)
  val alive_pmons: t ref list -> t ref list

  (** [hp_string m] is a string representation of pokemon [m]'s 
      health and max health. *)
  val hp_string: t -> string 

  (** [string_of_mon mon] is the string representation of pokemon [mon]. *)
  val string_of_mon: t -> string 

  (** [string_of_mons mons] is the string representation of the list of 
      pokemons [mons]*)
  val string_of_mons: t ref list -> string 

  (** [restore mons] restores all mons to full health*)
  val restore_mons: t ref list -> unit

  (** [give_xp mon cpu_lvl wild] gives [mon] the correct ammount of experience
      after defeating the pokemon of lvl [cpu_lvl]. [wild] is true 
      if this pokemon was a wild pokemon, and false otherwise. *)
  val give_xp: t -> float -> bool -> unit


end

module Pokemon : PokeSig