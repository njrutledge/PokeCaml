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
  type t_moves = Moves.t list
  type t = {
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
  }

  val set_file : string -> unit
  val create_pokemon: string -> float -> t
  val change_hp : t -> t_hp -> unit
  val incr_stats : t -> unit
  val fainted : t -> bool 
  val get_name : t -> string
  val get_type : t -> t_type list
  val get_moves : t -> Moves.t list
  val get_hp : t -> t_hp
  val get_max_hp : t -> t_hp
  val get_attack : t -> t_attack
  val get_defense : t -> t_defense
  val get_speed : t -> t_speed
  val get_move : t -> string -> Moves.t
  val format_moves_names : t -> string
  val format_moves_all: t -> string
end

module Pokemon : PokeSig