open Moves

module type StatsSig = sig
  type t = Yojson.Basic.t
  val mon: string ref
  val get_data: string -> Yojson.Basic.t
end

module Stats : StatsSig

module type PokeSig = sig
  include MoveSig
  type t
  type t_type = string 
  type t_hp = int
  type t_attack = int
  type t_defense = int
  type t_speed = int 
  type t_moves = Moves.t list

  val stats : t
  val change_hp : t -> int -> unit
  val incr_stats : t -> unit
  val fainted : t -> bool 
  val get_type : t -> t_type
  val get_moves : t -> Moves.t list
  val get_hp : t -> int
  val get_max_hp : t -> int 
  val get_attack : t -> int
  val get_defense : t -> int
  val get_speed : t -> int
end

module type PokemonMaker = functor (S: StatsSig) -> PokeSig

module Pokemon : PokemonMaker