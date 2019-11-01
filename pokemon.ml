open Moves
open Types
open Yojson.Basic.Util

module type StatsSig = sig
  type t = Yojson.Basic.t
  val mon: string ref
  val get_data: string -> Yojson.Basic.t
end

module Stats : StatsSig = struct
  type t = Yojson.Basic.t
  let json = 
    "testmons.json"
    |> Yojson.Basic.from_file
  let mon = ref "Mon1"
  let get_data mon_name = json |> member mon_name 
end

module type PokeSig = sig
  include MoveSig

  type t
  type t_type = string 
  type t_hp = int
  type t_attack = int
  type t_defense = int
  type t_speed = int 
  type t_moves = Moves.t list

  val stats: t
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

module Pokemon (S: StatsSig) = struct
  type t_type = string
  type t_hp = int
  type t_attack = int
  type t_defense = int
  type t_speed = int 
  type t_moves = Moves.t list
  type t = {
    el_type: t_type;
    mutable max_hp : t_hp;
    mutable hp: t_hp;
    mutable attack: t_attack;
    mutable defense: t_defense;
    mutable speed: t_speed;
    mutable moves: t_moves;
    evolution: string;

  }
  let stats = 
    let () = print_endline !S.mon in 
    let () = print_endline "NAME PRINTED ABOVE" in 
    let json = S.get_data !S.mon in 
    {
      el_type = 
        json 
        |> member "Type"
        |> to_string;
      max_hp = 
        json 
        |> member "Stats"
        |> member "HP"
        |> to_int;
      hp = 
        json 
        |> member "Stats"
        |> member "HP"
        |> to_int;
      attack = 
        json 
        |> member "Stats"
        |> member "ATK"
        |> to_int; 
      speed = 
        json 
        |> member "Stats"
        |> member "SPE"
        |> to_int;
      defense = 
        json 
        |> member "Stats"
        |> member "DEF"
        |> to_int;
      moves = [];
      evolution = ""
      (*json
        |> member "Evolution"
        |> to_string;*)
    }
  let get_max_hp mon = mon.max_hp

  let change_hp mon hp = 
    mon.hp <- (      
      let new_hp = mon.hp + hp in
      if new_hp < get_max_hp mon 
      then new_hp 
      else get_max_hp mon
    )

  let incr_stats mon = failwith "Unimplemented"

  let fainted mon = mon.hp <= 0

  let get_type mon = mon.el_type

  let get_moves mon = mon.moves  

  let get_hp mon = mon.hp

  let get_max_hp mon = mon.max_hp

  let get_attack mon = mon.attack

  let get_defense mon = mon.defense

  let get_speed mon = mon.speed

end