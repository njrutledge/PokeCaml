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
  type t_type = string 
  type t_hp = float
  type t_attack = float
  type t_defense = float
  type t_speed = float 
  type t_moves = Moves.t list
  type t = {
    el_type: t_type;
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
  val get_type : t -> t_type
  val get_moves : t -> Moves.t list
  val get_hp : t -> t_hp
  val get_max_hp : t -> t_hp
  val get_attack : t -> t_attack
  val get_defense : t -> t_defense
  val get_speed : t -> t_speed
  val get_move : t -> string -> Moves.t
end

module M = Moves

module Pokemon : PokeSig = struct
  type t_type = string
  type t_hp = float
  type t_attack = float
  type t_defense = float
  type t_speed = float
  type t_moves = M.t list
  type t = {
    el_type: t_type;
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
  let file_name = ref ""

  let set_file str = 
    file_name := str

  let get_data mon = 
    let json = Yojson.Basic.from_file !file_name in 
    json |> member mon

  let create_pokemon mon_name start_lvl = 
    let json = get_data mon_name in 
    {
      el_type = 
        json 
        |> member "Type"
        |> to_string;
      name = mon_name;
      max_hp = 
        json 
        |> member "Stats"
        |> member "HP"
        |> to_float;
      hp = 
        json 
        |> member "Stats"
        |> member "HP"
        |> to_float;
      attack = 
        json 
        |> member "Stats"
        |> member "ATK"
        |> to_float; 
      speed = 
        json 
        |> member "Stats"
        |> member "SPE"
        |> to_float;
      defense = 
        json 
        |> member "Stats"
        |> member "DEF"
        |> to_float;
      moves = 
        json
        |> member "Moves"
        |> to_list
        |> List.map to_string 
        |> List.map Moves.create_move;
      evolution = "";
      (*json
        |> member "Evolution"
        |> to_string;*)
      lvl = start_lvl;
    }
  let get_max_hp mon = mon.max_hp

  let change_hp mon hp = 
    mon.hp <- (      
      let new_hp = mon.hp +. hp in
      if new_hp < get_max_hp mon 
      then new_hp 
      else get_max_hp mon
    )

  let incr_stats mon = failwith "Unimplemented"

  let fainted mon = mon.hp <= 0.

  let get_name mon = mon.name

  let get_type mon = mon.el_type

  let get_moves mon = mon.moves  

  let get_hp mon = mon.hp

  let get_max_hp mon = mon.max_hp

  let get_attack mon = mon.attack

  let get_defense mon = mon.defense

  let get_speed mon = mon.speed

  let get_move mon move = 
    let rec find_move (lst : M.t list) =
      match lst with 
      | [] -> failwith "move not in moveset"
      | h :: t -> if move = h.move_name then h else find_move t
    in find_move mon.moves

end