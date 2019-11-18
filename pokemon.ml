open Moves
open Types
open Yojson.Basic.Util
exception UnknownMove of string

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

  val set_file : string -> unit
  val create_pokemon: string -> float -> t
  val get_max_hp : t -> t_hp
  val change_hp : t -> t_hp -> unit
  val incr_stats : t -> unit
  val fainted : t -> bool 
  val get_name : t -> string
  val get_type : t -> t_type list
  val get_moves : t -> Moves.t array 
  val get_hp : t -> t_hp
  val get_attack : t -> t_attack
  val get_defense : t -> t_defense
  val get_speed : t -> t_speed
  val get_move : t -> int -> Moves.t
  val get_lvl: t -> float
  val get_xp: t -> float
  val set_hp : t -> t_hp -> unit
  val format_moves_names : t -> string
  val format_moves_all: t -> string
  val retreat: t ref list -> bool
  val alive_pmons: t ref list -> t ref list 
  val string_of_mon: t -> string
  val string_of_mons: t ref list -> string
  val restore_mons: t ref list -> unit
  val give_xp: t -> float -> bool -> unit
  val lvl_up: t -> bool
end

module M = Moves

module Pokemon : PokeSig = struct
  exception UnknownMove of string
  type t_type = string
  type t_hp = float
  type t_attack = float
  type t_defense = float
  type t_speed = float
  type t_moves = M.t array 
  type t = {
    el_type: t_type list;
    mutable name : string;
    mutable max_hp : t_hp;
    mutable hp: t_hp;
    mutable lvl: float;
    mutable xp: float;
    mutable attack: t_attack;
    mutable defense: t_defense;
    mutable speed: t_speed;
    mutable moves: t_moves;
    evolution: string;
  }
  (**[file_name] is the name of the file containing all the pokemon. *)
  let file_name = ref "testsmons.json"

  let set_file str = 
    file_name := str

  (** [get_data m] is the Yojson.Basic.t that matches 
      the pokemon of name [mon]. *)
  let get_data mon = 
    let json = Yojson.Basic.from_file !file_name in 
    json |> member mon

  (** [fix_move_array a] is an array of length 4 containing all moves in [a]. *)
  let fix_move_array arr = 
    let moves = Array.make 4 arr.(0) in 
    for i = 0 to Array.length arr do 
      moves.(i) <- arr.(i)
    done;
    moves

  let create_pokemon mon_name start_lvl = 
    let json = get_data mon_name in 
    {
      el_type = 
        json 
        |> member "Types"
        |> to_list
        |> List.map to_string;
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
        |> List.map Moves.create_move
        |> Array.of_list;
      (*|> fix_move_array;*)
      evolution = "";
      (*json
        |> member "Evolution"
        |> to_string;*)
      lvl = start_lvl;
      xp = 0.;
    }

  let get_max_hp mon = mon.max_hp

  let change_hp mon hp = 
    mon.hp <- begin 
      let new_hp = mon.hp +. hp in
      if new_hp < get_max_hp mon 
      then max 0.0 new_hp 
      else get_max_hp mon
    end

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

  let get_move mon move = mon.moves.(move)

  let get_lvl mon = mon.lvl

  let get_xp mon = mon.xp

  let set_hp mon hp = mon.hp <- hp

  let format_moves_names mon = 
    let acc = ref "" in 
    for i = 0 to (Array.length mon.moves) - 1 do 
      acc := 
        !acc ^ string_of_int (i + 1) ^ ". " ^ Moves.name mon.moves.(i) ^ "\n"
    done;
    !acc

  let format_moves_all mon = 
    let acc = ref "" in 
    for i = 0 to (Array.length mon.moves) - 1 do 
      acc := 
        !acc ^ string_of_int (i + 1) ^ ". " ^ Moves.to_string mon.moves.(i) ^ "\n"
    done;
    !acc

  (*  let rec format_moves_all' moves acc =
      match moves with 
      | [] -> acc
      | h :: [] -> format_moves_all' [] (acc ^ (Moves.to_string h))
      | h :: t -> format_moves_all' t (acc ^ Moves.to_string h ^ "\n")
      in (format_moves_all' (get_moves mon) "\n") 
  *)
  let retreat party = 
    List.fold_left (fun acc p -> acc && fainted !p) true party

  let rec alive_pmons mons = 
    List.filter (fun x -> not (fainted !x)) mons 

  let string_of_mon (mon : t) =
    ("{" ^ (get_name mon) ^ " - hp: " ^ (string_of_float (get_hp mon))
     ^ " | level: " ^ (mon |> get_lvl |> Int.of_float |> string_of_int) 
     ^ " | xp: " ^ (mon.xp /. (Float.pow mon.lvl 3.)*.100.|> Int.of_float |>
                    string_of_int) ^ "%" ^ "}")

  let rec string_of_mons = function
    | [] -> ""
    | p :: t -> (string_of_mon !p) ^ "\n" ^ (string_of_mons t)

  let rec restore_mons mons =
    match mons with
    | [] -> ()
    | h :: t -> set_hp !h (get_max_hp !h); restore_mons t

  let give_xp mon cp_mon_lvl wild = 
    let a = if wild then 1.0 else 1.5 in  
    let b = 50. in 
    let frac = (Float.pow (2. *. cp_mon_lvl +. 10.) 2.5) 
               /. (Float.pow (cp_mon_lvl +. mon.lvl +. 10.) 2.5) in 
    let exp = (a *. b *. cp_mon_lvl /. 5. *. frac +. 1.) in 
    mon.xp <- mon.xp +. exp

  let stat_update mon = 
    mon.lvl <- mon.lvl +. 1.;
    mon.max_hp <- mon.max_hp *. 1.02; 
    mon.attack <- mon.attack *. 1.02; 
    mon.defense <- mon.defense *. 1.02; 
    mon.speed <- mon.speed *. 1.02

  let lvl_up mon =
    let rec lvl_up' change =  
      if mon.xp >= Float.pow mon.lvl 3. 
      then begin 
        stat_update mon; lvl_up' true 
      end
      else change in 
    lvl_up' false 
end