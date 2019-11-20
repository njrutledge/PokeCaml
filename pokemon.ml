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
    "pokemon.json"
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
  val lvl_up: t -> bool
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
  val retreat: t array -> bool
  val alive_pmons: t array -> t array 
  val hp_string: t -> string 
  val string_of_mon: t -> string
  val string_of_mons: t array -> string
  val restore_mons: t array -> unit
  val give_xp: t -> float -> bool -> unit
  val add_mon: t array -> t -> t array 
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
  let file_name = ref "pokemon.json"

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

  (** [stat_update m] updates the stats of [m] for leveling up. *)
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

  let create_pokemon mon_name start_lvl = 
    let json = get_data mon_name in 
    let pmon = 
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
        lvl = 1.;
        xp = Float.pow (start_lvl-.1.) 3.;
      } in 
    ignore (lvl_up pmon);
    pmon.hp <- pmon.max_hp;
    pmon 

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

  let retreat party = 
    Array.fold_left (fun acc p -> acc && fainted p) true party

  let rec alive_pmons mons = 
    mons
    |> Array.to_list
    |> List.filter (fun x -> not (fainted x))
    |> Array.of_list

  let calc_xp_percent mon = 
    (mon.xp -. (Float.pow (mon.lvl-.1.) 3.)) /. (Float.pow mon.lvl 3.)*. 100.
    |> Int.of_float 
    |> string_of_int

  (** [hp_string m] is a string representation of pokemon [m]'s 
      health and max health. *)
  let hp_string mon = 
    if fainted mon then "fainted"
    else  
      let curr_hp = 
        if 0. < (get_hp mon) && (get_hp mon) <= 1. then "1"
        else (string_of_int (Int.of_float (get_hp mon))) 
      in "hp: " ^ curr_hp ^ "/" ^ (string_of_int (Int.of_float (get_max_hp mon)))

  let string_of_mon (mon : t) =
    ("{" ^ (get_name mon) ^ " - " ^ hp_string mon
     ^ " | level: " ^ (mon |> get_lvl |> Int.of_float |> string_of_int) 
     ^ " | xp: " ^ calc_xp_percent mon ^ "%" ^ "}")

  let rec string_of_mons mons =
    (*Array.fold_left (fun acc p -> acc ^ "\n" ^ (string_of_mon p)) "" mons*)
    let acc = ref "" in 
    for i = 0 to (Array.length mons) - 1 do 
      acc := 
        !acc ^ string_of_int (i + 1) ^ ". " ^ string_of_mon mons.(i) ^ "\n"
    done;
    !acc

  let rec restore_mons mons =
    Array.iter (fun x -> x.hp <- x.max_hp) mons

  let give_xp mon cp_mon_lvl wild = 
    let a = if wild then 1.0 else 1.5 in  
    let b = 50. in 
    let frac = (Float.pow (2. *. cp_mon_lvl +. 10.) 2.5) 
               /. (Float.pow (cp_mon_lvl +. mon.lvl +. 10.) 2.5) in 
    let exp = (a *. b *. cp_mon_lvl /. 5. *. frac +. 1.) in 
    mon.xp <- mon.xp +. exp

  let add_mon mons new_mon= 
    let len = Array.length mons in 
    if len >= 6 then failwith "trying to create a party of more than 6" else
      Array.init (len + 1) (fun i -> if i <> len then mons.(i) else new_mon) 
end