open Types
open Yojson.Basic.Util

module type MoveSig = sig
  type t = 
    {
      move_name : string;
      description : string;
      power : float;
      is_special: bool;
      accuracy: float;
      el_type: Types.t;
      status: string list;
      mutable pp : int;
      max_pp : int
    }
  val create_move: string -> t
  val name: t -> string 
  val to_string: t -> string
  val get_pp: t -> int
  val get_max_pp: t -> int
  val get_acc: t -> float
  val get_is_special: t -> bool
  val get_status: t -> string list
  val set_pp: t -> int -> unit
  val decr_pp: t -> unit
  val to_string_name : t -> string
  val to_string: t -> string
end

module Moves : MoveSig = struct
  type t = {
    move_name : string;
    description : string;
    power : float;
    is_special: bool;
    accuracy : float;
    el_type : Types.t;
    status : string list;
    mutable pp : int;
    max_pp : int;
  }

  (** [json] is the json file holding all pokemon moves. *)
  let json = Yojson.Basic.from_file "moves.json"
  let create_move move = 
    try
      let m_j = json |> member move in 
      {
        move_name = move;
        description = m_j |> member "desc" |> to_string;
        power = m_j |> member "power" |> to_float;
        is_special = m_j |> member "special" |> to_bool;
        status = m_j |> member "status" |> to_list|> List.map (to_string);
        accuracy = m_j |> member "accuracy" |> to_float;
        el_type = m_j |> member "type" |> to_string;
        pp = m_j |> member "uses" |> to_int;
        max_pp = m_j |> member "uses" |> to_int;
      }
    with _ -> failwith ("move error: " ^ move)

  let name m = m.move_name

  let get_pp m = m.pp

  let get_max_pp m = m.max_pp

  let get_acc m = m.accuracy

  let get_is_special m = m.is_special

  let get_status m = m.status

  let set_pp m pp = m.pp <- pp

  let decr_pp m = m.pp <- m.pp - 1

  let to_string_name m = "Name: " ^ m.move_name ^ " | PP: " ^ string_of_int m.pp
                         ^ "/" ^ string_of_int m.max_pp

  let to_string m = 
    "Name: " ^ m.move_name ^ "\nPower: " ^ string_of_float m.power ^ ", ACC: " ^
    string_of_float m.accuracy ^ ", Type: " ^ m.el_type ^ ", PP: " ^
    string_of_int m.pp ^ "/" ^ string_of_int m.max_pp ^ "\n" ^
    "\tDescription: " ^ m.description ^ "\n"

end