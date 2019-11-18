open Types
open Yojson.Basic.Util

module type MoveSig = sig
  type t = 
    {
      move_name: string;
      description: string;
      power : float;
      accuracy: float;
      el_type: Types.t;
    }
  val create_move: string -> t
  val name: t -> string 
  val to_string: t -> string
end

module Moves : MoveSig = struct
  type t = {
    move_name: string;
    description: string;
    power : float;
    accuracy: float;
    el_type: Types.t;
  }

  (** [json] is the json file holding all pokemon moves. *)
  let json = Yojson.Basic.from_file "moves.json"
  let create_move move = 
    let m_j = json |> member move in 
    {
      move_name = move;
      description = m_j |> member "desc" |> to_string;
      power = m_j |> member "power" |> to_float;
      accuracy = m_j |> member "accuracy" |> to_float;
      el_type = m_j |> member "type" |> to_string;
    }

  let name m = 
    m.move_name

  let to_string m = 
    "Name: " ^ m.move_name ^ "\nPower: " ^ string_of_float m.power ^ ", ACC: " ^
    string_of_float m.accuracy ^ ", Type: " ^ m.el_type ^ "\n" ^
    "\tDescription: " ^ m.description ^ "\n"

end