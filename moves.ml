open Types
open Yojson.Basic.Util

module type MoveSig = sig
  type t = 
    {
      move_name: string;
      description: string;
      power : float;
      accuracy: float;
      el_type: Types.t list
    }
  val create_move: string -> t
end

module Moves : MoveSig = struct
  type t = {
    move_name: string;
    description: string;
    power : float;
    accuracy: float;
    el_type: Types.t list
  }
  let json = Yojson.Basic.from_file "moves.json"
  let create_move move = 
    let m_j = json |> member move in 
    {
      move_name = move;
      description = m_j |> member "desc" |> to_string;
      power = m_j |> member "power" |> to_float;
      accuracy = m_j |> member "accuracy" |> to_float;
      el_type = m_j |> member "types" |> to_list |> List.map to_string
    }
end