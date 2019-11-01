open Types

module type MoveSig = sig
  type t
end

module Moves : MoveSig = struct
  type t = Move of (string * int * int * string list)
end