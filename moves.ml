open Types

module type Moves = sig
  type ('n, 'p, 'a, 't) t
end

module Moves = struct
  type t = Move of (string * float * float * type) list
end