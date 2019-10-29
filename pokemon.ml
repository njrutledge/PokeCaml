type e_type = Normal|Fire|Water|Grass|Electric|Ground
open Moves
module type PokeSig = sig
  include Moves
  type t = e_type
  type hp = int
  type attack = int
  type defense = int
  type speed = int 
  type moves = Moves list
end
module MakePokemon (P : PokeSig)= struct
  type t = Type
  val hp
  val 

end