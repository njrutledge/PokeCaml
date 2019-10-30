open Yojson.Basic.Util

module type types = sig
  type t = Normal | Fire | Water | Grass | Electric | Ground 
end

let type_matrix json =  
  let n = json |> member "size" |> to_int in 
  let mat = Array.make_matrix n n 0.0 in 
  for i=0 to n do 