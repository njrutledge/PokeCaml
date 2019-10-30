open Yojson.Basic.Util

type t = string

let rec hash f i= function
  | [] -> f 
  | h::t -> hash (fun x -> if x = h then i else f x) (i+1) t

let json_data jdata = 
  jdata |> member "cols" |> to_list |> List.map to_float

let rec add i j m = function
  | [] -> ()
  | h::t -> m.(i).(j) <- h;
    add (i+1) j m t

let rec cols j m = function
  | [] -> m
  | h::t -> add 0 j m h; cols (j+1) m t

let type_matrix json =  
  let n = json |> member "size" |> to_int in 
  let m = Array.make_matrix n n 0.0 in 
  let data_lst = json |> member "data" |>to_list|> List.map json_data in 
  cols 0 m data_lst;