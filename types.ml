open Yojson.Basic.Util

type t = string
exception UnknownType of t

let hash lst = 
  let rec create_hash f i = function
    | [] -> f 
    | h::t -> create_hash (fun x -> if x = h then i else f x) (i+1) t
  in create_hash (fun x -> raise (UnknownType x)) 0 lst

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
  let json_data_lst = json |> member "data" |>to_list in 
  let data = json_data_lst|> List.map json_data in 
  let hash_fun = json_data_lst |> List.map to_string |> hash in 
  (cols 0 m data,hash_fun)