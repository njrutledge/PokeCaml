open Yojson.Basic.Util

type t = string
exception UnknownType of t


(** [hash lst] is the function that maps each type as a string in [lst] 
    to a number. the number each type is mapped to is it's row and column 
    in the type matrix. *)
let hash lst = 
  let rec create_hash f i = function
    | [] -> f 
    | h::t -> create_hash (fun x -> if x = h then i else f x) (i+1) t
  in create_hash (fun x -> raise (UnknownType x)) 0 lst

(** [json_data j] parses a given data entry and returns the data as floats. *)
let json_data jdata = 
  jdata |> member "col" |> to_list |> List.map to_float

(** [add i j m lst] adds all elements of [lst] to matrix [m] column [j],
    where [i] is the current row. 
    Requires: [i] starts at 0, the length of [lst] is at most the length
    of a column of matrix [m], and j does not succeed the number of rows of 
    matrix [m]. *)
let rec add i j m = function
  | [] -> ()
  | h::t -> m.(i).(j) <- h;
    add (i+1) j m t

(** [cols j m lst] adds all elements of [lst] to column [j] of matrix [m]. *)
let rec cols j m = function
  | [] -> ()
  | h::t -> 
    add 0 j m h; 
    cols (j+1) m t

(** [type_names] is a list of the type names in order, for testing purposes. *)
let type_names = ["NORM"; "FIGT"; "FLYN"; "PISN"; "GRND"; "ROCK"; "BUG "; 
                  "GHST"; "STEL"; "FIRE"; "WATR"; "GRSS"; "ELCT"; "PSYC";
                  "ICE "; "DRGN"; "DARK"; "FARY"]


let test_print_type_mat m hf = 
  print_string ("    |");
  for i = 0 to Array.length m-1 do 
    print_string (List.nth type_names i ^ "|");
  done;
  print_endline ("");
  for i = 0 to Array.length m-1 do 
    print_string (List.nth type_names i ^ "|");
    for j = 0 to Array.length m.(0)-1 do
      let n = m.(i).(j) in
      if n = 0.5 then 
        print_string ((m.(i).(j)|>string_of_float)^" |")
      else 
        print_string ((m.(i).(j)|>string_of_float)^"  |")
    done;
    ignore (read_line ());
  done


let type_matrix_and_hash json =  
  let n = json |> member "size" |> to_int in 
  let m = Array.make_matrix n n 0.0 in 
  let json_data_lst = json |> member "data" |>to_list in 
  let data = json_data_lst|> List.map json_data in 
  let hash_fun = 
    json_data_lst 
    |> List.map(fun x -> member "type" x |> to_string) 
    |> hash in 
  cols 0 m data;
  (m, hash_fun)