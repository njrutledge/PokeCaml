let sleep_speed = ref 1.0

let change_speed () = 
  if !sleep_speed = 1.0 then sleep_speed := 0.5
  else if !sleep_speed = 0.5 then sleep_speed := 0.0
  else sleep_speed := 1.0

(** [execute_quit] quits the game. *)
let execute_quit () = 
  ANSITerminal.(print_string [cyan] "\nThanks for playing!\n "); 
  exit 0

let get_sleep_speed () = !sleep_speed

let rec get_y_n () =
  print_string "> ";
  match read_line () with 
  | "Yes" | "Y" | "y" | "yes" -> true
  | "No" | "N" | "n" | "no" -> false
  | "quit" -> execute_quit ()
  | _ -> get_y_n ()

