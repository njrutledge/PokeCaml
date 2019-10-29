(** Raised when the player tries to do something illegal. *)
exception IllegalMove of string

(** [update] is the type of update to the game. *) 
type update = 
    State of State.t 
  | Adv of Adventure.t 
  | Both of Adventure.t*State.t
  | None

(** [pp_string s] pretty-prints string [s], as given in test.ml *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt]
    to pretty-print each element of [lst], as given in test.ml *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [h] -> acc ^ pp_elt h
      | h1::(h2::t as t') ->
        if n=100 then acc ^ "..."  (* stop printing long list *)
        else loop (n+1) (acc ^ (pp_elt h1) ^ "; ") t'
    in loop 0 "" lst
  in  pp_elts lst


let print_items items = 
  if items = "" then () else print_endline("the town has items " ^ items)

(** *)
let print_items_in_town adv state = 
  state 
  |> State.current_town_id 
  |> Adventure.town_items adv
  |> pp_list pp_string 
  |> print_items
(** [execute_quit] quits the adventure. *)
let execute_quit adv = 
  ANSITerminal.(print_string [cyan] "\nThanks for playing!\n "); 
  exit 0

(** [execute_go adv st ph] is the state update of the adventure after running 
    [state.go adv st ph'], where [ph'] is the string representation of 
    string list [ph].
    Raises [IllegalMove msg] if the move is invalid. *)
let execute_go adv state exit = 
  match State.go exit adv state with
  | Legal (t) -> State t
  | Illegal (msg) -> raise (IllegalMove msg)

let execute_lock adv st exit = 
  Adv (State.lock_door adv st exit)

let execute_unlock adv st exit = 
  Adv (State.unlock_door adv st exit)

let execute_take adv st item = 
  Both (
    Adventure.take_item adv (State.current_town_id st) item,
    State.add_item st item
  )

let execute_drop adv st item =
  Both (
    Adventure.drop_item adv (State.current_town_id st) item,
    State.drop_item st item
  )

let execute_Bag st = 
  ANSITerminal.(print_string [cyan] 
                  ("bag: " ^ pp_list pp_string (State.bag st)));
  None

(** [execute_score adv state] prints the adventurer's current score, and then 
    returns an update of none. *)
let execute_score adv state = 
  print_string "\nCurrent score: "; 
  ANSITerminal.(print_string [blue] 
                  (string_of_int (State.calc_score adv state)));
  None

(** [execute_command adv state input] is the update created by executing
    command [input] on adventure [adv] and state [state].  *)
let rec execute_command adv state input = 
  match Command.parse input with 
  | Quit -> execute_quit adv
  | Go(phrase) -> execute_go adv state (String.concat " " phrase)
  | Lock(phrase) -> execute_lock adv state (String.concat " " phrase)
  | Unlock(phrase) -> execute_unlock adv state (String.concat " " phrase)
  | Take(phrase) -> execute_take adv state (String.concat " " phrase)
  | Drop(phrase) -> execute_drop adv state (String.concat " " phrase)
  | Bag -> execute_Bag state
  | Score ->  execute_score adv state

(** [get_command adv state input] *)
let rec get_command adv state input = 
  try
    execute_command adv state input
  with 
  |Command.Empty -> 
    ANSITerminal.(print_string [red]
                    "\nError: please give command.\n ");
    print_string "> ";
    get_command adv state (read_line ())
  | Command.Malformed ->
    ANSITerminal.(print_string [red]
                    "\nError: please input valid command.\n");
    print_string "> ";
    get_command adv state (read_line ())
  | IllegalMove (msg) -> print_string msg; 
    print_string "> ";
    get_command adv state (read_line ())
  | Adventure.UnknownItem it -> 
    print_string ("\nThere is no item \"" ^ it ^ "\" in the town.\n");
    print_string "> ";
    get_command adv state (read_line ())
  | State.ItemNotFound it ->
    print_string ("\nYou do not have item \"" ^ it ^"\".\n");
    print_string "> ";
    get_command adv state (read_line ())
  | State.KeyNotFound ->
    print_string("\nYou do not have the correct key.\n");
    print_string "> ";
    get_command adv state (read_line ())
  | Adventure.UnknownExit ex -> 
    print_string("\nExit \"" ^ ex ^ "\" does not exist.\n");
    print_string "> ";
    get_command adv state (read_line ())

(** [in_list el lst] is true if [el] is in the list [lst]. *)
let rec in_list el = function
  | [] -> false
  | h::t -> if h=el then true else in_list el t

(** [contains_needed_items adv st] is true if the treasure town has all the
    necessary items to win. *)
let contains_needed_items adv st = 
  let rec match_item item_lst = begin function
    | [] -> true
    | h::t -> if in_list h item_lst then match_item item_lst t else false
  end
  in match_item (Adventure.town_items adv (Adventure.treasure_town_id adv))
    (Adventure.treasure_town_needed_items adv)

(** [show_win_msg adv sc] prints the winning message of adventure [adv]
    associated with final score [sc]. *)
let show_win_msg adv score = 
  ANSITerminal.(print_string [cyan] (Adventure.win_msg adv score ^ "\n"));
  print_string "Final score: ";
  print_int (score);
  print_endline "";
  exit 0

(** [check_win adv state] will print the win message, final score of [state],
    and exit the game if the player has placed all the necessary items 
    in the treasure town of adventure [adv]. *)
let check_win adv st = 
  if State.current_town_id st = Adventure.treasure_town_id adv then
    if contains_needed_items adv st then 
      show_win_msg adv (State.calc_score adv st)
    else () 
  else ()

(** [loop adv state] executes a REPL for the game. Quits on recieving 
    "quit". *)
let rec loop adv state print_desc= 
  print_string "\n";
  if print_desc then begin
    state |> State.current_town_id |> Adventure.description adv |> print_endline;
    print_items_in_town adv state;
  end
  else ();
  check_win adv state;
  print_string "> ";
  get_command adv state (read_line ()) 
  |> function 
  | State state'-> loop adv state' true
  | Adv adv' -> loop adv' state true
  | Both (adv', state') -> loop adv' state' true
  | None -> loop adv state false

(** [play_game f] starts the adventure in file [f]. *)
let play_game f =
  try
    loop 
      (Adventure.from_json (Yojson.Basic.from_file f))
      (State.init_state (Adventure.from_json (Yojson.Basic.from_file f)))
      true
  with _ -> ANSITerminal.(print_string [red]
                            "\nEncountered Error while grabbing file. Please \
                             rerun and try a new file.\n")


(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to the 3110 Text Adventure Game engine.\n");
  print_endline "Please enter the name of the game file you want to load.\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> play_game file_name

(* Execute the game engine. *)
let () = main ()
