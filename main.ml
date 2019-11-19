open Pokemon
module PM = Pokemon

(** Raised when the player tries to do something illegal. *)
exception IllegalMove of string
exception NotInPC

(** [update] is the type of update to the game. *) 
type update = 
    State of State.t 
  | Adv of Adventure.t 
  | Both of Adventure.t * State.t
  | None

(** [pp_string s] pretty-prints string [s], as given in test.ml *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_bag_entry (s,t)] pretty prints the entry of bag with item [s] and number
    of that item [t]. *)
let pp_bag_entry (s, t) = "\"" ^ Item.string_of_item s 
                          ^ "\"" ^ " : " ^ string_of_int !t

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

let execute_take adv st item = 
  State (State.add_item st item)

let execute_Bag st = 
  ANSITerminal.(print_string [cyan] 
                  ("bag: " ^ pp_list pp_bag_entry (State.bag st)));
  None

let execute_party adv st = 
  print_endline (PM.string_of_mons (State.get_party st));
  None

let execute_go_route adv st route = 
  match State.route route adv st with 
  | Legal (t) -> State t
  | Illegal (msg) -> raise (IllegalMove msg)

let in_pokecenter state = 
  let state_name = state |> State.current_town_id |> String.split_on_char ' ' in
  match List.hd state_name with
  | "PokeCenter" -> true
  | _ -> false

let execute_heal state = state |> State.get_party |> PM.restore_mons; None

let execute_buy state phrase = failwith "Buy Unimplemented"
(*try begin
  let amt = int_of_string (phrase |> List.rev |> List.hd) in
  end
  with begin
  failure int_of_string
  end*)

let execute_map adv state = failwith "Map unimplemented"
(*let exits = Adventure.exits adv (State.current_town_id state) in
  let rec print_exits acc e = 
  match e with
  | [] -> acc ^ "]"
  | h :: t -> print_exits (h ^ )*)

(** [execute_command adv state input] is the update created by executing
    command [input] on adventure [adv] and state [state].  *)
let rec execute_command adv state input = 
  match Command.parse input with 
  | Quit -> execute_quit adv
  | Go(phrase) -> execute_go adv state (String.concat " " phrase)
  | GoRoute(phrase) -> execute_go_route adv state (String.concat " " phrase)
  | Take(phrase) -> execute_take adv state (String.concat " " phrase)
  | Party -> execute_party adv state
  | Bag -> execute_Bag state
  | Heal -> if in_pokecenter state then execute_heal state else raise NotInPC
  | Map -> execute_map adv state
  | Buy(phrase) -> 
    if in_pokecenter state then execute_buy state (String.concat " " phrase) 
    else raise NotInPC

(** [get_command adv state input] *)
let rec get_command adv state input = 
  try 
    execute_command adv state input
  with 
  | Command.Empty -> 
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

(** [show_win_msg adv sc] prints the winning message of adventure [adv]
    associated with final score [sc]. *)
let show_win_msg adv score = 
  ANSITerminal.(print_string [cyan] (Adventure.win_msg adv score ^ "\n"));
  print_string "Final score: ";
  print_int (score);
  print_endline "";
  exit 0

(** [loop adv state] executes a REPL for the game. Quits on recieving 
    "quit". *)
let rec loop adv state print_desc= 
  print_string "\n";
  if print_desc then begin
    state |> State.current_town_id |> Adventure.description adv |> print_endline;
  end
  else ();
  print_string "> ";
  get_command adv state (read_line ()) 
  |> function 
  | State state'-> loop adv state' true
  | Adv adv' -> loop adv' state true
  | Both (adv', state') -> loop adv' state' true
  | None -> loop adv state false

(** [play_game f] starts the adventure in file [f]. *)
let play_game f =
  (*try*)
  loop 
    (Adventure.from_json (Yojson.Basic.from_file f))
    (State.init_state (Adventure.from_json (Yojson.Basic.from_file f)))
    true
(*with _ -> ANSITerminal.(print_string [red]
                          "\nEncountered Error while grabbing file. Please \
                           rerun and try a new file.\n")*)


(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.(print_string [yellow]
                  Ascii.pokemon_opening);
  (*print_endline "\n\n";
  ANSITerminal.(print_string [red]
                  Ascii.str3110);*)
  print_endline "\n\n";
  (*print_string  "> ";
    match read_line () with
    | exception End_of_file -> ()
    | file_name ->*) play_game "adv.json"

(* Execute the game engine. *)
let () = main ()
