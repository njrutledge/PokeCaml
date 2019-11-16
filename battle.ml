(* TODO: ascii art, add moves for cpu, stab, better ai, certain abilities,
   items, crits, stat affecting moves, self damaging moves, variable damage moves,
   more types, special vs physical *)
exception BattleOver
open Moves
open Pokemon
module PM = Pokemon

(** Raised when the player tries to do something illegal. *)
exception IllegalMove of string

(** [execute_quit] quits the adventure. *)
let execute_quit mon = 
  ANSITerminal.(print_string [cyan] "\nThanks for playing!\n "); 
  exit 0

(* https://bulbapedia.bulbagarden.net/wiki/Damage *)
let damage level power attack defense modifier = 
  (*2.*.level/.5.+.2.
    |> ( *. ) (power*.(attack/.defense))
    |> (/.) 50.
    |> (+.) 2. 
    |> ( *. ) modifier*)
  modifier*.((((((2.*.level)/.5.)+. 2.)*.power*.(attack/.defense))/. 50.) +. 2.)

let rec get_modifier move_type acc mat hash = function
  | [] -> acc
  | h::t -> get_modifier move_type (acc*.mat.(hash move_type).(hash h)) mat hash t

(** [execute_go adv st ph] is the state update of the adventure after running 
    [state.go adv st ph'], where [ph'] is the string representation of 
    string list [ph].
    Raises [IllegalMove msg] if the move is invalid. *)
let execute_attack (atk_mon : PM.t) (def_mon : PM.t) move_name = 
  let json = Yojson.Basic.from_file "type_matrix.json" in 
  let type_mat_and_hash = Types.type_matrix_and_hash json in
  let type_mat = fst type_mat_and_hash in 
  let hash = snd type_mat_and_hash in 
  let move = PM.get_move atk_mon move_name in 
  let modifier = (get_modifier move.el_type 1. type_mat hash (PM.get_type def_mon)) in
  let move_damage = damage 
      (PM.get_lvl atk_mon)
      move.power
      (PM.get_attack atk_mon)
      (PM.get_defense def_mon)
      modifier
  in
  print_endline (PM.get_name atk_mon ^ " used " ^ move_name);
  if modifier = 0. then 
    print_endline ("It has no effect!") 
  else if modifier < 1. then 
    print_endline ("It's not very effective...")
  else if modifier >= 2. then
    print_endline ("It's super effective!")
  else ();
  PM.change_hp def_mon (-.move_damage)

let execute_item atk_mon def_mon item = 
  failwith "unimplemented"


let rec execute_command atk_mon def_mon input = 
  match Btlcmd.parse input with 
  | Quit -> execute_quit atk_mon
  | Attack(phrase) -> execute_attack atk_mon def_mon (String.concat " " phrase)
  | Item(phrase) -> execute_item atk_mon def_mon (String.concat " " phrase)
  | MovesInfo ->
    ANSITerminal.(print_string [green] 
                    ("Move info: \n" ^ (PM.format_moves_all atk_mon) ^ "\n"));
    print_string "> ";
    execute_command atk_mon def_mon (read_line ())

let rec get_command atk_mon def_mon input = 
  try
    execute_command atk_mon def_mon input
  with 
  | Btlcmd.Empty -> 
    ANSITerminal.(print_string [red]
                    "\nError: please give command.\n ");
    print_string "> ";
    get_command atk_mon def_mon (read_line ())
  | Btlcmd.Malformed ->
    ANSITerminal.(print_string [red]
                    "\nError: please input valid command.\n");
    print_string "> ";
    get_command atk_mon def_mon (read_line ())
  | Pokemon.UnknownMove m -> 
    ANSITerminal.(print_string [red] "\nError: move not valid. Please input a valid command.\n");
    print_string "> ";
    get_command atk_mon def_mon (read_line ())

let execute_cpu_turn player_mon cpu_mon = 
  Random.self_init();
  let cpu_moves = PM.get_moves cpu_mon in 
  let used_move = List.nth cpu_moves (Random.int (List.length cpu_moves)) 
                  |> Moves.name in 
  execute_attack cpu_mon player_mon used_move

(** [loop adv state] executes a REPL for the game. Quits on recieving 
    "quit". *)
let rec loop (player_mon : PM.t) (cpu_mon : PM.t) = 
  print_string "\n";
  print_endline ("--" ^ (PM.get_name cpu_mon) ^ "--");
  print_endline ("< hp" ^ ": " ^ string_of_float (PM.get_hp cpu_mon) ^ " >");
  print_string "\n";
  print_endline ("--" ^ PM.get_name player_mon ^ "--");
  print_endline ("< hp" ^ ": " ^ string_of_float (PM.get_hp player_mon) ^ " >");
  print_string "\n";
  print_endline "Choose your move:";
  print_endline (PM.format_moves_names player_mon);
  print_string "\n";
  print_string "> ";
  get_command player_mon cpu_mon (read_line ());
  if PM.fainted cpu_mon then begin 
    (*print_endline Ascii.caml;*)
    print_endline "player wins!";
    raise BattleOver
  end
  else ();
  print_string "\n";
  execute_cpu_turn player_mon cpu_mon;
  if PM.fainted player_mon then begin
    (*print_endline Ascii.suprise;*)
    print_endline "player loses!";
    raise BattleOver
  end
  else ();
  print_endline "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~";
  loop player_mon cpu_mon

(** [play_game f] starts the adventure in file [f]. *)
let play_game file p_mons bag money enemy_mons =
  PM.set_file file ;
  (*print_endline Ascii.pokemon_opening;
    print_endline "Which pokemon?";
    print_string "> ";
    let def = read_line () in*)
  match p_mons, enemy_mons with
  | (h1 :: t1), (h2 :: t2) -> let fst_mon = h1 in 
    let fst_enemy_mon = h2 in 
    begin try loop fst_mon fst_enemy_mon
      with BattleOver -> 
        if (PM.retreat p_mons) then (p_mons, bag, money, false) else begin 
          ANSITerminal.(print_string [green] 
                          ("\nDo you want to keep going? [Y/N]\n"));
          let rec get_response () =
            print_string "> ";
            match read_line () with 
            | "Yes" | "Y" | "y"-> true
            | "No" | "N" | "n"-> false
            | _ -> get_response ()
          in (p_mons, bag, money, get_response ())
        end
    end 
  | _ -> failwith "Empty mons"

let main (player_team, bag, money, enemy_team) = 

  ANSITerminal.(print_string [red] "\n\nStarting battle\n");
  play_game "testmons.json" player_team bag money enemy_team

(* Execute the game engine. *)