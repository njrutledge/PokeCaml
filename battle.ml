(* TODO: ascii art, add moves for cpu, stab, better ai, certain abilities,
   items, crits, stat affecting moves, self damaging moves, variable damage moves,
   more types, special vs physical *)
exception BattleWon
exception BattleLost
exception PlayerDown of string 
exception CPUDown of string 
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

let shake ball f = 
  let b = begin 
    match ball with 
    | Item.PokeBall -> 255.
    | Item.GreatBall -> 200. 
    | Item.UltraBall -> 150.
    | _ -> failwith "Only ball can shake"
  end in
  let d = (255. *. 100.) /. b in
  let d_round = d |> Float.to_int |> Int.to_float in begin
    if d_round >= 256. then print_endline "The ball shakes 3 times."
    else let x = d_round *. f /. 255. in 
      let x_round = x |> Float.to_int |> Int.to_float in begin
        if x_round < 10. then print_endline "The ball missed!"
        else if x_round < 30. then print_endline "The ball shakes 1 time."
        else if x_round < 70. then print_endline "The ball shakes 2 times."
        else print_endline "The ball shakes 3 times."
      end;
  end;
  false

(* [catch def_mon ball] determines whether or not a wild pokemon is
   caught, using the gen 1 catch formula here: https://bulbapedia.bulbagarden.net/wiki/Catch_rate#Capture_method_.28Generation_I.29  *)
let catch mon ball (rate : float) = 
  if ball = Item.MasterBall then true
  else begin Random.self_init(); 
    (*let n = begin 
      match ball with 
      | Item.PokeBall -> Random.float 256.
      | Item.GreatBall -> Random.float 201. 
      | Item.UltraBall -> Random.float 151.
      | _ -> failwith "Ball only to catch"
      end in *)
    let m = Random.float 256. in
    let b = (if ball = Item.GreatBall then 8. else 12.) in
    let f = (PM.get_max_hp mon *. 255. *. 4.) /. (PM.get_hp mon *. b) in
    let f_round = f |> Float.to_int |> Int.to_float in 
    if f >= m then true else shake ball f_round 
  end

let execute_item atk_mon def_mon item bag = 
  match (List.assoc item bag) > 0 with 
  | exception Not_found -> print_endline ("You don't have any " ^ Item.string_of_item item ^ " to use!")
  | _ ->  match item with 
    | Item.Potion -> PM.change_hp atk_mon ((PM.get_max_hp atk_mon)*. 0.3);
      print_endline ((PM.get_name atk_mon) ^ "'s HP was restored by 30%.")
    | Item.HyperPotion -> PM.change_hp atk_mon ((PM.get_max_hp atk_mon)*. 0.6);
      print_endline ((PM.get_name atk_mon) ^ "'s HP was restored by 60%.")
    | Item.FullRestore -> PM.change_hp atk_mon (PM.get_max_hp atk_mon);
      print_endline ((PM.get_name atk_mon) ^ "'s HP was restored by 100%. ")
    | Item.PokeBall | Item.GreatBall | Item.UltraBall | Item.MasterBall -> begin
        let caught = catch def_mon item 255. in begin
          if caught then print_endline ("\n" ^ (PM.get_name def_mon) ^ " was caught!\n") 
          else print_endline ("\n" ^ (PM.get_name def_mon) ^ " wasn't caught.\n") 
        end
      end

let rec execute_command party atk_mon def_mon bag input = 
  match Btlcmd.parse input with 
  | Quit -> execute_quit atk_mon
  | Attack(phrase) -> execute_attack atk_mon def_mon (String.concat " " phrase)
  | Item(phrase) -> execute_item atk_mon def_mon (Item.item_of_string (String.concat " " phrase)) bag
  | MovesInfo ->
    ANSITerminal.(print_string [green] 
                    ("Move info: \n" ^ (PM.format_moves_all atk_mon) ^ "\n"));
    print_string "> ";
    execute_command party atk_mon def_mon bag (read_line ())
  | Party -> ANSITerminal.(print_string [blue] (PM.string_of_mons party));
    print_string "> ";
    execute_command party atk_mon def_mon bag (read_line ()) 

let rec get_command party atk_mon def_mon bag input = 
  try
    execute_command party atk_mon def_mon bag input
  with 
  | Btlcmd.Empty -> 
    ANSITerminal.(print_string [red]
                    "\nError: please give command.\n ");
    print_string "> ";
    get_command party atk_mon def_mon bag (read_line ())
  | Btlcmd.Malformed ->
    ANSITerminal.(print_string [red]
                    "\nError: please input valid command.\n");
    print_string "> ";
    get_command party atk_mon def_mon bag (read_line ())
  | Pokemon.UnknownMove m -> 
    ANSITerminal.(print_string [red] "\nError: move not valid. Please input a
    valid command.\n");
    print_string "> ";
    get_command party atk_mon def_mon bag (read_line ())
  | Item.InvalidItem i -> 
    ANSITerminal.(print_string [red] "\nError: invalid item name.\n");
    print_string "> ";
    get_command party atk_mon def_mon bag (read_line ())

let execute_cpu_turn player_mon cpu_mon = 
  Random.self_init();
  let cpu_moves = PM.get_moves cpu_mon in 
  let used_move = List.nth cpu_moves (Random.int (List.length cpu_moves)) 
                  |> Moves.name in 
  execute_attack cpu_mon player_mon used_move

(** [loop adv state] executes a REPL for the game. Quits on recieving 
    "quit". *)
let rec loop p_team cpu_team (player_mon : PM.t) (cpu_mon : PM.t) 
    (bag : (Item.t * int) list) = 
  print_string "\n";
  print_endline ("--" ^ (PM.get_name cpu_mon) ^ "--");
  print_endline ("< hp" ^ ": " 
                 ^ (PM.get_hp cpu_mon |> Int.of_float |>
                    string_of_int) ^ "/" 
                 ^ (PM.get_max_hp cpu_mon |> Int.of_float |>
                    string_of_int) ^ " >");
  print_string "\n";
  print_endline ("--" ^ PM.get_name player_mon ^ "--");
  print_endline ("< hp" ^ ": " 
                 ^ (PM.get_hp player_mon |> Int.of_float |>
                    string_of_int) ^ "/" 
                 ^ (PM.get_max_hp player_mon |> Int.of_float |> 
                    string_of_int) ^ " >");
  print_string "\n";
  print_endline "Choose your move:";
  print_endline (PM.format_moves_names player_mon);
  print_string "\n";
  print_string "> ";
  get_command p_team player_mon cpu_mon bag (read_line ());
  if PM.fainted cpu_mon then 
    if PM.retreat cpu_team then
      raise BattleWon
    else raise (CPUDown (PM.get_name cpu_mon))
  else ();
  print_string "\n";
  execute_cpu_turn player_mon cpu_mon;
  if PM.fainted player_mon then begin
    if PM.retreat p_team then
      raise BattleLost
    else raise (PlayerDown (PM.get_name player_mon))
  end
  else ();
  print_endline "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~";
  loop p_team cpu_team player_mon cpu_mon bag

(** [get_next_pm mons] is the next pokemon to go out after prompting the 
    player to select their next pokemon from [mons]. *)
let rec get_next_pm mons = 
  print_string "> ";
  let next = read_line () in 
  match List.find_opt (fun x -> PM.get_name x = next) mons with 
  | Some p -> p
  | None -> ANSITerminal.(print_string [red] "Invalid Pokemon.");
    get_next_pm mons 

let rec get_y_n () =
  print_string "> ";
  match read_line () with 
  | "Yes" | "Y" | "y" | "yes" -> true
  | "No" | "N" | "n" | "no" -> false
  | _ -> get_y_n ()

let rec battle_handler b m cpu p_mons cpu_mons pmon cpumon = 
  try loop p_mons cpu_mons pmon cpumon b
  with 
  | BattleLost -> ANSITerminal.(print_string [red] 
                                  ("\nYou lost! retreating back to town...\n"));
    (p_mons, b, m, false) 
  | BattleWon -> ANSITerminal.(print_string [green] 
                                 ("\nDo you want to keep going? [Y/N]\n"));
    (p_mons, b, m, get_y_n ())
  | PlayerDown mon -> 
    ANSITerminal.(print_string [yellow]
                    ("\n"^mon^ 
                     " fainted! Who will you send out next?\n"));
    let alive_mons = PM.alive_pmons p_mons in 
    print_endline (PM.string_of_mons alive_mons);
    battle_handler b m cpu p_mons cpu_mons (get_next_pm alive_mons) cpumon
  | CPUDown mon ->
    match (PM.alive_pmons cpu_mons) with 
    | [] -> failwith "Did not successfully end battle."
    | next::_ -> 
      ANSITerminal.
        (print_string [yellow]
           ("\n" ^ mon ^ " fainted! " ^ cpu 
            ^ " is about to send out " ^ PM.get_name next 
            ^ ". Do you want to switch pokemon? [Y/N]\n"));
      if get_y_n () then 
        let alive_mons = PM.alive_pmons p_mons in 
        print_endline (PM.string_of_mons alive_mons);
        battle_handler b m cpu p_mons cpu_mons (get_next_pm alive_mons) next 
      else battle_handler b m cpu p_mons cpu_mons pmon next 

let main (player_team, bag, money, cpu_team, cpu) = 
  ANSITerminal.(print_string [red] "\n\nStarting battle\n");
  PM.set_file "testmons.json";
  match player_team, cpu_team with
  | (pmon :: _), (cpumon :: _) -> 
    battle_handler bag money cpu player_team cpu_team pmon cpumon 
  | _ -> failwith "Empty mons"

(* Execute the game engine. *) 