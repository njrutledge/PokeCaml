(* TODO: ascii art, add moves for cpu, stab, better ai, certain abilities,
   items, crits, stat affecting moves, self damaging moves, variable damage
   moves, more types, special vs physical *)

open Moves
open Pokemon
module PM = Pokemon

(** [BattleWon p] is raised when battle is won. *)
exception BattleWon of PM.t array

(** Raised when battle is lost *)
exception BattleLost

(** Raised when a player pokemon faints. *)
exception PlayerDown of string 

(** Raised when a cpu pokemon faints. *)
exception CPUDown of string * PM.t

(** [IllegalItem i] is raised when the player tries to use item [i] 
    but does not have any of [i] in their bag. *)
exception IllegalItem of string

(** [BattleRun] is raised when the player successfully runs from a battle. *)
exception BattleRun

(** sleep is used for delay in text printing. *)
let sleep = 1.0

let count = ref 0.

(** [execute_quit] quits the adventure. *)
let execute_quit () = 
  ANSITerminal.(print_string [cyan] "\nThanks for playing!\n "); 
  exit 0

(** [get_y_n ()] is true if the user inputs an affirmative, false if negative, 
    quits the application if inputs quit, and loops otherwise. *)
let rec get_y_n () =
  print_string "> ";
  match read_line () with 
  | "Yes" | "Y" | "y" | "yes" -> true
  | "No" | "N" | "n" | "no" -> false
  | "quit" -> execute_quit ()
  | _ -> get_y_n ()

let rec next_mon mons = 
  print_string "> ";
  try 
    let next = int_of_string (read_line ()) in 
    if next >= 1 && next <= Array.length mons then
      next - 1
    else begin ANSITerminal.(print_string [red] "Invalid Pokemon.\n");
      next_mon mons
    end 
  with _ -> 
    (ANSITerminal.(print_string [red] "Invalid Pokemon.\n");
     next_mon mons) 

let critical_hit speed = 
  let t = speed /. 2. in
  let t_round = t |> Int.of_float |> Float.of_int in
  Random.self_init();
  let r = Random.float 256. in
  if r < t_round then begin print_endline "A critical hit!\n"; true end else false

(* https://bulbapedia.bulbagarden.net/wiki/Damage *)
let damage level power attack defense speed modifier = 
  let level' = if critical_hit speed then 2. *. level 
    else level in
  modifier*.((((((2.*.level')/.5.)+. 2.)*.power*.(attack/.defense))/. 50.) +. 2.)

let rec get_modifier move_type acc mat hash = function
  | [] -> acc
  | h :: t -> get_modifier move_type (acc*.mat.(hash move_type).(hash h)) mat hash t

(** [execute_go adv st ph] is the state update of the adventure after running 
    [state.go adv st ph'], where [ph'] is the string representation of 
    string list [ph].
    Raises [IllegalMove msg] if the move is invalid. *)
let execute_attack (atk_mon : PM.t) (def_mon : PM.t) move_id = 
  let json = Yojson.Basic.from_file "type_matrix.json" in 
  let type_mat_and_hash = Types.type_matrix_and_hash json in
  let type_mat = fst type_mat_and_hash in 
  let hash = snd type_mat_and_hash in 
  let move = PM.get_move atk_mon (move_id) in 
  print_endline (PM.get_name atk_mon ^ " used " ^ (Moves.name move) ^ "!");
  let modifier = (get_modifier move.el_type 1. type_mat hash (PM.get_type
                                                                def_mon)) in
  let move_damage = 
    if (Moves.name move) = "super fang" 
    then ((PM.get_hp def_mon) /. 2.)
    else damage 
        (PM.get_lvl atk_mon)
        move.power
        (PM.get_attack atk_mon)
        (PM.get_defense def_mon)
        (PM.get_speed atk_mon)
        modifier
  in
  if modifier = 0. then 
    print_endline ("It has no effect!") 
  else if modifier < 1. then 
    print_endline ("It's not very effective...")
  else if modifier >= 2. then
    print_endline ("It's super effective!")
  else ();
  PM.change_hp def_mon (-.move_damage)

let b_calc ball =  
  match ball with 
  | Item.PokeBall -> 255.
  | Item.GreatBall -> 200. 
  | Item.UltraBall -> 150.
  | _ -> failwith "Only ball can shake, never should have this error"

(** [print_shake] prints "*shake*" with a delay afterwards. *)
let print_shake () = print_endline "*shake*"; Unix.sleep 1

(** [shake ball f] is the handler for printing how many times the ball should
    shake if the pokemon is not caught. *)
let shake ball f = 
  let b = b_calc ball in 
  let d = (128. *. 100.) /. b in
  let d_round = d |> Float.to_int |> Int.to_float in begin
    if d_round >= 256. then begin
      print_shake (); print_shake (); print_shake ();
      print_endline "The pokemon broke free!"
    end 
    else begin
      let x = d_round *. f /. 255. in 
      let x_round = x |> Float.to_int |> Int.to_float in 
      if x_round < 10. then print_endline "The ball missed!"
      else if x_round < 30. then begin
        print_shake (); print_endline "The pokemon broke free!" 
      end
      else if x_round < 70. then begin
        print_shake(); print_shake(); print_endline "The pokemon broke free!" 
      end
      else begin
        print_shake(); print_shake () ; print_shake ();
        print_endline "The pokemon broke free!"
      end
    end
  end;
  false

(* [catch def_mon ball] determines whether or not a wild pokemon is
   caught, using the gen 1 catch formula here: https://bulbapedia.bulbagarden.net/wiki/Catch_rate#Capture_method_.28Generation_I.29  *)
let catch mon ball (rate : float) = 
  if ball = Item.MasterBall then begin 
    print_shake(); print_shake(); print_shake(); true
  end
  else begin 
    Random.self_init(); 
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
    if f_round >= m then begin 
      print_shake(); print_shake(); print_shake(); true 
    end
    else shake ball f_round 
  end

let handle_add_mon party mon = 
  if Array.length party = 6 then begin 
    print_endline ("Do you want to add " ^ PM.get_name mon 
                   ^ " to your party?[Y/N]");
    if get_y_n () then begin 
      print_endline ("Swap " ^ PM.get_name mon ^ " with which party member?");
      print_endline (PM.string_of_mons party);
      let swap_n = next_mon party in
      let mon_to_box = party.(swap_n) in  
      party.(swap_n) <- mon;
      print_endline (PM.get_name mon_to_box ^ " was sent to a box and " ^
                     PM.get_name mon ^ " was added to your party!");
      raise (BattleWon party)
    end 
    else 
      print_endline (PM.get_name mon ^ " was sent to a box.");
    raise (BattleWon party)
  end 
  else raise (BattleWon (PM.add_mon party mon))

let execute_item atk_mon def_mon item bag party cpu= 
  let i_count = 
    try (List.assoc item bag) 
    with Not_found ->
      raise (IllegalItem ("You don't have any " ^ Item.string_of_item item 
                          ^ "s to use!\n")) in
  if !i_count <= 0 
  then raise (IllegalItem ("You don't have any " ^ Item.string_of_item item 
                           ^ "s to use!\n"))
  else 
    i_count := !i_count - 1;
  match item with 
  | Item.Potion -> PM.change_hp atk_mon ((PM.get_max_hp atk_mon)*. 0.3);
    print_endline ((PM.get_name atk_mon) ^ "'s HP was restored by 30%.")
  | Item.HyperPotion -> PM.change_hp atk_mon ((PM.get_max_hp atk_mon)*. 0.6);
    print_endline ((PM.get_name atk_mon) ^ "'s HP was restored by 60%.")
  | Item.FullRestore -> PM.change_hp atk_mon (PM.get_max_hp atk_mon);
    print_endline ((PM.get_name atk_mon) ^ "'s HP was restored by 100%. ")
  | Item.PokeBall | Item.GreatBall | Item.UltraBall | Item.MasterBall ->
    begin
      if cpu = "wild" then 
        let caught = catch def_mon item 255. in 
        if caught then begin 
          print_endline ("\n" ^ (PM.get_name def_mon) ^ " was caught!\n");
          handle_add_mon party def_mon
        end 
        else print_endline ("\n" ^ (PM.get_name def_mon) ^ " wasn't caught.\n") 
      else print_endline ("\n" ^ cpu ^ " blocked the ball!")
    end

let string_of_bag bag =
  let rec string_of_bag' bag (acc : string) = 
    match bag with
    | [] -> acc
    | (i, n) :: t -> begin
        if !n > 0 then 
          string_of_bag' t (acc ^ (Item.string_of_item i) ^ ": " ^
                            (string_of_int !n) ^ "\n")
        else string_of_bag' t acc
      end
  in (string_of_bag' bag "\n") ^ "\n"

(** [get_next_pm mons] is the next pokemon to go out after prompting the 
    player to select their next pokemon from [mons]. *)
let get_next_pm mons = 
  print_endline ("Who do you want to send out next?");
  print_endline (PM.string_of_mons mons);
  mons.(next_mon mons)

let execute_run party p_mon cpu_mon bag = 
  let p_speed = PM.get_speed p_mon in
  let cpu_speed = PM.get_speed cpu_mon in 
  let b_mod b = b mod 256 in
  if p_speed > cpu_speed then raise BattleRun
  else begin
    let b = cpu_speed |> (/.) 4. |> Int.of_float |> b_mod |> Float.of_int in
    if b = 0. then raise BattleRun
    else begin    
      let f = ((p_speed *. 32.) /. b) +. 30. *. !count in
      if f > 255. then raise BattleRun 
      else begin
        Random.self_init();
        let r = Random.float 256. in 
        if r < f then raise BattleRun
        else count := !count +. 1.; ()
      end
    end 
  end

let rec execute_command party atk_mon def_mon bag cpu input = 
  match Btlcmd.parse input with 
  | Quit -> execute_quit ()
  | Attack(i) -> (if 1 <= i && i <= Array.length (PM.get_moves atk_mon) then 
                    begin count := 0.0; execute_attack atk_mon def_mon (i-1) end
                  else raise (Pokemon.UnknownMove (string_of_int i))); None
  | Item(phrase) -> 
    (execute_item atk_mon def_mon
       (Item.item_of_string (String.concat " " phrase))
       bag party cpu); None
  | Bag -> ANSITerminal.(print_string [cyan] (string_of_bag bag));
    print_string "> ";
    execute_command party atk_mon def_mon bag cpu (read_line ())
  | MovesInfo ->
    ANSITerminal.(print_string [cyan] 
                    ((PM.format_moves_all atk_mon) ^ "\n"));
    print_string "> ";
    execute_command party atk_mon def_mon bag cpu (read_line ())
  | Party -> ANSITerminal.(print_string [cyan] 
                             ("\n" ^ (PM.string_of_mons party) ^ "\n"));
    print_string "> ";
    execute_command party atk_mon def_mon bag cpu (read_line ())
  | Switch -> Some (get_next_pm (PM.alive_pmons party))
  | Run -> if cpu = "wild" then begin execute_run party atk_mon def_mon bag;
      print_endline (string_of_float !count); 
      None end 
    else begin 
      ANSITerminal.(print_string [red] 
                      ("\nYou cannot run from this battle!\n"));
      print_endline "> ";
      execute_command party atk_mon def_mon bag cpu (read_line ()) 
    end 


let rec get_command party atk_mon def_mon bag cpu input = 
  try
    execute_command party atk_mon def_mon bag cpu input
  with 
  | Btlcmd.Empty -> 
    ANSITerminal.(print_string [red]
                    "\nError: please give command.\n ");
    print_string "> ";
    get_command party atk_mon def_mon bag cpu (read_line ())
  | Btlcmd.Malformed ->
    ANSITerminal.(print_string [red]
                    "\nError: please input valid command.\n");
    print_string "> ";
    get_command party atk_mon def_mon bag cpu (read_line ())
  | Pokemon.UnknownMove m -> 
    ANSITerminal.(print_string [red] 
                    ("\nError: move not valid. Please input a "
                     ^ "valid move number.\n"));
    print_string "> ";
    get_command party atk_mon def_mon bag cpu (read_line ())
  | Item.InvalidItem i -> 
    ANSITerminal.(print_string [red] "\nError: invalid item name.\n");
    print_string "> ";
    get_command party atk_mon def_mon bag cpu (read_line ())
  | IllegalItem i -> 
    ANSITerminal.(print_string [red] ("\nError: " ^ i));
    print_string "> ";
    get_command party atk_mon def_mon bag cpu (read_line ())

let execute_cpu_turn player_mon cpu_mon = 
  Random.self_init();
  let cpu_moves = PM.get_moves cpu_mon in 
  let used_move = (Random.int (Array.length cpu_moves)) in 
  execute_attack cpu_mon player_mon used_move

(** [percent_hp_color precent hp_str] prints the string of the hp in the
    correct color.*)
let percent_hp_color percent hp_str = 
  if percent >= 50 then 
    ANSITerminal.(print_string [green] hp_str)
  else if percent >= 20 then 
    ANSITerminal.(print_string [yellow] hp_str)
  else 
    ANSITerminal.(print_string [red] hp_str)

(** [cpu_hp_percent cpu_mon] prints the hp of [cpu_mon] as a percent. *)
let cpu_hp_percent cpu_mon = 
  let num = (PM.get_hp cpu_mon) in
  let denom = (PM.get_max_hp cpu_mon) in
  let percent_float = (num /. denom) *. 100. in
  let percent = if 0.<= percent_float && percent_float <= 1. then 1 else
      Int.of_float percent_float in 
  let str_percent = "< hp: " ^ (string_of_int percent) ^ "% >" in
  percent_hp_color percent str_percent

(** [player_hp_percent p_mon] prints the hp of [p_mon] as a percent. *)
let player_hp_percent p_mon = 
  let num = (PM.get_hp p_mon) in
  let denom = (PM.get_max_hp p_mon) in
  let percent_float = (num /. denom) *. 100. in
  let percent = if 0.<= percent_float && percent_float <= 1. then 1 else
      Int.of_float percent_float in 
  let hp_str = "< " ^ PM.hp_string p_mon ^ " >" in
  percent_hp_color percent hp_str

(** [loop adv state] executes a REPL for the game. Quits on recieving 
    "quit". *)
let rec loop p_team cpu_team player_mon cpu_mon bag cpu = 
  print_string "\n";
  print_endline ("--" ^ (PM.get_name cpu_mon) ^ "--");
  cpu_hp_percent cpu_mon;
  print_endline "\n";
  print_endline ("--" ^ PM.get_name player_mon ^ "--");
  player_hp_percent player_mon;
  print_endline "\n";
  print_endline "Choose your move:";
  print_endline (PM.format_moves_names player_mon);
  print_endline "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~";
  print_string "> ";
  let res = get_command p_team player_mon cpu_mon bag cpu (read_line ()) in 
  Unix.sleepf sleep;
  if PM.fainted cpu_mon then 
    if PM.retreat cpu_team then
      raise (BattleWon p_team)
    else raise (CPUDown (PM.get_name cpu_mon, player_mon))
  else ();
  print_string "\n";
  begin match res with 
    | Some p -> execute_cpu_turn p cpu_mon
    | None -> execute_cpu_turn player_mon cpu_mon
  end;
  Unix.sleepf sleep;
  if PM.fainted player_mon then begin
    if PM.retreat p_team then
      raise BattleLost
    else raise (PlayerDown (PM.get_name player_mon))
  end
  else ();
  match res with 
  | Some p -> loop p_team cpu_team p cpu_mon bag cpu
  | None -> loop p_team cpu_team player_mon cpu_mon bag cpu 

let rec give_xp_all cpu_lvl wild mons = 
  let give_xp = (fun m -> PM.give_xp m cpu_lvl wild; 
                  if PM.lvl_up m then print_endline (PM.get_name m ^ " leveled up!")
                  else ();) in
  Array.iter (give_xp) mons

let rec battle_handler b m cpu p_mons cpu_mons pmon cpumon = 
  if cpu <> "wild" then
    ANSITerminal.(print_string [yellow] 
                    (cpu ^ " sends out " ^ (PM.get_name cpumon) ^ "!\n"))
  else ();
  try loop p_mons cpu_mons pmon cpumon b cpu 
  with 
  | BattleLost -> PM.restore_mons cpu_mons; 
    ANSITerminal.(print_string [red] 
                    ("\nYou lost! Retreating back to town...\n"));
    PM.restore_mons p_mons;
    (p_mons, b, m, false) 
  | BattleWon party -> 
    if cpu = "wild" then 
      ANSITerminal.(print_string [yellow] ("You defeated the wild " ^ PM.get_name cpumon ^ "!\n"))
    else 
      ANSITerminal.(print_string [yellow] ("You defeated " ^ cpu ^ "!\n"));
    print_endline "The pokemon in your party gain experience!";
    give_xp_all (PM.get_lvl cpumon) (cpu = "wild") (PM.alive_pmons p_mons);
    ANSITerminal.(print_string [green] ("\nDo you want to keep going? [Y/N]\n"));
    (party, b, m, get_y_n ())
  | PlayerDown mon -> 
    ANSITerminal.(print_string [yellow]
                    ("\n"^mon^ 
                     " fainted! Who will you send out next?\n"));
    let alive_mons = PM.alive_pmons p_mons in 
    battle_handler b m cpu p_mons cpu_mons (get_next_pm alive_mons) cpumon
  | CPUDown (mon, curr_pmon) ->
    let alive_mons = PM.alive_pmons p_mons in 
    let next = (PM.alive_pmons cpu_mons).(0) in 
    give_xp_all (PM.get_lvl cpumon) false alive_mons;
    ANSITerminal.
      (print_string [yellow]
         ("\n" ^ mon ^ " fainted! " ^ cpu 
          ^ " is about to send out " ^ PM.get_name next 
          ^ ". Do you want to switch pokemon? [Y/N]\n"));
    if get_y_n () then begin 
      battle_handler b m cpu p_mons cpu_mons (get_next_pm alive_mons) next 
    end 
    else battle_handler b m cpu p_mons cpu_mons curr_pmon next 
  | BattleRun -> ANSITerminal.(print_string [red] ("\nGot away safely!\n"));
    ANSITerminal.(print_string [green] ("\nDo you want to keep going? [Y/N]\n"));
    (p_mons, b, m, get_y_n ())

let main (player_team, bag, money, cpu_team, cpu) = 
  let alive_p_team = PM.alive_pmons player_team in 
  let pmon = alive_p_team.(0) in 
  let cpumon = cpu_team.(0) in 
  if cpu = "wild" then begin
    count := 0.;
    ANSITerminal.(print_string [yellow] (
        "\nA wild " ^ (PM.get_name cpumon) ^ " appeared!\n"))
  end 
  else 
    ANSITerminal.(print_string [yellow] (cpu ^ " challenges you to a battle!\n"));
  battle_handler bag money cpu player_team cpu_team pmon cpumon 
(* Execute the game engine. *) 