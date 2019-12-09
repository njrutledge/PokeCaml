open Pokemon
open Moves
(** [PM] is an instance of the Pokemon module. *)
module PM = Pokemon

(** Raised when the player tries to do something illegal. *)
exception IllegalMove of string

(** Raised when the player is not in the pokecenter and tries to heal. *)
exception NotInPC

(** Raised when the player tries to buy an invalid amount of an item. *)
exception InvalidBuyNum

(** Raised when the player does not have the money to buy an item. *)
exception InsuffFunds

(** [update] is the type of update to the game. *) 
type update = 
    State of State.t 
  | Adv of Adventure.t 
  | Both of Adventure.t * State.t
  | None

(** [pp_string s] pretty-prints string [s], as given in test.ml *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_int n] pretty-prints the number [n]. *)
let pp_int x =
  let rec form_str n acc =
    if n >= 1000 then begin 
      let x = n mod 1000 in 
      let str_x = if x = 0 then "000"
        else if x < 10 then "00" ^ string_of_int x 
        else if x < 100 then "0" ^ string_of_int x 
        else string_of_int x
      in 
      if acc <> "" then 
        form_str (n/1000) (str_x ^ "," ^ acc)
      else form_str (n/1000) str_x
    end 
    else if acc <> "" then string_of_int n ^ "," ^ acc
    else string_of_int n
  in
  form_str x ""

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
        else loop (n+1) (acc ^ (pp_elt h1) ^ "\n") t'
    in loop 0 "" lst
  in  pp_elts lst

(** [starters st] prompts the player to pick a starter pokemon if they haven't
    already and returns a new state with the starter in the party. *)
let rec starters st = 
  ANSITerminal.(print_string [green] 
                  ("Professor Oak calls to you as you enter:"
                   ^ "\n\"Ah there you are! Its time that you start your "
                   ^ "pokemon adventure. I have 3 great pokemon here for you "
                   ^ "to choose from, pick your favorite!\"\n"));
  ANSITerminal.(print_string [cyan] 
                  "1. Bulbasaur\n2. Charmander\n3. Squirtle\n\n\n");
  print_string "> ";
  match read_line () with 
  | "1" | "Bulbasaur" | "bulbasaur" -> 
    ANSITerminal.(print_string [green] Ascii.bulbasaur);
    State (State.starter st "Bulbasaur")
  | "2" | "Charmander" | "charmander" -> 
    ANSITerminal.(print_string [red] Ascii.charmander);
    State (State.starter st "Charmander")
  | "3" | "Squirtle" | "squirtle" -> 
    ANSITerminal.(print_string [cyan] Ascii.squirtle);
    State (State.starter st "Squirtle")
  | "4" | "Pikachu" | "pikachu" -> begin 
      ANSITerminal.(print_string [yellow] Ascii.surp_pika);
      State (State.starter st "Pikachu")
    end
  | "quit" -> Global.execute_quit ()
  | _ -> starters st 

(** [execute_go adv st ph] is the state update of the adventure after running 
    [state.go adv st ph'], where [ph'] is the string representation of 
    string list [ph].
    Raises [IllegalMove msg] if the move is invalid. *)
let execute_go adv state exit = 
  match State.go exit adv state with
  | Legal (t) -> if State.current_town_id t = "lab" && State.get_party t = [||] 
    then starters t
    else State t
  | Illegal (msg) -> raise (IllegalMove msg)

(** [execute_bag st] prints the contents of the current bag in [st]. *)
let execute_bag st = 
  ANSITerminal.(print_string [cyan] 
                  ("bag : \n" 
                   ^ "money : ₽" 
                   ^ pp_int !(State.get_money st) 
                   ^ "\n" 
                   ^ pp_list pp_bag_entry (State.bag st)));
  None

(** [execute_party adv st] prints the contents of the current party in [st]. *)
let execute_party st = 
  ANSITerminal.(print_string [cyan] (PM.string_of_mons (State.get_party st)));
  None

(** [print_give_badge st] prints that the player obtained the badge in [st]. *)
let print_give_badge st = 
  let rec find_leader = function
    | [] -> ()
    | h::t -> let name_lst = String.split_on_char ' ' h in 
      match name_lst with 
      | h1::h2::_ -> if h1 = "Gym" && h2 = "Leader" then 
          ANSITerminal.(print_string [green] ("You got the gym badge for " 
                                              ^ State.current_town_id st ^ "!"))
        else find_leader t 
      |_ -> find_leader t 
  in 
  find_leader (State.get_def_tr st)

(** [execute_go_route adv st route] takes [route] from the current town in [st]
    in adventure [adv]. *)
let execute_go_route adv st route = 
  if Array.length (State.get_party st) = 0 then begin
    ANSITerminal.(print_string [yellow] 
                    ("There are dangerous pokemon out there!" 
                     ^ " You should get a pokemon before leaving.\n"));
    None end
  else begin
    let badges = State.get_badges st in 
    let needed_badge = 
      Adventure.req_badge adv (State.current_town_id st) route in 
    if not (List.mem needed_badge badges) && needed_badge <> "" then begin 
      ANSITerminal.(print_string [yellow] ("You need to get this town's badge "
                                           ^ "before you can continue!\n"));
      None
    end 
    else begin 
      match State.route route adv st with 
      | Illegal (msg) -> raise (IllegalMove msg)
      | Legal (st') -> 
        let adv' = st' |> State.get_def_tr |> Adventure.defeat_trainers adv in 
        print_give_badge st';    
        PM.restore_mons (State.get_party st);
        Both (adv', State.clear_def_trs st') end
  end 

(** [in_pokecenter st] is true if the current location of [st] is a
    pokecenter. *)
let in_pokecenter state = 
  let state_name = state |> State.current_town_id |> String.split_on_char ' ' in
  match List.hd state_name with
  | "PokeCenter" -> true
  | _ -> false

(** [execute_heal st] heals all party members in [st] to full health. *)
let execute_heal state = state |> State.get_party |> PM.restore_mons;
  print_endline ("\nThank you for waiting. Your Pokemon have been restored to"
                 ^ " full health. We hope to see you again!"); None

(** [execute_buy st phrase] buys the specified item and ammount as given in 
    [phrase], adding the items to [st] if there is enough money in [st]. *)
let execute_buy state phrase = 
  try begin
    let amt = int_of_string (phrase |> List.hd) in
    let item = List.tl phrase |> String.concat " " in 
    let item_cost = Item.cost_of_item item in
    let money = (State.get_money state) in 
    let total = item_cost * amt in
    if !money - total > 0 then begin 
      money := (!money - total);
      print_endline("\nBought " ^ string_of_int amt ^ " " ^ item ^ ".");
      ANSITerminal.(print_string [red] ("You have " ^ string_of_int !money ^ 
                                        " left."));
      State (State.add_item state (Item.item_of_string item) amt)
    end 
    else raise InsuffFunds
  end
  with 
  | InsuffFunds -> raise InsuffFunds
  | _ -> raise InvalidBuyNum

(** [execute_map a st] prints out the map of the current town of [st] in 
    adventure [a]. *)
let execute_map adv state = begin
  let exits = Adventure.exits adv (State.current_town_id state) in
  let rec exit_str acc e = 
    match e with
    | [] -> acc
    | h :: t ->  exit_str (" - " ^h ^ "\n" ^ acc) t in
  let str = "Here are the places you can go:\n" ^ exit_str "" exits in 
  ANSITerminal.(print_string [cyan] str); None end

(** [execute_moves adv state num] prints the moves of the pokemon corresponding
    to [num] in the players current party in [state].*)
let execute_moves adv state num_txt = begin
  try
    let num = int_of_string (num_txt) in 
    if num >= 1 && num <= Array.length (State.get_party state) then begin
      let mon = (State.get_party state).(num - 1) in
      let moves =  mon |> PM.format_moves_all in
      print_endline ((PM.get_name mon) ^ ":\n" ^ moves); end
    else 
      ANSITerminal.(print_string [red] "Invalid Pokemon.\n");
  with _ -> 
    ANSITerminal.(print_string [red] "Invalid Pokemon.\n"); 
end;
  None 

(** [execute_badges state] is the string representation of what badges the
    player has. *)
let execute_badges state = 
  begin 
    match State.get_badges state with
    | [] -> print_endline "\nYou don't have any badges yet!"
    | badges -> print_endline (pp_list pp_string badges)
  end;
  None

(** [execute_save st] prompts the player to overwrite a save file and create
    a new one. *)
let execute_save state = 
  let overwrite = 
    if Sys.file_exists "save.json" then begin 
      ANSITerminal.(print_string [yellow] 
                      ("This will overwrite your current save file, "^ 
                       "continue? [Y/N]\n")); 
      Global.get_y_n ()
    end 
    else true in 
  if overwrite then begin 
    ANSITerminal.(print_string [cyan] "Saving...\n");
    State.save state;
    ANSITerminal.(print_string [cyan] "Your game has been saved!\n");
  end 
  else ();
  None

(** [execute_shop state] prints a formatted list of 
    the items one can buy in the shop. *)
let execute_shop state () = 
  let items = ["potion"; "super potion"; "hyper potion"; "full restore"; 
               "pokeball"; "great ball"; "ultra ball";
               "master ball"; "antidote"; "paralyze heal"; "awakening"; 
               "ice heal"; "burn heal"; "full heal"] in
  ANSITerminal.(print_string [green] ("You currently have ₽" ^ string_of_int
                                        !(State.get_money state) ^ " left.\n"));
  ANSITerminal.(print_string [cyan] (Item.format_items items)); None

(** [execute_tgm state] does ??? It's a secret to everyone. *)
let execute_tgm state = 
  let st' = State.add_item state (Item.item_of_string "master ball") 1000 in 
  let st'' = State.add_item st' (Item.item_of_string "full restore") 1000 in
  let party = State.get_party st'' in PM.restore_mons party;
  State.get_money st'' := 
    int_of_float (float_of_int !(State.get_money st'') *. 1.5);
  State st''

(** [execute_pc_print st] prints the pokemon currently in the player's pc
    in state [st]. *)
let execute_pc_print state = 
  ANSITerminal.(print_string [cyan] "Here are the pokemon in your pc:\n");
  let print_fun i (name, lvl, _, _ ) = 
    ANSITerminal.(print_string [cyan]
                    (string_of_int (i + 1) ^ ". " ^ " lvl " ^ string_of_int lvl 
                     ^ " " ^ name ^ "\n"))
  in 
  ignore (List.mapi print_fun (State.get_pc state));
  ANSITerminal.(print_string [cyan] 
                  ("\nTo get more information about a pokemon,"
                   ^ " type info #\nTo switch a pokemon in your PC with"
                   ^ " a pokemon in your party, type switch # #\n"));
  None

(** [execute_info st phrase] prints the information of the pokemon [phrase]
    in the pc in state [st]. *)
let execute_info st phrase = 
  let mon_num = try (int_of_string phrase) with Failure _ -> - 1 in 
  let (name, lvl, xp, moves) = try (List.nth (State.get_pc st) (mon_num -1)) 
    with Failure _ -> ("",0,0.0,[]) in  
  if name = "" then 
    ANSITerminal.(print_string [red] "Please input a valid number\n")
  else begin 
    ANSITerminal.
      (print_string [cyan] 
         (name ^ " lvl " ^ (string_of_int lvl) ^ " xp " 
          ^ (string_of_float 
               ((xp/.(float_of_int(lvl-1)**3.)*.100.)|>Float.trunc))));
    let move_str = List.fold_left (fun acc x -> acc ^ x ^"\n") "" moves in 
    ANSITerminal.(print_string [cyan] ("\nmoves:\n" ^ move_str))
  end;
  None

(** [execute switch st phrase] switches two pokemon in your party, the numbers
    are contained in [phrase]. *)
let execute_switch st phrase = 
  let (i1,i2) = match phrase with 
    | x::y::[] -> (try (int_of_string x - 1, int_of_string y - 1)
                   with Failure _ -> (-1, -1))
    | _ -> (-1, -1)
  in 
  if i1 = -1 || i2 = -1 then begin 
    ANSITerminal.(print_string [red] ("Please input two valid integers"));
    None
  end 
  else if i1 >= Array.length (State.get_party st) then begin 
    ANSITerminal.(print_string [red] "Please input two valid intergers");
    None
  end
  else begin 
    let party = State.get_party st in
    let pc = State.get_pc st in
    match List.nth_opt pc i2 with 
    | Some (name, lvl, xp, str_moves) ->
      let moves = List.map(fun x -> Moves.create_move x) str_moves in 
      let pc_mon = PM.create_pokemon name lvl moves in 
      PM.set_xp pc_mon xp;
      let party_mon = party.(i1) in 
      let st' = State.change_pc st i2 party_mon in 
      party.(i1) <- pc_mon; 
      ANSITerminal.(print_string [green] 
                      ("You switched out your "
                       ^ PM.get_name party_mon ^ " for " 
                       ^ PM.get_name pc_mon ^ "!"));
      State st'
    | None -> None
  end

(** [execute_swap state phrase] calls helper execute_switch to switch two
    pokemon in the player's party. *)
let execute_swap state phrase = 
  let (i1,i2) = match phrase with 
    | x::y::[] -> (try (int_of_string x - 1, int_of_string y - 1)
                   with Failure _ -> (-1, -1))
    | _ -> (-1, -1)
  in 
  if i1 = -1 || i2 = -1 then begin 
    ANSITerminal.(print_string [red] ("Please input two valid integers"));
    None
  end 
  else begin 
    let party = State.get_party state in 
    let len = Array.length party in 
    if (i1>=len || i2 >= len) then begin 
      ANSITerminal.(print_string [red] "Please use valid integer range");
      None
    end 
    else begin
      State.swap_party state i1 i2;
      None
    end 
  end 

(** [execute_command adv state input] is the update created by executing
    command [input] on adventure [adv] and state [state]. *)
let rec execute_command adv state input = 
  match Command.parse input with 
  | Quit -> Global.execute_quit ()
  | Go(phrase) -> execute_go adv state (String.concat " " phrase)
  | GoRoute(phrase) -> execute_go_route adv state (String.concat " " phrase)
  | Shop -> if in_pokecenter state then execute_shop state ()
    else raise NotInPC
  | Party -> execute_party state
  | Bag -> execute_bag state
  | Heal -> if in_pokecenter state then execute_heal state else raise NotInPC
  | PC -> if in_pokecenter state then execute_pc_print state else raise NotInPC
  | Map -> execute_map adv state 
  | Buy(phrase) -> 
    if in_pokecenter state then execute_buy state phrase
    else raise NotInPC
  | Badges -> execute_badges state
  | Moves(phrase) -> execute_moves adv state (String.concat " " phrase)
  | Save -> execute_save state 
  | TGM -> execute_tgm state
  | Info phrase -> execute_info state (String.concat " " phrase)
  | Switch phrase -> execute_switch state phrase
  | Swap phrase -> execute_swap state phrase

(** [get_command adv state input] tries to execute the user's command [input]
    with state [st] and adventure [adv]. Catches any errors during this 
    execution, prints a valid error message, and loops. *)
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
  | Adventure.UnknownBadge it -> 
    print_string ("\nYou can't progress because you don't have the  \"" 
                  ^ it ^ "\" badge.\n");
    print_string "> ";
    get_command adv state (read_line ())
  | State.ItemNotFound it ->
    print_string ("\nYou do not have item \"" ^ it ^"\".\n");
    print_string "> ";
    get_command adv state (read_line ())
  | State.BadgeNotFound ->
    print_string("\nYou do not have the correct badge.\n");
    print_string "> ";
    get_command adv state (read_line ())
  | Adventure.UnknownExit ex -> 
    print_string("\nExit \"" ^ ex ^ "\" does not exist.\n");
    print_string "> ";
    get_command adv state (read_line ())
  | NotInPC -> 
    print_string("\nYou're not in a Pokemon Center.\n");
    print_string "> ";
    get_command adv state (read_line ())
  | InsuffFunds -> 
    print_string("\nYou don't have enough money to buy that.\n");
    print_string "> ";
    get_command adv state (read_line ())
  | InvalidBuyNum -> 
    print_string("\nPlease enter a valid number and word (e.g. \"buy 5 master "
                 ^ "ball\")\n");
    print_string "> ";
    get_command adv state (read_line ())

(** [in_list el lst] is true if [el] is in the list [lst]. *)
let rec in_list el = function
  | [] -> false
  | h :: t -> if h = el then true else in_list el t

(** [win_help state] is the win helper. *)
let win_help state = 
  ANSITerminal.(print_string [white] ("\n\n\n" ^ Ascii.clarkson ^ "\n\n\n"));
  ANSITerminal.
    (print_string [green] 
       ("Congratulations on winning!"^
        " Your pursuits will go down in the Pokemon 3110 Hall of Fame." 
        ^ "\n\n\n"));
  ANSITerminal.(print_string [cyan] "\nThanks for playing!\n ")

(** [loop adv state] executes a REPL for the game. Quits on recieving 
    "quit". *)
let rec loop adv state print_desc = 
  if (State.current_town_id state) = "Hall of Fame" then win_help state
  else begin
    print_string "\n";
    if print_desc then begin
      state 
      |> State.current_town_id 
      |> Adventure.description adv 
      |> print_endline;
    end
    else ();
    print_string "> ";
    get_command adv state (read_line ()) 
    |> function 
    | State state'-> loop adv state' true
    | Adv adv' -> loop adv' state true
    | Both (adv', state') -> loop adv' state' true
    | None -> loop adv state false
  end

(** [play_game f] starts the adventure in file [f]. *)
let play_game f =
  ANSITerminal.(print_string [yellow]
                  Ascii.pokemon_opening);
  print_endline "\n\n";
  ANSITerminal.(print_string [red]
                  Ascii.str3110);
  print_endline "\n\n";
  let adv = (Adventure.from_json (Yojson.Basic.from_file f)) in 
  let state = 
    if Sys.file_exists "save.json" then begin 
      print_endline "Save detected! Use save file? [Y/N]" ;
      if Global.get_y_n () then State.load () 
      else State.init_state adv
    end 
    else State.init_state adv in 
  let adv' = Adventure.defeat_trainers adv (State.get_def_tr_all state) in 
  loop adv' state true

(* Execute the game engine. *)
let () = play_game ("adv.json")