open Moves
open Pokemon
module PM = Pokemon

(** [BattleWon p, cp] is raised when battle is won, where
    [p] is the current party and [cp] is [Some m] if [m] is a mon 
    to be added to the player's cp, and is [None] otherwise. *)
exception BattleWon of PM.t array * (PM.t option)

(** Raised when battle is lost *)
exception BattleLost

(** Raised when a player pokemon faints. *)
exception PlayerDown of string 

(** Raised when a cpu pokemon faints. *)
exception CPUDown of string

(** [IllegalItem i] is raised when the player tries to use item [i] 
    but does not have any of [i] in their bag. *)
exception IllegalItem of string

(** [BattleRun] is raised when the player successfully runs from a battle. *)
exception BattleRun

(** [NoPP] is raised when a pokemon tries to use a fully exhausted move. *)
exception NoPP

(** [count] is the count of the amount of times run in a row. *)
let count = ref 0.

(** [execute_quit] quits the adventure. *)
let execute_quit () = 
  ANSITerminal.(print_string [cyan] "\nThanks for playing!\n "); 
  exit 0

(** [get_move_num] asks the user to input a number from 1 to 5 inclusive.
    If a valid number is entered, it returns the 
    inputed number mi, otherwise it asks the
    user to try input a valid number. *)
let rec get_move_num () = 
  print_string "> ";
  try 
    let next = int_of_string (read_line ()) in 
    if next >= 1 && next <= 5 then
      next - 1 
    else begin 
      ANSITerminal.(print_string [red] ("Invalid move. Please enter a number " ^
                                        " 1 to 5.\n"));
      get_move_num ()
    end 
  with _ -> 
    ANSITerminal.(print_string [red] ("Invalid move. Please enter a valid " ^
                                      "number 1 to 5.\n"));
    get_move_num ()  

(** [next_mon mons] asks the user to input a number corresponding to the 
    pokemon in [mons] they want to send out. If a valid number is entered, that
    number - 1 is returned (which will be a number between 0 and 3 inclusive).
    Otherwise, it prompts the user to input a valid number. *)
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

(** [effect_help atk_mon def_mon eff_type effect] is the helper that handles
    moves that affects status (e.g. stat changes). *)
let effect_help atk_mon def_mon eff_type effect =
  Random.self_init ();
  let rand = Random.float 100.0 in 
  match effect with 
  | change :: "self" :: per_chance :: [] -> 
    if (float_of_string per_chance >= rand) then begin 
      let c = if (int_of_string change) > 0 then "up" else "down" in
      ANSITerminal.(print_string [red]  (PM.get_name atk_mon ^ "'s " 
                                         ^ eff_type ^ " went " ^ c ^ "!\n"));
      PM.change_stage atk_mon eff_type (int_of_string change);
      Unix.sleepf (Global.get_sleep_speed ())
    end else ()
  | change :: "foe" :: per_chance :: [] -> 
    if (float_of_string per_chance >= rand) then begin 
      let c = if int_of_string change > 0 then "up" else "down" in
      ANSITerminal.(print_string [red] (PM.get_name def_mon ^ "'s " 
                                        ^ eff_type ^ " went " ^ c ^ "!\n"));
      PM.change_stage def_mon eff_type (int_of_string change);
      Unix.sleepf (Global.get_sleep_speed ())
    end else ()
  | _ -> ANSITerminal.(print_string [red] ("Invalid " ^ eff_type ^ " effect."
                                           ^ "skipping this effect..."))

(** [status_help atk_mon def_mon status_type info] correctly applies a status
    of type [status_type] to either [atk_mon] or [def_mon] using the 
    information in [info]. *)
let status_help atk_mon def_mon status_type info = 
  let target, chance = begin 
    match info with 
    | "self" :: c :: [] -> (atk_mon, float_of_string c)
    | "foe" :: c :: [] -> (def_mon, float_of_string c)
    | _ -> (atk_mon, -1.0)
  end 
  in 
  Random.self_init ();
  let rand = Random.float 100.0 in 
  if chance = -1.0 then 
    ANSITerminal.(print_string [red] ("Invalid " ^ status_type ^ " status."
                                      ^ "skipping this status..."))
  else if chance >= rand && PM.get_status target = "" then begin 
    let n = PM.get_name target in
    match status_type with
    | "sleep" -> PM.set_status target "sleep"; 
      print_endline (n ^ " was put to sleep!");
      Unix.sleepf (Global.get_sleep_speed ())
    | "paralyze" -> PM.set_status target "paralyze"; 
      print_endline (n ^ " was paralyzed! It may be unable to move!");
      Unix.sleepf (Global.get_sleep_speed ())
    | "confuse" -> if (fst (PM.get_confusion target)) then () 
      else 
        begin 
          PM.set_confusion target (true, 4); 
          print_endline (n ^ " became confused!");
          Unix.sleepf (Global.get_sleep_speed ())
        end 
    | "burn" -> if not (List.mem "fire" (PM.get_type target)) then begin
        PM.set_status target "burn"; 
        print_endline (n ^ " was burned!");
        Unix.sleepf (Global.get_sleep_speed ())
      end
    | "poison" -> if not (List.mem "poison" (PM.get_type target)) && 
                     not (List.mem "steel" (PM.get_type target)) then begin
        PM.set_status target "poison"; 
        print_endline (n ^ " was poisoned!");
        Unix.sleepf (Global.get_sleep_speed ())
      end
    | "frozen" -> PM.set_status target "frozen"; 
      print_endline (n ^ " was frozen!");
      Unix.sleepf (Global.get_sleep_speed ())
    | "flinch" -> PM.set_status target "flinch"; 
      print_endline (n ^ " flinched!");
      Unix.sleepf (Global.get_sleep_speed ())
    | _ -> ANSITerminal.(print_string [red] 
                           ("Invalid status name: "  ^ status_type  
                            ^ " skipping this status..."))
  end 
  else ()

(** [heal_help atk_mon def_mon x info] handles a healing effect caused by 
    a move used by [atk_mon] against [def_mon] dealing [x] ammount of damage. 
    [info]. is a list of string containing first 
    the target (["self"] or ["foe"]) and then the ammount healed, which is 
    a decimal ammount of the damage delt [x], or is ["full"] if the move 
    fully heals. *)
let heal_help atk_mon def_mon heal damage info = 
  let (target, amt) = 
    match info with 
    | "self" :: amt :: [] -> (atk_mon, amt)
    | "foe" :: amt :: [] -> (def_mon, amt)
    | _ -> (atk_mon, "")
  in 
  if amt = "" then ANSITerminal.(print_string [red] 
                                   "Invalid heal, skipping heal ")
  else if amt = "full" then begin 
    PM.set_hp target (PM.get_max_hp target);
    print_endline (PM.get_name target ^ " was fully healed!");
  end 
  else if amt = "half" then begin 
    PM.change_hp target (PM.get_max_hp target /. 2.);
    print_endline (PM.get_name target ^ " restored half its health!");
  end 
  else try 
      if heal then begin 
        PM.change_hp target (float_of_string amt *. damage);
        print_endline (PM.get_name target ^ " was healed by the attack!");
      end 
      else begin PM.change_hp target (-. float_of_string amt *. damage);
        print_endline (PM.get_name target ^ " was hurt by recoil!");
      end 
    with Failure e -> 
      ANSITerminal.(print_string [red] ("Invalid heal ammount: " ^ amt 
                                        ^ ", skipping heal "));
      Unix.sleepf (Global.get_sleep_speed ())

(** [endeavor_help atk_mon def_mon] applies the effects of move "endeavor"
    onto [def_mon] when used by [atk_mon]. *)
let endeavor_help atk_mon def_mon = 
  let atk_hp = PM.get_hp atk_mon in 
  let def_hp = PM.get_hp def_mon in
  if atk_hp <= def_hp then PM.set_hp def_mon atk_hp 
  else ()

(** [gambit_help atk_mon def_mon] applies the effects of move "final gambit"
    onto [def_mon] when used by [atk_mon]. *)
let gambit_help atk_mon def_mon = 
  let damage = PM.get_hp atk_mon in 
  PM.change_hp def_mon damage;
  PM.change_hp atk_mon damage

(** [effect_handler atk_mon def_mon effects] applies the correct effect 
    [effect] of a move to [atk_mon] and/or [def_mon]. *)
let effect_handler atk_mon def_mon effects damage = 
  let rec apply_effects = function
    | [] -> ()
    | h :: t -> 
      match String.split_on_char ' ' h with
      | "ATK" :: tl -> effect_help atk_mon def_mon "attack" tl; apply_effects t
      | "SPA" :: tl -> effect_help atk_mon def_mon "special attack" tl;
        apply_effects t
      | "DEF" :: tl -> effect_help atk_mon def_mon "defense" tl; apply_effects t
      | "SPD" :: tl -> effect_help atk_mon def_mon "special defense" tl;
        apply_effects t
      | "SPE" :: tl -> effect_help atk_mon def_mon "speed" tl; apply_effects t
      | "ACC" :: tl -> effect_help atk_mon def_mon "accuracy" tl;
        apply_effects t
      | "sleep" :: tl -> status_help atk_mon def_mon "sleep" tl; apply_effects t
      | "paralyze" :: tl -> status_help atk_mon def_mon "paralyze" tl;
        apply_effects t
      | "burn" :: tl -> status_help atk_mon def_mon "burn" tl; apply_effects t
      | "frozen" :: tl -> status_help atk_mon def_mon "frozen" tl;
        apply_effects t
      | "poison" :: tl -> status_help atk_mon def_mon "poison" tl;
        apply_effects t
      | "flinch" :: tl -> status_help atk_mon def_mon "flinch" tl;
        apply_effects t
      | "confuse" :: tl -> status_help atk_mon def_mon "confuse" tl;
        apply_effects t
      | "clear" :: [] -> PM.set_status atk_mon ""; 
        PM.set_confusion atk_mon (false, 0);
        print_endline (PM.get_name atk_mon ^ " cleared its status!" );
        apply_effects t
      | "heal" :: tl -> heal_help atk_mon def_mon true damage tl;
        apply_effects t
      | "hurt" :: tl -> heal_help atk_mon def_mon false damage tl;
        apply_effects t
      | "endeavor" :: [] -> endeavor_help atk_mon def_mon ; apply_effects t
      | "gambit" :: [] -> gambit_help atk_mon def_mon ; apply_effects t
      | "rest" :: [] -> PM.rem_status atk_mon; PM.set_status atk_mon "rest";
        PM.set_sleep_counter atk_mon 2; 
        PM.set_hp atk_mon (PM.get_max_hp atk_mon);
        print_endline (PM.get_name atk_mon ^ " fully cured itself, "
                       ^ "and fell asleep!");
        Unix.sleepf (Global.get_sleep_speed ());
        apply_effects t
      | _ -> ANSITerminal.(print_string  [red] 
                             ("This move has raised an invalid effect. The " 
                              ^ h ^ "effect will not take place.\n"));
        apply_effects t
  in apply_effects effects

(** [critical_hit speed] is whether a critical hit occurred using [speed] in 
    the calculation. *)
let critical_hit speed = 
  let t = speed /. 2. in
  let t_round = t |> Int.of_float |> Float.of_int in
  Random.self_init();
  let r = Random.float 256. in
  if r < t_round then begin print_endline "A critical hit!";
    Unix.sleepf (Global.get_sleep_speed ());
    true end
  else false

(** [damage level power attack defense speed modifier] is the damage done using
    the calulation provided by https://bulbapedia.bulbagarden.net/wiki/Damage *)
let damage level power attack defense speed modifier = 
  let level' = if power <> 0. && critical_hit speed then 2. *. level 
    else level in
  modifier*.(((((2.*.level'/.5.)+. 2.)*.power*.(attack/.defense))/. 50.) +. 2.)

(** [get_modifier move_type acc mat hash def_type] is the damage modifier 
    of based on [move_type] and [def_type]. *)
let rec get_modifier move_type acc mat hash = function
  | [] -> acc
  | h :: t -> 
    get_modifier move_type (acc *. mat.(hash move_type).(hash h)) mat hash t

(** [check_zero_pp_all moves] checks to see if all moves in [moves] are out of
    pp. *)
let check_zero_pp_all moves =
  Array.for_all (fun x -> (Moves.get_pp x) = 0) moves

(** [check_hit acc] checks if a move with accuracy [acc] will hit. *)
let check_hit acc = 
  let t = acc *. 2.55 in
  let r = Random.self_init(); Random.float 256. in
  if r < t then true 
  else false 

(** [check_confusion mon] checks if [mon] is confused. *)
let check_confusion mon = 
  let name = PM.get_name mon in 
  Random.self_init ();
  let r = Random.float 100.0 in 
  let (confused, count) = PM.get_confusion mon in 
  if confused then begin 
    print_endline (name ^ " is confused!");
    Unix.sleepf (Global.get_sleep_speed ());
    Random.self_init ();
    let rand = Random.int 3 + 1 in 
    if rand > count then begin 
      PM.set_confusion mon (false, 0);
      print_endline (name ^ " snapped out of confusion!");
      Unix.sleepf (Global.get_sleep_speed ());
      true 
    end 
    else if r <= (100. /. 3.) then begin
      print_endline ("It hurts itself in confusion!");
      Unix.sleepf (Global.get_sleep_speed ());
      let dam = damage 
          (PM.get_lvl mon |> Float.of_int)
          40.0
          (PM.get_attack mon false)
          (PM.get_defense mon false)
          (PM.get_speed mon)
          1.0 in 
      PM.change_hp mon (-.dam);
      PM.set_confusion mon (confused, count-1);
      false 
    end 
    else true 
  end 
  else true

(** [can_attack mon] is whether or not mon can attack (i.e. under status
    condition). *)
let can_attack mon =
  let name = PM.get_name mon in 
  Random.self_init (); 
  let r = Random.float 100. in
  let attacking = 
    match PM.get_status mon with 
    | "paralyze" -> 
      if r <= 25.0 then begin
        print_endline (name ^ " is paralyzed! It can't " ^ "move!"); 
        Unix.sleepf (Global.get_sleep_speed ());
        false end
      else true 
    | "rest" -> 
      let counter = PM.get_sleep_counter mon in 
      if counter = 0 then begin
        print_endline (name ^ " woke up!");
        Unix.sleepf (Global.get_sleep_speed ()); 
        PM.rem_status mon;
        true end
      else begin (PM.set_sleep_counter mon (counter - 1)); 
        print_endline (name ^ " is fast asleep."); 
        Unix.sleepf (Global.get_sleep_speed ());
        false end
    | "sleep" -> 
      Random.self_init ();
      let rand = Random.int 6 + 1 in 
      let counter = PM.get_sleep_counter mon in 
      if rand > counter then begin (PM.set_sleep_counter mon (counter - 1)); 
        print_endline (name ^ " is fast asleep.");
        Unix.sleepf (Global.get_sleep_speed ());
        false end 
      else begin print_endline (name ^ " woke up!"); 
        Unix.sleepf (Global.get_sleep_speed ());
        PM.rem_status mon;
        true  
      end 
    | "frozen" -> 
      if r <= 80. then begin
        print_endline (name ^ " is frozen solid."); 
        Unix.sleepf (Global.get_sleep_speed ());
        false end
      else begin 
        print_endline (name ^ " thawed out!");
        Unix.sleepf (Global.get_sleep_speed ());
        PM.rem_status mon; 
        true end
    | "flinch" -> 
      PM.rem_status mon;
      false 
    | _ -> true
  in 
  if attacking then check_confusion mon else false 

(** [handle_move atk_mon def_mon move] calculates damage of [atk_mon]
    using [move] on [def_mon], and then applies the damage with any
    effects the move causes. *)
let handle_move atk_mon def_mon move = 
  let speed = if PM.get_status atk_mon = "paralyze"
    then (PM.get_speed atk_mon)/. 2. else PM.get_speed atk_mon in
  let json = Yojson.Basic.from_file "type_matrix.json" in 
  let type_mat_and_hash = Types.type_matrix_and_hash json in
  let type_mat = fst type_mat_and_hash in 
  let hash = snd type_mat_and_hash in 
  print_endline (PM.get_name atk_mon ^ " used " ^ (Moves.name move) ^ "!");
  Unix.sleepf (Global.get_sleep_speed ());
  let modifier = (get_modifier move.el_type 1. type_mat hash (PM.get_type
                                                                def_mon)) in
  let power = move.power in 
  let is_special = move.is_special in 
  let status = Moves.get_status move in
  let move_damage = 
    if (Moves.name move) = "super fang" 
    then ((PM.get_hp def_mon) /. 2.)
    else begin 
      let dam = damage 
          (PM.get_lvl atk_mon |> Float.of_int)
          power
          (PM.get_attack atk_mon is_special)
          (PM.get_defense def_mon is_special)
          speed
          modifier in 
      if PM.get_status atk_mon = "burn" && not is_special then dam /. 2. 
      else dam 
    end 
  in 
  effect_handler atk_mon def_mon status move_damage;
  if power <> 0.0 then begin 
    if modifier = 0. then begin 
      print_endline ("It has no effect!");
      Unix.sleepf (Global.get_sleep_speed ())
    end 
    else if modifier < 1. then begin 
      print_endline ("It's not very effective...");
      Unix.sleepf (Global.get_sleep_speed ())
    end 
    else if modifier >= 2. then begin 
      print_endline ("It's super effective!");
      Unix.sleepf (Global.get_sleep_speed ());
    end 
    else ();
    let new_hp = PM.get_hp def_mon -. move_damage in 
    if (Moves.name move) = "false swipe" && new_hp <= 0. 
    then PM.set_hp def_mon 1.0
    else PM.change_hp def_mon (-.move_damage)
  end
  else ()

(** [execute_go adv st ph is_cpu] is the state update of the adventure 
    after running [state.go adv st ph'], where [ph'] is the 
    string representation of string list [ph]. 
    [is_cpu] is whether [atk_mon] is a cpu.
    Raises [IllegalMove msg] if the move is invalid. *)
let execute_attack (atk_mon : PM.t) (def_mon : PM.t) move_idx is_cpu = 
  if not (can_attack atk_mon) then ()
  else begin 
    let move = PM.get_move atk_mon (move_idx) in 
    let new_pp = move |> Moves.get_pp in
    if not is_cpu then begin
      if new_pp <= 0 then raise NoPP
      else Moves.decr_pp (PM.get_moves (atk_mon)).(move_idx); end
    else ();
    count := 0.0; 
    let acc = (Moves.get_acc move) *. (PM.get_accuracy atk_mon) in
    let hit = check_hit acc in

    if hit then handle_move atk_mon def_mon move
    else begin print_endline (PM.get_name atk_mon ^ " used " ^ (Moves.name move)
                              ^ "!");
      Unix.sleepf (Global.get_sleep_speed ());
      print_endline ("\nThe attack missed!") ;
      Unix.sleepf (Global.get_sleep_speed ());
    end
  end 

(** [b_calc ball] calculates the value that b should have in the capture formula
    depending on which Pokeball [ball] is used. *)
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
      print_endline "The pokemon broke free!";
      Unix.sleepf (Global.get_sleep_speed ())
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
      end;
      Unix.sleepf (Global.get_sleep_speed ())
    end
  end;
  false

(** [get_status_thresh status] gets the status threshold for a particular
    status. *)
let get_status_thresh status = 
  match status with
  | "sleep" | "frozen" -> 25. 
  | "burn" | "paralyze" | "poison" -> 12.
  | _ -> 0.

(** [get_n ball] gets the n value for ball for catch. *)
let get_n ball = 
  match ball with 
  | Item.PokeBall -> Random.float 256.
  | Item.GreatBall -> Random.float 201. 
  | Item.UltraBall -> Random.float 151.
  | _ -> failwith "Ball only to catch"

(** [catch_status_help mon ball rate status] is the catch helper for if [mon]
    has status [status]. *)
let catch_status_help mon ball rate status = 
  Random.self_init(); 
  let n = get_n ball in
  let status_thresh = get_status_thresh status in
  let status_catch = if n < status_thresh then true else false in
  if status_catch then true  
  else if (n -. status_thresh) > rate then false 
  else false

(** [catch def_mon ball] determines whether or not a wild pokemon is
    caught, using the gen 1 catch formula here: 
    https://bulbapedia.bulbagarden.net/wiki/Catch_rate#Capture_method_.28Generation_I.29  *)
let catch mon ball (rate : float) = 
  if ball = Item.MasterBall then begin 
    print_shake(); print_shake(); print_shake(); true
  end
  else begin 
    let status = PM.get_status mon in 
    let status_catch = 
      if status <> "" then catch_status_help mon ball rate status
      else false
    in 
    if status_catch = true then begin 
      print_shake(); print_shake(); print_shake(); true
    end 
    else begin
      let m = Random.float 256. in 
      let b = (if ball = Item.GreatBall then 8. else 12.) in
      let f = (PM.get_max_hp mon *. 255. *. 4.) /. (PM.get_hp mon *. b) in
      let f_round = f |> Float.to_int |> Int.to_float in 
      if f_round >= m then begin 
        print_shake(); print_shake(); print_shake(); true 
      end
      else shake ball f_round 
    end 
  end

(** [handle_add_mon party mon] is the helper that handles adding a new pokemon
    [mon] to the party [party]. *)
let handle_add_mon party mon = 
  if Array.length party = 6 then begin 
    print_endline ("Do you want to add " ^ PM.get_name mon 
                   ^ " to your party?[Y/N]");
    if Global.get_y_n () then begin 
      print_endline ("Swap " ^ PM.get_name mon ^ " with which party member?");
      print_endline (PM.string_of_mons party);
      let swap_n = next_mon party in
      let mon_to_box = party.(swap_n) in  
      party.(swap_n) <- mon;
      print_endline (PM.get_name mon_to_box ^ " was sent to a box and " ^
                     PM.get_name mon ^ " was added to your party!");
      raise (BattleWon (party, Some mon_to_box))
    end 
    else 
      print_endline (PM.get_name mon ^ " was sent to a box.");
    raise (BattleWon (party, Some mon))
  end 
  else raise (BattleWon (PM.add_mon party mon, None))

(** [execute_item atk_mon def_mon item bag party cpu] handles using an item from
    the player's bag [bag]. *)
let execute_item atk_mon def_mon item bag party cpu = 
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
  | Item.Potion -> PM.change_hp atk_mon 20.0;
    print_endline ((PM.get_name atk_mon) ^ "'s HP was restored by 20 points!")
  | Item.SuperPotion -> PM.change_hp atk_mon 50.0;
    print_endline ((PM.get_name atk_mon) ^ "'s HP was restored by 50 points!")
  | Item.Antidote -> if PM.get_status atk_mon = "poison" then begin
      print_endline ((PM.get_name atk_mon) ^ "'s poison was cured!");
      PM.rem_status atk_mon end 
    else print_endline "The antidote had no effect.";
  | Item.ParalyzeHeal -> if PM.get_status atk_mon = "paralyze" then begin
      print_endline ((PM.get_name atk_mon) ^ "'s paralysis was cured!");
      PM.rem_status atk_mon end
    else print_endline "The paralyze heal had no effect.";
  | Item.Awakening -> if PM.get_status atk_mon = "sleep" then begin
      print_endline ((PM.get_name atk_mon) ^ " woke up!");
      PM.rem_status atk_mon end
    else print_endline "The awakening had no effect.";
  | Item.IceHeal -> if PM.get_status atk_mon = "frozen" then begin
      print_endline ((PM.get_name atk_mon) ^ " thawed out!");
      PM.rem_status atk_mon end
    else print_endline "The ice heal had no effect.";
  | Item.BurnHeal -> if PM.get_status atk_mon = "burn" then begin
      print_endline ((PM.get_name atk_mon) ^ "'s burn was cured!");
      PM.rem_status atk_mon end
    else print_endline "The burn heal had no effect!";
  | Item.FullHeal -> if PM.get_status atk_mon != "" 
                     || fst (PM.get_confusion atk_mon) = true then begin
      print_endline ((PM.get_name atk_mon) ^ " was cured!");
      PM.rem_status atk_mon; PM.set_confusion atk_mon (false, 0) end
    else print_endline "The full heal had no effect!";
  | Item.HyperPotion -> PM.change_hp atk_mon ((PM.get_max_hp atk_mon)*. 0.6);
    print_endline ((PM.get_name atk_mon) ^ "'s HP was restored by 60%.")
  | Item.FullRestore -> PM.change_hp atk_mon (PM.get_max_hp atk_mon);
    PM.rem_status atk_mon; PM.set_confusion atk_mon (false, 0);
    print_endline ((PM.get_name atk_mon) ^ " was fully healed!")
  | Item.PokeBall | Item.GreatBall | Item.UltraBall | Item.MasterBall ->
    begin
      if cpu = "wild" then 
        let caught = catch def_mon item (255. /. 2.) in 
        if caught then begin 
          print_endline ("\n" ^ (PM.get_name def_mon) ^ " was caught!\n");
          handle_add_mon party def_mon
        end 
        else print_endline ("\n" ^ (PM.get_name def_mon) ^ " wasn't caught.\n") 
      else print_endline ("\n" ^ cpu ^ " blocked the ball!")
    end;
    Unix.sleepf (Global.get_sleep_speed ())

(** [string_of_bag] is the string representation of bag [bag]. *)
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

(** [execute_run party p_mon cpu_mon bag] is the calculator helper for if the
    player tries to run. *)
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

(** [execute_tgm party] does ??? It's a secret to everyone. *)
let execute_tgm party cpu_mon = 
  PM.restore_mons party;
  PM.set_hp cpu_mon 0.0

(** [execute_change_speed ()] changes the speed of printing in battles. *)
let execute_change_speed () = 
  Global.change_speed ();
  let speed = Global.get_sleep_speed () in 
  if speed = 1.0 then begin
    ANSITerminal.(print_string [yellow] "text speed changed to slow!\n");
    print_string "\n> " end
  else if speed = 0.5 then begin
    ANSITerminal.(print_string [yellow] "text speed changed to fast!\n");
    print_string "\n> " end
  else begin
    ANSITerminal.(print_string [yellow] "text speed changed to instant!\n");
    print_string "\n> " end

(** [execute_command party atk_mon def_mon bag cpu input] executes the correct
    command for [input] using [party], [atk_mon], 
    [def_mon], [bag], and [cpu]. *)
let rec execute_command party atk_mon def_mon bag cpu input = 
  match Btlcmd.parse input with 
  | Quit -> execute_quit ()
  | Attack(i) -> begin 
      if 1 <= i && i <= Array.length (PM.get_moves atk_mon) 
      then begin 
        execute_attack atk_mon def_mon (i-1) false 
      end
      else raise (Pokemon.UnknownMove (string_of_int i)); None 
    end
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
  | TGM -> execute_tgm party def_mon; None
  | ChangeSpeed -> execute_change_speed (); 
    execute_command party atk_mon def_mon bag cpu (read_line ())

(** [get_command party atk_mon def_mon bag cpu input] executes [input] 
    appropriately using [party] [atk_mon] [def_mon] [bag] [cpu] [input] *)
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
  | NoPP -> 
    ANSITerminal.(print_string [red] 
                    ("\nYou have no PPs left for this move!\n"));
    print_string "> ";
    get_command party atk_mon def_mon bag cpu (read_line ())

(** [execute_cpu_turn player_mon cpu_mon] runs the cpu's turn. *)
let execute_cpu_turn player_mon cpu_mon = 
  Random.self_init();
  let cpu_moves = PM.get_moves cpu_mon in 
  let used_move = (Random.int (Array.length cpu_moves)) in 
  execute_attack cpu_mon player_mon used_move true

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

(** [rand_mon mon1 mon2] chooses a random pokemon out of [mon1] and [mon3]. *)
let rand_mon mon1 mon2 = 
  let r = Random.self_init(); Random.int 1 in
  if r = 0 then 1
  else 2

(** [check_speed mon1 mon2] is the comparator function for speeds of [mon1] and
    [mon2]. *)
let check_speed mon1 mon2 = 
  match Float.compare (PM.get_speed mon1) (PM.get_speed mon2) with
  | -1 -> 2
  | 0 -> rand_mon mon1 mon2
  | _ -> 1

(** [get_status mon] is the string representation of the status of [mon]. 
    If [mon]'s status is ["rest"], it instead returns ["sleep"]. *)
let get_status mon = 
  let status = PM.get_status mon in 
  if status = "rest" then "sleep" else status 

(** [loop adv state] executes a REPL for the game. Quits on recieving 
    "quit". *)
let rec loop p_team cpu_team player_mon cpu_mon bag cpu = 
  (*let first = check_speed player_mon cpu_mon in *)
  print_string "\n";
  print_endline ("-- lv." ^ (string_of_int (PM.get_lvl cpu_mon)) 
                 ^ " " ^ (PM.get_name cpu_mon) ^ " --");
  cpu_hp_percent cpu_mon;
  print_string "\n";
  ANSITerminal.(print_string [yellow] (get_status cpu_mon));
  print_string "\n\n";
  print_endline ("-- lv." ^ (string_of_int (PM.get_lvl player_mon)) 
                 ^ " " ^ (PM.get_name player_mon) ^" --");
  player_hp_percent player_mon;
  print_string "\n";
  ANSITerminal.(print_string [yellow] (get_status player_mon));
  print_string "\n";
  print_endline "Choose your move:";
  print_endline (PM.format_moves_names player_mon);
  print_endline "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~";
  print_string "> ";
  let res = get_command p_team player_mon cpu_mon bag cpu (read_line ()) in 
  let mon = match res with 
    | Some p -> p 
    | None -> player_mon 
  in 
  if PM.fainted cpu_mon then 
    if PM.retreat cpu_team then
      raise (BattleWon (p_team, None))
    else raise (CPUDown (PM.get_name cpu_mon))
  else ();
  print_string "\n";
  begin match res with 
    | Some p -> execute_cpu_turn p cpu_mon
    | None -> execute_cpu_turn player_mon cpu_mon
  end;
  if PM.get_status mon = "burn" then begin 
    print_endline ((PM.get_name mon) ^ " is hurt by its burn!");
    PM.change_hp mon (PM.get_max_hp mon /. -16.);
    Unix.sleepf (Global.get_sleep_speed ())
  end 
  else if PM.get_status mon = "poison" then begin
    print_endline ((PM.get_name mon) ^ " is hurt by poison!");
    PM.change_hp mon (PM.get_max_hp mon /. -8.);
    Unix.sleepf (Global.get_sleep_speed ()) end
  else ();
  if PM.get_status cpu_mon = "burn" then begin 
    print_endline ((PM.get_name cpu_mon) ^ " is hurt by its burn!");
    PM.change_hp cpu_mon (PM.get_max_hp cpu_mon /. -16.);
    Unix.sleepf (Global.get_sleep_speed ())
  end 
  else if PM.get_status cpu_mon = "poison" then begin
    print_endline ((PM.get_name cpu_mon) ^ " is hurt by poison!");
    PM.change_hp cpu_mon (PM.get_max_hp cpu_mon /. -8.);
    Unix.sleepf (Global.get_sleep_speed ())
  end
  else ();
  if PM.fainted player_mon then begin
    if PM.retreat p_team then
      raise BattleLost
    else raise (PlayerDown (PM.get_name player_mon))
  end
  else ();
  match res with 
  | Some p -> loop p_team cpu_team p cpu_mon bag cpu
  | None -> loop p_team cpu_team player_mon cpu_mon bag cpu 

(** [learn_moves mon st_lvl end_lvl] is the helper for when the pokemon levels
    up and needs to learn moves. *)
let rec learn_moves mon st_lvl end_lvl =
  if st_lvl > end_lvl then () 
  else begin 
    match PM.get_new_move mon st_lvl with 
    | Some move -> begin 
        let num_moves = Array.length (PM.get_moves mon) in 
        let mon_name = PM.get_name mon in 
        let move_name = (Moves.name move) in
        if num_moves < 4 then begin 
          print_endline (mon_name ^ " learned " ^ move_name ^ "!");
          PM.add_move mon (num_moves) move
        end 
        else begin 
          print_endline (mon_name ^ " wants to learn the move " 
                         ^ move_name ^ ", but already knows four moves. " ^
                         "Should a move be deleted and replaced with " ^
                         move_name ^ "? [Y/N]");
          if Global.get_y_n () then begin
            print_endline ("What move should " ^ mon_name ^ " forget?");
            print_string (PM.format_moves_all mon);
            print_string ("5. " ^ Moves.to_string move ^ "\n");
            let n = get_move_num () in 
            if n != 4 then begin 
              print_endline ("1, 2, 3, and ... poof!\n"^ mon_name ^ " forgot " 
                             ^ Moves.name (PM.get_moves mon).(n) 
                             ^ " and learned "
                             ^ move_name ^"!");
              (PM.get_moves mon).(n)<-move
            end 
            else  print_endline (PM.get_name mon ^ " did not learn " ^ move_name
                                 ^ ".");
          end
          else begin 
            print_endline (PM.get_name mon ^ " did not learn " ^ move_name
                           ^ ".");
            learn_moves mon (st_lvl + 1) end_lvl
          end 
        end
      end 
    | None -> learn_moves mon (st_lvl + 1) end_lvl
  end

(** [give_xp cpu_lvl wild mon] is the helper that handles giving xp to a
     member of the party. *)
let give_xp cpu_lvl wild mon =  
  let st_lvl = PM.get_lvl mon + 1 in 
  PM.give_xp mon (int_of_float (2.0 *. float_of_int cpu_lvl)) wild; 
  if PM.lvl_up mon then begin
    print_endline ("\n" ^ PM.get_name mon ^ " leveled up!");
    learn_moves mon st_lvl (PM.get_lvl mon);
  end
  else ()

(** [give_xp_all cpu_lvl wild a_mons party] is the function that handles 
    giving xp to alive members [a_mons] of the party [party]. *)
let rec give_xp_all cpu_lvl wild alive_party party = 
  Array.iter (give_xp cpu_lvl wild) alive_party;
  Array.iteri (fun i mon -> party.(i) <- begin
      match PM.evolve mon with
      | (new_mon, true) -> 
        ANSITerminal.(print_string [green]
                        (("\nCongratulations! Your " ^ PM.get_name mon) 
                         ^ " evolved into " ^ (PM.get_name new_mon) ^ "!\n"));
        new_mon
      | _ -> mon
    end) party

let rec battle_handler b m cpu p_mons cpu_mons pmon cpumon cpu_money finale = 
  if cpu <> "wild" then
    ANSITerminal.(print_string [yellow] 
                    (cpu ^ " sends out " ^ (PM.get_name cpumon) ^ "!\n"))
  else ();
  try loop p_mons cpu_mons pmon cpumon b cpu 
  with 
  | BattleLost -> PM.restore_mons cpu_mons;
    PM.reset_stages pmon;
    PM.reset_stages cpumon; 
    let lost_money = (int_of_float (0.1 *. (float_of_int !m))) in
    m := !m - lost_money;
    ANSITerminal.(print_string [red] (Ascii.surp_pika ^ "\n\n"));    
    ANSITerminal.(print_string [red] 
                    ("\nYou lost the battle! Lost ₽" ^ string_of_int lost_money 
                     ^ " as well. Retreating back to town...\n"));
    PM.restore_mons p_mons;
    (p_mons, b, m, false, None) 
  | BattleWon (party, box_mon) -> 
    PM.reset_stages pmon;
    PM.reset_stages cpumon; 
    if cpu = "wild" then 
      ANSITerminal.(print_string [yellow] ("\nYou defeated the wild " 
                                           ^ PM.get_name cpumon ^ "!\n"))
    else begin
      ANSITerminal.(print_string [yellow] ("You defeated " ^ cpu ^ "!\n"));
      ANSITerminal.(print_string [yellow] 
                      ("You gained ₽" ^ (string_of_int cpu_money ) ^ "!\n"));  
      m := (!m + cpu_money)
    end;
    print_endline "The pokemon in your party gain experience!";
    give_xp_all(PM.get_lvl cpumon) (cpu = "wild") (PM.alive_pmons p_mons) party;
    if not finale then begin
      ANSITerminal.(print_string [green] 
                      "\nDo you want to keep going? [Y/N]\n");
      (party, b, m, Global.get_y_n (), box_mon)
    end
    else (party, b, m, true, box_mon)
  | PlayerDown mon -> 
    ANSITerminal.(print_string [yellow]
                    ("\n"^mon^ 
                     " fainted! Who will you send out next?\n"));
    PM.reset_stages pmon;
    let alive_mons = PM.alive_pmons p_mons in 
    battle_handler b m cpu p_mons cpu_mons (get_next_pm alive_mons) cpumon
      cpu_money finale
  | CPUDown (mon) ->
    PM.reset_stages cpumon;
    let next = (PM.alive_pmons cpu_mons).(0) in 
    give_xp_all (PM.get_lvl cpumon) false (PM.alive_pmons p_mons) p_mons;
    let alive_mons = PM.alive_pmons p_mons in 
    ANSITerminal.
      (print_string [yellow]
         ("\n" ^ mon ^ " fainted! " ^ cpu 
          ^ " is about to send out " ^ PM.get_name next 
          ^ ". Do you want to switch pokemon? [Y/N]\n"));
    if Global.get_y_n () then begin
      PM.reset_stages pmon;
      battle_handler b m cpu p_mons cpu_mons (get_next_pm alive_mons) next
        cpu_money finale 
    end 
    else battle_handler b m cpu p_mons cpu_mons pmon next cpu_money finale
  | BattleRun -> 
    PM.reset_stages pmon;
    PM.reset_stages cpumon;
    ANSITerminal.(print_string [red] ("\nGot away safely!\n"));
    ANSITerminal.(print_string [green] 
                    ("\nDo you want to keep going? [Y/N]\n"));
    (p_mons, b, m, Global.get_y_n (), None)

let main (player_team, bag, money, cpu_team, cpu, cpu_money, fin) = 
  let alive_p_team = PM.alive_pmons player_team in 
  let pmon = alive_p_team.(0) in 
  let cpumon = cpu_team.(0) in 
  if cpu = "wild" then begin
    count := 0.;
    ANSITerminal.(print_string [yellow] (
        "\nA wild " ^ (PM.get_name cpumon) ^ " appeared!\n"))
  end 
  else 
    ANSITerminal.(print_string [yellow] 
                    (cpu ^ " challenges you to a battle!\n"));
  battle_handler bag money cpu player_team cpu_team pmon cpumon cpu_money fin