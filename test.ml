(** For testing, we mostly used playtesting. This was due to the fact that 
    our main things to be tested existed in battle.ml, which requires too much
    user input to make any unit tests. Also, we only exposed the main
    call to battle, and thus why we have no unit tests for battle. This 
    also carries over for the same thing in testing main.ml function. 
    We have a unit test for each method in adventure and in state, where further
    testing was done in play testing. We made a few tests for type effectiveness
    multiplers, but our main test for type effectiveness was printing out the
    entire matrix and checking the numbers by hand, as this method was much
    faster than doing 18^2 checks. For testing pokemon, we tested the creation 
    of a few pokemon, where each test of a single pokemon checks to make sure
    it was created correctly. We also have a unit test for checking evolution,
    and for all other tests (learning moves, leveling up, etc) we tested
    via playtesting. *)

open OUnit2
open Adventure
open Command
open State
open Pokemon 
open Moves


(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists.  That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates.  Second, they must contain the same elements,
    though not necessarily in the same order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  &&
  List.length lst2 = List.length uniq2
  &&
  uniq1 = uniq2

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt]
    to pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [h] -> acc ^ pp_elt h
      | h1::(h2::t as t') ->
        if n=100 then acc ^ "..."  (* stop printing long list *)
        else loop (n+1) (acc ^ (pp_elt h1) ^ "; ") t'
    in loop 0 "" lst
  in "[" ^ pp_elts lst ^ "]"

(** [command_of_string cmd] is a string representation of the command [cmd]. *)
let string_of_command = function
  | Quit -> "quit"
  | Bag -> "bag"
  | Party -> "party"
  | Heal -> "heal"
  | Map -> "map"
  | Shop -> "shop"
  | Save -> "save"
  | TGM -> "tgm"
  | PC -> "pc"
  | Badges -> "badges"
  | Go (lst) -> "go " ^ pp_list pp_string lst
  | GoRoute (lst) -> "go route" ^ pp_list pp_string lst
  | Buy (lst) -> "buy " ^ pp_list pp_string lst
  | Moves (lst) -> "moves " ^ pp_list pp_string lst
  | Swap (lst)-> "swap " ^ pp_list pp_string lst
  | Switch (lst) -> "switch " ^ pp_list pp_string lst
  | Info (lst) -> "info " ^ pp_list pp_string lst

let string_of_btlcmd = function
  | Btlcmd.Attack (i) -> "attack" ^ string_of_int i
  | Btlcmd.Item (lst) -> "item" ^ pp_list pp_string lst
  | Btlcmd.Bag -> "bag"
  | Btlcmd.MovesInfo -> "moves info"
  | Btlcmd.Party -> "party"
  | Btlcmd.Run -> "run"
  | Btlcmd.Switch -> "switch"
  | Btlcmd.Quit -> "quit"
  | Btlcmd.TGM -> "tgm"

let result_of_string = function 
  | Legal(t) -> "Legal: " ^ current_town_id t
  | Illegal msg -> "Illegal: " ^ msg

(** [make_malform_test name input] constructs an OUnit test named [name]
    checking if parsing the bad string [input] 
    raises a [Malformed] exception. *)
let make_malformed_test name input = 
  name >:: (fun _ -> assert_raises Malformed (fun () -> parse input))

(** [make_malform_btl_test name input] constructs an OUnit test named [name]
    checking if parsing the bad string [input] 
    raises a [Btlcmd.Malformed] exception. *)
let make_malformed_btl_test name input = 
  name >:: (fun _ -> assert_raises Btlcmd.Malformed
               (fun () -> Btlcmd.parse input))

(** [make_command_test name cmd exp_cmd] constructs an OUnit test named [name]
    checking the quality of [Battle.parse cmd] with [exp_cmd]. *)
let make_command_test name cmd exp_cmd = 
  name >:: (fun _ ->
      assert_equal exp_cmd (parse cmd) ~printer:string_of_command)

(** [make_btlcmd_test name cmd exp_cmd] constructs an OUnit test named [name]
    checking the quality of [Btlcmd.parse cmd] with [exp_cmd]. *)
let make_btlcmd_test name cmd exp_cmd = 
  name >:: (fun _ ->
      assert_equal exp_cmd (Btlcmd.parse cmd) ~printer:string_of_btlcmd)

(** [make_go_test_illegal name exit adv st] constructs an OUnit test case that
    asserts the quality of [Illegal] with [go exit adv stp]. *)
let make_go_test_Illegal name exit adv st = 
  name >:: (fun _ ->
      assert_equal (Illegal ("\nExit \"" ^ exit ^ "\" does not exist.\n"))
        (go exit adv st)
        ~printer:result_of_string)

(** [get_cur_town res] gets the current town of the [state.t] in [res].
    Requires: [res] is [Legal st]. *)
let get_cur_town = function
  | Legal st -> current_town_id st
  | Illegal _ -> failwith "Must be Legal"

let get_state = function
  | Legal st -> st
  | Illegal _ -> failwith "must be legal"

let make_command_test name cmd exp_cmd = 
  name >:: (fun _ ->
      assert_equal exp_cmd (parse cmd) ~printer:string_of_command)

(** [make_go_test_legal_cur name exit adv st exp_town] constructs
    an OUnit test case that asserts the quality of 
    [exp_town] with that of the current town of [go exit adv st]. *)
let make_go_test_legal_cur name exit adv st exp_town = 
  name >:: (fun _ ->
      assert_equal exp_town (get_cur_town (go exit adv st))
        ~printer:(fun x -> x))

(** [make_type_test name mat atk def hash exp_mult] constructs an 
    OUnit test case that asserts the quality of [exp_mult] with 
    [mat.(hash atk).(hash def)]. *)
let make_type_test name mat atk def hash exp_mult = 
  name >:: (fun _ ->
      assert_equal exp_mult (mat.(hash atk).(hash def))
        ~printer:string_of_float)

let make_mon_creation_test name mon pname atk spa def spd speed hp moves = 
  name >:: (fun _ ->
      assert_equal pname (PM.get_name mon) ~printer:(fun x -> x);
      assert_equal atk (PM.get_attack mon false) ~printer:string_of_float;
      assert_equal spa (PM.get_attack mon true) ~printer:string_of_float;
      assert_equal def (PM.get_defense mon false) ~printer:string_of_float;
      assert_equal spd (PM.get_defense mon true) ~printer:string_of_float;
      assert_equal speed (PM.get_speed mon) ~printer:string_of_float;
      assert_equal hp (PM.get_hp mon) ~printer:string_of_float;
      assert_equal hp (PM.get_max_hp mon) ~printer:string_of_float;
      assert_equal moves (PM.get_moves mon))
(********************************************************************
   End helper functions.
 ********************************************************************)

let mat_fun = "type_matrix.json"
              |> Yojson.Basic.from_file
              |> Types.type_matrix_and_hash
let type_mat = fst mat_fun
let hash = snd mat_fun
let type_tests = 
  [ 
    make_type_test "fire vs fire" type_mat "fire" "fire" hash 0.5;
    make_type_test "fire vs grass" type_mat "fire" "grass" hash 2.0;
    make_type_test "fire vs water" type_mat "fire" "water" hash 0.5;
    make_type_test "grass vs fire" type_mat "grass" "fire" hash 0.5;
    make_type_test "grass vs grass" type_mat "grass" "grass" hash 0.5;
    make_type_test "grass vs water" type_mat "grass" "water" hash 2.0;
    make_type_test "water vs fire" type_mat "water" "fire" hash 2.0;
    make_type_test "water vs grass" type_mat "water" "grass" hash 0.5;
    make_type_test "water vs water" type_mat "water" "water" hash 0.5;
    make_type_test "normal vs normal" type_mat "normal" "normal" hash 1.0;
    make_type_test "electric vs ground" type_mat "electric" "ground" hash 0.0;
    make_type_test "ground vs fire" type_mat "ground" "fire" hash 2.0;
    "unknown type plasma" >:: (fun _ -> 
        assert_raises (Types.UnknownType "plasma") 
          (fun () -> type_mat.(hash "plasma"). (hash "fire")));
  ]

module PM = Pokemon
let thun_lst = [Moves.create_move "thundershock"]
let w_gun_lst = [Moves.create_move "water gun"]
let mon1 = PM.create_pokemon "Pikachu" 1 thun_lst
let squirt1 = PM.create_pokemon "Squirtle" 1 w_gun_lst
let _ = PM.set_xp squirt1 4096.0; PM.lvl_up squirt1
let (squirt2,_) = PM.evolve squirt1
let warto16 = PM.create_pokemon "Wartortle" 16 w_gun_lst
let move_arr = Array.of_list thun_lst
let pokemon_tests = 
  [
    make_mon_creation_test "Pikachu test" 
      mon1 "Pikachu" 55.0 50.0 40.0 40.0 90.0 40.0 move_arr;
    "evolve test">:: (fun _ ->
        assert_equal "Wartortle" (PM.get_name squirt2)~printer:(fun x -> x));
  ]
let json = Yojson.Basic.from_file "adv.json"
let adv = Adventure.from_json json
let state = State.init_state adv

let adventure_tests = 
  [
    "start town" >:: (fun _ -> 
        assert_equal "My house" (Adventure.start_town adv));
    "desc check" >:: (fun _ -> 
        assert_equal "\"Welcome to Java Town's Pokecenter!\" - Nurse Joy"
          (Adventure.description adv "PokeCenter JTown"));
    "exits check" >:: (fun _ ->
        assert_equal ["outside"] (Adventure.exits adv "My house"));
    "next town" >:: (fun _ ->
        assert_equal "Gates Town" 
          (Adventure.next_town adv "My house" "outside"));
    "next town exn exit" >:: (fun _ ->
        assert_raises (UnknownExit "inside") 
          (fun () -> Adventure.next_town adv "My house" "inside"));
    "next town exn town" >:: (fun _ ->
        assert_raises (UnknownTown "mah houz")
          (fun () -> Adventure.next_town adv "mah houz" "outside"));

  ]

let command_tests = 
  [
    make_command_test "quit command" "quit" Quit;
    make_command_test "inventory command" "bag" Bag;
    make_command_test "party command" "party" Party;
    make_command_test "heal command" "heal" Heal;
    make_command_test "map command" "map" Map;
    make_command_test "badges command" "badges" Badges;
    make_command_test "shop command" "shop" Shop;
    make_command_test "save command" "map" Map;
    make_command_test "tgm command" "tgm" TGM;
    make_command_test "pc command" "pc" PC;
    make_command_test "go command" "go pokecenter" (Go["pokecenter"]);
    make_command_test "go route command" "go route 1110" 
      (GoRoute["route"; "1110"]);
    make_command_test "buy command" "buy potion 3" (Buy["potion"; "3"]);
    make_command_test "moves command" "moves pikachu" (Moves["pikachu"]);
    make_command_test "swap command" "go pokecenter" (Go["pokecenter"]);
    make_command_test "switch command" "switch 1 2" (Switch["1";"2"]);
    make_command_test "info command" "info 18" (Info["18"]);

    make_malformed_test "bad verb" "cheat win game";
    make_malformed_test "bad quit" "quit now";
    make_malformed_test "bad bag" "bag good sir";
    make_malformed_test "bad party" "party rockers in the house tonight";
    make_malformed_test "bad heal" "heal all my mons";
    make_malformed_test "bad map" "map quest";
    make_malformed_test "bad badges" "badges galore";
    make_malformed_test "bad shop" "shop WallMart";
    make_malformed_test "bad TGM" "tgm 3spooky5me";
    make_malformed_test "bad pc" "pc master sword";
    make_malformed_test "bad go" "go";
    make_malformed_test "bad buy" "buy";
    make_malformed_test "bad moves" "moves";
    make_malformed_test "bad swap" "swap";
    make_malformed_test "bad switch" "switch";
    make_malformed_test "bad info" "info";

    make_btlcmd_test "attack command" "attack 1" (Btlcmd.Attack 1);
    make_btlcmd_test "item command" "item great ball"
      (Btlcmd.Item["great"; "ball"]);
    make_btlcmd_test "bag command" "bag" (Btlcmd.Bag);
    make_btlcmd_test "moves info command" "moves" (Btlcmd.MovesInfo);
    make_btlcmd_test "party command" "party" (Btlcmd.Party);
    make_btlcmd_test "run command" "run" (Btlcmd.Run); 
    make_btlcmd_test "switch command" "switch" (Btlcmd.Switch);
    make_btlcmd_test "quit command" "quit" (Btlcmd.Quit);
    make_btlcmd_test "tgm command" "tgm" (Btlcmd.TGM);

    make_malformed_btl_test "bad attack" "attack";
    make_malformed_btl_test "bad item" "item";
    make_malformed_btl_test "bad moves info" "moves please";
    make_malformed_btl_test "bad party" "party party party party party party";
    make_malformed_btl_test "bad run" "run away";
    make_malformed_btl_test "bad switch" "switch a roo";
    make_malformed_btl_test "bad info" "info about everything in the universe";
  ]

let state_tests = 
  [
    "init location" >:: (fun _ -> 
        assert_equal "My house" (State.current_town_id state));
    "execute go" >:: (fun _ ->
        assert_equal "Gates Town" 
          (match State.go "outside" adv state with 
           | Legal st-> State.current_town_id st
           | _ -> failwith ""));
    "get money" >:: (fun _ ->
        assert_equal 1000 (!(State.get_money state)));
    "get party" >:: (fun _ ->
        assert_equal [||] (State.get_party state));
    "get bag" >:: (fun _ ->
        assert_equal [(Item.item_of_string "potion", ref 5); 
                      (Item.item_of_string "pokeball", ref 10); 
                      (Item.item_of_string "great ball", ref 3);
                      (Item.item_of_string "antidote", ref 3);]
          (State.bag state));
  ]

let suite =
  "test suite for pokemon 3110"  >::: List.flatten [
    adventure_tests;
    command_tests;
    state_tests;
    type_tests;
    pokemon_tests;
  ]

let _ = run_test_tt_main suite
(*let _ = Types.test_print_type_mat type_mat hash*)