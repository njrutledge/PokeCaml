open OUnit2
open Adventure
open Command
open State
open Pokemon 

(********************************************************************
   Here are some helper functions for your testing of set-like lists. 
 ********************************************************************)

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

(* These tests demonstrate how to use [cmp_set_like_lists] and 
   [pp_list] to get helpful output from OUnit. *)
let cmp_demo = 
  [
    "order is irrelevant" >:: (fun _ -> 
        assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
          ["foo"; "bar"] ["bar"; "foo"]);
    (* Uncomment this test to see what happens when a test case fails.
       "duplicates not allowed" >:: (fun _ -> 
        assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
          ["foo"; "foo"] ["foo"]);
    *)
  ]
(** [command_of_string cmd] is a string representation of the command [cmd]. *)
let string_of_command = function
  | Quit -> "quit"
  | Go (lst) -> "go " ^ pp_list pp_string lst
  | Take (lst) -> "grab " ^ pp_list pp_string lst
  | Bag -> "bag"

let result_of_string = function 
  | Legal(t) -> "Legal: " ^ current_town_id t ^ " " ^
                pp_list pp_string (visited t)
  | Illegal msg -> "Illegal: " ^ msg

(** [make_malform_test name input] constructs an OUnit test named [name]
    checking if parsing the bad string [input] 
    raises a [Malformed] exception. *)
let make_malformed_test name input = 
  name >:: (fun _ -> assert_raises Malformed (fun () -> parse input))

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

(** [get_visited res] gets the visited towns of the [state.t] in [res].
    Requires: [res] is [Legal st]. *)
let get_visited = function
  | Legal st -> visited st
  | Illegal _ -> failwith "must be legal"

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

(** [make_go_test_legal_vis name exit adv st exp_visited] constructs an 
    OUnit test case that asserts the quality of [exp_visited] 
    with the visited towns of [go exit adv st]. *)
let make_go_test_legal_vis name exit adv st exp_visited = 
  name >:: (fun _ ->
      assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
        exp_visited (get_visited (go exit adv st)))

let make_type_test name mat atk def hash exp_mult = 
  name >:: (fun _ ->
      assert_equal exp_mult (mat.(hash atk).(hash def))
        ~printer:string_of_float)

let set_mon_name mon =
  Stats.mon := mon

module Mon2 = Pokemon (Stats)
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

let _ = 
  Stats.mon := "Mon1" 
module Mon1 = Pokemon (Stats)
let pokemon_tests = 
  [
    "hp = 4" >:: (fun _ -> 
        assert_equal 4 (Mon1.get_hp Mon1.stats)~printer:string_of_int) 
  ]

let suite =
  "test suite for A2"  >::: List.flatten [
    (*adventure_tests;
      command_tests;
      state_tests;*)
    type_tests;
    pokemon_tests;
  ]

let _ = run_test_tt_main suite