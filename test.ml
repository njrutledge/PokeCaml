open OUnit2
open Adventure
open Command
open State

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
  | Lock (lst) -> "lock " ^ pp_list pp_string lst
  | Unlock (lst) -> "unlock " ^ pp_list pp_string lst
  | Take (lst) -> "grab " ^ pp_list pp_string lst
  | Drop (lst) -> "drop " ^ pp_list pp_string lst
  | Score -> "score"
  | Inventory -> "inventory"
  | Doot (lst) -> "doot " ^ pp_list pp_string lst

let result_of_string = function 
  | Legal(t) -> "Legal: " ^ current_room_id t ^ " " ^
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

(** [get_cur_room res] gets the current room of the [state.t] in [res].
    Requires: [res] is [Legal st]. *)
let get_cur_room = function
  | Legal st -> current_room_id st
  | Illegal _ -> failwith "Must be Legal"

(** [get_visited res] gets the visited rooms of the [state.t] in [res].
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

(** [make_go_test_legal_cur name exit adv st exp_room] constructs
    an OUnit test case that asserts the quality of 
    [exp_room] with that of the current room of [go exit adv st]. *)
let make_go_test_legal_cur name exit adv st exp_room = 
  name >:: (fun _ ->
      assert_equal exp_room (get_cur_room (go exit adv st))
        ~printer:(fun x -> x))

(** [make_go_test_legal_vis name exit adv st exp_visited] constructs an 
    OUnit test case that asserts the quality of [exp_visited] 
    with the visited rooms of [go exit adv st]. *)
let make_go_test_legal_vis name exit adv st exp_visited = 
  name >:: (fun _ ->
      assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
        exp_visited (get_visited (go exit adv st)))


(********************************************************************
   End helper functions.
 ********************************************************************)

let lonely_room = from_json (Yojson.Basic.from_file "lonely_room.json")
let ho_plaza = from_json (Yojson.Basic.from_file "ho_plaza.json")

let adventure_tests =
  [
    "lonely_room start room id" >:: (fun _ -> 
        assert_equal "the room" (start_room lonely_room)
          ~printer:(fun x -> x));
    "ho_plaza start room id" >:: (fun _ -> 
        assert_equal "ho plaza" (start_room ho_plaza)
          ~printer:(fun x -> x));

    "lonely_room room ids" >:: (fun _ ->
        assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
          ["the room"]
          (room_ids lonely_room));
    "ho_plaza room ids" >:: (fun _ -> 
        assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
          ["ho plaza"; "health"; "tower"; "nirvana"]
          (room_ids ho_plaza));

    "Description of lonely_room start room" >:: (fun _ ->
        assert_equal "A very lonely room." (description lonely_room "the room")
          ~printer:(fun x -> x));
    "Description of ho_plaza nirvana" >:: (fun _ ->
        assert_equal "You have reached a higher level of existence.  \
                      There are no more words."
          (description ho_plaza "nirvana")
          ~printer:(fun x -> x));
    "Unknown room test description" >:: (fun _ ->
        assert_raises (UnknownRoom "room 2")
          (fun () -> description lonely_room "room 2"));

    "Exits of lonely_room" >:: (fun _ ->
        assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
          [] (exits lonely_room "the room"));
    "ho_plaza room ids" >:: (fun _ -> 
        assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
          ["southwest"; "south west"; "Cornell Health"; "Gannett"; "chimes"; 
           "concert"; "clock tower"]
          (exits ho_plaza "ho plaza"));
    "unknown room test exits" >:: (fun _ ->
        assert_raises (UnknownRoom "room 2") 
          (fun () -> exits lonely_room "room 2"));

    "ho_plaza to ho plaza from health" >:: (fun _ ->
        assert_equal "ho plaza" (next_room ho_plaza "health" "north east")
          ~printer:(fun x -> x));
    "ho_plaza to nirvana from tower" >:: (fun _ ->
        assert_equal "nirvana" (next_room ho_plaza "tower" "higher")
          ~printer:(fun x -> x));
    "unknown room test next_room" >:: (fun _ ->
        assert_raises (UnknownRoom "Uris") 
          (fun () -> next_room ho_plaza "Uris" "bailey"));
    "unknown exit test next_room" >:: (fun _ ->
        assert_raises (UnknownExit "bailey") 
          (fun () -> next_room ho_plaza "ho plaza" "bailey"));

    "No next rooms of the room in lonely_room" >:: (fun _ ->
        assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
          [] (next_rooms lonely_room "the room"));
    "Next rooms of ho plaza in ho_plaza" >:: (fun _ ->
        assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
          ["tower"; "health"] (next_rooms ho_plaza "ho plaza"));
    "unknown room test next_rooms" >:: (fun _ ->
        assert_raises (UnknownRoom "Uris") 
          (fun () -> next_rooms ho_plaza "Uris"));
    (*"Score of start room of lonely room" >:: (fun _ -> 
        assert_equal 10 (score lonely_room "the room")
          ~printer:string_of_int);*)
  ]

let command_tests =
  [
    make_command_test "quit command" "quit" Quit;

    "Empty input" >:: (fun _ -> 
        assert_raises Empty (fun () -> parse ""));
    "Spaces only" >:: (fun _ -> 
        assert_raises Empty (fun () -> parse "      "));

    make_command_test "go command" "go north east" (Go["north";"east"]);
    make_command_test "lock command" "lock door" (Lock["door"]);
    make_command_test "unlock command" "unlock door" (Unlock["door"]);
    make_command_test "take command" "take key" (Take["key"]);
    make_command_test "drop command" "drop key" (Drop["key"]);
    make_command_test "score command" "score" Score;
    make_command_test "inventory command" "inventory" Inventory;
    make_command_test "inv command" "inv" Inventory;
    make_command_test "doot command" "doot doot" (Doot["doot"]);

    make_malformed_test "bad verb" "cheat find exit";
    make_malformed_test "bad quit" "quit now";
    make_malformed_test "bad go" "go";
    make_malformed_test "bad lock" "lock";
    make_malformed_test "bad unlock" "unlock";
    make_malformed_test "bad take" "take";
    make_malformed_test "bad drop" "drop";
    make_malformed_test "bad score" "score please";
    make_malformed_test "bad inventory" "inventory now";
    make_malformed_test "bad doot" "doot";
  ]
(* Creating initial state for lonely_room *)
let lonely_init_st = init_state lonely_room
(* Creating initial state for ho_plaza *)
let ho_init_st = init_state ho_plaza
(* Creating state of ho_plaza at tower *)
let tower_st = 
  match (go "chimes" ho_plaza ho_init_st) with
  | Legal st -> st
  | Illegal _-> failwith "ERROR"

let state_tests =
  [
    "current room at start of lonely_room" >:: (fun _ ->
        assert_equal "the room" (current_room_id lonely_init_st)
          ~printer:(fun x->x));
    "current room at start of ho_plaza" >:: (fun _ ->
        assert_equal "ho plaza" (current_room_id ho_init_st));

    make_go_test_Illegal "ho plaza to bailey" "up" ho_plaza ho_init_st;
    make_go_test_legal_cur "ho plaza to health: current room health" 
      "Cornell Health" ho_plaza ho_init_st "health";
    make_go_test_legal_vis "ho plaza to health: visited [health; ho plaza]"
      "Gannett" ho_plaza ho_init_st ["health";"ho plaza"];
    make_go_test_legal_cur "ho plaza tower -> nirvana"
      "higher" ho_plaza tower_st "nirvana"; 
    make_go_test_legal_vis "ho plaza -> tower -> nirvana"
      "higher" ho_plaza tower_st ["tower";"ho plaza";"nirvana"];

    "init score lonely_room" >:: (fun _ ->
        assert_equal 10 (calc_score lonely_room lonely_init_st)
          ~printer:string_of_int);
    "score tower_st" >:: (fun _ ->
        assert_equal 7 (calc_score ho_plaza tower_st)
          ~printer:string_of_int);
  ]
let suite =
  "test suite for A2"  >::: List.flatten [
    adventure_tests;
    command_tests;
    state_tests;
  ]

let _ = run_test_tt_main suite
