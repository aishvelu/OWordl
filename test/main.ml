open OUnit2
open Game
open Owordl
open Command
open State

(** Testing plan: Our test suite includes automatic testing by Ounit for
    the majority of our functions in command.ml, state.ml, and
    owordl.ml—more specifically, the functions exposed in the
    corresponding .mli files. This was because the majority of our
    functions could easily be compared with string and integer expected
    outputs. However, we did perform manual tests on specific functions
    in state.ml. Specifically, we manually tested the functions
    insert_row and create_tile_list which both outputed graphics and
    were best verified by looking at the generated output on the
    terminal when playing the game. In terms of how test cases were
    developed, we exclusively perfomed glass-box testing, as we first
    wrote the code for any one function and then created tests based on
    the implementation. With this method of testing, we attempted to
    achieve full path coverage by ensuring every line of code within a
    given function was tested at least once. Hence, we believe our test
    suite displays correctness of the system.*)

(********************************************************************
   Here are some helper functions for your testing of set-like lists.
 ********************************************************************)

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists. That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates. Second, they must contain the same elements,
    though not necessarily in the same order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  && List.length lst2 = List.length uniq2
  && uniq1 = uniq2

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

(*------------------OWORDL TEST HELPERS---------------------*)
let length_test
    (name : string)
    (game : Owordl.t)
    (expected_output : int) : test =
  name >:: fun _ -> assert_equal expected_output (length game)

(*------------------COMMAND TEST HELPERS--------------------*)
let parse_test
    (name : string)
    (input : string)
    (game : Owordl.t)
    dict
    (expected_output : command) : test =
  name >:: fun _ -> assert_equal expected_output (parse input game dict)

(*raise empty*)
let parse_empty_test
    (name : string)
    (input : string)
    (game : Owordl.t)
    (dict : Owordl.t) : test =
  name >:: fun _ ->
  assert_raises Empty (fun () -> parse input game dict)

(*raise malformed*)
let parse_malformed_test
    (name : string)
    (input : string)
    (game : Owordl.t)
    (dict : Owordl.t) : test =
  name >:: fun _ ->
  assert_raises Malformed (fun () -> parse input game dict)

(*raise invalid*)
let parse_invalid_test
    (name : string)
    (input : string)
    (game : Owordl.t)
    (dict : Owordl.t) : test =
  name >:: fun _ ->
  assert_raises Invalid (fun () -> parse input game dict)

let parse_length_test
    (name : string)
    (input : string)
    (game : Owordl.t)
    (dict : Owordl.t) : test =
  name >:: fun _ ->
  assert_raises Length (fun () -> parse input game dict)

(*----------------------STATE TEST HELPERS-----------------------*)

(*helper function to concatentate a list of integer and character tuples
  into a string*)
let print_list lst =
  List.fold_left ( ^ ) ""
    (List.map
       (fun (x1, x2) ->
         string_of_int x1 ^ " " ^ String.make 1 x2 ^ ", ")
       lst)

let explode_test
    (name : string)
    (word : string)
    (expected_output : char list) : test =
  name >:: fun _ -> assert_equal expected_output (explode word)

let create_map_test
    (name : string)
    (input : char list)
    (expected_output : (int * char) list) : test =
  name >:: fun _ ->
  assert_equal expected_output (create_map input) ~printer:print_list

let match_maps_test
    (name : string)
    (input_guess : (int * char) list)
    (input_word : (int * char) list)
    (expected_output : string list) : test =
  name >:: fun _ ->
  assert_equal expected_output (match_maps input_guess input_word)

let get_greens_test
    (name : string)
    (input_guess : (int * char) list)
    (input_word : (int * char) list)
    (expected_output : char list) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (get_greens (match_maps input_guess input_word))

let get_yellows_test
    (name : string)
    (input_guess : (int * char) list)
    (input_word : (int * char) list)
    (expected_output : char list) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (get_yellows (match_maps input_guess input_word))

let get_reds_test
    (name : string)
    (input_guess : (int * char) list)
    (input_word : (int * char) list)
    (expected_output : char list) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (get_reds (match_maps input_guess input_word))

let exists_lst_test
    (name : string)
    (acc : char list)
    (mystery_lst : char list)
    (guess_lst : char list)
    (expected_output : char list) : test =
  name >:: fun _ ->
  assert_equal expected_output (exists_lst acc mystery_lst guess_lst)

let not_exists_lst_test
    (name : string)
    (acc : char list)
    (mystery_lst : char list)
    (guess_lst : char list)
    (expected_output : char list) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (not_exists_lst acc mystery_lst guess_lst)

let update_keyboard_test
    (name : string)
    (curr_keyboard : (char * color) list)
    (reds : char list)
    (yellows : char list)
    (greens : char list)
    (expected_output : (char * color) list) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (update_keyboard curr_keyboard reds yellows greens)

let play_init_helper (guess : char list) (game : Owordl.t) =
  match play guess game (init_state game) with
  | Illegal st_illegal -> st_illegal
  | Legal st_legal -> st_legal

(* an updated state of type state.t that results from a play move with
   2nd+ guess in game*)
let play_helper (guess : char list) (game : Owordl.t) (state : State.t)
    =
  match play guess game state with
  | Illegal st_illegal -> st_illegal
  | Legal st_legal -> st_legal

let play_legal_guess_num_test
    (name : string)
    (guess : char list)
    (game : Owordl.t)
    (state : State.t)
    (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (get_guess_num (play_helper guess game state))

let play_legal_colors_test
    (name : string)
    (guess : char list)
    (game : Owordl.t)
    (state : State.t)
    (expected_output : color array) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (get_colors (play_helper guess game state))

(* asserts [expected_output] is the correct mystery word encoded in the
   updated state' resulting from a Legal play move. *)
let play_legal_mystery_test
    (name : string)
    (guess : char list)
    (game : Owordl.t)
    (state : State.t)
    (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (get_mystery (play_helper guess game state))

(*asserts [expected_output] is the correct keyboard encoded in the
  updated state' resulting from a Legal play move. *)
let play_legal_keyboard_test
    (name : string)
    (guess : char list)
    (game : Owordl.t)
    (state : State.t)
    (expected_output : (char * color) list) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (get_keyboard (play_helper guess game state))

(* asserts [expected_output] is the correct number of guesses encoded by
   an Illegal play move. *)
let play_illegal_guess_num_test
    (name : string)
    (guess : char list)
    (game : Owordl.t)
    (state : State.t)
    (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (get_guess_num (play_helper guess game state))

(********************************************************************
   End helper functions.
 ********************************************************************)

(**Character lists and maps to be used unit tests throughout the test
   suite.*)
let able_exploded = explode "able"

let camel_exploded = explode "camel"
let water_exploded = explode "water"
let which_exploded = explode "which"
let house_exploded = explode "house"
let paper_exploded = explode "paper"
let better_exploded = explode "better"
let great_exploded = explode "great"
let might_exploded = explode "might"
let chops_exploded = explode "chops"
let pears_exploded = explode "pears"
let first_exploded = explode "first"
let while_map = create_map (explode "while")
let white_map = create_map (explode "white")
let spear_map = create_map (explode "spear")
let pears_map = create_map (explode "pears")
let learn_map = create_map (explode "learn")
let chops_map = create_map (explode "chops")
let great_map = create_map (explode "great")
let might_map = create_map (explode "might")
let first_map = create_map (explode "first")

(**Add strings containing JSON here, and use them as the basis for unit
   tests.*)
let json_level0 = Yojson.Basic.from_file "data/level0.json"

let json_level1 = Yojson.Basic.from_file "data/level1.json"
let json_level2 = Yojson.Basic.from_file "data/level2.json"
let json_dictionary = Yojson.Basic.from_file "data/dictionary.json"
let level0 = from_json json_level0
let level1 = from_json json_level1
let level2 = from_json json_level2
let dictionary = from_json json_dictionary

(*State inputs to the play legal tests and play illegal tests in
  level0*)
let init_state_to_test_l0 = play_init_helper able_exploded level0
let mystery_word_from_test_l0 = get_mystery init_state_to_test_l0
let colors_array_from_test_l0 = get_colors init_state_to_test_l0
let keyboard_from_test_l0 = get_keyboard init_state_to_test_l0

let guess_two_state_to_test_l0 =
  play_helper able_exploded level0 init_state_to_test_l0

let guess_three_state_to_test_l0 =
  play_helper able_exploded level0 guess_two_state_to_test_l0

let guess_four_state_to_test_l0 =
  play_helper able_exploded level0 guess_three_state_to_test_l0

let guess_five_state_to_test_l0 =
  play_helper able_exploded level0 guess_four_state_to_test_l0

(*State inputs to the play legal tests and play illegal tests in
  level1*)
let init_state_to_test_l1 = play_init_helper camel_exploded level1
let mystery_word_from_test_l1 = get_mystery init_state_to_test_l1
let colors_array_from_test_l1 = get_colors init_state_to_test_l1
let keyboard_from_test_l1 = get_keyboard init_state_to_test_l1

let guess_two_state_to_test_l1 =
  play_helper camel_exploded level1 init_state_to_test_l1

let guess_three_state_to_test_l1 =
  play_helper camel_exploded level1 guess_two_state_to_test_l1

let guess_four_state_to_test_l1 =
  play_helper camel_exploded level1 guess_three_state_to_test_l1

let guess_five_state_to_test_l1 =
  play_helper camel_exploded level1 guess_four_state_to_test_l1

(*State inputs to the play legal tests and play illegal tests in
  level2*)
let init_state_to_test_l2 = play_init_helper better_exploded level2
let mystery_word_from_test_l2 = get_mystery init_state_to_test_l2
let colors_array_from_test_l2 = get_colors init_state_to_test_l2
let keyboard_from_test_l2 = get_keyboard init_state_to_test_l2

let guess_two_state_to_test_l2 =
  play_helper better_exploded level2 init_state_to_test_l2

let guess_three_state_to_test_l2 =
  play_helper better_exploded level2 guess_two_state_to_test_l2

let guess_four_state_to_test_l2 =
  play_helper better_exploded level2 guess_three_state_to_test_l2

let guess_five_state_to_test_l2 =
  play_helper better_exploded level2 guess_four_state_to_test_l2

(* You should not be testing any helper functions here. Test only the
   functions exposed in the [.mli] files. Do not expose your helper
   functions. See the handout for an explanation. *)

(* ---------------------BEGIN CALLING UNIT
   TESTS------------------------*)

let owordl_tests =
  [
    length_test "test length level 0" level0 4;
    length_test "test length level 1" level1 5;
    length_test "test length level 2" level2 6;
  ]

let command_tests =
  [
    parse_test "test quit" "quit" level1 dictionary Quit;
    parse_malformed_test "test for malformed exception in level0" "play"
      level0 dictionary;
    parse_malformed_test "test for malformed exception in level0"
      "play hello world" level0 dictionary;
    parse_malformed_test "test for malformed exception in level0" "able"
      level0 dictionary;
    parse_malformed_test "test for malformed exception in level0"
      "quit able" level0 dictionary;
    parse_malformed_test "test for malformed exception in level1" "play"
      level1 dictionary;
    parse_malformed_test "test for malformed exception in level1"
      "play hello world" level1 dictionary;
    parse_malformed_test "test for malformed exception in level1"
      "hello" level1 dictionary;
    parse_malformed_test "test for malformed exception in level1"
      "quit hello" level1 dictionary;
    parse_malformed_test "test for malformed exception in level2" "play"
      level2 dictionary;
    parse_malformed_test "test for malformed exception in level2"
      "play hello world" level2 dictionary;
    parse_malformed_test "test for malformed exception in level2"
      "better" level2 dictionary;
    parse_malformed_test "test for malformed exception in level2"
      "quit better" level2 dictionary;
    parse_empty_test "test for empty exception in level0" "" level0
      dictionary;
    parse_empty_test "test for empty exception in level1" "" level1
      dictionary;
    parse_empty_test "test for empty exception in level2" "" level2
      dictionary;
    parse_length_test "test a word not of 4 letters" "play camel" level0
      dictionary;
    parse_length_test "test a word not of 5 letters" "play camels"
      level1 dictionary;
    parse_length_test "test a word not of 6 letters" "play dual" level2
      dictionary;
    parse_length_test "test a word not of 6 letters" "play glasses"
      level2 dictionary;
    parse_test "test a word that is 4 letters" "play able" level0
      dictionary
      (Play [ 'a'; 'b'; 'l'; 'e' ]);
    parse_test "test a word that is 5 letters" "play camel" level1
      dictionary
      (Play [ 'c'; 'a'; 'm'; 'e'; 'l' ]);
    parse_test "test a word that is 6 letters" "play camera" level2
      dictionary
      (Play [ 'c'; 'a'; 'm'; 'e'; 'r'; 'a' ]);
    parse_invalid_test "test yhjk is not in the english dictionary"
      "play yhjk" level0 dictionary;
    parse_invalid_test "test thnk is not in the english dictionary"
      "play thnk" level0 dictionary;
    parse_invalid_test "test 1234 is not in the english dictionary"
      "play 1234" level0 dictionary;
    parse_invalid_test "test ?. is not in the english dictionary"
      "play ?." level0 dictionary;
    parse_invalid_test "test avava is not in the english dictionary"
      "play avava" level1 dictionary;
    parse_invalid_test "test 12345 is not in the english dictionary"
      "play 12345" level1 dictionary;
    parse_invalid_test "test aaaaa is not in the english dictionary"
      "play aaaaa" level1 dictionary;
    parse_invalid_test "test () is not in the english dictionary"
      "play ()" level1 dictionary;
    parse_invalid_test "test helloo is not in the english dictionary"
      "play helloo" level2 dictionary;
    parse_invalid_test "test asdfgh is not in the english dictionary"
      "play asdfgh" level2 dictionary;
    parse_invalid_test "test 123456 is not in the english dictionary"
      "play 123456" level2 dictionary;
    parse_invalid_test "test 123456 is not in the english dictionary"
      "play hello??" level2 dictionary;
  ]

let state_tests =
  [
    explode_test "test explode on empty" "" [];
    explode_test "test explode on guess" " " [ ' ' ];
    explode_test "test explode on hey" "hey" [ 'h'; 'e'; 'y' ];
    explode_test "test explode on guess" "guess"
      [ 'g'; 'u'; 'e'; 's'; 's' ];
    explode_test "test explode on camel" "camel"
      [ 'c'; 'a'; 'm'; 'e'; 'l' ];
    explode_test "test explode on water" "water"
      [ 'w'; 'a'; 't'; 'e'; 'r' ];
    explode_test "test explode on which" "which"
      [ 'w'; 'h'; 'i'; 'c'; 'h' ];
    explode_test "test explode on house" "house"
      [ 'h'; 'o'; 'u'; 's'; 'e' ];
    explode_test "test explode on paper" "paper"
      [ 'p'; 'a'; 'p'; 'e'; 'r' ];
    explode_test "test explode on better" "better"
      [ 'b'; 'e'; 't'; 't'; 'e'; 'r' ];
    explode_test "test explode on great" "great"
      [ 'g'; 'r'; 'e'; 'a'; 't' ];
    explode_test "test explode on might" "might"
      [ 'm'; 'i'; 'g'; 'h'; 't' ];
    explode_test "test explode on chops" "chops"
      [ 'c'; 'h'; 'o'; 'p'; 's' ];
    explode_test "test explode on pears" "pears"
      [ 'p'; 'e'; 'a'; 'r'; 's' ];
    create_map_test "test creating a map for camel" camel_exploded
      [ (1, 'c'); (2, 'a'); (3, 'm'); (4, 'e'); (5, 'l') ];
    create_map_test "test creating a map for water" water_exploded
      [ (1, 'w'); (2, 'a'); (3, 't'); (4, 'e'); (5, 'r') ];
    create_map_test "test creating a map for which" which_exploded
      [ (1, 'w'); (2, 'h'); (3, 'i'); (4, 'c'); (5, 'h') ];
    create_map_test "test creating a map for house" house_exploded
      [ (1, 'h'); (2, 'o'); (3, 'u'); (4, 's'); (5, 'e') ];
    create_map_test "test creating a map for paper" paper_exploded
      [ (1, 'p'); (2, 'a'); (3, 'p'); (4, 'e'); (5, 'r') ];
    create_map_test "test creating a map for better" better_exploded
      [ (1, 'b'); (2, 'e'); (3, 't'); (4, 't'); (5, 'e'); (6, 'r') ];
    create_map_test "test creating a map for great" great_exploded
      [ (1, 'g'); (2, 'r'); (3, 'e'); (4, 'a'); (5, 't') ];
    create_map_test "test creating a map for might" might_exploded
      [ (1, 'm'); (2, 'i'); (3, 'g'); (4, 'h'); (5, 't') ];
    create_map_test "test creating a map for chops" chops_exploded
      [ (1, 'c'); (2, 'h'); (3, 'o'); (4, 'p'); (5, 's') ];
    create_map_test "test creating a map for pears" pears_exploded
      [ (1, 'p'); (2, 'e'); (3, 'a'); (4, 'r'); (5, 's') ];
    create_map_test "test creating a map for first" first_exploded
      [ (1, 'f'); (2, 'i'); (3, 'r'); (4, 's'); (5, 't') ];
    match_maps_test "test matching maps for while and white" while_map
      white_map
      [
        "w : correct";
        "h : correct";
        "i : correct";
        "l : incorrect";
        "e : correct";
      ];
    match_maps_test "test matching maps for spear and pears" spear_map
      pears_map
      [
        "s : wrong position";
        "p : wrong position";
        "e : wrong position";
        "a : wrong position";
        "r : wrong position";
      ];
    match_maps_test "test matching maps for learn and chops" learn_map
      chops_map
      [
        "l : incorrect";
        "e : incorrect";
        "a : incorrect";
        "r : incorrect";
        "n : incorrect";
      ];
    match_maps_test "test matching maps for great and might" great_map
      might_map
      [
        "g : wrong position";
        "r : incorrect";
        "e : incorrect";
        "a : incorrect";
        "t : correct";
      ];
    match_maps_test "test matching maps for first and first" first_map
      first_map
      [
        "f : correct";
        "i : correct";
        "r : correct";
        "s : correct";
        "t : correct";
      ];
    match_maps_test "test matching maps for great and first" great_map
      first_map
      [
        "g : incorrect";
        "r : wrong position";
        "e : incorrect";
        "a : incorrect";
        "t : correct";
      ];
    match_maps_test "test matching maps for while and first" while_map
      first_map
      [
        "w : incorrect";
        "h : incorrect";
        "i : wrong position";
        "l : incorrect";
        "e : incorrect";
      ];
    match_maps_test "test matching maps for white and first" white_map
      first_map
      [
        "w : incorrect";
        "h : incorrect";
        "i : wrong position";
        "t : wrong position";
        "e : incorrect";
      ];
    match_maps_test "test matching maps for spear and first" spear_map
      first_map
      [
        "s : wrong position";
        "p : incorrect";
        "e : incorrect";
        "a : incorrect";
        "r : wrong position";
      ];
    match_maps_test "test matching maps for pears and first" pears_map
      first_map
      [
        "p : incorrect";
        "e : incorrect";
        "a : incorrect";
        "r : wrong position";
        "s : wrong position";
      ];
    match_maps_test "test matching maps for learn and first" learn_map
      first_map
      [
        "l : incorrect";
        "e : incorrect";
        "a : incorrect";
        "r : wrong position";
        "n : incorrect";
      ];
    get_greens_test "test greens for while and white" while_map
      white_map [ 'w'; 'h'; 'i'; 'e' ];
    get_greens_test "test greens for spear and pears" spear_map
      pears_map [];
    get_greens_test "test greens for learn and chop" learn_map chops_map
      [];
    get_greens_test "test greens for great and might" great_map
      might_map [ 't' ];
    get_greens_test "test greens for first and first" first_map
      first_map
      [ 'f'; 'i'; 'r'; 's'; 't' ];
    get_yellows_test "test yellows for while and white" while_map
      white_map [];
    get_yellows_test "test yellows for spear and pears" spear_map
      pears_map
      [ 's'; 'p'; 'e'; 'a'; 'r' ];
    get_yellows_test "test yellows for learn and chop" learn_map
      chops_map [];
    get_yellows_test "test yellows for great and might" great_map
      might_map [ 'g' ];
    get_yellows_test "test yellows for first and first" first_map
      first_map [];
    get_reds_test "test reds for while and white" while_map white_map
      [ 'l' ];
    get_reds_test "test reds for spear and pears" spear_map pears_map [];
    get_reds_test "test reds for learn and chop" learn_map chops_map
      [ 'l'; 'e'; 'a'; 'r'; 'n' ];
    get_reds_test "test reds for great and might" great_map might_map
      [ 'r'; 'e'; 'a' ];
    get_reds_test "test reds for first and first" first_map first_map [];
    update_keyboard_test
      "test updating 0 characters on initial keyboard"
      [
        ('q', Blue);
        ('w', Blue);
        ('e', Blue);
        ('r', Blue);
        ('t', Blue);
        ('y', Blue);
        ('u', Blue);
        ('i', Blue);
        ('o', Blue);
        ('p', Blue);
        ('a', Blue);
        ('s', Blue);
        ('d', Blue);
        ('f', Blue);
        ('g', Blue);
        ('h', Blue);
        ('j', Blue);
        ('k', Blue);
        ('l', Blue);
        ('z', Blue);
        ('x', Blue);
        ('c', Blue);
        ('v', Blue);
        ('b', Blue);
        ('n', Blue);
        ('m', Blue);
      ]
      [] [] []
      [
        ('q', Blue);
        ('w', Blue);
        ('e', Blue);
        ('r', Blue);
        ('t', Blue);
        ('y', Blue);
        ('u', Blue);
        ('i', Blue);
        ('o', Blue);
        ('p', Blue);
        ('a', Blue);
        ('s', Blue);
        ('d', Blue);
        ('f', Blue);
        ('g', Blue);
        ('h', Blue);
        ('j', Blue);
        ('k', Blue);
        ('l', Blue);
        ('z', Blue);
        ('x', Blue);
        ('c', Blue);
        ('v', Blue);
        ('b', Blue);
        ('n', Blue);
        ('m', Blue);
      ];
    update_keyboard_test
      "test updating 0 characters on a previously updated keyboard"
      [
        ('q', Red);
        ('w', Green);
        ('e', Red);
        ('r', Blue);
        ('t', Green);
        ('y', Blue);
        ('u', Green);
        ('i', Blue);
        ('o', Blue);
        ('p', Green);
        ('a', Green);
        ('s', Green);
        ('d', Blue);
        ('f', Red);
        ('g', Blue);
        ('h', Blue);
        ('j', Blue);
        ('k', Blue);
        ('l', Blue);
        ('z', Blue);
        ('x', Blue);
        ('c', Blue);
        ('v', Blue);
        ('b', Blue);
        ('n', Blue);
        ('m', Blue);
      ]
      [] [] []
      [
        ('q', Red);
        ('w', Green);
        ('e', Red);
        ('r', Blue);
        ('t', Green);
        ('y', Blue);
        ('u', Green);
        ('i', Blue);
        ('o', Blue);
        ('p', Green);
        ('a', Green);
        ('s', Green);
        ('d', Blue);
        ('f', Red);
        ('g', Blue);
        ('h', Blue);
        ('j', Blue);
        ('k', Blue);
        ('l', Blue);
        ('z', Blue);
        ('x', Blue);
        ('c', Blue);
        ('v', Blue);
        ('b', Blue);
        ('n', Blue);
        ('m', Blue);
      ];
    update_keyboard_test
      "test updating 1 red character on initial keyboard"
      [
        ('q', Blue);
        ('w', Blue);
        ('e', Blue);
        ('r', Blue);
        ('t', Blue);
        ('y', Blue);
        ('u', Blue);
        ('i', Blue);
        ('o', Blue);
        ('p', Blue);
        ('a', Blue);
        ('s', Blue);
        ('d', Blue);
        ('f', Blue);
        ('g', Blue);
        ('h', Blue);
        ('j', Blue);
        ('k', Blue);
        ('l', Blue);
        ('z', Blue);
        ('x', Blue);
        ('c', Blue);
        ('v', Blue);
        ('b', Blue);
        ('n', Blue);
        ('m', Blue);
      ]
      [ 'q' ] [] []
      [
        ('q', Red);
        ('w', Blue);
        ('e', Blue);
        ('r', Blue);
        ('t', Blue);
        ('y', Blue);
        ('u', Blue);
        ('i', Blue);
        ('o', Blue);
        ('p', Blue);
        ('a', Blue);
        ('s', Blue);
        ('d', Blue);
        ('f', Blue);
        ('g', Blue);
        ('h', Blue);
        ('j', Blue);
        ('k', Blue);
        ('l', Blue);
        ('z', Blue);
        ('x', Blue);
        ('c', Blue);
        ('v', Blue);
        ('b', Blue);
        ('n', Blue);
        ('m', Blue);
      ];
    update_keyboard_test
      "test updating 1 red character on a previously updated keyboard"
      [
        ('q', Blue);
        ('w', Green);
        ('e', Blue);
        ('r', Blue);
        ('t', Blue);
        ('y', Blue);
        ('u', Blue);
        ('i', Blue);
        ('o', Blue);
        ('p', Blue);
        ('a', Blue);
        ('s', Blue);
        ('d', Blue);
        ('f', Blue);
        ('g', Green);
        ('h', Blue);
        ('j', Blue);
        ('k', Blue);
        ('l', Blue);
        ('z', Blue);
        ('x', Blue);
        ('c', Blue);
        ('v', Red);
        ('b', Blue);
        ('n', Blue);
        ('m', Blue);
      ]
      [ 'q' ] [] []
      [
        ('q', Red);
        ('w', Green);
        ('e', Blue);
        ('r', Blue);
        ('t', Blue);
        ('y', Blue);
        ('u', Blue);
        ('i', Blue);
        ('o', Blue);
        ('p', Blue);
        ('a', Blue);
        ('s', Blue);
        ('d', Blue);
        ('f', Blue);
        ('g', Green);
        ('h', Blue);
        ('j', Blue);
        ('k', Blue);
        ('l', Blue);
        ('z', Blue);
        ('x', Blue);
        ('c', Blue);
        ('v', Red);
        ('b', Blue);
        ('n', Blue);
        ('m', Blue);
      ];
    update_keyboard_test
      "test updating 1 yellow character on initial keyboard"
      [
        ('q', Blue);
        ('w', Blue);
        ('e', Blue);
        ('r', Blue);
        ('t', Blue);
        ('y', Blue);
        ('u', Blue);
        ('i', Blue);
        ('o', Blue);
        ('p', Blue);
        ('a', Blue);
        ('s', Blue);
        ('d', Blue);
        ('f', Blue);
        ('g', Blue);
        ('h', Blue);
        ('j', Blue);
        ('k', Blue);
        ('l', Blue);
        ('z', Blue);
        ('x', Blue);
        ('c', Blue);
        ('v', Blue);
        ('b', Blue);
        ('n', Blue);
        ('m', Blue);
      ]
      [] [ 'b' ] []
      [
        ('q', Blue);
        ('w', Blue);
        ('e', Blue);
        ('r', Blue);
        ('t', Blue);
        ('y', Blue);
        ('u', Blue);
        ('i', Blue);
        ('o', Blue);
        ('p', Blue);
        ('a', Blue);
        ('s', Blue);
        ('d', Blue);
        ('f', Blue);
        ('g', Blue);
        ('h', Blue);
        ('j', Blue);
        ('k', Blue);
        ('l', Blue);
        ('z', Blue);
        ('x', Blue);
        ('c', Blue);
        ('v', Blue);
        ('b', Yellow);
        ('n', Blue);
        ('m', Blue);
      ];
    update_keyboard_test
      "test updating multiple yellow characters on initial keyboard"
      [
        ('q', Blue);
        ('w', Blue);
        ('e', Blue);
        ('r', Blue);
        ('t', Blue);
        ('y', Blue);
        ('u', Blue);
        ('i', Blue);
        ('o', Blue);
        ('p', Blue);
        ('a', Blue);
        ('s', Blue);
        ('d', Blue);
        ('f', Blue);
        ('g', Blue);
        ('h', Blue);
        ('j', Blue);
        ('k', Blue);
        ('l', Blue);
        ('z', Blue);
        ('x', Blue);
        ('c', Blue);
        ('v', Blue);
        ('b', Blue);
        ('n', Blue);
        ('m', Blue);
      ]
      [] [ 'b'; 'a'; 'd' ] []
      [
        ('q', Blue);
        ('w', Blue);
        ('e', Blue);
        ('r', Blue);
        ('t', Blue);
        ('y', Blue);
        ('u', Blue);
        ('i', Blue);
        ('o', Blue);
        ('p', Blue);
        ('a', Yellow);
        ('s', Blue);
        ('d', Yellow);
        ('f', Blue);
        ('g', Blue);
        ('h', Blue);
        ('j', Blue);
        ('k', Blue);
        ('l', Blue);
        ('z', Blue);
        ('x', Blue);
        ('c', Blue);
        ('v', Blue);
        ('b', Yellow);
        ('n', Blue);
        ('m', Blue);
      ];
    update_keyboard_test
      "test updating 1 green character on initial keyboard"
      [
        ('q', Blue);
        ('w', Blue);
        ('e', Blue);
        ('r', Blue);
        ('t', Blue);
        ('y', Blue);
        ('u', Blue);
        ('i', Blue);
        ('o', Blue);
        ('p', Blue);
        ('a', Blue);
        ('s', Blue);
        ('d', Blue);
        ('f', Blue);
        ('g', Blue);
        ('h', Blue);
        ('j', Blue);
        ('k', Blue);
        ('l', Blue);
        ('z', Blue);
        ('x', Blue);
        ('c', Blue);
        ('v', Blue);
        ('b', Blue);
        ('n', Blue);
        ('m', Blue);
      ]
      [] [] [ 'o' ]
      [
        ('q', Blue);
        ('w', Blue);
        ('e', Blue);
        ('r', Blue);
        ('t', Blue);
        ('y', Blue);
        ('u', Blue);
        ('i', Blue);
        ('o', Green);
        ('p', Blue);
        ('a', Blue);
        ('s', Blue);
        ('d', Blue);
        ('f', Blue);
        ('g', Blue);
        ('h', Blue);
        ('j', Blue);
        ('k', Blue);
        ('l', Blue);
        ('z', Blue);
        ('x', Blue);
        ('c', Blue);
        ('v', Blue);
        ('b', Blue);
        ('n', Blue);
        ('m', Blue);
      ];
    update_keyboard_test
      "test updating multiple green characters on initial keyboard"
      [
        ('q', Blue);
        ('w', Blue);
        ('e', Blue);
        ('r', Blue);
        ('t', Blue);
        ('y', Blue);
        ('u', Blue);
        ('i', Blue);
        ('o', Blue);
        ('p', Blue);
        ('a', Blue);
        ('s', Blue);
        ('d', Blue);
        ('f', Blue);
        ('g', Blue);
        ('h', Blue);
        ('j', Blue);
        ('k', Blue);
        ('l', Blue);
        ('z', Blue);
        ('x', Blue);
        ('c', Blue);
        ('v', Blue);
        ('b', Blue);
        ('n', Blue);
        ('m', Blue);
      ]
      [] [] [ 'o'; 'u'; 'a'; 'k' ]
      [
        ('q', Blue);
        ('w', Blue);
        ('e', Blue);
        ('r', Blue);
        ('t', Blue);
        ('y', Blue);
        ('u', Green);
        ('i', Blue);
        ('o', Green);
        ('p', Blue);
        ('a', Green);
        ('s', Blue);
        ('d', Blue);
        ('f', Blue);
        ('g', Blue);
        ('h', Blue);
        ('j', Blue);
        ('k', Green);
        ('l', Blue);
        ('z', Blue);
        ('x', Blue);
        ('c', Blue);
        ('v', Blue);
        ('b', Blue);
        ('n', Blue);
        ('m', Blue);
      ];
    update_keyboard_test
      "test updating multiple red characters on initial keyboard"
      [
        ('q', Blue);
        ('w', Blue);
        ('e', Blue);
        ('r', Blue);
        ('t', Blue);
        ('y', Blue);
        ('u', Blue);
        ('i', Blue);
        ('o', Blue);
        ('p', Blue);
        ('a', Blue);
        ('s', Blue);
        ('d', Blue);
        ('f', Blue);
        ('g', Blue);
        ('h', Blue);
        ('j', Blue);
        ('k', Blue);
        ('l', Blue);
        ('z', Blue);
        ('x', Blue);
        ('c', Blue);
        ('v', Blue);
        ('b', Blue);
        ('n', Blue);
        ('m', Blue);
      ]
      [ 'q'; 'w'; 'i'; 'z' ] [] []
      [
        ('q', Red);
        ('w', Red);
        ('e', Blue);
        ('r', Blue);
        ('t', Blue);
        ('y', Blue);
        ('u', Blue);
        ('i', Red);
        ('o', Blue);
        ('p', Blue);
        ('a', Blue);
        ('s', Blue);
        ('d', Blue);
        ('f', Blue);
        ('g', Blue);
        ('h', Blue);
        ('j', Blue);
        ('k', Blue);
        ('l', Blue);
        ('z', Red);
        ('x', Blue);
        ('c', Blue);
        ('v', Blue);
        ('b', Blue);
        ('n', Blue);
        ('m', Blue);
      ];
    update_keyboard_test
      "test updating multiple red characters on previously updated \
       keyboard, with the letters updated already being red"
      [
        ('q', Red);
        ('w', Red);
        ('e', Blue);
        ('r', Blue);
        ('t', Blue);
        ('y', Blue);
        ('u', Blue);
        ('i', Red);
        ('o', Blue);
        ('p', Blue);
        ('a', Blue);
        ('s', Blue);
        ('d', Blue);
        ('f', Blue);
        ('g', Blue);
        ('h', Blue);
        ('j', Blue);
        ('k', Blue);
        ('l', Blue);
        ('z', Red);
        ('x', Blue);
        ('c', Blue);
        ('v', Blue);
        ('b', Blue);
        ('n', Blue);
        ('m', Blue);
      ]
      [ 'q'; 'w'; 'i'; 'z' ] [] []
      [
        ('q', Red);
        ('w', Red);
        ('e', Blue);
        ('r', Blue);
        ('t', Blue);
        ('y', Blue);
        ('u', Blue);
        ('i', Red);
        ('o', Blue);
        ('p', Blue);
        ('a', Blue);
        ('s', Blue);
        ('d', Blue);
        ('f', Blue);
        ('g', Blue);
        ('h', Blue);
        ('j', Blue);
        ('k', Blue);
        ('l', Blue);
        ('z', Red);
        ('x', Blue);
        ('c', Blue);
        ('v', Blue);
        ('b', Blue);
        ('n', Blue);
        ('m', Blue);
      ];
    update_keyboard_test
      "test updating multiple red characters on a previously updated \
       keyboard"
      [
        ('q', Blue);
        ('w', Blue);
        ('e', Blue);
        ('r', Blue);
        ('t', Blue);
        ('y', Blue);
        ('u', Blue);
        ('i', Blue);
        ('o', Blue);
        ('p', Blue);
        ('a', Blue);
        ('s', Blue);
        ('d', Blue);
        ('f', Blue);
        ('g', Blue);
        ('h', Blue);
        ('j', Blue);
        ('k', Blue);
        ('l', Blue);
        ('z', Blue);
        ('x', Green);
        ('c', Blue);
        ('v', Blue);
        ('b', Red);
        ('n', Blue);
        ('m', Red);
      ]
      [ 'q'; 'w'; 'i'; 'z' ] [] []
      [
        ('q', Red);
        ('w', Red);
        ('e', Blue);
        ('r', Blue);
        ('t', Blue);
        ('y', Blue);
        ('u', Blue);
        ('i', Red);
        ('o', Blue);
        ('p', Blue);
        ('a', Blue);
        ('s', Blue);
        ('d', Blue);
        ('f', Blue);
        ('g', Blue);
        ('h', Blue);
        ('j', Blue);
        ('k', Blue);
        ('l', Blue);
        ('z', Red);
        ('x', Green);
        ('c', Blue);
        ('v', Blue);
        ('b', Red);
        ('n', Blue);
        ('m', Red);
      ];
    update_keyboard_test
      "test updating 1 character of each color on initial keyboard"
      [
        ('q', Blue);
        ('w', Blue);
        ('e', Blue);
        ('r', Blue);
        ('t', Blue);
        ('y', Blue);
        ('u', Blue);
        ('i', Blue);
        ('o', Blue);
        ('p', Blue);
        ('a', Blue);
        ('s', Blue);
        ('d', Blue);
        ('f', Blue);
        ('g', Blue);
        ('h', Blue);
        ('j', Blue);
        ('k', Blue);
        ('l', Blue);
        ('z', Blue);
        ('x', Blue);
        ('c', Blue);
        ('v', Blue);
        ('b', Blue);
        ('n', Blue);
        ('m', Blue);
      ]
      [ 'q' ] [ 'i' ] [ 'o' ]
      [
        ('q', Red);
        ('w', Blue);
        ('e', Blue);
        ('r', Blue);
        ('t', Blue);
        ('y', Blue);
        ('u', Blue);
        ('i', Yellow);
        ('o', Green);
        ('p', Blue);
        ('a', Blue);
        ('s', Blue);
        ('d', Blue);
        ('f', Blue);
        ('g', Blue);
        ('h', Blue);
        ('j', Blue);
        ('k', Blue);
        ('l', Blue);
        ('z', Blue);
        ('x', Blue);
        ('c', Blue);
        ('v', Blue);
        ('b', Blue);
        ('n', Blue);
        ('m', Blue);
      ];
    play_legal_guess_num_test "test 2nd guess's updated guess number"
      camel_exploded level1 init_state_to_test_l1 2;
    play_legal_colors_test "test 2nd guess's updated colors array"
      camel_exploded level1 init_state_to_test_l1
      colors_array_from_test_l1;
    play_legal_mystery_test "test 2nd guess's updated mystery word"
      camel_exploded level1 init_state_to_test_l1
      mystery_word_from_test_l1;
    play_legal_keyboard_test "test 2nd guess's updated keyboard"
      camel_exploded level1 init_state_to_test_l1 keyboard_from_test_l1;
    play_legal_guess_num_test "test 3rd guess's updated guess number"
      camel_exploded level1 guess_two_state_to_test_l1 3;
    play_legal_guess_num_test "test 4th guess's updated guess number"
      camel_exploded level1 guess_three_state_to_test_l1 4;
    play_legal_guess_num_test "test 5th guess's updated guess number"
      camel_exploded level1 guess_four_state_to_test_l1 5;
    play_illegal_guess_num_test "test illegal 6th guess" camel_exploded
      level1 guess_five_state_to_test_l1 6;
    not_exists_lst_test "testing two empty lists" [] [] [] [];
    not_exists_lst_test
      "testing two empty lists with a non-empty accumulator"
      [ 'a'; 'b' ] [] [] [ 'a'; 'b' ];
    not_exists_lst_test "testing two identical lists, one char" []
      [ 'a' ] [ 'a' ] [];
    not_exists_lst_test
      "testing two identical lists, one char with a non-empty \
       accumulator"
      [ 'a'; 'b' ] [ 'a' ] [ 'a' ] [ 'a'; 'b' ];
    not_exists_lst_test "testing two identical lists" []
      [ 'a'; 'b'; 'c' ] [ 'a'; 'b'; 'c' ] [];
    not_exists_lst_test
      "testing two identical lists with a non-empty accumulator" [ 'a' ]
      [ 'a'; 'b'; 'c' ] [ 'a'; 'b'; 'c' ] [ 'a' ];
    not_exists_lst_test
      "testing two unique lists, no characters in common" []
      [ 'a'; 'b'; 'c' ] [ 'e'; 'f'; 'g' ] [ 'g'; 'f'; 'e' ];
    not_exists_lst_test
      "testing two unique lists, no characters in common with a \
       non-empty accumulator"
      [ 'a' ]
      [ 'b'; 'c'; 'd'; 'h'; 'i' ]
      [ 'e'; 'f'; 'g' ] [ 'g'; 'f'; 'e'; 'a' ];
    not_exists_lst_test "testing two unique lists, characters in common"
      []
      [ 'a'; 'b'; 'c'; 'd'; 'h' ]
      [ 'd'; 'e'; 'f'; 'g'; 'h' ]
      [ 'g'; 'f'; 'e' ];
    not_exists_lst_test
      "testing two unique lists, characters in common with a non-empty \
       accumulator"
      [ 'a' ]
      [ 'a'; 'b'; 'c'; 'd'; 'h' ]
      [ 'd'; 'e'; 'f'; 'g'; 'h' ]
      [ 'g'; 'f'; 'e'; 'a' ];
    exists_lst_test "testing two empty lists" [] [] [] [];
    exists_lst_test
      "testing two empty lists with a non-empty accumulator" [ 'a' ] []
      [] [ 'a' ];
    exists_lst_test
      "testing two unique lists with no characters in common" []
      [ 'a'; 'b' ] [ 'c'; 'd' ] [];
    exists_lst_test "testing two unique lists with 'a' in common" []
      [ 'a'; 'c'; 'd'; 'b' ] [ 'a'; 'f'; 'g' ] [ 'a' ];
    exists_lst_test
      "testing two unique lists with 'a' and 'b' in common" []
      [ 'a'; 'c'; 'd'; 'b' ] [ 'a'; 'b' ] [ 'b'; 'a' ];
    exists_lst_test
      "testing two unique lists with no characters in common and a \
       non-empty accumulator"
      [ 'a' ] [ 'c'; 'b' ] [ 'y'; 'z' ] [ 'a' ];
    exists_lst_test
      "testing two identicle lists with characters in common and a \
       non-empty accumulator"
      [ 'a' ] [ 'c'; 'b' ] [ 'b'; 'f'; 'g' ] [ 'b'; 'a' ];
    exists_lst_test
      "testing two identical lists with 'a' and 'b' in common" []
      [ 'a'; 'b' ] [ 'a'; 'b' ] [ 'b'; 'a' ];
    exists_lst_test
      "testing two identical lists with 'a' and 'b' in common and a \
       non-empty accumulator"
      [ 'c' ] [ 'a'; 'b' ] [ 'a'; 'b' ] [ 'b'; 'a'; 'c' ];
    exists_lst_test
      "testing two identical lists with 'a' in common and a non-empty \
       accumulator "
      [ 'a'; 'b' ]
      [ 'a'; 'b'; 'c'; 'd'; 'e' ]
      [ 'a'; 'f'; 'g'; 'h'; 'i' ]
      [ 'a'; 'a'; 'b' ];
  ]

let suite =
  "test suite for A2"
  >::: List.flatten [ owordl_tests; command_tests; state_tests ]

let _ = run_test_tt_main suite
