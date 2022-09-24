open Owordl
open Command

type color =
  | Red
  | Green
  | Yellow
  | Blue

type tile = {
  letter : string;
  background_color : color;
}

type t = {
  word_map : (int * char) list;
  guess_map : (int * char) list;
  mystery_word : char list;
  red : char list;
  yellow : char list;
  green : char list;
  colors : color array;
  guesses : int;
  keyboard : (char * color) list;
  board : tile array array;
}

(*-----------LAYOUT STUFF START--------------------*)
(*default tile for initializing an "empty" array*)
let default_tile = { letter = "x"; background_color = Blue }
let grid_skeleton n = Array.make_matrix 6 n default_tile

(**helper function for getting the correct ansiterminal color shortcut*)
let match_color d =
  match d with
  | Red -> ANSITerminal.on_red
  | Green -> ANSITerminal.on_green
  | Yellow -> ANSITerminal.on_yellow
  | Blue -> ANSITerminal.on_blue

let print_matrix st =
  let current_board = st.board in
  let len1 = Array.length current_board in
  for i = 0 to len1 - 1 do
    print_endline "\n";
    let c = current_board.(i) in
    let len2 = Array.length c in
    for j = 0 to len2 - 1 do
      let tile_color = match_color c.(j).background_color in
      ANSITerminal.print_string
        [ tile_color; ANSITerminal.black ]
        (" " ^ c.(j).letter ^ " ");
      ANSITerminal.move_cursor 2 0
    done
  done

(*helper for insert row*)
let create_tile_list current_guess current_colors =
  let row = Array.make (List.length current_guess) default_tile in
  for i = 0 to List.length current_guess - 1 do
    let l =
      String.make 1 ((fun (_, c) -> c) (List.nth current_guess i))
    in
    let c = Array.get current_colors i in
    let tile = { letter = l; background_color = c } in
    Array.set row i tile
  done;
  row

let insert_row
    current_board
    current_guesses
    current_guess
    current_colors =
  let row_num = current_guesses in
  current_board.(row_num) <-
    create_tile_list current_guess current_colors;
  current_board

(*-----------LAYOUT ELEMENTS END--------------------*)
let get_keyboard st = st.keyboard
let get_guess_num st = st.guesses
let get_guess_map st = st.guess_map

(*function to get the array of colors for the current guess*)
let get_colors st = st.colors
let get_word_map st = st.word_map

let get_mystery st =
  String.concat "" (List.map (String.make 1) st.mystery_word)

let create_map lst =
  let next_val =
    let counter = ref 0 in
    fun () ->
      counter := !counter + 1;
      !counter
  in
  List.map (fun element -> (next_val (), element)) lst

let rec exists_lst
    (acc : char list)
    (mystery_lst : char list)
    (guess_lst : char list) : char list =
  match guess_lst with
  | [] -> acc
  | h :: t ->
      if List.mem h mystery_lst then exists_lst (h :: acc) mystery_lst t
      else exists_lst acc mystery_lst t

let rec not_exists_lst
    (acc : char list)
    (mystery_lst : char list)
    (guess_lst : char list) : char list =
  match guess_lst with
  | [] -> acc
  | h :: t ->
      if not (List.mem h mystery_lst) then
        not_exists_lst (h :: acc) mystery_lst t
      else not_exists_lst acc mystery_lst t

let explode s =
  let rec exp i l = if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

(*cited from
  https://stackoverflow.com/questions/10068713/string-to-list-of-char*)
let init_state game =
  let mystery_list = explode (mystery_word game) in
  {
    word_map = create_map mystery_list;
    guess_map = create_map (List.init (length game) (fun _ -> ' '));
    mystery_word = mystery_list;
    red = [];
    yellow = [];
    green = [];
    colors = Array.make (length game) Blue;
    guesses = 0;
    keyboard =
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
    board = grid_skeleton (length game);
  }

type result =
  | Legal of t
  | Illegal of t

let rec match_maps
    (guess : (int * char) list)
    (word : (int * char) list) =
  match guess with
  | [] -> []
  | (i, v) :: t ->
      let letter = String.make 1 v in
      if v = List.assoc i word then
        (letter ^ " : correct") :: match_maps t word
      else if not (List.mem v (List.map (fun (_, x) -> x) word)) then
        (letter ^ " : incorrect") :: match_maps t word
      else (letter ^ " : wrong position") :: match_maps t word

let rec get_greens annotated_positions =
  match annotated_positions with
  | [] -> []
  | h :: t ->
      let lst = String.split_on_char ' ' h in
      if List.mem "correct" lst then
        String.get (List.nth lst 0) 0 :: get_greens t
      else get_greens t

let rec get_yellows annotated_positions =
  match annotated_positions with
  | [] -> []
  | h :: t ->
      let lst = String.split_on_char ' ' h in
      if List.mem "position" lst then
        String.get (List.nth lst 0) 0 :: get_yellows t
      else get_yellows t

let rec get_reds annotated_positions =
  match annotated_positions with
  | [] -> []
  | h :: t ->
      let lst = String.split_on_char ' ' h in
      if List.mem "incorrect" lst then
        String.get (List.nth lst 0) 0 :: get_reds t
      else get_reds t

let set_colors annotated_positions colors_array =
  for i = 0 to Array.length colors_array - 1 do
    colors_array.(i) <-
      (match
         String.split_on_char ' ' (List.nth annotated_positions i)
       with
      | s when List.mem "correct" s -> Green
      | s when List.mem "incorrect" s -> Red
      | s when List.mem "position" s -> Yellow
      | _ -> Blue)
  done

let print_output st =
  let list = match_maps st.guess_map st.word_map in
  print_string (String.concat "\n" list);
  print_endline ""

let winning_condition st =
  if st.guess_map = st.word_map then true else false

let rec set_keyboard_color keyboard color_lst color =
  match keyboard with
  | [] -> []
  | (l, c) :: t ->
      if List.mem l color_lst then
        (l, color) :: set_keyboard_color t color_lst color
      else (l, c) :: set_keyboard_color t color_lst color

let update_keyboard curr_keyboard red_lst yellow_lst green_lst =
  let red_set_keyboard = set_keyboard_color curr_keyboard red_lst Red in
  let yellow_set_keyboard =
    set_keyboard_color red_set_keyboard yellow_lst Yellow
  in
  let green_set_keyboard =
    set_keyboard_color yellow_set_keyboard green_lst Green
  in
  green_set_keyboard

let play guess game st =
  let guessed_map = create_map guess in
  let annotated_positions = match_maps guessed_map st.word_map in
  let red_letters = not_exists_lst st.red st.mystery_word guess in
  let existing_letters = exists_lst [] st.mystery_word guess in
  let yellow_letters = get_yellows annotated_positions in
  let green_letters = get_greens annotated_positions in
  let updated_colors =
    let _ = set_colors annotated_positions st.colors in
    st.colors
  in
  let num_guesses = st.guesses + 1 in
  if st.guesses < 5 then (
    assert (List.length existing_letters <= List.length guessed_map);
    Legal
      {
        word_map = st.word_map;
        guess_map = guessed_map;
        mystery_word = st.mystery_word;
        red = red_letters;
        yellow = yellow_letters;
        green = green_letters;
        colors = updated_colors;
        guesses = num_guesses;
        keyboard =
          update_keyboard st.keyboard red_letters yellow_letters
            green_letters;
        board =
          insert_row st.board (num_guesses - 1) guessed_map
            updated_colors;
      })
  else
    Illegal
      {
        word_map = st.word_map;
        guess_map = guessed_map;
        mystery_word = st.mystery_word;
        red = red_letters;
        yellow = yellow_letters;
        green = green_letters;
        colors = updated_colors;
        guesses = num_guesses;
        keyboard =
          update_keyboard st.keyboard red_letters yellow_letters
            green_letters;
        board =
          insert_row st.board (num_guesses - 1) guessed_map
            updated_colors;
      }
