open Game
open Owordl
open Command
open State
(*open Layout*)

(** [play_game f] starts the adventure in file [f]. *)

(*create a function to disply the keyboard and call it in play help*)
let string_of_color color =
  match color with
  | Red -> ANSITerminal.red
  | Yellow -> ANSITerminal.yellow
  | Green -> ANSITerminal.green
  | Blue -> ANSITerminal.blue

let print_keyboard keyboard =
  List.iter
    (fun elt ->
      let letter_of_color = string_of_color (snd elt) in
      let print_typical =
        ANSITerminal.print_string
          [ letter_of_color; ANSITerminal.Bold ]
          ((fst elt |> String.make 1) ^ " ")
      in
      if fst elt = 'a' then (
        ANSITerminal.print_string [ ANSITerminal.blue ] "\n  ";
        print_typical)
      else if fst elt = 'z' then (
        ANSITerminal.print_string [ ANSITerminal.blue ] "\n     ";
        print_typical)
      else print_typical)
    keyboard

(*printing helpers for play help*)
let print_legal_keyboard st =
  print_matrix st;
  print_endline "\n";
  print_endline "";
  print_keyboard (get_keyboard st);
  print_endline "\n"

let print_congrats () =
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "\n\
    \ █▀▀ █▀▀█ █▀▀▄ █▀▀▀ █▀▀█ █▀▀█ ▀▀█▀▀ █▀▀ █ \n\
    \ █░░ █░░█ █░░█ █░▀█ █▄▄▀ █▄▄█ ░░█░░ ▀▀█ ▀ \n\
    \ ▀▀▀ ▀▀▀▀ ▀░░▀ ▀▀▀▀ ▀░▀▀ ▀░░▀ ░░▀░░ ▀▀▀ ▄ \n\n"

let print_you () =
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "\n\n\n\
    \  \n\
    \ ██╗░░░██╗░█████╗░██╗░░░██╗\n\
    \ ╚██╗░██╔╝██╔══██╗██║░░░██║\n\
    \ ░╚████╔╝░██║░░██║██║░░░██║\n\
    \ ░░╚██╔╝░░██║░░██║██║░░░██║\n\
    \ ░░░██║░░░╚█████╔╝╚██████╔╝\n\
    \ ░░░╚═╝░░░░╚════╝░░╚═════╝░\n\n\n"

let print_won () =
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "\n\n\n\
    \  \n\n\
    \ ░██╗░░░░░░░██╗░█████╗░███╗░░██╗\n\
    \ ░██║░░██╗░░██║██╔══██╗████╗░██║\n\
    \ ░╚██╗████╗██╔╝██║░░██║██╔██╗██║\n\
    \ ░░████╔═████║░██║░░██║██║╚████║\n\
    \ ░░╚██╔╝░╚██╔╝░╚█████╔╝██║░╚███║\n\
    \ ░░░╚═╝░░░╚═╝░░░╚════╝░╚═╝░░╚══╝\n\n\n"

let print_the () =
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "\n\
    \     ████████╗██╗░░██╗███████╗\n\
    \     ╚══██╔══╝██║░░██║██╔════╝\n\
    \     ░░░██║░░░███████║█████╗░░\n\
    \     ░░░██║░░░██╔══██║██╔══╝░░\n\
    \     ░░░██║░░░██║░░██║███████╗\n\
    \     ░░░╚═╝░░░╚═╝░░╚═╝╚══════╝\n\n\n"

let print_game () =
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "\n\n\
    \             ░██████╗░░█████╗░███╗░░░███╗███████╗\n\
    \            ██╔════╝░██╔══██╗████╗░████║██╔════╝\n\
    \            ██║░░██╗░███████║██╔████╔██║█████╗░░\n\
    \            ██║░░╚██╗██╔══██║██║╚██╔╝██║██╔══╝░░\n\
    \            ╚██████╔╝██║░░██║██║░╚═╝░██║███████╗\n\
    \            ░╚═════╝░╚═╝░░╚═╝╚═╝░░░░░╚═╝╚══════╝\n\n\n"

let print_winning_condition st =
  print_congrats ();
  print_you ();
  print_won ();
  print_the ();
  print_game ();
  ANSITerminal.print_string [ ANSITerminal.green ] "You beat OWordl! \n"

let print_illegal_keyboard st =
  print_matrix st;
  print_endline "\n";
  print_endline "";
  print_keyboard (get_keyboard st);
  print_endline "\n"

let print_over () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\n\
    \               ░█████╗░██╗░░░██╗███████╗██████╗░\n\
    \               ██╔══██╗██║░░░██║██╔════╝██╔══██╗\n\
    \               ██║░░██║╚██╗░██╔╝█████╗░░██████╔╝\n\
    \               ██║░░██║░╚████╔╝░██╔══╝░░██╔══██╗\n\
    \               ╚█████╔╝░░╚██╔╝░░███████╗██║░░██║\n\
    \               ░╚════╝░░░░╚═╝░░░╚══════╝╚═╝░░╚═╝\n\n"

let print_have () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\n\n\
    \               ██╗░░██╗░█████╗░██╗░░░██╗███████╗\n\
    \               ██║░░██║██╔══██╗██║░░░██║██╔════╝\n\
    \               ███████║███████║╚██╗░██╔╝█████╗░░\n\
    \               ██╔══██║██╔══██║░╚████╔╝░██╔══╝░░\n\
    \               ██║░░██║██║░░██║░░╚██╔╝░░███████╗\n\
    \               ╚═╝░░╚═╝╚═╝░░╚═╝░░░╚═╝░░░╚══════╝\n\n"

let print_lost () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\n\n\n\
    \                       ██╗░░░░░░█████╗░░██████╗████████╗\n\
    \                       ██║░░░░░██╔══██╗██╔════╝╚══██╔══╝\n\
    \                       ██║░░░░░██║░░██║╚█████╗░░░░██║░░░\n\
    \                       ██║░░░░░██║░░██║░╚═══██╗░░░██║░░░\n\
    \                       ███████╗╚█████╔╝██████╔╝░░░██║░░░\n\
    \                       ╚══════╝░╚════╝░╚═════╝░░░░╚═╝░░░\n\n"

let print_game_over st =
  print_game ();
  print_over ();
  print_you ();
  print_have ();
  print_lost ();

  ANSITerminal.print_string [ ANSITerminal.red ]
    ("You ran out of guesses. The word was \"" ^ get_mystery st ^ "\"")

let print_malformed g =
  print_endline
    ("Invalid format. Please enter \"play\" followed by your "
    ^ string_of_int (length g)
    ^ " letter guess")

let print_length g =
  print_endline
    ("you must enter a "
    ^ string_of_int (length g)
    ^ " letter word. try again")

let print_empty g =
  print_endline
    ("Empty input. please enter \"play\" followed by your "
    ^ string_of_int (length g)
    ^ " letter guess")

let print_invalid g =
  print_endline
    ("This word is not in the English dictionary. please enter \
      \"play\" followed by your "
    ^ string_of_int (length g)
    ^ " letter guess")

let print_play_promt st =
  print_endline
    ("\n please enter \"play\" followed by your "
    ^ string_of_int (String.length (get_mystery st))
    ^ " letter guess");
  print_string "> "

let print_you_quit () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\n\n\n\n\
    \       ██╗░░░██╗░█████╗░██╗░░░██╗  ░██████╗░██╗░░░██╗██╗████████╗\n\
    \       ╚██╗░██╔╝██╔══██╗██║░░░██║  ██╔═══██╗██║░░░██║██║╚══██╔══╝\n\
    \       ░╚████╔╝░██║░░██║██║░░░██║  ██║██╗██║██║░░░██║██║░░░██║░░░\n\
    \       ░░╚██╔╝░░██║░░██║██║░░░██║  ╚██████╔╝██║░░░██║██║░░░██║░░░\n\
    \       ░░░██║░░░╚█████╔╝╚██████╔╝  ░╚═██╔═╝░╚██████╔╝██║░░░██║░░░\n\
    \       ░░░╚═╝░░░░╚════╝░░╚═════╝░  ░░░╚═╝░░░░╚═════╝░╚═╝░░░╚═╝░░░\n\n"

let print_please () =
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "\n\n\n\n\
    \              ██████╗░██╗░░░░░███████╗░█████╗░░██████╗███████╗\n\
    \              ██╔══██╗██║░░░░░██╔════╝██╔══██╗██╔════╝██╔════╝\n\
    \              ██████╔╝██║░░░░░█████╗░░███████║╚█████╗░█████╗░░\n\
    \              ██╔═══╝░██║░░░░░██╔══╝░░██╔══██║░╚═══██╗██╔══╝░░\n\
    \              ██║░░░░░███████╗███████╗██║░░██║██████╔╝███████╗\n\
    \              ╚═╝░░░░░╚══════╝╚══════╝╚═╝░░╚═╝╚═════╝░╚══════╝\n\n"

let print_play () =
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "\n\n\n\n\n\n\
    \              ██████╗░██╗░░░░░░█████╗░██╗░░░██╗\n\
    \              ██╔══██╗██║░░░░░██╔══██╗╚██╗░██╔╝\n\
    \              ██████╔╝██║░░░░░███████║░╚████╔╝░\n\
    \              ██╔═══╝░██║░░░░░██╔══██║░░╚██╔╝░░\n\
    \              ██║░░░░░███████╗██║░░██║░░░██║░░░\n\
    \              ╚═╝░░░░░╚══════╝╚═╝░░╚═╝░░░╚═╝░░░\n\n"

let print_again () =
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "\n\n\n\
    \                     ░█████╗░░██████╗░░█████╗░██╗███╗░░██╗\n\
    \                     ██╔══██╗██╔════╝░██╔══██╗██║████╗░██║\n\
    \                     ███████║██║░░██╗░███████║██║██╔██╗██║\n\
    \                     ██╔══██║██║░░╚██╗██╔══██║██║██║╚████║\n\
    \                     ██║░░██║╚██████╔╝██║░░██║██║██║░╚███║\n\
    \                     ╚═╝░░╚═╝░╚═════╝░╚═╝░░╚═╝╚═╝╚═╝░░╚══╝\n\n"

let rec play_help game state dict =
  print_play_promt state;
  let input = read_line () in
  try
    let parsed_input = parse input game dict in
    match parsed_input with
    | Play parsed_input -> begin
        let play_result = play parsed_input game state in
        match play_result with
        | Legal play_result' ->
            print_legal_keyboard play_result';
            if winning_condition play_result' then
              print_winning_condition play_result'
            else play_help game play_result' dict
        | Illegal play_result' ->
            print_illegal_keyboard play_result';
            if winning_condition play_result' then
              print_winning_condition play_result'
            else print_game_over play_result';
            exit 0
      end
    | Quit ->
        print_you_quit ();
        print_please ();
        print_play ();
        print_again ();
        print_endline "you have left the game"
  with
  | Malformed ->
      print_malformed game;
      play_help game state dict
  | Length ->
      print_length game;
      play_help game state dict
  | Empty ->
      print_empty game;
      play_help game state dict
  | Invalid ->
      print_invalid game;
      play_help game state dict

let play_game game =
  let cmz =
    try game |> Yojson.Basic.from_file
    with Sys_error s ->
      print_endline "Invalid file. Restart Game with Valid File.";
      exit 0
  in
  play_help (from_json cmz)
    (init_state (from_json cmz))
    (from_json
       (Yojson.Basic.from_file
          ("data" ^ Filename.dir_sep ^ "dictionary.json")))

let data_dir_prefix = "data" ^ Filename.dir_sep

(** [main ()] prompts for the game to play, then starts it. *)
let print_welcome () =
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "\n\n\
    \  ░██╗░░░░░░░██╗███████╗██╗░░░░░░█████╗░░█████╗░███╗░░░███╗███████╗\n\
    \ ░██║░░██╗░░██║██╔════╝██║░░░░░██╔══██╗██╔══██╗████╗░████║██╔════╝\n\
    \ ░╚██╗████╗██╔╝█████╗░░██║░░░░░██║░░╚═╝██║░░██║██╔████╔██║█████╗░░\n\
    \ ░░████╔═████║░██╔══╝░░██║░░░░░██║░░██╗██║░░██║██║╚██╔╝██║██╔══╝░░\n\
    \ ░░╚██╔╝░╚██╔╝░███████╗███████╗╚█████╔╝╚█████╔╝██║░╚═╝░██║███████╗\n\
    \ ░░░╚═╝░░░╚═╝░░╚══════╝╚══════╝░╚════╝░░╚════╝░╚═╝░░░░░╚═╝╚══════╝\n\
    \  \n\n"

let print_to () =
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "\n\n\
    \  ████████╗░█████╗░\n\
    \  ╚══██╔══╝██╔══██╗\n\
    \  ░░░██║░░░██║░░██║\n\
    \  ░░░██║░░░██║░░██║\n\
    \  ░░░██║░░░╚█████╔╝\n\
    \  ░░░╚═╝░░░░╚════╝░\n\
    \  \n\n"

let print_owordle () =
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "\n\n\
    \  ░█████╗░░██╗░░░░░░░██╗░█████╗░██████╗░██████╗░██╗░░░░░\n\
    \  ██╔══██╗░██║░░██╗░░██║██╔══██╗██╔══██╗██╔══██╗██║░░░░░\n\
    \  ██║░░██║░╚██╗████╗██╔╝██║░░██║██████╔╝██║░░██║██║░░░░░\n\
    \  ██║░░██║░░████╔═████║░██║░░██║██╔══██╗██║░░██║██║░░░░░\n\
    \  ╚█████╔╝░░╚██╔╝░╚██╔╝░╚█████╔╝██║░░██║██████╔╝███████╗\n\
    \  ░╚════╝░░░░╚═╝░░░╚═╝░░░╚════╝░╚═╝░░╚═╝╚═════╝░╚══════╝\n\
    \  \n\n"

let main () =
  print_welcome ();
  print_to ();
  print_owordle ();
  print_endline
    (*call grid_skeleton*)
    "Welcome to our word guessing game! Please enter the name of the \
     level you want to load.\n";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> play_game (data_dir_prefix ^ file_name ^ ".json")

(* Execute the game engine. *)
let () = main ()
