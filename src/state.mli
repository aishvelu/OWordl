(** Representation of dynamic game state. *)

open Owordl

type color =
  | Red
  | Green
  | Yellow
  | Blue

(** The abstract type of colors for each letter in grid. *)

type t
(** The abstract type of values representing the game state. *)

val explode : string -> char list
(**[explode str] take a stings and parses it into a list of characters.*)

val create_map : char list -> (int * char) list
(*[create_map char_lst] outputs a tuple list of characters and their
  associated index positions within a list for a given character list
  [char_lst].*)

val exists_lst : char list -> char list -> char list -> char list
(**[exists_lst acc mystery_lst guess_lst] accumulates characters that
   are in both mystery_lst and guess_lst onto an initiually empty list,
   acc*)

val not_exists_lst : char list -> char list -> char list -> char list
(**[not_exists_lst acc mystery_lst guess_lst] accumulates characters
   that are not in both mystery_lst and guess_lst onto an initiually
   empty list, acc*)

val winning_condition : t -> bool
(**[winning_condition st] returns true when player has guessed the right
   word in their limited guesses*)

val get_guess_map : t -> (int * char) list
(**[get_guess_map st] returns each character of a guess mapped to its
   index*)

val get_guess_num : t -> int
(**[get_guess_map st] returns the number of guesses the user is up to*)

val get_colors : t -> color array
(**[get_colors st] returns array of colors for a current guess*)

val get_mystery : t -> string
(**[get_mystery st] returns string of mystery word for when player loses*)

val explode : string -> char list
(** [explode str] is a character list of the characters in [str] in
    order. *)

val print_output : t -> unit
(** [print_output st] prints the output response to they player's guess,
    informing them of the correctness of each letter in the player's
    guess*)

val init_state : Owordl.t -> t
(** [init_state a] is the initial state of the game when playing
    adventure [a]. In that state the adventurer is currently located in
    the starting room, and they have visited only that room. *)

val update_keyboard :
  (char * color) list ->
  char list ->
  char list ->
  char list ->
  (char * color) list
(** [update_keyboard st] changes the colors of the characters in the
    keyboard based on latest guess*)

val print_matrix : t -> unit
(** [print_matrix st] prints the current board *)

val get_keyboard : t -> (char * color) list
(**[get_keyboard st] returns the keyboard to display in current state*)

type result =
  | Legal of t
  | Illegal of t

(** The type representing the result of an attempted movement. *)

val match_maps : (int * char) list -> (int * char) list -> string list
(** [match_maps guess_map word_map] is a string list of comparisons used
    to tell if the proper characters are in the proper places*)

val get_greens : string list -> char list
(** [get_greens annotated_pos_lst] is a character list of all the
    guessed letters that exist in the mystery word and are in the
    correct positions*)

val get_yellows : string list -> char list
(** [get_yellows annotated_pos_lst] is a character list of all the
    guessed letters that exist in the mystery word ut are in the
    incorrect positions*)

val get_reds : string list -> char list
(** [get_reds annotated_pos_lst] is a character list of all the guessed
    letters that do not exist in the mystery word*)

val play : char list -> Owordl.t -> t -> result
(** [play guess game st] is [r] if guessing the word parsed into the
    list guess in state [st] and game [game] results in [r]. If the
    number of guesses encoded in state [st] is less than or equal to 5,
    then [r] is [Legal st'], where [st'] is the player's updated state
    after making the guess [guess]. Otherwise, the result is
    [Illegal st'].

    Effects: none. [play] is not permitted to do any printing. *)
