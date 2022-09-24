(** Representation of static owordl data.*)

type t
(** The abstract type of games that consists of all the words within a
    certain game level. *)

val from_json : Yojson.Basic.t -> t
(** [from_json j] is the game generated from the words that [j]
    contains. Requires: [j] is a valid JSON file of words. *)

val game_words : t -> string list
(** [game_words g] is a list of all of the words in game [g]. *)

val mystery_word : t -> string
(** [mystery_word g] is the random word generated from the words in game
    [g]. *)

val print_word : t -> unit
(** [print_word g] prints the random word outputed from [mystery_word g]
    followed by a newline.*)

val length : t -> int
(** [length g] returns the length that the guess should be.*)

val invalid_word : string -> t -> bool
(** [invalid_word str g] returns true if [str] is a member of the words
    belonging to game [g]; else, it returns false. *)
