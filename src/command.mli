(** Parsing of player commands. *)

open Owordl

(* Citation: Slighly adapted from CS3110 A2 command.mli file. *)
type guess = char list
(** The type [guess] represents the object phrase that can be part of a
    player command. Each element of the list represents a word of the
    object phrase, where a {i word} is defined as a consecutive sequence
    of non-space characters. Thus, no element of the list should contain
    any leading, internal, or trailing spaces. The list is in the same
    order as the words in the original player command. For example:

    - If the player command is ["play camel"], then the object phrase is
      [\["camel"\]].

    - If the player command is ["play camel     "], then the object
      phrase is again [\["camel"\]].

    An [object_phrase] is not permitted to be the empty list. *)

(** The type [command] represents a player command that is decomposed
    into a verb and possibly an object phrase. *)

type command =
  | Play of guess
  | Quit

exception Empty
(** Raised when an empty command is parsed. *)

exception Malformed
(** Raised when a malformed command is encountered. *)

exception Length
(** Raised when the guess of a play command is not the number of
    characters associated with current game level. *)

exception Invalid
(** Raised when the guess of a play command is not a word contained in
    the current game. *)

val parse : string -> Owordl.t -> Owordl.t -> command
(** [parse str g] parses a player's input into a [command], as follows.
    The first word (i.e., consecutive sequence of non-space characters)
    of [str] becomes the verb. The rest of the words, if any, become the
    object phrase. Examples:

    - [parse "    play   camel  "] is [Play \["camel"\]]
    - [parse "quit"] is [Quit].

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space
    characters (only ASCII character code 32; not tabs or newlines,
    etc.).

    Raises: [Empty] if [str] is the empty string or contains only
    spaces.

    Raises: [Malformed] if the command is malformed. A command is
    {i malformed} if the verb is neither "quit" nor "play", or if the
    verb is "quit" and there is a non-empty object phrase, or if the
    verb is "play" and there is an empty object phrase.*)
