open Game

module type OwordlSig = sig
  type t

  (*add exceptions *)

  val from_json : Yojson.Basic.t -> t
  val mystery_word : t -> string
  val print_word : t -> unit
end

module OwordlCheck : OwordlSig = Owordl

module type CommandSig = sig
  type guess = char list

  type command =
    | Play of guess
    | Quit

  exception Empty
  exception Malformed
  exception Length
  exception Invalid

  val parse : string -> Owordl.t -> Owordl.t -> command
end

module CommandCheck : CommandSig = Command

module type StateSig = sig
  type t

  val init_state : Owordl.t -> t
  val create_map : char list -> (int * char) list
  val explode : string -> char list

  type result =
    | Legal of t
    | Illegal of t

  val play : char list -> Owordl.t -> t -> result
end

module StateCheck : StateSig = State

module type AuthorSig = sig
  val hours_worked : int
end

module AuthorCheck : AuthorSig = Author

let _ = if Author.hours_worked < 0 then exit 1
