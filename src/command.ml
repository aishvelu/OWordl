open Owordl

type guess = char list

type command =
  | Play of guess
  | Quit

exception Empty
exception Malformed
exception Length
exception Invalid

let length game = String.length (mystery_word game)

let remove_spaces str =
  let str_lst = String.split_on_char ' ' (str |> String.trim) in
  List.filter (fun x -> not (x = "")) str_lst

let explode s =
  let rec exp i l = if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

(* Citation:
   https://stackoverflow.com/questions/10068713/string-to-list-of-char *)

let parse str g dict =
  match remove_spaces str with
  | [] -> raise Empty
  | h :: t when h = "play" && t <> [] && List.length t = 1 ->
      let guess_word = List.nth t 0 in
      if invalid_word guess_word dict then
        if List.length (explode (List.nth t 0)) = length g then
          Play (explode (List.nth t 0))
        else raise Length
      else raise Invalid
  | [ "quit" ] -> Quit
  | _ -> raise Malformed
