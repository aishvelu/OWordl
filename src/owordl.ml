open Yojson.Basic.Util
open Random

type t = { words : string list }

let helper_json_list file name = file |> member name |> to_list
let helper_json_string file = file |> to_string

let from_json json =
  {
    words = List.map helper_json_string (helper_json_list json "words");
  }

let game_words g = g.words

let mystery_word g =
  let _ = Random.self_init () in
  let words = game_words g in
  let rand_index = Random.int (List.length words - 1) in
  String.lowercase_ascii (List.nth words rand_index)

let length g = String.length (mystery_word g)
let print_word g = Printf.printf "%s\n" (mystery_word g)

(*Citation for the dictionary json:
  https://github.com/maxlchen/OScrabl/blob/master/OScrabl/dictionary.json*)
let invalid_word guess dict_json = List.mem guess dict_json.words
