open Yojson.Basic.Util

type level = {
  letters: string;
  words: string list;
  bonus_word: string;
}

type t = {
  levels: level list;
  game_score: int;
  num_words_found: int;
  levels_completed: int 
}

(** [make_level level] creates a record of type level based on the information
    stored in the Yojson.Basic.t [level]*)
let make_level level ={
  letters = level |> member "letters" |> to_string;
  words = level |> member "words" |> to_list |> List.map to_string;
  bonus_word = level |> member "bonus_word" |> to_string;
}

let make_game json = {
  levels = json |> member "levels"|> to_list |> List.map make_level;
  game_score = 0;
  num_words_found = 0;
  levels_completed = 0
}

(** [handle_win game] prints the ending statistics in [game] and then exits.*)
let handle_win game = 
  print_endline("Congratulations! You beat all of the levels");
  let lvs = game.levels_completed in 
  let sc = game.game_score in 
  let wrds = game.num_words_found in 
  Printf.printf "Levels Played: %d Words Found: %d Total Score: %d\n" lvs wrds sc;
  exit 0 

let pick_level (game:t) (curr_level)= 
  match List.nth_opt game.levels curr_level with 
  | None -> handle_win game
  | Some level -> level

let get_letters level = 
  level.letters

let get_words level = 
  level.words

let get_bonus_word level = 
  level.bonus_word

let update_game score words levels game = {
  levels = game.levels;
  game_score = game.game_score + score;
  num_words_found = game.num_words_found + words;
  levels_completed = game.levels_completed + levels;
}

let get_levels_completed game = 
  game.levels_completed

let get_total_score game =
  game.game_score

let  get_num_words game =
  game.num_words_found

(** [custom_level ltrs words bonus] creates a level record with the given 
    [ltrs], [words], and [bonus] word. *)
let custom_level ltrs words bonus = {
    letters = ltrs;
    words = words;
    bonus_word = bonus
  }

let add_level ltrs words bonus game = 
  let lvl = custom_level ltrs words bonus in 
{
  levels = game.levels@[lvl];
  game_score = game.game_score;
  num_words_found = game.num_words_found;
  levels_completed = game.levels_completed
}