

type t = {
  letters: string;
  words: string list;
  bonus_word: string;
  level_score: int;
  found_words: string list;
  curr_level: int;
  hints_remaining: int;
}


let make_level game curr_level= 
  let level = Game.pick_level game curr_level in 
  {
    letters = Game.get_letters level;
    words = Game.get_words level;
    bonus_word = Game.get_bonus_word level;
    level_score = 0;
    found_words = [];
    curr_level = curr_level;
    hints_remaining = 3;
  }

(** [is_new_word level word] is true if [word] is a valid word in the level,
    [level] that has not been found yet. False, otherwise.*)
let is_new_word level word = 
  List.mem word level.words && not (List.mem word level.found_words)

(** [is_new_bonus level word] is true if [word] is the bonus word of [level]
    and has not been found yet. False, otherwise.*)
let is_new_bonus level word = 
  word = level.bonus_word && not (List.mem word level.found_words)

(** [points word] is the number of points that [word] is worth*)
let points word = String.length word

(** [update_word level word] is [level] with [word] found.
    I.e. added to found_words and its points awarded.
    Requires: [word] is a valid word of [level], but is not the bonus word.*)
let update_word level word = {
  letters = level.letters;
  words = level.words;
  bonus_word = level.bonus_word;
  level_score = level.level_score + points(word);
  found_words = word::level.found_words;
  curr_level = level.curr_level;
  hints_remaining = level.hints_remaining;
}

(** [update_bonus level word] is [level] with [word] found.
    I.e. added to found_words and its points awarded.
    Requires: [word] is the bonus word of [level]*)
let update_bonus level word = {
  letters = level.letters;
  words = level.words;
  bonus_word = level.bonus_word;
  level_score = level.level_score + 2*points(word);
  found_words = word::level.found_words;
  curr_level = level.curr_level;
  hints_remaining = level.hints_remaining;
}


let guess_word level word = 
  if is_new_word level word then update_word level word 
  else if is_new_bonus level word then update_bonus level word
  else level

let get_score level = 
  level.level_score

let get_words level = 
  level.words

let get_found_words level = 
  level.found_words

let get_bonus level = 
  level.bonus_word

let get_letters level = 
  level.letters

let check_level level =
  (List.length level.words) + 1 = List.length level.found_words


let use_hint level = 
  let all_words = List.rev_append (get_words level) [get_bonus level] in
  let missing_words = 
  (List.filter (fun x -> not (List.mem x (get_found_words level))) all_words) in
  
  match missing_words with 
  | [] -> print_endline("You found all the words."); ' '
  | h::t -> String.get h 0

let next_level game level = 
  let updated_game = 
    Game.update_game level.level_score (List.length level.found_words) 1 game in
  let next = make_level updated_game (level.curr_level + 1) in
  (updated_game, next)  


let end_level game level= 
    Game.update_game level.level_score (List.length level.found_words) 1 game

let dec_hints level = {
  letters = level.letters;
  words= level.words;
  bonus_word= level.bonus_word;
  level_score= level.level_score;
  found_words= level.found_words;
  curr_level = level.curr_level;
  hints_remaining= level.hints_remaining -1;
}

let get_hints_remaining level = 
  level.hints_remaining