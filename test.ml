open Game
open OUnit2
(** TEST PLAN 
    We tested the Game and Level modules using OUnit and tested the 
    Interface and Addons module manually by playing the game. This also tested
    several Game and Level features, making sure to test next_level, and 
    end_level in the Level module with this method. We used a Black Box approach
    when developing our test suite. Our system relies on modules only doing what 
    is said in their specifications and so if we know that it is doing this 
    correctly then we can be assured that the rest of our system is correct.

    Extractor and Gamefiles:
    We tested loading all gamefiles in make play and they were all successful,
    this means that the extractor is working as intended and outputting the 
    correct json format for games and levels.
*)

(** Look at game0.json to find the first and last levels. Our scoring system
    is 1 point for every letter in the word, 2 if it is the bonus word.*)
let game0 = (Yojson.Basic.from_file ((Sys.getcwd())^"/extractor/Outputs/game0.json"))
let test_game = Game.make_game game0

let first_level = Game.pick_level test_game 0
let last_level = Game.pick_level test_game 19

let passed_first_level = Game.update_game 8 2 1 test_game
let passed_last_level = Game.update_game 23 6 1 test_game
let passed_last_and_first = Game.update_game 23 6 1 passed_first_level

let game_tests = [
  "Test get_letters first_level" >:: (fun _ ->
      assert_equal  "GYM" (Game.get_letters first_level) );

  "Test get_letters last_level" >:: (fun _ ->
      assert_equal  "TRAP" (Game.get_letters last_level) );

  "Test get_words first_level" >:: (fun _ ->
      assert_equal  ["MY"] (Game.get_words first_level) );

  "Test get_words last_level" >:: (fun _ ->
      assert_equal  ["TRAP";"TAP"; "PAT"; "RAP"; "PART"] (Game.get_words last_level) );

  "Test get_bonus_word first_level" >:: (fun _ ->
      assert_equal  "GYM" (Game.get_bonus_word first_level) );

  "Test get_bonus_word last_level" >:: (fun _ ->
      assert_equal  "ART" (Game.get_bonus_word last_level) );

  "Test get_total_score passed_first_level" >:: (fun _ ->
      assert_equal  8 (Game.get_total_score passed_first_level) );

  "Test get_total_score passed_last_level" >:: (fun _ ->
      assert_equal  23 (Game.get_total_score passed_last_level) );

  "Test get_num_words passed_first_level" >:: (fun _ ->
      assert_equal  2 (Game.get_num_words passed_first_level) );

  "Test get_num_words passed_last_level" >:: (fun _ ->
      assert_equal  6 (Game.get_num_words passed_last_level) );

  "Test get_levels_completed passed_first_level" >:: (fun _ ->
      assert_equal  1 (Game.get_levels_completed passed_first_level) );

  "Test get_levels_completed passed_last_level" >:: (fun _ ->
      assert_equal  1 (Game.get_levels_completed passed_last_level) );

  "Test get_total_score passed_last_and_first" >:: (fun _ ->
      assert_equal  31 (Game.get_total_score passed_last_and_first) );

  "Test get_num_words passed_last_and_first" >:: (fun _ ->
      assert_equal  8 (Game.get_num_words passed_last_and_first) );

  "Test get_levels_completed passed_last_and_first" >:: (fun _ ->
      assert_equal  2 (Game.get_levels_completed passed_last_and_first) );
]

(** Helper function to assist with pipeline order*)
let guess_word word level = Level.guess_word level word 

(** Look at game1.json to find the first and last levels. *)
let game1 = (Yojson.Basic.from_file ((Sys.getcwd())^"/extractor/Outputs/game1.json"))
let test_game = Game.make_game game1

let first_level = Level.make_level test_game 0
let last_level = Level.make_level test_game 19

let first_level_guessed_one = Level.guess_word first_level "TO"
let first_level_missed_one = Level.guess_word first_level "TESTING"
let first_level_guessed_bonus = Level.guess_word first_level "TWO"
let first_level_completed = first_level_guessed_one |> guess_word "TOW"
                            |> guess_word "TWO"
let first_used_one = Level.dec_hints first_level
let first_used_two = Level.dec_hints first_used_one

let last_level_guessed_one = Level.guess_word last_level "GO"
let last_level_missed_one = Level.guess_word last_level "TESTING"
let last_level_guessed_bonus = Level.guess_word last_level "GOSH"
let last_level_completed = last_level_guessed_one |> guess_word "GOT"
                            |> guess_word "HOG" |> guess_word "HOGS" 
                            |> guess_word "GHOST" |> guess_word "TO" 
                            |> guess_word "SO" |> guess_word "SHOT" 
                            |> guess_word "HOT" |> guess_word "HOST" 
                            |> guess_word "GOSH"


(** Helper functions to test Level module *)
let assert_list l1 l2 = 
    List.sort compare l1 = List.sort compare l2

let sort_string s = 
s |> String.to_seq |> List.of_seq |> List.sort compare |>
List.to_seq |> String.of_seq

let assert_string s1 s2 = 
    sort_string s1 = sort_string s2
    
let score_tests = [
    "Test get_score first_level" >:: (fun _ ->
    assert_equal  0 (Level.get_score first_level) );
    
    "Test get_score first_level_guessed_one" >:: (fun _ ->
    assert_equal  2 (Level.get_score first_level_guessed_one) );
    
    "Test get_score first_level_missed_one" >:: (fun _ ->
    assert_equal  0 (Level.get_score first_level_missed_one) );
    
    "Test get_score first_level_guessed_bonus" >:: (fun _ ->
    assert_equal  6 (Level.get_score first_level_guessed_bonus) );
    
    "Test get_score first_level_completed" >:: (fun _ ->
    assert_equal  11 (Level.get_score first_level_completed) );
    
    "Test get_score last_level" >:: (fun _ ->
    assert_equal  0 (Level.get_score last_level) );
    
    "Test get_score last_level_guessed_one" >:: (fun _ ->
    assert_equal  2 (Level.get_score last_level_guessed_one) );
    
    "Test get_score last_level_missed_one" >:: (fun _ ->
    assert_equal  0 (Level.get_score last_level_missed_one) );
    
    "Test get_score last_level_guessed_bonus" >:: (fun _ ->
    assert_equal  8 (Level.get_score last_level_guessed_bonus) );
    
    "Test get_score last_level_completed" >:: (fun _ ->
    assert_equal  40 (Level.get_score last_level_completed) );

]
let found_words_tests = [
    "Test get_found_words first_level" >:: (fun _ ->
    assert_equal  [] (Level.get_found_words first_level) );
    
    "Test get_found_words first_level_guessed_one" >:: (fun _ ->
    assert_equal  ["TO"] (Level.get_found_words first_level_guessed_one) );
    
    "Test get_found_words first_level_missed_one" >:: (fun _ ->
    assert_equal  [] (Level.get_found_words first_level_missed_one) );
    
    "Test get_found_words first_level_guessed_bonus" >:: (fun _ ->
    assert_equal  ["TWO"] (Level.get_found_words first_level_guessed_bonus) );
    
    "Test get_found_words first_level_completed" >:: (fun _ ->
    let words = ["TWO";"TOW";"TO"] in 
    let found = Level.get_found_words first_level_completed in 
    assert_equal  true (assert_list  words found) );
    
    "Test get_found_words last_level" >:: (fun _ ->
    assert_equal  [] (Level.get_found_words last_level) );
    
    "Test get_found_words last_level_guessed_one" >:: (fun _ ->
    assert_equal ["GO"] (Level.get_found_words last_level_guessed_one) );
    
    "Test get_found_words last_level_missed_one" >:: (fun _ ->
    assert_equal  [] (Level.get_found_words last_level_missed_one) );
    
    "Test get_found_words last_level_guessed_bonus" >:: (fun _ ->
    assert_equal  ["GOSH"] (Level.get_found_words last_level_guessed_bonus) );
    
    "Test get_found_words last_level_completed" >:: (fun _ ->
    let words = 
    ["GO";"GOT";"HOG";"HOGS";"GHOST";"TO";"SO";"SHOT";"HOT";"HOST";"GOSH"] in 
    let found = Level.get_found_words last_level_completed in 
    assert_equal true (assert_list  words found) );
]
let words_bonus_and_letters_tests = [
    "Test get_words first_level" >:: (fun _ ->
    let all_words = ["TOW";"TO"] in
    let words = Level.get_words first_level in 
    assert_equal  true (assert_list  all_words words) );
    
    "Test get_words last_level" >:: (fun _ ->
    let all_words = 
    ["GO";"GOT";"HOG";"HOGS";"GHOST";"TO";"SO";"SHOT";"HOT";"HOST"] in 
    let words = Level.get_words last_level in 
    assert_equal  true (assert_list  all_words words) );
    
    "Test get_bonus first_level" >:: (fun _ ->
    assert_equal true  (assert_string "TWO" (Level.get_bonus first_level) ));
    
    "Test get_bonus last_level" >:: (fun _ ->
    assert_equal  true (assert_string "GOSH" (Level.get_bonus last_level)) );
    
    "Test get_letters first_level" >:: (fun _ ->
    assert_equal  true (assert_string "TOW" (Level.get_letters first_level)) );
    
    "Test get_letters last_level" >:: (fun _ ->
    assert_equal  true (assert_string "GHOST" (Level.get_letters last_level)) );
]
let check_level_tests = [
    "Test check_level first_level" >:: (fun _ ->
    assert_equal  false (Level.check_level first_level) );
    
    "Test check_level last_level" >:: (fun _ ->
    assert_equal  false (Level.check_level last_level));
    
    "Test check_level first_level_completed" >:: (fun _ ->
    assert_equal  true (Level.check_level first_level_completed) );
    
    "Test check_level last_level_completed" >:: (fun _ ->
    assert_equal  true (Level.check_level last_level_completed) );
]
(**NOt Done *)
let hint_tests = [
    "Test Using a Hint" >:: (fun _ ->
    assert_equal  'T' (Level.use_hint first_level) );

    "Test Using a Hint Last level" >:: (fun _ ->
    assert_equal  true (List.mem (Level.use_hint last_level) ['H';'G';'T';'S']));

    "Test Start with 3 hints" >:: (fun _ ->
    assert_equal 3 (Level.get_hints_remaining first_level));

    "Test Start with 3 hints final level" >:: (fun _ ->
    assert_equal 3 (Level.get_hints_remaining last_level));

    "Test Decrimenting Hints" >:: (fun _ ->
    assert_equal 2 (Level.get_hints_remaining first_used_one));

    "Test Decrimenting Hints Part 2" >:: (fun _ ->
    assert_equal 1 (Level.get_hints_remaining first_used_two));

    "Test Use Hint All words found" >:: (fun _ ->
    assert_equal ' ' (Level.use_hint first_level_completed));

    "Test Use Hint All words found Part 2" >:: (fun _ ->
    assert_equal ' ' (Level.use_hint last_level_completed));
]

let level_tests = List.flatten [score_tests;found_words_tests;
words_bonus_and_letters_tests; check_level_tests; hint_tests]

let tests = List.flatten [game_tests; level_tests]

let suite = "suite" >::: tests

let () = run_test_tt_main suite