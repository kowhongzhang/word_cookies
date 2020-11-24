open Game 
open Level
open Addons
module Print = ANSITerminal

(** [print_start] prints the welcome message of the game*)
let print_start =
  Print.print_string[Print.cyan]
    ("Welcome to Word Cookies!
    You will be presented with a set of letters.
    Use those letters to make words, and try to find all the words in the level.
    The more words you find the more points you'll get.
    Try to find the bonus word in each level to get even more points!
    Just type in the word to guess.
    If you want a hint type hint;; if you want to quit type quit;;
    If you want to enter the level generator type generator;;
    How many words can you find!\n")

(** [blankify word] is a string of '_'s with length of [word]  *)
let blankify word = 
  String.make (String.length word) '-'

(** [mem l e] is true iff [e] is not in list [l] *)
let mem l e = 
  not (List.mem e l)

(** [get_not_found words found_words] is the list of words that have not been 
    found. *)
let get_not_found (words:string list) (found_words:string list) = 
  List.filter (mem found_words) words 

(** [compare_lengths] is 1 if length of [s1] is greater than [s2], 0 if equal,
    -1 if otherwise. *)
let compare_lengths s1 s2 = 
  Int.compare (String.length s1) (String.length s2)

(** [merge l1 l2] is a list of sorted and merged elements from [l1] and [l2]. *)
let merge (l1:'a list) (l2:'a list) = 
  List.sort compare_lengths (l1@l2)

(** [print_list lst] prints a string list with spaces separating each string.*)
let rec print_list lst = 
  match lst with 
  | [] -> ()
  | h::t -> print_string(h); print_char(' '); print_list t

(** [print_words words found_words] prints the list of all words in the game.
    If the word has yet to be found then it will be replaced with a string
    of - of equal length.*)
let print_words (words:string list) (found_words:string list) bonus =
  let not_found = get_not_found words found_words in 
  merge (List.map blankify not_found) (List.filter ((<>) bonus) found_words) |> print_list

(** [fix_bonus lv] is the bonus word of level if it has been found,
    bonus word replaced with blanks otherwise.*)
let fix_bonus lv = 
  let bonus_word = get_bonus lv in 
  if List.mem (bonus_word) (get_found_words lv) then bonus_word 
  else blankify bonus_word

(** [print_level lv] prints the information of lv to the screen. *)
let print_level lv = 
  let bonus = fix_bonus lv in 
  print_string("Letters: "); print_string(get_letters lv);print_endline("");
  print_string("Bonus Word: "); print_string(bonus); print_string("       ");
  print_string("Level Score: "); lv |> get_score |> print_int; print_endline("");
  print_string("Words: "); print_words (get_words lv) (get_found_words lv) (get_bonus lv);
  print_endline("")

(** [print_exit game level] updates [game] to reflect the increase in stats from
    completing [level] and thenprints all of the game statistics that the player
    has reached. I.e. the number of words guessed correctly, the player's current
    score, and the number of levels played.  *)
let print_exit game level= 
  let finished_game = end_level game level in 
  let lvs = get_levels_completed finished_game in 
  let sc = get_total_score finished_game in 
  let wrds = get_num_words finished_game in 
  Printf.printf "Levels Played: %d Words Found: %d Total Score: %d\n" lvs wrds sc

(** [handle_hint lv] handles the case where the user asks for it on [lv] *)
let handle_hint lv = 
   if (get_hints_remaining lv = 0) then 
      (Print.print_string[Print.red]("No hints remaining for this level!\n\n"); lv)
    else
      let first_letter = use_hint lv in
      let first_letter_string = String.make 1 first_letter in
      Print.print_string[Print.blue] 
        ("One of the missing words start with '"^first_letter_string^"'!\n\n");
      dec_hints lv

(** [handle_input game lv] takes in input from the player and updates the level
    information accordingly. It also prints a helpful message based on whether
    the player got a word correct.*)
let rec handle_input (game, lv) = 
  let input = (read_line ()) |> String.trim |> String.uppercase_ascii in
  if input = "QUIT;;" then (print_exit game lv; exit 0)
  else if input = "HINT;;" then (game, handle_hint lv)
  else if input = "COOKIES;;" then (Easter_Eggs.cookies (); (game,lv))
  else if input = "WORD;;" then (Easter_Eggs.word (); (game,lv))
  else if input = "WORD COOKIES;;" then (Easter_Eggs.word_cookies ();(game,lv))
  else if input = "GENERATOR;;" then ((Level_Generator.create_level game), lv)
  else let output_lv = guess_word lv input in
    if (get_score lv = get_score output_lv) 
    then (Print.print_string[Print.red] "Invalid word!\n\n"; (game,output_lv);)
    else (Print.print_string[Print.green] "You got a word right!\n\n"; (game,output_lv))

(** [check_win game lv] is the next_level of [game] if every word has been
    discovered, [lv] otherwise *)
let check_win (game, lv) = 
  if check_level lv then next_level game lv else (game,lv)

(** [play lv] plays the game on the level [lv]. It prints the level information,
    takes in input, updates the level information and repeats.*)
let rec play (game, lv) =
  print_level lv; 
  handle_input (game,lv) |> check_win |> play

(** [get_valid_game_number] is the number of the game file that the user
    chooses to play.*)
let rec get_valid_game_number () = 
  try (let k = read_int() in
      if k = 42 then k else 
       match k with 
       | x when (k <20 && k>(-1)) -> x
       | _ -> (Print.print_string[Print.red]("Error! Please try again.\n"); 
               get_valid_game_number ())) 
  with Failure(_) -> Print.print_string[Print.red]("Error! Please try again.\n");
    get_valid_game_number()

let run () = 
  print_start;
  Print.print_string[Print.green]("Please select a gamefile (0-19) 
  or select file 42 to get a blank file to use with the level generator\n");
  let k = get_valid_game_number () in
  let game = 
    (Sys.getcwd()^"/extractor/Outputs/game"^string_of_int k^".json") 
    |> Yojson.Basic.from_file |> make_game in 
  let level = make_level game 0 in 
  play (game, level) 

(** Call run to begin the game.*)
let () = run ()