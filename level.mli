(** Representation of the current level

    Stores all of the information for the current level that the player is on.
    Handles the computation of updating the Game State as the player guesses
    words. 
    I.e. Stores which words the player has found as well as their score.
*)

(** The abstract type of values representing a level in the game. *)
type t

(** [make_level game curr_level hints] is the Level.t object representing the
    [curr_level] level of the game [game].*)
val make_level: Game.t -> int -> t 

(** [quess_word level word] is [level] with [word] revealed and points
    awarded if [word] is an accepted word or a bonus word, [level] otherwise.*)
val guess_word: t -> string -> t 

(** [get_score level] is the player's current score.*)
val get_score: t -> int

(** [get_words level] is a list of all of the words in the [level],
     does not include the bonus word*)
val get_words: t -> string list 

(** [get_found_words level] is a list of all of the words in the
    [level] that the player has found.*)
val get_found_words: t -> string list 

(** [get_bonus level] is the bonus word of the [level]*)
val get_bonus: t -> string

(** [get_letters level] is the string of letters of the [level] *)
val get_letters: t -> string

(** [check_level level] is true when level is completed *)
val check_level: t -> bool

(** [use_hint level] gives the first letter of an undiscovered word, or ' ',
    if there's no undiscovered word *)
val use_hint: t -> char

(** [next_level game level] updates [game] with the statistics of the 
    player's score, number of words found, and levels completed, and then
    creates a new returns the next level of the game.*)
val next_level: Game.t -> t -> Game.t * t 

(** [end_level level game] returns [game] updated with the statistics of the 
    player's score, number of words found, and levels completed.*)
val end_level: Game.t -> t -> Game.t

(** [dec_hints t] gives t with decremented [hints_remaining]. *)
val dec_hints: t -> t

(** [get_hints_remaining t] is the number of hints available. *)
val get_hints_remaining: t -> int