(** Representation of static game data.

    This module represents the data stored in the levels.json file.
    It handles loaded the file into Game.t object which carries all of the data
    necessary to run the game
*)

(** The abstract representation of the Game *)
type t 

(** The abstract representation of a level in the game *)
type level

(** [make_game json] is the Game.t object that has the levels in [json] *)
val make_game: Yojson.Basic.t -> t

(** [pick_level game curr_level] is the [curr_level]'th [level] in [game]. *)
val pick_level: t -> int -> level 

(** [get_letters level] is the string of the letters in [level] *)
val get_letters: level -> string

(** [get_words level] is a list of the words in [level] *)
val get_words: level -> string list

(** [get_bonus_word level] is the bonus word in [level] *)
val get_bonus_word: level -> string

(** [update_game score words levels game] is [game] updated to reflect
    the increase in [score], the new number of [words] found, and the number 
    of [levels] compeleted. *)
val update_game: int -> int -> int -> t -> t

(** [get_levels_completed game] is the total number of levels the player has
    compeleted in [game] *)
val get_levels_completed: t -> int

(** [get_total_score game] is the player's overall score in [game] *)
val get_total_score: t -> int 

(** [get_num_words game] is the total number of worsd the player has found
    in [game] *)
val get_num_words: t -> int

(** [add_level ltrs words bonus game] is game with a level added that has
    letters [ltrs], words [words], and bonus word [bonus] *)
val add_level: string -> string list -> string -> t -> t