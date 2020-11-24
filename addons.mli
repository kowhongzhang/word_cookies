(** Addons contains the modules that are used to implement the various
    addons to the game. *)

(** A module that matches [Generatr] is suitable to use as a Level Generator *)
module type Generator = sig 
  
  (** [create_level [game]] takes in user input to create level and adds that 
      level to [game]*)
  val create_level: Game.t -> Game.t

end 
(** [Level_Generator] contains all of the necessary code to implement the level
    generator in the game. *)
module Level_Generator : Generator 

(** A module that matches [EasterEggs] is suitable to use for implementing
    all of the Easter Eggs in the game. *)
module type EasterEggs = sig 

  (** [word ()] prints a random word. *)
  val word: unit -> unit

  (** [cookies ()] prints a recipe for chocolate-chip cookies. *)
  val cookies: unit -> unit

  (** [word_cookies ()] prints a random word from the chocolate-chip cookie
      recipe *)
  val word_cookies: unit -> unit

end
(** [Easter_Eggs] contains all of the necessary code to implement the Easter Eggs 
    found in the game. *)
module Easter_Eggs: EasterEggs