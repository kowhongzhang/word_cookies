(** Interface provides function(s) that can run the game *)

(** [run ()] starts the game by asking the player which game file they would
    like to use, creating the Game.t record for the file, making a level from 
    that game and playing the game on that level. *)
val run : unit -> unit