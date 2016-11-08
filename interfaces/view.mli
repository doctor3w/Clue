open Data

(* Prompts the user for a file so that it can be imported into the Model *)
val prompt_filename : unit -> string

(* Prints a description of who's turn it is. *)
val print_turn : game -> unit

(* Prompts the user for whether he rolls dice or not. *)
val prompt_move : move list -> string

(* Prints a description of whether the player elected to Roll or Passage. *)
val print_move : move -> unit

(* Prompts the user for his. *)
val prompt_movement : (string * loc) list -> string

val print_movement : (string * loc) -> unit