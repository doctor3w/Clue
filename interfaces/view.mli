open Data

(* Displays the provided error message. *)
val display_error : string -> ()

(* Displays a description of who's turn it is. *)
val display_turn : game -> ()

(* Prompts the user for whether he rolls dice or not. *)
val prompt_move : move list -> string

(* Displays a description of whether the agent elected to Roll or Passage. *)
val display_move : move -> ()

(* Prompts the user for his. *)
val prompt_movement : (string * loc) list -> string

(* Displays the movement the agen took on his turn *)
val display_movement : (string * loc) -> ()

(* Prompts the user for a guess.
 * Takes in the current location (must be a room) and
 * a bool which says whether or not it is the final accusation.
 * returns a string of the user's response. *)
val prompt_guess : loc -> bool -> string

(* Displays a guess (by the user or AI). *)
val display_guess : guess -> ()

(* Prompts the user for a card to show.
 * Can be any card from the provided hand, and must be in the guess.
 * Can be none if there is no card to show. *)
val prompt_answer : hand -> guess -> string

(* Displays the card shown to the human agent and by whom.
 * If None, the user is told just told who showed a card, but not the details
 * of the card. *)
val display_answer : card option -> string -> ()

(* Displays end game victory text. *)
val display_victory : () -> ()