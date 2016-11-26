open Data

(* Displays the provided error message. *)
val display_error : string -> unit

(* Displays a description of who's turn it is. *)
val display_turn : public -> unit

(* Prompts the user for a file so that it can be imported into the Model *)
val prompt_filename : unit -> string

(* Prompts the user for whether he rolls dice or not. *)
val prompt_move : move list -> string

(* Displays a description of what the agent rolled. *)
val display_dice_roll : int -> unit

(* Displays a description of whether the agent elected to Roll or Passage. *)
val display_move : move -> unit

(* Prompts the user for the room they would like to go to.
 * [loc * (string * bool)] the location and whether or not room [string] is
 * accessible. The second string parameter is the acc_room name. *)
val prompt_movement : (loc * (string * bool)) list -> string -> string

(* Displays the movement the agen took on his turn *)
val display_movement : (string * bool) -> unit

(* Prompts the user for a guess.
 * Takes in the current location (must be a room) and
 * a bool which says whether or not it is the final accusation.
 * returns a string of the user's response. *)
val prompt_guess : loc -> bool -> string

(* Displays a guess (by the user or AI). *)
val display_guess : guess -> unit

(* Prompts the user for a card to show.
 * Can be any card from the provided hand, and must be in the guess.
 * Can be none if there is no card to show. *)
val prompt_answer : hand -> guess -> string

(* Displays the card shown to the human agent and by whom.
 * If None, no card could be shown. If false, the user is not shown the
 * details of the card. *)
val display_answer : card option -> string -> bool -> unit

(* Displays end game victory text, string is who won. *)
val display_victory : string -> unit

(* Displays arbitrary text. *)
val display_message : string -> unit

val show_sheet : sheet -> unit

val show_hand : hand -> unit