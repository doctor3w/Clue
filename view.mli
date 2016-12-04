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

(* Displays the movement the agent took on its turn *)
val display_movement : loc * (string * bool) -> unit

(* Displays the relocation of suspect [string] to the Room loc upon a guess.
 * This method is not for moving a player after picking a location to go
 * to themselves. *)
val display_relocate : string -> loc -> unit

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

(* Displays that the player [string] could not answer with a card.
 * This is different from no one being able to show a card. *)
val display_no_answer : string -> unit

(* Displays end game victory text, string is who won. *)
val display_victory : string -> unit

(* Displays arbitrary text. *)
val display_message : string -> unit

(* Displays the sheet passed in *)
val show_sheet : sheet -> unit

(* Displays the hand passed in *)
val show_hand : hand -> unit

(* Prompts the user to continue the game. Allows a period for the
 * user to think. *)
val prompt_continue : unit -> unit

(* Prompts the user at the end of the game to exit. Mainly for GUI so it
 * doesn't quit as soon as the game ends. *)
val prompt_end_game : unit -> unit