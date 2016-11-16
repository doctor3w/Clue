open Data
open Agent

(* Displays the provided error message. *)
let display_error e = failwith ""

(* Displays a description of who's turn it is. *)
let display_turn pub = failwith ""

(* Prompts the user for a file so that it can be imported into the Model *)
let prompt_filename () = failwith ""

(* Prompts the user for whether he rolls dice or not. *)
let prompt_move = failwith ""

(* Displays a description of whether the agent elected to Roll or Passage. *)
let display_move m = failwith ""

let display_dice_roll i = failwith ""

(* Prompts the user for his. *)
let prompt_movement = failwith ""

(* Displays the movement the agen took on his turn *)
let display_movement (s, l) = failwith ""

(* Prompts the user for a guess.
 * Takes in the current location (must be a room) and
 * a bool which says whether or not it is the final accusation.
 * returns a string of the user's response. *)
let prompt_guess = failwith ""

(* Displays a guess (by the user or AI). *)
let display_guess = failwith ""

(* Prompts the user for a card to show.
 * Can be any card from the provided hand, and must be in the guess.
 * Can be none if there is no card to show. *)
let prompt_answer = failwith ""

(* Displays the card shown to the human agent and by whom.
 * If None, the user is told just told who showed a card, but not the details
 * of the card. *)
let display_answer = failwith ""

(* Displays end game victory text. *)
let display_victory pl_name = failwith ""

(* Displays end game victory text. *)
let display_message s = print_endline s
