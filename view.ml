open Data

(* returns f () after a delay of [secs] seconds. No delay if testing *)
let after_delay (f: unit -> 'a) (secs:float) : 'a =
	let b = if !testing then true else (Thread.delay secs; true) in
	match b with
	| true -> f ()
	| false -> failwith ("Can never be false: " ^ Pervasives.__LOC__)

(* Displays the provided error message stored in [e] in color red. *)
let display_error (e:string): unit =
	match !view_type with
	| CLI -> Cli.display_error e
	| GUI -> Gui.display_error e

(* Displays a description of who's turn it is.
   The format of print statment is "This is [player]'s turn",
   where the information about the [player] is stored in [pub] *)
let display_turn (pub:public): unit =
	match !view_type with
	| CLI -> Cli.display_turn pub
	| GUI -> Gui.display_turn pub

(* Prompts the user for a file so that it can be imported into the Model *)
let prompt_filename () : string =
	match !view_type with
	| CLI | GUI -> Cli.prompt_filename ()

(* Prompts the user for whether he rolls dice or not.
	[prompt_move] takes [l], which is a move list and prints out all the
	possible moves the player could take. All of the option are in color peach *)
let prompt_move moves =
	match !view_type with
	| CLI -> Cli.prompt_move moves
	| GUI -> Gui.prompt_move moves

(* Displays a description of whether the agent elected to Roll or Passage. *)
let display_move (m:move) : unit =
	match !view_type with
	| CLI -> Cli.display_move m
	| GUI -> Gui.display_move m

(* Displays a description of what the agent rolled. *)
let display_dice_roll i =
	match !view_type with
	| CLI -> Cli.display_dice_roll i
	| GUI -> Gui.display_dice_roll i

let prompt_movement move_ops acc_room =
	match !view_type with
	| CLI | GUI -> Cli.prompt_movement move_ops acc_room
	(* Despite GUI having a prompt movement, DO NOT USE IT HERE. *)

(* Displays the movement the agent took on its turn *)
let display_movement (l, (str, b)) =
	match !view_type with
	| CLI -> Cli.display_movement (l, (str, b))
	| GUI -> Gui.display_movement (l, (str, b))

let display_relocate who loc =
	match !view_type with
	| CLI -> Cli.display_relocate who loc
	| GUI -> Gui.display_relocate who loc

(* Prompts the user for a guess.
 * Takes in the current location (must be a room) and
 * a bool which says whether or not it is the final accusation.
 * returns a string of the user's response. *)
let prompt_guess loc b =
	match !view_type with
	| CLI -> Cli.prompt_guess loc b
	| GUI -> Gui.prompt_guess loc b

(* Displays a guess (by the user or AI). *)
let display_guess g =
	match !view_type with
	| CLI -> Cli.display_guess g
	| GUI -> Gui.display_guess g

(* Prompts the user for a card to show.
 * Can be any card from the provided hand, and must be in the guess.
 * Cannot be none, only call when a card they have a card in the guess. *)
let prompt_answer hand guess =
	match !view_type with
	| CLI -> Cli.prompt_answer hand guess
	| GUI -> Gui.prompt_answer hand guess

(* Displays when a person cannot show a card from their hand *)
let display_no_answer name =
	match !view_type with
	| CLI -> Cli.display_no_answer name
	| GUI -> Gui.display_no_answer name

(* Displays the card shown to the human agent and by whom.
 * If None, no card could be shown. If false, the user is not shown the
 * details of the card. *)
let display_answer (card_opt:card option) str b : unit =
	match !view_type with
	| CLI -> Cli.display_answer card_opt str b
	| GUI -> Gui.display_answer card_opt str b

(* Displays end game victory text. *)
let display_victory pl_name =
	match !view_type with
	| CLI -> Cli.display_victory pl_name
	| GUI -> Gui.display_victory pl_name

(* Displays any message *)
let display_message s =
	match !view_type with
	| CLI -> Cli.display_message s
	| GUI -> Gui.display_message s

(* Methods for displaying the sheet *)
let show_sheet sheet =
	match !view_type with
	| CLI | GUI -> Cli.show_sheet sheet

let show_hand hand =
	match !view_type with
	| CLI | GUI -> Cli.show_hand hand

let prompt_continue () : unit =
	if !testing then () else
	match !view_type with
	| CLI -> Cli.prompt_continue ()
	| GUI -> Gui.prompt_continue ()

let prompt_end_game () : unit =
	match !view_type with
	| CLI -> Cli.prompt_end_game ()
	| GUI -> Gui.prompt_end_game ()