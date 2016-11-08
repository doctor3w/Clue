(**
 * [loc] respresents a location on the board, holding the name of the room
 * or the coordinates of the space. The list represents all the locations
 * accessible from that location.
 *)
type loc = Room of string * loc list | Space of (int * int) * loc list

(**
 * [card] respresents a board game card, which is one of three things.
 * The string is the name on that card.
 *)
type card = Suspect of string | Weapon of string | Room of string

(* The hand is just a card list *)
type hand = card list

type guess = card * card * card

(* Info represents what we know of a specific card *)
type info = Mine | ShownBy of string | Unknown | Envelope

(* [sheet] is a map from a [card] to [info] *)
type sheet

type agent = Human | DumbAI | SmartAI

type move = Roll | Passage

(* [player] represents user info, whether it be AI or human, they contain the
 * same type of information. *)
type player = {id: string;
               suspect: string;
               hand: hand;
               curr_loc: loc;
               sheet: sheet;
               agent: agent}

type game = {players: player list; curr_player: string; board: loc}

(* [import_board] takes in a filename of a game configuration file and
 * converts the file into a usable game model for stepping through. *)
val import_board : string -> model

(* [get_move_options] gets the options of Roll and Passage that the current
 * player can make. *)
val get_move_options : model -> move list

(* [get_movement_options] gets the options of the locations that the current
 * player can move to. These options also come with a description in one of
 * the following fashions:
 *        head towars [room name]
 *        go into [room name] *)
val get_movement_options : model -> (string * loc) list
