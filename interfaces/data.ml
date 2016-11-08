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

type game = {players: player list; curr_player: string; board: loc;
             envelope: guess}
