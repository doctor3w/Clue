(* open Agent *)

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
type card = Suspect of string | Weapon of string | Room_c of string

module OrderedCard = struct
  type t = card
  let compare c1 c2 =
    match c1, c2 with
    | Room_c(s1), Room_c(s2) -> Pervasives.compare s1 s2
    | _, Room_c(_) -> -1
    | Room_c(_), _ -> 1
    | Weapon(s1), Weapon(s2) -> Pervasives.compare s1 s2
    | _, Weapon(_) -> -1
    | Weapon(_), _ -> 1
    | Suspect(s1), Suspect(s2) -> Pervasives.compare s1 s2
    | _, Suspect(_) -> -1
    | Suspect(_), _ -> 1
end

(* The hand is just a card list *)
type hand = card list

(* A guess is three cards, one of each type.
 * They must be in the order Suspect * Weapon * Room *)
type guess = card * card * card

(* Info represents what we know of a specific card *)
type info = Mine | ShownBy of string | Unknown | Envelope

(* [sheet] is a map from a [card] to [info] *)
type sheet

(* [agent] is a type representing what type of agent, specifically which
 * module should be used to call the prompts on *)
type agent = Human_t | DumbAI_t | SmartAI_t

(* [move] is a type of move that you choose to do at the beginning
 * of a turn. *)
type move = Roll | Passage of loc

(* [listens] represents listening data for responsive AI *)
type listens

(* [player] represents user info, whether it be AI or human, they contain the
 * same type of information. *)
type player = {suspect: string;
               hand: hand;
               curr_loc: loc;
               sheet: sheet;
               agent: agent;
               is_out: bool}

type public = {curr_player: string;
               board: loc;
               acc_room: string;
               listen_data: listens}

(* [game] is the current state of the game. The players represent all agents
 * of the game, curr_player is the current players turn, board is a
 * representation of the game board used for drawing, envelope is the winning
 * guess. *)
type game = {players: player list;
             public: public;
             envelope: guess;
             ai_only: bool}
(* type game = {players: player list;
             curr_player: string;
             board: loc;
             envelope: guess;
             acc_room: string;
             listen_data: listens} *)

