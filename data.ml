(* open Agent *)



(**
 * [loc] respresents a location on the board, holding the name of the room
 * or the coordinates of the space. The list represents all the locations
 * accessible from that location.
 *)
(*type loc = Room of string * loc list | Space of (int * int) * loc list*)

(**
 * [card] respresents a board game card, which is one of three things.
 * The string is the name on that card.
 *)
type card = Suspect of string | Weapon of string | Room of string

module OrderedCard = struct
  type t = card
  let compare c1 c2 =
    match c1, c2 with
    | Room(s1), Room(s2) -> Pervasives.compare s1 s2
    | _, Room(_) -> -1
    | Room(_), _ -> 1
    | Weapon(s1), Weapon(s2) -> Pervasives.compare s1 s2
    | _, Weapon(_) -> -1
    | Weapon(_), _ -> 1
    | Suspect(s1), Suspect(s2) -> Pervasives.compare s1 s2
end

module CardMap = Map.Make(OrderedCard)

(* The hand is just a card list *)
type hand = card list

(* A guess is three cards, one of each type.
 * They must be in the order Suspect * Weapon * Room *)
type guess = card * card * card
type deck = card list * card list * card list

(* Info represents what we know of a specific card *)
type card_info = Mine of string list | ShownBy of string | Unknown | Envelope
type note = No_Note
          | HumanNote of string
          | NotInHand of string list
          | MaybeInHand of string list

type sheet_data = {
  card_info: card_info;
  note: note
}
(* [sheet] is a map from a [card] to [sheet_data] *)
type sheet = sheet_data CardMap.t

(* [agent] is a type representing what type of agent, specifically which
 * module should be used to call the prompts on *)
type agent = Human_t | DumbAI_t | SmartAI_t | ResponsiveAI_t

(* [move] is a type of move that you choose to do at the beginning
 * of a turn. *)

type coord = int*int

module Coord = struct
  type t = coord
  let compare (r1,c1) (r2,c2) =
    if r1 < r2 then -1
    else if r1 > r2 then 1
    else if c1 < c2 then -1
    else if c1 > c2 then 1
    else 0
end

module CoordMap = Map.Make(Coord)
module StringMap = Map.Make(String)

type player_temp = {
  p_id:string; play_ord:int; start:int*int
}

type rect = int*int*int*int
(* rect is x0,x1.y0,y1*)

type room_temp = {
  r_id: string;
  rect: rect;
  passages: string list;
  exits: (int*int) list
}

(* x0, x1, y0, y1 *)

type coord_info = Space of (int*int) | Room_Rect of string * (int*int*int*int)

type loc =
{
  info: coord_info;
  edges: (int*int) list
}

type board =
{
  dim: int*int;
  loc_map: loc CoordMap.t;
  room_coords: coord StringMap.t
}

type move = Roll | Passage of loc

(* [listens] represents listening data for responsive AI *)
type listens = {cards_weight: (card * float) array;
                 mutable susp_known: bool;
                 mutable weap_known: bool;
                 mutable room_known: bool;}

(* [player] represents user info, whether it be AI or human, they contain the
 * same type of information. *)
type player = {suspect: string;
               hand: hand;
               curr_loc: loc;
<<<<<<< HEAD
               cards_weight: (card * float) array;
=======
>>>>>>> responsive
               sheet: sheet;
               agent: agent;
               is_out: bool;
               listen: listen}

type public = {curr_player: string;
               board: board;
               acc_room: string;
               deck: deck;
               player_order: string list;
               mutable current_guess: guess;
               (*listen_data: listens*)
               }

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

let game_init = {
  players = [];
  envelope = (Suspect "", Weapon "", Room "");
  public = {
    curr_player = "";
    acc_room = "";
    board = {
      dim = (-1,-1);
      loc_map = CoordMap.empty;
      room_coords = StringMap.empty
    };
    deck = ([],[],[]);
    player_order = []
  };
  ai_only = false
}

