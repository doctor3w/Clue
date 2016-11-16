open Data
open DumbAI

(* [answer_move] gets the type of movement the agent wants to perform,
 * so either roll the dice or take a secret passage if possible  *)
let answer_move pl pub moves = match pl.agent with
  | DumbAI_t -> DumbAI.answer_move pl pub moves
  | _ -> DumbAI.answer_move pl pub moves

(* [get_movement] passes in a list of locations that could be moved to,
 * and returns the agent's choice of movement *)
let get_movement pl pub move_ops = match pl.agent with
  | DumbAI_t -> DumbAI.get_movement pl pub move_ops
  | _ -> DumbAI.get_movement pl pub move_ops

(* [get_geuss] takes in a game sheet and the current location and returns
 * a card list of 1 room, 1 suspect, and 1 weapon that the agent guesses. *)
let get_guess pl pub = match pl.agent with
  | DumbAI_t -> DumbAI.get_guess pl pub
  | _ -> DumbAI.get_guess pl pub

(* [get_accusation] takes in a game sheet and the current location and returns
 * a card list of 1 room, 1 suspect, and 1 weapon that the agent thinks is
 * inside the envelope. *)
let get_accusation pl pub = match pl.agent with
  | DumbAI_t -> DumbAI.get_accusation pl pub
  | _ -> DumbAI.get_accusation pl pub

(* [get_answer] takes in a hand and the current guess and returns Some card
 * if a card from the hand and also in the list can be shown. Returns None
 * if no card can be shown. *)
let get_answer pl pub guess = match pl.agent with
  | DumbAI_t -> DumbAI.get_answer pl pub guess
  | _ -> DumbAI.get_answer pl pub guess
