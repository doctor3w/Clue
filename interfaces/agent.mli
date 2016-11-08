open View
open Model

(* [answer_move] gets the type of movement the agent wants to perform,
 * so either roll the dice or take a secret passage if possible  *)
val answer_move : move list -> move

(* [get_movement] passes in a list of locations that could be moved to,
 * and returns the agent's choice of movement *)
val get_movement : (string * loc) list -> loc

(* [get_geuss] takes in a game sheet and the current location and returns
 * a card list of 1 room, 1 suspect, and 1 weapon that the agent guesses. *)
val get_guess : sheet -> loc -> guess

(* [get_answer] takes in a hand and the current guess and returns Some card
 * if a card from the hand and also in the list can be shown. Returns None
 * if no card can be shown. *)
val get_answer : hand -> guess -> card option

