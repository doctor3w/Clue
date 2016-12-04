open Data

(* [answer_move] gets the type of movement the agent wants to perform,
 * so either roll the dice or take a secret passage if possible  *)
val answer_move : player -> public -> move list -> move

(* [get_movement] passes in a list of locations that could be moved to,
 * and returns the agent's choice of movement *)
val get_movement : player -> public -> movement list -> int -> PathMap.t -> movement

(* [get_guess] takes in a game sheet and the current location and returns
 * a card list of 1 room, 1 suspect, and 1 weapon that the agent guesses. *)
val get_guess : player -> public -> guess

(* [get_accusation] takes in a game sheet and the current location and returns
 * a card list of 1 room, 1 suspect, and 1 weapon that the agent thinks is
 * inside the envelope. *)
val get_accusation : player -> public -> guess

(* [get_answer] takes in a hand and the current guess and returns Some card
 * if a card from the hand and also in the list can be shown. Returns None
 * if no card can be shown. *)
val get_answer : player -> public -> guess -> card option

(* [show_card pl pu c g] updates the players sheet based on the new card seen
 * and the guess. If card is None, then that means no one had cards in the
 * guess and needs to be updated accordingly. Also needs to use process of
 * elimination for certain AIs. The string is who showed's suspect ID. *)
val show_card : player -> public -> (string * card) option -> guess -> player

(* Adds [sus] to [pl]'s list of 'shown to people' for a specific card [card] *)
val show_person : player -> card -> string -> player

(* Same as take_notes but happens initially at the beginning of the game. *)
val first_take_note : player -> public -> player

(* [take_notes pl pu current_guess suspect_option] updates the ResponsiveAIs
 * listen structure and sheet. The guess is the current guess during the game
 * and the suspect_option a string option where if someone shows a card the name
 * of the suspect is passed in as Some (suspect) if no one shows a card, it is
    None *)
val take_notes : player -> public -> guess -> string option -> player

