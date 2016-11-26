open Data

(* [answer_move] gets the type of movement the agent wants to perform,
 * so either roll the dice or take a secret passage if possible  *)
let answer_move player public  move_list : move = failwith "responsiveai answer_move"

(* [get_movement] passes in a list of locations that could be moved to,
 * and returns the agent's choice of movement *)
let get_movement player public move_option_list: loc= failiwith "responsiveai get_movement"

(* [get_guess] takes in a game sheet and the current location and returns
 * a card list of 1 room, 1 suspect, and 1 weapon that the agent guesses. *)
let get_guess player public :guess= failiwith "responsiveai get_guess"

(* [get_accusation] takes in a game sheet and the current location and returns
 * a card list of 1 room, 1 suspect, and 1 weapon that the agent thinks is
 * inside the envelope. *)
let get_accusation player public :guess= failiwith "responsiveai get_accusation"

(* [get_answer] takes in a hand and the current guess and returns Some card
 * if a card from the hand and also in the list can be shown. Returns None
 * if no card can be shown. *)
let get_answer player public guess : card option = failwith "responsiveai get_answer"

(* [take_notes pl pu] updates the ResponsiveAIs sheet based on the listen data
 * in public. *)
let take_notes player public :player = failwith "responsiveai take_notes"

