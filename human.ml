open Data

module Display = Cli

exception Bad_input
exception Wrong_room

let normalize s = String.lowercase_ascii (String.trim s)

let eq_str s1 s2 = (normalize s1) = (normalize s2)

let contains_xs s xs =
  let cat = if List.length xs = 1 then List.hd xs
            else List.fold_left (fun acc el -> match acc with
                                               | "" -> el
                                               | a -> a^"\|"^el) "" xs in
  let r = Str.regexp cat in
  try ignore (Str.search_forward r s 0); true with Not_found -> false

let parse_move str =
  let norm = normalize str in
  if contains_xs norm ["roll";"dice"] then Roll
  else if contains_xs norm ["passage"] then Passage
  else raise Bad_input

(* [answer_move] gets the type of movement the agent wants to perform,
 * so either roll the dice or take a secret passage if possible  *)
let rec answer_move pl pub moves =
  try parse_move (Display.prompt_move moves) with
  | Bad_input ->
    Display.display_error "Please enter roll or passage";
    answer_move pl pub moves

let parse_movement str move_ops =
  let norm = normalize str in
  let f (s, l) =
    let ss = split (regexp "[ \t]+") (normalize s) in
    let ss_rev = List.rev ss in
    try List.hd ss_rev with _ -> failwith "Loc description was blank" in
  let rooms = List.map f move_ops in
  if contains_xs norm rooms then
    let selected = Str.matched_string norm in
    let norm_moves = List.map (fun (s,l) -> (normalize s, l)) move_ops in
    try List.assoc norm norm_moves with Not_found -> raise Bad_input
  else raise Bad_input

(* [get_movement] passes in a list of locations that could be moved to,
 * and returns the agent's choice of movement *)
let rec get_movement pl pub move_ops =
  try parse_movement (Display.prompt_movement move_ops) with
  | Bad_input ->
    Display.display_error "Please enter a valid location to move to.";
    get_movement pl pub move_ops

(* [card_norm_map card] takes a list of cards and normalizes the card names
 * and returns a map of the normalized cards to the original card. *)
let card_norm_map card =
  match card with
  | Room s -> ((normalize s), Room s)
  | Weapon s -> ((normalize s), Weapon s)
  | Suspect s -> ((normalize s), Suspect s)

(* [match_card str cards] checks if the string is contained in the mapping
 * of normalized strings to and then returns the card variant of the entered
 * string. *)
let match_card str cards =
  if contains_xs str (List.map (fun (k,v) -> k) cards) then
    let selected = Str.matched_string str in
    try List.assoc selected cards with Not_found -> raise Not_found
  else raise Bad_input

(* [parse_guess str (ss, ws, rs)] takes a string and deck and gets the cards
 * in the sring and puts it into a guess tuple. *)
let parse_guess str (ss, ws, rs) =
  let norm = normalize str in
  let ss_norm = List.map card_norm_map ss in
  let ws_norm = List.map card_norm_map ws in
  let rs_norm = List.map card_norm_map rs in
  (match_card norm ss_norm, match_card norm ws_norm, match_card norm rs_norm)

(* [handle_guess pl pub] parses the guess and then makes sure the room guess
 * is the room the player is currently in. *)
let handle_guess pl pub =
  let prompt = Display.prompt_guess pl.curr_loc false in
  let (s, w, r) = parse_guess prompt pub.deck in
  match r, pl.curr_loc.info with
  | Room r1, Room_Rect (r2, _) when eq_str r1 r2 -> (s, w, r)
  | _, _ -> raise Wrong_room

(* [get_guess] takes in a game sheet and the current location and returns
 * a card list of 1 room, 1 suspect, and 1 weapon that the agent guesses. *)
let rec get_guess pl pub =
  try handle_guess pl pub with
  | Bad_input ->
    Display.display_error "Please enter a valid guess (must be real cards).";
    get_guess pl pub
  | Wrong_room ->
    Display.display_error "The room must match the room you are in.";
    get_guess pl pub

(* [get_accusation] takes in a game sheet and the current location and returns
 * a card list of 1 room, 1 suspect, and 1 weapon that the agent thinks is
 * inside the envelope. *)
let rec get_accusation pl pub =
  try parse_guess (Display.prompt_guess pl.curr_loc true) pub.deck with
  | Bad_input ->
    Display.display_error
      "Please enter a valid accusation (must be real cards).";
    get_accusation pl pub

let parse_answer str hand (s, w, r) =
  let norm = normalize str in
  let hand_norm = List.map card_norm_map hand in
  let card = match_card str hand_norm in
  if card = s || card = w || card = r then card else raise Bad_input

(* [get_answer] takes in a hand and the current guess and returns Some card
 * if a card from the hand and also in the list can be shown. Returns None
 * if no card can be shown. *)
let rec get_answer pl pub guess =
  try parse_answer (Display.prompt_answer pl.hand guess) with
  | Bad_input ->
    Display.display_error "Please enter a valid card from your hand.";
    get_answer pl pub guess

(* [take_notes pl pu] updates the ResponsiveAIs sheet based on the listen data
 * in public. *)
let take_notes pl pub = pl
