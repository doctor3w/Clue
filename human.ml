open Data

module Display = View

(* Bad_input is raised when human input cannot be parsed. *)
exception Bad_input

(* Wrong_room is raised when the human guesses a room that they are not
 * currently in. *)
exception Wrong_room

(* Normalizes a string *)
let normalize s = String.lowercase_ascii (String.trim s)

(* Checks if two normalized strings are equal *)
let eq_str s1 s2 = (normalize s1) = (normalize s2)

(* [contains_xs s xs] checks if any xs appear in the string s *)
let contains_xs s xs =
  let f_or acc el =
    match acc with
    | "" -> el
    | a -> a^"\\|"^el in
  let cat =
    if List.length xs = 1 then List.hd xs
    else List.fold_left f_or "" xs in
  let r = Str.regexp cat in
  try ignore (Str.search_forward r s 0); true with Not_found -> false

(* check_sheet_or_hand parses the human text and prints the sheet or hand if
 * the string matches "sheet" or "hand" respectively. *)
let check_sheet_or_hand text pl =
  if contains_xs (normalize text) ["sheet"] then
    (Display.show_sheet pl.sheet; true)
  else if contains_xs (normalize text) ["hand"] then
    (Display.show_hand pl.hand; true)
  else false

(* [parse_move str] parses the movement input str and returns one of two
 * options. Will raise Bad_input on nothing. *)
let parse_move str moves =
  let norm = normalize str in
  let passages = List.filter (fun m -> not (m = Roll)) moves in
  let extr_loc l = match l.info with
    | Room_Rect (s, i) -> (normalize s, Passage l)
    | _ -> raise Bad_input in
  let fmap m = match m with
    | Passage l -> extr_loc l
    | _ -> raise Bad_input in
  let mappings = List.map fmap passages in
  if contains_xs norm ["roll";"dice"] then Roll
  else if contains_xs norm (List.map (fun (k,v) -> k) mappings) then
    let selected = Str.matched_string norm in
    try List.assoc selected mappings with _ -> raise Bad_input
  else raise Bad_input

(* [answer_move] gets the type of movement the agent wants to perform,
 * so either roll the dice or take a secret passage if possible  *)
let rec answer_move pl pub moves =
  let text = Display.prompt_move moves in
  try parse_move text moves with
  | Bad_input ->
    if check_sheet_or_hand text pl then ()
    else Display.display_error "Please enter roll or one of the passages";
    answer_move pl pub moves

(* [parse_movement str move_ops] parses the movement input str and sees
 * if it is one in the movement options. *)
let parse_movement str move_ops =
  let norm = normalize str in
  let rooms = List.map (fun (_, (s, _)) -> normalize s) move_ops in
  if contains_xs norm rooms then
    let selected = Str.matched_string norm in
    let norm_moves =
      List.map (fun (l, (s, b)) -> (normalize s, (l, (s, b)))) move_ops in
    try List.assoc selected norm_moves with Not_found -> raise Bad_input
  else raise Bad_input

(* [get_movement] passes in a list of locations that could be moved to,
 * and returns the agent's choice of movement *)
let rec get_movement pl pub move_ops roll pm =
  if !view_type = GUI then
    let l = Gui.prompt_movement pm pub.acc_room roll in
    let (s, b) = try List.assoc l move_ops with | _ -> ("nowhere", false)
    in (l, (s, b))
  else
    let text = Display.prompt_movement move_ops pub.acc_room in
    try parse_movement text move_ops with
    | Bad_input ->
      if check_sheet_or_hand text pl then ()
      else Display.display_error "Please enter a valid location to move to.";
      get_movement pl pub move_ops roll pm

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
let handle_guess text pl pub =
  let (s, w, r) = parse_guess text pub.deck in
  match r, pl.curr_loc.info with
  | Room r1, Room_Rect (r2, _) when eq_str r1 r2 -> (s, w, r)
  | _, _ -> raise Wrong_room

(* [get_guess] takes in a game sheet and the current location and returns
 * a card list of 1 room, 1 suspect, and 1 weapon that the agent guesses. *)
let rec get_guess pl pub =
  let text = Display.prompt_guess pl.curr_loc false in
  try handle_guess text pl pub with
  | Bad_input ->
    if check_sheet_or_hand text pl then ()
    else
      Display.display_error "Please enter a valid guess (must be real cards).";
    get_guess pl pub
  | Wrong_room ->
    if check_sheet_or_hand text pl then ()
    else Display.display_error "The room must match the room you are in.";
    get_guess pl pub

(* [get_accusation] takes in a game sheet and the current location and returns
 * a card list of 1 room, 1 suspect, and 1 weapon that the agent thinks is
 * inside the envelope. *)
let rec get_accusation pl pub =
  let text = Display.prompt_guess pl.curr_loc true in
  try parse_guess text pub.deck with
  | Bad_input ->
    if check_sheet_or_hand text pl then ()
    else
      Display.display_error
        "Please enter a valid accusation (must be real cards).";
    get_accusation pl pub

(* [parse_answer str hand guess] parses the answer input str and returns the
 * associated card from the hand. Also checks if it's part of the guess. *)
let parse_answer str hand (s, w, r) =
  let norm = normalize str in
  let hand_norm = List.map card_norm_map hand in
  let card = match_card norm hand_norm in
  if card = s || card = w || card = r then card else raise Bad_input

(* [get_answer] takes in a hand and the current guess and returns Some card
 * if a card from the hand and also in the list can be shown. Returns None
 * if no card can be shown. *)
let rec get_answer pl pub guess =
  let text = Display.prompt_answer pl.hand guess in
  try
    Some (parse_answer text pl.hand guess)
  with
  | Bad_input ->
    if check_sheet_or_hand text pl then ()
    else
      Display.display_error
        "Please enter a valid card from your hand that's in the guess.";
    get_answer pl pub guess

(* [take_notes pl pu] updates the ResponsiveAIs sheet based on the listen data
 * in public. *)
let take_notes pl pub current_guess suspect_option = pl
