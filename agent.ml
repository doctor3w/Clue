open Data

module Display = View

(* [answer_move] gets the type of movement the agent wants to perform,
 * so either roll the dice or take a secret passage if possible  *)
let answer_move pl pub moves = match pl.agent with
  | DumbAI_t -> DumbAI.answer_move pl pub moves
  | SmartAI_t -> SmartAI.answer_move pl pub moves
  | Human_t -> Human.answer_move pl pub moves
  | ResponsiveAI_t -> Responsive.answer_move pl pub moves

(* [get_movement] passes in a list of locations that could be moved to,
 * and returns the agent's choice of movement *)
let get_movement pl pub move_ops roll pm : movement = match pl.agent with
  | DumbAI_t -> DumbAI.get_movement pl pub move_ops
  | SmartAI_t -> SmartAI.get_movement pl pub move_ops
  | Human_t -> Human.get_movement pl pub move_ops roll pm
  | ResponsiveAI_t -> Responsive.get_movement pl pub move_ops

(* [get_geuss] takes in a game sheet and the current location and returns
 * a card list of 1 room, 1 suspect, and 1 weapon that the agent guesses. *)
let get_guess pl pub = match pl.agent with
  | DumbAI_t -> DumbAI.get_guess pl pub
  | SmartAI_t -> SmartAI.get_guess pl pub
  | Human_t -> Human.get_guess pl pub
  | ResponsiveAI_t -> Responsive.get_guess pl pub

(* [get_accusation] takes in a game sheet and the current location and returns
 * a card list of 1 room, 1 suspect, and 1 weapon that the agent thinks is
 * inside the envelope. *)
let get_accusation pl pub = match pl.agent with
  | DumbAI_t -> DumbAI.get_accusation pl pub
  | SmartAI_t -> SmartAI.get_accusation pl pub
  | Human_t -> Human.get_accusation pl pub
  | ResponsiveAI_t -> Responsive.get_accusation pl pub

(* [get_answer] takes in a hand and the current guess and returns Some card
 * if a card from the hand and also in the list can be shown. Returns None
 * if no card can be shown. *)
let get_answer pl pub guess = match pl.agent with
  | DumbAI_t -> DumbAI.get_answer pl pub guess
  | SmartAI_t -> SmartAI.get_answer pl pub guess
  | Human_t -> Human.get_answer pl pub guess
  | ResponsiveAI_t -> Responsive.get_answer pl pub guess

(* Turns card data from unknown to envelope in sheet. Only if unknown is
 * the data changed. *)
let unk_to_env card sheet =
  let data = CardMap.find card sheet in
  let data' = match data.card_info with
    | Unknown -> {data with card_info=Envelope}
    | _ -> data in
  CardMap.add card data' sheet

(* Finds all the unknown cards in the list [cards] *)
let rec poe_finder cards unks sheet =
  match cards with
  | [] -> unks
  | h::t -> extract_card_info h t unks sheet
(* extracts the card and adds it if it's Unknown. If an envelope card
 * is found, then there are no unknown's technically. *)
and extract_card_info h t unks sheet =
  let data = CardMap.find h sheet in
  match data.card_info with
  | ShownBy _ | Mine _ -> poe_finder t unks sheet
  | Unknown -> poe_finder t (h::unks) sheet
  | Envelope -> []

(* Updates the sheet if unks only has one unknown card. *)
let poe_update unks sheet =
  if List.length unks = 1 then unk_to_env (List.hd unks) sheet
  else sheet

(* Based on the pl_type, the function will go through each type of card
 * and use the process of elimination to deduce if there's one final card
 * then it should be made the envelope card in the sheet. *)
let process_of_elimination sheet pub pl_typ =
  let (ss, ws, rs) = pub.deck in
  match pl_typ with
  | DumbAI_t -> sheet (* He's five and doesn't know POE *)
  | _ -> (* process of elimination *)
    let s_unks = poe_finder ss [] sheet in
    let w_unks = poe_finder ws [] sheet in
    let r_unks = poe_finder rs [] sheet in
    poe_update s_unks sheet |> poe_update w_unks |> poe_update r_unks

(* Displays the sheet if the agent is human and the type is GUI.
 * Meant for updating the GUI. *)
let show_sheet sheet agent =
  if agent = Human_t && !view_type = GUI then
    Gui.show_sheet sheet
  else ()

(* [show_card pl pu c g] updates the players sheet based on the new card seen
 * and the guess. If card is None, then that means no one had cards in the
 * guess and needs to be updated accordingly. Also needs to use process of
 * elimination for certain AIs *)
let show_card pl pub answer (s, w, r) =
  if pl.agent = ResponsiveAI_t then
    Responsive.show_card pl pub answer (s,w,r)
  else
    match answer with
    | None ->
      let () = Display.display_answer None "" (pl.agent = Human_t) in
      let sheet' = unk_to_env s pl.sheet |> unk_to_env w |> unk_to_env r in
      show_sheet sheet' pl.agent;
      {pl with sheet = sheet'}
    | Some(sus, card) ->
      let () = Display.display_answer (Some card) sus (pl.agent = Human_t) in
      let data = CardMap.find card pl.sheet in
      let data' = {data with card_info= ShownBy(sus)} in
      let sheet' = CardMap.add card data' pl.sheet in
      let sheet'' = process_of_elimination sheet' pub pl.agent in
      show_sheet sheet'' pl.agent;
      {pl with sheet = sheet''}

(* Adds [sus] to [pl]'s list of 'shown to people' for a specific card [card] *)
let show_person pl card sus =
  let data = CardMap.find card pl.sheet in
  let card_info = match data.card_info with
    | Mine l -> Mine (sus::l)
    | x -> x in
  let data' = {data with card_info=card_info} in
  let sheet' = CardMap.add card data' pl.sheet in
  show_sheet sheet' pl.agent;
  {pl with sheet = sheet'}


let first_take_note pl pub = match pl.agent with
  | ResponsiveAI_t -> Responsive.first_take_note pl pub
  | _ -> pl

(* [take_notes pl pu] updates the ResponsiveAIs sheet based on the listen data
 * in public. *)
let take_notes pl pub current_guess suspect_option = match pl.agent with
  | DumbAI_t -> DumbAI.take_notes pl pub current_guess suspect_option
  | SmartAI_t -> SmartAI.take_notes pl pub current_guess suspect_option
  | Human_t -> Human.take_notes pl pub current_guess suspect_option
  | ResponsiveAI_t -> Responsive.take_notes pl pub current_guess suspect_option

