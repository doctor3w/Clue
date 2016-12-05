open Data
open Model

(* returns a random element from the list [lst] *)
let rand_from_lst lst =
  let len = List.length lst in
  if len = 0 then failwith "no lst"
  else if len = 1 then List.hd lst
  else List.nth lst (Random.int len)

(* true if the player [me] knows the suspect *)
let knows_sus me =
  let pairs = CardMap.bindings me.sheet in
  let f acc (c, i) = match (c, i.card_info) with
                 | (Suspect _, Envelope) -> acc || true
                 | _ -> acc in
  List.fold_left f false pairs

(* true if the player [me] knows the weapon *)
let knows_weap me =
  let pairs = CardMap.bindings me.sheet in
  let f acc (c, i) = match (c, i.card_info) with
                 | (Weapon _, Envelope) -> acc || true
                 | _ -> acc in
  List.fold_left f false pairs

(* true if the player [me] knows the room *)
let knows_room me =
  let pairs = CardMap.bindings me.sheet in
  let f acc (c, i) = match (c, i.card_info) with
                 | (Room _, Envelope) -> acc || true
                 | _ -> acc in
  List.fold_left f false pairs

(* returns true if the location corresponds with a card marked as
 * Mine in [me].sheet *)
let is_my_room me loc =
  let f c = match (CardMap.find c me.sheet).card_info with
            | Mine _ -> true
            | _ -> false in
  match loc.info with
  | Room_Rect (s, _) -> f (Room s)
  | _ -> false

(* returns true if the location corresponds with a card marked as
 * Envelope in [me].sheet *)
let is_env_room me loc =
  let f c = match (CardMap.find c me.sheet).card_info with
            | Envelope -> true
            | _ -> false in
  match loc.info with
  | Room_Rect (s, _) -> f (Room s)
  | _ -> false

(* returns true if the location corresponds with a card marked as
 * Unknown in [me].sheet *)
let is_unknown_room me loc =
  let f c = match (CardMap.find c me.sheet).card_info with
            | Unknown -> true
            | _ -> false in
  match loc.info with
  | Room_Rect (s, _) -> f (Room s)
  | _ -> false

(* true if card [c] is marked as Mine in [me].sheet *)
let is_my_card me c =
  match (CardMap.find c me.sheet).card_info with
  | Mine _ -> true
  | _ -> false

(* true if card [c] is marked as Unknown in [me].sheet *)
let is_unknown_card me c =
  match (CardMap.find c me.sheet).card_info with
  | Unknown -> true
  | _ -> false

(* true if card [c] is marked as Envelope in [me].sheet *)
let is_env_card me c =
  match (CardMap.find c me.sheet).card_info with
  | Envelope -> true
  | _ -> false

(* [answer_move] gets the type of movement the agent wants to perform,
 * so either roll the dice or take a secret passage if possible  *)
let answer_move  (me:player) public (passages: move list) : move =
  let knows_room = knows_room me in
  let fold f acc pass =
    match pass with
    | Passage r when f me r -> (Passage r)::acc
    | _ -> acc in
  if knows_room then Roll
  else
    let unknowns = List.fold_left (fold is_unknown_room) [] passages in
    if List.length unknowns > 0 then rand_from_lst unknowns else Roll

(* true if s is the acc_room, takes in an element from [movelst] below *)
let is_acc_room public (_, (s, _)) = s = public.acc_room

(* [get_movement] passes in a list of locations that could be moved to,
 * and returns the agent's choice of movement *)
let get_movement (me:player) public (movelst:movement list) =
  let sus_env = knows_sus me in
  let weap_env = knows_weap me in
  let room_env = knows_room me in
  if sus_env && weap_env && room_env
  then
    match List.filter (is_acc_room public) movelst with
    | [(l, (s, b))] -> (l, (s, b))
    | _ -> failwith "can't find accusation room"
  else
    let b = (fun x -> not (is_acc_room public x)) in
    let movelst' = List.filter b movelst in
    if room_env then
      let f acc el = match el with
      | (l, (s, true)) when is_my_card me (Room s) -> (l, (s, true))::acc
      | _ -> acc in
      let g acc el = match el with
      | (l, (s, true)) when is_env_card me (Room s) -> (l, (s, true))::acc
      | _ -> acc in
      let h acc el = match el with
      | (l, (s, false)) when is_my_card me (Room s) -> (l, (s, false))::acc
      | _ -> acc in
      let p acc el = match el with
      | (l, (s, false)) when is_env_card me (Room s) -> (l, (s, false))::acc
      | _ -> acc in
      let fs = List.fold_left f [] movelst' in
      if List.length fs > 0 then rand_from_lst fs
      else let gs = List.fold_left g [] movelst' in
      if List.length gs > 0 then rand_from_lst gs
      else let hs = List.fold_left h [] movelst' in
      if List.length hs > 0 then rand_from_lst hs
      else let ps = List.fold_left p [] movelst' in
      if List.length ps > 0 then rand_from_lst ps
      else if List.length movelst' > 0 then rand_from_lst movelst'
      else failwith ("impossible in " ^ Pervasives.__LOC__)
    else
      let f' acc el = match el with
      | (l, (s, true)) when is_unknown_card me (Room s) -> (l, (s, true))::acc
      | _ -> acc in
      let h' acc el = match el with
      | (l, (s, false)) when is_unknown_card me (Room s)-> (l, (s, false))::acc
      | _ -> acc in
      let fs = List.fold_left f' [] movelst' in
      if List.length fs > 0 then rand_from_lst fs
      else let hs = List.fold_left h' [] movelst' in
      if List.length hs > 0 then rand_from_lst hs
      else if List.length movelst' > 0 then rand_from_lst movelst'
      else failwith ("impossible in " ^ Pervasives.__LOC__)

(* gets the cards in the sheet with a certain info *)
let get_cards_with_info info sheet =
  let l = CardMap.filter (fun _ data -> (data.card_info = info)) sheet in
  List.map (fun (c,_) -> c) (CardMap.bindings l)

(* Makes a Room card out of a room location *)
let get_card_for_loc l = match l.info with
  | Room_Rect (s, i) -> Room s
  | _ -> failwith "Not a room"

(* [get_guess] takes in a game sheet and the current location and returns
 * a card list of 1 room, 1 suspect, and 1 weapon that the agent guesses. *)
let get_guess pl public : guess =
  let r = get_card_for_loc pl.curr_loc in
  let unks = get_cards_with_info Unknown pl.sheet in
  let envs = get_cards_with_info Envelope pl.sheet in
  let mins = pl.hand in
  let s_only c = match c with Suspect s -> true | _ -> false in
  let w_only c = match c with Weapon s -> true | _ -> false in
  let my_s = List.filter s_only mins in
  let my_w = List.filter w_only mins in
  let env_s = List.filter s_only envs in
  let env_w = List.filter w_only envs in
  let unks_s = List.filter s_only unks in
  let unks_w = List.filter w_only unks in
  let s =
    if knows_sus pl then
      try List.hd my_s with _ ->
      try List.hd env_s with _ ->
      failwith "Known but not known"
    else
      if List.length unks_s = 0 then failwith "No card to guess"
      else List.nth unks_s (Random.int (List.length unks_s)) in
  let w =
    if knows_weap pl then
      try List.hd my_w with _ ->
      try List.hd env_w with _ ->
      failwith "Known but not known"
    else
      if List.length unks_w = 0 then failwith "No card to guess"
      else List.nth unks_w (Random.int (List.length unks_w)) in
  (s, w, r)

(* [get_accusation] takes in a game sheet and the current location and returns
 * a card list of 1 room, 1 suspect, and 1 weapon that the agent thinks is
 * inside the envelope. *)
let get_accusation (me:player) public : guess =
  let bindings = CardMap.bindings me.sheet in
  let f (s, w, r) (c, i) =
    match c, i.card_info with
    | Suspect _, Envelope -> (c, w, r)
    | Weapon _, Envelope -> (s, c, r)
    | Room _, Envelope -> (s, w, c)
    | _ -> (s, w, r) in
  List.fold_left f (Suspect "", Weapon "", Room "") bindings

(* picks which card to show based on having shown a card before *)
let pick_to_show lst cp =
  let pre_shown = List.filter (fun (c, shn) -> List.mem cp shn) lst in
  match pre_shown with
  | [] -> fst (rand_from_lst lst)
  | [(c, shn)] -> c
  | lst' -> fst (rand_from_lst lst')

(* [get_answer] takes in a hand and the current guess and returns Some card
 * if a card from the hand and also in the list can be shown. Returns None
 * if no card can be shown. *)
let get_answer (me:player) public guess : card option =
  let (sus, weap, room) = guess in
  let cp = public.curr_player in
  let f acc el = match (CardMap.find el me.sheet).card_info with
                 | Mine [] -> (el, [])::acc
                 | Mine lst -> (el, lst)::acc
                 | _ -> acc in
  let mine_info = List.fold_left f [] (sus::weap::[room]) in
  match mine_info with
  | [] -> None
  | [(c, lst)] -> Some c
  | lst -> Some (pick_to_show lst cp)

(* [take_notes pl pu] updates the ResponsiveAIs sheet based on the listen data
 * in public. *)
let take_notes pl pub current_guess suspect_option = pl
