open Data

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
  let f c i acc = match (c, i.card_info) with
                 | (Room _, Envelope) -> acc || true
                 | _ -> acc in
  CardMap.fold f me.sheet false

(* returns true if the location corresponds with a card marked as
 * Unknown in [me].sheet *)
let is_unknown_room me loc =
  let f c = match (CardMap.find c me.sheet).card_info with
            | Unknown -> true
            | _ -> false in
  match loc.info with
  | Room_Rect (s, _) -> f (Room s)
  | _ -> false

(* [answer_move] gets the type of movement the agent wants to perform,
 * so either roll the dice or take a secret passage if possible  *)
let answer_move pl pub moves : move =
  let knows_room = knows_room pl in
  let fold f acc pass =
    match pass with
    | Passage r when f pl r -> (Passage r)::acc
    | _ -> acc in
  if knows_room then Roll
  else
    let unknowns = List.fold_left (fold is_unknown_room) [] moves in
    if List.length unknowns > 0 then rand_from_lst unknowns else Roll

(* Picks a random element of the list *)
let pick_random lst =
  let len = List.length lst in
  if len = 0 then None
  else Some (List.nth lst (Random.int len))

(* removes an option so that it can't be selected *)
let rec remove_op op checked tl =
  match tl with
  | [] -> checked
  | h::t -> if h = op then checked@t else remove_op op (h::checked) t

(* Used when the AI can't use a certain list of places. Should never
 * be propogated up *)
exception No_place_to_go

(* [get_movement] passes in a list of locations that could be moved to,
 * and returns the agent's choice of movement *)
let get_movement pl pub move_ops : movement =
  let rec go mops =
    match pick_random mops with
    | Some (l, (s, b)) ->
      let r_info = (CardMap.find (Room(s)) pl.sheet).card_info in
      if r_info = Unknown then (l, (s, b))
      else go (remove_op (l, (s, b)) [] mops)
    | None -> raise No_place_to_go in
  let go_acc () =
    let accl =
        List.filter (fun (_, (s, _)) -> (s = pub.acc_room)) move_ops in
    if List.length accl = 0 then failwith "No acc room exists"
    else List.hd accl in
  if knows_sus pl && knows_weap pl && knows_room pl then (* Knows All *)
    go_acc ()
  else
    let no_acc_f = (fun (_, (s, _)) -> not (s = pub.acc_room)) in
    let no_acc = List.filter no_acc_f move_ops in
    if List.length no_acc = 0 then (pl.curr_loc, ("no where", false))
    else
      let can_reach = List.filter (fun (l, (s, b)) -> b) no_acc in
      try go can_reach with No_place_to_go ->
      try go no_acc with No_place_to_go ->
      match pick_random no_acc with
      | Some (l, (s, b)) -> (l, (s, b))
      | None -> (pl.curr_loc, ("no where", false))

(* gets the cards in the sheet with a certain info *)
let get_cards_with_info info sheet =
  let l = CardMap.filter (fun _ data -> (data.card_info = info)) sheet in
  List.map (fun (c,_) -> c) (CardMap.bindings l)

(* [get_accusation] takes in a game sheet and the current location and returns
 * a card list of 1 room, 1 suspect, and 1 weapon that the agent thinks is
 * inside the envelope. *)
let get_accusation pl pub : guess =
  let unks = get_cards_with_info Unknown pl.sheet in
  let envs = get_cards_with_info Envelope pl.sheet in
  let s_only c = match c with Suspect s -> true | _ -> false in
  let w_only c = match c with Weapon s -> true | _ -> false in
  let r_only c = match c with Room s -> true | _ -> false in
  let env_s = List.filter s_only envs in
  let env_w = List.filter w_only envs in
  let env_r = List.filter r_only envs in
  let unks_s = List.filter s_only unks in
  let unks_w = List.filter w_only unks in
  let unks_r = List.filter r_only unks in
  let s = if List.length env_s = 0 then
            if List.length unks_s = 0 then failwith "No card"
            else List.nth unks_s (Random.int (List.length unks_s))
          else List.hd env_s in
  let w = if List.length env_w = 0 then
            if List.length unks_w = 0 then failwith "No card"
            else List.nth unks_w (Random.int (List.length unks_w))
          else List.hd env_w in
  let r = if List.length env_r = 0 then
            if List.length unks_r = 0 then failwith "No card"
            else List.nth unks_r (Random.int (List.length unks_r))
          else List.hd env_r in
  (s, w, r)

(* Makes a Room card out of a room location *)
let get_card_for_loc l = match l.info with
  | Room_Rect (s, i) -> Room s
  | _ -> failwith "Not a room"

(* [get_guess] takes in a game sheet and the current location and returns
 * a card list of 1 room, 1 suspect, and 1 weapon that the agent guesses. *)
let get_guess pl pub : guess =
  let r = get_card_for_loc pl.curr_loc in
  let (s, w, _) = get_accusation pl pub in
  (s, w, r)
  (* let unks = get_cards_with_info Unknown pl.sheet in
  let envs = get_cards_with_info Envelope pl.sheet in
  let s_only c = match c with Suspect s -> true | _ -> false in
  let w_only c = match c with Weapon s -> true | _ -> false in
  let env_s = List.filter s_only envs in
  let env_w = List.filter w_only envs in
  let unks_s = List.filter s_only unks in
  let unks_w = List.filter w_only unks in
  let s = if List.length env_s = 0 then
            if List.length unks_s = 0 then
              failwith "No card"
            else
              List.nth unks_s (Random.int (List.length unks_s))
          else List.hd env_s in
  let w = if List.length env_w = 0 then
            if List.length unks_w = 0 then
              failwith "No card"
            else
              List.nth unks_w (Random.int (List.length unks_w))
          else List.hd env_w in
  (s, w, r) *)

(* [get_answer_hand] takes in a hand from the player,the current guess and
 * returns Some card. if a card from the hand and also in the list can be shown.
 * Returns None if no card can be shown. *)
let rec get_answer_hand hand pub ((s,w,r) as guess) : card option=
  match hand with
  | [] -> None
  | h::t-> if s = h || w = h || r = h then Some h
           else get_answer_hand t pub guess

(* [get_answer] takes in a hand and the current guess and returns Some card
 * if a card from the hand and also in the list can be shown. Returns None
 * if no card can be shown. *)
let get_answer pl pub guess :card option =
  get_answer_hand pl.hand pub guess


(* [take_notes pl pu] updates the ResponsiveAIs sheet based on the listen data
 * in public. *)
let take_notes pl pub current_guess suspect_option = pl
