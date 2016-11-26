open Data

(* [answer_move] gets the type of movement the agent wants to perform,
 * so either roll the dice or take a secret passage if possible  *)
let rec answer_move pl pub moves : move =
  match moves with
  | []-> Roll
  | h::t->
    match h with
    | Roll -> answer_move pl pub t
    | Passage (loc) ->
        match loc.info with
        | Space (_,_) -> Roll
        | Room_Rect (name,_) ->
          let r_info = (CardMap.find (Room(name)) pl.sheet).card_info in
            if r_info = Unknown then h
            else answer_move pl pub t

let pick_random lst =
  let len = List.length lst in
  if len = 0 then None
  else Some (List.nth lst (Random.int len))

let rec remove_op op checked tl =
  match tl with
  | [] -> checked
  | h::t -> if h = op then checked@t else remove_op op (h::checked) t

exception No_place_to_go

(* [get_movement] passes in a list of locations that could be moved to,
 * and returns the agent's choice of movement *)
let rec get_movement pl pub move_ops : loc =
  let rec go mops =
    match pick_random mops with
    | Some (l, (s, b)) ->
      let r_info = (CardMap.find (Room(s)) pl.sheet).card_info in
      if r_info = Unknown then l
      else go (remove_op (l, (s, b)) [] mops)
    | None -> raise No_place_to_go in
  let no_acc =
    List.filter (fun (_, (s, _)) -> not (s = pub.acc_room)) move_ops in
  if List.length no_acc = 0 then pl.curr_loc
  else
    let filtered = List.filter (fun (l, (s, b)) -> b) no_acc in
    try
      if List.length filtered = 0 then go no_acc
      else go filtered
    with No_place_to_go ->
      let accl =
        List.filter (fun (_, (s, _)) -> (s = pub.acc_room)) move_ops in
      let (l, (s, b)) =
        if List.length accl = 0 then failwith "No acc room exists"
        else List.hd accl in
      l

let print_card c = match c with
  | Suspect s -> print_string ("Suspect "^s^": ")
  | Weapon s -> print_string ("Weapon "^s^": ")
  | Room s -> print_string ("Room "^s^": ")

let print_data d = match d.card_info with
  | Mine _ -> print_endline ("Mine")
  | ShownBy s -> print_endline ("Shown by "^s)
  | Unknown -> print_endline ("Unknown")
  | Envelope -> print_endline ("Envelope")

let get_cards_with_info info sheet =
  let l =
    CardMap.filter (fun _ data -> (data.card_info = info)) sheet in
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
            if List.length unks_s = 0 then
              (* print_sheet *)
              let () = ignore (List.map (fun (c,d) -> print_card c; print_data d; c) (CardMap.bindings pl.sheet)) in
              failwith "No card"
            else
              List.nth unks_s (Random.int (List.length unks_s))
          else List.hd env_s in
  let w = if List.length env_w = 0 then
            if List.length unks_w = 0 then
              (* print_sheet *)
              let () = ignore (List.map (fun (c,d) -> print_card c; print_data d; c) (CardMap.bindings pl.sheet)) in
              failwith "No card"
            else
              List.nth unks_w (Random.int (List.length unks_w))
          else List.hd env_w in
  let r = if List.length env_r = 0 then
            if List.length unks_r = 0 then
              (* print_sheet *)
              let () = ignore (List.map (fun (c,d) -> print_card c; print_data d; c) (CardMap.bindings pl.sheet)) in
              failwith "No card"
            else
              List.nth unks_r (Random.int (List.length unks_r))
          else List.hd env_r in
  (s, w, r)

let get_card_for_loc l = match l.info with
  | Room_Rect (s, i) -> Room s
  | _ -> failwith "Not a room"

(* [get_guess] takes in a game sheet and the current location and returns
 * a card list of 1 room, 1 suspect, and 1 weapon that the agent guesses. *)
let get_guess pl pub: guess =
  let r = get_card_for_loc pl.curr_loc in
  let unks = get_cards_with_info Unknown pl.sheet in
  let envs = get_cards_with_info Envelope pl.sheet in
  let s_only c = match c with Suspect s -> true | _ -> false in
  let w_only c = match c with Weapon s -> true | _ -> false in
  let env_s = List.filter s_only envs in
  let env_w = List.filter w_only envs in
  let unks_s = List.filter s_only unks in
  let unks_w = List.filter w_only unks in
  let s = if List.length env_s = 0 then
            if List.length unks_s = 0 then
              (* print_sheet *)
              let () = ignore (List.map (fun (c,d) -> print_card c; print_data d; c) (CardMap.bindings pl.sheet)) in
              failwith "No card"
            else
              List.nth unks_s (Random.int (List.length unks_s))
          else List.hd env_s in
  let w = if List.length env_w = 0 then
            if List.length unks_w = 0 then
              (* print_sheet *)
              let () = ignore (List.map (fun (c,d) -> print_card c; print_data d; c) (CardMap.bindings pl.sheet)) in
              failwith "No card"
            else
              List.nth unks_w (Random.int (List.length unks_w))
          else List.hd env_w in
  (s, w, r)

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
let get_answer pl pub guess :card option=
  get_answer_hand pl.hand pub guess


(* [take_notes pl pu] updates the ResponsiveAIs sheet based on the listen data
 * in public. *)
let take_notes pl pub = pl
