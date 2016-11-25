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

(* [get_movement] passes in a list of locations that could be moved to,
 * and returns the agent's choice of movement *)
let rec get_movement pl pub move_ops : loc =
  match move_ops with
  | []-> pl.curr_loc
  | (loc, (str, b))::t ->
    match loc.info with
    | Room_Rect (name,_)->
      let r_info = (CardMap.find (Room(name)) pl.sheet).card_info in
        if r_info = Unknown then loc
        else get_movement pl pub t
    | Space (_,_) -> get_movement pl pub t

(* [guess_acc_handler] returns a list of tuples that flips the bindngs of
 * card to sheet data to sheet_data to card  *)
let guess_acc_handler pl pub =
  let info_list = CardMap.bindings pl.sheet in
  let flipped_binding = List.map (fun (a,b) -> (b,a)) info_list in
    List.fold_left (fun acc (a,c) -> (a.card_info,c)::acc) [] flipped_binding

(* [get_c_lst] returns a list with the needed card_info*)
let rec get_c_lst lst c_inf=
  match lst with
  | []->[]
  | (inf,card)::t-> if inf = c_inf then card::get_c_lst t c_inf
                    else get_c_lst t c_inf

let get_acc lst :guess =
  let suspect_list =
    List.filter (fun ele -> match ele with
                            | Suspect s -> true
                            | _ -> false) lst in
  let weapon_list =
    List.filter (fun ele -> match ele with
                            | Weapon s -> true
                            | _ -> false) lst in
  let room_list =
    List.filter (fun ele -> match ele with
                            | Room s -> true
                            | _ -> false) lst in
  (List.nth suspect_list 0, List.nth weapon_list 0, List.nth room_list 0)


(* [get_accusation] takes in a game sheet and the current location and returns
 * a card list of 1 room, 1 suspect, and 1 weapon that the agent thinks is
 * inside the envelope. *)
let get_accusation pl pub : guess=
  let card_info_list = guess_acc_handler pl pub in
    get_acc (get_c_lst card_info_list Envelope)

let get_g guess_list pl pub=
  let r_lst = List.fold_left (fun acc e -> match e with
                                | Room(_)->e::acc
                                | _ -> acc) [] guess_list in
  let s_lst = List.fold_left (fun acc e -> match e with
                                | Suspect(_)->e::acc
                                | _ -> acc) [] guess_list in
  let w_lst = List.fold_left (fun acc e -> match e with
                                | Weapon(_)->e::acc
                                | _ -> acc) [] guess_list in
    let r = Random.int (List.length r_lst) in
    let s = Random.int (List.length s_lst) in
    let w = Random.int (List.length w_lst) in
      (List.nth s_lst s,List.nth w_lst w, List.nth r_lst r)

(* [get_guess] takes in a game sheet and the current location and returns
 * a card list of 1 room, 1 suspect, and 1 weapon that the agent guesses. *)
let get_guess pl pub: guess =
  let card_info_list = guess_acc_handler pl pub in
  let guess_list  = (get_c_lst card_info_list Unknown
                    @ get_c_lst card_info_list Envelope) in
    if List.length guess_list = 0 then get_accusation pl pub
    else get_g guess_list pl pub

(* (Suspect ("Red"),Weapon("pistol"),Room("Bathroon")) *)

(* [get_answer_hand] takes in a hand from the player,the current guess and
 * returns Some card. if a card from the hand and also in the list can be shown.
 * Returns None if no card can be shown. *)
let rec get_answer_hand hand pub guess : card option=
  let (s,w,r)= guess in
  match hand with
  | []->None
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
