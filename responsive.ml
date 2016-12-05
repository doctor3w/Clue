open Data

module Display = View

(* Used when the AI can't use a certain list of places. Should never
 * be propogated up *)
exception No_place_to_go

(* returns positive result of i mod n*)
let p_mod i n = ((i mod n)+n) mod n

(* finds the index of [x] in [lst] *)
let rec find x lst =
    match lst with
    | [] -> failwith ("Not Found: " ^ Pervasives.__LOC__)
    | h :: t -> if x = h then 0 else 1 + find x t

(* returns corresponding index of [sus] in the matrix *)
let suspect_to_index public (sus:string) : int =
	find sus public.player_order

(* returns the corresponding suspect name based on the index [i] *)
let index_to_suspect public i : string = List.nth public.player_order i

(* returns corresponding index of the [card] in the matrix *)
let card_to_index public (card:card) : int =
	let (s_lst, w_lst, r_lst) = public.deck in
	let deck' = s_lst @ w_lst @ r_lst in
	find card deck'

(* returns the corresponding card based on the index [i] *)
let index_to_card public i : card =
	let (s_lst, w_lst, r_lst) = public.deck in
	let deck' = s_lst @ w_lst @ r_lst in
	List.nth deck' i

(* returns a random element in [lst] *)
let rand_from_lst lst =
  let len = List.length lst in
  if len = 0 then failwith ("no lst: " ^ Pervasives.__LOC__)
  else let n = Random.int len in
    List.nth lst n

(* return the max number in the list *)
let my_max = function
  | [] -> failwith ("empty list: " ^ Pervasives.__LOC__)
  | x::xs -> List.fold_left max x xs

(* true if card [c] is marked as Envelope in [me].sheet *)
let is_env_card me c =
  match (CardMap.find c me.sheet).card_info with
  | Envelope -> true
  | _ -> false

(* return a list that contains all of the envelope cards
  [me] knows so far *)
let current_deck_to_env public me =
  let (s_lst, w_lst, r_lst) = public.deck in
  let deck' = s_lst@w_lst@r_lst in
  let rec f lst =
    match lst with
    | [] -> []
    | h::t -> if is_env_card me h
              then h::(f t)
              else f t in
  f deck'

(* returns Some card if [lst] contains a suspect card else return None*)
let rec find_final_suspect lst =
  match lst with
  | [] -> None
  | (Suspect s)::t -> Some (Suspect s)
  | _::t -> find_final_suspect t

(* returns Some card if [lst] contains a weapon card else return None *)
let rec find_final_weapon lst =
  match lst with
  | [] -> None
  | (Weapon s)::t -> Some (Weapon s)
  | _::t -> find_final_weapon t

(* returns Some card if [lst] contains a room card,else return None*)
let rec find_final_room lst =
  match lst with
  | [] -> None
  | (Room s)::t -> Some (Room s)
  | _::t -> find_final_room t

(* count how many [t] the array [a] has *)
let count_listenchoice a t =
  let counter = ref 0 in
  let len = Array.length a in
  for index = 0 to (len-1) do
    if a.(index) = t then counter := !counter + 1 else ()
  done;
  !counter

(* checks if the entire array [a] is all Not_in_hand*)
let is_all_notinhand a =
  let count = count_listenchoice a Not_in_hand in
  Array.length a = count

(* updates [counter] with the number of Known in the matrix *)
let rec helper matrix public (lst: card list) counter =
  match lst with
  | [] -> ()
  | h::t ->
    let h_index = card_to_index public h in
    if count_listenchoice matrix.(h_index) Known = 1 then
      counter:= !counter +1
    else ();
    helper matrix public t counter

(* Given a card list, which contains either suspects, or weapons,
  or rooms, check if all but one is known
  PreC: lst contains all cards for one type *)
let rec all_but_one_known matrix public (lst: card list) =
  let counter = ref 0 in
  helper matrix public lst counter;
  !counter = (List.length lst)-1

(* change all of the elements in array [a] into Env *)
let rewrite_env a =
  let len = Array.length a in
  for index = 0 to (len-1) do
    a.(index) <- Env
  done

(* Used with p_to_room to get a card from a loc *)
let extr_loc n =
  match n.info with
  | Room_Rect (r,_) -> Room r
  | _ -> failwith ("not a room card: " ^ Pervasives.__LOC__)

(* turns a passage into a room card *)
let p_to_room passage =
  match passage with
  | Roll -> failwith ("not gonna happen: " ^ Pervasives.__LOC__)
  | Passage n -> extr_loc n

(* [is_r_env_known] checks if player knows the room card in envelope *)
let is_r_env_known player =
  let s =
    CardMap.filter (fun _ data -> (data.card_info = Envelope)) player.sheet in
  let b = CardMap.bindings s in
  let f (c, i) = match c with
    | Room (n) -> true
    | _ -> false in
  let r = List.filter f b in
  not (r = [])

(* [is_w_env_known] checks if player knows the room card in envelope *)
let is_w_env_known player =
  let s =
    CardMap.filter (fun _ data -> (data.card_info = Envelope)) player.sheet in
  let b = CardMap.bindings s in
  let f (c, i) = match c with
    | Weapon (n) -> true
    | _ -> false in
  let r = List.filter f b in
  not (r = [])

  (* [is_s_env_known] checks if player knows the room card in envelope *)
let is_s_env_known player =
  let s =
    CardMap.filter (fun _ data -> (data.card_info = Envelope)) player.sheet in
  let b = CardMap.bindings s in
  let f (c, i) = match c with
    | Suspect (n) -> true
    | _ -> false in
  let r = List.filter f b in
  not (r = [])

(* [check_p_farthest] checks if the farthest player from me knows any of the
 * passages *)
let rec check_p_farthest player public new_p_lst =
  match new_p_lst with
  | [] -> false
  | h::t -> let r = p_to_room h in
            let r_i = card_to_index public r in
            let pi = suspect_to_index public player.suspect in
            let farthest = p_mod (pi-1) (List.length public.player_order) in
            let b = (player.listen.(r_i).(farthest) = Known ) in
            let b2 = check_p_farthest player public t in
            b || b2

(* returns a list of passages that are not Known in listens *)
let rec check_p_known player public passage_list =
  match passage_list with
  | [] -> []
  | h::t->
    let r = p_to_room h in
    let r_i = card_to_index public r in
    if Array.exists (fun x -> x = Known) player.listen.(r_i) then
      check_p_known player public t
    else h:: check_p_known player public t

(* returns the largest element in [lst] *)
let my_max lst = match lst with
  | [] -> failwith ("my max has an empty list" ^ Pervasives.__LOC__)
  | x::xs -> List.fold_left max x xs

(* returns either Roll or Passage with most not_in_hand *)
let rec p_most_not_in_hand player public passage_list =
  let r_p_lst = List.map (fun p -> (p_to_room p,p)) passage_list in
  let ri_p_lst = List.map (fun (r,p) -> (card_to_index public r,p)) r_p_lst in
  let f (ri,p) = (count_listenchoice player.listen.(ri) Not_in_hand, p) in
  let counted_p = List.map f ri_p_lst in
  let c_lst = List.map (fun (c,p)-> c) counted_p in
  let ci = my_max c_lst in
  if ci = 0 then Roll
  else List.assoc ci counted_p


(* [is_p_env]
 * 1. if player knows room env and room env is passage
 * - check the rest of passages with farthest player,
 * if one or both of the passage are known, we go to passage else ROLL
 * 2. if player knows room env and room env is not passage
 * - check the farther player if he has one or more of the passage,
 * if yes take one of the passage, if not ROLL
 * 3. if player does not know room env
 * - check how many of the passages are Known, filter away those are known,
 * if there is nothing left, ROLL,
 * if there are passages left, count not_in_hand
 * and go to passage with the most not_in_hand else ROLL *)
let is_p_env player public passage_list =
  if is_r_env_known player then
    let f p =
      let r = p_to_room p in
      match (CardMap.find r player.sheet).card_info with
      | Envelope -> true
      | _ -> false in
    let env_lst = List.filter f passage_list in
    let new_p_lst =
        List.filter (fun x -> not (List.mem x env_lst)) passage_list in
    if List.length env_lst > 0 then
      if check_p_farthest player public new_p_lst then rand_from_lst new_p_lst
      else Roll
    else
      if check_p_farthest player public passage_list then
        rand_from_lst passage_list
      else Roll
  else
    if check_p_known player public passage_list = [] then Roll
    else p_most_not_in_hand player public passage_list

(* [answer_move] gets the type of movement the agent wants to perform,
 * so either roll the dice or take a secret passage if possible  *)
let answer_move player public move_list : move =
  if is_r_env_known player && is_w_env_known player && is_s_env_known player
  then
    Roll
  else
    let f a = match a with
      | Roll -> false
      | Passage _-> true in
    let passage = List.filter f move_list in
    if passage  = [] then Roll else is_p_env player public passage

(* true if s is the acc_room, takes in an element from [movelst] below *)
let is_acc_room public (_, (s, _)) = s = public.acc_room

(* turns movement list to room list *)
let m_to_r move_option_list =
  List.map (fun (l,(s,b)) -> (Room s, (l,(s,b)))) move_option_list

(* check if I have a card in move list *)
let check_hand_rooms pl card_move_list=
  let rs = List.map (fun (a,b)->a) card_move_list in
  List.filter (fun x -> List.mem x rs) pl.hand

(* check if room env is in move_option_list *)
let check_env_in_move r move_option_list =
  let lst = List.filter (fun (l,(s,b))-> Room s = r) move_option_list in
  not (lst = [])

(* return a card list where all cards in [card_lst] are filtered off
    if they have known in [matrix] *)
let get_notknwon_cards card_lst matrix public =
  let card_array_list =
    List.map (fun x -> (x, matrix.(card_to_index public x))) card_lst in
  let rec f c_a_list =
    match c_a_list with
    | [] -> []
    | h::t -> if Array.exists (fun e -> e = Known) (snd h) then f t
              else h::(f t) in
  let new_lst = f card_array_list in
  List.map (fun y -> fst y) new_lst

(* returns the maximum number and its corresponding card that Listen_choice [t]
 * appears in the matrix *)
let most_type card_lst (matrix: listen_choice array array) public t =
  let array_card_list =
    List.map (fun x -> (matrix.(card_to_index public x), x)) card_lst in
  let count_card_list =
    List.map (fun x -> (count_listenchoice (fst x) t, snd x))
      array_card_list in
  let count_list = List.map (fun x -> fst x) count_card_list in
  let max_num = my_max count_list in
  (max_num, List.assoc max_num count_card_list)

(* logic for [r_env_not_known]:
 * filter away any room cards that are known,
 * if all cards in accessible is known then do the same logic with not accessible,
 * else
 *  count most not not_in_hand and go to that room ,
 *  if not_in_hand count is 0 then go to room with most pure_unknown
 *  else randomly pick maybe_in_hand *)
let r_env_not_known pl public card_m_lst =
  let c_lst = List.map (fun (c,move)-> c) card_m_lst in
  let c_not_known_lst = get_notknwon_cards c_lst pl.listen public in
  if List.length c_not_known_lst = 0 then raise No_place_to_go
  else
    let (i,c) = most_type c_not_known_lst pl.listen public Not_in_hand in
      if i > 0 then List.assoc c card_m_lst
      else
        let (i',c') =
          most_type c_not_known_lst pl.listen public Pure_unknown in
        if i' > 0 then List.assoc c' card_m_lst
        else
          snd (rand_from_lst card_m_lst)

(* [get_movement] passes in a list of locations that could be moved to,
 * and returns the agent's choice of movement
 * logic for get_movement:
 * 1.if you know all three cards, go to accusation room :accessible or not
 * 2.else: make two lists one accessible the other not
 * 2.1 if you know room envelope, check my cards first
 *   if i have rooms in my hand else just go to room envelope
 *   if there is no where to go, pick a room randomly
 * 2.2 if you do not know room envelope, calls [r_env_not_known] twice with
 * accessible movement and not accessible movement *)
let get_movement pl public movelst: movement =
if is_r_env_known pl && is_w_env_known pl && is_s_env_known pl
then
  match List.filter (is_acc_room public) movelst with
  | [(l, (s, b))] -> (l, (s, b))
  | _ -> failwith ("can't find accusation room: " ^ Pervasives.__LOC__)
else
  let b = (fun x -> not (is_acc_room public x)) in
  let movelst' = List.filter b movelst in
  let access = List.filter (fun (l,(s,b))-> b ) movelst' in
  let not_access = List.filter (fun (l,(s,b))-> not b) movelst' in
  let c_access = m_to_r access in
  let my_card = check_hand_rooms pl c_access in
  let c_not_access = m_to_r not_access in
  let env_lst = current_deck_to_env public pl in
    if is_r_env_known pl then
      if List.length c_access >0 then
        if List.length my_card > 0 then
          let i = Random.int (List.length my_card) in
          List.assoc (List.nth my_card i) c_access
        else
          let env = find_final_room env_lst in
          let r = match env with
          | None -> failwith ("player knows room in env, still None"
                              ^ Pervasives.__LOC__)
          | Some n -> n in
          if check_env_in_move r access then
            List.assoc r c_access
          else snd (rand_from_lst c_access)
      else snd (rand_from_lst c_not_access)
    else
      if List.length access > 0 then
        try r_env_not_known pl public c_access with
        | No_place_to_go -> r_env_not_known pl public c_not_access
      else r_env_not_known pl public c_not_access


(* return true if there is one card in a card list [lst] is in Env *)
let is_env_in_list matrix (lst:card list) public =
  let f ele = card_to_index public ele in
  let index_list = List.map f lst in
  let rec g counter index_lst =
    match index_lst with
    | [] -> counter
    | h::t ->
      if matrix.(h).(0) = Env then g (counter+1) t
      else g counter t in
  let final = g 0 index_list in
  if final = 1 then true else false

let no_env sheet card = (CardMap.find card sheet).card_info <> Envelope

let loc_to_card loc =
  match loc.info with
  | Room_Rect (s, _) -> (Room s)
  | _ -> failwith ("trying to guess from not room: "
                   ^ Pervasives.__LOC__)

(* sorts out other listen_choice when there is no Not_in_Hand information *)
let next_step (i,c) lst matrix public =
  if i = 0 then
    let (i',c') = most_type lst matrix public Pure_unknown in
    if i' = 0 then snd (most_type lst matrix public Maybe_in_hand) else c'
  else c

(* processes all situation when the envelope a type of card is unknown *)
  let false_helper lst matrix public =
  let no_known_list = get_notknwon_cards lst matrix public in
  let most_notinhand = most_type no_known_list matrix public Not_in_hand in
  next_step most_notinhand no_known_list matrix public

let triple_fst (a,b,c) = a
let triple_snd (a,b,c) = b
let triple_thd (a,b,c) = c

(* [separate_hand] sorts out the three types of cards in player.hand *)
let separate_hand player =
  let h = player.hand in
  let rec f (a,b,c) lst =
    match lst with
    | [] -> (a,b,c)
    | (Suspect s)::t -> f ((Suspect s)::a, b, c) t
    | (Weapon s)::t -> f (a, (Weapon s)::b, c) t
    | (Room s)::t -> f (a, b, (Room s)::c) t
  in f ([],[],[]) h

(* [get_guess] takes in a game sheet and the current location and returns
 * a card list of 1 room, 1 suspect, and 1 weapon that the agent guesses.
 * logic for [get_guess]
 * 1. if we know both envelope of suspect and weapon then make random guess of
 * except the two envelope cards
 * 2. If one of the card is known to be in envelope then
 * 2.1 the type of card known to be in envelope : guess my own card or envelope
 * 2.2 the other card unknown: go through listens,
 *    filter out what cards are known; count most not_in_hand and pick that one
 *     if there is no not_in_hand, then pick most Pure_Unknown, else random from
 *     the rest
 * 3. if no envlope card for both types then follow same logic for each card in
      2.2  *)
let get_guess player public : guess =
  let (s_lst, w_lst, r_lst) = public.deck in
  let matrix = player.listen in
  let sheet = player.sheet in
  match (find_final_suspect (current_deck_to_env public player) <> None),
        (find_final_weapon (current_deck_to_env public player) <> None) with
  | true, true ->
    let s = rand_from_lst (List.filter (no_env sheet) s_lst) in
    let w = rand_from_lst (List.filter (no_env sheet) w_lst) in
    (s,w,(loc_to_card player.curr_loc))
  | true, false ->
    let w = false_helper w_lst matrix public in
    let s =
      if triple_fst (separate_hand player) <> []
      then rand_from_lst (triple_fst (separate_hand player))
      else (match find_final_suspect (current_deck_to_env public player) with
        | Some c -> c
        | None -> failwith ("It can't be None: " ^ Pervasives.__LOC__)) in
    (s,w,(loc_to_card player.curr_loc))
  | false, true ->
    let s = false_helper s_lst matrix public in
    let w =
      if triple_snd (separate_hand player) <> []
      then rand_from_lst (triple_snd (separate_hand player))
      else (match find_final_weapon (current_deck_to_env public player) with
        | Some c -> c
        | None ->failwith ("It can't be None: " ^ Pervasives.__LOC__)) in
    (s,w,(loc_to_card player.curr_loc))
  | false, false ->
    let s = false_helper s_lst matrix public in
    let w = false_helper w_lst matrix public in
    (s,w,(loc_to_card player.curr_loc))

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

(* [process_of_elimination]goes through each type of card
 * and use the process of elimination to deduce if there's one final card
 * then it should be made the envelope card in the sheet. *)
let process_of_elimination sheet pub =
  let (ss, ws, rs) = pub.deck in
    let s_unks = poe_finder ss [] sheet in
    let w_unks = poe_finder ws [] sheet in
    let r_unks = poe_finder rs [] sheet in
    poe_update s_unks sheet |> poe_update w_unks |> poe_update r_unks

(* [listen_unk_to_env] updates listens when the AI makes a guess and no one
 * shows a card *)
let listen_unk_to_env listen player public (s,w,r) :unit=
  let s_index = card_to_index public s in
  let w_index = card_to_index public w in
  let r_index = card_to_index public r in
  let me = suspect_to_index public player.suspect in
  for j = 0 to List.length public.player_order -1
    do (
     if j = me then ()
      else
      listen.(s_index).(j)<- Env;
      listen.(w_index).(j)<-Env;
      listen.(r_index).(j)<-Env) done

(* [listen_ans_update] updates listens when someone shows the AI a card *)
let listen_ans_update listen sus card public =
  let y_len = List.length public.player_order in
  let c_index = card_to_index public card in
  let sus_index = suspect_to_index public sus in
    listen.(c_index).(sus_index)<-Known;
  for j = 0 to (y_len-1)
  do (if j = sus_index
      then () else listen.(c_index).(j)<-Not_in_hand) done;
    if is_all_notinhand listen.(c_index) then rewrite_env listen.(c_index)
    else
      let (ss, ws, rs) = public.deck in
      if all_but_one_known listen public ss then rewrite_env listen.(c_index)
      else ();
      if all_but_one_known listen public ws then rewrite_env listen.(c_index)
      else ();
      if all_but_one_known listen public rs then rewrite_env listen.(c_index)
      else ()

(* [show_card pl pu c g] updates the players sheet based on the new card seen
 * and the guess. If card is None, then that means no one had cards in the
 * guess and needs to be updated accordingly. Also needs to use process of
 * elimination for certain AIs. The string is who showed's suspect ID. *)
let show_card pl public answer (s,w,r) :player =
  match answer with
  | None ->
    let () = Display.display_answer None "" false in
    let sheet' = unk_to_env s pl.sheet |> unk_to_env w |> unk_to_env r in
    listen_unk_to_env pl.listen pl public (s,w,r);{pl with sheet = sheet'}
  | Some (sus, card) ->
    let () = Display.display_answer (Some card) sus false in
    let data = CardMap.find card pl.sheet in
    let data' = {data with card_info= ShownBy(sus)} in
    let sheet' = CardMap.add card data' pl.sheet in
    listen_ans_update pl.listen sus card public;
    {pl with sheet = (process_of_elimination sheet' public)}


(* [get_accusation] takes in a game sheet and the current location and returns
 * a card list of 1 room, 1 suspect, and 1 weapon that the agent thinks is
 * inside the envelope. *)
let get_accusation player public :guess=
  let bindings = CardMap.bindings player.sheet in
    let f (s, w, r) (c, i) =
      match c, i.card_info with
      | Suspect _, Envelope -> (c, w, r)
      | Weapon _, Envelope -> (s, c, r)
      | Room _, Envelope -> (s, w, c)
      | _ -> (s, w, r) in
    List.fold_left f (Suspect "", Weapon "", Room "") bindings

(* [room_not_preferred] chooses suspect and weapon card over room card because
 * room card is the hardest to narrow down choices *)
let room_not_preferred lst =
  let f (c,shn) = match c with
                  | Suspect n | Weapon n -> true
                  | _ -> false in
                    List.filter f lst

(* [pick_to_show] chooses weapon or suspect to the other play over room due to
 * room is the hardest information to get if the current player has more than
 * one card in their hand that matches the current player's guess *)
let pick_to_show lst cp =
  let pre_shown = List.filter (fun (c, shn) -> List.mem cp shn) lst in
  match pre_shown with
  | [] -> fst (rand_from_lst lst)
  | [(c, shn)] -> c
  | lst' -> let no_rooms = room_not_preferred lst' in
            if List.length no_rooms > 0 then fst (rand_from_lst no_rooms)
            else fst (rand_from_lst lst')

(* [get_answer] takes in a hand and the current guess and returns Some card
 * if a card from the hand and also in the list can be shown. Returns None
 * if no card can be shown. *)
let get_answer (me:player) public guess : card option =
  let (sus, weap, room) = guess in
  let cp = public.curr_player in
  let f acc el = match (CardMap.find el me.sheet).card_info with
                | Mine []-> (el,[])::acc
                | Mine lst -> (el, lst)::acc
                | _ -> acc in
  let mine_info = List.fold_left f [] (sus::weap::[room]) in
  match mine_info with
  | [] -> None
  | [(c, lst)] -> Some c
  | lst -> Some (pick_to_show lst cp)

(* [get_accusation] takes in a game sheet and the current location and returns
 * a card list of 1 room, 1 suspect, and 1 weapon that the agent thinks is
 * inside the envelope. *)
let get_accusation player public :guess=
  let bindings = CardMap.bindings player.sheet in
    let f (s, w, r) (c, i) =
      match c, i.card_info with
      | Suspect _, Envelope -> (c, w, r)
      | Weapon _, Envelope -> (s, c, r)
      | Room _, Envelope -> (s, w, c)
      | _ -> (s, w, r) in
    List.fold_left f (Suspect "", Weapon "", Room "") bindings

let room_not_preferred lst =
  let f (c,shn) = match c with
                  | Suspect n | Weapon n -> true
                  | _ -> false in
                    List.filter f lst

(* [pick_to_show] chooses weapon or suspect to the other play over room due to
 * room is the hardest information to get if the current player has more than
 * one card in their hand that matches the current player's guess *)
let pick_to_show lst cp =
  let pre_shown = List.filter (fun (c, shn) -> List.mem cp shn) lst in
  match pre_shown with
  | [] -> fst (rand_from_lst lst)
  | [(c, shn)] -> c
  | lst' -> let not_rooms = room_not_preferred lst' in
            if List.length not_rooms > 0 then fst (rand_from_lst not_rooms)
            else fst (rand_from_lst lst')

(* [get_answer] takes in a hand and the current guess and returns Some card
 * if a card from the hand and also in the list can be shown. Returns None
 * if no card can be shown. *)
let get_answer (me:player) public guess : card option =
  let (sus, weap, room) = guess in
  let cp = public.curr_player in
  let f acc el = match (CardMap.find el me.sheet).card_info with
                | Mine lst -> (el, lst)::acc
                | _ -> acc in
  let mine_info = List.fold_left f [] (sus::weap::[room]) in
  match mine_info with
  | [] -> None
  | [(c, lst)] -> Some c
  | lst -> Some (pick_to_show lst cp)

(* if there is no player showing a card, then the responsive AI can
  make sure that all of the other players other than the guesser
  don't have the three card *)
let none_helper (matrix:listens) public s_index w_index r_index =
  let y_len = List.length public.player_order in
  for p_index1 = 0 to (y_len - 1)
    do (if (p_index1 = suspect_to_index public public.curr_player)
      then ()
      else ( if (matrix.(s_index).(p_index1) = Env)
             then () else (matrix.(s_index)).(p_index1)<-Not_in_hand )) done;
  for p_index2 = 0 to (y_len - 1)
    do (if (p_index2 = suspect_to_index public public.curr_player)
      then ()
      else ( if matrix.(w_index).(p_index2) = Env
             then () else (matrix.(w_index)).(p_index2)<-Not_in_hand )) done;
  for p_index3 = 0 to (y_len - 1)
    do (if (p_index3 = suspect_to_index public public.curr_player)
      then ()
      else ( if matrix.(r_index).(p_index3) = Env
             then () else (matrix.(r_index)).(p_index3)<-Not_in_hand )) done

(* Given a [matrix], and a specific location in the matrix,
  check if it's Pure_unknown then.
  If it is, then turn it into Maybe_in_hand;
  else, nothing changed *)
let match_helper matrix x_index y_index =
  let new_cell =
    match matrix.(x_index).(y_index) with
    | Pure_unknown -> Maybe_in_hand
    | _ -> matrix.(x_index).(y_index) in
        matrix.(x_index).(y_index) <- new_cell

(* return the number of rows where the cards are known for player [j].
  [i_len] is the number of total cards *)
let if_column_helper matrix j i_len=
  let counter = ref 0 in
  for index = 0 to (i_len-1)
  do (if matrix.(index).(j) = Known
    then counter := !counter + 1
    else ()) done;
  !counter

(* If the number of rows where the cards are known for player [j]
  is equal to the number of cards player [j] has,
  then turn all Pure_unknown and Maybe_in_hand into Not_in_hand;
  else, nothing changes. *)
let column_helper matrix j i_len player =
  if ((if_column_helper matrix j i_len) = List.length player.hand)
  then for index = 0 to (i_len - 1)
     do (if matrix.(index).(j) = Pure_unknown
          || matrix.(index).(j) = Maybe_in_hand
       then matrix.(index).(j) <- Not_in_hand
       else ()) done
  else ()

(* Find the index in [matrix] where the entire array (i.e. matrix.(index)
  doesn't include any Knwon. Then put the card into ref_l; rewrite the
  entire array to Env
   PreC: [all_but_one_known] for lst is true *)
(* if the entire row for a card is all filled up with Not_in_hand,
  it must be in the envelope *)
let compile_notinhand matrix public x_len ref_l =
  for index = 0 to (x_len-1)
  do (if is_all_notinhand matrix.(index)
    then ((ref_l := (index_to_card public index) :: !ref_l);
       rewrite_env matrix.(index))
    else ()) done

(* update player.listen when responsiveAI first gets the hand *)
let first_take_note player public: player =
  let matrix = player.listen in
  let p_index = suspect_to_index public player.suspect in
  let hand = player.hand in
  let y_len = List.length public.player_order in
  let (s_lst, w_lst, r_lst) = public.deck in
  let deck' = s_lst@w_lst@r_lst in
  let x_len = List.length deck' in
  for i = 0 to (x_len-1)
  do matrix.(i).(p_index) <- Not_in_hand done;
  let rec help ha public =
    (match ha with
    | [] -> ()
    | h::t ->
      (let c_index = card_to_index public h in
      matrix.(c_index).(p_index) <- Known;
      for i1 = 0 to (p_index-1)
      do (matrix.(c_index).(i1) <- Not_in_hand) done;
      for i2 = (p_index+1) to (y_len-1)
      do (matrix.(c_index).(i2) <- Not_in_hand) done;
      help t public)) in help hand public;
  player

(* returns player with a updated [player].sheet *)
let update_player player l =
  let f acc el =
    let sh = acc.sheet in
    let d = CardMap.find el sh in
    let d' = match d.card_info with
            | Unknown -> {d with card_info = Envelope}
            | _ -> d in
    let sh' = CardMap.add el d' sh in
    {acc with sheet = sh'} in
  List.fold_left f player l

(* updates [ref_l] and rewrites [matrix] based on [lst]*)
let compile_known matrix public lst ref_l =
  let counter = ref None in
  let index_lst = List.map (fun x -> card_to_index public x) lst in
  let len = List.length index_lst in
  for i = (List.nth index_lst 0) to List.nth index_lst (len-1)
     do (if (Array.exists (fun x -> x = Known) matrix.(i)) = false
       then (counter := Some i;
           ref_l := (index_to_card public i) :: !ref_l;
           rewrite_env matrix.(i))
       else ()) done

(* updates row [x_index]in [matrix] between column [asking_index] and column
 * [answering_index] to Not_in_hand  *)
let adjacent_helper matrix x_index asking_index answering_index y_len=
  if (asking_index < answering_index)
  then (for new_i = (asking_index+1) to (answering_index-1)
      do (if matrix.(x_index).(new_i) = Known
        then ()
        else matrix.(x_index).(new_i) <- Not_in_hand) done)
  else (for new_i1 = 0 to (answering_index-1)
      do (if matrix.(x_index).(new_i1) = Known
        then ()
        else matrix.(x_index).(new_i1) <- Not_in_hand) done;
      for new_i2 = (asking_index+1) to (y_len-1)
      do (if matrix.(x_index).(new_i2) = Known
        then ()
        else matrix.(x_index).(new_i2) <- Not_in_hand) done)

(* [take_notes] takes [player[, [public], [current_guess] and [str_option],
 * guess is the current guess by some other player and
 * string_option is if there is a player having any card to show him.
 * It returns a new player as the output with an updated listen and sheet
 *)
let take_notes player public guess str_option: player =
  let l = ref [] in
  let (s_lst, w_lst, r_lst) = public.deck in
  let deck' = s_lst@w_lst@r_lst in
  let x_len = List.length deck' in
  let y_len = List.length public.player_order in
  let (s,w,r) = guess in
  let s_index = card_to_index public s in
  let w_index = card_to_index public w in
  let r_index = card_to_index public r in
  let matrix = player.listen in
  match str_option with
  (* None should be find *)
  | None -> (none_helper matrix public s_index w_index r_index;
            (if all_but_one_known matrix public s_lst
             then compile_known matrix public s_lst l
             else ());
             (if all_but_one_known matrix public w_lst
             then compile_known matrix public w_lst l
             else ());
             (if all_but_one_known matrix public r_lst
             then compile_known matrix public r_lst l
             else ());
             (compile_notinhand matrix public x_len l);
             update_player player !l)
  | Some str ->
  (* update all the Pure_unknown to Maybe_in_hand *)
    (let p_index = suspect_to_index public str in
     match_helper matrix s_index p_index;
     match_helper matrix w_index p_index;
     match_helper matrix r_index p_index;
  (* if two cards are not in the string's hand, then the third card must in
    it's hand, since the string showed the player something *)
    (match matrix.(s_index).(p_index),
        matrix.(w_index).(p_index),
        matrix.(r_index).(p_index) with
    | Not_in_hand, Not_in_hand, Maybe_in_hand
      -> (matrix.(r_index).(p_index) <- Known;
         for i_r1 = 0 to (p_index-1)
         do (matrix.(r_index).(i_r1) <- Not_in_hand) done;
         for i_r2 = (p_index+1) to (y_len-1)
         do (matrix.(r_index).(i_r2) <- Not_in_hand) done;)
    | Not_in_hand, Maybe_in_hand, Not_in_hand
      -> (matrix.(w_index).(p_index) <- Known;
         for i_w1 = 0 to (p_index-1)
         do (matrix.(r_index).(i_w1) <- Not_in_hand) done;
         for i_w2 = (p_index+1) to (y_len-1)
         do (matrix.(r_index).(i_w2) <- Not_in_hand) done;)
    | Maybe_in_hand, Not_in_hand, Not_in_hand
      -> matrix.(s_index).(p_index) <- Known;
         for i_s1 = 0 to (p_index-1)
         do (matrix.(r_index).(i_s1) <- Not_in_hand) done;
         for i_s2 = (p_index+1) to (y_len-1)
         do (matrix.(r_index).(i_s2) <- Not_in_hand) done;
    | _,_,_ -> ());
  (* if the player only has n cards in hands and he already has n known,
    then any maybe_in_hand must be not_in_hand *)
    for y=0 to (y_len-1)
    do column_helper matrix y x_len player done;
  (* might need to do more to compile the data *)
  (* update not_in_hand if the player answering the guess is not adjacent
    to the player asking*)
    let asking_index = suspect_to_index public public.curr_player in
    let answering_index = suspect_to_index public str in
    adjacent_helper matrix s_index asking_index answering_index y_len;
    adjacent_helper matrix w_index asking_index answering_index y_len;
    adjacent_helper matrix r_index asking_index answering_index y_len;
  (* compile data *)
    (if all_but_one_known matrix public s_lst
    then compile_known matrix public s_lst l
    else ());
    (if all_but_one_known matrix public w_lst
    then compile_known matrix public w_lst l
    else ());
    (if all_but_one_known matrix public r_lst
    then compile_known matrix public r_lst l
    else ());
    (compile_notinhand matrix public x_len l);
    update_player player !l)