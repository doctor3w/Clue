open Data

module Display = Cli

let rec find x lst =
    match lst with
    | [] -> failwith "Not Found"
    | h :: t -> if x = h then 0 else 1 + find x t

let suspect_to_index public (sus:string) : int =
	find sus public.player_order

let index_to_suspect public i : string = List.nth public.player_order i

let card_to_index public (card:card) : int =
	let (s_lst, w_lst, r_lst) = public.deck in
	let deck' = s_lst @ w_lst @ r_lst in
	find card deck'

let index_to_card public i : card =
	let (s_lst, w_lst, r_lst) = public.deck in
	let deck' = s_lst @ w_lst @ r_lst in
	List.nth deck' i

let rand_from_lst lst =
  let len = List.length lst in
  if len = 0 then failwith "no lst"
  else let n = Random.int len in
    List.nth lst n

(* count how many [t] the array [a] has *)
let count_listenchoice a t =
  let counter = ref 0 in
  let len = Array.length a in
  for index = 0 to (len-1)
  do (if a.(index) = t
     then counter := !counter + 1
     else ()) done;
  !counter

(* check if the entire array [a] is all Not_in_hand*)
let is_all_notinhand a =
  let count = count_listenchoice a Not_in_hand in
  Array.length a = count

let rec helper matrix public (lst: card list) counter =
  match lst with
  | [] -> ()
  | h::t ->
            (let h_index = card_to_index public h in
            (if count_listenchoice matrix.(h_index) Known = 1
            then counter:= !counter +1
            else ());
            helper matrix public t counter)

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
  for index = 0 to (len-1)
  do a.(index) <- Env done

(* turns a passage into a room card *)
let p_to_room passage =
  match passage with
  | Roll -> failwith "not gonna happen"
  | Passage n ->
    match n.info with
    | Room_Rect (r,_) -> Room r
    | _ -> failwith "not a room card"

(* [is_r_env_known] checks if player knows the room card in envelope *)
let is_r_env_known player =
  let s = CardMap.filter (fun _ data -> (data.card_info = Envelope)) player.sheet in
  let b = CardMap.bindings s in
  let r = List.filter (fun (c,i) -> match c with
                                    | Room (n)-> true
                                    | _->false) b in
  not(r = [])

(* [check_p_farthest] checks if the farthest player from me knows any of the
 * passages *)
let rec check_p_farthest player public new_p_lst =
  match new_p_lst with
  | []-> false
  | h::t-> let r = p_to_room h in
            let r_i = card_to_index public r in
            let pi = suspect_to_index public player.suspect in
            let farthest = if pi = (List.length public.player_order)-1 then 0
                           else pi-1 in
            (player.listen.(r_i).(farthest) = Known )
            || check_p_farthest player public t

(* returns a list of passages that are not Known in listens *)
let rec check_p_known player public passage_list =
  match passage_list with
  | [] -> []
  | h::t->let r = p_to_room h in
          let r_i = card_to_index public r in
          if Array.exists (fun x -> x = Known) player.listen.(r_i) then
            check_p_known player public t
          else h:: check_p_known player public t

let my_max lst = match lst with
  | [] -> failwith ""
  | x::xs -> List.fold_left max x xs

(* returns either Roll or Passage with most not_in_hand *)
let rec p_most_not_in_hand player public passage_list =
  let r_p_lst = List.map (fun p -> (p_to_room p,p)) passage_list in
  let ri_p_lst = List.map (fun (r,p) -> (card_to_index public r,p)) r_p_lst in
  let counted_p =
    List.map (fun (ri,p)->
             ((count_listenchoice player.listen.(ri) Not_in_hand),p)) ri_p_lst in
  let c_lst = List.map (fun (c,p)-> c) counted_p in
  let ci = my_max c_lst in
    if ci = 0 then Roll else
    List.assoc ci counted_p

(* [is_p_env] checks that if the passage room is in the envelope then ROLL
  else it calls its helper [check_farthest] to further determine move*)
let is_p_env player public passage_list =
  if is_r_env_known player then
    let f p =
            (let r = p_to_room p in
            match (CardMap.find r player.sheet).card_info with
            | Envelope -> true
            | _ -> false )in
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
    else Roll


(* [answer_move] gets the type of movement the agent wants to perform,
 * so either roll the dice or take a secret passage if possible  *)
let answer_move player public move_list : move =
  let passage = List.filter (fun a -> match a with
                                      | Roll -> false
                                      | Passage _-> true) move_list in
  if passage  = [] then Roll else is_p_env player public passage

(* [get_movement] passes in a list of locations that could be moved to,
 * and returns the agent's choice of movement *)
let get_movement player public move_option_list: loc= failwith "responsiveai get_movement"


(* [get_guess] takes in a game sheet and the current location and returns
 * a card list of 1 room, 1 suspect, and 1 weapon that the agent guesses. *)
let get_guess player public :guess= failwith "responsiveai get_guess"


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
let listen_ans_update listen sus card player public =
  let c_index = card_to_index public card in
  let sus_index = suspect_to_index public sus in
  let pl_index = suspect_to_index public player.suspect in
    listen.(c_index).(sus_index)<-Known;
    if pl_index < sus_index then
      for j = pl_index + 1 to (sus_index -1)
      do (listen.(c_index).(j)<-Not_in_hand) done
    else
      for j = sus_index - 1 downto 0
      do (listen.(c_index).(j)<-Not_in_hand) done;
      for j = pl_index + 1 to List.length public.player_order -1
      do (listen.(c_index).(j)<-Not_in_hand) done;
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
      let sheet' = unk_to_env s pl.sheet |> unk_to_env w |> unk_to_env r in
      listen_unk_to_env pl.listen pl public (s,w,r);{pl with sheet = sheet'}
  | Some (sus, card) ->
      let data = CardMap.find card pl.sheet in
      let data' = {data with card_info= ShownBy(sus)} in
      let sheet' = CardMap.add card data' pl.sheet in
      listen_ans_update pl.listen sus card pl public;
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
  | lst' -> match (room_not_preferred lst') with
            | [] -> fst (rand_from_lst lst')
            | _  -> fst (rand_from_lst (room_not_preferred lst'))

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
  | lst' -> match (room_not_preferred lst') with
            | [] -> fst (rand_from_lst lst')
            | _  -> fst (rand_from_lst (room_not_preferred lst'))

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
      else ((matrix.(s_index)).(p_index1) <- Not_in_hand )) done;
  for p_index2 = 0 to (y_len - 1)
    do (if (p_index2 = suspect_to_index public public.curr_player)
      then ()
      else ((matrix.(w_index)).(p_index2) <- Not_in_hand )) done;
  for p_index3 = 0 to (y_len - 1)
    do (if (p_index3 = suspect_to_index public public.curr_player)
      then ()
      else ((matrix.(r_index)).(p_index3) <- Not_in_hand )) done

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

(* [take_notes] is only called

  when another player is showing a card to another player
  /no one could show a card to another player.

  It takes four inputs:  player, public, current_guess and string_option,
  where player is responsive AI itself, public is just the type public,
  guess is the current guess by some other player and
  string_option is if there is a player having any card to show him.

  It returns a new player as the output
  where we mainly change the player.listen and possibly player.sheet.
 *)

 (* !!!!!!!!!!!!! need to update the note when responsive AI gets the hand.
    However, take_note is not called at that time *)
let take_notes player public guess str_option: player =
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
         player)
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
    column_helper matrix p_index x_len player;
  (* might need to do more to compile the data *)
  (* update not_in_hand if the player answering the guess is not adjacent
    to the player asking*)
    let asking_index = suspect_to_index public player.suspect in
    let answering_index = suspect_to_index public str in
    if (asking_index < answering_index)
    then (for new_i = (asking_index+1) to (answering_index-1)
        do (matrix.(s_index).(new_i) <- Not_in_hand;
          matrix.(w_index).(new_i) <- Not_in_hand;
          matrix.(r_index).(new_i) <- Not_in_hand;) done)
    else (for new_i1 = 0 to (answering_index-1)
        do  (matrix.(s_index).(new_i1) <- Not_in_hand;
           matrix.(w_index).(new_i1) <- Not_in_hand;
           matrix.(r_index).(new_i1) <- Not_in_hand;) done;
        for new_i2 = (asking_index+1) to (y_len-1)
        do  (matrix.(s_index).(new_i2) <- Not_in_hand;
           matrix.(w_index).(new_i2) <- Not_in_hand;
           matrix.(r_index).(new_i2) <- Not_in_hand;) done);
  (* compile data *)
  (* if the entire row for a card is all filled up with Not_in_hand,
    it must be in the envelope *)
    let l = ref [] in
    for ii = 0 to (x_len - 1)
    do (if is_all_notinhand matrix.(ii)
        then (rewrite_env matrix.(ii);
          l:= (index_to_card public ii)::(!l))
        else ()) done;
    let rec update_player player l = match !l with
      | [] -> player
      | h::t ->
          let data = CardMap.find h player.sheet in
            let data' = {data with card_info = Envelope} in
            let sheet' = CardMap.add h data' player.sheet in
            update_player {player with sheet = sheet'} (ref t)
    in update_player player l)
