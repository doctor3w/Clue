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

(* [answer_move] gets the type of movement the agent wants to perform,
 * so either roll the dice or take a secret passage if possible  *)
let answer_move player public  move_list : move = failwith "responsiveai answer_move"

(* [get_movement] passes in a list of locations that could be moved to,
 * and returns the agent's choice of movement *)
let get_movement player public move_option_list: loc= failwith "responsiveai get_movement"

(* [get_guess] takes in a game sheet and the current location and returns
 * a card list of 1 room, 1 suspect, and 1 weapon that the agent guesses. *)
let get_guess player public :guess= failwith "responsiveai get_guess"
(* make a guess based on the priority of listens structure  *)

(* [show_card pl pu c g] updates the players sheet based on the new card seen
 * and the guess. If card is None, then that means no one had cards in the
 * guess and needs to be updated accordingly. Also needs to use process of
 * elimination for certain AIs. The string is who showed's suspect ID. *)
let show_card player public answer (s,w,r) :player =
(* TODO:
 what show_card needs to do is just to call take_note?
 *)

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


let none_helper (matrix:listens) public s_index w_index r_index = 
	let y_len = List.length public.player_order in
	for p_index1 = 0 to (y_len - 1)
		do (if (p_index1 = suspect_to_index public public.curr_player)
		 	then ()
		 	else ((matrix.(s_index)).(p_index1) <- Not_in_hand p_index1)) done;
	for p_index2 = 0 to (y_len - 1)
		do (if (p_index2 = suspect_to_index public public.curr_player)
		 	then ()
		 	else ((matrix.(w_index)).(p_index2) <- Not_in_hand p_index2)) done;
	for p_index3 = 0 to (y_len - 1)
		do (if (p_index3 = suspect_to_index public public.curr_player)
		 	then ()
		 	else ((matrix.(r_index)).(p_index3) <- Not_in_hand p_index3)) done;

let match_helper matrix x_index y_index = 
	let new_cell = 
		(match matrix.(x_index).(y_index) with
		| Pure_unknown -> Maybe_in_hand y_index
		| _ -> matrix.(x_index).(y_index)) in
	matrix.(x_index).(y_index) <- new_cell

(* return the number of rows where the cards are known for player [j].
	[i_len] is the number of total cards *)
let if_column_helper matrix j i_len= 
	let counter = ref 0 in
	for index = 0 to (i_len-1) 
	do (if matrix.(index).(j) = Known j 
		then counter := !counter + 1
		else ()) done;
	!counter

let column_helper matrix j i_len player = 
	if ((if_column_helper matrix j i_len) = List.length player.hand)
	then for index = 0 to (i_len - 1) 
		 do (if matrix.(index).(j) = Pure_unknown 
		 			|| matrix.(index).(j) = Maybe_in_hand j 
		 	 then matrix.(index).(j) <- Not_in_hand j
		 	 else ()) done
	else ()

let helper a =
  let len = Array.length a in
  let default = ref true in
  for index = 0 to (len-1)
    do (let case = (match a.(index) with
      | Not_in_hand j -> true
      | _ -> false) in
       default:= (!default) && case ) done;
  !default

let rewrite_env a = 
	let len = Array.length a in
	for index = 0 to (len-1) 
	do a.(index) <- Env done

(* [take_notes pl pu] updates the ResponsiveAIs sheet based on the listen data
 * in public. *)
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
		| Not_in_hand a, Not_in_hand b, Maybe_in_hand c 
			-> (matrix.(r_index).(p_index) <- Known c;
			   for i_r1 = 0 to (p_index-1)
			   do (matrix.(r_index).(i_r1) <- Not_in_hand i_r1) done;
			   for i_r2 = (p_index+1) to (y_len-1)
			   do (matrix.(r_index).(i_r2) <- Not_in_hand i_r2) done;)
		| Not_in_hand a, Maybe_in_hand b, Not_in_hand c 
			-> (matrix.(w_index).(p_index) <- Known b;
			   for i_w1 = 0 to (p_index-1)
			   do (matrix.(r_index).(i_w1) <- Not_in_hand i_w1) done;
			   for i_w2 = (p_index+1) to (y_len-1)
			   do (matrix.(r_index).(i_w2) <- Not_in_hand i_w2) done;)
		| Maybe_in_hand a, Not_in_hand b, Not_in_hand c 
			-> matrix.(s_index).(p_index) <- Known a;
			   for i_s1 = 0 to (p_index-1)
			   do (matrix.(r_index).(i_s1) <- Not_in_hand i_s1) done;
			   for i_s2 = (p_index+1) to (y_len-1)
			   do (matrix.(r_index).(i_s2) <- Not_in_hand i_s2) done;
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
			  do (matrix.(s_index).(new_i) <- Not_in_hand (new_i);
			 	  matrix.(w_index).(new_i) <- Not_in_hand (new_i);
			 	  matrix.(r_index).(new_i) <- Not_in_hand (new_i);) done)
		else (for new_i1 = 0 to (answering_index-1)
			  do  (matrix.(s_index).(new_i1) <- Not_in_hand (new_i1);
			 	   matrix.(w_index).(new_i1) <- Not_in_hand (new_i1);
			 	   matrix.(r_index).(new_i1) <- Not_in_hand (new_i1);) done; 
			  for new_i2 = (asking_index+1) to (y_len-1)
			  do  (matrix.(s_index).(new_i2) <- Not_in_hand (new_i2);
			 	   matrix.(w_index).(new_i2) <- Not_in_hand (new_i2);
			 	   matrix.(r_index).(new_i2) <- Not_in_hand (new_i2);) done);
	(* compile data *)
	(* if the entire row for a card is all filled up with Not_in_hand,
		it must be in the envelope *)
		let l = ref [] in
		for ii = 0 to (x_len - 1)
		do (if helper matrix.(ii)
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

  (* TODO: Right now take_notes only compares data - it does not process all
  information such as Known/Not_in_hand and probability. This is essentially a
  massive helper function that is needed for guess and movement (in both guess
  and movement, take_note is not changed - in cases where they are called to access
 information and use it)
  In other cases of processing others showing me card and others showing other
player card, we need to update the matrix in take note*)


