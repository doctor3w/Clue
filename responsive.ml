open Data

(* [answer_move] gets the type of movement the agent wants to perform,
 * so either roll the dice or take a secret passage if possible  *)
let answer_move player public  move_list : move = failwith "responsiveai answer_move"

(* [get_movement] passes in a list of locations that could be moved to,
 * and returns the agent's choice of movement *)
let get_movement player public move_option_list: loc= failiwith "responsiveai get_movement"

(* [get_guess] takes in a game sheet and the current location and returns
 * a card list of 1 room, 1 suspect, and 1 weapon that the agent guesses. *)
let get_guess player public :guess= failiwith "responsiveai get_guess"

(* [get_accusation] takes in a game sheet and the current location and returns
 * a card list of 1 room, 1 suspect, and 1 weapon that the agent thinks is
 * inside the envelope. *)
let get_accusation player public :guess= failiwith "responsiveai get_accusation"

(* [rand_from_lst] returns a random element in the list, 
    where [lst] is the input list *)
let rand_from_lst lst =
  let len = List.length lst in
  if len = 0 then failwith "no lst"
  else let n = Random.int len in
    List.nth lst n

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

(* [take_notes pl pu] updates the ResponsiveAIs sheet based on the listen data
 * in public. *)
let take_notes player public :player = failwith "responsiveai take_notes"

