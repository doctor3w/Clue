open Data

module Display = Cli

let rec find x lst =
    match lst with
    | [] -> failwith "Not Found"
    | h :: t -> if x = h then 0 else 1 + find x t

let suspect_to_index public sus =
	find sus public.fixed_players

let card_to_index public card =
	let (s_lst, w_lst, r_lst) = public.deck in
	let deck' = s_lst @ w_lst @ r_lst in
	find card deck'

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

let helper a =
  let len = Array.length a in
  let default = ref true in
  for index = 0 to (len - 1)
    do (let case = (match a.(index) with
      | (Not_in_hand i, f) -> true
      | _ -> false) in
       default:= (!default) && case ) done;
  !default

(* Given a string [sus], a string [curr_player] and a string list [players], 
   return true if [sus] is adjacent to [curr_player] (either left or right)
   PreC: curr_player and sus are in the players *)
let is_adjacent (players: string list)(curr_player:string)(sus:string):bool =
  let len = List.length players in
  let index_curr = find curr_player players in
  let index_sus = find sus players in
  if (index_curr = len - 1) then (if index_sus = len - 2 || index_sus = 0 
                      then true else false)
  else if (index_curr = 0) then (if index_sus = 1 || index_sus = len - 1
                  then true else false)
  else (if index_sus = index_curr - 1 || index_sus = index_curr + 1 
          then true else false)

(* [take_notes pl pu] updates the ResponsiveAIs sheet based on the listen data
 * in public. *)
let take_notes player public : player = failwith "Unimplemented"
  (* TODO: Right now take_notes only compares data - it does not process all
  information such as Known/Not_in_hand and probability. This is essentially a
  massive helper function that is needed for guess and movement (in both guess
  and movement, take_note is not changed - in cases where they are called to access
 information and use it)
  In other cases of processing others showing me card and others showing other
player card, we need to update the matrix in take note*)


