open Data
open Model

(* [answer_move] gets the type of movement the agent wants to perform,
 * so either roll the dice or take a secret passage if possible  *)
let answer_move  (me:player) public passages:(move list) : move =
failwith "unimplemented SmartAI.answer_move"

(* [get_movement] passes in a list of locations that could be moved to,
 * and returns the agent's choice of movement *)
let get_movement (me:player) public (movelst:(string * loc) list) : loc =
  failwith "unimplemented SmartAI.get_movement"

(* [get_guess] takes in a game sheet and the current location and returns
 * a card list of 1 room, 1 suspect, and 1 weapon that the agent guesses. *)
let get_guess (me:player) public : guess =
  failwith "unimplemented SmartAI.get_guess"

(* [get_accusation] takes in a game sheet and the current location and returns
 * a card list of 1 room, 1 suspect, and 1 weapon that the agent thinks is
 * inside the envelope. *)
let get_accusation (me:player) public : guess =
  failwith "unimplemented SmartAI.get_accusation"

let pick_to_show lst cp =
  let pre_shown = List.filter (fun (c, shn) -> List.mem cp shn) lst in
  match pre_shown with
  | [] -> List.nth lst (Random.int (List.length lst))
  | [(c, shn)] -> c
  | lst' -> List.nth lst' (Random.int (List.length lst'))

(* [get_answer] takes in a hand and the current guess and returns Some card
 * if a card from the hand and also in the list can be shown. Returns None
 * if no card can be shown. *)
let get_answer (me:player) public guess : card option =
  let (sus, weap, room) = guess in
  let cp = public.curr_player in
  let sus_sht = (CardMap.find sus m.sheet).card_info in
  let weap_sht = (CardMap.find weap m.sheet).card_info in
  let room_sht = (CardMap.find room m.sheet).card_info in
  let f acc el = match c with | Mine lst -> (c, lst)::acc | _ -> acc in
  let mine_info = List.fold_left f [] (sus::weap::room) in
  match mine_info with
  | [] -> None
  | [(c, lst)] -> Some c
  | lst -> Some (pick_to_show cp)

let env_nm card me =
  let f c = let sd = CardMap.find c me.sheet in
    match sd.card_info with
    | Unknown -> let sht' = CardMap.add c {sd with card_info = Envelope} me.sheet in
                 {me with sheet = sht'}
    | _ -> me

let deduce_env me:player =
  let bindings = CardMap.bindings me.sheet in
  let fs = (fun (c,i) -> match c with | Suspect _ -> true | _ -> false ) in
  let fw = (fun (c,i) -> match c with | Weapon _ -> true | _ -> false ) in
  let fr = (fun (c,i) -> match c with | Room _ -> true | _ -> false ) in
  let suss = List.filter fs bindings in
  let weaps = List.filter fw bindings in
  let rooms = List.filter fr bindings in
  let count_unkn = (fun acc (c, i) -> match i.card_info with
                                      | Unknown -> acc+1
                                      | _ -> acc) in
  let env_un acc (c, i) =
    match i.card_info with
    | Unknown -> CardMap.add c {i with card_info = Envelope} acc
    | _ -> acc in
  let f lst acc = if List.fold_left count_unkn 0 lst = 1
                  then List.fold_left env_un acc lst
                  else acc in
  let sht' = me.sheet |> f suss |> f weaps |> f rooms in
  {me with sheet = sht'}


(* [show_card pl pu c g] updates the players sheet based on the new card seen
 * and the guess. If card is None, then that means no one had cards in the
 * guess and needs to be updated accordingly. Also needs to use process of
 * elimination for certain AIs. The string is who showed's suspect ID. *)
let show_card (me:player) public (shown:(string * card) option) guess : player =
  let sht = (fun c -> CardMap.find c me.sheet) in
  let (s, w, r) = guess in
  let me' = match shown with
  | None -> me |> env_nm s |> env_nm w |> env_nm r
  | Some (sus, c) -> let old_sd = sht c in
                     let sht'  = CardMap.add
                     {old_c_info with card_info = (ShownBy sus)} me.sheet in
                     {me with sheet = sht'} in
  me' |> deduce_env

(* Adds [sus] to [pl]'s list of 'shown to people' for a specific card [card] *)
let show_person (me:player) card  (who:string) : player =
  let sd = CardMap.find card me.sheet in
  let shown = match sd.info with
              | Mine s -> s
              | _ -> failwith "showed card not in hand!" in
  let shown' = if List.mem who shown then shown else who::shown in
  {me with sheet=(CardMap.add card {sd with card_info = Mine shown'})}

(* [take_notes pl pu] updates the ResponsiveAIs sheet based on the listen data
 * in public. *)
let take_notes (me:player) public : player =
  failwith "unimplemented SmartAI.take_notes"