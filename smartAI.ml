open Data
open Model

(* returns a random element from the list [lst] *)
let rand_from_lst lst =
  let len = List.length lst in
  if len = 0 then failwith "no lst"
  else let n = Random.int len in
    List.nth lst n

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
  if knows_room then
    let f acc pass =
      match pass with
      | Passage r when is_my_room me r -> (Passage r)::acc
      | _ -> acc in
    let g acc pass =
      match pass with
      | Passage r when is_env_room me r -> (Passage r)::acc
      | _ -> acc in
    let my_rooms = List.fold_left f [] passages in
    if List.length my_rooms > 0 then rand_from_lst my_rooms
    else let env_room = List.fold_left g [] passages in
      if List.length env_room > 0 then rand_from_lst env_room else
      Roll
  else
    let h acc pass =
      match pass with
      | Passage r when is_unknown_room me r -> (Passage r)::acc
      | _ -> acc in
    let unknowns = List.fold_left h [] passages in
    if List.length unknowns > 0 then rand_from_lst unknowns else Roll

(* true if s is the acc_room, takes in an element from [movelst] below *)
let is_acc_room public (_, (s, _)) = s = public.acc_room

(* [get_movement] passes in a list of locations that could be moved to,
 * and returns the agent's choice of movement *)
let get_movement (me:player) public (movelst:(loc * (string * bool)) list) : loc =
  let sus_env = knows_room me in
  let weap_env = knows_room me in
  let room_env = knows_room me in
  let acc_id = public.acc_room in
  if sus_env && weap_env && room_env
  then
    match List.filter (is_acc_room public) movelst with
    | [(l, _)] -> l
    | _ -> failwith "can't find accusation room"
  else
    let b = (fun x -> not (is_acc_room public x)) in
    let movelst' = List.filter b movelst in
    if room_env
    then
      let f acc el = match el with
      | (l, (s, true)) when is_my_card me (Room s) -> l::acc
      | _ -> acc in
      let g acc el = match el with
      | (l, (s, true)) when is_env_card me (Room s) -> l::acc
      | _ -> acc in
      let h acc el = match el with
      | (l, (s, false)) when is_my_card me (Room s) -> l::acc
      | _ -> acc in
      let p acc el = match el with
      | (l, (s, false)) when is_env_card me (Room s) -> l::acc
      | _ -> acc in
      let fs = List.fold_left f [] movelst' in
      if List.length fs > 0 then rand_from_lst fs
      else let gs = List.fold_left g [] movelst' in
      if List.length gs > 0 then rand_from_lst gs
      else let hs = List.fold_left h [] movelst' in
      if List.length hs > 0 then rand_from_lst hs
      else let ps = List.fold_left p [] movelst' in
      if List.length ps > 0 then rand_from_lst ps
      else failwith ("impossible " ^ Pervasives.__LOC__)
    else
      let f' acc el = match el with
      | (l, (s, true)) when is_unknown_card me (Room s) -> l::acc
      | _ -> acc in
      let h' acc el = match el with
      | (l, (s, false)) when is_unknown_card me (Room s) -> l::acc
      | _ -> acc in
      let fs = List.fold_left f' [] movelst' in
      if List.length fs > 0 then rand_from_lst fs
      else let hs = List.fold_left h' [] movelst' in
      if List.length hs > 0 then rand_from_lst hs
      else failwith ("impossible " ^ Pervasives.__LOC__)


(* [get_guess] takes in a game sheet and the current location and returns
 * a card list of 1 room, 1 suspect, and 1 weapon that the agent guesses. *)
let get_guess (me:player) public : guess =
  let room = match me.curr_loc.info with
             | Room_Rect (s, _) -> (Room s)
             | _ -> failwith "trying to guess from not room" in
  let sus_env = knows_sus me in
  let weap_env = knows_weap me in
  let l = CardMap.bindings me.sheet in
  let s_u lst = let u = List.filter (fun (c, i) -> match c, i.card_info with
                                       | Suspect _, Unknown -> true
                                       | _ -> false) lst in
                  if List.length u > 0 then rand_from_lst u |> fst
                  else failwith "don't know sus, but no sus unknown" in
  let w_u lst = let u = List.filter (fun (c, i) -> match c, i.card_info with
                                       | Weapon _, Unknown -> true
                                       | _ -> false) lst in
                  if List.length u > 0 then rand_from_lst u |> fst
                  else failwith "don't know weap, but no weaps unknown" in
  let s_m lst = let m = List.filter (fun (c, i) -> match c, i.card_info with
                                       | Suspect _, Mine _ -> true
                                       | _ -> false) lst in
                  if List.length m > 0 then rand_from_lst m |> fst
                else
                  let n = List.filter (fun (c, i) -> match c, i.card_info with
                                       | Suspect _, Envelope -> true
                                       | _ -> false) lst in
                    if List.length n > 0 then rand_from_lst n |> fst
                    else failwith "no sus_mine/env for guess" in
  let w_m lst = let m = List.filter (fun (c, i) -> match c, i.card_info with
                                       | Weapon _, Mine _ -> true
                                       | _ -> false) lst in
                  if List.length m > 0 then rand_from_lst m |> fst
                  else
                    let n = List.filter (fun (c, i) -> match c, i.card_info with
                                       | Weapon _, Envelope -> true
                                       | _ -> false) lst in
                      if List.length n > 0 then rand_from_lst n |> fst
                      else failwith "no weap_mine/env for guess" in
  match sus_env, weap_env with
  | true, true    -> (s_m l, w_m l, room)
  | true, false   -> (s_m l, w_u l, room)
  | false, true   -> (s_u l, w_m l, room)
  | false, false  -> (s_u l, w_u l, room)

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
  (*let sus_sht = (CardMap.find sus me.sheet).card_info in
  let weap_sht = (CardMap.find weap me.sheet).card_info in
  let room_sht = (CardMap.find room me.sheet).card_info in*)
  let f acc el = match (CardMap.find el me.sheet).card_info with
                | Mine lst -> (el, lst)::acc
                | _ -> acc in
  let mine_info = List.fold_left f [] (sus::weap::[room]) in
  match mine_info with
  | [] -> None
  | [(c, lst)] -> Some c
  | lst -> Some (pick_to_show lst cp)

(* let env_nm card me =
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
 *)

(* [show_card pl pu c g] updates the players sheet based on the new card seen
 * and the guess. If card is None, then that means no one had cards in the
 * guess and needs to be updated accordingly. Also needs to use process of
 * elimination for certain AIs. The string is who showed's suspect ID. *)
(* let show_card (me:player) public (shown:(string * card) option) guess : player =
  let sht = (fun c -> CardMap.find c me.sheet) in
  let (s, w, r) = guess in
  let me' = match shown with
  | None -> me |> env_nm s |> env_nm w |> env_nm r
  | Some (sus, c) -> let old_sd = sht c in
                     let sht'  = CardMap.add
                     {old_c_info with card_info = (ShownBy sus)} me.sheet in
                     {me with sheet = sht'} in
  me' |> deduce_env *)

(* Adds [sus] to [pl]'s list of 'shown to people' for a specific card [card] *)
(* let show_person (me:player) card  (who:string) : player =
  let sd = CardMap.find card me.sheet in
  let shown = match sd.info with
              | Mine s -> s
              | _ -> failwith "showed card not in hand!" in
  let shown' = if List.mem who shown then shown else who::shown in
  {me with sheet=(CardMap.add card {sd with card_info = Mine shown'})} *)

(* [take_notes pl pu] updates the ResponsiveAIs sheet based on the listen data
 * in public. *)
let take_notes (me:player) public : player = me
