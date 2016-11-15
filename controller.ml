open View
open Cli
open Model
open Data
open Agent
open DumbAI

let Display = Cli

(* Gets the current player and the next player records *)
let get_cur_next plrs cur =
  let rec helper ps t =
    match t with
    | [] -> failwith "no player found"
    | h::[] when h.suspect = cur -> (h, List.hd ps)
    | c::n::t' -> if c.suspect = cur then (c, n) else helper ps (n::t')
  in helper plrs plrs

let string_of_movement l ops = match l with
  | Room(s, _) ->
    "Entered "^s
  | Space((x,y), _) ->
    "Landed on space "^(string_of_int x)^", "^(string_of_int y)

let handle_move m =
    match move with
    | Roll ->
      let dice_roll = (Random.int 11) + 2 in
      let () = Display.display_dice_roll dice_roll in
      let movement_opt = Model.get_movement_options dice_roll in
      Who.get_movement movement_opt
    | Passage l -> l

let handle_movement = function
  | Room(s, _) when s = game.public.acc_room -> `Accusation
  | Room(_, _) -> `Guess
  | _ -> `End_turn

let replace_player pl lst =
  let rec helper pls t =
    match t with
    | [] -> pls
    | pl'::t' ->
      if pl'.suspect = pl.suspect then helper (pl::pls) t'
      else helper (pl'::pls) t'
  in List.rev (helper [] lst)

(* May not be needed *)
let compare_guess (s1, w1, r1) (s2, w2, r2) =
  failwith ""

(* [step] Recursively progresses through the game by doing one agent turn
 * at a time. *)
let rec step game =
  let (curr_p, next_p) = get_cur_next game.players game.curr_player in
  let () = Display.display_turn game in
  let Who = match curr_p.agent with
            | DumbAI_t -> DumbAI
            | _ -> DumbAI in
  let move = Who.answer_move (Model.get_move_options game) in
  let () = Display.display_move move in
  let movement = handle_move move in
  let () = Who.display_movement (string_of_movement movement, movement) in
  let curr_p' = {curr_p with curr_location = movement} in
  match handle_movement movement with
  | `Accusation ->
    let guess = Who.get_accusation curr_p game.public in
    if guess = game.envelope then (* Win *)
    else (* Lose, out *)
  | `Guess -> failwith ""
  | `End_turn -> step {game with curr_player=next_p.suspect;
                                 players=(replace_player curr_p')}

let start file_name =
  let load_go fl = step (Model.import_board fl) in
  match file_name with
  | None -> load_go (Display.prompt_filename ())
  | Some s -> load_go fl