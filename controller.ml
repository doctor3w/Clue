open View
open Cli
open Model
open Data
open Agent

exception No_players
exception Player_not_found

module Display = Cli

(* Gets the current player and the next player records *)
let get_cur_next plrs cur =
  let rec helper ps t =
    match t with
    | [] -> raise Player_not_found
    | c::n::t' ->
      if c.suspect = cur then (c, n) else helper ps (n::t')
    | h::[] when h.suspect = cur -> (h, List.hd ps)
    | h::[] -> helper ps []
  in if plrs = [] then raise No_players else helper plrs plrs

let string_of_movement l = match l with
  | Room(s, _) ->
    "Entered "^s
  | Space((x,y), _) ->
    "Landed on space "^(string_of_int x)^", "^(string_of_int y)

let handle_move game curr_p m =
    match m with
    | Roll ->
      let dice_roll = (Random.int 11) + 2 in
      let () = Display.display_dice_roll dice_roll in
      let movement_opt = Model.get_movement_options game dice_roll in
      Agent.get_movement curr_p game.public movement_opt
    | Passage l -> l

let handle_movement game = function
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

let rec check_for_humans pls =
  match pls with
  | [] -> false
  | pl::t -> if not pl.is_out && pl.agent = Human_t then true
             else check_for_humans t

let rec check_all_out pls =
  match pls with
  | [] -> true
  | pl::t -> if pl.is_out then check_for_humans t else false

(* [step] Recursively progresses through the game by doing one agent turn
 * at a time.
 * Requires: game has at least one player. *)
let rec step game =
  let (curr_p, next_p) = get_cur_next game.players game.public.curr_player in
  if curr_p.is_out then
    if not (check_all_out game.players) then
      step {game with public={game.public with curr_player=next_p.suspect}}
    else Display.display_message "Game over."
  else
    let () = Display.display_turn game.public in
    let move_ops = Model.get_move_options game
    let move = Agent.answer_move curr_p game.public move_ops in
    let () = Display.display_move move in
    let movement = handle_move game curr_p move in
    let () = Display.display_movement (string_of_movement movement, movement) in
    let curr_p' = {curr_p with curr_loc = movement} in
    match handle_movement game movement with
    | `Accusation -> handle_accusation curr_p' next_p game
    | `Guess -> handle_guess curr_p next_p game
    | `End_turn -> handle_end_turn curr_p' next_p game

and handle_accusation curr_p next_p game =
  let guess = Agent.get_accusation curr_p game.public in
  if guess = game.envelope then
    Display.display_victory curr_p.suspect
  else (* Lose, out *)
    let message =
      (curr_p.suspect^" guessed incorrectly, and is out of the game.") in
    let () = Display.display_message message in
    let curr_p' = {curr_p with is_out = true} in
    let pls' = replace_player curr_p' game.players in
    let guard = (check_for_humans pls' || game.ai_only) in
    if guard && not (check_all_out pls') then
      let pub = {game.public with curr_player=next_p.suspect} in
      step {game with players = pls'; public = pub}
    else
      Display.display_message "Game over."

and handle_guess curr_p next_p game =
  failwith ""

and handle_end_turn curr_p next_p game =
  let pub = {game.public with curr_player=next_p.suspect} in
  let pls = replace_player curr_p game.players in
  step {game with public=pub; players=pls}

let start file_name =
  let load_go fl =
    try step (Model.import_board fl) with
    | No_players -> Display.display_error "No players in game file"
    | Player_not_found -> Display.display_error "No player with suspect name"
  in match file_name with
  | None -> load_go (Display.prompt_filename ())
  | Some s -> load_go s