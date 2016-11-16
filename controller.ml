open View
open Cli
open Model
open Data
open Agent

module Display = Cli

(* Gets the current player and the next player records *)
let get_cur_next plrs cur =
  let rec helper ps t =
    match t with
    | [] -> failwith "no player found"
    | c::n::t' ->
      if c.suspect = cur && not c.is_out then (c, n) else helper ps (n::t')
    | h::[] when h.suspect = cur -> (h, List.hd ps)
    | h::t' -> helper ps t'
  in helper plrs plrs

let string_of_movement l = match l with
  | Room(s, _) ->
    "Entered "^s
  | Space((x,y), _) ->
    "Landed on space "^(string_of_int x)^", "^(string_of_int y)

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

let remove_player pl lst =
  let rec helper pls t =
    match t with
    | [] -> pls
    | pl'::t' ->
      if pl'.suspect = pl.suspect then helper pls t'
      else helper (pl'::pls) t'
  in List.rev (helper [] lst)

(* May not be needed *)
let compare_guess (s1, w1, r1) (s2, w2, r2) =
  failwith ""

(* module WhoMake (A : Agent) = struct
  let answer_move pl pub moves = A.answer_move pl pub moves
  let get_movement pl pub move_ops = A.get_movement pl pub move_ops
  let get_guess pl pub = A.get_movement pl pub
  let get_accusation pl pub = A.get_accusation pl pub
  let get_answer pl pub g = A.get_answer pl pub g
end *)

(* [step] Recursively progresses through the game by doing one agent turn
 * at a time. *)
let rec step game =
  let (curr_p, next_p) = get_cur_next game.players game.public.curr_player in
  let () = Display.display_turn game.public in
  let move =
    Agent.answer_move curr_p game.public (Model.get_move_options game) in
  let () = Display.display_move move in
  let handle_move m =
    match m with
    | Roll ->
      let dice_roll = (Random.int 11) + 2 in
      let () = Display.display_dice_roll dice_roll in
      let movement_opt = Model.get_movement_options game dice_roll in
      Agent.get_movement curr_p game.public movement_opt
    | Passage l -> l in
  let movement = handle_move move in
  let () = Display.display_movement (string_of_movement movement, movement) in
  let curr_p' = {curr_p with curr_loc = movement} in
  match handle_movement game movement with
  | `Accusation ->
    let guess = Agent.get_accusation curr_p game.public in
    if guess = game.envelope then
      Display.display_victory curr_p.suspect
    else (* Lose, out *)
      ()
  | `Guess -> failwith ""
  | `End_turn ->
    let pub = {game.public with curr_player=next_p.suspect} in
    step {game with public=pub;
                    players=(replace_player curr_p' game.players)}

let start file_name =
  let load_go fl = step (Model.import_board fl) in
  match file_name with
  | None -> load_go (Display.prompt_filename ())
  | Some s -> load_go s