open View
open Cli
open Model
open Data
open Agent
open DumbAI

let Display = Cli

(*  *)
let get_cur_next plrs cur =
  let rec helper ps t =
    match t with
    | [] -> failwith "no player found"
    | h::[] when h.suspect = cur -> (h, List.hd ps)
    | c::n::t' -> if c.suspect = cur then (c, n) else helper ps (n::t')
  in helper plrs plrs

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
  match move with
  | RollDice -> begin
    let movement_opt = Model.get_movement_options ((Random.int 11) + 2) in
    let movement = Who.get_movement movement_opt in
    let curr_p' = {curr_p with curr_location = movement} in
    match movement with
    | Room(s, l) when s = "envelope" ->
    | _ -> (* Get Guess *)
  | Passage ->


let start file_name =
  let load_go fl = step (Model.import_board fl) in
  match file_name with
  | None -> load_go (Display.prompt_filename ())
  | Some s -> load_go fl