open Data
open Yojson.Basic

exception BadConfig

let load_json file_name =
  let len = String.length file_name in
  if len < 5 then raise BadConfig (* can't end in .json *)
  else if((String.sub file_name (len-5) 5) = ".json")
  then try (Yojson.Basic.from_file file_name) with _ -> raise BadConfig
  else raise BadConfig

(* pre: the lists are non-empty *)
let pick_env sus_lst weap_lst room_lst : (card * card * card) * card list =
  let sus_shuff = List.permute sus_lst in
  let weap_shuff = List.permute weap_lst in
  let room_shuff = List.permute room_lst in
  match (sus_shuff, weap_shuff, room_shuff) with
  | ((h1:t1), (h2:t2), (h3:t3)) -> ((h1, h2, h3), t1@t2@t3)
  | ([],_,_) | (_,[],_) | (_,_,[]) -> raise BadConfig

let deal_hands game deck =
  let p_count = List.length game.players in
  let d_count = List.length deck in
  let

(* [import_board] takes in a filename of a game configuration file and
 * converts the file into a usable game model for stepping through. *)

let import_board (file_name: string) : game =
  let json = load_json file_name in


  let (env * deck) = pick_env sus_lst weap_lst room_lst in




(* [get_move_options] gets the options of Roll and Passage that the current
 * player can make. *)
let get_move_options (g: game) : move list =
  failwith "unimplemented"

(* [get_movement_options] gets the options of the locations that the current
 * player can move to. These options also come with a description in one of
 * the following fashions:
 *        head towars [room name]
 *        go into [room name] *)

let get_movement_options (game: game) (steps: int) : (string * loc) list =
  failwith "unimplemented"