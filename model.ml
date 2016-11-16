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
let pick_env (sus_lst, weap_lst, room_lst) : (card * card * card) * card list =
  let sus_shuff = List.permute sus_lst in
  let weap_shuff = List.permute weap_lst in
  let room_shuff = List.permute room_lst in
  match (sus_shuff, weap_shuff, room_shuff) with
  | ((h1:t1), (h2:t2), (h3:t3)) -> ((h1, h2, h3), t1@t2@t3)
  | ([],_,_) | (_,[],_) | (_,_,[]) -> raise BadConfig

(* d is a card list where all card types have been jumbled *)
let deal_hands game d =
  let p_count = List.length game.players in
  let d_count = List.length deck in
  let deal_card = (fun p -> c -> {p with hand = c::hand}) in
  let d_shuff = List.permute d in
  let rec loop n d players =
    match d with
    | [] -> players
    | h::t -> let f = (fun i a -> if (n mod p_count) = i
                                  then deal_card p h
                                  else p))
              in loop (n+1) t (List.mapi players f)
  in {game with players = (loop 0 d_shuff game.players)}

let extract_pair_from_assoc s asc =
  let rec loop = function
  | [] -> failwish "no such item: " ^ s
  | (h,p)::t -> if h = s then p else loop t
in loop asc

type player_temp = {
  id:string; play_ord:int; start:int*int
}

let make_temp_player json =
  let asc = Yojson.Basic.Util.to_assoc json in
  let p_temp = {id=""; play_ord=-1; start=(=1,-1)} in
  let rec loop lst acc =
  match lst with
  | [] -> p_temp
  | ("id", s)::t -> let acc = {acc with id = (Yojson.Basic.Util.to_string x)}
                 in loop t acc
  | ("play_order", n)::t -> let acc = {acc with play_ord =
                                      (Yojson.Basic.Util.to_int n)}
                            in loop t acc
  | ("start_space", j)::t -> let ((s1,n1)::(s2,n2::[]) =
                                                Yojson.Basic.Util.to_assoc j in
                          in let r = if s1 = "row"
                                     then Yojson.Basic.Util.to_int n1
                                     else Yojson.Basic.Util.to_int n2
                          in let c = if s1 = "col"
                                     then Yojson.Basic.Util.to_int n1
                                     else Yojson.Basic.Util.to_int n2
                          in let acc = {acc with start = (r, c)}
                          in loop t acc
  in loop asc p_temp



(* [import_board] takes in a filename of a game configuration file and
 * converts the file into a usable game model for stepping through. *)

let import_board (file_name: string) : game =
  let json = load_json file_name in
  let main_asc = Yojson.Basic.Util.to_assoc json in
  let cardify_weap = (fun s -> Weapon s) in
  let weap_lst = extract_pair_from_assoc "weapons" asc
                 |> Yojson.Basic..filter_string
                 |> (fun x -> List.map x cardify_weap) in
  let deck = (sus_lst, weap_lst, room_lst) in
  let game =
  let (env * mixed_deck) = pick_env sus_lst weap_lst room_lst in
  let



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