open Data
open Yojson.Basic

exception BadConfig

module YJ = Yojson.Basic.Util

let load_json file_name =
  let len = String.length file_name in
  if len < 5 then raise BadConfig (* can't end in .json *)
  else if((String.sub file_name (len-5) 5) = ".json")
  then try (Yojson.Basic.from_file file_name) with _ -> raise BadConfig
  else raise BadConfig

let shuffle_lst lst =
  failwith "unimplemented"

(* pre: the lists are non-empty *)
let pick_env (sus_lst, weap_lst, room_lst) : (card * card * card) * card list =
  let sus_shuff = shuffle_lst sus_lst in
  let weap_shuff = shuffle_lst weap_lst in
  let room_shuff = shuffle_lst room_lst in
  match (sus_shuff, weap_shuff, room_shuff) with
  | ((h1::t1), (h2::t2), (h3::t3)) -> ((h1, h2, h3), t1@t2@t3)
  | ([],_,_) | (_,[],_) | (_,_,[]) -> raise BadConfig

(* d is a card list where all card types have been jumbled *)
let deal_hands game d =
  let p_count = List.length game.players in
  let d_count = List.length d in
  let deal_card = (fun p c -> {p with hand = c::p.hand}) in
  let d_shuff = shuffle_lst d in
  let rec loop n d players =
    match d with
    | [] -> players
    | h::t -> let f = (fun i p -> if (n mod p_count) = i
                                  then deal_card p h
                                  else p)
              in loop (n+1) t (List.mapi f players)
  in {game with players = (loop 0 d_shuff game.players)}

let extract_pair_from_assoc s asc =
  let rec loop = function
  | [] -> failwith ("no such item: " ^ s)
  | (h,p)::t -> if h = s then p else loop t
in loop asc

type player_temp = {
  p_id:string; play_ord:int; start:int*int
}

(* rect is x0,x1.y0,y1*)
type room_temp = {
  r_id:string;
  rect:(int*int*int*int);
  passages: string list;
  exits: (int*int) list
}

let extract_coord j : int*int =
  match (Yojson.Basic.Util.to_assoc j) with
  | ((s1,n1)::(s2,n2)::[]) -> let r = if s1 = "row"
                                      then Yojson.Basic.Util.to_int n1
                                      else Yojson.Basic.Util.to_int n2
                           in let c = if s1 = "col"
                                      then Yojson.Basic.Util.to_int n1
                                      else Yojson.Basic.Util.to_int n2
                           in (r, c)
  | _ -> failwith "invalid coord"

let make_temp_player json =
  let asc = Yojson.Basic.Util.to_assoc json in
  let p_temp = {p_id = ""; play_ord = -1; start = (-1, -1)} in
  let rec loop lst acc =
  match lst with
  | [] -> acc
  | ("id", s)::t -> let acc = {acc with p_id = (Yojson.Basic.Util.to_string s)}
                 in loop t acc
  | ("play_order", n)::t -> let acc = {acc with play_ord =
                                      (Yojson.Basic.Util.to_int n)}
                            in loop t acc
  | ("start_space", j)::t -> let coord = extract_coord j
                          in let acc = {acc with start = coord}
                          in loop t acc
  | (s,_)::t -> failwith ("can't recognize within player: " ^ s)
  in loop asc p_temp

type rt = {left:int; top:int; right:int; bottom:int}
let extract_rect r =
  let asc = Yojson.Basic.Util.to_assoc r in
  let rct = {left = -1; top = -1; right = -1; bottom = -1} in
  let rec loop lst acc =
    match lst with
    | [] -> acc
    | ("left", n)::t -> let acc = {acc with left=(YJ.to_int n)}
                        in loop t acc
    | ("right", n)::t -> let acc = {acc with right=(YJ.to_int n)}
                        in loop t acc
    | ("top", n)::t -> let acc = {acc with top=(YJ.to_int n)}
                        in loop t acc
    | ("bottom", n)::t -> let acc = {acc with bottom=(YJ.to_int n)}
                        in loop t acc
    | (s,_)::t -> failwith ("can't recognize within rectangle: " ^ s)
  in let rrt = loop asc rct in (rrt.left, rrt.right, rrt.bottom, rrt.top)

let make_temp_room json =
  let asc = Yojson.Basic.Util.to_assoc json in
  let r_temp = {r_id =""; rect =(-1, -1, -1, -1); passages = []; exits = []} in
  let rec loop lst acc =
    match lst with
    | [] -> acc
    | ("id", s)::t -> let acc = {acc with r_id = (YJ.to_string s)}
                      in loop t acc
    | ("rect", r)::t -> let acc = {acc with rect = (extract_rect r)}
                        in loop t acc
    | ("passages", lst)::t -> let ps = YJ.filter_string (YJ.to_list lst)
                              in let acc = {acc with passages = ps}
                              in loop t acc
    | ("exits", lst)::t -> let exs =
                              YJ.convert_each (extract_coord) lst
                           in let acc = {acc with exits = exs}
                           in loop t acc
    | (s,_)::t -> failwith ("can't recognize within room: " ^ s)
  in loop asc r_temp


(* [import_board] takes in a filename of a game configuration file and
 * converts the file into a usable game model for stepping through. *)

let import_board (file_name: string) : game =
  let json = load_json file_name in
  let asc = Yojson.Basic.Util.to_assoc json in
  let cardify_sus = (fun s -> Suspect s) in
  let cardify_weap = (fun s -> Weapon s) in
  let cardify_room = (fun s -> Room_c s) in
  let weap_lst = extract_pair_from_assoc "weapons" asc
                 |> YJ.to_list
                 |> YJ.filter_string
                 |> List.map cardify_weap in
  let sus_temp_lst = extract_pair_from_assoc "suspects" asc
                     |> YJ.to_list
                     |> YJ.convert_each make_temp_player in
  let sus_id_lst = List.map sus_temp_lst (fun x -> x.p_id) in
  let sus_lst = List.map sus_id_lst cardify_sus in
  let room_temp_lst = extract_pair_from_assoc "rooms" asc
                      |> YJ.to_list
                      |> (YJ.convert_each make_temp_room) in
  let room_id_lst = List.map room_temp_lst (fun x -> x.r_id) in
  let room_lst = List.map room_id_lst cardify_room in
  let deck = (sus_lst, weap_lst, room_lst) in
  let game = () in
  let (env, mixed_deck) = pick_env deck in
  game



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