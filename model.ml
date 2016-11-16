open Data
open Yojson.Basic



(*type player_temp = {
  p_id:string; play_ord:int; start:int*int
}

(* rect is x0,x1.y0,y1*)
type room_temp = {
  r_id:string;
  rect:(int*int*int*int);
  passages: string list;
  exits: (int*int) list
}*)

open Board

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

let deal_card p c =
  let sd' = {(CardMap.find c p.sheet) with card_info = Mine []} in
  {p with hand = c::p.hand;
          sheet = CardMap.add c sd' p.sheet}


(* d is a card list where all card types have been jumbled *)
let deal_hands game d =
  let p_count = List.length game.players in
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

let extract_dim j : int*int =
  let asc = YJ.to_assoc j in
  let r = List.assoc "row_count" asc |> YJ.to_int in
  let c = List.assoc "col_count" asc |> YJ.to_int in
  (r,c)

let make_temp_player json : player_temp =
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

let make_agent_lst j =
  let lst = YJ.to_list j in
  let f s = match s with
  | "Human" -> Human_t
  | "DumbAI" -> DumbAI_t
  | "SmartAI" -> SmartAI_t
  | "ResponsiveAI" -> ResponsiveAI_t
  | s -> failwith ("unrecongnized agent type: " ^ s) in

  let f' acc el =
    let el' = YJ.to_assoc el in
    let sus = List.assoc "suspect" el' |> YJ.to_string in
    let at = List.assoc "agent_type" el' |> YJ.to_string |> f in
    (sus, at)::acc in
  List.fold_left f' [] lst


let default_sheet full_deck =
  let f acc el = CardMap.add el {card_info=Unknown; note=No_Note} acc
  in  List.fold_left f CardMap.empty full_deck

let add_player game full_deck player_temp agent_lst =
  let p = {
    suspect = player_temp.p_id;
    hand = [];
    sheet = default_sheet full_deck;
    is_out = false;
    agent = List.assoc player_temp.p_id agent_lst;
    curr_loc = CoordMap.find player_temp.start game.public.board.loc_map
  } in {game with players = p::game.players}

(* [import_board] takes in a filename of a game configuration file and
 * converts the file into a usable game model for stepping through. *)
let import_board (file_name: string) : game =
  let json = load_json file_name in
  let asc = Yojson.Basic.Util.to_assoc json in
  let cardify_sus = (fun s -> Suspect s) in
  let cardify_weap = (fun s -> Weapon s) in
  let cardify_room = (fun s -> Room s) in
  let dim = extract_pair_from_assoc "dim" asc
            |> extract_dim in
  let acc_id = extract_pair_from_assoc "acc_room" asc
                 |> YJ.to_string in
  let weap_lst = extract_pair_from_assoc "weapons" asc
                 |> YJ.to_list
                 |> YJ.filter_string
                 |> List.map cardify_weap in
  let sus_temp_lst = extract_pair_from_assoc "suspects" asc
                     |> (YJ.convert_each make_temp_player)
                     |> List.sort (fun x y -> Pervasives.compare x.play_ord y.play_ord) in
  let sus_id_lst = List.map (fun x -> x.p_id) sus_temp_lst in
  let sus_lst = List.map cardify_sus sus_id_lst in
  let room_temp_lst = (extract_pair_from_assoc "rooms" asc)
                      |> (YJ.convert_each make_temp_room) in
  let room_id_lst = (List.map (fun x -> x.r_id) room_temp_lst)
                    |> List.filter (fun x' -> x' != acc_id) in
  let room_lst = List.map cardify_room room_id_lst in
  let deck = (sus_lst, weap_lst, room_lst) in
  let full_deck = sus_lst@(weap_lst@room_lst) in
  let (env, mixed_deck) = pick_env deck in
  let game = { game_init with
    envelope=env;
    public = { game_init.public with
      curr_player = (match sus_id_lst with
                     | h::t -> h
                     | _ -> failwith "no suspects found");
      acc_room = acc_id;
      deck = deck;
    }
  } in
  let board' = fill_board dim room_temp_lst in
  let game = {game with public = {game.public with board = board'}} in
  let agent_lst = extract_pair_from_assoc "agent_config" asc
                  |> make_agent_lst in
  let f' = (fun acc el -> add_player acc full_deck el agent_lst) in
  let game = List.fold_left f' game sus_temp_lst in
  deal_hands game full_deck


let get_curr_player (game: game) : player =
  let rec loop = function
  | [] -> failwith "can't find current player"
  | h::t -> if h.suspect = game.curr_player then h else loop t
in loop game.players


let remove_dups lst =
  let rec loop lst acc =
    match lst with
    | [] -> acc
    | h::t -> if List.mem h acc then loop t acc
              else loop t (h::acc)
  in loop lst []

(* [get_move_options] gets the options of Roll and Passage that the current
 * player can make. *)

let get_move_options (g : game) : move list =
  let cp = get_curr_player g in
  let add_if_room loc passages =
    match (loc.info) with
    | Room (s, _) -> (Passage loc)::passages
    | Space _ -> passages in
  let rec loop locs passages =
    match locs with
    | [] -> Roll::passages
    | h::t -> loop t (add_if_room h passages)
  in match cp.curr_loc.info with
    | Room (s, _) -> loop cp.curr_loc.edges []
    | Space _ -> [Roll]


(* [get_movement_options] gets the options of the locations that the current
 * player can move to. These options also come with a description in one of
 * the following fashions:
 *        head towards [room name]
 *        go into [room name] *)

let get_movement_options (g: game) (steps: int) : (string * loc) list =
  failwith "unimplemented"
  (* let rec f = (fun acc el -> step_loop el (steps-1) acc)
  and step_loop loc steps loclst =
    if steps = 0 then loclst else
    match (loc: loc) with
    | Room (name, lst) -> loc::loclst
    | Space ((r,c), lst) -> (List.fold_left f loclst lst)
  in let f' = (fun acc el -> match el with | Space _ -> f acc el | _ -> acc)
  in let start_loc = (get_curr_player g).curr_loc
  in let step1 = match start_loc with
                 Space(_, lst) | Room (_, lst) -> lst
  in let lst_dups = List.fold_left f' [] step1
  in let lst_nodups = remove_dups lst_dups
  in let lst_final = List.filter (fun x -> x != start_loc) lst_nodups
  in let room_name loc = match (loc: loc) with Room (n,_) -> n | _ -> failwith "not a room"
  in List.map (fun loc -> ("head towards " ^ (room_name loc), loc)) lst_final *)

