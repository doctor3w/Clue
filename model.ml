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
exception Not_json

module YJ = Yojson.Basic.Util

(* loads a json object from file [file_name] *)
let load_json file_name =
  let len = String.length file_name in
  if len < 5 then failwith "not a json file" (* can't end in .json *)
  else if ((String.sub file_name (len-5) 5) = ".json") then
    Yojson.Basic.from_file file_name
  else failwith ("not a .json, filename: " ^ file_name)

(* returns a shuffled copy of [lst] *)
let shuffle_lst lst =
  let len = List.length lst in
  let weight = List.map (fun x -> (Random.int (len*10), x)) in
  let sort = List.sort (fun (x1,x2) (y1,y2) -> Pervasives.compare x1 y1) in
  let unweight = List.map (fun (x1,x2) -> x2) in
  lst |> weight |> sort |> unweight

(* pre: the lists are non-empty *)
let pick_env (sus_lst, weap_lst, room_lst) : (card * card * card) * card list =
  let sus_shuff = shuffle_lst sus_lst in
  let weap_shuff = shuffle_lst weap_lst in
  let room_shuff = shuffle_lst room_lst in
  match (sus_shuff, weap_shuff, room_shuff) with
  | ((h1::t1), (h2::t2), (h3::t3)) -> ((h1, h2, h3), t1@t2@t3)
  | ([],_,_) | (_,[],_) | (_,_,[]) -> failwith "bad deck, ln 46"

(* used by deal_hands to add an individual card [c] to the hand of
 * player [p] *)
let deal_card p c =
  let sd = try (CardMap.find c p.sheet) with _ -> failwith "can't deal card" in
  let sd' = {sd with card_info = Mine []} in
  {p with hand = c::p.hand; sheet = CardMap.add c sd' p.sheet}

(* d is a card list where all card types have been jumbled *)
let deal_hands game d =
  let p_count = List.length game.players in
  let d_shuff = shuffle_lst d in
  let rec loop n d players =
    match d with
    | [] -> players
    | h::t ->
      let f i p = if (n mod p_count) = i then deal_card p h else p in
      loop (n+1) t (List.mapi f players)
  in {game with players = (loop 0 d_shuff game.players)}

(* functionally similar to List.assoc *)
let extract_pair_from_assoc s asc =
  let rec loop = function
  | [] -> failwith ("no such item: " ^ s)
  | (h,p)::t -> if h = s then p else loop t
in loop asc

(* extracts a coordinate [(int*int)] from json [j]
 * [j] must define "row" and "col" *)
let extract_coord j : int*int =
  match (Yojson.Basic.Util.to_assoc j) with
  | ((s1,n1)::(s2,n2)::[]) ->
    let y =
      if s1 = "row" then Yojson.Basic.Util.to_int n1
      else Yojson.Basic.Util.to_int n2 in
    let x =
      if s1 = "col" then Yojson.Basic.Util.to_int n1
      else Yojson.Basic.Util.to_int n2 in
    (x, y)
  | _ -> failwith "invalid coord"

let extract_color j : int*int*int =
  match (Yojson.Basic.Util.to_assoc j) with
  | [r; g; b] -> let r'= snd r |> Yojson.Basic.Util.to_int in
                 let g'= snd g |> Yojson.Basic.Util.to_int in
                 let b'= snd b |> Yojson.Basic.Util.to_int in
                 (r', g', b')
  | _ -> failwith ("color of wrong form: " ^ Pervasives.__LOC__)

(* extracts the dimensions (row_count * col_count) from json [j] *)
let extract_dim j : int*int =
  let asc = YJ.to_assoc j in
  let r = List.assoc "row_count" asc |> YJ.to_int in
  let c = List.assoc "col_count" asc |> YJ.to_int in
  (r,c)

(* extracts a [player_temp] record from [json] *)
let make_temp_player json : player_temp =
  let asc = Yojson.Basic.Util.to_assoc json in
  let p_temp = {p_id =""; play_ord = -1; start = (-1,-1); color = (-1,-1,-1)} in
  let rec loop lst acc =
  match lst with
  | [] -> acc
  | ("id", s)::t ->
    let acc = {acc with p_id = (Yojson.Basic.Util.to_string s)}
    in loop t acc
  | ("play_order", n)::t ->
    let acc = {acc with play_ord = (Yojson.Basic.Util.to_int n)}
    in loop t acc
  | ("start_space", j)::t ->
    let coord = extract_coord j
    in let acc = {acc with start = coord}
    in loop t acc
  | ("color", j)::t ->
    let color = extract_color j
    in let acc = {acc with color = color}
    in loop t acc
  | (s,_)::t ->
    failwith ("can't recognize within player: " ^ s)
  in loop asc p_temp

(* used as a rectangle that defines the bounds of a room *)
type rt = {left:int; top:int; right:int; bottom:int}

(* extracts an [rt[ from the json [r] *)
let extract_rect r =
  let asc = Yojson.Basic.Util.to_assoc r in
  let rct = {left = -1; top = -1; right = -1; bottom = -1} in
  let rec loop lst acc =
    match lst with
    | [] -> acc
    | ("left", n)::t ->
      let acc = {acc with left=(YJ.to_int n)}
      in loop t acc
    | ("right", n)::t ->
      let acc = {acc with right=(YJ.to_int n)}
      in loop t acc
    | ("top", n)::t ->
      let acc = {acc with top=(YJ.to_int n)}
      in loop t acc
    | ("bottom", n)::t ->
      let acc = {acc with bottom=(YJ.to_int n)}
      in loop t acc
    | (s,_)::t ->
      failwith ("can't recognize within rectangle: " ^ s)
  in let rrt = loop asc rct in (rrt.left, rrt.right, rrt.bottom, rrt.top)

(* extracts a [temp_room] record from [json] *)
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

(* extracts an agent_type from [j] *)
let make_agent_lst j =
  let lst = YJ.to_list j in
  let f s = match s with
    | "Human" -> Human_t
    | "DumbAI" -> DumbAI_t
    | "SmartAI" -> SmartAI_t
    | "ResponsiveAI" -> ResponsiveAI_t
    | "RandomAI" -> let n = Random.int 3 in
                    if      n = 0 then DumbAI_t
                    else if n = 1 then SmartAI_t
                    else               ResponsiveAI_t
    | s -> failwith ("unrecongnized agent type: " ^ s) in
  let f' acc el =
    let el' = YJ.to_assoc el in
    let sus = List.assoc "suspect" el' |> YJ.to_string in
    let at = List.assoc "agent_type" el' |> YJ.to_string |> f in
    (sus, at)::acc in
  List.fold_left f' [] lst

(* returns the default_sheet out of [deck] with [Unknown] for every entry *)
let default_sheet full_deck =
  let f acc el = CardMap.add el {card_info=Unknown; note=No_Note} acc
  in  List.fold_left f CardMap.empty full_deck

(* returns [game] with an extra player corresponding to [player_temp] *)
let add_player game full_deck player_temp agent_lst =
  let loc =
    try (CoordMap.find player_temp.start game.public.board.loc_map) with
    | _ -> failwith "can't find start coord" in
  let dcount = (List.length full_deck) in
  let pcount = (List.length agent_lst) in
  let p = {
    suspect = player_temp.p_id;
    hand = [];
    sheet = default_sheet full_deck;
    is_out = false;
    agent = List.assoc player_temp.p_id agent_lst;
    curr_loc = loc;
    listen = Array.make_matrix dcount pcount Pure_unknown;
    color = player_temp.color;
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
                    |> List.filter (fun x' -> x' <> acc_id) in
  let room_lst = List.map cardify_room room_id_lst in
  let deck = (sus_lst, weap_lst, room_lst) in
  let full_deck = sus_lst@(weap_lst@room_lst) in
  let (env, mixed_deck) = pick_env deck in
  let game = {game_init with
    envelope=env;
    public = {game_init.public with
      curr_player = (match sus_id_lst with
                     | h::t -> h
                     | _ -> failwith("no suspects found: "^Pervasives.__LOC__));
      acc_room = acc_id;
      deck = deck;
      player_order = sus_id_lst
    }
  } in
  let board' = fill_board dim room_temp_lst in
  let game = {game with public = {game.public with board = board'}} in
  let agent_lst = extract_pair_from_assoc "agent_config" asc
                  |> make_agent_lst in
  let f' = (fun acc el -> add_player acc full_deck el agent_lst) in
  let game = List.fold_left f' game sus_temp_lst in
  let is_ai = (function | Human_t -> false | _ -> true) in
  let all_ai =
    List.fold_left (fun acc (_,el) -> acc&&(is_ai el)) true agent_lst in
  let game = {game with ai_only = all_ai} in
  deal_hands game (shuffle_lst mixed_deck)

(* returns the player object that correspondes to the corresponding
 * to the current player in [game] *)
let get_curr_player (game: game) : player =
  let rec loop = function
  | [] -> failwith ("can't find current player: "^Pervasives.__LOC__)
  | h::t -> if h.suspect = game.public.curr_player then h else loop t
in loop game.players

(* [remove_dups lst] is [lst] with no duplicate elements.
* The first instance of each element stays in the list. *)
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
  let start_loc = cp.curr_loc in
  let f pass coord =
    let (x,y) = coord in
    try
      let loc = CoordMap.find coord g.public.board.loc_map in
      match loc.info with
      | Space _ -> pass
      | Room_Rect (s, _) -> Passage(loc)::pass
    with _ -> failwith ("couldn't find coord ("
      ^ Pervasives.string_of_int x ^ ", "
      ^ Pervasives.string_of_int y ^ "): "
      ^ Pervasives.__LOC__) in
  match start_loc.info with
  | Space _ -> [Roll]
  | Room_Rect _ -> List.fold_left f [Roll] start_loc.edges

(* [make_pathmap] is a new PathMap.t representing all paths within
 * the board [board] originating from the coordinate defined by
 * [start_loc].  [fast_out] allows for creation to terminate early.
 * it must be defined as a function (frontier set, settled set) -> bool
 * and will terminate to return the map [stld] if it is ever true.
 * to get the full PathMap, use (fun _ -> false) *)
let make_pathmap board start_loc (fast_out: (PathMap.t * PathMap.t) -> bool) =
  let settled = PathMap.make start_loc in
  let frontier = PathMap.empty in
  let (x,y) = start_loc in
  let loc =
    try (CoordMap.find start_loc board.loc_map) with
    | Not_found -> failwith ("couldn't find start_loc ("
      ^ Pervasives.string_of_int x ^ ", "
      ^ Pervasives.string_of_int y ^ ") in make_pathmap") in
  let add_next coord steps frnt stld =
    let loc = CoordMap.find coord board.loc_map in
    let f (n, bp) acc el =
      if not (PathMap.mem el stld) then
        PathMap.put el (n, bp) acc
      else acc in
    match loc.info with
    | Space (x,y) -> List.fold_left (f ((steps), (x,y))) frnt loc.edges
    | _ -> frnt in
  let is_space c = let l = CoordMap.find c board.loc_map in
    match l.info with Room_Rect _ -> false | Space _ -> true in
  let g acc el = PathMap.put el (1, start_loc) acc in
  let rec loop frnt stld =
    if PathMap.is_empty frnt || (fast_out (frnt, stld)) then stld else
    let (k, (n, bp), frnt') = PathMap.poll_min frnt in
    let stld' = PathMap.put k (n, bp) stld in
    let frnt'' = add_next k (n+1) frnt' stld' in
    loop frnt'' stld'
  in match loc.info with
  | Space _ -> loop (add_next start_loc 1 frontier settled) settled
  | Room_Rect _ ->
    let no_pass = List.filter is_space loc.edges in
    loop (List.fold_left g frontier no_pass) settled


(* [get_movement_options] gets the options of the locations that the current
 * player can move to. These options also come with a description in one of
 * the following fashions:
 *        head towards [room name]
 *        go into [room name]
 * it also returns the full pathmap to be sent to the GUI *)
let get_movement_options (g: game) (steps: int) =
  let b = g.public.board in
  let start_loc = (get_curr_player g).curr_loc in
  let coord =
    match start_loc.info with
    | Space (x,y) | Room_Rect (_,(x,_,y,_)) -> (x, y) in
  let full_paths = make_pathmap b coord (fun pm -> false) in
  let room_lst = StringMap.bindings b.room_coords in
  let room_lst = match start_loc.info with
    | Room_Rect (s, _) -> List.filter (fun (s', bi) -> s <> s') room_lst
    | _ -> room_lst in
  let f (s, (x,y)) =
    let coord' = PathMap.nth_step_towards (x,y) steps full_paths in
    let loc = CoordMap.find coord' b.loc_map in
    match loc.info with
    | Room_Rect (s', _) when s' = s -> (loc, (s, true))
    | _ -> (loc, (s, false)) in
  (List.map f room_lst, full_paths)