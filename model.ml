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
  if len < 5 then failwith "not a json, ln 26" (* can't end in .json *)
  else if((String.sub file_name (len-5) 5) = ".json")
  then Yojson.Basic.from_file file_name
  (*try (Yojson.Basic.from_file file_name) with _ -> failwith "can't make json, ln 28" *)
  else failwith ("not a .json, ln 30, filename: " ^ file_name)

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

let deal_card p c =
  let sd = try (CardMap.find c p.sheet) with _ -> failwith "can't deal card" in
  let sd' = {sd with card_info = Mine []} in
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
  | ((s1,n1)::(s2,n2)::[]) -> let y = if s1 = "row"
                                      then Yojson.Basic.Util.to_int n1
                                      else Yojson.Basic.Util.to_int n2
                           in let x = if s1 = "col"
                                      then Yojson.Basic.Util.to_int n1
                                      else Yojson.Basic.Util.to_int n2
                           in (x, y)
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
  let loc = try (CoordMap.find player_temp.start game.public.board.loc_map)
            with _ -> failwith "can't find start coord" in
  let p = {
    suspect = player_temp.p_id;
    hand = [];
    sheet = default_sheet full_deck;
    is_out = false;
    agent = List.assoc player_temp.p_id agent_lst;
    curr_loc = loc;
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
  let is_ai = (function | Human_t -> false | _ -> true) in
  let all_ai = List.fold_left (fun acc (_,el) -> acc&&(is_ai el)) true agent_lst
in let game = {game with ai_only = all_ai} in
  deal_hands game (shuffle_lst mixed_deck)


let get_curr_player (game: game) : player =
  let rec loop = function
  | [] -> failwith "can't find current player"
  | h::t -> if h.suspect = game.public.curr_player then h else loop t
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
  let start_loc = cp.curr_loc in
  let f pass coord =
    let (x,y) = coord in
    try
    (let loc = CoordMap.find coord g.public.board.loc_map in
    match loc.info with
    | Space _ -> pass
    | Room_Rect (s, _) -> Passage(loc)::pass)
    with _ -> failwith ("couldn't find coord ("
      ^ Pervasives.string_of_int x ^ ", "
      ^ Pervasives.string_of_int y ^ ")") in
  match start_loc.info with
  | Space _ -> [Roll]
  | Room_Rect _ -> List.fold_left f [Roll] start_loc.edges


module PathMap = struct
  type backpointer = (int*coord)
  type t = backpointer CoordMap.t

  let empty = CoordMap.empty
  let mem = CoordMap.mem
  let is_empty = CoordMap.is_empty

  let make start =
    CoordMap.add start (0, start) empty

  let put (k:coord) ((s':int), (bp':coord)) (map: t) : t =
    if CoordMap.mem k map
    then let (s, bp) = try (CoordMap.find k map) with _ -> failwith "line 277" in
      if (s' < s) then CoordMap.add k (s', bp') map
      else map
    else CoordMap.add k (s', bp') map

  let poll_min (map:t) : (coord * backpointer * t) =
    if is_empty map then failwith "can't poll empty PathMap" else
    let f k (e1, e2) (ak, (a1, a2)) = if e1 < a1 then (k, (e1, e2)) else (ak, (a1, a2)) in
    let (k, v) = CoordMap.fold f map (CoordMap.choose map) in
    let map' = CoordMap.remove k map in
    (k, v, map')

  let length_to coord map =
    let (x, y) = coord in
    let (n, bp) = try (CoordMap.find coord map)
    with Not_found -> failwith ("couldn't find coord ("
      ^ Pervasives.string_of_int x ^ ", "
      ^ Pervasives.string_of_int y ^ ") in length_to") in n

  let nth_step_towards coord step map =
    let rec loop c =
      let (x,y) = c in
      let (n, bp) = try (CoordMap.find c map)
      with Not_found -> failwith ("couldn't find coord ("
      ^ Pervasives.string_of_int x ^ ", "
      ^ Pervasives.string_of_int y ^ ") in nth_step_towards") in
      if n <= step then c else loop bp
    in loop coord

end

let make_pathmap board start_loc fast_out =
  let settled = PathMap.make start_loc in
  let frontier = PathMap.empty in
  let (x,y) = start_loc in
  let loc = try (CoordMap.find start_loc board.loc_map)
  with Not_found -> failwith ("couldn't find start_loc ("
      ^ Pervasives.string_of_int x ^ ", "
      ^ Pervasives.string_of_int y ^ ") in make_pathmap") in
  let add_next coord steps frnt stld =
    let loc = CoordMap.find coord board.loc_map in
    let f (n, bp) acc el = if not (PathMap.mem el stld)
                           then PathMap.put el (n, bp) acc
                           else acc in
    match loc.info with
    | Space (x,y) -> List.fold_left (f ((steps), (x,y))) frnt loc.edges
    | _ -> frnt in
  let is_space c = let l = CoordMap.find c board.loc_map in
    match l.info with Room_Rect _ -> false | Space _ -> true in
  let g acc el = PathMap.put el (1, start_loc) acc in
  let rec loop frnt stld =
    if PathMap.is_empty frnt || fast_out then stld else
    let (k, (n, bp), frnt') = PathMap.poll_min frnt in
    let stld' = PathMap.put k (n, bp) stld in
    let frnt'' = add_next k (n+1) frnt' stld' in
    loop frnt'' stld'
  in match loc.info with
  | Space _ -> loop (add_next start_loc 0 frontier settled) settled
  | Room_Rect _ -> let no_pass = List.filter is_space loc.edges in
                   loop (List.fold_left g frontier no_pass) settled


(* [get_movement_options] gets the options of the locations that the current
 * player can move to. These options also come with a description in one of
 * the following fashions:
 *        head towards [room name]
 *        go into [room name] *)

let get_movement_options (g: game) (steps: int) =
  let b = g.public.board in
  let start_loc = (get_curr_player g).curr_loc in
  let coord =
    match start_loc.info with
    | Space (x,y) | Room_Rect (_,(x,_,y,_)) -> (x, y) in
  let full_paths = make_pathmap b coord false in
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
  List.map f room_lst


  (*let b = g.public.board in
  let start_loc = (get_curr_player g).curr_loc in
  let rec step_loop steps acc coord =
    if steps < 0 then acc else
    let loc = CoordMap.find coord b.loc_map in
    match (loc.info) with
    | Room_Rect (s, _) -> ("go into "^s, loc)::acc
    | Space _ -> List.fold_left (step_loop (steps-1)) acc loc.edges in
  let init = List.fold_left (step_loop (steps-1)) [] st



  art_loc.edges in
  let no_start = List.filter (fun (s,l) -> l != start_loc) init in
  remove_dups no_start*)

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

