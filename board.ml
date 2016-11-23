open Data

(*module Coord = struct
  type t = int*int
  let compare (r1,c1) (r2,c2) =
    if r1 < r2 then -1
    else if r1 > r2 then 1
    else if c1 < c2 then -1
    else if c1 > c2 then 1
    else 0
end

module CoordMap = Map.Make(Coord)
module StringMap = Map.Make(String)*)

(* row * col *)
(*type coord = int*int

(* x0, x1, y0, y1 *)
type rect = int*int*int*int

type coord_info = Space of (int*int) | Room_Rect of string * (int*int*int*int)

type loc' =
{
  info: coord_info;
  edges: (int*int) list
}

type board =
{
  dim: int*int;
  loc_map: loc' CoordMap.t;
  room_coords: coord StringMap.t
} *)

let rec add_edge board coord1 coord2 both =
  let ((a1, a2), (b1, b2)) = (coord1, coord2) in
  let loc1 = try (CoordMap.find coord1 board.loc_map)
    with _ -> failwith ("couldn't find loc ("
                      ^ (Pervasives.string_of_int a1) ^ ", "
                      ^ (Pervasives.string_of_int a2) ^ ") for ("
                      ^ (Pervasives.string_of_int b1) ^ ", "
                      ^ (Pervasives.string_of_int b2) ^")")
  (*|> (fun x -> match x with Some y -> y | None -> failwith "no coord") *)in
  let loc1' = {loc1 with edges = coord2::loc1.edges} in
  let board' = {board with loc_map = (CoordMap.add coord1 loc1' board.loc_map)} in
  if both then (add_edge board' coord2 coord1 false) else board'

(* do this before adding edges! *)
let fill_room board room_temp =
  let s = room_temp.r_id in
  let rect = room_temp.rect in
  let loc' = {info = Room_Rect (s, rect); edges = []} in
  let (x0,x1,y0,y1) = rect in
  let rec loopx board (x',y') =
    let rec loopy board (x',y') =
      if y' > y1 then board
      else let board = {board with loc_map = CoordMap.add (x',y') loc' board.loc_map}
      in loopy board (x', y'+1)
    in if x' > x1 then board
      else let board = loopy board (x', y')
      in loopx board (x'+1,y')
  in loopx board (x0, y0)

let fill_spaces board =
  let (r, c) = board.dim in
  let (r, c) = (r-1,c-1) in
  let rec loopx board (x', y') =
    let rec loopy board (x', y') =
      if y' > r then board else
      let f (x,y) acc = if CoordMap.mem (x,y) acc then acc
                        else CoordMap.add (x,y) {info=Space(y,x); edges=[]} acc in
      let lm' = f (x', y') board.loc_map in
      let board' = {board with loc_map = lm'} in
      loopy board' (x', y'+1)
    in if x' > c then board else
      let board' = loopy board (x', y')
      in loopx board' (x'+1, y')
  in loopx board (0,0)

let build_empty_board r c =
  {dim = (r,c);
   loc_map = CoordMap.empty;
   room_coords = StringMap.empty}

let edgify_room board room_temp =
  let s = room_temp.r_id in
  let r_coord = (StringMap.find s board.room_coords) in
  let f acc el = add_edge acc r_coord el true in
  let board' = List.fold_left f board room_temp.exits in
  let f' acc el = add_edge board' r_coord (StringMap.find s board.room_coords) false in
  List.fold_left f' board' room_temp.passages

let add_edge_if_space board c1 c2 =
  let (r,c) = board.dim in
  let (r,c) = (r-1,c-1) in
  let (x2,y2) = c2 in
  if x2 < 0 || x2 > c || y2 < 0 || y2 > r then board else
  match (CoordMap.find c1 board.loc_map).info with
  | Space _ -> add_edge board c1 c2 false
  | Room_Rect _ -> board

let edgify_spaces board =
  let (r,c) = board.dim in
  let (r,c) = (r-1,c-1) in
  let rec loopx board (x,y) =
    let rec loopy board (x,y) =
      if y > r then board
    else let board' = add_edge_if_space board (x,y) (x-1,y) in
         let board' = add_edge_if_space board (x,y) (x+1,y) in
         let board' = add_edge_if_space board (x,y) (x,y-1) in
         let board' = add_edge_if_space board (x,y) (x,y+1) in
         loopy board' (x,y+1)
    in if x > c then board
    else let board' = loopy board (x,y) in
    loopx board' (x+1,y) in
  loopx board (0,0)

let fill_board (r, c) room_temp_lst =
  let empty = build_empty_board r c in
  let f = (fun acc el ->
              let acc = (fill_room acc el) in
              let (x0,x1,y0,y1) = el.rect in
              let rc' = StringMap.add (el.r_id) (y0, x0) (acc.room_coords) in
              {acc with room_coords = rc'}) in
  let roomed = List.fold_left f empty room_temp_lst in
  let filled = fill_spaces roomed in
  let room_edged = List.fold_left edgify_room filled room_temp_lst in
  edgify_spaces room_edged

