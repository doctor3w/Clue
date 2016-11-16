open Data
open Model

module Coord = struct
  type t = int*int
  let compare (r1,c1) (r2,c2) =
    if r1 < r2 then -1
    else if r1 > r2 then 1
    else if c1 < c2 then -1
    else if c1 > c2 then 1
    else 0

end

module CoordMap = Map.Make(Coord)
module StringMap = Map.Make(String)

(* row * col *)
type coord = int*int

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
}

let add_edge board coord1 coord2 both =
  let loc1 = board.loc_map.find coord1 |>
    match x with Some x -> x | None -> failwith "no coord" in
  let loc1' = {loc1 with edges = coord2::loc1.edges} in
  let board' = {board with loc_map = (CoordMap.add coord1 loc1')} in
  if both then (add_edge board' coord2 coord1 false) else board'

(* do this before adding edges! *)
let fill_room board room_temp =
  let s = room_temp.r_id in
  let rect = room_temp.rect in
  let loc' = {info = Room_Rect (s, rect); edge = []} in
  let (x0,x1,y0,y1) = rect in
  let rec loopx board (x',y') =
    let rec loopy board (x',y') =
      if y' > y1 then board
      else let board = {board with loc_map = CoordMap.add (y',x') loc'}
      in loopy board (x', y'+1)
    in if x' > x1 then board
      else let board = loopy board (x', y')
      in loopx board (x'+1,y')
  in loopx board (x0, y0)

let fill_spaces board =
  let (r, c) = board.dim in
  let (r, c) = (r-1,c-1) in
  let rec loopx board (x', y') =
    let rec loopy board (x' y') =
      if y' > r then board else
      let f = (function None -> Space (y', x') | s -> s) in
      let lm' = CoordMap.change board.loc_map (x', y') f in
      let board' = {board with loc_map = lm'} in
      loopy board' (x' y'+1)
    in if x' > c then board else
      let board' = loopy board (x' y')
      in loopx board' (x'+1, y')

let build_empty_board r c =
  {dim = (r,c);
   loc_map = CoordMap.empty;
   room_coords = StringMap.empty}

let edgify_room board room_temp =
  let s = room_temp.r_id in
  let r_coord = board.room_coords.find s in
  let f acc el = add_edge acc r_coord el true in
  let board' = List.fold_left f board room_temp.exits in
  let f' acc el = add_edge r_coord (board.room_coords.find el) false in
  List.fold_left f' board' room_temp.passages

let add_edge_if_space board c1 c2 =
  match

let edgify_spaces board =
  let (r,c) = board.dim in
  let (r,c) = (r-1,c-1) in
  let loopx board (x,y)

let fill_board (r, c) room_temp_lst =
  let empty = build_empty_board r c in
  let f = (fun acc el -> (fill_room acc el)
           |> let (x0,x1,y0,y1) = el.rect in
              let rc' = StringMap.add acc.room_coords el.r_id (y0, x0) in
              {acc with room_coords = rc'}) in
  let roomed = List.fold_left f empty room_temp_lst in
  let filled = fill_spaces roomed in
  let room_edged = List.fold_left edgify_room room_temp_lst in

