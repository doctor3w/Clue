open Data

(* returns [board] with an extra edge defined from [coord1] to [coord2].
 * if [both] is true, [add edge board' coord2 coord1 false] is called *)
let rec add_edge board coord1 coord2 both =
  let ((a1, a2), (b1, b2)) = (coord1, coord2) in
  let loc1 = try (CoordMap.find coord1 board.loc_map)
    with _ -> failwith ("couldn't find loc ("
                      ^ (Pervasives.string_of_int a1) ^ ", "
                      ^ (Pervasives.string_of_int a2) ^ ") for ("
                      ^ (Pervasives.string_of_int b1) ^ ", "
                      ^ (Pervasives.string_of_int b2) ^")") in
  let loc1' = {loc1 with edges = coord2::loc1.edges} in
  let board' =
    {board with loc_map = (CoordMap.add coord1 loc1' board.loc_map)} in
  if both then (add_edge board' coord2 coord1 false) else board'

(* adds the room to [board.loc_map] for all coordinates that fall within the
 * [room] defined by [room_temp]
 * do this before adding edges! *)
let fill_room board room_temp =
  let s = room_temp.r_id in
  let rect = room_temp.rect in
  let loc' = {info = Room_Rect (s, rect); edges = []} in
  let (x0,x1,y0,y1) = rect in
  let rec loopx board (x',y') =
    let rec loopy board (x',y') =
      if y' > y1 then board
      else
        let loc_map = CoordMap.add (x',y') loc' board.loc_map in
        loopy {board with loc_map = loc_map} (x', y'+1) in
    if x' > x1 then board
    else
      let board = loopy board (x', y') in
      loopx board (x'+1,y') in
  loopx board (x0, y0)

(* puts spaces in all remaining coordinates within the dimensions of [board[ *)
let fill_spaces board =
  let (r, c) = board.dim in
  let (r, c) = (r-1,c-1) in
  let rec loopx board (x', y') =
    let rec loopy board (x', y') =
      if y' > r then board else
      let f (x,y) acc =
        if CoordMap.mem (x,y) acc then acc
        else CoordMap.add (x,y) {info=Space(x,y); edges=[]} acc in
      let lm' = f (x', y') board.loc_map in
      let board' = {board with loc_map = lm'} in
      loopy board' (x', y'+1)
    in if x' > c then board else
      let board' = loopy board (x', y')
      in loopx board' (x'+1, y')
  in loopx board (0,0)

(* builds a completely empty board with [r] rows and [c] columns *)
let build_empty_board r c =
  {dim = (r,c);
   loc_map = CoordMap.empty;
   room_coords = StringMap.empty}

(* adds the exits and passages from a room to [loc.edges] for the
 * corresponding room in [board.room_locs] and [board.loc_map] *)
let edgify_room board room_temp =
  let s = room_temp.r_id in
  let r_coord = (StringMap.find s board.room_coords) in
  let f acc el = add_edge acc r_coord el true in
  let board' = List.fold_left f board room_temp.exits in
  let f' acc el =
    add_edge board' r_coord (StringMap.find el board.room_coords) false in
  List.fold_left f' board' room_temp.passages

(* adds an edge from [c1] to [c2] iff they are both Spaces *)
let add_edge_if_space board c1 c2 =
  let (r,c) = board.dim in
  let (r,c) = (r-1,c-1) in
  let (x2,y2) = c2 in
  if x2 < 0 || x2 > c || y2 < 0 || y2 > r then board else
  let loc1 = (CoordMap.find c1 board.loc_map).info in
  let loc2 = (CoordMap.find c2 board.loc_map).info in
  match (loc1, loc2) with
  | (Space _, Space _) -> add_edge board c1 c2 false
  | _ -> board

(* puts edges between all adjacent spaces in [board] *)
let edgify_spaces board =
  let (r,c) = board.dim in
  let (r,c) = (r-1,c-1) in
  let rec loopx board (x,y) =
    let rec loopy board (x,y) =
      if y > r then board
      else
        let board' = add_edge_if_space board (x,y) (x-1,y) in
        let board' = add_edge_if_space board' (x,y) (x+1,y) in
        let board' = add_edge_if_space board' (x,y) (x,y-1) in
        let board' = add_edge_if_space board' (x,y) (x,y+1) in
        loopy board' (x,y+1)
    in if x > c then board
    else
      let board' = loopy board (x,y) in
      loopx board' (x+1,y) in
  loopx board (0,0)

(* makes sure every [coord] within any given room matches every other coord
 * in that room.  This allows rooms to be looked up by any interior coord *)
let finish_rooms board =
  let f coord loc board =
    match loc.info with
    | Room_Rect (s, rect) ->
      let coord' = StringMap.find s board.room_coords in
      let loc' = CoordMap.find coord' board.loc_map in
      {board with loc_map = (CoordMap.add coord loc' board.loc_map)}
    | Space _ -> board in
  CoordMap.fold f board.loc_map board

(* makes a new board with dimensions (r, c) containing a room for each
 * element in [room_temp_lst] with all edges and spaces necessary for full
 * traversal.  Model calls this from [import_board] and it is the only
 * part of Board called anywhere in the project. *)
let fill_board (r, c) room_temp_lst =
  let empty = build_empty_board r c in
  let f acc el =
    let acc = (fill_room acc el) in
    let (x0,x1,y0,y1) = el.rect in
    let rc' = StringMap.add (el.r_id) (x0, y0) (acc.room_coords) in
    {acc with room_coords = rc'} in
  let roomed = List.fold_left f empty room_temp_lst in
  let filled = fill_spaces roomed in
  let room_edged = List.fold_left edgify_room filled room_temp_lst in
  let fully_edged = edgify_spaces room_edged in
  finish_rooms fully_edged

