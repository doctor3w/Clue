open Data


module IntMap = Map.Make(Int32)

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
    loc_map: (loc' IntMap.t) IntMap.t
  }

let build_empty_board r c =
  {dim = (r,c);
   loc_map = IntMap.empty }