open Data
open Graphics

(* (x, y, w, h) *)
type grect = int*int*int*int
type transform = grect -> grect

type graphic_board = {
  mutable board: board;
  mutable sheet_disp: string;
  mutable win_bounds: int*int;
  mutable b_window: grect;
  mutable roll_window: grect;
  mutable player_locs: loc StringMap.t;
  mutable player_colors: Graphics.color StringMap.t;
}

let window = {
    win_bounds = (600, 450);
    sheet_disp = "";
    b_window = (10, 80, 360, 360);
    roll_window = (380, 10, 210, 60);
    board = {
      dim = (-1,-1);
      loc_map = CoordMap.empty;
      room_coords = StringMap.empty
    };
    player_locs = StringMap.empty;
    player_colors = StringMap.empty;
  }

let player_border = Graphics.black
let room_color = Graphics.rgb 191 191 191
let space_color = Graphics.rgb 255 255 255
let door_color = Graphics.rgb 119 61 20
let highlight_color = Graphics.rgb 126 249 32
let roll_color = Graphics.rgb 249 83 32

(* partially applies the [rect] as four arguments to f *)
let grect_curry f rect =
  let (x, y, w, h) = rect in
  f x y w h

(* turns the [rect] from the board to a grect of (x, y, w, h)
 * instead of (x0, x1, y0, y1) *)
let rect_to_grect rect =
  let (x0, x1, y0, y1) = rect in
  (x0, y0, x1+1-x0, y1+1-y0)

(* scales [grect] from the point (0, 0) by [scale] *)
let scale_grect (x_scale, y_scale) grect =
  let (x, y, w, h) = grect in
  (x*x_scale, y*y_scale, w*x_scale, h*y_scale)

(* shifts [grect] by [x_shift] along the x axis and by
 * [y_shift] along the y axis *)
let shift_grect (x_shift, y_shift) grect =
  let (x, y, w, h) = grect in
  (x+x_shift, y+y_shift, w, h)

(* returns true if point [pt] is in [grect] *)
let is_in_rect pt grect =
  let (x, y, w, h) = grect in
  let (x', y') = pt in
  if (w < 0 || h < 0) then failwith "bad_rect"
  else (x' >= x && x' <= (x+w) && y' >= y && y' <= (y+h))

(* fills the rect with color [fl] then outlines in with color [ln] *)
let draw_filled_rect x y w h ln fl =
  if x < 0 || y < 0 || w < 2 || h < 2
  then failwith "bad_rectangle to draw"
  else
    Graphics.set_color fl;
    Graphics.fill_rect (x) (y) (w) (h);
    Graphics.set_color ln;
    Graphics.draw_rect x y w h

(* fills the ellipse with color [fl] then outlines in with color [ln] *)
let draw_filled_ellipse x y rx ry ln fl =
  if x < 0 || y < 0 || rx < 1 || ry < 1
  then failwith "bad_ellipse to draw"
  else
    Graphics.set_color fl;
    Graphics.fill_ellipse (x) (y) (rx) (ry);
    Graphics.set_color ln;
    Graphics.draw_ellipse x y rx ry

(* draws a filled ellipse using [draw_filled_ellips] but takes the
 * same arguments as [draw_filled_rect] *)
let draw_ell_in_rect x y w h ln fl =
  let rx = w/2 in
  let ry = h/2 in
  draw_filled_ellipse (x+rx) (y+ry) rx ry ln fl

let center_text_in_rect x y w h s =
  let (w', h') = Graphics.text_size s in
  let (buffer_w, buffer_h) = ((w - w')/2, (h - h')/2) in
  Graphics.moveto (x+buffer_w) (y+buffer_h);
  Graphics.draw_string s

(* returns the (x, y) of the next mouse click relative to the window,
 * doesn't terminate until the mouse is clicked in the window *)
let get_next_click_pos () =
  let click = Graphics.wait_next_event [Button_down] in
  (click.mouse_x, click.mouse_y)

(* returns the relative (x, y) of the next mouse click within the
 * grect defined by (x, y, w, h), doesn't terminate until the mouse is clicked
 * within the bounds of the window. *)
let rec get_next_click_in_rect x y w h () =
  let rect = (x, y, w, h) in
  let pt = get_next_click_pos () in
  if is_in_rect pt rect then
    let (x', y') = pt in
    (x' - x, y' - y)
  else get_next_click_in_rect x y w h ()


(* returns the relative (x, y) of the next mouse click within one of the
 * grects defined in rects, doesn't terminate until the mouse is clicked
 * within the bounds of one of those rects. *)
let rec get_next_click_in_rects (rects: (string*grect) list) () =
  (*let rect = (x, y, w, h) in*)
  let pt = get_next_click_pos () in
  let rec loop lst =
    match lst with
    | [] -> get_next_click_in_rects rects ()
    | (s, (x, y, w, h))::t -> if is_in_rect pt (x, y, w, h)
                      then
                        let (x', y') = pt in
                          (s, (x' - x, y' - y))
                      else loop t in
  loop rects

(* gets (x_mult, y_mult) used to scale a 1x1 square to the correct size in
 * window.b_window *)
let get_mults () =
  let (dimx, dimy) = window.board.dim in
  let (_, _, winw, winh) = window.b_window in
  (winw/dimx, winh/dimy)

let get_b_transform () =
  let (xb, yb, wb, hb) = window.b_window in
  let (x_mult, y_mult) = get_mults () in
  let transform x = x |> scale_grect (x_mult, y_mult)
                      |> shift_grect (xb, yb) in
  transform

(* translates a relative mouse position in the board_window with a coord that
 * can be looked up in the board *)
let translate_to_coord pt =
  let (x_mult, y_mult) = get_mults () in
  let (x', y') = pt in
  (x'/x_mult, y'/y_mult)

let adjust_coord_if_room coord =
  let loc = CoordMap.find coord window.board.loc_map in
  match loc.info with
  | Room_Rect (_, (x, _, y, _)) | Space (x, y) -> (x, y)


let draw_exits transform loc =
  Graphics.set_line_width 3;
  Graphics.set_color door_color;
  let rect = match loc.info with
  | Room_Rect (s, r) -> r
  | _ -> failwith ("can't draw exits for Space " ^ Pervasives.__LOC__) in
  let (x0, x1, y0, y1) = rect in
  let draw_exit coord =
    match (CoordMap.find coord window.board.loc_map).info with
    | Space (x, y) -> let (gx, gy, gw, gh) = transform (x, y, 1, 1) in
                      if x < x0 then (Graphics.moveto (gx+gw) (gy+1);
                                     Graphics.lineto (gx+gw) (gy+gh-1))
                 else if x > x1 then (Graphics.moveto (gx) (gy+1);
                                     Graphics.lineto (gx) (gy+gh-1))
                 else if y < y0 then (Graphics.moveto (gx+1) (gy+gh);
                                     Graphics.lineto (gx+gw-1) (gy+gh))
                 else if y > y1 then (Graphics.moveto (gx+1) (gy);
                                     Graphics.lineto (gx+gw-1) (gy))
                 else ()
    | Room_Rect _ -> () in
  List.iter draw_exit loc.edges; Graphics.set_line_width 1

(* clears the window and draws the board *)
let draw_board () =
  (grect_curry draw_filled_rect window.b_window) Graphics.black Graphics.white;
  let room_bind = StringMap.bindings window.board.room_coords in
  let (xb, yb, wb, hb) = window.b_window in
  let (x_mult, y_mult) = get_mults () in
  let transform x = x |> scale_grect (x_mult, y_mult)
                      |> shift_grect (xb, yb) in
  let f (s, coord) =
    let loc = CoordMap.find coord window.board.loc_map in
    let (s, rect) = match loc.info with
      | Room_Rect (s', rect') -> (s', rect')
      | _ -> failwith ("loc must be room_rect: " ^ Pervasives.__LOC__) in
    let grect = rect
    |> rect_to_grect
    |> (scale_grect (x_mult, y_mult))
    |> (shift_grect (xb, yb)) in
    let (gx, gy, gh, gw) = grect in
    (grect_curry draw_filled_rect grect) Graphics.black room_color;
    (grect_curry center_text_in_rect grect) s;
    draw_exits transform loc in
  let g (coord, loc) =
    match loc.info with
    | Space (x, y) -> let grect = (x, y, 1, 1)
                      |> (scale_grect (x_mult, y_mult))
                      |> (shift_grect (xb, yb)) in
            (grect_curry draw_filled_rect grect) Graphics.black space_color
    | _ -> () in
  let coord_binds = CoordMap.bindings window.board.loc_map in
  List.iter g coord_binds;
  List.iter f room_bind

let draw_players_in_rooms transform rect_binds =
  let rec count_rect rect count lst =
    match lst with
    | [] -> count
    | (sus, rect')::t -> if rect = rect' then count_rect rect (count+1) t
                         else count_rect rect count t in
  let rec place_player placed queue =
    match queue with
    | [] -> ()
    | (sus, (x0, x1, y0, y1))::t ->
          let pcount = count_rect (x0, x1, y0, y1) 0 placed in
          let w = x1-x0 in
          let r = pcount/w in
          let c = pcount mod w in
          let grect = transform (x0+c, y0+r, 1, 1) in
          let col = StringMap.find sus window.player_colors in
          (grect_curry draw_ell_in_rect grect) player_border col;
          place_player ((sus, (x0, x1, y0, y1))::placed) t in
  place_player [] rect_binds

let draw_players () =
  let sus_binds = StringMap.bindings window.player_locs in
  let (xb, yb, wb, hb) = window.b_window in
  let (x_mult, y_mult) = get_mults () in
  let rlst = ref [] in
  let transform x = x |> scale_grect (x_mult, y_mult)
                      |> shift_grect (xb, yb) in
  let f (sus, loc) =
    match loc.info with
    | Space (x, y) -> let grect = transform (x, y, 1, 1) in
                      let fl = StringMap.find sus window.player_colors in
                      (grect_curry draw_ell_in_rect grect) player_border fl
    | Room_Rect (s, rect) -> rlst := (sus, rect)::(!rlst) in
  List.iter f sus_binds;
  draw_players_in_rooms transform !rlst

let draw_roll () =
  let grect = window.roll_window in
  (grect_curry draw_filled_rect grect) Graphics.black roll_color;
  (grect_curry center_text_in_rect grect) "ROLL"

let highlight_roll () =
  let grect = window.roll_window in
  (grect_curry draw_filled_rect grect) Graphics.black highlight_color;
  (grect_curry center_text_in_rect grect) "ROLL"

let redraw () =
  draw_board ();
  draw_players ();
  draw_roll ();
  ()

let highlight_loc transform loc =
  match loc.info with
  | Space (x, y) -> let (gx, gy, gw, gh) = transform (x, y, 1, 1) in
                    let grect' = (gx+1, gy+1, gw-2, gh-2) in
                    Graphics.set_color highlight_color;
                    (grect_curry Graphics.draw_rect grect')
  | Room_Rect (s, rect) -> let (gx, gy, gw, gh) = transform (rect_to_grect rect) in
                    let grect' = (gx+1, gy+1, gw-2, gh-2) in
                    Graphics.set_color highlight_color;
                    (grect_curry Graphics.draw_rect grect')

let highlight_coord transform coord =
  let loc = (CoordMap.find coord window.board.loc_map) in
  highlight_loc transform loc

(* Displays the provided error message. *)
let display_error (e_msg: string) : unit =
  failwith "Unimplemented gui.display_error"

(* Displays a description of who's turn it is. *)
let display_turn public : unit =
  failwith "Unimplemented gui.display_turn"

(* Prompts the user for a file so that it can be imported into the Model *)
let prompt_filename () : string =
  failwith "Unimplemented gui.prompt_filename"

(* Prompts the user for whether he rolls dice or not. *)
let prompt_move (movelst: move list) : string =
  failwith "Unimplemented gui.prompt_move"

let prompt_move_gui (movelst: move list) : move =
  let transform = get_b_transform () in
  let f acc move =
    match move with
    | Passage loc -> highlight_loc transform loc; loc::acc
    | Roll -> highlight_roll (); acc in
  let loclst = List.fold_left f [] movelst in
  let rectlst = [("board", window.b_window); ("roll", window.roll_window)] in
  let rec loop () =
    match get_next_click_in_rects rectlst () with
    | ("board", pt) -> let coord = translate_to_coord pt in
                       let loc = CoordMap.find coord window.board.loc_map in
                       if List.mem loc loclst then Passage loc else loop ()
    | ("roll", _) -> draw_roll (); Roll
    | (s, _) -> failwith ("not an included string " ^ s ^ ": " ^ Pervasives.__LOC__) in
  loop ()


(* Displays a description of what the agent rolled. *)
let display_dice_roll (roll: int) : unit =
  failwith "Unimplemented gui.display_dice_roll"

(* Displays a description of whether the agent elected to Roll or Passage. *)
let display_move move : unit =
  failwith "Unimplemented gui.display_move"

(* Prompts the user for the room they would like to go to.
 * [loc * (string * bool)] the location and whether or not room [string] is
 * accessible. The second string parameter is the acc_room name. *)
let prompt_movement (movelst : (loc * (string * bool)) list) (acc_room:string) : string =
  failwith "Unimplemented gui.prompt_movement"

let prompt_movement_with_pm pathmap acc_room roll : loc =
  let pm = PathMap.filter (fun (x, y) (n, (x', y')) -> n <= roll) pathmap in
  let highlight_coords = PathMap.keys pm in
  let (xb, yb, wb, hb) = window.b_window in
  let (x_mult, y_mult) = get_mults () in
  let transform x = x |> scale_grect (x_mult, y_mult)
                      |> shift_grect (xb, yb) in
  List.iter (highlight_coord transform) highlight_coords;
  draw_players ();
  let rec loop () =
    let f = grect_curry get_next_click_in_rect window.b_window in
    let click_coord = translate_to_coord (f ())
                      |> adjust_coord_if_room in
    if List.mem click_coord highlight_coords
    then CoordMap.find click_coord window.board.loc_map
    else loop () in
  loop ()

(* Displays the movement the agent took on its turn *)
let display_movement (end_move :(string * bool)) : unit =
  failwith "Unimplemented gui.display_movement"

(* Displays the relocation of suspect [string] to the Room loc *)
let display_relocate (who:string) loc : unit =
  window.player_locs <- StringMap.add who loc window.player_locs;
  redraw ()

(* Prompts the user for a guess.
 * Takes in the current location (must be a room) and
 * a bool which says whether or not it is the final accusation.
 * returns a string of the user's response. *)
let prompt_guess loc (is_acc: bool) : string =
  failwith "Unimplemented gui.prompt_guess"

(* Displays a guess (by the user or AI). *)
let display_guess guess : unit =
  failwith "Unimplemented gui.display_guess"

(* Prompts the user for a card to show.
 * Can be any card from the provided hand, and must be in the guess.
 * Can be none if there is no card to show. *)
let prompt_answer hand guess : string =
  failwith "Unimplemented gui.prompt_answer"

(* Displays the card shown to the human agent and by whom.
 * If None, no card could be shown. If false, the user is not shown the
 * details of the card. *)
let display_answer (c:card option) (who: string) (detail: bool) : unit =
  failwith "Unimplemented gui.display_answer"

(* Displays that the player [string] could not answer with a card.
 * This is different from no one being able to show a card. *)
let display_no_answer (who: string) : unit =
  failwith "Unimplemented gui.display_no_answer"

(* Displays end game victory text, string is who won. *)
let display_victory (who: string) : unit =
  failwith "Unimplemented gui.display_victory"

(* Displays arbitrary text. *)
let display_message (text: string) : unit =
  failwith "Unimplemented gui.display_error"

let init game =
  window.board <- game.public.board;
  let f me =
    let sus = me.suspect in
    let color = Graphics.rgb (Random.int 256) (Random.int 256) (Random.int 256)
    in window.player_locs <- StringMap.add sus me.curr_loc window.player_locs;
    window.player_colors <- StringMap.add sus color window.player_colors
  in List.iter f game.players;
  Graphics.open_graph "";
  redraw ()

let show_sheet sheet : unit =
  ()

let show_hand hand : unit =
  ()