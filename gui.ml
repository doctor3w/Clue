open Data
open Str
open Graphics

(* (x, y, w, h) *)
type grect = int*int*int*int
type transform = grect -> grect

type graphic_board = {
  mutable board: board;
  mutable sheet: sheet;
  mutable sheet_disp: string;
  mutable win_bounds: int*int;
  mutable b_window: grect;
  mutable s_window: grect;
  mutable roll_window: grect;
  mutable info_window: grect;
  mutable log_window: grect;
  mutable po_window: grect;
  mutable player_locs: loc StringMap.t;
  mutable player_colors: Graphics.color StringMap.t;
  mutable last_info: string;
  mutable roll_text: string;
  mutable curr_player: string;
  mutable sus_count: int;
  mutable weap_count: int;
  mutable room_count: int;
  mutable log: (string * Graphics.color) list;
  mutable po_lst: string list;
}

let window = {
    win_bounds = (960, 720);
    sheet_disp = "";
    sheet = CardMap.empty;
    b_window = (10, 80, 480, 480);
    s_window = (500, 80, 210, 630);
    roll_window = (500, 10, 210, 60);
    info_window = (10, 10, 480, 60);
    log_window = (720, 10, 230, 700);
    po_window = (10, 570, 480, 140);
    board = {
      dim = (-1,-1);
      loc_map = CoordMap.empty;
      room_coords = StringMap.empty
    };
    player_locs = StringMap.empty;
    player_colors = StringMap.empty;
    last_info = "CLUE";
    curr_player = "";
    roll_text = "CONTINUE";
    sus_count = 1;
    weap_count = 1;
    room_count = 1;
    log = [("Welcome to CLUE.", Graphics.black)];
    po_lst = []
  }

let player_border = Graphics.black
let room_color = Graphics.rgb 191 191 191
let space_color = Graphics.rgb 255 255 255
let door_color = Graphics.rgb 119 61 20
let highlight_color = Graphics.rgb 126 249 32
let roll_color = Graphics.rgb 249 83 32
let suspect_color = Graphics.rgb 186 244 141
let weapon_color = Graphics.rgb 244 224 141
let room_sheet_color = Graphics.rgb 141 225 244
let deck_border_color = Graphics.black
let lock_color = Graphics.rgb 63 63 63
let answer_back = Graphics.rgb 31 31 31
let dark_space_color = Graphics.rgb 127 127 127
let dark_room_color = Graphics.rgb 63 63 63
let tick_color = Graphics.rgb 41 63 39


(* Delays for [secs] seconds
 * figured out after looking at this post from StackOverflow:
 * Unix.Unix_error(Unix.EINTR, "select", "") *)
let gui_delay (secs:float) =
  let start_time = Unix.gettimeofday () in
  let rec loop () =
    let dur = start_time +. secs -. Unix.gettimeofday () in
    if dur > 0.0 then
      try Thread.delay dur
      with Unix.Unix_error(Unix.EINTR, _, _) -> loop ()
  in if !testing then () else loop ()

(* returns f () after a delay of [secs] seconds. No delay if testing *)
let after_delay (f: unit -> 'a) (secs:float) : 'a =
  let b = if !testing then true else (Thread.delay secs; true) in
  match b with
  | true -> f ()
  | false -> failwith ("Can never be false: " ^ Pervasives.__LOC__)

(* true iff the current player is the player whose sheet is displayed *)
let is_my_turn () =
  window.curr_player = window.sheet_disp

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
  if (w < 0 || h < 0) then failwith ("bad_rect: "^Pervasives.__LOC__)
  else (x' >= x && x' <= (x+w) && y' >= y && y' <= (y+h))

(* fills the rect with color [fl] then outlines in with color [ln] *)
let draw_filled_rect x y w h ln fl =
  if x < 0 || y < 0 || w < 1 || h < 1
  then let rcts = ("(" ^ Pervasives.string_of_int x ^ ", "
                      ^ Pervasives.string_of_int y ^ ", "
                      ^ Pervasives.string_of_int w ^ ", "
                      ^ Pervasives.string_of_int h ^ ")" ) in
    failwith ("bad_rectangle to draw: " ^ rcts)
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

(* turns the string [s] into a list of strings that
 * can each be printed narrower than [w], splitting on
 * white space. Used mainly by the log. *)
let split_long_string w s =
  let lst = Str.split (regexp_string " ") s in
  let rec loop acc s lst =
    match lst with
    | [] -> acc @ [s]
    | h::t -> let s' = if s <> "" then s ^ " " ^ h else h in
              let (w', h') = Graphics.text_size s' in
              if (w' > w) then loop (acc @ [s]) ("  " ^ h) t
              else loop acc s' t in
  loop [] "" lst

let split_long_string_no_indent w s =
  let lst = Str.split (regexp_string " ") s in
  let rec loop acc s lst =
    match lst with
    | [] -> acc @ [s]
    | h::t -> let s' = if s <> "" then s ^ " " ^ h else h in
              let (w', h') = Graphics.text_size s' in
              if (w' > w) then loop (acc @ [s]) (h) t
              else loop acc s' t in
  loop [] "" lst

let split_on_all_chars w s =
  let lst = Str.full_split (regexp "[a-zA-Z0-9]") s in
  let lst = List.map (function Str.Text s | Str.Delim s -> s) lst in
  let rec loop acc s lst =
    match lst with
    | [] -> List.rev (s::(List.rev acc))
    | h::t -> let s' = if s <> "" then s ^ h else h in
              let (w', h') = Graphics.text_size s' in
              if (w' > w) then loop (List.rev (s::(List.rev acc))) (h) t
              else loop acc s' t in
  loop [] "" lst

let strict_split_string w s =
  let lst = split_long_string_no_indent w s in
  let rec loop acc lst =
    match lst with
    | [] -> acc
    | h::t -> let (w', _) = Graphics.text_size h in
              if w' > w
              then let lst' = split_on_all_chars w h in
                loop (acc@lst') t
              else loop (List.rev (h::(List.rev acc))) t in
  List.filter (fun s -> s <> "") (loop [] lst)

(* prints the string [s] centered in the grect (x, y, w, h) *)
let center_text_in_rect x y w h s =
  let lst = Str.split (regexp "[\n]+") s in
  let rec length_check (aw, ah) acc lst =
    match lst with
    | [] -> ((aw, ah), acc)
    | h::t -> let (w', h') = Graphics.text_size h in
              if (w' > w)
                then let l_split = strict_split_string w h in
                  let l_split = List.filter (fun s -> s <> "") l_split in
                  length_check (aw, ah) (acc) (l_split@t)
              else if (w' > aw)
                then length_check (w', h') (List.rev (h::(List.rev acc))) t
              else length_check (aw, ah) (List.rev (h::(List.rev acc))) t in
  let (biggest_size, lst') = length_check (0, 0) [] lst in
  let count = List.length lst' in
  let rec loop n lst =
    match lst with
    | [] -> ()
    | s'::t ->
      let (w', h') = biggest_size in
      let h'' = h'*count in
      let (buffer_w, buffer_h) = ((w - w')/2, (h - h'')/2) in
      let buffer_w = if buffer_w < 0 then 0 else buffer_w in
      let buffer_h = if buffer_h < 0 then 0 else buffer_h in
      Graphics.moveto (x+buffer_w) (y+buffer_h+(count - 1 - n)*h');
      Graphics.draw_string s';
      loop (n+1) t in
  loop 0 lst'

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

let translate_to_card pt =
  let (sx, sy, sw, sh) = window.s_window in
  let count = CardMap.cardinal window.sheet in
  let hght = if count = 0 then 1 else sh / count in
  let space = (sh - (hght * count))/2 in
  let sus_top = sy+sh in
  let sus_bot = sus_top - (window.sus_count*hght) in
  let weap_top = sus_bot - space in
  let weap_bot = weap_top - (window.weap_count*hght) in
  let room_top = weap_bot - space in
  (* let room_bot = sy in *)
  let (x', y') = pt in
  let index =
    if      y' <= room_top then ((sh - y' - space*2) / hght)
    else if y' <= weap_top then ((sh - y' - space) / hght)
    else                     ((sh - y') / hght) in
  let bind = CardMap.bindings window.sheet in
  (index, fst (List.nth bind index))

let card_to_index c =
  let bindings = CardMap.bindings window.sheet in
  let rec loop n lst =
    match lst with
    | (card, _)::t -> if c = card then n else loop (n+1) t
    | _ -> failwith ("card not found: " ^ Pervasives.__LOC__) in
  loop 0 bindings

(* converts an HSV color to a Graphics.color.
 * Equations from www.rapidtables.com/convert/colors/hsv-to-rgb.htm *)
let hsv_to_rgb (h, s, v) =
  let (h, s, v) = (Pervasives.float h, Pervasives.float s, Pervasives.float v) in
  let rec fmod a b =
    if a < 0. then fmod (a +. b) b
    else if a >= b then fmod (a -. b) b
    else a in
  let c = v *. s in
  let h' = h/.60.0 in
  let h'' = fmod h' 2.0 in
  let x = c *. (1. -. (Pervasives.abs_float (h'' -. 1.))) in
  let m = v -. c in
  let (r',g',b') =
    if      0.   <= h && h < 60.  then (c,  x, 0.)
    else if 60.  <= h && h < 120. then (x,  c, 0.)
    else if 120. <= h && h < 180. then (0., c,  x)
    else if 180. <= h && h < 240. then (0., x,  c)
    else if 240. <= h && h < 300. then (x,  0., c)
    else if 300. <= h && h < 360. then (c,  0., x)
    else (0., 0., 0.) in
  let (r, g, b) = ((r'+.m)*.255., (g'+.m)*.255., (b'+.m)*.255.) in
  let trunc = Pervasives.truncate in
  Graphics.rgb (trunc r) (trunc g) (trunc b)

(* picks n equally spaced colors using evenly spaced HSVs and converting them
 * to RGBs *)
let pick_n_colors n =
  let rec loop acc j k =
    if j = k then acc else loop (j*(360/k)::acc) (j+1) k in
  let hues = loop [] 0 n in
  List.map (fun h -> hsv_to_rgb (h, 1, 1)) hues

(* draws the lines marking the exits to a room *)
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
let draw_board highlights () =
  (grect_curry draw_filled_rect window.b_window) Graphics.black Graphics.white;
  let is_highlight = List.length highlights <> 0 in
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
    (* let (gx, gy, gh, gw) = grect in *)
    let dr = (grect_curry draw_filled_rect grect) in
      (if is_highlight && not (List.mem coord highlights)
        then dr Graphics.black dark_room_color
        else dr Graphics.black room_color);
    (grect_curry center_text_in_rect grect) s;
    draw_exits transform loc in
  let g (coord, loc) =
    match loc.info with
    | Space (x, y) -> let grect = (x, y, 1, 1)
                      |> (scale_grect (x_mult, y_mult))
                      |> (shift_grect (xb, yb)) in
                      let dr = (grect_curry draw_filled_rect grect) in
                      if is_highlight && not (List.mem coord highlights)
                        then dr Graphics.black dark_space_color
                        else dr Graphics.black space_color
    | _ -> () in
  let coord_binds = CoordMap.bindings window.board.loc_map in
  List.iter g coord_binds;
  List.iter f room_bind

(* places players in rooms such that multiple players in the same rooms
 * won't overlap unless the room is actually too small to hold them *)
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

(* draws a colored circle for each player on the board in their corresponding
 * locations *)
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

(* draws the lower right button as an unhighlighted ROLL button *)
let draw_roll () =
  let grect = window.roll_window in
  (grect_curry draw_filled_rect grect) Graphics.black roll_color;
  (grect_curry center_text_in_rect grect) window.roll_text

(* draws the lower right button as a highlighted ROLL button *)
let highlight_roll text () =
  let grect = window.roll_window in
  window.roll_text <- text;
  (grect_curry draw_filled_rect grect) Graphics.black highlight_color;
  (grect_curry center_text_in_rect grect) window.roll_text

let set_roll_text text =
  window.roll_text <- text;
  draw_roll ()

type marking = MyCard of int | Env | Unk | SB of Graphics.color

(* draws the mark that corrsponds to [marking] in [grect] *)
let make_mark grect marking =
  match marking with
  | MyCard n ->
    let c = StringMap.find (window.sheet_disp) window.player_colors in
    (grect_curry draw_filled_rect grect) deck_border_color c;
    (grect_curry center_text_in_rect grect) (Pervasives.string_of_int n)
  | SB c -> (grect_curry draw_filled_rect grect) deck_border_color c
  | Unk -> (grect_curry draw_filled_rect grect) deck_border_color Graphics.white;
           (grect_curry center_text_in_rect grect) "?"
  | Env -> let (gx, gy, gw, gh) = grect in
           let (gx', gy') = (gx + gw/2, gy + gh/2) in
           (grect_curry draw_filled_rect grect) Graphics.black Graphics.white;
           Graphics.set_color Graphics.black;
           Graphics.moveto gx (gy+gh);
           Graphics.lineto gx' gy';
           Graphics.lineto (gx+gw) (gy+gh)

(* draws the sheet defined in [window_sheet] to fill [window.s_window] *)
let draw_sheet () =
  let grect = window.s_window in
  let (sx, sy, sw, sh) = grect in
  (grect_curry draw_filled_rect grect) Graphics.black Graphics.black;
  let card_counts = CardMap.cardinal window.sheet in
  let hght = if card_counts = 0 then 0 else sh/card_counts in
  let spacer = (sh - (hght * card_counts))/2 in
  let n = ref 0 in
  let spaces = ref 0 in
  let space1 = ref 0 in
  let space2 = ref 0 in
  let f card c_info =
    let (back_color, name) =
      match (card) with
      | Suspect s -> (suspect_color, s)
      | Weapon s -> (if !spaces = 0 then (space1 := !n; spaces := 1) else ());
                    (weapon_color, s)
      | Room s ->   (if !spaces = 1 then (space2 := !n; spaces := 2) else ());
                    (room_sheet_color, s) in
    let marking =
      match (c_info.card_info) with
      | Mine lst -> MyCard (List.length lst)
      | ShownBy who -> SB (StringMap.find who window.player_colors)
      | Unknown -> Unk
      | Envelope -> Env in
    let y' = sy + (card_counts - 1 - !n)*hght + (2- !spaces)*spacer in
    n := !n + 1;
    let grect' = (sx, y', sw, hght) in
    let grect_text = (sx, y', sw - hght, hght) in
    let grect_mark = (sx + sw - hght, y', hght, hght) in
    (grect_curry draw_filled_rect grect') deck_border_color back_color;
    (grect_curry center_text_in_rect grect_text) name;
    make_mark grect_mark marking in
  if window.sheet_disp = "" then ()
  else CardMap.iter f window.sheet

let draw_lock grect =
  let (x, y, w, h) = grect in
  let grect_box = (x + w/5, y + h/6, (w*3)/5, h/3) in
  let (xa, ya, rx, ry, a1, a2) = (x + (w/2), y + h/2, w/5, h/5, 0, 180) in
  (grect_curry draw_filled_rect grect_box) Graphics.black lock_color;
  Graphics.set_color Graphics.black;
  Graphics.set_line_width 3;
  Graphics.draw_arc xa ya rx ry a1 a2;
  Graphics.set_line_width 1

let draw_picked grect lock =
  if lock then draw_lock grect
  else
    let (x, y, w, h) = grect in
    let grect' = (x + w/6, y + h/6, (2*w)/3, (2*h)/3) in
    grect_curry draw_filled_rect grect' Graphics.black lock_color

let mark_sheet_guess (c_list: (card * bool) list) =
  draw_sheet ();
  let count = CardMap.cardinal window.sheet in
  let (sx, sy, sw, sh) = window.s_window in
  let hght = if count = 0 then 1 else sh/count in
  let space = (sh - (count * hght)) / 2 in
  let rec loop lst =
    match lst with
    | [] -> ()
    | (c, b)::t -> let i = card_to_index c in
                   let y = if      i < window.sus_count
                          then  sy + sh - ((i + 1) * hght)
                        else if i < window.sus_count + window.weap_count
                          then  sy + sh - ((i + 1) * hght) - space
                        else    sy + sh - ((i + 1) * hght) - 2*space in
                let grect = (sx, y, hght, hght) in
                draw_picked grect b; loop t in
  loop c_list

(* draws the info box with the most recent info message *)
let draw_info () =
  let grect = window.info_window in
  (grect_curry draw_filled_rect grect) Graphics.black Graphics.white;
  (grect_curry center_text_in_rect grect) window.last_info

(* changes the text in the info box and redraws it *)
let set_info s =
  window.last_info <- s; draw_info ()

let draw_log () =
  grect_curry draw_filled_rect window.log_window Graphics.black Graphics.white;
  let (x', y', w', h') = window.log_window in
  let top = y' + h' - 3 in
  Graphics.moveto (x'+3) (y'+3);
  (* prints the log from the bottom up *)
  let rec loop lst (x, y) (acc: (string*Graphics.color) list) =
    match lst with
    | [] -> (Graphics.set_color Graphics.black; acc)
    | (s', col')::t'' -> let (w, h) = Graphics.text_size s' in
                       let (w, s, col, t) = if w > (w' - 6) then
                       let lst' = split_long_string (w' - 6) s' in
                       let lst' = List.map (fun s -> (s, col')) lst' in
                       let ((sh, scol), t') = match List.rev lst' with
                          | (sh', scol')::tt -> ((sh', scol'), tt)
                          | _ -> failwith ("can't be empty: "
                                          ^ Pervasives.__LOC__) in
                       let (w'', _) = Graphics.text_size sh in
                       (w'', sh, scol, (List.rev (t'))@t'')
                      else (w, s', col', t'') in
                     if (Graphics.current_y () >= top - h) then acc
                     else
                      (Graphics.set_color col;
                      Graphics.draw_string s;
                      Graphics.rmoveto (-1*w) (h+1);
                      loop t (x-h, y-w) ((s, col)::acc)) in

  let acc = loop (List.rev window.log) (x', y') [] in
  window.log <- acc

let add_to_log lst =
  let rec loop lst acc =
    match lst with
    | [] -> acc
    | h::t -> loop t (List.rev (h::(List.rev acc))) in
  window.log <- loop lst window.log;
  draw_log ()

let draw_tick x dim py gy =
  let x_cent = x+dim/2 in
  let x_min = x_cent - (dim/3) in
  let x_max = x_cent + (dim/3) in
  let y_point = py - 10 in
  let y_min = gy + 15 in
  let y_max = (y_point + y_min)/2 in
  let ary = Array.of_list [(x_cent, y_point); (x_max, y_max); (x_max, y_min);
                           (x_min, y_min); (x_min, y_max)] in
  Graphics.set_color tick_color;
  Graphics.fill_poly ary;
  Graphics.set_color Graphics.black;
  Graphics.draw_poly ary

let draw_po tick () =
  grect_curry draw_filled_rect window.po_window Graphics.black Graphics.white;
  let (has_tick, tick_who) =
    match tick with
    | Some s -> (true, s)
    | None -> (false, "") in
    let (gx, gy, gw, gh) = window.po_window in
  let count = List.length window.po_lst in
  let max_w = if count <> 0 then (gw - 15*(count+1))/count
    else failwith ("count should never be 0: " ^ Pervasives.__LOC__) in
  let max_h = (gh - 30)/2 in
  let dim = if max_w < max_h then max_w else max_h in
  let space = (gw - dim*count)/(count+1) in
  let rec loop ps n =
    match ps with
    | [] -> ()
    | h::t -> let color = StringMap.find h window.player_colors in
              let x = gx + (n+1)*space + n*dim in
              let y = gy + gh - 15 - dim in
              let grect = (x, y, dim, dim) in
              grect_curry draw_ell_in_rect grect Graphics.black color;
              let _ = if has_tick && (h = tick_who)
                      then draw_tick x dim y gy else () in
              loop t (n+1) in
  loop (window.po_lst) 0

let po_start_turn () =
  draw_po (None) ()

let po_set_answer_tick who =
  draw_po (Some who) ()

(* redraws the entire window *)
let redraw () =
  draw_board [] ();
  draw_players ();
  draw_roll ();
  draw_sheet ();
  draw_info ();
  draw_log ();
  draw_po None ();
  ()

(* Displays the relocation of suspect [string] to the Room loc *)
let display_relocate (who:string) loc : unit =
  window.player_locs <- (StringMap.add who loc window.player_locs);
  redraw ()

(* highlights a location on the board *)
let highlight_loc transform loc =
  match loc.info with
  | Space (x, y) -> let (gx, gy, gw, gh) = transform (x, y, 1, 1) in
                    let grect' = (gx+1, gy+1, gw-2, gh-2) in
                    Graphics.set_color highlight_color;
                    (grect_curry Graphics.draw_rect grect')
  | Room_Rect (s, rect) -> let (gx, gy, gw, gh) = transform (rect_to_grect rect) in
                    let grect' = (gx+1, gy+1, gw-2, gh-2) in
                    Graphics.set_color highlight_color;
                    (grect_curry Graphics.draw_rect grect');
                    draw_exits transform loc

(* uses [highlight_loc] to highlight the location that corresponds to [coord] *)
let highlight_coord transform coord =
  let loc = (CoordMap.find coord window.board.loc_map) in
  highlight_loc transform loc

(* Displays the provided error message. *)
let display_error (e_msg: string) : unit =
  set_info ("ERROR: " ^ e_msg)

(* splits a [lst] at the first occurence of [el] and moves all preceeding
 * elements to the end. [cycle_to] maintains all relative order if the list
 * is considered cyclic. *)
let cycle_to el lst =
  let rec loop back front =
    match front with
    | [] -> back
    | h::t -> if h = el then front@back
              else loop (List.rev (h::(List.rev back))) t in
  loop [] lst

(* Displays a description of who's turn it is. *)
let display_turn (public:Data.public) : unit =
  let this_turn = public.curr_player in
  window.curr_player <- this_turn;
  let s = ("It is " ^ window.curr_player ^ "'s turn.") in
  window.po_lst <- cycle_to this_turn window.po_lst;
  po_start_turn ();
  set_info s;
  add_to_log [(s, StringMap.find this_turn window.player_colors)];
  if not (is_my_turn ()) then gui_delay 1.0 else gui_delay 0.5

(* Prompts the user for a file so that it can be imported into the Model *)
let prompt_filename () : string =
  failwith "Unimplemented gui.prompt_filename"

(* Prompts the user for whether he rolls dice or not. *)
let prompt_move_gui (movelst: move list) : move =
  let coords = ref [] in
  let add_loc_coord loc =
    match loc.info with
    Room_Rect (_,(x,_,y,_)) | Space (x,y) -> coords := (x,y)::(!coords) in
  let f acc move =
    match move with
    | Passage loc -> add_loc_coord loc; loc::acc
    | Roll -> highlight_roll "ROLL" (); acc in
  let loclst = List.fold_left f [] movelst in
  draw_board !coords ();
  draw_players();
  let rectlst = [("board", window.b_window); ("roll", window.roll_window)] in
  let rec loop () =
    match get_next_click_in_rects rectlst () with
    | ("board", pt) -> let coord = translate_to_coord pt in
                       let loc = CoordMap.find coord window.board.loc_map in
                       if List.mem loc loclst then Passage loc else loop ()
    | ("roll", _) -> draw_roll (); Roll
    | (s, _) -> failwith ("not an included string " ^ s ^ ": "
                          ^ Pervasives.__LOC__) in
  (if List.length loclst = 0 then set_info "ROLL THE DICE"
  else set_info "SELECT A PASSAGE or ROLL THE DICE");
  loop ()

let prompt_move (movelst: move list) : string =
  let loc_name loc = match loc.info with
  | Room_Rect (s, _) -> s
  | _ -> failwith ("can't have passage to space: " ^ Pervasives.__LOC__) in
  match prompt_move_gui movelst with
  | Roll -> "roll"
  | Passage loc -> loc_name loc

(* Displays a description of what the agent rolled. *)
let display_dice_roll (roll: int) : unit =
  let s = (" has rolled a " ^ (Pervasives.string_of_int roll) ^ ".") in
  set_info (window.curr_player ^ s);
  add_to_log [((window.curr_player ^ s), Graphics.black)];
  if not (is_my_turn ()) then gui_delay 1.0 else ()

(* Displays a description of whether the agent elected to Roll or Passage. *)
let display_move move : unit =
  let f loc = match loc.info with
  | Room_Rect (s,_) -> s
  | Space _ -> failwith ("can't take a passage to a space: "
                          ^ Pervasives.__LOC__)
  in match move with
  | Passage loc ->
    let s = window.curr_player ^ " has taken the passage to " ^ (f loc) in
    set_info s; add_to_log [(s, Graphics.black)];
    display_relocate window.curr_player loc
  | Roll ->
    let s = window.curr_player ^ " rolled the dice." in
    set_info s; add_to_log [(s, Graphics.black)];
    if not (is_my_turn ()) then gui_delay 1.0 else ()

(* Prompts the user for the room they would like to go to.
 * [loc * (string * bool)] the location and whether or not room [string] is
 * accessible. The second string parameter is the acc_room name. *)
let prompt_movement pathmap acc_room roll : loc =
  let guard = (fun (x, y) (n, (x', y')) -> n <= roll && n > 0) in
  let pm = PathMap.filter guard pathmap in
  let hc' = PathMap.keys pm in
  let start_loc = StringMap.find window.curr_player window.player_locs in
  let coord = match start_loc.info with
              | Space (x, y) | Room_Rect (_,(x,_,y,_)) -> (x,y) in
  let highlight_coords = List.filter (fun l -> not (l = coord)) hc' in
  (*let (xb, yb, wb, hb) = window.b_window in
  let (x_mult, y_mult) = get_mults () in
  let transform x = x |> scale_grect (x_mult, y_mult)
                      |> shift_grect (xb, yb) in
  List.iter (highlight_coord transform) highlight_coords;*)
  draw_board highlight_coords ();
  draw_players ();
  let rec loop () =
    let f = grect_curry get_next_click_in_rect window.b_window in
    let click_coord = translate_to_coord (f ())
                      |> adjust_coord_if_room in
    if List.mem click_coord highlight_coords
    then CoordMap.find click_coord window.board.loc_map
    else loop () in
  set_info (window.last_info ^ "\nSELECT A PLACE TO MOVE TO");
  loop ()

(* Displays the movement the agent took on its turn *)
let display_movement (l, (s, b)) : unit =
  display_relocate window.curr_player l;
  let log_text = if b then window.curr_player ^ " entered the " ^ s
               else window.curr_player ^ " headed towards the " ^ s in

  add_to_log [(log_text, Graphics.black)];
  if not (is_my_turn ()) then gui_delay 1.0 else ()

(* Prompts the user for a guess.
 * Takes in the current location (must be a room) and
 * a bool which says whether or not it is the final accusation.
 * returns a string of the user's response. *)
let prompt_guess loc (is_acc: bool) : string =
  let has_room = ref (not is_acc) in
  let has_weap = ref false in
  let has_sus = ref false in
  po_set_answer_tick window.curr_player;
  let r = ref
    (match (is_acc, loc.info) with
    | (true, _) -> (Room "")
    | (false, Room_Rect (s, _)) -> (Room s)
    | _ -> failwith ("guess must be from room: " ^ Pervasives.__LOC__)) in
  let w = ref (Weapon "") in
  let s = ref (Suspect "") in
  (if is_acc then set_info "MAKE YOUR FINAL ACCUSATION"
  else set_info "MAKE YOUR GUESS");
  let rec loop () =
    let marks = if !has_room then [((!r), (not is_acc))] else [] in
    let marks = if !has_weap then  ((!w), false)::marks  else marks in
    let marks = if !has_sus  then  ((!s), false)::marks  else marks in
    mark_sheet_guess marks;
    let rects = if (!has_sus && !has_weap && !has_room)
      then (highlight_roll "GUESS" ();
        [("guess", window.roll_window); ("sheet", window.s_window)])
      else [("sheet", window.s_window)] in
    match get_next_click_in_rects rects () with
    | ("guess", _) -> draw_roll (); (!s, !w, !r)
    | ("sheet", pt) -> set_card (translate_to_card pt); loop ()
    | _ -> loop ()
  and set_card (n, card) =
    match (n, card) with
    | (i, Suspect _) -> s := card; has_sus := true
    | (i, Weapon _) -> w := card; has_weap := true
    | (i, Room _) -> has_room := true;
                if is_acc then r := card else () in
  set_roll_text "GUESS";
  let (csus, cweap, croom) = loop () in
  draw_roll ();
  match csus, cweap, croom with
  | Suspect sus, Weapon weap, Room room -> (sus ^ ", " ^ weap ^ ", " ^ room)
  | _ -> failwith ("guess in wrong order: " ^ Pervasives.__LOC__)


(* Displays a guess (by the user or AI). *)
let display_guess guess : unit =
  let guesser = window.curr_player in
  po_set_answer_tick window.curr_player;
  let _ = match guess with
  | (Suspect who, Weapon what, Room where) ->
    let s0 = guesser ^ " thinks " in
    let s1 = "it was " ^ who in
    let s2 = "with the " ^ what in
    let s3 = "in the " ^ where ^ "." in
    set_info (s0 ^ s1 ^ "\n" ^ s2 ^ "\n" ^ s3);
    add_to_log [(s0, StringMap.find guesser window.player_colors);
                ("    "^s1, Graphics.black);
                ("    "^s2, Graphics.black);
                ("    "^s3, Graphics.black)]

  | _ -> failwith ("bad guess order: " ^ Pervasives.__LOC__) in
  if not (is_my_turn ()) then gui_delay 3.0 else ()


let make_rects lst =
  let (bx, by, bw, bh) = window.b_window in
  let (w, h) = (bw/4, (bw*3)/8) in
  let count = List.length lst in
  let rec loop n acc lst =
    match lst with
    | [] -> acc
    | (Suspect s)::t -> let x =
                          if count = 1
                            then  bx + (bw - w)/2
                          else if count = 2
                            then  bx + ((n+1)*bw)/6 + w*n
                          else    bx + ((n+1)*bw)/16 + w*n in
                        let grect = (x, by+(bh-h)/2, w, h) in
                        let r = (grect_curry draw_filled_rect grect) in
                        r Graphics.black suspect_color;
                        (grect_curry center_text_in_rect grect s);
                        loop (n+1) (("suspect", grect)::acc) t
    | (Weapon s)::t ->  let x =
                          if count = 1
                            then  bx + (bw - w)/2
                          else if count = 2
                            then  bx + ((n+1)*bw)/6 + w*n
                          else    bx + ((n+1)*bw)/16 + w*n in
                        let grect = (x, by+(bh-h)/2, w, h) in
                        let r = (grect_curry draw_filled_rect grect) in
                        r Graphics.black weapon_color;
                        (grect_curry center_text_in_rect grect s);
                        loop (n+1) (("weapon", grect)::acc) t
    | (Room s)::t ->    let x =
                          if count = 1
                            then  bx + (bw - w)/2
                          else if count = 2
                            then  bx + ((n+1)*bw)/6 + w*n
                          else    bx + ((n+1)*bw)/16 + w*n in
                        let grect = (x, by+(bh-h)/2, w, h) in
                        let r = (grect_curry draw_filled_rect grect) in
                        r Graphics.black room_sheet_color;
                        (grect_curry center_text_in_rect grect s);
                        loop (n+1) (("room", grect)::acc) t in
  loop 0 [] lst

(* Prompts the user for a card to show.
 * Can be any card from the provided hand, and must be in the guess.
 * Can be none if there is no card to show. *)
let rec prompt_answer hand guess : string =
  set_info (window.last_info^"\nPICK A CARD TO SHOW");
  po_set_answer_tick window.sheet_disp;
  grect_curry draw_filled_rect window.b_window Graphics.black answer_back;
  let (sus, weap, room) = guess in
  let can_show = if List.mem sus hand then [sus] else [] in
  let can_show = if List.mem weap hand then weap::can_show else can_show in
  let can_show = if List.mem room hand then room::can_show else can_show in
  let rects = make_rects can_show in
  let click = get_next_click_in_rects rects () in
  let extract_card_text c = match c with
    | Suspect s | Weapon s | Room s -> s in
  draw_board [] (); draw_players ();
  match click with
  | ("suspect", _) -> extract_card_text sus
  | ("weapon", _) -> extract_card_text weap
  | ("room", _) -> extract_card_text room
  | _ -> prompt_answer hand guess

(* Displays the card shown to the human agent and by whom.
 * If None, no card could be shown. If false, the user is not shown the
 * details of the card. *)
let display_answer (c:card option) (who: string) (detail: bool) : unit =
  let my_ans = who = window.sheet_disp in
  po_set_answer_tick who;
  let card_detail c =
    match c with
    | Suspect s -> "a Suspect: " ^ s
    | Weapon s -> "a Weapon: " ^ s
    | Room s -> " a Room: " ^ s in
  let s =
    match detail, c with
    | true, None -> "Nobody could show a card."
    | false, None -> "Nobody could show a card."
    | true, Some c -> who ^ " shows you " ^ card_detail c
    | false, Some c -> who ^ " showed a card from their hand." in
  set_info s; add_to_log [(s, Graphics.black)];
  if not my_ans then gui_delay 0.0 else ()

(* Displays that the player [string] could not answer with a card.
 * This is different from no one being able to show a card. *)
let display_no_answer (who: string) : unit =
  po_set_answer_tick who;
  set_info (who ^ " has nothing to show.");
  add_to_log [(who ^ " has nothing to show.", Graphics.black)];
  gui_delay 2.0

(* Displays end game victory text, string is who won. *)
let display_victory (who: string) : unit =
  set_info (who ^ " WINS!")

(* Displays arbitrary text. *)
let display_message (text: string) : unit =
  set_info text;
  gui_delay 2.0

let init game =
  Graphics.open_graph "";
  window.board <- game.public.board;
  window.player_locs <- StringMap.empty;
  window.player_colors <- StringMap.empty;
  let (s, w, r) = game.public.deck in
  window.sus_count <- List.length s;
  window.weap_count <- List.length w;
  window.weap_count <- List.length r;
  window.po_lst <- game.public.player_order;
  (*let colors = pick_n_colors p_count in*)
  let count = ref 0 in
  let f me =
    let sus = me.suspect in
    let (r,g,b) = me.color in
    let color = Graphics.rgb r g b in
    count := !count + 1;
    window.player_locs <- StringMap.add sus me.curr_loc window.player_locs;
    window.player_colors <- StringMap.add sus color window.player_colors;
    if me.agent = Human_t then
      if true (* window.sheet_disp = "" *)
      then (window.sheet_disp <- sus; window.sheet <- me.sheet)
      else failwith ("found two human agents: " ^ Pervasives.__LOC__)
    else ()
  in List.iter f game.players;
  if List.length game.players > 0 && window.sheet_disp = ""
  then let p = List.hd game.players in
    (window.sheet_disp <- p.suspect; window.sheet <- p.sheet)
  else ();
  let (gx, gy) = window.win_bounds in
  Graphics.resize_window gx gy;
  Graphics.set_window_title "CLUE";
  let s = "You are " ^ window.sheet_disp ^ "." in
  set_info (s);
  add_to_log [(s, StringMap.find window.sheet_disp window.player_colors)];
  redraw ()

let show_sheet sheet : unit =
  window.sheet <- sheet;
  draw_sheet ()

let show_hand hand : unit =
  ()

let prompt_continue () : unit =
  let cont_rect = window.roll_window in
  let rects = [("continue", cont_rect)] in
  let loop () =
    match get_next_click_in_rects rects () with
    | ("continue", _) -> ();
    | _ -> failwith ("not a defined rectangle: "
                     ^ Pervasives.__LOC__)  in
  highlight_roll "CONTINUE" ();
  loop ();
  draw_roll ()

let prompt_end_game () : unit =
  let cont_rect = window.roll_window in
  let rects = [("quit", cont_rect)] in
  let loop () =
    match get_next_click_in_rects rects () with
    | ("quit", _) -> ();
    | _ -> failwith ("not a defined rectangle"
                      ^ Pervasives.__LOC__) in
  highlight_roll "QUIT" ();
  loop ()