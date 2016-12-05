open OUnit2
open Data
open Controller
open Model
open Responsive

let game1 = Model.import_board "tests/classic_dumb.json"
let game2 = Model.import_board "tests/small_mixed.json"
let p1a = List.hd game1.players
let p2a = List.hd game2.players

let c1 = (7, 0)
let c2 = (6, 10)
let coord_of_loc loc = match loc.info with
                       Space (x, y) | Room_Rect (_,(x,_,y,_)) -> (x, y)

let (_, pm1) = get_movement_options game1 12
let (_, pm2) = get_movement_options game2 12

let loc1 = {info = Room_Rect("1",(1,1,1,1));
            edges = []}

let blue = Suspect "blue"
let green = Suspect "green"
let red = Suspect "red"
let yellow = Suspect "yellow"
let s_lst = [blue;green;red;yellow]

let gun = Weapon "gun"
let rope = Weapon "rope"
let sword = Weapon "sword"
let w_lst = [gun;rope;sword]

let room1 = Room "1"
let room2 = Room "2"
let room3 = Room "3"
let room4 = Room "4"
let r_lst = [room1;room2;room3;room4]

(* let redp = {suspect="red";
               hand=[gun; room3];
               curr_loc=loc1;
               sheet=default_sheet (s_lst@w_lst@r_lst);
               agent=ResponsiveAI_t;
               is_out=false;
               listen= Array.make_matrix 11 4 Pure_unknown}

let bluep = {suspect="blue";
               hand=[sword; room2];
               curr_loc=loc1;
               sheet=default_sheet (s_lst@w_lst@r_lst);
               agent=ResponsiveAI_t;
               is_out=false;
               listen= Array.make_matrix 11 4 Pure_unknown}

let greenp = {suspect="green";
               hand=[red; yellow];
               curr_loc=loc1;
               sheet=default_sheet (s_lst@w_lst@r_lst);
               agent=ResponsiveAI_t;
               is_out=false;
               listen= Array.make_matrix 11 4 Pure_unknown}

let yellowp = {suspect="yellow";
               hand=[green; room4];
               curr_loc=loc1;
               sheet=default_sheet (s_lst@w_lst@r_lst);
               agent=ResponsiveAI_t;
               is_out=false;
               listen= Array.make_matrix 11 4 Pure_unknown} *)


let pub = {
    curr_player = "red";
    acc_room = "acc";
    board = {
      dim = (-1,-1);
      loc_map = CoordMap.empty;
      room_coords = StringMap.empty;
    };
    deck = (s_lst,w_lst,r_lst);
    player_order = ["red";"blue";"green";"yellow"];
  }


let pathmap_tests =
[
  "pm1_id" >:: (fun _ -> assert_equal 0 (PathMap.length_to c1 pm1));
  "pm2_id" >:: (fun _ -> assert_equal 0 (PathMap.length_to c2 pm2));
  "pm1_first_step" >:: (fun _ -> assert_equal 1 (PathMap.length_to (7,1) pm1));
  "pm1_snd_step" >:: (fun _ -> assert_equal 2 (PathMap.length_to (8,1) pm1));
  "pm1_wall_nostep" >:: (fun _ -> assert_equal 8
    (PathMap.length_to
      (coord_of_loc (CoordMap.find (6,0) game1.public.board.loc_map))
      pm1));
  "pm1_proof_no_passage" >:: (fun _ -> assert_equal 30
    (PathMap.length_to (18, 19) pm1));
  "pm1_proof_closest" >:: (fun _ -> assert_equal (7, 5)
    (PathMap.nth_step_towards (7, 7) 5 pm1));
]

let responsive_tests =
[
  (* "take_note" >:: (fun _ -> assert_equal 0 (PathMap.length_to c1 pm1)); *)
]

let tests = pathmap_tests@responsive_tests

let suite =
  "Clue test suite"
  >::: tests

let _ = run_test_tt_main suite
