open OUnit2
open Data
open Controller
open Model

let game1 = Model.import_board "classic_dumb.json"
let game2 = Model.import_board "small_mixed.json"
let p1a = List.hd game1.players
let p2a = List.hd game2.players

let c1 = (7, 0)
let c2 = (6, 10)
let coord_of_loc loc = match loc.info with
                       Space (x, y) | Room_Rect (_,(x,_,y,_)) -> (x, y)

let (_, pm1) = get_movement_options game1 12
let (_, pm2) = get_movement_options game2 12

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

let tests = pathmap_tests

let suite =
  "Clue test suite"
  >::: tests

let _ = run_test_tt_main suite
