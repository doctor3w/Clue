open OUnit2
open Data
open Controller
open Model
open Agent

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


let default_sheet full_deck =
  let f acc el = CardMap.add el {card_info=Unknown; note=No_Note} acc
  in  List.fold_left f CardMap.empty full_deck

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

let redp = {suspect="red";
               hand=[gun; room3];
               curr_loc=loc1;
               sheet=default_sheet (s_lst@w_lst@r_lst);
               agent=DumbAI_t;
               is_out=false;
               listen= Array.make_matrix 11 4 Pure_unknown;
               color= (255,0,0);}

let greenp = {suspect="green";
               hand=[red;yellow];
               curr_loc=loc1;
               sheet=default_sheet (s_lst@w_lst@r_lst);
               agent=SmartAI_t;
               is_out=false;
               listen= Array.make_matrix 11 4 Pure_unknown;
               color= (255,0,0);}

let update_hand player hand =
  let f acc el =
    let sh = acc.sheet in
    let d = CardMap.find el sh in
    let d' = match d.card_info with
            | Unknown -> {d with card_info = Mine []}
            | _ -> d in
    let sh' = CardMap.add el d' sh in
    {acc with sheet = sh'} in
  List.fold_left f player hand

let greenp_m = update_hand greenp greenp.hand

let yellowp = {suspect="yellow";
               hand=[green; room4];
               curr_loc=loc1;
               sheet=default_sheet (s_lst@w_lst@r_lst);
               agent=ResponsiveAI_t;
               is_out=false;
               listen= Array.make_matrix 11 4 Pure_unknown;
               color= (255,0,0);}

let yellowp_m = update_hand yellowp yellowp.hand


let redr =  {suspect="red";
               hand=[gun; room3];
               curr_loc=loc1;
               sheet=default_sheet (s_lst@w_lst@r_lst);
               agent=ResponsiveAI_t;
               is_out=false;
               listen= Array.make_matrix 11 4 Pure_unknown;
               color = (255,0,0)}
let pub = {
    curr_player = "yellow";
    acc_room = "acc";
    board = {
      dim = (-1,-1);
      loc_map = CoordMap.empty;
      room_coords = StringMap.empty;
    };
    deck = (s_lst,w_lst,r_lst);
    player_order = ["red";"blue";"green";"yellow"];
  }


let pubr = {
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

let valid_answer_move move =
  match move with
  | Roll | Passage _-> true
 (*  | _ ->failwith "invalid" *)

let loc2 = {info = Room_Rect("2",(1,1,1,1)); edges = []}
let move_list = [Roll; Passage (loc1)]
let move_options = [(loc2,("2",true));(loc1,("1",false))]

let valid_move (movement: loc*(string*bool))=
  movement = (loc2,("2",true))

let valid_guess guess =
  let (s,w,r) = guess in
  match (s,w,r) with
  | (Suspect _, Weapon _, Room _) -> true
  | _ -> false

let valid_accusation pl guess =
  let (s,w,r) = guess in
  let valid = match (s,w,r) with
              | (Suspect _, Weapon _, Room _) -> true
              | _->false in
  let is_s = (CardMap.find s pl.sheet).card_info in
  let is_w = (CardMap.find w pl.sheet).card_info in
  let is_r = (CardMap.find r pl.sheet).card_info in
  is_s = Envelope && is_w = Envelope && is_r = Envelope && valid

let update_player player (s,w,r) =
  let lst = [s;w;r] in
  let f acc el =
    let sh = acc.sheet in
    let d = CardMap.find el sh in
    let d' = match d.card_info with
            | Unknown -> {d with card_info = Envelope}
            | _ -> d in
    let sh' = CardMap.add el d' sh in
    {acc with sheet = sh'} in
  List.fold_left f player lst

let m =   [|[|Not_in_hand; Pure_unknown; Pure_unknown; Pure_unknown|];
          [|Not_in_hand; Pure_unknown; Pure_unknown; Pure_unknown|];
          [|Not_in_hand; Pure_unknown; Pure_unknown; Pure_unknown|];
          [|Not_in_hand; Pure_unknown; Pure_unknown; Pure_unknown|];
          [|Known; Not_in_hand; Not_in_hand; Not_in_hand|];
          [|Not_in_hand; Pure_unknown; Pure_unknown; Pure_unknown|];
          [|Not_in_hand; Pure_unknown; Pure_unknown; Pure_unknown|];
          [|Not_in_hand; Pure_unknown; Pure_unknown; Pure_unknown|];
          [|Not_in_hand; Pure_unknown; Pure_unknown; Pure_unknown|];
          [|Known; Not_in_hand; Not_in_hand; Not_in_hand|];
          [|Not_in_hand; Pure_unknown; Pure_unknown; Pure_unknown|]|]

let redp_env = update_player redp (blue,rope,room3)
let greenp_env = update_player greenp (blue,rope,room3)
let yellowp_env = update_player yellowp (blue,rope,room3)

let agent_tests =
[
  "dumbai answer move">:: (fun _->assert_equal true
      (valid_answer_move (Agent.answer_move redp pub move_list)));
  "smartai answer move">:: (fun _->assert_equal true
      (valid_answer_move (Agent.answer_move greenp pub move_list)));
  "responsive answer move">:: (fun _->assert_equal true
      (valid_answer_move (Agent.answer_move yellowp pub move_list)));
  "dumbai get_movement">::(fun _->assert_equal (loc2,("2",true))
       (DumbAI.get_movement redp pub move_options));
  "responsive movement" >:: (fun _->assert_equal (loc2,("2",true))
        (Responsive.get_movement yellowp pub move_options));
  "dumbai get guess" >::(fun _-> assert_equal true
                          (valid_guess (Agent.get_guess redp pub)));
  "smartai get guess" >::(fun _-> assert_equal true
                          (valid_guess (Agent.get_guess greenp pub)));
  "Responsive get guess" >:: (fun _->assert_equal true
                          (valid_guess (Agent.get_guess yellowp pub)));
  "dumbai get accusation" >:: (fun _->assert_equal true
        (valid_accusation (redp_env) (Agent.get_accusation redp_env pub)));
  "smartai get accusation" >:: (fun _->assert_equal true
        (valid_accusation greenp_env (Agent.get_accusation greenp_env pub)));
  "Responsive get_accusation" >:: (fun _->assert_equal true
        (valid_accusation yellowp_env (Agent.get_accusation yellowp_env pub)));
  "dumbai get_answer" >:: (fun _->assert_equal true
    (None = Agent.get_answer redp pub (yellow,rope,room4)));
  "dumbai get_answer 1" >:: (fun _->assert_equal true
    (None <> Agent.get_answer redp pub (yellow,gun,room3)));
  "smartai get_answer" >:: (fun _->assert_equal true
    (None <> Agent.get_answer greenp_m pub (yellow, rope, room3)));
  "responsive get_answer" >:: (fun _->assert_equal true
    (None <> Agent.get_answer yellowp_m pub (yellow,rope,room4)));

  "responsive first_take_note" >:: (fun _->assert_equal {redr with listen = m}
  (Agent.first_take_note redr pubr));

]

let tests = pathmap_tests@agent_tests

let suite =
  "Clue test suite"
  >::: tests

let _ = run_test_tt_main suite
