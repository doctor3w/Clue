open OUnit2
open Controller

let tests =
[

]

let suite =
  "Clue test suite"
  >::: tests

let _ = run_test_tt_main suite
