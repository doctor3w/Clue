(**
 * Main starts up the game using the passed in CLI argument as the game config.
 *)
let () =
  let () = print_string "Initializing...\n\n\n\n\n\n\nWelcome to Clue!\n" in
  let file_name = try Some Sys.argv.(1) with Invalid_argument _ -> None in
  Controller.start file_name