(**
 * Main starts up the game using the passed in CLI argument as the game config.
 *)
let () =
  let file_name = Sys.argv.(1) in
  let model = Model.import_board file_name in
  Controller.step model