open View
open Cli
open Model
open Data

let Display = Cli

(* [step] Recursively progresses through the game by doing one agent turn
 * at a time. *)
let step model =
  failwith "unimplemented"

let start file_name =
  let load_go fl = step (Model.import_board fl) in
  match file_name with
  | None -> load_go (Display.prompt_filename ())
  | Some s -> load_go fl