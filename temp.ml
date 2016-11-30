


let display_message view text =
  match view with
  | CLI -> Cli.display_message text
  | GUI -> Gui.display_message text

let prompt_filename view () =
  match view with
  | CLI | GUI -> Cli.prompt_filename ()