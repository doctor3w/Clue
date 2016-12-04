(**
 * Main starts up the game using the passed in CLI argument as the game config.
 *)
open Data

(* Raised when input arguments are invalid. *)
exception Not_an_option

(* Prints the usage of the program *)
let print_usage () =
  print_endline "USAGE:";
  print_endline "";
  print_endline "    ./main.byte [OPTIONS] [file_name]";
  print_endline "";
  print_endline "OPTIONS:";
  print_endline "    -gui : Uses a GUI instead of CLI";
  print_endline "";
  print_endline "file_name : optional filename to load on run";
  print_endline ""

(* Parses the arguments passed in when program is launched *)
let parse_args () =
  if Array.length Sys.argv > 2 then
    let g_or_c =
      try
        if Sys.argv.(1) = "-gui" then GUI else raise Not_an_option
      with
      | Invalid_argument _ -> CLI in
    let file_name = try Some Sys.argv.(2) with Invalid_argument _ -> None in
    (file_name, g_or_c)
  else
    let g_or_c =
      try
        if Sys.argv.(1) = "-gui" then GUI else CLI
      with
      | Invalid_argument _ -> CLI in
    let file_name =
      if g_or_c = GUI then None
      else try Some Sys.argv.(1) with Invalid_argument _ -> None in
    (file_name, g_or_c)

let () =
  let () = ANSI.print_title () in
  try
    let (fn, gc) = parse_args () in
    Controller.start fn gc
  with
  | Not_an_option ->
    Cli.display_error "\nThat's an invalid argument option!";
    print_usage ()