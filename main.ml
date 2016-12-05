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
  let g_or_c = ref CLI in
  let fn = ref None in
  let sd = ref (-1) in
  let r = Str.regexp "[0-9]+" in
  let json_r = Str.regexp "json" in
  for i = 1 to Array.length Sys.argv - 1 do
    let seed = try ignore (Str.search_forward r Sys.argv.(i) 0); true
               with _ -> false in
    let json = try ignore (Str.search_forward json_r Sys.argv.(i) 0); true
               with _ -> false in
    if Sys.argv.(i) = "-gui" then
      g_or_c := GUI
    else if seed then
      sd := int_of_string (Sys.argv.(i))
    else if json then
      fn := Some (Sys.argv.(i))
    else raise Not_an_option
  done;
  if !sd = -1 then Random.self_init ()
  else Random.init !sd;
  (!fn, !g_or_c)

let () =
  let () = ANSI.print_title () in
  try
    let (fn, gc) = parse_args () in
    Controller.start fn gc
  with
  | Not_an_option ->
    Cli.display_error "\nThat's an invalid argument option!";
    print_usage ()