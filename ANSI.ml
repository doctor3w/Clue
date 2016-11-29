(**
 * This is a modified version of the ANSITerminal package.
 * It is used for printing out strings one character a time but still with
 * the colored text of ANSITerminal. Most of the code is the same, but the
 * single character feature is new.
 **)

(* Custom colors can carry an int in the range of 0-255 *)
type color =
    Black | Red | Green | Yellow | Blue | Magenta | Cyan | White | Default
    | Custom of int

type style =
  | Reset | Bold | Underlined | Blink | Inverse | Hidden
  | Foreground of color
  | Background of color

let black = Foreground Black
let red = Foreground Red
let green = Foreground Green
let yellow = Foreground Yellow
let blue = Foreground Blue
let magenta = Foreground Magenta
let cyan = Foreground Cyan
let white = Foreground White
let default = Foreground Default
let custom c = Foreground (Custom c)

let on_black = Background Black
let on_red = Background Red
let on_green = Background Green
let on_yellow = Background Yellow
let on_blue = Background Blue
let on_magenta = Background Magenta
let on_cyan = Background Cyan
let on_white = Background White
let on_default = Background Default

let style_to_string s = match s with
  | Reset -> "0"
  | Bold -> "1"
  | Underlined -> "4"
  | Blink -> "5"
  | Inverse -> "7"
  | Hidden -> "8"
  | Foreground Black -> "30"
  | Foreground Red -> "31"
  | Foreground Green -> "32"
  | Foreground Yellow -> "33"
  | Foreground Blue -> "34"
  | Foreground Magenta -> "35"
  | Foreground Cyan -> "36"
  | Foreground White -> "37"
  | Foreground (Custom c) -> "38;5;"^(string_of_int c)
  | Foreground Default -> "39"
  | Background Black -> "40"
  | Background Red -> "41"
  | Background Green -> "42"
  | Background Yellow -> "43"
  | Background Blue -> "44"
  | Background Magenta -> "45"
  | Background Cyan -> "46"
  | Background White -> "47"
  | Background Default -> "49"
  | Background (Custom c) -> "48;5;"^(string_of_int c)

let print_with pr style txt =
  pr "\027[";
  pr (String.concat ";" (List.map style_to_string style));
  pr "m";
  pr txt;
  pr "\027[0m"

let print_string style txt = print_with print_string style txt

(* turn the testing flag on to remove the delay *)
let testing = false
let char_delay = if testing then 0. else 0.035

(* Possible way to delay instead of with Thread *)
let sleep sec = ignore (Unix.select [] [] [] sec)

let rec print_list l cs =
  for i = 0 to (List.length l)-1 do
    print_string cs (List.nth l i);
    Thread.delay char_delay;
    flush stdout
  done

let rec string_list s l =
  if String.length s = 1 then s::l
  else
    let c = String.sub s 0 1 in
    string_list (String.sub s 1 (String.length s - 1)) (c::l)

(* takes in a string and prints one character at a time *)
let print_chars (cs: style list) s =
  let l = string_list s [] in
  print_list (List.rev l) cs


let print_title () =
  let s =
  " ▄████▄   ██▓     █    ██ ▓█████  ▐██▌
▒██▀ ▀█  ▓██▒     ██  ▓██▒▓█   ▀  ▐██▌
▒▓█    ▄ ▒██░    ▓██  ▒██░▒███    ▐██▌
▒▓▓▄ ▄██▒▒██░    ▓▓█  ░██░▒▓█  ▄  ▓██▒
▒ ▓███▀ ░░██████▒▒▒█████▓ ░▒████▒ ▒▄▄
░ ░▒ ▒  ░░ ▒░▓  ░░▒▓▒ ▒ ▒ ░░ ▒░ ░ ░▀▀▒
  ░  ▒   ░ ░ ▒  ░░░▒░ ░ ░  ░ ░  ░ ░  ░
░          ░ ░    ░░░ ░ ░    ░       ░
░ ░          ░  ░   ░        ░  ░ ░
░                                      " in
  print_string [] "\n\n\n\n";
  print_string [red] s; print_string [] "\n"