type color =
    Black | Red | Green | Yellow | Blue | Magenta | Cyan | White | Default

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

let print_with pr style txt =
  pr "\027[";
  pr (String.concat ";" (List.map style_to_string style));
  pr "m";
  pr txt;
  pr "\027[0m"

let print_string style txt = print_with print_string style txt