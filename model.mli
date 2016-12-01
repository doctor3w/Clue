open Data

(* [import_board] takes in a filename of a game configuration file and
 * converts the file into a usable game model for stepping through. *)
val import_board : string -> game

(* [get_move_options] gets the options of Roll and Passage that the current
 * player can make. *)
val get_move_options : game -> move list

(* [get_movement_options] gets the options of the locations that the current
 * player can move to within the dice roll int. These options come with
 * the room name [string] that is headed for and a [bool] to tell whether
 * that room can be entered. *)
val get_movement_options : game -> int -> ((loc * (string * bool)) list * PathMap.t)
