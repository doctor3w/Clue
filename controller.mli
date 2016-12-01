open View
open Model
open Data

(* [step] Recursively progresses through the game by doing one agent turn
 * at a time. *)
val step : game -> unit

(* [start] uses view to prompt for a filename which is then passed to the
 * model to build the initial game state. [start] terminates by calling [step]*)
val start : string option -> view -> unit