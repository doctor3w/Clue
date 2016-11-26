open Data
open ANSITerminal

(* Displays the provided error message stored in [e] in color red. *)
let display_error (e:string): unit = print_string [red] (e ^ "\n")

(* Displays a description of who's turn it is.
   The format of print statment is "This is [player]'s turn",
   where the information about the [player] is stored in [pub] *)
let display_turn (pub:public): unit =
	let () = print_string [] "\n-----------------------------------\n\nThis is " in
	let () = print_string [cyan] pub.curr_player in
	print_endline "'s turn."

(* Prompts the user for a file so that it can be imported into the Model *)
let prompt_filename () : string =
	print_string [red]
		"Please enter the name of the game file you want to load.\n\n";
	print_string [red] "> ";
	read_line ()

(* Prompts the user for whether he rolls dice or not.
	[prompt_move] takes [l], which is a move list and prints out all the
	possible moves the player could take. All of the option are in color green *)
let prompt_move moves =
	let intro =
		"Would you like to roll the dice or take a passage? Choose from:\n" in
	let disp_loc l =
		match l.info with
		| Room_Rect (s, i) -> "Take the Passage into " ^ s ^ "; \n"
		| _ -> "" in
	let fold acc move =
		match move with
		| Roll -> "Roll dice; \n"
		| Passage loc -> disp_loc loc in
	let print_st = List.fold_left fold "" moves in
	print_string [Bold] intro;
	print_string [magenta] print_st;
	print_string [green] "\n>>> ";
	read_line ()


(* Displays a description of whether the agent elected to Roll or Passage. *)
let display_move (m:move) : unit =
	let disp_loc l =
		match l.info with
		| Room_Rect (s,i) ->
			print_endline ("The player elected to take the Passage to " ^ s)
		| _ -> failwith "A space is not displayed here"
	in match m with
	| Roll -> print_string [] "The player elected to Roll "
	| Passage loc -> disp_loc loc

(* Displays a description of what the agent rolled. *)
let display_dice_roll i =
	print_endline ("and "^(string_of_int i)^" was rolled.")

(* Prompts the user for his. *)
let string_of_int_tuple (a,b) =
	"(" ^ (string_of_int a) ^ "," ^ (string_of_int b) ^")"

let prompt_movement move_ops acc_room =
	let intro = "After rolling, these are the possble options for moving:\n" in
	let helper acc (_, (str, b)) =
		let comb =
			let is_acc = if str = acc_room then " (Accusation Room)" else "" in
			if b then "Enter "^str^is_acc^";\n"
			else "Head towards "^str^is_acc^";\n" in
		acc^comb in
	let str_list = List.fold_left helper "" move_ops in
	print_string [Bold] intro;
	print_string [blue] str_list;
	print_string [green] "\n>>> ";
	read_line ()

(* Displays the movement the agen took on his turn *)
let display_movement (str, b) =
	let intro = "The player " in
	let str =
		if b then "entered the "^str^".\n"
		else "headed towards the "^str^".\n" in
	print_string [] (intro^str)

(* Prompts the user for a guess.
 * Takes in the current location (must be a room) and
 * a bool which says whether or not it is the final accusation.
 * returns a string of the user's response. *)
let prompt_guess loc b =
	match loc.info with
	| Room_Rect (s,i) ->
		(if not b then
			print_string [Bold] ("You are in the "^s^". What is your guess?\n")
		else
			print_string [Bold]
				"You are in the accusation room. What is your final accusation?\n");
		print_string [green] "\n>>> ";
		read_line ()
	| _ ->
		print_string [red] "This is not a room. You can't make a guess here. ";
		""

(* Displays a guess (by the user or AI). *)
let display_guess g =
	match g with
	| (Suspect s1, Weapon s2, Room s3) ->
		let () = print_string [] "\nThe player thinks: \n    " in
		let () = print_string [green] s1 in
		let () = print_string [] " is the suspect, \n    " in
		let () = print_string [yellow] s2 in
		let () = print_string [] " is the weapon, and \n    " in
		let () = print_string [blue] s3 in
		print_string [] " is the room.\n"
	| (_,_,_) -> failwith "A guess has to follow the order Suspect * Weapon * Room"

(* Prompts the user for a card to show.
 * Can be any card from the provided hand, and must be in the guess.
 * Cannot be none, only call when a card they have a card in the guess. *)
let showable hand (s, w, r) =
  let p c = (s = c || w = c || r = c) in
  List.filter p hand

let rec prompt_answer hand guess =
	let cards_showable = showable hand guess in
	let intro = "\nYou can show a card, which one will you show? " in
	let f acc c =
		match c with
		| Suspect s -> acc^s^"; \n"
		| Weapon s -> acc^s^"; \n"
		| Room s -> acc^s^"; \n" in
	let str = List.fold_left f "" cards_showable in
	print_string [] intro;
	print_string [blue] str;
	print_string [green] "\n>>> ";
	read_line ()

(* Displays the card shown to the human agent and by whom.
 * If None, no card could be shown. If false, the user is not shown the
 * details of the card. *)
let display_answer card_opt str b =
	let () = print_string [] "\n" in
	let print_card s =
		print_string [magenta] str;
		print_string [] " shows you ";
		print_string [yellow] s;
		print_string [] " from their hand.\n" in
	if b then
		match card_opt with
		| Some (Suspect s) -> print_card s;
		| Some (Weapon s) -> print_card s;
		| Some (Room s) -> print_card s;
		| None -> print_string [] "No one has a card to show. "
	else
		match card_opt with
		| Some c ->
			print_string [magenta] str;
			print_string [] " showed a card from their hand.\n";
		| None -> print_string [] "No one has a card to show. "

(* Displays end game victory text. *)
let display_victory pl_name =
	print_string [yellow] "\nCongratulations!!!\n";
	print_string [magenta] pl_name;
	print_string [] " just won the game!\n\n"

let display_message s = print_endline s