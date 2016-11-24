open Data
open ANSITerminal

(* Displays the provided error message stored in [e] in color red. *)
let display_error (e:string): unit = print_string [red] (e ^ "\n")

(* Displays a description of who's turn it is.
   The format of print statment is "This is [player]'s turn",
   where the information about the [player] is stored in [pub] *)
let display_turn (pub:public): unit =
	let () = print_string [black] "This is " in
	let () = print_string [cyan] pub.curr_player in
	print_endline "'s turn"

(* Prompts the user for a file so that it can be imported into the Model *)
let prompt_filename () : string =
	"Please enter the name of the game file you want to load. "

(* Prompts the user for whether he rolls dice or not.
	[prompt_move] takes [l], which is a move list and prints out all the
	possible moves the player could take. All of the option are in color magenta *)
let rec prompt_move l =
	let intro = "Moves you could make: " in
	let rec moves_can_be_done l =
		(match l with
		 | [] -> ""
		 | h::t ->
			 (match h with
			 | Roll -> "Roll a dice; " ^ (moves_can_be_done t)
			 | Passage loc ->
 				match loc.info with
 				| Room_Rect (s, i) ->
				  	"Take the Passage in " ^ s ^ "; "^(moves_can_be_done t)
 				| _ -> (moves_can_be_done t))) in
	let format = intro ^ (moves_can_be_done l) in
	let len = String.length format in
	(String.sub format 0 (len-2))^"."

(* Displays a description of whether the agent elected to Roll or Passage. *)
let display_move (m:move) : unit =
	match m with
	| Roll -> print_endline "The agent elected to Roll"
	| Passage loc ->
		(match loc.info with
		| Room_Rect (s,i) ->
			print_endline ("The agent elected to
						    take the Passage in " ^ s)
		| _ -> failwith "A space is not displayed here")

(* Displays a description of what the agent rolled. *)
let display_dice_roll i =
	let print_info =
		"A dice was just rolled and it turned to be " ^ (string_of_int i) in
	print_endline print_info

(* Prompts the user for his. *)
let string_of_int_tuple (a,b) =
	"(" ^ (string_of_int a) ^ "," ^ (string_of_int b) ^")"

let prompt_movement l =
	let intro = "All the possible movements you could make: " in
	let helper (str,loc) =
		(match loc.info with
		| Room_Rect (s,i) ->
			(str^" the "^ s^"; ")
		| Space (a,b) ->
			str^" the space at "^(string_of_int_tuple (a,b))^"; ") in
	let str_list = List.map helper l in
	let len = String.length (intro ^ (List.fold_right (^) str_list "")) in
	let format =
		String.sub (intro ^ (List.fold_right (^) str_list "")) 0 (len-2) in
	format^"."

(* Displays the movement the agen took on his turn *)
let display_movement (str, loc) =
	match loc.info with
	| Room_Rect (s,i) ->
		(print_string [black] (str^" the ");
		 print_string [cyan] (s^"\n"))
	| Space (a,b) ->
		(print_string [black] (str^" the space at ");
		 print_string [cyan] ((string_of_int_tuple (a,b))^"\n"))

(* Prompts the user for a guess.
 * Takes in the current location (must be a room) and
 * a bool which says whether or not it is the final accusation.
 * returns a string of the user's response. *)
let prompt_guess loc b =
	match loc.info with
	| Room_Rect (s,i) ->
		(match b with
			| true ->
				("Would you like to make an accusation in the " ^ s ^ "?")
			| false ->
			    ("Would you like to make a guess in the " ^ s ^ "?"))
	| _ -> "This is not a room. You can't make a guess here. "

(* Displays a guess (by the user or AI). *)
let display_guess g =
	match g with
	| (Suspect s1, Weapon s2, Room s3) ->
		let () = print_string [black] "The player assumes " in
		let () = print_string [green] s1 in
		let () = print_string [black] " is the suspect, " in
		let () = print_string [yellow] s2 in
		let () = print_string [black] " is the weapon, and " in
		let () = print_string [blue] s3 in
		print_endline "is the room"
	| (_,_,_) -> failwith "A guess has to follow the order Suspect * Weapon * Room"

(* Prompts the user for a card to show.
 * Can be any card from the provided hand, and must be in the guess.
 * Can be none if there is no card to show. *)
let triple_fst (a,b,c) = a
let triple_snd (a,b,c) = b
let triple_thd (a,b,c) = c

let prompt_answer_helper c g =
	if c = triple_fst g then true
	else if c = triple_snd g then true
	else if c = triple_thd g then true
	else false

let rec prompt_answer hand guess =
	print_string [black] "Cards you could show: ";
	let rec cards_can_be_shown h =
		(match h with
			| [] -> (print_string [black] "")
			| h::t ->
				if prompt_answer_helper h guess
				then match h with
		 			| Suspect s ->
					   (print_string [yellow] "Suspect ";
					    print_string [yellow] s;
					    print_string [black] " ";
					    cards_can_be_shown t)
		 		 	| Weapon s ->
	 				   (print_string [green] "Weapon ";
					    print_string [green] s;
					    print_string [black] " ";
					    cards_can_be_shown t)
		 		 	| Room s ->
	 				   (print_string [blue] "Room ";
					    print_string [blue] s;
					    print_string [black] " ";
					    cards_can_be_shown t)
			 	else cards_can_be_shown t) in
	cards_can_be_shown hand;
	print_endline ""

(* Displays the card shown to the human agent and by whom.
 * If None, the user is told just told who showed a card, but not the details
 * of the card. *)
let display_answer card_opt str b =
	match b with
	| true ->
		(match card_opt with
		| Some c ->
			(match c with
				| Suspect s ->
					(print_string [magenta] str;
				 	print_string [black] " has ";
				 	print_string [yellow] ("Suspect " ^ s);
				 	print_endline " in hand")
				| Weapon s ->
					(print_string [magenta] str;
				 	print_string [black] " has ";
				 	print_string [green] ("Weapon " ^ s);
				 	print_endline " in hand")
				| Room s ->
					(print_string [magenta] str;
				 	print_string [black] " has ";
				 	print_string [blue] ("Room " ^ s);
				 	print_endline " in hand"))
		| None ->
		    (print_string [magenta] str;
		    print_endline " has nothing to show you"))
	| _ ->
		(match card_opt with
		| Some c ->
			(print_string [magenta] str;
			 print_endline " showed a card")
		| None ->
			(print_string [magenta] str;
		    print_endline " has nothing to show you"))

(* Displays end game victory text. *)
let display_victory pl_name =
	print_endline "Congratulations!!!";
	print_string [magenta] pl_name;
	print_endline " just won the game! The game is over."

(* Displays end game victory text. *)
let display_message s = print_endline s
