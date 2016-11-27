test:
	ocamlbuild -pkgs oUnit,yojson,str,ANSITerminal,graphics main_test.byte && ./main_test.byte

play:
	ocamlbuild -pkgs oUnit,yojson,str,ANSITerminal,graphics main.byte && ./main.byte "tests/game.json"

choose:
	ocamlbuild -pkgs oUnit,yojson,str,ANSITerminal,graphics main.byte && ./main.byte

check:
	bash checkenv.sh && bash checktypes.sh

clean:
	ocamlbuild -clean
	rm -f checktypes.ml
