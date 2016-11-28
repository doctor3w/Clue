test:
	ocamlbuild -pkgs oUnit,yojson,str,ANSITerminal,graphics -tag thread main_test.byte && ./main_test.byte

play:
	ocamlbuild -pkgs oUnit,yojson,str,ANSITerminal,graphics -tag thread main.byte && ./main.byte "tests/game.json"

choose:
	ocamlbuild -pkgs oUnit,yojson,str,ANSITerminal,graphics -tag thread main.byte && ./main.byte

thread:
	ocamlbuild -pkgs oUnit,yojson,str,ANSITerminal,graphics -tag thread main.byte && ./main.byte

check:
	bash checkenv.sh && bash checktypes.sh

clean:
	ocamlbuild -clean
	rm -f checktypes.ml
