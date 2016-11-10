test:
	ocamlbuild -pkgs oUnit,yojson,str,ANSITerminal main_test.byte && ./main_test.byte

play:
	ocamlbuild -pkgs oUnit,yojson,str,ANSITerminal main.byte && ./main.byte "test/game.json"

check:
	bash checkenv.sh && bash checktypes.sh

clean:
	ocamlbuild -clean
	rm -f checktypes.ml
