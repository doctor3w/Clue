test:
	ocamlbuild -pkgs oUnit,yojson,str,graphics -tag thread main_test.byte && ./main_test.byte

play:
	ocamlbuild -pkgs oUnit,yojson,str,graphics -tag thread main.byte && ./main.byte "tests/game.json"

play-gui:
	ocamlbuild -pkgs oUnit,yojson,str,graphics -tag thread main.byte && ./main.byte -gui "tests/game.json"

choose:
	ocamlbuild -pkgs oUnit,yojson,str,graphics -tag thread main.byte && ./main.byte

choose-gui:
	ocamlbuild -pkgs oUnit,yojson,str,graphics -tag thread main.byte && ./main.byte -gui

check:
	bash checkenv.sh && bash checktypes.sh

clean:
	ocamlbuild -clean
	rm -f checktypes.ml
