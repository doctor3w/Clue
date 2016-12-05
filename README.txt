# MS2 README

We have included with our project a Makefile in order to make compiling and running our game very simple. The packages you will need installed are listed as below: 

- oUnit
- Yojson
- Str
- Graphics

I believe Str and Graphics come preinstalled, but this is not definite. Yojson and oUnit can be installed with opam. Graphics should come installed. 

On Macs, X11 must be installed. That can be downloaded here: https://www.xquartz.org

To run our program, you can call make with a number of different arguments: 

```
make : runs our test unit suite.
make test : runs our test unit suite. 
make play : runs the game file named game.json in the directory tests. 
make play-gui : runs the game file named game.json in the directory tests with a graphical user interface rather than the CLI. 
make choose : runs the game, allowing you to later type in the file you wish to load. 
make choose-gui : runs the game, allowing you to later type in the file you wish to load with a GUI. 

make clean : cleans the build. 
```
Optionally, our game can also be run directly from the byte file. To complile run:
`ocamlbuild -pkgs oUnit,yojson,str,graphics -tag thread main.byte`
Then with the `main.byte` file you can run it with this usage:

```
USAGE: 

./main.byte [OPTIONS] [FILE_NAME]

These can be in any order
OPTIONS:
	-gui : Uses a GUI instead of CLI
FILE_NAME: 
	optional filename to load on run
```