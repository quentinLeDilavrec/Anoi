# Hanoi

The file `hanoi.ml` contains the basic code to solve the Tower of Hanoi problem.
To launch the program, make sure you installed the `graphics` module then simply type `ocaml hanoi.ml`.
You should see something like
```
...
| move a disc from peg 0 to peg 2
| move a disc from peg 1 to peg 2
| move a disc from peg 0 to peg 1
| move a disc from peg 2 to peg 0
| move a disc from peg 2 to peg 1
| move a disc from peg 0 to peg 1
| move a disc from peg 2 to peg 0
| move a disc from peg 1 to peg 2
| move a disc from peg 1 to peg 0
| move a disc from peg 2 to peg 0
| move a disc from peg 2 to peg 1
| move a disc from peg 0 to peg 1
| move a disc from peg 0 to peg 2
| move a disc from peg 1 to peg 2
| move a disc from peg 0 to peg 1
| move a disc from peg 2 to peg 0
| move a disc from peg 2 to peg 1
| move a disc from peg 0 to peg 1
...
Total number of moves: 255
```
The file `hanoi_extended.ml` contains some extensions, mainly a graphical visualization of the successive movements needed to solve the problem.
As above, make sure you installed the `graphics` module then type `ocaml hanoi_extended.ml` to launch the program.
