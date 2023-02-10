# A Haskell Sudoku solver

This repo contains a simple solver for Sudoku puzzles, written in Haskell.

To run the solver, load the file `sudoku.hs` into `ghci`, and enter the
command `sudoku puzzle`. The puzzle in question is the one which was 
claimed a few years ago to be the hardest Sudoku puzzle ever devised.

The puzzle is assumed to be a 9x9 grid (a list of nine lists, each of which contains nine `Int`s) where an empty
square is represented by 0 and a non-empty one by the number (in the range
1 to 9 inclusive) it contains.

The `sudoku` function returns an error message if no solution can be found, otherwise the first solution found is returned. For an example of an unsolvable puzzle, try entering `sudoku $ setValueAt (8,8) 3 puzzle` - the only possible solution 
has 2 in the bottom right-hand corner, so setting that square to any other non-zero value makes the puzzle unsolvable.

The program uses a straightforward, brute-force algorithm to solve the puzzle.
For any given grid:

* Find an empty square.
* If there is no empty square, the grid is solved.
* Determine which values can go into the empty square (because they do
not appear anywhere in the same row, column or box). 
* For each such value, replace the
0 at the relevant position in the source grid with that value, and solve the revised grid recursively.

The `solve` function returns a list of solutions, each of which is a grid where 
all 81 values are non-zero. The `sudoku` function takes the head of the
returned list; thus evaluation terminates as soon as a solution is found.