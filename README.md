# A Haskell Sudoku solver

This repo contains a simple solver for Sudoku puzzles, written in Haskell.

To run the solver, load the file `sudoku.hs` into `ghci`, and enter the
command `sudoku puzzle`. The puzzle in question is [the one which was 
claimed in 2012 to be the hardest Sudoku ever devised](https://abcnews.go.com/blogs/headlines/2012/06/can-you-solve-the-hardest-ever-sudoku).

Alternatively, the `sudoku` function can be invoked on any puzzle, which is represented by a
list of `Int`s.
The input puzzle is validated to ensure that:

* it is a 9x9 grid (a list of nine lists, each of which contains nine `Int`s).
* every square is either empty (represented by the value 0) or contains one
of the values 1 to 9 inclusive.
* no row, column or box contains duplicate values in its non-empty
squares.

The `sudoku` function returns an error message if 
the puzzle is invalid or no solution can be found, otherwise the first solution found is returned. 
To see examples of the various error conditions and their associated messages, try entering:

* `sudoku $ tail puzzle` for the case where the grid does not have 9 rows.
* `sudoku $ map nub puzzle` for the case where not all rows have 9 columns.
* `sudoku $ setValueAt (5,4) 41 puzzle` for the case where a cell has an invalid value.
* `sudoku $ setValueAt (8,0) 4 puzzle` for the case where a row contains duplicate values.
* `sudoku $ setValueAt (3,8) 8 puzzle` for the case where a column contains duplicate values.
* `sudoku $ setValueAt (8,0) 1 puzzle` for the case where a box contains duplicate values.
* `sudoku $ setValueAt (8,8) 3 puzzle` for the case where the puzzle is unsolvable - the only possible solution 
has 2 in the bottom right-hand corner, so setting that square to 3, 5 or 7 (the values other
than 2 which are not duplicated in the same row, column or box) makes the puzzle unsolvable though still apparently valid.

The program uses a straightforward algorithm which tries every possibility to solve the puzzle.
For any given grid:

* Find an empty square. If there is no empty square, the grid is solved.
* Determine which values can go into the empty square (because they do
not appear anywhere in the same row, column or box). 
* For each such value, replace the
0 at the relevant position in the source grid with that value, and solve the revised grid recursively.

The `solve` function returns a list containing
all the possible solutions for the input grid; each solution is a grid where 
every square contains a non-zero value.
The `sudoku` function takes the head of the
returned list; thus evaluation terminates as soon as a solution is found.

Note that the structure of the puzzle is not hard-coded throughout the program, but is
entirely driven by the assignment of two fields at the top of the file:

* `permittedValues`, which is a list containing the values that can validly be
inserted into a square. The size of this also determines the grid size, which is the
number of squares in each row, column and box.
* `emptySquare`, which is the value that represents an empty square.

By default, these are set respectively to the values 1 to 9 inclusive and 0; but
different values could be specified to deal with differently-sized puzzles. 
The only constraints on this (which are validated, and if violated cause
an error)
are that `permittedValues` should not be empty, or contain duplicate values or the value of `emptySquare`.

The sizes of the boxes within the puzzle are calculated based on the convention that:

* Each box is as nearly square as possible: if the grid size is not a square number, the difference between the number of rows and the number of columns is as small as possible.
* A box that is not square has more columns than rows.

This implies that in each box, the number of rows is the largest divisor of the grid size that is not greater than its square root, and the number of columns is the grid size divided by the number of rows. It also implies that the grid size should ideally not be a prime number, because that would mean that each box is contiguous with a single row.
