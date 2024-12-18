# A Haskell Sudoku solver

This repo contains a simple solver for Sudoku puzzles, written in Haskell. It is constructed using the
`stack` tool.

To run the solver, with the base directory of the repo as the current directory type the command 
`stack run sudoku-exe`. This will solve the puzzle that is defined using the name `puzzle`, which
is [the one which was 
claimed in 2012 to be the hardest Sudoku ever devised](https://abcnews.go.com/blogs/headlines/2012/06/can-you-solve-the-hardest-ever-sudoku).

Alternatively, the following options can be entered to solve other puzzles:

* `k` for the Killer Sudoku puzzle defined using the name `killerPuzzle`. Note that this might take
a while (about four minutes on my computer), but it will solve the puzzle.
* `k2` for the Killer Sudoku puzzle defined using the name `killerPuzzle2`. This is a much easier
puzzle and takes about a second to solve.
* `e` for an empty Sudoku puzzle (one that is not seeded with any numbers).

## Puzzle types

### Sudoku

A Sudoku puzzle is represented by a list of lists of `Int`, which contains the grid of values that
must be filled in. Each empty square in the grid contains the value 0; each non-empty square contains
the relevant value.

The input puzzle is validated to ensure that:

* it is a 9x9 grid (a list of nine lists, each of which contains nine `Int`s).
* every square is either empty (represented by the value 0) or contains one
of the values 1 to 9 inclusive.
* no row, column or box contains duplicate values in its non-empty
squares.

### Killer Sudoku

A Killer Sudoku puzzle contains a number regions.which are normally denoted in print by a dotted outline.
A region is a collection of
up to nine (normally contiguous) squares whose values in the solution are all different, and add up
to the total associated with the region.

The input representation of a Killer Sudoku puzzle uses the following structures to define the regions:

* A grid where each square contains a character that represents the region of which the square is part.
* A lookup table where the key is the character representing each region, and the value is the required
total of the values in the region.

The regions of a puzzle are validated to ensure that:

* the total of each region is not less than the minimum, and not more than the maximum, total for a region of the
specified size.
* the totals of all the regions add up to 405, which is the only possible sum of all the values in the grid.

## Code structure

The basic function for solving a puzzle of either type is `sudoku`. It returns an error message if 
the puzzle is invalid or no solution can be found, otherwise the first solution found is returned.

The program uses a straightforward algorithm which tries every possibility to solve the puzzle.
For any given puzzle:

* Find an empty square. If there is no empty square, the current grid is a solution.
* Determine which values can go into the empty square, because they do
not appear anywhere in the same row, column or box, and in the case of a Killer Sudoku they also do not
appear in the same region and are able to combine with those values that are already in the region
to make the required total.
* For each such value, make a copy of the puzzle where the 
0 at the relevant position in the source grid is replaced with that value, and solve the revised 
puzzle recursively.

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
