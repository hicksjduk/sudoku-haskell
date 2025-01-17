# A Haskell Sudoku solver

This repo contains a simple solver for Sudoku and Killer Sudoku puzzles, written in Haskell. 
It is constructed using the `stack` tool.

To run the solver, with the base directory of the repo as the current directory type the command 
`stack run sudoku`. This will solve the puzzle that is defined using the name `puzzle`, which
is [the one which was 
claimed in 2012 to be the hardest Sudoku ever devised](https://abcnews.go.com/blogs/headlines/2012/06/can-you-solve-the-hardest-ever-sudoku). On my
computer, it takes about a third of a second to solve
this puzzle.

Alternatively, the following options can be added to the command above to solve other puzzles:

* `k` for the Killer Sudoku puzzle defined using the name `killerPuzzle`. Note that this might take
a while (just under a minute on my computer), but it will complete.
* `k2` for the Killer Sudoku puzzle defined using the name `killerPuzzle2`. This is a much easier
puzzle and takes about two seconds to solve.
* `e` for an empty Sudoku puzzle (one that is not seeded with any numbers).

## Puzzle types

### Sudoku

A Sudoku puzzle consists of a square grid, which is further divided into boxes such that each square
is in one box. There are the same number of boxes as there are rows, and
each box contains the same number of squares as a row.
In a valid solution, every possible value appears exactly once in each row, each column and each box. A puzzle is
seeded with a number of values.

Within the solver, a Sudoku puzzle is represented by a list of lists of `Int`, 
which contains the grid of values that
must be filled in. Each empty square in the grid contains the value 0; each non-empty square contains
the relevant value.

The input puzzle is validated to ensure that:

* it is a 9x9 grid (a list of nine lists, each of which contains nine `Int`s).
* every square is either empty (represented by the value 0) or contains one
of the values 1 to 9 inclusive.
* no row, column or box contains duplicate values in its non-empty
squares.

### Killer Sudoku

A Killer Sudoku puzzle is like a Sudoku puzzle, but the information that is used to solve
the puzzle is supplied not by seeding the grid with values, but by dividing it into regions.
A region is a collection of
up to nine squares whose values in the solution are all different, and add up
to the total associated with the region.
A region is normally denoted in a printed puzzle by a dotted outline
enclosing its squares, and a number (its total) above the square in the region that is closest to
the top left-hand corner of the puzzle.

In the input representation of a Killer Sudoku puzzle, each row of the grid is represented by a tuple containing
a `String` and a list of `Int`. In the string, each square in the row is represented by a character which uniquely
identifies the region to which the square belongs. The list (which may be empty) contains the totals of the regions 
whose first squares are in the row, in left-to-right order of those first squares.

The regions of a puzzle are validated to ensure that:

* the size of each region (the number of squares it contains) is not less than 1 and not more than 9.
* the total of each region is not less than the minimum, and not more than the maximum, total for a region of the
specified size.
* the totals of all the regions add up to 405, which is the only possible sum of all the values in the grid.

## Data structures

The basic unit of information that is used to solve a puzzle is the `Dimension`. A dimension is a set of squares
which all have to contain different values in the solution; each row, column, box and (if appropriate) region is a
dimension. At any given stage in the solution of a puzzle, a dimension contains a list of its empty squares, and
a list of the combinations of values that can appear in those empty squares.

A `Puzzle` contains a list of lists of `Dimensions`, with the dimensions being grouped by type 
(row, column, box, region). The grouping by type means (in particular) that it is much more efficient 
to search for the dimensions that contain a particular square, since we know that a square only appears
in one dimension of each type. Each list of dimensions of a particular type is sorted in ascending order
of (a) the number of empty squares and (b) the number of possible values; and, when determining which
dimension to take an empty square from, the heads of the lists are sorted in the same order, so that
the dimension that is processed is the one with the fewest possible combinations of empty squares and
possible values.

## Code structure

The basic function for solving a puzzle of either type is `sudoku`. It returns an error message if 
the puzzle is invalid, otherwise it invokes `solve` to solve
the puzzle and the first solution found (if any) is returned. If no solution can be found, an error message is
returned.

The `solve` function uses a straightforward algorithm which tries every possibility to solve the puzzle.
For any given puzzle:

* Find an empty square. If there is no empty square, the puzzle is solved.
* Determine which values can go into the empty square, A value can only go into the square if 
it is one of the possible values in every dimension that contains the square.
* For each such value, make a copy of the puzzle where the 
0 at the relevant position in the source grid is replaced with that value, and solve the revised 
puzzle recursively.

The `solve` function returns a list containing
all the possible solutions for the input puzzle; each solution is a grid where 
every square contains a non-zero value that satisfies
the constraints of the puzzle.
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
