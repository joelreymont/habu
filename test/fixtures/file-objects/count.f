\ count.f - a counter and a greeting the other fixture files use, LATER, which
\ reopen.f stores an xt into, and MARKED, the cell reopen.f marks.

package FILE-OBJECTS-COUNT
public
variable TALLY
variable LATER
variable MARKED
: BUMP ( -- ) TALLY @ 1 + TALLY ! ;
: HI ( -- ) ." Hi" cr ;
;package

\ A package with no private wordlist: reopen.f gives it one.
: FILE-OBJECTS-LATE:ONE ( -- n ) 1 ;

\ Ends on a cell, so shape.f's data starts where count.f's ends.
align
