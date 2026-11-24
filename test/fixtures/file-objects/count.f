\ count.f - a counter and a greeting the other fixture files use, LATER, which
\ reopen.f stores an xt into, MARKED, the cell reopen.f marks, and SPARE, which
\ mark.f stores an xt into.

package FILE-OBJECTS-COUNT
public
variable TALLY
variable LATER
variable MARKED
variable SPARE
: BUMP ( -- ) TALLY @ 1 + TALLY ! ;
: HI ( -- ) ." Hi" cr ;
;package

\ A package with no private wordlist: reopen.f gives it one.
: FILE-OBJECTS-LATE:ONE ( -- n ) 1 ;

package FILE-OBJECTS-COUNT
private
\ Keeps the xt, so the program reaches FILE-OBJECTS-LATE:ONE.
variable KEEP-ONE  ' FILE-OBJECTS-LATE:ONE KEEP-ONE xt!
;package

\ Ends on a cell, so shape.f's data starts where count.f's ends.
align
