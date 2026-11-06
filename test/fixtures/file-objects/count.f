\ count.f - a counter and a greeting the other fixture files use.

package FILE-OBJECTS-COUNT
public
variable TALLY
: BUMP ( -- ) TALLY @ 1 + TALLY ! ;
: HI ( -- ) ." Hi" cr ;
;package

\ A package with no private wordlist: reopen.f gives it one.
: FILE-OBJECTS-LATE:ONE ( -- n ) 1 ;
