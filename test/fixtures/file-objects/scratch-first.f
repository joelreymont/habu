split-str
package FILE-OBJECTS-SCRATCH-FIRST
private
variable SLOT   SLOT scratch-cell-mark
SCRATCH-VARIABLE VAR
8 SCRATCH-BUFFER: BUF
1 c,  8 SCRATCH-ALLOT      \ a reserve after an odd byte: SCRATCH-ALLOT aligns before it marks
TYPED-VARIABLE TVAR n SCRATCH-ONLY
1 TYPED-BUFFER TBUF n SCRATCH-ONLY
public
: STORE ( n -- ) dup SLOT ! dup VAR ! dup BUF c! dup TVAR ! 0 TBUF ! ;
: FETCH ( -- n ) SLOT @ ;
: VAR-FETCH ( -- n ) VAR @ ;
: BUF-FETCH ( -- n ) BUF c@ ;
: TVAR-FETCH ( -- n ) TVAR @ ;
: TBUF-FETCH ( -- n ) 0 TBUF @ ;
;package
