package FILE-OBJECTS-SCRATCH-FIRST
private
variable SLOT   SLOT scratch-cell-mark
SCRATCH-VARIABLE VAR
8 SCRATCH-BUFFER: BUF
public
: STORE ( n -- ) dup SLOT ! dup VAR ! BUF c! ;
: FETCH ( -- n ) SLOT @ ;
: VAR-FETCH ( -- n ) VAR @ ;
: BUF-FETCH ( -- n ) BUF c@ ;
;package
