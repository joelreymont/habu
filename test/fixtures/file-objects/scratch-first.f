\ scratch-first.f - a package with a marked scratch cell, a SCRATCH-VARIABLE and
\ a SCRATCH-BUFFER:, which scratch-second.f's top level stores into and
\ scratch-main.f reads at its top level and in its program.

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
