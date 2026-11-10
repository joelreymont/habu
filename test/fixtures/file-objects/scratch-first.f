\ scratch-first.f - a package with one scratch cell, which scratch-second.f's
\ top level stores into and scratch-main.f's top level reads.

package FILE-OBJECTS-SCRATCH-FIRST
private
variable SLOT   SLOT scratch-cell-mark
public
: STORE ( n -- ) SLOT ! ;
: FETCH ( -- n ) SLOT @ ;
;package
