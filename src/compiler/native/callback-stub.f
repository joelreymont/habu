\ One callback declaration's native C entry, carried through its host emission.
\ A native compiler arms this before it seals the accessor's host emission
\ (NCOMP after its shadow work, T0) and clears it when that emission retires.
require src/lib/prelude.f
require src/lib/errors.f

package NCBSTUB
private
variable ARMED
variable SLOT
variable THUNK
variable ENTRY

public

: SET ( n n -- ) {: k:n target:n :}
   ARMED @ if E-NCOMP-STATE throw then
   k 0 < if E-NCOMP-STATE throw then
   k SLOT !  target THUNK !  0 ENTRY !  1 ARMED ! ;

: CLEAR ( -- )
   0 ARMED !  0 SLOT !  0 THUNK !  0 ENTRY ! ;

: ARMED? ( -- bool ) ARMED @ 0<> ;
: SLOT@ ( -- n ) ARMED? 0= if E-NCOMP-STATE throw then SLOT @ ;
: THUNK@ ( -- n ) ARMED? 0= if E-NCOMP-STATE throw then THUNK @ ;
: ENTRY! ( n -- ) ARMED? 0= if E-NCOMP-STATE throw then ENTRY ! ;
: ENTRY@ ( -- n ) ARMED? 0= if E-NCOMP-STATE throw then ENTRY @ ;

;package
