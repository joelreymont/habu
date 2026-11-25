\ One callback declaration's native C entry while its accessor compiles.
require src/lib/prelude.f
require src/lib/error.f

package NCBSTUB
private
variable ARMED
variable SLOT
variable THUNK
variable ENTRY

public

: SET ( n n -- ) {: k:n target:n :}
   ARMED @ if E-COMP-STATE throw then
   k 0 < if E-COMP-STATE throw then
   k SLOT !  target THUNK !  0 ENTRY !  1 ARMED ! ;

: CLEAR ( -- )
   0 ARMED !  0 SLOT !  0 THUNK !  0 ENTRY ! ;

: ARMED? ( -- bool ) ARMED @ 0<> ;
: SLOT@ ( -- n ) ARMED? 0= if E-COMP-STATE throw then SLOT @ ;
: THUNK@ ( -- n ) ARMED? 0= if E-COMP-STATE throw then THUNK @ ;
: ENTRY! ( n -- ) ARMED? 0= if E-COMP-STATE throw then ENTRY ! ;
: ENTRY@ ( -- n ) ARMED? 0= if E-COMP-STATE throw then ENTRY @ ;

;package
