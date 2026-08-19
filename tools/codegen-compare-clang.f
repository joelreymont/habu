\ codegen-compare-clang.f - the clang reference column. One concern: measuring
\ one C twin as one row of the comparison's store.
\
\ WHAT THE THIRD COLUMN IS FOR. The other two columns say which of two habu code
\ generators emits less and runs faster, which is a comparison with itself. This
\ one says how far either is from what a production optimising compiler makes of
\ the same program on the same machine. That number is the parity target the
\ optimisation work is aimed at, and the report's largest gaps are its priority
\ list.
\
\ THE DATA THE TWINS OWN. Three corpora measure a side effect over storage the
\ corpus itself holds. The twins cannot share that storage - they are a
\ different program, not a second compilation of the same one - so they carry
\ their own, filled from the same constants, and OPEN runs each corpus's C setup
\ function once before anything is measured. What makes that honest is not the
\ memory but the answers: the recorded outputs of a reference row are compared
\ against the same pinned values the engine's row is.

require lib/errors.f
require lib/prelude.f
require lib/string.f
require tools/codegen-compare-cc.f
require tools/codegen-compare-macho.f
require tools/codegen-compare-cabi.f

package CODEGEN-CLANG

private

variable OPENED

\ Call a twin that takes nothing and answers nothing worth keeping: the four
\ setup functions, which fill the twins' own copies of the pinned data.
: SETUP-CALL ( ptr u8 n -- )
   CODEGEN-CABI:FN CODEGEN-CABI:I0 drop ;

: OPEN ( -- )
   CODEGEN-MACHO:LOAD
   CODEGEN-CABI:OPEN
   s" hc2_setup" SETUP-CALL
   s" hc3_setup" SETUP-CALL
   s" hc4_setup" SETUP-CALL
   -1 OPENED ! ;

public

\ Is there a reference column in this process? Answers the same thing every
\ time it is asked, and builds the reference the first time.
: PRESENT? ( -- bool )
   OPENED @ 0<> if 0 0= exit then
   CODEGEN-CC:READY? 0= if 0 0= 0= exit then
   OPEN
   0 0= ;

\ Why there is no reference column, when there is none.
: ABSENT-WHY$ ( -- ptr u8 n )
   CODEGEN-CC:ABSENT-WHY$ ;

: FLAGS$ ( -- ptr u8 n )
   CODEGEN-CC:FLAGS$ ;

\ How many bytes clang emitted for the whole reference object, and how many of
\ those are literal pools that belong to no one twin.
: TEXT-BYTES ( -- n )
   CODEGEN-MACHO:TEXT-BYTES ;

: POOL-BYTES ( -- n )
   CODEGEN-MACHO:POOL-BYTES ;

;package
