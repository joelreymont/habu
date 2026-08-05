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
\ HOW A ROW IS MEASURED, AND WHY THE FLOOR IS PER ROW. A twin is reached through
\ an FFI call, which costs several times what a `bl` costs, so a reference row's
\ raw time is mostly marshalling. Each row therefore records a floor of its own:
\ the row's OWN timing body, run again with the twin replaced by an empty C
\ function of the SAME signature. The difference is the emitted code, and the
\ subtraction is exact because the two runs marshal identical arguments through
\ identical stores into identical registers - the only thing that differs is
\ what the callee does with them. tools/codegen-compare-core.f's BODY-PICOS
\ reads that floor for a reference row and the path's calibration row for an
\ engine or chain row, so all three columns' body costs are one kind of number.
\
\ AND WHY THERE IS A CALIBRATION ROW AS WELL. The path still needs one, because
\ every path's rows are normalised against an empty call of that path, and
\ because the harness's oldest discipline is that each column prints the floor
\ it was measured against. The reference column's is the empty zero-argument
\ call - the FFI entry cost with nothing in it - and it is the row named after
\ the corpus's own empty word, which is exactly the program it is the twin of.
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
require tools/codegen-compare-core.f
require tools/codegen-compare-cc.f
require tools/codegen-compare-macho.f
require tools/codegen-compare-cabi.f

package CODEGEN-CLANG

private

variable FN-CELL
variable OPENED

\ Call a twin that takes nothing and answers nothing worth keeping: the four
\ setup functions, which fill the twins' own copies of the pinned data.
: SETUP-CALL ( ptr u8 n -- )
   CODEGEN-CABI:FN CODEGEN-CABI:I0 drop ;

: FN! ( ptr u8 n -- )
   CODEGEN-CABI:FN FN-CELL ! ;

: OPEN ( -- )
   CODEGEN-MACHO:LOAD
   CODEGEN-CABI:OPEN
   s" hc2_setup" SETUP-CALL
   s" hc3_setup" SETUP-CALL
   s" hc4_setup" SETUP-CALL
   -1 OPENED ! ;

public

\ Where the twin being measured starts. A timing body and a vectors body both
\ read it, so the one body serves for the row and for the row's floor: MEASURE
\ points it at the twin, records the row, points it at the empty function of the
\ same signature, and times the same body again.
: FN@ ( -- n )
   FN-CELL @ ;

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

\ One reference row: the corpus word it is the twin of, the C symbol that is the
\ twin, the C symbol that is the empty function of the same signature, a body
\ that calls FN@ once for timing, and a body that calls it on the row's pinned
\ inputs and hands every answer to VECTOR.
\ typed-local-lint: allow-bare-local - timing and vectors are quotation bodies,
\ and a local annotation cannot carry a quotation effect.
: MEASURE ( ptr u8 n ptr u8 n ptr u8 n [ -- ] [ -- ] -- )
   {: a:ptr u:n ta:ptr tu:n fa:ptr fu:n timing vectors :}
   ta tu FN!
   a u  ta tu CODEGEN-MACHO:BYTES  timing vectors CODEGEN-COMPARE:MEASURE-CLANG
   fa fu FN!
   timing CODEGEN-COMPARE:TIME-ONLY CODEGEN-COMPARE:FLOOR! ;

\ The reference column's floor: an empty C function reached through the same
\ foreign call every other reference row is reached through, measured and
\ declared this path's calibration row. It is the twin of the corpus's own empty
\ word, so it carries that word's name and pairs with it in the report.
: CALIBRATE ( -- )
   s" CODEGEN-CORPUS:NOOP" s" hf_i0" s" hf_i0"
   [: FN@ CODEGEN-CABI:I0 drop ;]
   [: ;]
   MEASURE
   CODEGEN-COMPARE:CALIBRATE ;

;package
