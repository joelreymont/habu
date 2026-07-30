\ insn-refusal.f - one refused encoding, run in its own engine.
\
\ The refusals in `src/arch/arm64/asm.f` all call `die`: they write a diagnostic
\ and end the process with code 72. That refusal cannot be observed from inside
\ the engine that triggered it, so `test/compiler/insn-cases.f` runs this file
\ in a child engine, one row at a time, and reads the child's exit status.
\
\ The row to run comes from the environment, and it is an index into the very
\ same frozen tables the rest of the gate reads, so a row cannot be refused here
\ and encoded there. Which environment variable is set says which table:
\
\   HABU_INSN_RESERVED_ROW - a reserved-register row. The child emits it
\     through the real mnemonic. If `XREG?` refuses, the child never returns
\     and the parent sees 72; if it does not, the child exits 0 and the parent's
\     row - which records the word instead of a code - is the one that has to
\     hold.
\   HABU_INSN_RANGE_ROW - an operand outside the field it goes in, or one a
\     scale division would round down.
\   HABU_INSN_LIMM_ROW - a mask `>LIMM` cannot pack.
\
\ Running with neither set is a mistake in the caller, not a refusal, so it ends
\ with its own code rather than pretending the row passed.
\
\ Run by: `test/compiler/insn-cases.f`.

require lib/prelude.f
require lib/errors.f
require lib/string.f
require lib/adt/option.f
require test/compiler/insn-schema.f
require test/compiler/insn-cases.f

package COMPILER-INSN-REFUSAL
using COMPILER-INSN-PROOF
private

64 constant NO-ROW-RC

\ An unset or unparseable variable answers -1, which selects no row rather than
\ row zero.
: ROW# ( ptr u8 n -- n )
   GETENV STR>NUMBER? MATCH option
     none OF -1 ENDOF
     some OF ENDOF
   ;MATCH ;

: RESERVED-ROW ( n -- ) {: r:n :}
   r RES-FORM@ r RES-A@ r RES-B@ r RES-C@ r RES-MASK@ COMPILER-INSN-CASES:EMIT-ROW drop ;

: RANGE-ROW ( n -- ) {: r:n :}
   r OOR-FORM@ r OOR-A@ r OOR-B@ r OOR-C@ 0 COMPILER-INSN-CASES:EMIT-ROW drop ;

: LIMM-ROW ( n -- ) {: r:n :}
   r LIMM-BAD-MASK >LIMM drop ;

public

: RUN ( -- )
   s" HABU_INSN_RESERVED_ROW" ROW# dup 0 >= if RESERVED-ROW exit then drop
   s" HABU_INSN_RANGE_ROW" ROW# dup 0 >= if RANGE-ROW exit then drop
   s" HABU_INSN_LIMM_ROW" ROW# dup 0 >= if LIMM-ROW exit then drop
   s" insn-refusal: no row selected" NO-ROW-RC die ;

;using
;package

COMPILER-INSN-REFUSAL:RUN
