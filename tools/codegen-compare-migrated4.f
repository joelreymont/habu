\ codegen-compare-migrated4.f - the fourth corpus's bodies, compiled by the new
\ chain and published as ordinary words. One concern: putting the fourth corpus's
\ new-chain words into the dictionary before anything measures them.
\
\ This is tools/codegen-compare-migrated.f's discipline over the fourth corpus,
\ and everything that file says applies here: the source is handed to the engine,
\ the engine compiles it through the path it compiles every definition through,
\ the tape the chain then recompiles is the one the checker's own reader filled
\ while it certified that body, and the publication seam points the `-N` word's
\ dictionary record at the chain's emission. A body that is not well typed Habu
\ does not compile at all, and nothing here catches: a body the chain refuses is
\ a claim this file made and did not keep.
\
\ NINE OF THE TEN, AND THE ONE THAT IS NOT HERE. PRESSURE-LOOP is absent, and its
\ absence is the finding rather than an omission: the chain refuses it, by name,
\ with E-A64RA-SPILL, and tools/codegen-compare-new4.f declares it a gap naming
\ the capability it waits for. tools/codegen-compare-test.f hands the corpus's own
\ text to the same migration entry this file uses and checks that the refusal is
\ that code and not some other, so "the chain cannot compile it" is a measurement
\ here and not a sentence.
\
\ THE SUBSTITUTIONS, AND THERE IS ONLY THE ONE KIND. Every body carries `-N` on
\ the name it defines, which is the migration's own convention, and the three call
\ rows call `C-ADD1-N`, `C-MUL2-N`, `C-AND7-N` and `C-XOR5-N` where the corpus
\ writes the names without it - the discipline tools/codegen-compare-migrated2.f
\ established when it migrated CELL-FIELD for VEC-COPY-CELLS to call: the new
\ column's word is the new chain's code all the way down, so a row measures a
\ program of the chain's making rather than a chain-compiled shell around the
\ engine's loops. NO CONSTANT IS RESPELLED - the corpus writes its four
\ sixty-four-bit literals in decimal precisely so that this file can hand over the
\ corpus's own text - no operation is changed, no local is renamed and no
\ annotation is added or removed.
\
\ THE REGISTER BUDGETS, AND WHY THE LOOP ONE IS EIGHTEEN AND NOT SIXTEEN. A
\ migration states how many scratch general registers its routine may use, and
\ the third corpus's loops state sixteen. This corpus states EIGHTEEN, which is
\ not a bigger guess but the whole of what the machine has: the pool starts at x0
\ and src/compiler/a64-effect.f refuses x18 in any routine's register set, since
\ the platform reserves it - a nineteenth register is E-A64EFF-GPR, measured. The
\ reason to state the maximum here rather than a comfortable number is
\ PRESSURE-LOOP: a refusal under a budget somebody chose is a fact about the
\ budget, and a refusal under the largest budget the architecture allows is a fact
\ about the compiler. Every other loop row states the same eighteen so that the
\ one refusal is not the only row measured under it.
\
\ Straight-line rows state eight, as the other three corpora's do. Dot
\ habu-choose-the-register-a95390ac carries taking the number off the routine
\ instead of the caller.

require lib/errors.f
require lib/prelude.f
require src/compiler/native/migrate.f
require tools/codegen-compare-corpus4.f

package CODEGEN-MIGRATED4

private

8 constant REGS                   \ general registers a straight-line routine may use
18 constant LOOP-REGS             \ x0..x17: the whole pool, x18 being platform-reserved

\ Where a published word's code starts, read off its own dictionary record. This
\ is the same reader tools/codegen-compare-core.f measures a size with, on the
\ same record, so the address a call site is given and the bytes the table
\ reports come from one authority.
: ENTRY ( ptr u8 n -- n )
   XREF-FIND dup XREF-FOUND? 0= if E-NPUB-NAME throw then
   XREF-START ;

\ ---- the four callees the call rows reach through -----------------------------
\ The corpus's own four, verbatim but for the `-N`. They are not rows and they
\ are not measured: they are what the call rows call, migrated so that the new
\ column's call rows are the new chain's code all the way down.
: C-ADD1 ( -- )
   s" : C-ADD1-N ( n -- n ) 1 + ;" 1 1 REGS NMIGRATE:DEFINE ;

: C-MUL2 ( -- )
   s" : C-MUL2-N ( n -- n ) 2 * ;" 1 1 REGS NMIGRATE:DEFINE ;

: C-AND7 ( -- )
   s" : C-AND7-N ( n -- n ) 7 and ;" 1 1 REGS NMIGRATE:DEFINE ;

: C-XOR5 ( -- )
   s" : C-XOR5-N ( n -- n ) 5 xor ;" 1 1 REGS NMIGRATE:DEFINE ;

\ ---- the three call rows ------------------------------------------------------

\ Five call sites over four callees. It is the row the inlining rule at the head
\ of tools/codegen-compare-corpus4.f is about: the engine's code for this body
\ contains no call instruction at all and the chain's contains five.
: CALL-FAN ( -- )
   s" C-ADD1-N" s" CODEGEN-CORPUS4:C-ADD1-N" ENTRY 1 1 NMIGRATE:CALLEE
   s" C-MUL2-N" s" CODEGEN-CORPUS4:C-MUL2-N" ENTRY 1 1 NMIGRATE:CALLEE
   s" C-AND7-N" s" CODEGEN-CORPUS4:C-AND7-N" ENTRY 1 1 NMIGRATE:CALLEE
   s" C-XOR5-N" s" CODEGEN-CORPUS4:C-XOR5-N" ENTRY 1 1 NMIGRATE:CALLEE
   s" : CALL-FAN-N ( n -- n ) C-ADD1-N C-MUL2-N C-AND7-N C-XOR5-N C-ADD1-N ;"
   1 1 REGS NMIGRATE:DEFINE-CALLING ;

\ Three calls a turn with three locals live across the loop and read after it.
: CALL-LOOP-3 ( -- )
   s" C-ADD1-N" s" CODEGEN-CORPUS4:C-ADD1-N" ENTRY 1 1 NMIGRATE:CALLEE
   s" C-MUL2-N" s" CODEGEN-CORPUS4:C-MUL2-N" ENTRY 1 1 NMIGRATE:CALLEE
   s" C-XOR5-N" s" CODEGEN-CORPUS4:C-XOR5-N" ENTRY 1 1 NMIGRATE:CALLEE
   s" : CALL-LOOP-3-N ( n n n n n -- n ) {: a:n b:n c:n seed:n len:n :} seed len 0 ?do C-ADD1-N C-MUL2-N C-XOR5-N loop a + b + c + ;"
   5 1 LOOP-REGS NMIGRATE:DEFINE-CALLING ;

\ Four calls a turn and nothing else in the body.
: TINY-CALLEE ( -- )
   s" C-ADD1-N" s" CODEGEN-CORPUS4:C-ADD1-N" ENTRY 1 1 NMIGRATE:CALLEE
   s" : TINY-CALLEE-N ( n n -- n ) {: seed:n len:n :} seed len 0 ?do C-ADD1-N C-ADD1-N C-ADD1-N C-ADD1-N loop ;"
   2 1 LOOP-REGS NMIGRATE:DEFINE-CALLING ;

\ ---- the two straight-line rows -----------------------------------------------

: WIDE-ARITY ( -- )
   s" : WIDE-ARITY-N ( n n n n n n -- n ) {: a:n b:n c:n d:n e:n f:n :} a b - c d - + e f - + ;"
   6 1 REGS NMIGRATE:DEFINE ;

: LADDER ( -- )
   s" : LADDER-N ( n -- n ) {: x:n :} x 1 < if 0 exit then x 2 < if 1 exit then x 4 < if 2 exit then x 8 < if 3 exit then x 16 < if 4 exit then x 32 < if 5 exit then x 64 < if 6 exit then x 128 < if 7 exit then 8 ;"
   1 1 REGS NMIGRATE:DEFINE ;

\ ---- the four loop rows with no call in them ----------------------------------

: BIG-CONSTS ( -- )
   s" : BIG-CONSTS-N ( n -- n ) {: len:n :} 0 len 0 ?do i 1234605616436508552 xor + i 7378697629483820646 xor + i -6148914691236517206 xor + i 1311768467294899695 xor + loop ;"
   1 1 LOOP-REGS NMIGRATE:DEFINE ;

: MANY-LOCALS ( -- )
   s" : MANY-LOCALS-N ( n n n n n n n n n -- n ) {: a:n b:n c:n d:n e:n f:n g:n h:n len:n :} 0 len 0 ?do a + b + c + d + e + f + g + h + loop ;"
   9 1 LOOP-REGS NMIGRATE:DEFINE ;

: FLOAT-MIX ( -- )
   s" : FLOAT-MIX-N ( n n -- n ) {: seed:n len:n :} seed len 0 ?do i s>f 0.5 f* f>s + loop ;"
   2 1 LOOP-REGS NMIGRATE:DEFINE ;

: STORE-LOAD ( -- )
   s" : STORE-LOAD-N ( ptr n n -- n ) {: cell:ptr len:n :} len 0 ?do cell @ 3 + cell ! loop cell @ ;"
   2 1 LOOP-REGS NMIGRATE:DEFINE ;

public

\ Publish the nine, and the four callees the call rows reach through first,
\ because a call site is given the address its callee's record already carries.
\ It is one word rather than thirteen top-level lines because a migration claims
\ code space at the engine's free slot, and the interpreter uses that slot for
\ the line it is running.
: RUN ( -- )
   C-ADD1
   C-MUL2
   C-AND7
   C-XOR5
   CALL-FAN
   CALL-LOOP-3
   TINY-CALLEE
   WIDE-ARITY
   LADDER
   BIG-CONSTS
   MANY-LOCALS
   FLOAT-MIX
   STORE-LOAD ;

;package

\ The definitions land where the current wordlist points when RUN executes, so
\ the corpus's package is reopened around the call: the `-N` words become
\ CODEGEN-CORPUS4 publics, beside the words they are compared against.
package CODEGEN-CORPUS4
public

CODEGEN-MIGRATED4:RUN

;package
