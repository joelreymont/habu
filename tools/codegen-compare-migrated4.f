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
\ ALL TWELVE, AND THE ONE THAT WAS NOT HERE. PRESSURE-LOOP was absent for as long
\ as the chain refused it by name with E-A64RA-SPILL, and its absence was the
\ finding rather than an omission. What closed it is not the allocator either:
\ src/compiler/native/loop.f now takes the fourteen reads and the thirteen
\ additions between them OUT of the loop body - none of them can change with the
\ turn and nothing in the body writes - and what is left is one addition into one
\ accumulator, which is the shape that pass already replaced with the arithmetic
\ the loop would have computed. So the row holds no loop at all, and the values
\ the allocator could not place inside a body are placed in the block above it.
\
\ CALL-PRESSURE WAS THE TWELFTH AND IS NOW A ROW. It was refused for as long as
\ every local a call could reach was handed over at the call whatever the callee
\ destroyed; the elaborator now asks whether the callee published a record and
\ hands over only when nothing survives the branch. Its callee here is the
\ chain's own C-LONG-N, which published one, so the row's eight crossing values
\ stay in registers the callee leaves alone and the body compiles.
\
\ THE SUBSTITUTIONS, AND THERE IS ONLY THE ONE KIND. Every body carries `-N` on
\ the name it defines, which is the migration's own convention, and the four call
\ rows call `C-ADD1-N`, `C-MUL2-N`, `C-AND7-N`, `C-XOR5-N` and `C-MAD-N` where
\ the corpus writes the names without it - the discipline tools/codegen-compare-migrated2.f
\ established when it migrated CELL-FIELD for VEC-COPY-CELLS to call: the new
\ column's word is the new chain's code all the way down, so a row measures a
\ program of the chain's making rather than a chain-compiled shell around the
\ engine's loops. NO CONSTANT IS RESPELLED - the corpus writes its four
\ sixty-four-bit literals in decimal precisely so that this file can hand over the
\ corpus's own text - no operation is changed, no local is renamed and no
\ annotation is added or removed.
\
\ NO ROW HERE STATES A REGISTER BUDGET. It used to: loop rows stated eighteen and
\ straight-line rows eight, as the other three corpora's did, and a refusal under
\ a budget somebody chose is a fact about the budget rather than about the
\ compiler. The migration entry now derives the pool from NABI:SCRATCH
\ (src/compiler/native/abi.f), every general register the machine and the engine
\ leave a routine, so every row here is measured under the same one and no row
\ carries a number that could go stale against it.

require lib/errors.f
require lib/prelude.f
require src/compiler/native/migrate.f
require tools/codegen-compare-core.f
require tools/codegen-compare-corpus4.f

package CODEGEN-MIGRATED4

private

\ ---- the four callees the call rows reach through -----------------------------
\ The corpus's own four, verbatim but for the `-N`. They are not rows and they
\ are not measured: they are what the call rows call, migrated so that the new
\ column's call rows are the new chain's code all the way down.
: C-ADD1 ( -- )
   s" : C-ADD1-N ( n -- n ) 1 + ;" NMIGRATE:DEFINE ;

: C-MUL2 ( -- )
   s" : C-MUL2-N ( n -- n ) 2 * ;" NMIGRATE:DEFINE ;

: C-AND7 ( -- )
   s" : C-AND7-N ( n -- n ) 7 and ;" NMIGRATE:DEFINE ;

: C-XOR5 ( -- )
   s" : C-XOR5-N ( n -- n ) 5 xor ;" NMIGRATE:DEFINE ;

\ ---- and the two callees the engine will not copy -----------------------------
\ C-MAD is what CALL-FAN-BIG-N calls, and the chain copies it: that row's whole
\ point is that the two inlining rules disagree about this one callee.
\
\ C-LONG is what CALL-PRESSURE-N calls, and neither generator copies it, so what
\ crosses that row's call is crossing a call and not an inlined body. It matters
\ MORE than the others that this one is the chain's compilation: a routine the
\ chain published records what its accepted allocation destroys
\ (src/compiler/native/clobber.f), the elaborator asks for that record before it
\ decides whether the row's crossing locals have to travel through a data-stack
\ slot, and the row compiles at all because the answer is yes. Against the
\ engine's C-LONG the same body is refused, which is the pair
\ tools/codegen-spill-probe.f pins.
: C-MAD ( -- )
   s" : C-MAD-N ( n -- n ) 3 * 5 + ;" NMIGRATE:DEFINE ;

: C-LONG ( -- )
   s" : C-LONG-N ( n -- n ) dup 3 * over 5 xor + swap 7 and + dup 11 * + 13 xor ;"
   NMIGRATE:DEFINE ;

\ ---- the three call rows ------------------------------------------------------

\ Five call sites over four callees. It is the row the inlining rule at the head
\ of tools/codegen-compare-corpus4.f is about: the engine's code for this body
\ contains no call instruction at all and the chain's contains five.
: CALL-FAN ( -- )
   s" : CALL-FAN-N ( n -- n ) C-ADD1-N C-MUL2-N C-AND7-N C-XOR5-N C-ADD1-N ;"
   NMIGRATE:DEFINE ;

\ Five sites over the callee the engine calls and the chain copies.
: CALL-FAN-BIG ( -- )
   s" : CALL-FAN-BIG-N ( n -- n ) C-MAD-N C-MAD-N C-MAD-N C-MAD-N C-MAD-N ;"
   NMIGRATE:DEFINE ;

\ Three calls a turn with three locals live across the loop and read after it.
: CALL-LOOP-3 ( -- )
   s" : CALL-LOOP-3-N ( n n n n n -- n ) {: a:n b:n c:n seed:n len:n :} seed len 0 ?do C-ADD1-N C-MUL2-N C-XOR5-N loop a + b + c + ;"
   NMIGRATE:DEFINE ;

\ Four calls a turn and nothing else in the body.
: TINY-CALLEE ( -- )
   s" : TINY-CALLEE-N ( n n -- n ) {: seed:n len:n :} seed len 0 ?do C-ADD1-N C-ADD1-N C-ADD1-N C-ADD1-N loop ;"
   NMIGRATE:DEFINE ;

\ Eight values live across a call the loop really makes, read only after it: the
\ seven locals and the trip count. The callee is C-LONG-N, which neither
\ generator copies.
: CALL-PRESSURE ( -- )
   s" : CALL-PRESSURE-N ( n n n n n n n n n -- n ) {: a:n b:n c:n d:n e:n f:n g:n seed:n len:n :} seed len 0 ?do C-LONG-N loop a + b + c + d + e + f + g + len + ;"
   NMIGRATE:DEFINE ;

\ ---- the two straight-line rows -----------------------------------------------

: WIDE-ARITY ( -- )
   s" : WIDE-ARITY-N ( n n n n n n -- n ) {: a:n b:n c:n d:n e:n f:n :} a b - c d - + e f - + ;"
   NMIGRATE:DEFINE ;

: LADDER ( -- )
   s" : LADDER-N ( n -- n ) {: x:n :} x 1 < if 0 exit then x 2 < if 1 exit then x 4 < if 2 exit then x 8 < if 3 exit then x 16 < if 4 exit then x 32 < if 5 exit then x 64 < if 6 exit then x 128 < if 7 exit then 8 ;"
   NMIGRATE:DEFINE ;

\ ---- the five loop rows with no call in them ----------------------------------

: PRESSURE-LOOP ( -- )
   s" : PRESSURE-LOOP-N ( ptr n n -- n ) {: base:ptr len:n :} 0 len 0 ?do base @ base 8 + @ base 16 + @ base 24 + @ base 32 + @ base 40 + @ base 48 + @ base 56 + @ base 64 + @ base 72 + @ base 80 + @ base 88 + @ base 96 + @ base 104 + @ + + + + + + + + + + + + + + loop ;"
   NMIGRATE:DEFINE ;

: BIG-CONSTS ( -- )
   s" : BIG-CONSTS-N ( n -- n ) {: len:n :} 0 len 0 ?do i 1234605616436508552 xor + i 7378697629483820646 xor + i -6148914691236517206 xor + i 1311768467294899695 xor + loop ;"
   NMIGRATE:DEFINE ;

: MANY-LOCALS ( -- )
   s" : MANY-LOCALS-N ( n n n n n n n n n -- n ) {: a:n b:n c:n d:n e:n f:n g:n h:n len:n :} 0 len 0 ?do a + b + c + d + e + f + g + h + loop ;"
   NMIGRATE:DEFINE ;

: FLOAT-MIX ( -- )
   s" : FLOAT-MIX-N ( n n -- n ) {: seed:n len:n :} seed len 0 ?do i s>f 0.5 f* f>s + loop ;"
   NMIGRATE:DEFINE ;

: STORE-LOAD ( -- )
   s" : STORE-LOAD-N ( ptr n n -- n ) {: cell:ptr len:n :} len 0 ?do cell @ 3 + cell ! loop cell @ ;"
   NMIGRATE:DEFINE ;

public

\ Publish the ten, and the four callees the call rows reach through first,
\ because a call site is given the address its callee's record already carries.
\ It is one word rather than thirteen top-level lines because a migration claims
\ code space at the engine's free slot, and the interpreter uses that slot for
\ the line it is running.
: RUN ( -- )
   C-ADD1
   C-MUL2
   C-AND7
   C-XOR5
   C-MAD
   C-LONG
   CALL-FAN
   CALL-FAN-BIG
   CALL-LOOP-3
   TINY-CALLEE
   CALL-PRESSURE
   WIDE-ARITY
   LADDER
   PRESSURE-LOOP
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
