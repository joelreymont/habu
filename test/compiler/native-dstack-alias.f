\ native-dstack-alias.f - what the chain's data-stack residency answers when a
\ CHECKED program stores through an address that lands in the very region the
\ routine keeps its slots in.
\
\     bin/hb --load test/compiler/native-dstack-alias.f
\
\ WHY THIS SUITE EXISTS. src/compiler/native/select.f drops a data-stack store
\ whenever its residency map says the cell already holds the value that position
\ publishes. Whether that is allowed rests on one question: can a program write
\ those cells behind the pass's back? The answer used to be written down as "no,
\ the type system forbids it", and the answer is YES - `run-in-stack` is a
\ checked primitive (src/core/checker.f) that installs a buffer the caller owns
\ as the data stack a routine's slots live in, so checked code holds the address
\ of the region and an ordinary `c!` reaches a live slot. This suite is the
\ witness for that, and for what follows from it.
\
\ WHAT FOLLOWS FROM IT IS NOT WHAT IT LOOKS LIKE. Memory is authoritative at the
\ transfer points and registers live only between them: a slot is read at the
\ store run and written at the load run, and in between the value is in a
\ register where no address reaches it. So ELIDING the store leaves the cell
\ exactly as the program last wrote it, and KEEPING the store writes the register
\ copy back over what the program put there. The engine's own emitter keeps
\ nothing in a register across a store, so it always shows what the program
\ wrote - and the pass agrees with it by eliding, not by killing.
\
\ THE FIRST CASE IS THEREFORE A TWO-COLUMN MEASUREMENT AND NOT AN ASSERTION
\ ABOUT THE CHAIN ALONE. The same body is compiled twice, once by the engine and
\ once through the whole chain, and run on the same shape of stack; the engine's
\ column is the answer of record. It is also non-vacuous in its own right: the
\ poked answer differs from the unpoked one, so a fixture whose store never
\ landed would read the unpoked value and fail.
\
\ AND IT IS A MINIMAL REPRODUCER OF THE DEFECT THIS SUITE WAS WRITTEN FOR. Before
\ the residency fact had one writer, the need pass planned a save-store the
\ emission then elided, the restore load it had ordered was read by nothing, and
\ the register verifier refused the chain's own emission with E-A64RAV-DKEEP.
\ This body is refused by name on that tree and compiles on this one.
\
\ THE LAST TWO CASES ARE THE POSITIONS A TRANSFER APPLIED TWICE WOULD SILENTLY
\ ELIDE. The transfer MUTATES the map, so a second application reads back what
\ the first one wrote and reports every position as already resident - which
\ drops stores carrying values no cell ever held. A computed value handed to a
\ call, and a computed value returned, are exactly those positions, and both
\ compute here. Measured: with a second application forced into the lowering
\ walk, this suite reds and all three bodies are refused (-8363 and -8610), so
\ the elision cannot be widened without a case saying so.

require lib/prelude.f
require lib/errors.f
require lib/memory.f
require lib/test.f
require src/compiler/native/migrate.f

\ ---- the program under test --------------------------------------------------
\ A package of its own, and PUBLIC, because the bodies below are compiled from
\ source text at run time and name these words the way any other program would -
\ qualified, through the same dictionary the chain resolves every callee through.
package DKA
public

variable SB                          \ base of the fresh data stack
variable SZ                          \ its size
variable ANS                         \ where a body leaves its answer

: ALLOC ( -- )
   MEM-ALLOC-64K SZ !  SB 0 ptr-field ! ;

: BASE ( -- ptr u8 )   SB 0 ptr-field @ ;
: SIZE ( -- n )        SZ @ ;

\ The callees a migrated body reaches, with BASE above. They are engine-compiled
\ on purpose: a callee the chain published carries a clobber record, and a site
\ that may KEEP its live values in registers never asks the map which cells
\ already hold them - which is the question this suite is about.
: SEVEN ( -- n )       7 ;
: ID ( n -- n )        0 + ;
: ANS! ( n -- )        ANS ! ;

\ THE ALIASING BODY. `SEVEN` leaves its result in slot zero and the residency map
\ names it there; `BASE` hands back the address of that very slot; the `c!`
\ overwrites its low byte; and `ID` is the site whose save-store the map says is
\ already true. 7 with its low byte replaced by 200 is 200.
: POKED ( -- )
   SEVEN 200 BASE c! ID ANS! ;

\ The same body with the store removed, so the case can show the store is what
\ moves the answer rather than the shape of the body.
: CLEAN ( -- )
   SEVEN ID ANS! ;

;package

package NDSA-TEST
private

\ `evaluate` is the metaprogramming boundary the checker does not model, and it
\ is how a caller is compiled for a word that did not exist when this file was.
\ Every execution of a migrated body goes through it rather than through a
\ compiled call site, for the reason LESSONS.md records: a call site can be
\ copied by the inliner, and a test written as one then proves nothing.
\ Retirement owner: habu-type-isolated-dynamic-244c0e2c.
TRUSTED: EV ( ptr u8 n -- ) evaluate ;
TRUSTED: EV-N ( ptr u8 n -- n ) evaluate ;

: ENTRY-OF ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u XREF-FIND dup XREF-FOUND? 0= if drop E-NPUB-NAME throw then
   XREF-START ;

\ Every body below names its callees and the chain resolves each of them off the
\ dictionary while it elaborates the body, so a migration is all a caller states.
: MIGRATE ( ptr u8 n -- )
   NMIGRATE:DEFINE ;

\ One run of an engine-compiled body on a fresh data stack, answering what it
\ left behind. The buffer is re-taken per run so no answer can survive into the
\ next one.
: RUN-ENGINE ( n -- n ) {: xt:n :}
   DKA:ALLOC
   0 DKA:ANS !
   xt DKA:BASE DKA:SIZE run-in-stack
   DKA:ANS @ ;

\ And one run of a migrated body, by name, on the same shape of stack.
: RUN-CHAIN ( ptr u8 n -- n ) {: a:ptr u:n :}
   DKA:ALLOC
   0 DKA:ANS !
   a u EV
   DKA:ANS @ ;

\ ---- 1. the witness ----------------------------------------------------------
: ALIAS-CASE ( -- )
   s" the engine shows the poke: a checked store reaches the routine's own slot"
   T-LABEL
   ['] DKA:CLEAN RUN-ENGINE 7 T=
   ['] DKA:POKED RUN-ENGINE 200 T=

   s" the same body through the chain compiles, where it used to be refused"
   T-LABEL
   s" : DKA-CV ( -- ) DKA:SEVEN 200 DKA:BASE c! DKA:ID DKA:ANS! ;" MIGRATE
   s" DKA-CV" ENTRY-OF 0 > TTRUE

   s" and it answers what the engine answers" T-LABEL
   s" ' DKA-CV DKA:BASE DKA:SIZE run-in-stack" RUN-CHAIN 200 T= ;

\ ---- 2. a computed value handed to a call ------------------------------------
\ Nothing ever put this value in a cell, so its store is one the pass must emit.
\ A transfer applied twice would have read its own first answer back and called
\ the position resident, and the callee would have taken whatever the cell held.
: SAVE-CASE ( -- )
   s" a value no cell held is stored before the call that takes it" T-LABEL
   s" : DKA-CW ( -- ) 41 1 + DKA:ID DKA:ANS! ;" MIGRATE
   s" ' DKA-CW DKA:BASE DKA:SIZE run-in-stack" RUN-CHAIN 42 T= ;

\ ---- 3. a computed value returned --------------------------------------------
\ The same question at the exit run, which is a second transfer and a second
\ chance to apply one twice.
: EXIT-CASE ( -- )
   s" and a computed result is published into the cell the caller reads" T-LABEL
   s" : DKA-CX ( -- n ) 41 1 + ;" MIGRATE
   s" DKA-CX" EV-N 42 T= ;

public

: RUN ( -- )
   ALIAS-CASE
   SAVE-CASE
   EXIT-CASE ;

;package

T-RESET
NDSA-TEST:RUN
T-REPORT
