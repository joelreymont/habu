\ ir-storage-schema.f - the shared frozen description of the storage contract.
\
\ The module lives in `package COMPILER-STORE-PROOF`. Its subject is the storage
\ and lifetime layer every other compiler IR module rests on - `IR-ARENA`
\ (src/compiler/ir/arena.f) and `IR-CTX` (src/compiler/ir/context.f) - and the
\ machine-checked model of that layer in `formal/Common/Storage.v`.
\
\ It holds data and nothing else. Six tables:
\
\   1. The pinned capacity constants. The model states the seed span, the
\      mapping size, the registry depth, the generation ceiling and the slot
\      width as numbers. Each one is frozen here beside the production constant
\      it mirrors, so `test/compiler/ir-storage-cases.f` can read the literal out
\      of the shipped source and `test/compiler/ir-storage-obligations.f` can ask
\      Rocq whether the model still holds the same number. Renumber one side and
\      the two sides disagree.
\
\   2. The check-before-write ordering rows. A capacity or lifetime check that
\      runs after the first write leaves a partial row behind on a rejected
\      call; the model's `arena_write_first_leaves_partial_row` exhibits exactly
\      that mutation. Each row names a word and the throw code that is its
\      guard, and the reader classifies the whole file first: a definition WRITES
\      if it carries a write token or CALLS a writer, and it GUARDS if it carries
\      that throw code or calls a guard. Both relations are closed under calls,
\      because `IR-ARENA:PUSH` reaches its ceiling check one call deep through
\      `GROW` and carries no `E-IR-ARENA-FULL` token of its own.
\
\   3. The frozen guard bodies. These small words are what make the model's
\      hypotheses true of the shipped code: index validation, ordinal minting,
\      owner checking, generation stepping and the scratch alignment. Each is
\      entirely guard, so each is frozen whole.
\
\   4. The arena vector rows. A row names a committed ceiling and an ordered
\      list of operations over two arenas, with the answer each operation must
\      receive: a number, or the exact throw code that must reject it. Every
\      step is compared, so a row needs no separate end state. These rows are
\      the one copy.
\      The cases file runs each row through the real `IR-CTX` and `IR-ARENA`
\      words, and the obligations file turns the very same row into a Rocq
\      obligation about `Habu.Common.Storage`. Neither side carries a copy.
\
\   5. The context vector rows. The same idea for the scratch bump allocator and
\      the module-serial budget.
\
\   6. The nesting depth rows. A row names how many contexts to open one inside
\      another and what the next entry after them must do: be accepted, or be
\      refused with an exact throw code. Two rows put the registry's depth limit
\      between them, so raising or lowering that limit makes one of them
\      disagree. The cases file opens the contexts for real, and the obligations
\      file asks Rocq the same question about the model's own nesting.
\
\ Where the two sides are not literally the same shape, and why that is sound:
\
\   - A Habu index is one packed cell holding (generation << 32 |
\     ordinal); the model's is a pair. What the vector rows exchange is only
\     ordinals, counts, values and refusals, so the packing never enters the
\     comparison. The packing itself is `Habu.Common.IdLaws`.
\   - Habu rejects with a named throw and the model rejects with `None`. The row
\     records the throw code, so the Habu diagnostic and the Rocq decision stay
\     bound to each other row by row rather than only agreeing that something
\     failed.
\   - Habu generations come from a process-global counter, so their values differ
\     between runs. No vector answer exposes a generation; cross-owner rows ask
\     only whether an index minted by one arena is refused by the other.
\
\ Consumers: `test/compiler/ir-storage-cases.f`,
\ `test/compiler/ir-storage-obligations.f`.

require lib/errors.f
require lib/string.f
require src/compiler/ir/arena.f
require src/compiler/ir/context.f

package COMPILER-STORE-PROOF
public

: MODEL-FILE$ ( -- ptr u8 n )
   s" formal/Common/Storage.v" ;

: CTX-FILE$ ( -- ptr u8 n )
   s" src/compiler/ir/context.f" ;

\ ---- 1. the pinned capacity constants ----------------------------------------
\ Each row is a production constant and the model definition that mirrors it.
\ `CDIGEST:SLOT-BYTES` lives in src/compiler/digest.f because both storage
\ modules read and write their cells through it.

8 constant PIN-COUNT

: PIN-FILE$ ( n -- ptr u8 n )
   case
      0 of s" src/compiler/ir/context.f" endof
      1 of s" src/compiler/ir/context.f" endof
      2 of s" src/compiler/ir/context.f" endof
      3 of s" src/compiler/ir/context.f" endof
      4 of s" src/compiler/ir/arena.f" endof
      5 of s" src/compiler/ir/arena.f" endof
      6 of s" src/compiler/ir/arena.f" endof
      7 of s" src/compiler/digest.f" endof
      E-CST-ROW throw
   endcase ;

: PIN-NAME$ ( n -- ptr u8 n )
   case
      0 of s" MAP-BYTES" endof
      1 of s" DEPTH-MAX" endof
      2 of s" GEN-MAX" endof
      3 of s" HDR-SLOTS" endof
      4 of s" SLOT-MAX" endof
      5 of s" SEED-CELLS" endof
      6 of s" LOCAL-BITS" endof
      7 of s" SLOT-BYTES" endof
      E-CST-ROW throw
   endcase ;

\ The name the model gives the same number.
: PIN-MODEL$ ( n -- ptr u8 n )
   case
      0 of s" map_bytes" endof
      1 of s" depth_max" endof
      2 of s" gen_max" endof
      3 of s" hdr_slots" endof
      4 of s" slot_max" endof
      5 of s" seed_cells" endof
      6 of s" local_bits" endof
      7 of s" slot_bytes" endof
      E-CST-ROW throw
   endcase ;

: PIN-VALUE ( n -- n )
   case
      0 of $40000 endof
      1 of 64 endof
      2 of $7FFFFFFF endof
      3 of 16 endof
      4 of 64 endof
      5 of 8 endof
      6 of 32 endof
      7 of 8 endof
      E-CST-ROW throw
   endcase ;

\ ---- 2. the check-before-write ordering --------------------------------------
\ The tokens that publish storage: a cell write, a cursor or capacity or state
\ publication, and the atomic step that commits a generation counter.
\
\ `AGEN!` is deliberately NOT a write token. Installing or retiring a registry
\ slot is the fail-closed action itself rather than a published write:
\ `IR-ARENA:RESOLVE` retires a slot whose owner died and then throws, and
\ `IR-ARENA:ABORT` retires a slot to consume the builder. Counting those as
\ writes would make every word that resolves a handle look like a writer whose
\ guard runs too late, which is the opposite of what those two words do.

7 constant WRITE-TOKENS

: WRITE-TOKEN$ ( n -- ptr u8 n )
   case
      0 of s" CDIGEST:SLOT!" endof
      1 of s" ACOUNT!" endof
      2 of s" ACAP!" endof
      3 of s" ACEIL!" endof
      4 of s" ASTATE!" endof
      5 of s" AOWNER!" endof
      6 of s" atomic-cas" endof
      E-CST-ROW throw
   endcase ;

8 constant ORDER-COUNT

: ORDER-FILE$ ( n -- ptr u8 n )
   case
      0 of s" src/compiler/ir/arena.f" endof
      1 of s" src/compiler/ir/arena.f" endof
      2 of s" src/compiler/ir/arena.f" endof
      3 of s" src/compiler/ir/arena.f" endof
      4 of s" src/compiler/ir/context.f" endof
      5 of s" src/compiler/ir/context.f" endof
      6 of s" src/compiler/ir/context.f" endof
      7 of s" src/compiler/ir/context.f" endof
      E-CST-ROW throw
   endcase ;

: ORDER-WORD$ ( n -- ptr u8 n )
   case
      0 of s" GROW" endof
      1 of s" PUSH" endof
      2 of s" NEW" endof
      3 of s" FREEZE" endof
      4 of s" SCRATCH-TAKE" endof
      5 of s" CTX-ENTER" endof
      6 of s" WITH-CONTEXT-BOUND" endof
      7 of s" MINT-TAKE" endof
      E-CST-ROW throw
   endcase ;

\ The throw code that is this word's guard. A definition carries the guard if it
\ names the code or calls something that does.
: ORDER-GUARD$ ( n -- ptr u8 n )
   case
      0 of s" E-IR-ARENA-FULL" endof
      1 of s" E-IR-ARENA-FULL" endof
      2 of s" E-IR-ARENA-CEIL" endof
      3 of s" E-IR-ARENA-FROZEN" endof
      4 of s" E-IR-CTX-SCRATCH" endof
      5 of s" E-IR-CTX-DEPTH" endof
      6 of s" E-IR-CTX-CEILING" endof
      7 of s" E-IR-CTX-SERIALS" endof
      E-CST-ROW throw
   endcase ;

\ ---- 3. the frozen guard bodies ----------------------------------------------
\ Each of these is what makes one of the model's hypotheses true of the shipped
\ code. `IR-CTX:CTX-ENTER` is frozen whole for a second reason: it is the
\ place the proofs came out weaker than the source comments claim, so the
\ exact text it was read against is pinned here. `Habu.Common.Storage`
\ records it as a finding.

10 constant GUARD-COUNT

: GUARD-FILE$ ( n -- ptr u8 n )
   case
      0 of s" src/compiler/ir/arena.f" endof
      1 of s" src/compiler/ir/arena.f" endof
      2 of s" src/compiler/ir/arena.f" endof
      3 of s" src/compiler/ir/arena.f" endof
      4 of s" src/compiler/ir/arena.f" endof
      5 of s" src/compiler/ir/context.f" endof
      6 of s" src/compiler/ir/context.f" endof
      7 of s" src/compiler/ir/context.f" endof
      8 of s" src/compiler/ir/context.f" endof
      9 of s" src/compiler/ir/context.f" endof
      E-CST-ROW throw
   endcase ;

: GUARD-WORD$ ( n -- ptr u8 n )
   case
      0 of s" IDX-AT" endof
      1 of s" NTH-RAW" endof
      2 of s" OWN-CHECK" endof
      3 of s" CEIL-OK" endof
      4 of s" AGEN-NEXT-N" endof
      5 of s" GEN-NEXT-N" endof
      6 of s" ALIGN8" endof
      7 of s" FIND-SLOT" endof
      8 of s" CTX-ENTER" endof
      9 of s" CE-SCOPE" endof
      E-CST-ROW throw
   endcase ;

: GUARD-BODY$ ( n -- ptr u8 n )
   case
      0 of s" {: slot:n raw:n :} raw PACK-GEN slot AGEN@ <> if E-IR-ARENA-OWNER throw then raw PACK-LOCAL dup slot ACOUNT@ >= if E-IR-ARENA-BOUND throw then" endof
      1 of s" {: slot:n k:n :} k 0 < k slot ACOUNT@ >= or if E-IR-ARENA-BOUND throw then slot AGEN@ k PACK MINT-IDX" endof
      2 of s" {: c:IR-CTX:ctx slot:n :} c IR-CTX:SERIAL slot AOWNER@ <> if E-IR-ARENA-OWNER throw then" endof
      3 of s" dup 1 < over CEIL-MAX > or if E-IR-ARENA-CEIL throw then drop" endof
      4 of s" dup 0 < over AGEN-MAX >= or if E-IR-ARENA-SERIALS throw then 1+" endof
      5 of s" dup 0 < over GEN-MAX >= or if E-IR-CTX-SERIALS throw then 1+" endof
      6 of s" 7 + 8 / 8 *" endof
      7 of s" {: g:n :} -1 DEPTH @ 0 ?do g i GEN@ = if drop i leave then loop" endof
      8 of s" drop DEPTH-ROOM DEPTH @ TAKE-GEN {: at:n g:n :} at CTX-INSTALL g at GEN! at 1+ DEPTH ! at CE-SCOPE" endof
      9 of s" {: at:n :} at BODY! [: CE-RUN ;] catch at CTX-RETIRE dup 0 <> if throw then drop" endof
      E-CST-ROW throw
   endcase ;

\ The token run that retires this context and truncates the registry back to the
\ depth saved at entry. It is what makes leaving a context release every child
\ context and every arena registered inside it in one step. It sits in
\ CTX-RETIRE, which CE-SCOPE runs after the catch around the body, so it is
\ reached whether the body returned or threw.
: TEARDOWN-RUN$ ( -- ptr u8 n )
   s" 0 at GEN! at DEPTH !" ;

: BODY-CALL$ ( -- ptr u8 n )
   s" MINT-CTX swap BODY@ execute" ;

\ ---- 4 and 5. the operation vocabulary ---------------------------------------

0 constant OP-PUSH          \ append the argument; answers its ordinal
1 constant OP-PEEK          \ read the cell at the argument ordinal
2 constant OP-USED          \ the live cell count
3 constant OP-FREEZE        \ consume the builder; answers the published size
4 constant OP-AT            \ read through the frozen view
5 constant OP-KEEP          \ mint and keep the index at the argument ordinal
6 constant OP-READ          \ read the kept index in this arena
7 constant OP-ABORT         \ consume the builder without publishing; answers 0
8 constant OP-COUNT

0 constant COP-SCRATCH      \ bump-allocate the argument bytes; answers the used total
1 constant COP-MINT         \ mint one module identity; answers the minted total
2 constant COP-USED         \ the scratch bytes used so far
3 constant COP-MINTED       \ the modules minted so far
4 constant COP-COUNT

\ ---- what each vector row is there to show -----------------------------------
\ Every role must be covered by at least one row. The tables carry no digest, so
\ the roles are the freeze: a row that stops being covered fails the coverage
\ check in the cases file rather than quietly shrinking what the gate asks.

0 constant ROLE-APPEND      \ appending never disturbs a published ordinal, across a growth step
1 constant ROLE-CEILING     \ a full arena refuses a new cell and stays readable
2 constant ROLE-FREEZE      \ a frozen view answers what the live arena answered
3 constant ROLE-CROSS       \ an index minted by one arena is refused by the other
4 constant ROLE-SCRATCH     \ bump allocation is monotone and bounded by the mapping
5 constant ROLE-BUDGET      \ the module-serial budget is spent exactly once
6 constant ROLE-ABORT       \ aborting retires the arena, and every index dies with it
7 constant ROLE-DEPTH       \ nesting contexts stops exactly at the registry depth
8 constant ROLE-COUNT

: ROLE-NAME$ ( n -- ptr u8 n )
   case
      0 of s" append" endof
      1 of s" ceiling" endof
      2 of s" freeze" endof
      3 of s" cross_owner" endof
      4 of s" scratch" endof
      5 of s" budget" endof
      6 of s" abort" endof
      7 of s" depth" endof
      E-CST-ROW throw
   endcase ;

private

\ ---- storage -----------------------------------------------------------------

$80 constant STEP-CAP
$10 constant SCN-CAP

create STEP-WHICH STEP-CAP cells allot
create STEP-OP STEP-CAP cells allot
create STEP-ARG STEP-CAP cells allot
create STEP-ANS STEP-CAP cells allot
create STEP-CLASS STEP-CAP cells allot

create SCN-ROLE SCN-CAP cells allot
create SCN-CEIL SCN-CAP cells allot
create SCN-BASE SCN-CAP cells allot
create SCN-LEN SCN-CAP cells allot

variable STEP-N
variable SCN-N
variable OPEN-BASE

create CSTEP-OP STEP-CAP cells allot
create CSTEP-ARG STEP-CAP cells allot
create CSTEP-ANS STEP-CAP cells allot
create CSTEP-CLASS STEP-CAP cells allot

create CSCN-ROLE SCN-CAP cells allot
create CSCN-CEIL SCN-CAP cells allot
create CSCN-BASE SCN-CAP cells allot
create CSCN-LEN SCN-CAP cells allot

variable CSTEP-N
variable CSCN-N
variable COPEN-BASE

create DSCN-DEPTH SCN-CAP cells allot
create DSCN-CLASS SCN-CAP cells allot

variable DSCN-N

: STEP-RANGE ( n -- ) {: i:n :}
   i 0 < i STEP-N @ >= or if E-CST-ROW throw then ;

: SCN-RANGE ( n -- ) {: i:n :}
   i 0 < i SCN-N @ >= or if E-CST-ROW throw then ;

: CSTEP-RANGE ( n -- ) {: i:n :}
   i 0 < i CSTEP-N @ >= or if E-CST-ROW throw then ;

: CSCN-RANGE ( n -- ) {: i:n :}
   i 0 < i CSCN-N @ >= or if E-CST-ROW throw then ;

: DSCN-RANGE ( n -- ) {: i:n :}
   i 0 < i DSCN-N @ >= or if E-CST-ROW throw then ;

\ ---- arena table builders ----------------------------------------------------
\ One step: which arena it addresses, the operation, its argument, the answer it
\ must receive, and the throw code that must reject it. An accepted step carries
\ class 0 and its answer; a rejected step carries its code and the unusable
\ answer -1.

: STEP+ ( n n n n n -- ) {: which:n op:n arg:n ans:n class:n :}
   STEP-N @ STEP-CAP >= if E-CST-ROW throw then
   which STEP-WHICH STEP-N @ cells + !
   op STEP-OP STEP-N @ cells + !
   arg STEP-ARG STEP-N @ cells + !
   ans STEP-ANS STEP-N @ cells + !
   class STEP-CLASS STEP-N @ cells + !
   STEP-N @ 1+ STEP-N ! ;

: A-OK ( n n n -- ) {: op:n arg:n ans:n :}
   0 op arg ans 0 STEP+ ;

: A-NO ( n n n -- ) {: op:n arg:n class:n :}
   0 op arg -1 class STEP+ ;

: B-OK ( n n n -- ) {: op:n arg:n ans:n :}
   1 op arg ans 0 STEP+ ;

: SEQ ( -- )
   STEP-N @ OPEN-BASE ! ;

: ;SEQ ( n n -- ) {: role:n ceiling:n :}
   SCN-N @ SCN-CAP >= if E-CST-ROW throw then
   role SCN-ROLE SCN-N @ cells + !
   ceiling SCN-CEIL SCN-N @ cells + !
   OPEN-BASE @ SCN-BASE SCN-N @ cells + !
   STEP-N @ OPEN-BASE @ - SCN-LEN SCN-N @ cells + !
   SCN-N @ 1+ SCN-N ! ;

\ ---- the arena vector rows ---------------------------------------------------
\ Read a block as: open, drive these operations in this order, close with the
\ role and the committed ceiling both arenas are created with.

\ Nine cells into an arena seeded at eight forces one growth step. Every earlier
\ ordinal still reads its own value afterwards, which is the whole content of
\ "growth is invisible to a reader".
: APPEND-ROW ( -- )
   SEQ
      OP-PUSH 10 0 A-OK   OP-PUSH 11 1 A-OK   OP-PUSH 12 2 A-OK
      OP-PUSH 13 3 A-OK   OP-PUSH 14 4 A-OK   OP-PUSH 15 5 A-OK
      OP-PUSH 16 6 A-OK   OP-PUSH 17 7 A-OK   OP-PUSH 18 8 A-OK
      OP-PEEK 0 10 A-OK   OP-PEEK 7 17 A-OK   OP-PEEK 8 18 A-OK
      OP-USED 0 9 A-OK
   ROLE-APPEND 16 ;SEQ ;

\ A ceiling of two cells is reached without any growth, so the refusal is the
\ ceiling's and not the span's. The arena stays readable afterwards.
: CEILING-ROW ( -- )
   SEQ
      OP-PUSH 5 0 A-OK
      OP-PUSH 6 1 A-OK
      OP-PUSH 7 E-IR-ARENA-FULL A-NO
      OP-USED 0 2 A-OK
      OP-PEEK 1 6 A-OK
   ROLE-CEILING 2 ;SEQ ;

\ The frozen view answers what the live arena answered, and every builder word
\ left holding the consumed handle is refused.
: FREEZE-ROW ( -- )
   SEQ
      OP-PUSH 7 0 A-OK
      OP-PUSH 8 1 A-OK
      OP-FREEZE 0 2 A-OK
      OP-AT 1 8 A-OK
      OP-AT 0 7 A-OK
      OP-AT 2 E-IR-ARENA-BOUND A-NO
      OP-PEEK 1 E-IR-ARENA-FROZEN A-NO
      OP-PUSH 9 E-IR-ARENA-FROZEN A-NO
      OP-USED 0 E-IR-ARENA-FROZEN A-NO
   ROLE-FREEZE 8 ;SEQ ;

\ An index minted by arena B names arena B. Arena A refuses it before the
\ ordinal is used, even though that ordinal is inside arena A's own range.
: CROSS-ROW ( -- )
   SEQ
      OP-PUSH 77 0 B-OK
      OP-KEEP 0 0 B-OK
      OP-PUSH 11 0 A-OK
      OP-READ 0 E-IR-ARENA-OWNER A-NO
      OP-READ 0 77 B-OK
      OP-USED 0 1 A-OK
      OP-USED 0 1 B-OK
   ROLE-CROSS 8 ;SEQ ;

\ Aborting consumes the builder and retires its registry slot at once, so the
\ index kept before the abort stops resolving and the handle itself is stale. The
\ other arena is untouched, which is what makes this the arena's own death and
\ not the whole registry's.
: ABORT-ROW ( -- )
   SEQ
      OP-PUSH 11 0 A-OK
      OP-KEEP 0 0 A-OK
      OP-READ 0 11 A-OK
      OP-ABORT 0 0 A-OK
      OP-READ 0 E-IR-ARENA-STALE A-NO
      OP-PUSH 21 0 B-OK
      OP-USED 0 1 B-OK
   ROLE-ABORT 8 ;SEQ ;

: BUILD-ARENA-ROWS ( -- )
   0 STEP-N !
   0 SCN-N !
   APPEND-ROW
   CEILING-ROW
   FREEZE-ROW
   CROSS-ROW
   ABORT-ROW ;

\ ---- context table builders --------------------------------------------------

: CSTEP+ ( n n n n -- ) {: op:n arg:n ans:n class:n :}
   CSTEP-N @ STEP-CAP >= if E-CST-ROW throw then
   op CSTEP-OP CSTEP-N @ cells + !
   arg CSTEP-ARG CSTEP-N @ cells + !
   ans CSTEP-ANS CSTEP-N @ cells + !
   class CSTEP-CLASS CSTEP-N @ cells + !
   CSTEP-N @ 1+ CSTEP-N ! ;

: C-OK ( n n n -- ) {: op:n arg:n ans:n :}
   op arg ans 0 CSTEP+ ;

: C-NO ( n n n -- ) {: op:n arg:n class:n :}
   op arg -1 class CSTEP+ ;

: CSEQ ( -- )
   CSTEP-N @ COPEN-BASE ! ;

: ;CSEQ ( n n -- ) {: role:n ceiling:n :}
   CSCN-N @ SCN-CAP >= if E-CST-ROW throw then
   role CSCN-ROLE CSCN-N @ cells + !
   ceiling CSCN-CEIL CSCN-N @ cells + !
   COPEN-BASE @ CSCN-BASE CSCN-N @ cells + !
   CSTEP-N @ COPEN-BASE @ - CSCN-LEN CSCN-N @ cells + !
   CSCN-N @ 1+ CSCN-N ! ;

\ ---- the context vector rows -------------------------------------------------
\ The scratch cursor starts just past the sixteen-slot header, and every answer
\ below is the bytes used since then. A take is rounded up to eight bytes, so
\ five bytes cost eight and no two spans can overlap. A refused take does not
\ move the cursor, which is why the used total repeats after each refusal. The
\ last accepted take fills the mapping exactly to its last byte.
\
\ The three large numbers below are the scratch capacity plus one, the capacity
\ itself, and the capacity less the thirty-two bytes already used. They are the
\ two refusal guards and the exact fill, in that order: the first is too big for
\ any cursor, the second fits the capacity but not the cursor it meets, and the
\ third lands on the last byte of the mapping. Changing IR-CTX:MAP-BYTES moves
\ all three, the used total after them, and `map_bytes` in
\ `formal/Common/Storage.v` together - the pinned capacity row above is what
\ makes forgetting either side fail.
: SCRATCH-ROW ( -- )
   CSEQ
      COP-SCRATCH 5 8 C-OK
      COP-SCRATCH 16 24 C-OK
      COP-SCRATCH 1 32 C-OK
      COP-USED 0 32 C-OK
      COP-SCRATCH 0 E-IR-CTX-SIZE C-NO
      COP-USED 0 32 C-OK
      COP-SCRATCH 262017 E-IR-CTX-SCRATCH C-NO
      COP-USED 0 32 C-OK
      COP-SCRATCH 262016 E-IR-CTX-SCRATCH C-NO
      COP-USED 0 32 C-OK
      COP-SCRATCH 261984 262016 C-OK
      COP-SCRATCH 1 E-IR-CTX-SCRATCH C-NO
      COP-USED 0 262016 C-OK
   ROLE-SCRATCH 4 ;CSEQ ;

\ The module budget is reserved against this context's ceiling before the global
\ identity is taken, so the count a full context reports is the count it spent.
: BUDGET-ROW ( -- )
   CSEQ
      COP-MINT 0 1 C-OK
      COP-MINT 0 2 C-OK
      COP-MINT 0 E-IR-CTX-SERIALS C-NO
      COP-MINTED 0 2 C-OK
   ROLE-BUDGET 2 ;CSEQ ;

: BUILD-CTX-ROWS ( -- )
   0 CSTEP-N !
   0 CSCN-N !
   SCRATCH-ROW
   BUDGET-ROW ;

\ ---- the nesting depth rows --------------------------------------------------
\ A row is a number of contexts to open one inside another, and what the entry
\ after them must do. The two rows below sit either side of the registry's depth
\ limit: at one context below the limit the next entry is still accepted, and at
\ the limit itself it is refused by name. Raising the limit makes the second row
\ answer nothing where a refusal was recorded, and lowering it makes the first
\ row answer a refusal where an acceptance was recorded, so the pair pins the
\ limit rather than any one side of it.
\
\ The two depths are written down rather than computed from DEPTH-MAX on
\ purpose. A row derived from the production constant would agree with any
\ renumbering of it and so would ask nothing; a renumbering is what the pinned
\ capacity row above is for.

: DROW+ ( n n -- ) {: depth:n class:n :}
   DSCN-N @ SCN-CAP >= if E-CST-ROW throw then
   depth DSCN-DEPTH DSCN-N @ cells + !
   class DSCN-CLASS DSCN-N @ cells + !
   DSCN-N @ 1+ DSCN-N ! ;

: BUILD-DEPTH-ROWS ( -- )
   0 DSCN-N !
   63 0 DROW+
   64 E-IR-CTX-DEPTH DROW+ ;

BUILD-ARENA-ROWS
BUILD-CTX-ROWS
BUILD-DEPTH-ROWS

public

: SCENARIOS ( -- n )        SCN-N @ ;
: STEPS ( -- n )            STEP-N @ ;

: SCN-ROLE@ ( n -- n )      dup SCN-RANGE cells SCN-ROLE + @ ;
: SCN-CEIL@ ( n -- n )      dup SCN-RANGE cells SCN-CEIL + @ ;
: SCN-BASE@ ( n -- n )      dup SCN-RANGE cells SCN-BASE + @ ;
: SCN-LEN@ ( n -- n )       dup SCN-RANGE cells SCN-LEN + @ ;

: STEP-WHICH@ ( n -- n )    dup STEP-RANGE cells STEP-WHICH + @ ;
: STEP-OP@ ( n -- n )       dup STEP-RANGE cells STEP-OP + @ ;
: STEP-ARG@ ( n -- n )      dup STEP-RANGE cells STEP-ARG + @ ;
: STEP-ANS@ ( n -- n )      dup STEP-RANGE cells STEP-ANS + @ ;
: STEP-CLASS@ ( n -- n )    dup STEP-RANGE cells STEP-CLASS + @ ;

: CSCENARIOS ( -- n )       CSCN-N @ ;
: CSTEPS ( -- n )           CSTEP-N @ ;

: CSCN-ROLE@ ( n -- n )     dup CSCN-RANGE cells CSCN-ROLE + @ ;
: CSCN-CEIL@ ( n -- n )     dup CSCN-RANGE cells CSCN-CEIL + @ ;
: CSCN-BASE@ ( n -- n )     dup CSCN-RANGE cells CSCN-BASE + @ ;
: CSCN-LEN@ ( n -- n )      dup CSCN-RANGE cells CSCN-LEN + @ ;

: CSTEP-OP@ ( n -- n )      dup CSTEP-RANGE cells CSTEP-OP + @ ;
: CSTEP-ARG@ ( n -- n )     dup CSTEP-RANGE cells CSTEP-ARG + @ ;
: CSTEP-ANS@ ( n -- n )     dup CSTEP-RANGE cells CSTEP-ANS + @ ;
: CSTEP-CLASS@ ( n -- n )   dup CSTEP-RANGE cells CSTEP-CLASS + @ ;

: DSCENARIOS ( -- n )       DSCN-N @ ;

: DSCN-DEPTH@ ( n -- n )    dup DSCN-RANGE cells DSCN-DEPTH + @ ;
: DSCN-CLASS@ ( n -- n )    dup DSCN-RANGE cells DSCN-CLASS + @ ;

;package
