\ ir-storage-schema.f - Native compiler storage and lifetime fixture rows.
\ These finite cases do not establish general lifetime safety.

require lib/errors.f
require lib/string.f
require src/compiler/ir/arena.f
require src/compiler/ir/context.f

package COMPILER-STORE-PROOF
public

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
\ Names used to identify generated examples.

0 constant ROLE-APPEND      \ appending never disturbs a published ordinal, across a growth step
1 constant ROLE-CEILING     \ a full arena refuses a new cell and stays readable
2 constant ROLE-FREEZE      \ a frozen view answers what the live arena answered
3 constant ROLE-CROSS       \ an index minted by one arena is refused by the other
4 constant ROLE-SCRATCH     \ small allocations are aligned and a bad size preserves usage
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
\ Small allocations and zero-size refusal apply to both allocators. Fixed-map
\ exhaustion is a model property only; dynamic growth is tested in ir-context.f.
: SCRATCH-ROW ( -- )
   CSEQ
      COP-SCRATCH 5 8 C-OK
      COP-SCRATCH 16 24 C-OK
      COP-SCRATCH 1 32 C-OK
      COP-USED 0 32 C-OK
      COP-SCRATCH 0 E-IR-CTX-SIZE C-NO
      COP-USED 0 32 C-OK
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
\ These two concrete depths exercise the accepted and refused nesting cases in
\ both implementations. They do not bind the model's registry representation.

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
