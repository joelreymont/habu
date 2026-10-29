\ t0-label.f - the control labels of the tier 0 compiler, package T0-LABEL.
\
\ The structured and counted-loop words of the seam (src/habu/t0-emit.f
\ CONTROL) keep the same bookkeeping on every target that branches by label:
\ the open structures, the open loop levels and their `leave` exits. This file
\ keeps it once and emits through the branch primitives below. An emitter
\ file (src/habu/t0-x64.f, src/habu/t0-arm64.f) implements them, and its
\ INSTALL stores them into its architecture's slot of each cell, as it does
\ T0-EMIT's, and stores this file's structure words into the seam's control
\ and loop cells. A primitive runs from the slot T0-EMIT selected.
\
\ A branch is the number a forward primitive answers, which only RESOLVE and
\ ENTRY-EQ read back; a head is an offset MARK answered, which a backward
\ branch targets. Comparisons are signed.

require src/habu/t0-emit.f

package T0-LABEL
public

DEFTYPE BRANCH     \ a forward branch, waiting for RESOLVE
DEFTYPE HEAD       \ a loop head, the target of a backward branch

\ ---- the branch primitives ---------------------------------------------------
\ MARK answers the head here. FWD-JUMP branches forward always, FWD-ZERO when
\ the vreg is zero, FWD-NE when the two vregs differ, FWD-NE-LIT when the vreg
\ differs from n (which answers T0-EMIT:IMM? true), FWD-GE when the first vreg
\ is at least the second; each answers the branch, which RESOLVE lands here.
\ FWD-CC branches forward when condition c of the last compare holds.
\ BACK-JUMP branches to the head always, BACK-ZERO when the vreg is zero and
\ BACK-CC when condition c of the last compare holds.
\ ENTRY-EQ makes a FWD-GE branch on equal operands only. LOOP-NEXT and
\ LOOP-STEP are T0-EMIT:LOOP-NEXT and T0-EMIT:LOOP-STEP to the head, without
\ the entry.
T0-EMIT:ARCHES TYPED-BUFFER MARK-XT [ -- head ]
T0-EMIT:ARCHES TYPED-BUFFER FWD-JUMP-XT [ -- branch ]
T0-EMIT:ARCHES TYPED-BUFFER FWD-ZERO-XT [ T0-EMIT:vreg -- branch ]
T0-EMIT:ARCHES TYPED-BUFFER FWD-NE-XT [ T0-EMIT:vreg T0-EMIT:vreg -- branch ]
T0-EMIT:ARCHES TYPED-BUFFER FWD-NE-LIT-XT [ T0-EMIT:vreg n -- branch ]
T0-EMIT:ARCHES TYPED-BUFFER FWD-GE-XT [ T0-EMIT:vreg T0-EMIT:vreg -- branch ]
T0-EMIT:ARCHES TYPED-BUFFER FWD-CC-XT [ T0-EMIT:cc -- branch ]
T0-EMIT:ARCHES TYPED-BUFFER BACK-JUMP-XT [ head -- ]
T0-EMIT:ARCHES TYPED-BUFFER BACK-ZERO-XT [ T0-EMIT:vreg head -- ]
T0-EMIT:ARCHES TYPED-BUFFER BACK-CC-XT [ T0-EMIT:cc head -- ]
T0-EMIT:ARCHES TYPED-BUFFER RESOLVE-XT [ branch -- ]
T0-EMIT:ARCHES TYPED-BUFFER ENTRY-EQ-XT [ branch -- ]
T0-EMIT:ARCHES TYPED-BUFFER LOOP-NEXT-XT [ T0-EMIT:vreg head -- ]
T0-EMIT:ARCHES TYPED-BUFFER LOOP-STEP-XT [ T0-EMIT:vreg T0-EMIT:vreg head -- ]

private

: SLOT ( -- n ) T0-EMIT:SELECTED ;

: MARK ( -- head ) SLOT MARK-XT @ execute ;
: FWD-JUMP ( -- branch ) SLOT FWD-JUMP-XT @ execute ;
: FWD-ZERO ( T0-EMIT:vreg -- branch ) SLOT FWD-ZERO-XT @ execute ;
: FWD-NE ( T0-EMIT:vreg T0-EMIT:vreg -- branch ) SLOT FWD-NE-XT @ execute ;
: FWD-NE-LIT ( T0-EMIT:vreg n -- branch ) SLOT FWD-NE-LIT-XT @ execute ;
: FWD-GE ( T0-EMIT:vreg T0-EMIT:vreg -- branch ) SLOT FWD-GE-XT @ execute ;
: FWD-CC ( T0-EMIT:cc -- branch ) SLOT FWD-CC-XT @ execute ;
: BACK-JUMP ( head -- ) SLOT BACK-JUMP-XT @ execute ;
: BACK-ZERO ( T0-EMIT:vreg head -- ) SLOT BACK-ZERO-XT @ execute ;
: BACK-CC ( T0-EMIT:cc head -- ) SLOT BACK-CC-XT @ execute ;
: RESOLVE ( branch -- ) SLOT RESOLVE-XT @ execute ;
: ENTRY-EQ ( branch -- ) SLOT ENTRY-EQ-XT @ execute ;

\ ---- the open structures -----------------------------------------------------
DYNAMIC-BUFFER CF-KIND n              \ the open structures, innermost last,
DYNAMIC-BUFFER CF-AT n                \ and each one's branch or head
variable CF-N
DYNAMIC-BUFFER LV-HEAD head           \ the open loop levels: the head,
DYNAMIC-BUFFER LV-ENTRY n             \ `?do`'s entry branch or -1,
DYNAMIC-BUFFER LV-FIRST n             \ and the level's first branch in LEAVES
variable LV-N
DYNAMIC-BUFFER LEAVES branch          \ the open levels' `leave` branches, innermost last
variable LEAVE-N

\ An open structure is a POSITION, a forward branch waiting for its target or
\ a loop head, or the start of a case beneath its `endof` exits. Which of the
\ two a position is follows from the word that pops it, which converts it back
\ to its type, so only the case start is marked.
0 constant POSITION
1 constant CASE-START

: CF-PUSH ( n n -- ) {: kind:n at:n :}
   CF-N @ {: k:n :}
   k 1 + CF-KIND-RESERVE  k 1 + CF-AT-RESERVE
   kind k CF-KIND !  at k CF-AT !
   k 1 + CF-N ! ;

: CF-POP ( -- n )
   CF-N @ 1 - {: k:n :}
   k CF-N !
   k CF-AT @ ;

: CF-TOP-KIND ( -- n ) CF-N @ 1 - CF-KIND @ ;

: AT+ ( n -- ) POSITION swap CF-PUSH ;

\ The innermost level and its head.
: LV ( -- n ) LV-N @ 1 - ;
: HEAD ( -- head ) LV LV-HEAD @ ;

public

\ Begin an emission with nothing open; an emitter's OPEN runs it.
: RESET ( -- ) 0 CF-N !  0 LV-N !  0 LEAVE-N ! ;

\ ---- control -----------------------------------------------------------------
: IF, ( T0-EMIT:vreg -- ) FWD-ZERO BRANCH>N AT+ ;

: IF-CC, ( T0-EMIT:cc -- ) T0-EMIT:CC-NOT FWD-CC BRANCH>N AT+ ;

: ELSE, ( -- ) FWD-JUMP CF-POP >BRANCH RESOLVE BRANCH>N AT+ ;

: THEN, ( -- ) CF-POP >BRANCH RESOLVE ;

: BEGIN, ( -- ) MARK HEAD>N AT+ ;

: WHILE, ( T0-EMIT:vreg -- )
   FWD-ZERO {: s:branch :}
   CF-POP {: m:n :}
   s BRANCH>N AT+
   m AT+ ;

: WHILE-CC, ( T0-EMIT:cc -- )
   T0-EMIT:CC-NOT FWD-CC {: s:branch :}
   CF-POP {: m:n :}
   s BRANCH>N AT+
   m AT+ ;

: REPEAT, ( -- ) CF-POP >HEAD BACK-JUMP CF-POP >BRANCH RESOLVE ;

: UNTIL, ( T0-EMIT:vreg -- ) CF-POP >HEAD BACK-ZERO ;

: UNTIL-CC, ( T0-EMIT:cc -- ) T0-EMIT:CC-NOT CF-POP >HEAD BACK-CC ;

: AGAIN, ( -- ) CF-POP >HEAD BACK-JUMP ;

: CASE, ( -- ) CASE-START 0 CF-PUSH ;

: OF, ( T0-EMIT:vreg T0-EMIT:vreg -- ) FWD-NE BRANCH>N AT+ ;

: OF-LIT ( T0-EMIT:vreg n -- ) FWD-NE-LIT BRANCH>N AT+ ;

: ENDOF, ( -- ) ELSE, ;

: ENDCASE, ( -- )
   begin CF-TOP-KIND CASE-START <> while CF-POP >BRANCH RESOLVE repeat
   CF-POP drop ;

\ ---- loops -------------------------------------------------------------------
: LOOP-OPEN ( T0-EMIT:vreg T0-EMIT:vreg bool -- )
   {: s:T0-EMIT:vreg l:T0-EMIT:vreg entry:bool :}
   entry if s l FWD-GE BRANCH>N else -1 then {: at:n :}
   LV-N @ {: k:n :}
   k 1 + LV-HEAD-RESERVE  k 1 + LV-ENTRY-RESERVE  k 1 + LV-FIRST-RESERVE
   MARK k LV-HEAD !
   at k LV-ENTRY !
   LEAVE-N @ k LV-FIRST !
   k 1 + LV-N ! ;

: LEAVE, ( -- )
   LEAVE-N @ {: n:n :}
   n 1 + LEAVES-RESERVE
   FWD-JUMP n LEAVES !
   n 1 + LEAVE-N ! ;

: LOOP-NEXT ( T0-EMIT:vreg -- ) HEAD SLOT LOOP-NEXT-XT @ execute ;

\ The level's `?do` entry now skips equal bounds only.
: LOOP-STEP ( T0-EMIT:vreg T0-EMIT:vreg -- )
   HEAD SLOT LOOP-STEP-XT @ execute
   LV LV-ENTRY @ {: at:n :}
   at 0 >= if at >BRANCH ENTRY-EQ then ;

: LOOP-CLOSE ( -- )
   LV {: k:n :}
   LEAVE-N @ k LV-FIRST @ ?do i LEAVES @ RESOLVE loop
   k LV-ENTRY @ {: at:n :}
   at 0 >= if at >BRANCH RESOLVE then
   k LV-FIRST @ LEAVE-N !
   k LV-N !
   T0-EMIT:LOOP-POP ;

;package
