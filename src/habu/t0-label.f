\ t0-label.f - the control labels of the tier 0 compiler, part LABEL of
\ package COMP.
\
\ Keeps the open structures, loop levels and `leave` exits once for every
\ target that branches by label (src/habu/t0-emit.f CONTROL). An emitter file
\ (src/habu/t0-x64.f, src/habu/t0-arm64.f) implements the branch primitives
\ below; its FILL-SLOT stores them in its architecture's slot of each cell, and
\ this file's structure words in the seam's control and loop cells.
\ A branch is the number a forward primitive answers, read back only by RESOLVE
\ and ENTRY-EQ; a head is an offset MARK answered. Comparisons are signed.

require src/habu/t0-emit.f

package COMP
package LABEL
public

DEFTYPE BRANCH
DEFTYPE HEAD

\ ---- the branch primitives ---------------------------------------------------
\ MARK answers the head here. FWD-JUMP branches forward always, FWD-ZERO when
\ the vreg is zero, FWD-NE when the two vregs differ, FWD-NE-LIT when the vreg
\ differs from n (which answers EMIT-IMM? true), FWD-GE when the first vreg
\ is at least the second; each answers the branch, which RESOLVE lands here.
\ FWD-CC branches forward when condition c of the last compare holds.
\ BACK-JUMP branches to the head always, BACK-ZERO when the vreg is zero and
\ BACK-CC when condition c of the last compare holds.
\ ENTRY-EQ makes a FWD-GE branch on equal operands only. LOOP-NEXT and
\ LOOP-STEP are EMIT-LOOP-NEXT and EMIT-LOOP-STEP to the head, without
\ the entry.
ARCHES TYPED-BUFFER MARK-XT [ -- head ]
ARCHES TYPED-BUFFER FWD-JUMP-XT [ -- branch ]
ARCHES TYPED-BUFFER FWD-ZERO-XT [ vreg -- branch ]
ARCHES TYPED-BUFFER FWD-NE-XT [ vreg vreg -- branch ]
ARCHES TYPED-BUFFER FWD-NE-LIT-XT [ vreg n -- branch ]
ARCHES TYPED-BUFFER FWD-GE-XT [ vreg vreg -- branch ]
ARCHES TYPED-BUFFER FWD-CC-XT [ cc -- branch ]
ARCHES TYPED-BUFFER BACK-JUMP-XT [ head -- ]
ARCHES TYPED-BUFFER BACK-ZERO-XT [ vreg head -- ]
ARCHES TYPED-BUFFER BACK-CC-XT [ cc head -- ]
ARCHES TYPED-BUFFER RESOLVE-XT [ branch -- ]
ARCHES TYPED-BUFFER ENTRY-EQ-XT [ branch -- ]
ARCHES TYPED-BUFFER LOOP-NEXT-XT [ vreg head -- ]
ARCHES TYPED-BUFFER LOOP-STEP-XT [ vreg vreg head -- ]

private

: SLOT ( -- n ) EMIT-SELECTED ;

: MARK ( -- head ) SLOT MARK-XT @ execute ;
: FWD-JUMP ( -- branch ) SLOT FWD-JUMP-XT @ execute ;
: FWD-ZERO ( vreg -- branch ) SLOT FWD-ZERO-XT @ execute ;
: FWD-NE ( vreg vreg -- branch ) SLOT FWD-NE-XT @ execute ;
: FWD-NE-LIT ( vreg n -- branch ) SLOT FWD-NE-LIT-XT @ execute ;
: FWD-GE ( vreg vreg -- branch ) SLOT FWD-GE-XT @ execute ;
: FWD-CC ( cc -- branch ) SLOT FWD-CC-XT @ execute ;
: BACK-JUMP ( head -- ) SLOT BACK-JUMP-XT @ execute ;
: BACK-ZERO ( vreg head -- ) SLOT BACK-ZERO-XT @ execute ;
: BACK-CC ( cc head -- ) SLOT BACK-CC-XT @ execute ;
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

: LV ( -- n ) LV-N @ 1 - ;
: HEAD ( -- head ) LV LV-HEAD @ ;

public

\ An emitter's OPEN runs this.
: RESET ( -- ) 0 CF-N !  0 LV-N !  0 LEAVE-N ! ;

\ ---- control -----------------------------------------------------------------
: IF, ( vreg -- ) FWD-ZERO BRANCH>N AT+ ;

: IF-CC, ( cc -- ) CC-NOT FWD-CC BRANCH>N AT+ ;

: ELSE, ( -- ) FWD-JUMP CF-POP >BRANCH RESOLVE BRANCH>N AT+ ;

: THEN, ( -- ) CF-POP >BRANCH RESOLVE ;

: BEGIN, ( -- ) MARK HEAD>N AT+ ;

: WHILE, ( vreg -- )
   FWD-ZERO {: s:branch :}
   CF-POP {: m:n :}
   s BRANCH>N AT+
   m AT+ ;

: WHILE-CC, ( cc -- )
   CC-NOT FWD-CC {: s:branch :}
   CF-POP {: m:n :}
   s BRANCH>N AT+
   m AT+ ;

: REPEAT, ( -- ) CF-POP >HEAD BACK-JUMP CF-POP >BRANCH RESOLVE ;

: UNTIL, ( vreg -- ) CF-POP >HEAD BACK-ZERO ;

: UNTIL-CC, ( cc -- ) CC-NOT CF-POP >HEAD BACK-CC ;

: AGAIN, ( -- ) CF-POP >HEAD BACK-JUMP ;

: CASE, ( -- ) CASE-START 0 CF-PUSH ;

: OF, ( vreg vreg -- ) FWD-NE BRANCH>N AT+ ;

: OF-LIT ( vreg n -- ) FWD-NE-LIT BRANCH>N AT+ ;

: ENDOF, ( -- ) ELSE, ;

: ENDCASE, ( -- )
   begin CF-TOP-KIND CASE-START <> while CF-POP >BRANCH RESOLVE repeat
   CF-POP drop ;

\ ---- loops -------------------------------------------------------------------
: LOOP-OPEN ( vreg vreg bool -- )
   {: s:vreg l:vreg entry:bool :}
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

: LOOP-NEXT ( vreg -- ) HEAD SLOT LOOP-NEXT-XT @ execute ;

\ Under `+loop` the `?do` entry skips equal bounds only: a step may count down.
: LOOP-STEP ( vreg vreg -- )
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
   EMIT-LOOP-POP ;

;package
;package
