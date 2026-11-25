split-str
\ t0-place.f - unit placement for the tier 0 emitters, part PLACE of package
\ COMP.
\
\ An emitter appends the open unit's body to RAW and files each site it must
\ find again at its raw offset, HERE-OFF. CLOSE appends the placed unit to OUT:
\ the prologue, then the body with its insertions, each an offset and a length
\ in raw order. EPI+ inserts an epilogue, all of one length, before the way
\ out emitted next. BR+ inserts, after a branch's placeholder, the bytes the
\ branch's final form has beyond it.
\
\ A placed offset is the raw one plus the prologue and the insertions before
\ it, found by binary search over their prefix sums. At one offset a branch's
\ insertion comes before an epilogue. An instruction follows both (PLACED); a
\ label follows the branch's and lands on the epilogue (LANDED). A longer
\ branch can move another out of its form's reach, so CLOSE asks for every
\ length until none grows, and a length never shrinks.
\
\ Frame, lowest first: env cell, record cells, slots (REC-OFF, SLOT-OFF).
\
\ Who calls what. The seam's cells hold OPEN, QUOT-OPEN, UNIT-OPEN, CLOSE,
\ BYTES, SLOTS and REC-CELLS!. From OPEN or a unit's start until CLOSE the
\ emitter's operations write RAW, ENV and their branches' BR-FORM and BR-ARG,
\ and call the other words; after the last CLOSE they may write OUT and append
\ to it. The target's hooks run in this file only, UNIT-RESET at a unit's
\ start, ADR-PUT in QUOT-OPEN and CLOSE, and the rest in CLOSE.

require src/lib/heap.f
require src/lib/vector.f
require src/habu/t0-emit.f
require src/habu/t0-label.f

package COMP
package PLACE
public

\ ---- the target's hooks ------------------------------------------------------
\ UNIT-RESET clears the target's own state. PROLOGUE and EPILOGUE append the
\ code for a frame of n bytes. The rest write OUT at placed offsets: SELF-PUT
\ the `recurse` call at n to byte 0, ADR-PUT the quotation address at the first
\ n to the second, and BRANCH-PUT branch k ( k at to ). BRANCH-LEN ( k d )
\ answers the bytes branch k takes beyond its placeholder, d the distance from
\ the placeholder's end to the target.
ARCHES TYPED-BUFFER UNIT-RESET-XT [ -- ]
ARCHES TYPED-BUFFER PROLOGUE-XT [ n ^VEC:vector<u8> -- ]
ARCHES TYPED-BUFFER EPILOGUE-XT [ n ^VEC:vector<u8> -- ]
ARCHES TYPED-BUFFER SELF-PUT-XT [ n -- ]
ARCHES TYPED-BUFFER ADR-PUT-XT [ n n -- ]
ARCHES TYPED-BUFFER BRANCH-LEN-XT [ n n -- n ]
ARCHES TYPED-BUFFER BRANCH-PUT-XT [ n n n -- ]

\ ---- the emission ------------------------------------------------------------
\ The vectors are null until OPEN makes them.
TYPED-VARIABLE RAW ^VEC:vector<u8> SCRATCH-ONLY \ the open unit's body
TYPED-VARIABLE OUT ^VEC:vector<u8> SCRATCH-ONLY \ the closed units, BYTES
variable ENV                          \ 1 when this unit keeps a record's address
DYNAMIC-BUFFER BR-FORM n              \ the open unit's branches: the target's form,
DYNAMIC-BUFFER BR-ARG n               \ and the form's condition or register,
DYNAMIC-BUFFER BR-LEN n               \ and the bytes beyond the placeholder

private

TYPED-VARIABLE EPI ^VEC:vector<u8> SCRATCH-ONLY \ the open unit's epilogue, built at CLOSE
variable FRAME-CELLS                  \ machine-stack cells this unit uses
variable REC-CELLS                    \ this unit's record cells
variable BODY-AT                      \ OUT offset of the closing unit's raw offset 0
variable NUNIT                        \ units begun in this emission
variable NINS                         \ the open unit's insertions, in raw order:
DYNAMIC-BUFFER INS-AT n               \ raw offset, branch or -1 for an epilogue,
DYNAMIC-BUFFER INS-BR n               \ and the lengths of those before each, with
DYNAMIC-BUFFER INS-SUM n              \ all of them last
variable NPEND
DYNAMIC-BUFFER PEND-AT n              \ the open unit's address rows: raw first
DYNAMIC-BUFFER PEND-KIND n            \ byte, kind, target, origin
DYNAMIC-BUFFER PEND-TO n
DYNAMIC-BUFFER PEND-ORIGIN n
variable NPCALL
DYNAMIC-BUFFER PCALL-AT n             \ the open unit's call and tail rows: raw
DYNAMIC-BUFFER PCALL-KIND n           \ first byte, kind, target, loc
DYNAMIC-BUFFER PCALL-TO n
DYNAMIC-BUFFER PCALL-LOC n
variable NSELF
DYNAMIC-BUFFER SELF-AT n              \ the open unit's `recurse` sites, raw
DYNAMIC-BUFFER QUOT-SITE n            \ the address in OUT that names each unit
variable NBR
DYNAMIC-BUFFER BR-AT n                \ the open unit's branches: raw site, bytes
DYNAMIC-BUFFER BR-HOLE n              \ of placeholder, and raw target or -1 while
DYNAMIC-BUFFER BR-TO n                \ open

: SLOT ( -- n ) EMIT-SELECTED ;

: UNIT-RESET ( -- ) SLOT UNIT-RESET-XT @ execute ;
: PROLOGUE ( n ^VEC:vector<u8> -- ) SLOT PROLOGUE-XT @ execute ;
: EPILOGUE ( n ^VEC:vector<u8> -- ) SLOT EPILOGUE-XT @ execute ;
: SELF-PUT ( n -- ) SLOT SELF-PUT-XT @ execute ;
: ADR-PUT ( n n -- ) SLOT ADR-PUT-XT @ execute ;
: BRANCH-LEN ( n n -- n ) SLOT BRANCH-LEN-XT @ execute ;
: BRANCH-PUT ( n n n -- ) SLOT BRANCH-PUT-XT @ execute ;

: INS+ ( n n -- ) {: at:n br:n :}
   NINS @ {: k:n :}
   k 1 + INS-AT-RESERVE  k 1 + INS-BR-RESERVE
   at k INS-AT !  br k INS-BR !
   k 1 + NINS ! ;

public

: HERE-OFF ( -- n ) RAW @ VEC:COUNT ;
: OUT-OFF ( -- n ) OUT @ VEC:COUNT ;

\ ---- the open unit's sites ---------------------------------------------------
\ Each site is at the raw offset of its instruction's first byte.
: PEND+ ( n n n n -- ) {: at:n kind:n to:n origin:n :}
   NPEND @ {: k:n :}
   k 1 + PEND-AT-RESERVE  k 1 + PEND-KIND-RESERVE  k 1 + PEND-TO-RESERVE
   k 1 + PEND-ORIGIN-RESERVE
   at k PEND-AT !  kind k PEND-KIND !  to k PEND-TO !  origin k PEND-ORIGIN !
   k 1 + NPEND ! ;

: PCALL+ ( n n n n -- ) {: at:n kind:n to:n loc:n :}
   NPCALL @ {: k:n :}
   k 1 + PCALL-AT-RESERVE  k 1 + PCALL-KIND-RESERVE
   k 1 + PCALL-TO-RESERVE  k 1 + PCALL-LOC-RESERVE
   at k PCALL-AT !  kind k PCALL-KIND !  to k PCALL-TO !  loc k PCALL-LOC !
   k 1 + NPCALL ! ;

: SELF+ ( n -- ) {: at:n :}
   NSELF @ {: k:n :}
   k 1 + SELF-AT-RESERVE
   at k SELF-AT !
   k 1 + NSELF ! ;

\ The epilogue goes in before raw offset n, which is HERE-OFF.
: EPI+ ( n -- ) -1 INS+ ;

\ A branch of the form and its argument to raw offset to, -1 until RESOLVE,
\ with a placeholder of hole bytes, above 0, appended for BRANCH-PUT to write.
: BR+ ( n n n n -- LABEL:branch ) {: form:n arg:n to:n hole:n :}
   NBR @ {: k:n :}
   k 1 + BR-AT-RESERVE  k 1 + BR-HOLE-RESERVE  k 1 + BR-TO-RESERVE
   k 1 + BR-FORM-RESERVE  k 1 + BR-ARG-RESERVE  k 1 + BR-LEN-RESERVE
   HERE-OFF k BR-AT !  hole k BR-HOLE !  to k BR-TO !
   form k BR-FORM !  arg k BR-ARG !  0 k BR-LEN !
   k 1 + NBR !
   RAW @ HERE-OFF hole + VEC:ENSURE
   HERE-OFF k INS+
   k LABEL:>BRANCH ;

: RESOLVE ( LABEL:branch -- ) HERE-OFF swap LABEL:BRANCH>N BR-TO ! ;

: MARK ( -- LABEL:head ) HERE-OFF LABEL:>HEAD ;

\ ---- the frame ---------------------------------------------------------------
: SLOTS ( n -- ) {: k:n :}
   k FRAME-CELLS @ > if k FRAME-CELLS ! then ;

\ The offset from the stack pointer of record cell n, and of slot k, in the
\ frame CLOSE sizes.
: REC-OFF ( n -- n ) ENV @ + CELL * ;
: SLOT-OFF ( n -- n ) REC-CELLS @ + REC-OFF ;

\ A record or env cell placed after the body began would move the offsets
\ already emitted.
: BODY-EMPTY ( -- ) HERE-OFF 0 <> if E-T0-UNIT throw then ;

: REC-CELLS! ( n -- ) BODY-EMPTY  REC-CELLS ! ;

private

: FRAME-BYTES ( -- n )
   ENV @ REC-CELLS @ + FRAME-CELLS @ + CELL * 15 + 15 invert and ;

\ ---- the insertions ----------------------------------------------------------
: INS-LEN ( n -- n ) INS-BR @ {: br:n :}
   br 0 < if EPI @ VEC:COUNT else br BR-LEN @ then ;

\ Orders the insertions: by offset, a branch's before an epilogue.
: INS-KEY ( n -- n ) {: k:n :}
   k INS-AT @ 2 *  k INS-BR @ 0 < if 1 + then ;

\ How many insertions have a key below n.
: INS-BELOW ( n -- n ) {: key:n :}
   0 NINS @
   begin 2dup < while
      2dup + 1 rshift {: mid:n :}
      mid INS-KEY key < if nip mid 1 + swap else drop mid then
   repeat drop ;

: SUMS ( -- )
   NINS @ {: n:n :}
   n 1 + INS-SUM-RESERVE
   0 n 0 ?do dup i INS-SUM !  i INS-LEN + loop
   n INS-SUM ! ;

\ Where the instruction at raw offset x lands in OUT, and where a branch to x
\ lands.
: PLACED ( n -- n ) {: x:n :}
   BODY-AT @ x +  x 1 + 2 * INS-BELOW INS-SUM @ + ;

: LANDED ( n -- n ) {: x:n :}
   BODY-AT @ x +  x 2 * 1 + INS-BELOW INS-SUM @ + ;

: BR-DIST ( n -- n ) {: k:n :}
   k BR-TO @ LANDED  k BR-AT @ PLACED k BR-HOLE @ +  - ;

: GROW ( n -- bool ) {: k:n :}
   k  k BR-DIST  BRANCH-LEN {: len:n :}
   len k BR-LEN @ > dup if len k BR-LEN ! then ;

: GROWN? ( -- bool ) false NBR @ 0 ?do i GROW or loop ;

: SIZE ( -- ) begin SUMS GROWN? 0= until ;

: COPY ( n n -- ) {: from:n to:n :}
   RAW @ VEC:BYTES drop from +  to from -  OUT @ VEC:APPEND ;

\ BRANCH-PUT writes over a branch's placeholder and its insertion.
: INSERT ( n -- ) {: k:n :}
   k INS-BR @ 0 < if EPI @ VEC:BYTES OUT @ VEC:APPEND exit then
   OUT @ OUT-OFF k INS-LEN + VEC:ENSURE ;

: COPY-BODY ( -- )
   0 NINS @ 0 ?do
      i INS-LEN 0 <> if i INS-AT @ COPY  i INSERT  i INS-AT @ then
   loop
   HERE-OFF COPY ;

\ ---- the closed unit's sites -------------------------------------------------
: FILE-ADDR ( n n n n -- ) {: at:n kind:n to:n origin:n :}
   kind ADDR-QUOT-KIND = if
      \ Unit 0 is the emission's entry, at OUT's start, and no QUOT-OPEN
      \ opens it.
      to 0= if at 0 ADR-PUT exit then
      to 1 + QUOT-SITE-RESERVE  at to QUOT-SITE ! exit
   then
   at kind to origin EMIT-ADDR-ROW+ ;

: FILE-ADDRS ( -- )
   NPEND @ 0 ?do
      i PEND-AT @ PLACED  i PEND-KIND @  i PEND-TO @  i PEND-ORIGIN @
      FILE-ADDR
   loop ;

: FILE-CALLS ( -- )
   NPCALL @ 0 ?do
      i PCALL-AT @ PLACED  i PCALL-KIND @  i PCALL-TO @  i PCALL-LOC @
      EMIT-CALL-ROW+
   loop ;

: PUT-SELVES ( -- ) NSELF @ 0 ?do i SELF-AT @ PLACED SELF-PUT loop ;

: PUT-BRANCHES ( -- )
   NBR @ 0 ?do i  i BR-AT @ PLACED  i BR-TO @ LANDED  BRANCH-PUT loop ;

: UNIT-BEGIN ( -- )
   RAW @ 0 VEC:RESIZE
   OUT-OFF EMIT-UNIT+
   NUNIT @ 1 + NUNIT !
   0 FRAME-CELLS !  0 REC-CELLS !  0 ENV !  0 NINS !
   0 NPEND !  0 NPCALL !  0 NSELF !  0 NBR !
   UNIT-RESET ;

: EMPTY ( -- ^VEC:vector<u8> ) HEAP:ALLOCATOR 0 [type] u8 VEC:NEW ;

public

\ ---- the units ---------------------------------------------------------------
: OPEN ( -- )
   RAW @ NULL-PTR = if
      EMPTY RAW !  EMPTY OUT !  EMPTY EPI !
   then
   OUT @ 0 VEC:RESIZE
   0 NUNIT !
   LABEL:RESET
   UNIT-BEGIN ;

\ Point unit n's address, in a closed unit of OUT, at the offset the unit
\ begins at.
: QUOT-OPEN ( n -- )
   {: n:n :}
   n NUNIT @ <> if E-T0-UNIT throw then
   n QUOT-SITE @ OUT-OFF ADR-PUT
   UNIT-BEGIN ;

: UNIT-OPEN ( -- ) UNIT-BEGIN ;

: CLOSE ( -- )
   FRAME-BYTES {: bytes:n :}
   bytes OUT @ PROLOGUE
   OUT-OFF BODY-AT !
   EPI @ 0 VEC:RESIZE
   bytes EPI @ EPILOGUE
   SIZE COPY-BODY
   FILE-ADDRS FILE-CALLS PUT-SELVES PUT-BRANCHES ;

: BYTES ( -- str ) OUT @ VEC:BYTES ;

;package
;package
