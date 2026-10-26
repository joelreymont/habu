\ tier0-compare-branch.f - tier 0 branches on a compare's flags when the
\ compare's flag is the register top at `if`, `while` or `until`.
\
\ A compare emits `cmp; cset; neg` (src/habu/jit.f VCMP-ENTRY). When the branch
\ keyword finds those two words last in the code and no open loop top recorded
\ at or past them, LVFUSE rewinds them and the branch is one b.cond instead of
\ a cbz on the flag. This file pins both halves through the real load path
\ (ARM's default tier is 0): the fused subjects hold a b.cond and no cset, the
\ refused ones (a loop top between the compare and the branch, a flag that is
\ copied, stored or returned, a float compare) keep the cset, and every
\ subject answers both outcomes of its condition.
\
\ Spans come from the DICTIONARY RECORD (src/habu/xref.f), as in
\ test/compiler/tier0-inline-prims.f.

require src/lib/test.f
require src/lib/errors.f
require src/habu/xref.f
require src/habu/code-bytes.f

package TIER0-COMPARE-BRANCH
private

$FF000010 constant BCOND-MASK   $54000000 constant BCOND-OP    \ b.cond
$FFFF0FE0 constant CSET-MASK    $9A9F07E0 constant CSET-OP     \ cset xR,<any>
$FFE0FFE0 constant NEG-MASK     $CB0003E0 constant NEG-OP      \ neg xR,xM
$FF000000 constant CBZ-MASK     $B4000000 constant CBZ-OP      \ cbz xR
$FFE0FFFF constant MOV17-MASK   $AA0003F1 constant MOV17-OP    \ mov x17,xM

\ CODE-BYTES:AT is the bounded view between an engine code address and a
\ readable pointer; this file adds none of its own.
: CODE@ ( n -- n )
   4 CODE-BYTES:AT drop {: p:ptr :}
   p c@
   p 1+ c@ 8 lshift or
   p 2 + c@ 16 lshift or
   p 3 + c@ 24 lshift or ;

variable MISSING                     \ names the dictionary did not answer for

: SPAN ( ptr u8 n -- n n ) {: na:ptr nu:n :}
   na nu XREF-FIND {: rec:ptr :}
   rec XREF-FOUND? 0= if
      1 MISSING +!
      s" tier0-compare-branch: no record for " type na nu type cr
      0 0 exit
   then
   rec XREF-START dup rec XREF-CODE-BYTES + ;

\ How many of the named word's instructions match op under mask.
: OPS ( ptr u8 n n n -- n ) {: na:ptr nu:n mask:n op:n :}
   na nu SPAN {: lo:n hi:n :}
   0
   hi lo ?do
      i CODE@ mask and op = if 1+ then
   4 +loop ;

: BCONDS ( ptr u8 n -- n )  BCOND-MASK BCOND-OP OPS ;
: CSETS ( ptr u8 n -- n )  CSET-MASK CSET-OP OPS ;
: NEGS ( ptr u8 n -- n )  NEG-MASK NEG-OP OPS ;
: CBZS ( ptr u8 n -- n )  CBZ-MASK CBZ-OP OPS ;
: MOV17S ( ptr u8 n -- n )  MOV17-MASK MOV17-OP OPS ;

variable CB-K                        \ CB-NEXT?'s countdown
variable CB-V                        \ CB-STORE's cell

public

\ ---- the subjects ------------------------------------------------------------
: CB-LT ( n n -- n ) < if 1 else 2 then ;
: CB-UNTIL ( n -- n ) begin 1- dup 0 = until ;
: CB-ZEQ ( n -- n ) begin 1- dup 0= until ;
: CB-WHILE ( n -- n ) begin dup 0 > while 1- repeat ;
\ ( acc i j ): the inner loop adds j to acc, the outer runs it i times.
: CB-NEST ( n -- n )
   0 swap begin dup 0 > while
      3 begin dup 0 > while rot 1+ rot rot 1- repeat drop
   1- repeat drop ;

: CB-NEXT? ( -- bool ) -1 CB-K +!  CB-K @ 0 > ;
\ The flag is on the stack at `begin`, so the loop top sits between the
\ compare and `while`: rewinding the compare would move the loop top.
: CB-FENCE ( n n -- ) < begin while CB-NEXT? repeat ;
: CB-DUP ( n n -- n ) < dup if drop 1 else drop 2 then ;
: CB-STORE ( n n -- ) < CB-V ! ;
: CB-FLAG ( n n -- bool ) < ;
: CB-FLT ( r r -- n ) f< if 1 else 2 then ;

private

: TEST-FUSED ( -- )
   s" a compare's flag at if, until and while becomes one b.cond" T-LABEL
   s" TIER0-COMPARE-BRANCH:CB-LT" BCONDS 1 T=
   s" TIER0-COMPARE-BRANCH:CB-LT" CSETS 0 T=
   s" TIER0-COMPARE-BRANCH:CB-LT" NEGS 0 T=
   s" TIER0-COMPARE-BRANCH:CB-LT" CBZS 0 T=
   s" TIER0-COMPARE-BRANCH:CB-UNTIL" BCONDS 1 T=
   s" TIER0-COMPARE-BRANCH:CB-UNTIL" CSETS 0 T=
   s" TIER0-COMPARE-BRANCH:CB-UNTIL" MOV17S 0 T=
   s" TIER0-COMPARE-BRANCH:CB-ZEQ" BCONDS 1 T=
   s" TIER0-COMPARE-BRANCH:CB-ZEQ" CSETS 0 T=
   s" TIER0-COMPARE-BRANCH:CB-ZEQ" MOV17S 0 T=
   s" TIER0-COMPARE-BRANCH:CB-WHILE" BCONDS 1 T=
   s" TIER0-COMPARE-BRANCH:CB-WHILE" CSETS 0 T=
   s" TIER0-COMPARE-BRANCH:CB-WHILE" CBZS 0 T=
   s" TIER0-COMPARE-BRANCH:CB-NEST" BCONDS 2 T=
   s" TIER0-COMPARE-BRANCH:CB-NEST" CSETS 0 T=
   s" TIER0-COMPARE-BRANCH:CB-NEST" CBZS 0 T= ;

: TEST-KEPT ( -- )
   s" a loop top after the compare, a used flag or a float compare keeps the cset" T-LABEL
   s" TIER0-COMPARE-BRANCH:CB-FENCE" CSETS 1 T=
   s" TIER0-COMPARE-BRANCH:CB-FENCE" CBZS 1 T=
   s" TIER0-COMPARE-BRANCH:CB-DUP" CSETS 1 T=
   s" TIER0-COMPARE-BRANCH:CB-DUP" BCONDS 0 T=
   s" TIER0-COMPARE-BRANCH:CB-STORE" CSETS 1 T=
   s" TIER0-COMPARE-BRANCH:CB-FLAG" CSETS 1 T=
   s" TIER0-COMPARE-BRANCH:CB-FLT" BCONDS 0 T=
   s" TIER0-COMPARE-BRANCH:CB-FLT" CBZS 1 T= ;

: TEST-VALUES ( -- )
   s" every subject answers both outcomes of its condition" T-LABEL
   3 4 CB-LT 1 T=
   4 3 CB-LT 2 T=
   3 3 CB-LT 2 T=
   -1 0 CB-LT 1 T=
   3 CB-UNTIL 0 T=
   1 CB-UNTIL 0 T=
   3 CB-ZEQ 0 T=
   1 CB-ZEQ 0 T=
   3 CB-WHILE 0 T=
   -2 CB-WHILE -2 T=
   4 CB-NEST 12 T=
   0 CB-NEST 0 T=
   3 CB-K !  1 2 CB-FENCE  CB-K @ 0 T=
   3 CB-K !  2 1 CB-FENCE  CB-K @ 3 T=
   3 4 CB-DUP 1 T=
   4 3 CB-DUP 2 T=
   3 4 CB-STORE  CB-V @ -1 T=
   4 3 CB-STORE  CB-V @ 0 T=
   3 4 CB-FLAG TTRUE
   4 3 CB-FLAG TFALSE
   1.0 2.0 CB-FLT 1 T=
   2.0 1.0 CB-FLT 2 T= ;

public

: RUN ( -- )
   T-RESET
   0 MISSING !
   TEST-FUSED
   TEST-KEPT
   TEST-VALUES
   MISSING @ 0 T=
   T-REPORT ;

;package

TIER0-COMPARE-BRANCH:RUN
