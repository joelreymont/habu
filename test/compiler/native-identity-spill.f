\ native-identity-spill.f - a tier-1 frame slot is never copied onto itself.
\
\ A local that stays live across a call is put in a frame slot, and every block
\ argument carrying it across a join is a copy between two values the coalescer
\ has already put in one class. The planner used to reload such a copy's operand
\ and store its result back, both naming the one slot that class owns, so each
\ live local cost an `ldr xN,[sp,#K]` standing directly in front of an
\ `str xN,[sp,#K]` at every join - 43,153 pairs in the baked engine and 9.3% of
\ the self-build (LESSONS.md 2026-09-16). A64RA plans neither row for a copy
\ whose ends share a slot and A64SPILL leaves the copy out of the rewrite: T4
\ below went from 112 instructions holding 16 such pairs to 80 holding none.
\
\ T4 is the reproducer that measurement named: two locals live across the first
\ call and three across the second and third. Its third guard cannot fire - c
\ and d are both `b HELP` - and is kept because the join it opens is one of the
\ three the pairs were counted at.
\
\ The span is read through the live dictionary (XREF-REC's start and exact code
\ length) and decoded as instruction words, so the case measures what the engine
\ really baked rather than a restatement of the planner's own decisions. The two
\ frame-access counts keep it from passing vacuously: a T4 that stopped using
\ the frame at all would hold no pairs either.

require lib/test.f
require src/habu/xref.f

1 set-tier

package IDENTITY-SPILL-SUBJECT
public
: HELP ( n -- n ) 1 + ;

: T4 ( n n -- n ) {: a:n b:n :}
   a 0 < if -1 throw then
   b HELP {: c:n :}
   c 0 < if -2 throw then
   b HELP {: d:n :}
   d 0 < if -3 throw then
   a b + c + d + ;
;package

package IDENTITY-SPILL
using IDENTITY-SPILL-SUBJECT

: NEG-A ( -- ) -1 4 T4 drop ;
: NEG-C ( -- ) 1 -5 T4 drop ;

\ ---- the emitted frame accesses ----------------------------------------------
\ ARM64 unsigned-offset frame access: ldr/str Xt,[Xn,#imm12*8]. A tier-1 routine
\ keeps its spill slots on sp, register 31, so a frame access is one of these two
\ forms over that base and the immediate names the slot.
$FFC00000 constant ACCESS-MASK
$F9400000 constant LDR-FORM
$F9000000 constant STR-FORM
31 constant SP-N

: W-AT ( ptr u8 n -- n ) {: p:ptr i:n :}
   i 4 * {: o:n :}
   p o + c@
   p o 1 + + c@ 8 lshift or
   p o 2 + + c@ 16 lshift or
   p o 3 + + c@ 24 lshift or ;

: BASE-OF ( n -- n )   5 rshift $1F and ;
: SLOT-OF ( n -- n )  10 rshift $FFF and ;

: FRAME-LDR? ( n -- bool ) {: w:n :}
   w ACCESS-MASK and LDR-FORM <> if false exit then
   w BASE-OF SP-N = ;

: FRAME-STR? ( n -- bool ) {: w:n :}
   w ACCESS-MASK and STR-FORM <> if false exit then
   w BASE-OF SP-N = ;

\ A reload of one slot standing directly in front of a store back into it.
: SAME-SLOT-PAIR? ( ptr u8 n -- bool ) {: p:ptr i:n :}
   p i W-AT {: w:n :}
   w FRAME-LDR? 0= if false exit then
   p i 1+ W-AT {: x:n :}
   x FRAME-STR? 0= if false exit then
   w SLOT-OF x SLOT-OF = ;

\ ---- one baked definition's own code -----------------------------------------
: SUBJECT ( ptr u8 n -- ptr n )
   XREF-FIND dup XREF-FOUND? TTRUE ;

: SPAN-BASE ( ptr n -- ptr u8 )
   XREF-START XREF-N>U8 ;

: SPAN-WORDS ( ptr n -- n )
   XREF-CODE-BYTES 4 / ;

: SPAN-FRAME-STORES ( ptr n -- n ) {: rec:ptr :}
   rec SPAN-BASE {: p:ptr :}
   0  rec SPAN-WORDS 0 ?do p i W-AT FRAME-STR? if 1+ then loop ;

: SPAN-FRAME-LOADS ( ptr n -- n ) {: rec:ptr :}
   rec SPAN-BASE {: p:ptr :}
   0  rec SPAN-WORDS 0 ?do p i W-AT FRAME-LDR? if 1+ then loop ;

: SPAN-PAIRS ( ptr n -- n ) {: rec:ptr :}
   rec SPAN-BASE {: p:ptr :}
   0  rec SPAN-WORDS 1- 0 ?do p i SAME-SLOT-PAIR? if 1+ then loop ;

: RUN ( -- )
   T-RESET
   s" every live local reaches the sum" T-LABEL
   3 4 T4 17 T=
   s" the guards pass on zero" T-LABEL
   0 0 T4 2 T=
   s" a below zero throws -1" T-LABEL
   ['] NEG-A -1 TTHROWS
   s" c below zero throws -2" T-LABEL
   ['] NEG-C -2 TTHROWS
   s" the subject really puts its locals in frame slots" T-LABEL
   s" IDENTITY-SPILL-SUBJECT:T4" SUBJECT SPAN-FRAME-STORES 3 >= TTRUE
   s" and reads them back out of those slots" T-LABEL
   s" IDENTITY-SPILL-SUBJECT:T4" SUBJECT SPAN-FRAME-LOADS 3 >= TTRUE
   s" no reload stands in front of a store back into its own slot" T-LABEL
   s" IDENTITY-SPILL-SUBJECT:T4" SUBJECT SPAN-PAIRS 0 T=
   T-REPORT ;

;using

' RUN
;package
execute
