\ native-arm-frame-order.f - the frame order across arms that never return.
\
\ An arm that throws reaches no frame access and no return, and neither does the
\ block that dispatches to it. Spill rewriting used to hand that dispatch block a
\ synthesized frame-order argument nothing ever reads, and the verifier refused
\ the module with E-A64RAV-ORDER; src/core/type-family.f PF-LAYOUT-REQUIRE and
\ lib/xml/scalar.f SCALAR-AT are the two shapes the tree carried. Each
\ definition below failed to compile at tier 1 or is a neighbour that did not.
\
\ THE NEIGHBOURS ARE THE POINT. An arm order, a missing default, a selector that
\ is not a call, and a throwing arm that READS the spilled local each still need
\ the lane, so a rewrite that withheld lanes too eagerly fails them instead.
\ Tier 1 first: the synthesized frame-order argument and the E-A64RAV-ORDER
\ refusal these definitions reproduce are the optimizing compiler's spill
\ rewriting; at the default tier nothing below is rewritten at all.
1 set-tier

require lib/test.f

package NARM
public

SUMTYPE step 0
   VARIANT ok n n ;VARIANT
   VARIANT raw n n ;VARIANT
;SUMTYPE

private

7401 constant E-ARM

: POLICY ( n -- n ) {: v:n :} v 3 and ;
\ A rejecting check after the MATCH, as REQUIRE-SCALAR after scalar.f's MATCH.
: REQ ( n -- ) {: v:n :} v 0 < if E-ARM throw then ;

: STEP ( n n -- step ) {: a:n b:n :}
   a 0= if a b NARM-STEP:RAW exit then
   a b NARM-STEP:OK ;

\ The reduced PF-LAYOUT-REQUIRE: a local live across the selector call, one arm
\ that returns it, one arm that throws, and a default that throws.
: ARMS ( n n -- n )
   {: sel:n kept:n :}
   sel POLICY CASE
      0 OF kept ENDOF
      1 OF E-ARM throw ENDOF
      E-ARM throw
   ENDCASE ;

\ The throwing arm READS the spilled local, so its block does need the lane.
: ARMS-RELOAD ( n n -- n )
   {: sel:n kept:n :}
   sel POLICY CASE
      0 OF kept ENDOF
      1 OF kept E-ARM + throw ENDOF
      E-ARM throw
   ENDCASE ;

: ARMS-THROW-FIRST ( n n -- n )
   {: sel:n kept:n :}
   sel POLICY CASE
      0 OF E-ARM throw ENDOF
      1 OF kept ENDOF
      E-ARM throw
   ENDCASE ;

\ No default: the dispatch block falls through to the return, which needs the
\ lane on that path.
: ARMS-NO-DEFAULT ( n n -- )
   {: sel:n kept:n :}
   sel POLICY CASE
      0 OF kept drop ENDOF
      1 OF E-ARM throw ENDOF
   ENDCASE ;

: ARMS-NO-CALL ( n n -- n )
   {: sel:n kept:n :}
   sel CASE
      0 OF kept ENDOF
      1 OF E-ARM throw ENDOF
      E-ARM throw
   ENDCASE ;

: ARMS-DEAD-LOCAL ( n n -- n )
   {: sel:n kept:n :}
   kept POLICY {: early:n :}
   sel POLICY CASE
      0 OF early ENDOF
      1 OF E-ARM throw ENDOF
      E-ARM throw
   ENDCASE ;

: ARMS-ONE ( n n -- n )
   {: sel:n kept:n :}
   sel POLICY CASE
      0 OF kept ENDOF
      E-ARM throw
   ENDCASE ;

: ARMS-IF ( n n -- n )
   {: sel:n kept:n :}
   sel POLICY 0= if kept exit then
   E-ARM throw ;

\ The SCALAR-AT shape: a MATCH over a call, one arm passing its payload on and
\ one arm throwing, with the locals of the caller live across the call.
: STEP-AT ( n n n -- n n )
   {: a:n b:n c:n :}
   a b + c +
   a b STEP MATCH step
      ok OF ENDOF
      raw OF 2drop E-ARM throw ENDOF
   ;MATCH
   {: total:n x:n y:n :}
   x REQ
   x y + total + y ;


\ ---- the frame need over a long chain of arms ---------------------------------
\ The need for a frame order runs BACKWARD along the edges from the blocks that
\ reach the frame, so WHERE the spills are decides how far it travels. Here the
\ arms come first and touch no frame, and the live set that spills is behind all
\ of them, so each arm block learns its need only from the block after it. That is
\ the shape whose fixpoint cost one pass over every block: measured 25 passes over
\ 35 blocks while F-NEED-PASS visited ascending, 3 once it visited descending. A
\ wrong visit order shows up here as a wrong answer, not as a slow one.
: NEED-TAIL ( n -- n )
   dup 5 < if 1 + then dup 5 < if 2 + then dup 5 < if 3 + then
   dup 5 < if 4 + then dup 5 < if 5 + then dup 5 < if 6 + then
   dup 5 < if 7 + then dup 5 < if 8 + then dup 5 < if 9 + then
   dup 5 < if 10 + then dup 5 < if 11 + then dup 5 < if 12 + then
   dup 5 < if 13 + then dup 5 < if 14 + then dup 5 < if 15 + then
   dup 5 < if 16 + then dup 5 < if 17 + then dup 5 < if 18 + then
   dup 5 < if 19 + then dup 5 < if 20 + then dup 5 < if 21 + then
   dup 5 < if 22 + then dup 5 < if 23 + then dup 5 < if 24 + then
   dup 1 + swap dup 2 + swap dup 3 + swap dup 4 + swap dup 5 + swap
   dup 6 + swap dup 7 + swap dup 8 + swap dup 9 + swap dup 10 + swap
   dup 11 + swap dup 12 + swap dup 13 + swap dup 14 + swap dup 15 + swap
   dup 16 + swap dup 17 + swap dup 18 + swap dup 19 + swap dup 20 + swap
   dup 21 + swap dup 22 + swap dup 23 + swap dup 24 + swap dup 25 + swap
   dup 26 + swap dup 27 + swap dup 28 + swap
   + + + + + + + + + + + + + + + + + + + + + + + + + + + + ;

\ The same two ingredients the other way round, at the k the spill-frame family
\ of tools/chain-scale.f uses: the live set that spills first, the arms behind it.
\ The plan then puts a store or a reload in nearly every block, so the need is
\ seeded almost everywhere and converges in three passes whichever way the blocks
\ are visited - the neighbour that keeps the reorder an optimisation rather than a
\ change of answer.
: NEED-HEAD ( n -- n )
   dup 1 + swap dup 2 + swap dup 3 + swap dup 4 + swap dup 5 + swap
   dup 6 + swap dup 7 + swap dup 8 + swap dup 9 + swap dup 10 + swap
   dup 11 + swap dup 12 + swap dup 13 + swap dup 14 + swap dup 15 + swap
   dup 16 + swap dup 17 + swap dup 18 + swap dup 19 + swap dup 20 + swap
   dup 21 + swap dup 22 + swap dup 23 + swap dup 24 + swap dup 25 + swap
   dup 26 + swap dup 27 + swap dup 28 + swap
   dup 5 < if 1 + then dup 5 < if 2 + then dup 5 < if 3 + then
   dup 5 < if 4 + then
   + + + + + + + + + + + + + + + + + + + + + + + + + + + + ;

public

: CASES ( -- )
   s" a returning arm beside a throwing arm and a throwing default" T-LABEL
   0 77 ARMS 77 T=
   [: 1 5 ARMS drop ;] E-ARM TTHROWSQ
   [: 2 5 ARMS drop ;] E-ARM TTHROWSQ

   s" a throwing arm that reads the spilled local still gets its lane" T-LABEL
   0 77 ARMS-RELOAD 77 T=
   [: 1 5 ARMS-RELOAD drop ;] E-ARM 5 + TTHROWSQ

   s" the throwing arm first" T-LABEL
   1 77 ARMS-THROW-FIRST 77 T=
   [: 0 5 ARMS-THROW-FIRST drop ;] E-ARM TTHROWSQ

   s" a dispatch that falls through to the return keeps its lane" T-LABEL
   0 77 ARMS-NO-DEFAULT
   [: 1 5 ARMS-NO-DEFAULT ;] E-ARM TTHROWSQ

   s" a selector that is not a call" T-LABEL
   0 77 ARMS-NO-CALL 77 T=
   [: 2 5 ARMS-NO-CALL drop ;] E-ARM TTHROWSQ

   s" a local read before the selector call" T-LABEL
   0 77 ARMS-DEAD-LOCAL 1 T=
   [: 1 5 ARMS-DEAD-LOCAL drop ;] E-ARM TTHROWSQ

   s" one arm and a throwing default" T-LABEL
   0 77 ARMS-ONE 77 T=
   [: 1 5 ARMS-ONE drop ;] E-ARM TTHROWSQ

   s" the same two arms as IF and THEN" T-LABEL
   0 77 ARMS-IF 77 T=
   [: 1 5 ARMS-IF drop ;] E-ARM TTHROWSQ

   s" a MATCH arm that throws beside one that passes its payload on" T-LABEL
   1 2 3 STEP-AT 2 T= 9 T=
   [: 0 2 3 STEP-AT 2drop ;] E-ARM TTHROWSQ

   s" a frame need that travels a chain of arms backwards" T-LABEL
   0 NEED-TAIL 580 T=

   s" the spill-frame shape, arms behind the live set" T-LABEL
   0 NEED-HEAD 412 T= ;

;package

T-RESET NARM:CASES T-REPORT
