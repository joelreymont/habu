\ native-order-exit.f - a spilled memory loop reaches a no-return exit.
require src/compiler/native/compiler.f
require src/lib/test.f

\ Tier 1: the bodies exist for the optimizing compiler's allocator, whose
\ spilled reload stands in a predecessor of the trap block.
1 set-tier

package NATIVE-ORDER-EXIT
private

\ Compilation must accept nonreturning cycles; the unbounded bodies are never run.
: FOREVER ( -- ) begin again ;
: FOREVER-DROP ( n -- ) drop begin again ;
: RETURN-OR-LOOP ( n -- n ) dup 0= if begin again then 1+ ;

: CHECK-RETURN ( -- )
   3 RETURN-OR-LOOP 4 <> if -2 throw then ;
CHECK-RETURN

: BYTE@ ( ptr u8 n -- n )
   BYTE+ c@ ;

: FAIL ( n -- )
   throw ;

: SCAN ( ptr u8 n -- n )
   {: a:ptr k:n :}
   0
   begin
      dup k <
   while
      a over BYTE@ drop
      1+
   repeat
   -1 FAIL ;

: SCAN-ABC ( -- ) s" abc" SCAN ;

: CHECK ( n -- )
   -1 <> if -2 throw then ;

' SCAN-ABC catch CHECK

\ The local crosses both calls and spills. On the last ELSE, its final reload
\ is in a predecessor of the trap block; the other arms return normally.
: MIX ( n -- n )
   dup 3 * over 5 xor + swap 7 and + dup 11 * + 13 xor ;

: CHOOSE ( n -- n )
   {: a:n :}
   a MIX 109 = if 1 else
      a MIX 185 = if 2 else -7191 throw then
   then ;
tier@ constant CHOOSE-TIER

T-RESET
s" the no-return exit subject was compiled at tier 1" T-LABEL
CHOOSE-TIER 1 T=

: CHECK-RETURNS ( -- )
   1 CHOOSE 1 <> if -2 throw then
   2 CHOOSE 2 <> if -2 throw then ;

: CHECK-TRAP ( n -- )
   -7191 <> if -2 throw then ;

CHECK-RETURNS
3 ' CHOOSE catch nip CHECK-TRAP

\ The zero-trip and exhausted edges have different frame histories; the trap
\ consumes neither. The early returning arm must still release its frame.
: EXHAUST ( n n -- ) {: k:n lim:n :}
   lim 0 ?do k MIX 109 = if unloop exit then loop -7192 throw ;

: CHECK-EXHAUST ( n -- )
   -7192 <> if -2 throw then ;

: EXHAUST-ZERO ( -- ) 2 0 EXHAUST ;
: EXHAUST-ALL ( -- ) 2 3 EXHAUST ;

1 1 EXHAUST
' EXHAUST-ZERO catch CHECK-EXHAUST
' EXHAUST-ALL catch CHECK-EXHAUST

\ A trap that reads the saved local still requires the incoming frame order.
: EXHAUST-READ ( n n -- ) {: k:n lim:n :}
   lim 0 ?do k MIX 109 = if unloop exit then loop
   k MIX drop -7192 throw ;

: EXHAUST-READ-ZERO ( -- ) 2 0 EXHAUST-READ ;
: EXHAUST-READ-ALL ( -- ) 2 3 EXHAUST-READ ;

1 1 EXHAUST-READ
' EXHAUST-READ-ZERO catch CHECK-EXHAUST
' EXHAUST-READ-ALL catch CHECK-EXHAUST

\ A loop-carried link crosses the comparison call and an early-return arm.
\ The comparison also returns early; both offset and rank order matter.
: NEXT-RECORD ( ptr n -- ptr ptr n ) 0 ptr-field ;
: NIL? ( ptr n -- bool ) BYTE-VIEW NULL$ drop = ;
: EDIT-OFF ( ptr n -- ptr n ) 4 cells + ;
: EDIT-RANK ( ptr n -- ptr n ) 6 cells + ;

: EDIT-BEFORE? ( ptr n ptr n -- bool )
   {: first second :}
   first EDIT-OFF @ second EDIT-OFF @ < if true exit then
   first EDIT-OFF @ second EDIT-OFF @ > if false exit then
   first EDIT-RANK @ second EDIT-RANK @ < ;

: INSERT-EDIT ( ptr n ptr ptr n -- )
   {: node list :}
   list begin dup @ NIL? 0= while
      dup @ node EDIT-BEFORE? 0= if
         dup @ node NEXT-RECORD ! node swap ! exit
      then @ NEXT-RECORD
   repeat node swap ! ;

PTR-VARIABLE HEAD
create FIRST-EDIT 7 cells allot
create MIDDLE-EDIT 7 cells allot
create LAST-EDIT 7 cells allot
create APPENDED-EDIT 7 cells allot
create TIED-EDIT 7 cells allot

: INIT-EDIT ( ptr n n n -- )
   {: node off:n rank:n :}
   NULL$ drop CELL-VIEW node NEXT-RECORD !
   off node EDIT-OFF ! rank node EDIT-RANK ! ;

: CHECK-EDIT ( ptr n n n -- ptr n )
   {: node off:n rank:n :}
   node EDIT-OFF @ off T=
   node EDIT-RANK @ rank T=
   node NEXT-RECORD @ ;

: CHECK-INSERT ( -- )
   NULL$ drop CELL-VIEW HEAD !
   FIRST-EDIT 1 1 INIT-EDIT MIDDLE-EDIT 3 1 INIT-EDIT
   LAST-EDIT 3 2 INIT-EDIT APPENDED-EDIT 4 1 INIT-EDIT
   TIED-EDIT 3 1 INIT-EDIT
   s" an empty list accepts its first node" T-LABEL
   LAST-EDIT HEAD INSERT-EDIT
   HEAD @ 3 2 CHECK-EDIT NIL? TTRUE
   s" insertion before the head returns early" T-LABEL
   FIRST-EDIT HEAD INSERT-EDIT
   HEAD @ 1 1 CHECK-EDIT 3 2 CHECK-EDIT NIL? TTRUE
   s" insertion after traversal orders equal offsets by rank" T-LABEL
   MIDDLE-EDIT HEAD INSERT-EDIT
   HEAD @ 1 1 CHECK-EDIT 3 1 CHECK-EDIT 3 2 CHECK-EDIT NIL? TTRUE
   s" append reaches the exhausted loop edge" T-LABEL
   APPENDED-EDIT HEAD INSERT-EDIT
   HEAD @ 1 1 CHECK-EDIT 3 1 CHECK-EDIT 3 2 CHECK-EDIT
      4 1 CHECK-EDIT NIL? TTRUE
   s" an equal offset and rank inserts before the existing node" T-LABEL
   TIED-EDIT HEAD INSERT-EDIT
   HEAD @ 1 1 CHECK-EDIT
      dup TIED-EDIT = TTRUE 3 1 CHECK-EDIT
      dup MIDDLE-EDIT = TTRUE 3 1 CHECK-EDIT 3 2 CHECK-EDIT
      4 1 CHECK-EDIT NIL? TTRUE ;

CHECK-INSERT

;package
T-REPORT
