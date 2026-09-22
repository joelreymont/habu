\ native-order-exit.f - a spilled memory loop reaches a no-return exit.
\ Tier 1 first: the unbounded bodies below are never run; they exist to be
\ accepted by the optimizing compiler's allocator, whose spilled reload stands
\ in a predecessor of the trap block.
1 set-tier

require src/compiler/native/compiler.f

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

: CHECK ( ptr u8 n n -- )
   >r 2drop r>
   -1 <> if -2 throw then ;

s" abc" ' SCAN catch CHECK

\ The local crosses both calls and spills. On the last ELSE, its final reload
\ is in a predecessor of the trap block; the other arms return normally.
: MIX ( n -- n )
   dup 3 * over 5 xor + swap 7 and + dup 11 * + 13 xor ;

: CHOOSE ( n -- n )
   {: a:n :}
   a MIX 109 = if 1 else
      a MIX 185 = if 2 else -7191 throw then
   then ;

: CHECK-RETURNS ( -- )
   1 CHOOSE 1 <> if -2 throw then
   2 CHOOSE 2 <> if -2 throw then ;

: CHECK-TRAP ( n n -- )
   nip -7191 <> if -2 throw then ;

CHECK-RETURNS
3 ' CHOOSE catch CHECK-TRAP

\ The zero-trip and exhausted edges have different frame histories; the trap
\ consumes neither. The early returning arm must still release its frame.
: EXHAUST ( n n -- ) {: k:n lim:n :}
   lim 0 ?do k MIX 109 = if unloop exit then loop -7192 throw ;

: CHECK-EXHAUST ( n n n -- )
   >r 2drop r> -7192 <> if -2 throw then ;

1 1 EXHAUST
2 0 ' EXHAUST catch CHECK-EXHAUST
2 3 ' EXHAUST catch CHECK-EXHAUST

\ A trap that reads the saved local still requires the incoming frame order.
: EXHAUST-READ ( n n -- ) {: k:n lim:n :}
   lim 0 ?do k MIX 109 = if unloop exit then loop
   k MIX drop -7192 throw ;

1 1 EXHAUST-READ
2 0 ' EXHAUST-READ catch CHECK-EXHAUST
2 3 ' EXHAUST-READ catch CHECK-EXHAUST

;package
