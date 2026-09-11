\ native-order-exit.f - a spilled memory loop reaches a no-return exit.

require src/compiler/native/compiler.f

package NATIVE-ORDER-EXIT
private

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

;package
