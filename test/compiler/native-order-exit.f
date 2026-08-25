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

;package
