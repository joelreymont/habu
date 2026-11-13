\ The native kernel object sections shared by both machine writers.
require src/lib/le.f
require src/lib/string.f
require src/core/bytes.f
require src/core/sha256.f
require src/habu/prims.f

package KERNEL-OBJECT
private

DYNAMIC-BUFFER OUT u8                \ the object being written
variable OUT-U
variable COUNT-AT                    \ the open section's count, as an offset in OUT
variable COUNT-N
SHA256-CTX-BYTES BUFFER: SHA-CTX


: U64+ ( n -- )
   OUT-U @ 8 + OUT-RESERVE
   0 OUT OUT-U @ + LE:U64!
   8 OUT-U +! ;

: SPAN+ ( ptr u8 n -- )
   {: a:ptr u:n :}
   u 7 + -8 and {: end:n :}
   OUT-U @ end + OUT-RESERVE
   a  0 OUT OUT-U @ +  u BYTE-COPY
   end u ?do 0  0 OUT OUT-U @ + i + c! loop
   end OUT-U +! ;

: NAME+ ( ptr u8 n -- ) dup U64+ SPAN+ ;


: SECTION ( -- ) OUT-U @ COUNT-AT !  0 COUNT-N !  0 U64+ ;
: ENTRY ( -- ) 1 COUNT-N +! ;
: ;SECTION ( -- ) COUNT-N @  0 OUT COUNT-AT @ + LE:U64! ;


: SYM+ ( ptr u8 n n -- )
   {: a:ptr u:n v:n :}
   a u NAME+  v U64+  ENTRY ;

: FLAG+ ( bool -- ) if 1 else 0 then U64+ ;

: CHECK-ROW+ ( n -- )
   {: row:n :}
   row PRIM-SPEC:KIND@ U64+
   row PRIM-SPEC:TRUSTED-ONLY? FLAG+
   row PRIM-SPEC:FIXED-ABI? FLAG+
   row PRIM-SPEC:C2-STOW-KIND U64+
   row PRIM-SPEC:NAME$ NAME+
   row PRIM-SPEC:PKG$ NAME+
   row PRIM-SPEC:CODE-LEN@ dup U64+
   0 ?do row i PRIM-SPEC:CODE@ U64+ loop ;

\ The table in its own order: an overloaded name's rows replay in it.
: CHECK-ROWS+ ( -- )
   OUT-U @ {: at:n :}
   0 U64+
   PRIM-SPEC:COUNT dup U64+
   0 ?do i CHECK-ROW+ loop
   OUT-U @ at - 8 -  0 OUT at + LE:U64! ;


: SUM ( -- )
   SHA-CTX  0 OUT 40 +  OUT-U @ 40 -  0 OUT 8 +  SHA256-IN ;

;package
