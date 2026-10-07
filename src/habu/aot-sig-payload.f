\ Stage signature rows, graph strings and registry bytes as one sidecar.
\ The ARM seed writer emits this buffer; its storage path is target neutral.
require src/habu/aot-decl.f

package AOT-SIG-PAYLOAD
using AOT-BUF
public

3 constant SEC-N
SEC-N 16 * 8 + constant TBL-BYTES
DYNAMIC-BUFFER STORAGE n
variable LEN
variable CUR

: BUF@ ( -- ptr u8 ) 0 STORAGE BYTE-VIEW ;

: U64! ( n n -- ) {: v:n at:n :}
   8 0 ?do  v i 8 * rshift $FF and  BUF@ at i + + c!  loop ;

: SEC-PTR ( n -- ptr u8 ) {: k:n :}
   k 0 = if AOT-SIG-BUF@ exit then
   k 1 = if AOT-SIG-STR-BUF@ exit then
   AOT-REG-BUF@ ;

: SEC-LEN ( n -- n ) {: k:n :}
   k 0 = if AOT-SIG-N @ SIG-ROW * exit then
   k 1 = if AOT-SIG-STR-LEN @ exit then
   AOT-REG-LEN @ ;

\ Nothing is published when the capture has no sidecar. The writer emits
\ this buffer in one run, so the offsets have no internal padding.
: BUILD ( -- )
   0 LEN !
   AOT-SIG-N @ 0= AOT-SIG-STR-LEN @ 0= and AOT-REG-LEN @ 0= and if exit then
   AOT-SECTION:PAYLOAD-BYTES CELL 1- + CELL / STORAGE-RESERVE
   SEC-N 0 U64!
   TBL-BYTES CUR !
   SEC-N 0 ?do
      CUR @  i 16 * 8 +  U64!
      i SEC-LEN  i 16 * 16 +  U64!
      CUR @ i SEC-LEN + CUR !
   loop
   TBL-BYTES CUR !
   SEC-N 0 ?do
      i SEC-LEN 0 > if
         i SEC-PTR  BUF@ CUR @ +  i SEC-LEN  BYTE-COPY
      then
      CUR @ i SEC-LEN + CUR !
   loop
   CUR @ LEN ! ;

;using
;package
