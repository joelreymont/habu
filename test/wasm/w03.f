\ w03.f - WASM-W03: W03's module, which test/wasm/link.f walks and
\ test/wasm/device.f validates and runs, and the bodies both files build.
\
\ BUILD makes a new link of W03's 136 functions: captured functions 0..134 take
\ k mod 17 lanes in and answer k / 17 out, each type its own, and each calls the
\ next; adapter 135, of no lanes, calls 0.

require lib/span.f
require src/arch/wasm/leb.f
require src/arch/wasm/link.f

package WASM-W03
private

\ A body is a code entry less its size: no local declared, the code, its end.
$100 constant BODY-CAP

public

BODY-CAP BUFFER: BODY-BUF
variable BODY-U

: B, ( n -- )
   BODY-BUF BODY-U @ + c!  1 BODY-U +! ;

: ROOM ( -- SPAN:span<u8> )
   BODY-BUF BODY-U @ +  BODY-CAP BODY-U @ -  SPAN:MAKE ;

: PAD5, ( n -- )  ROOM WLEB:U32-PAD! BODY-U +! ;

: BODY$ ( -- ptr u8 n )  BODY-BUF BODY-U @ ;

\ i32.const tag, then out zero lanes: a body of any lanes in.
: RET-BODY ( n n -- )
   {: tag:n out:n :}
   0 BODY-U !
   0 B,  $41 B, tag B,
   out 0 ?do  $42 B, 0 B,  loop
   $0B B, ;

\ ctx and in zero lanes, a call of index v answering out lanes, which are
\ dropped to its status, then mine zero lanes. The field is at 4 + 2 in.
: CALL-BODY ( n n n n -- )
   {: in:n out:n mine:n v:n :}
   0 BODY-U !
   0 B,  $20 B, 0 B,
   in 0 ?do  $42 B, 0 B,  loop
   $10 B, v PAD5,
   out 0 ?do  $1A B,  loop
   mine 0 ?do  $42 B, 0 B,  loop
   $0B B, ;

135 constant CHAIN

: IN-K ( n -- n )   17 mod ;
: OUT-K ( n -- n )  17 / ;

\ Function k's body, its call field holding v.
: CHAIN-BODY ( n n -- )
   {: k:n v:n :}
   k 1+ CHAIN < if
      k 1+ IN-K  k 1+ OUT-K  k OUT-K  v CALL-BODY
   else
      0 k OUT-K RET-BODY
   then ;

\ Each call field is added holding 0 and resolved by its CALL+.
: BUILD ( -- )
   WLINK:RESET
   CHAIN 0 do  i 0 CHAIN-BODY  BODY$ i IN-K i OUT-K 0 WLINK-ORIGIN:CAPTURED WLINK:FUNCTION+ drop  loop
   0 0 0 0 CALL-BODY  BODY$ 0 0 0 WLINK-ORIGIN:ADAPTER WLINK:FUNCTION+ drop
   CHAIN 1- 0 do  i  i 1+ IN-K 2 * 4 +  i 1+  WLINK:CALL+  loop
   CHAIN 4 0 WLINK:CALL+ ;

;package
