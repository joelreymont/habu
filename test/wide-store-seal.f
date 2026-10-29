\ wide-store-seal.f - wide ADT store seal regression.
\ The generated checked children use the zero-valued W=2 OK constructor: at the
\ first-cell boundary its payload would clear the seal latch if STR ran before
\ the guard, and at the later-cell boundary its tag would after the first
\ ordinary cell store. Exact ENGINE-ERROR:SEAL-VIOLATION catches a missing guard
\ or a write before the latch load, including loop iterations after the first.

require src/lib/errors.f
require src/lib/string.f
require src/lib/test.f
require src/lib/test/outcome.f
require src/lib/test/subject.f

$800 constant WSS-CAP
10000 constant WSS-TIMEOUT-MS

create WSS-OUT WSS-CAP allot
create WSS-ERR WSS-CAP allot

: WSS-LINE ( ptr u8 n -- )
   SB-APPEND
   $A SB-APPEND-C ;

: WSS-FAMILY ( -- )
   s" SUMTYPE wss-res<a,b>" WSS-LINE
   s"   VARIANT ok a ;VARIANT" WSS-LINE
   s"   VARIANT err b ;VARIANT" WSS-LINE
   s" ;SUMTYPE" WSS-LINE ;

: WSS-CONSTANTS ( -- )
   s" 37 constant WSS-PAYLOAD" WSS-LINE
   s" 90 constant WSS-E-PAYLOAD" WSS-LINE
   s" 91 constant WSS-E-TAG" WSS-LINE ;

: WSS-PRELUDE ( -- )
   WSS-FAMILY
   WSS-CONSTANTS
   s" : WSS-ZERO ( -- wss-res<n,n> ) 0 WSS--RES:OK ;" WSS-LINE ;

: WSS-ARM-LINE ( -- )
   S\" s\" WSS-ARMED\" type cr" WSS-LINE ;

: WSS-NORMAL-BODY ( -- )
   s" 1 LAYOUT-BUFFER WSS-MEM wss-res<n,n>" WSS-LINE
   s" : WSS-NORMAL ( -- )" WSS-LINE
   s"   WSS-PAYLOAD WSS--RES:OK 0 WSS-MEM ! ;" WSS-LINE
   WSS-ARM-LINE
   s" WSS-NORMAL" WSS-LINE ;

: WSS-NORMAL$ ( -- ptr u8 n )
   SB-RESET
   WSS-PRELUDE
   WSS-NORMAL-BODY
   SB$ ;

: WSS-FIRST-BODY ( -- )
   s" TRUSTED: WSS-FIRST-P ( -- ptr wss-res<n,n> ) data-base FRIEND-LATCH-CELL + ;" WSS-LINE
   s" : WSS-FIRST ( -- ) WSS-ZERO WSS-FIRST-P ! ;" WSS-LINE
   WSS-ARM-LINE
   s" WSS-FIRST" WSS-LINE ;

: WSS-FIRST$ ( -- ptr u8 n )
   SB-RESET
   WSS-PRELUDE
   WSS-FIRST-BODY
   SB$ ;

: WSS-LATER-BODY ( -- )
   s" TRUSTED: WSS-LATER-P ( -- ptr wss-res<n,n> )" WSS-LINE
   s"   data-base FRIEND-LATCH-CELL 1 cells - + ;" WSS-LINE
   s" : WSS-LATER ( -- ) WSS-ZERO WSS-LATER-P ! ;" WSS-LINE
   WSS-ARM-LINE
   s" WSS-LATER" WSS-LINE ;

: WSS-LATER$ ( -- ptr u8 n )
   SB-RESET
   WSS-PRELUDE
   WSS-LATER-BODY
   SB$ ;

\ The four transaction-band fixtures derive from the band's own ends, so they
\ follow the band when its extent changes. A W=2 store is 16 bytes: the lower
\ one starts one cell below TXN-STATE-OFF and crosses into it, the upper one
\ starts one cell below the band's end and crosses out, and the two neighbours
\ sit exactly clear on either side. A fixed address past the end can land in
\ STRING-ABI, which is writable, so a crossing that must trap would not.
: WSS-TXN-LOWER$ ( -- ptr u8 n )
   SB-RESET
   WSS-PRELUDE
   s" TRUSTED: WSS-TXN-P ( -- ptr wss-res<n,n> ) data-base TXN-STATE-OFF 1 cells - + ;" WSS-LINE
   s" : WSS-TXN ( -- ) WSS-ZERO WSS-TXN-P ! ;" WSS-LINE
   WSS-ARM-LINE
   s" WSS-TXN" WSS-LINE
   SB$ ;

: WSS-TXN-UPPER$ ( -- ptr u8 n )
   SB-RESET
   WSS-PRELUDE
   s" TRUSTED: WSS-TXN-P ( -- ptr wss-res<n,n> ) data-base TXN-STATE-OFF TXN-STATE-LEN + 1 cells - + ;" WSS-LINE
   s" : WSS-TXN ( -- ) WSS-ZERO WSS-TXN-P ! ;" WSS-LINE
   WSS-ARM-LINE
   s" WSS-TXN" WSS-LINE
   SB$ ;

: WSS-TXN-BEFORE$ ( -- ptr u8 n )
   SB-RESET
   WSS-PRELUDE
   s" TRUSTED: WSS-TXN-P ( -- ptr wss-res<n,n> ) data-base TXN-STATE-OFF 2 cells - + ;" WSS-LINE
   s" : WSS-TXN ( -- ) WSS-ZERO WSS-TXN-P ! ;" WSS-LINE
   WSS-ARM-LINE
   s" WSS-TXN" WSS-LINE
   SB$ ;

: WSS-TXN-AFTER$ ( -- ptr u8 n )
   SB-RESET
   WSS-PRELUDE
   s" TRUSTED: WSS-TXN-P ( -- ptr wss-res<n,n> ) data-base TXN-STATE-OFF TXN-STATE-LEN + + ;" WSS-LINE
   s" : WSS-TXN ( -- ) WSS-ZERO WSS-TXN-P ! ;" WSS-LINE
   WSS-ARM-LINE
   s" WSS-TXN" WSS-LINE
   SB$ ;

: WSS-RUN ( ptr u8 n -- len len outcome ) {: src:ptr srcu:n :}
   src srcu WSS-OUT WSS-CAP >LEN WSS-ERR WSS-CAP >LEN
   WSS-TIMEOUT-MS >MS SUBJECT:RUN ;

: WSS-EXPECT ( ptr u8 n n -- ) {: src:ptr srcu:n want:n :}
   src srcu WSS-RUN {: outu:len erru:len oc :}
   src srcu WSS-OUT outu LEN>N WSS-ERR erru LEN>N oc want T-OUTCOME-EXITED=
   WSS-OUT outu LEN>N s" WSS-ARMED" CONTAINS? TTRUE ;

: WSS-NORMAL-TEST ( -- )
   s" ordinary W=2 store remains green" T-LABEL
   WSS-NORMAL$ 0 WSS-EXPECT ;

: WSS-FIRST-TEST ( -- )
   s" first-cell protected intersection traps before mutation" T-LABEL
   WSS-FIRST$ ENGINE-ERROR:SEAL-VIOLATION WSS-EXPECT ;

: WSS-LATER-TEST ( -- )
   s" later-cell protected intersection traps before mutation" T-LABEL
   WSS-LATER$ ENGINE-ERROR:SEAL-VIOLATION WSS-EXPECT ;

: WSS-TXN-TEST ( -- )
   s" generated W=2 lower crossing traps through LPROTSPAN ABI" T-LABEL
   WSS-TXN-LOWER$ ENGINE-ERROR:SEAL-VIOLATION WSS-EXPECT
   s" generated W=2 upper crossing traps through LPROTSPAN ABI" T-LABEL
   WSS-TXN-UPPER$ ENGINE-ERROR:SEAL-VIOLATION WSS-EXPECT
   s" generated W=2 exact lower neighbor remains writable" T-LABEL
   WSS-TXN-BEFORE$ 0 WSS-EXPECT
   s" generated W=2 exact upper neighbor remains writable" T-LABEL
   WSS-TXN-AFTER$ 0 WSS-EXPECT ;

: WSS-MAIN ( -- )
   T-RESET
   WSS-NORMAL-TEST
   WSS-FIRST-TEST
   WSS-LATER-TEST
   WSS-TXN-TEST
   T-REPORT
   s" wide-store-seal: ok" type cr ;

WSS-MAIN
