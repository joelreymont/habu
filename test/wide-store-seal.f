\ wide-store-seal.f - wide ADT store seal regression.
\
\ The generated checked children use the zero-valued W=2 OK constructor.  At
\ the first-cell boundary its payload would clear the seal latch if STR ran
\ before the guard.  At the later-cell boundary its tag would do the same after
\ the first ordinary cell store.  Exact E-SEAL-VIOLATION catches a missing guard
\ or a write before the latch load; the TLP-STORE2-G lowering golden pins STR
\ after the remaining range guard.  Together they prove the protected cell is
\ unchanged, including on loop iterations after the first.

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/test/outcome.f
require lib/memory.f
require lib/fs.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f

$800 constant WSS-CAP
10000 constant WSS-TIMEOUT-MS

create WSS-OUT WSS-CAP allot
create WSS-ERR WSS-CAP allot

variable WSS-OUT-U

: WSS-LINE ( ptr u8 n -- )
   SB-APPEND
   $A SB-APPEND-C ;

: WSS-HB$ ( -- ptr u8 n )
   s" HABU_UNDER_TEST" >LEN PROC-ENV-DEFAULT$? if LEN>N exit then
   2drop
   s" HABU_UNDER_TEST" GETENV dup 0= if
      2drop s" bin/hb" exit
   then ;

: WSS-FAMILY ( -- )
   s" SUMTYPE wss-res 2" WSS-LINE
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
   s" create WSS-MEM 2 cells allot" WSS-LINE
   s" : WSS-MEM-P ( -- ptr wss-res<n,n> ) WSS-MEM ;" WSS-LINE
   s" : WSS-NORMAL ( -- )" WSS-LINE
   s"   WSS-PAYLOAD WSS--RES:OK WSS-MEM-P !" WSS-LINE
   s"   WSS-MEM @ WSS-PAYLOAD <> if WSS-E-PAYLOAD throw then" WSS-LINE
   s"   WSS-MEM cell+ @ 0 <> if WSS-E-TAG throw then ;" WSS-LINE
   WSS-ARM-LINE
   s" WSS-NORMAL" WSS-LINE ;

: WSS-NORMAL$ ( -- ptr u8 n )
   SB-RESET
   WSS-PRELUDE
   WSS-NORMAL-BODY
   SB$ ;

: WSS-FIRST-BODY ( -- )
   s" : WSS-FIRST-P ( -- ptr wss-res<n,n> ) data-base FRIEND-LATCH-CELL + ;" WSS-LINE
   s" : WSS-FIRST ( -- ) WSS-ZERO WSS-FIRST-P ! ;" WSS-LINE
   WSS-ARM-LINE
   s" WSS-FIRST" WSS-LINE ;

: WSS-FIRST$ ( -- ptr u8 n )
   SB-RESET
   WSS-PRELUDE
   WSS-FIRST-BODY
   SB$ ;

: WSS-LATER-BODY ( -- )
   s" : WSS-LATER-P ( -- ptr wss-res<n,n> )" WSS-LINE
   s"   data-base FRIEND-LATCH-CELL 1 cells - + ;" WSS-LINE
   s" : WSS-LATER ( -- ) WSS-ZERO WSS-LATER-P ! ;" WSS-LINE
   WSS-ARM-LINE
   s" WSS-LATER" WSS-LINE ;

: WSS-LATER$ ( -- ptr u8 n )
   SB-RESET
   WSS-PRELUDE
   WSS-LATER-BODY
   SB$ ;

: WSS-CAPTURE ( ptr u8 n -- len len outcome ) {: src:ptr srcu:n :}
   WSS-HB$ >LEN
   src srcu >LEN
   WSS-OUT WSS-CAP >LEN
   WSS-ERR WSS-CAP >LEN
   WSS-TIMEOUT-MS >MS
   RUN-ARGV-STDIN-CAPTURE-OUTCOME ;

: WSS-RUN ( ptr u8 n -- len len outcome )
   PROC-ARGV-RESET
   WSS-CAPTURE ;

: WSS-ASSERT-RC ( len len outcome n -- ) {: want:n :}
   want T-OUTCOME-EXITED=
   LEN>N drop
   LEN>N WSS-OUT-U !
   WSS-OUT WSS-OUT-U @ s" WSS-ARMED" CONTAINS? TTRUE ;

: WSS-NORMAL-TEST ( -- )
   s" ordinary W=2 store remains green" T-LABEL
   WSS-NORMAL$ WSS-RUN 0 WSS-ASSERT-RC ;

: WSS-FIRST-TEST ( -- )
   s" first-cell protected intersection traps before mutation" T-LABEL
   WSS-FIRST$ WSS-RUN E-SEAL-VIOLATION WSS-ASSERT-RC ;

: WSS-LATER-TEST ( -- )
   s" later-cell protected intersection traps before mutation" T-LABEL
   WSS-LATER$ WSS-RUN E-SEAL-VIOLATION WSS-ASSERT-RC ;

: WSS-MAIN ( -- )
   T-RESET
   WSS-NORMAL-TEST
   WSS-FIRST-TEST
   WSS-LATER-TEST
   T-REPORT
   s" wide-store-seal: ok" type cr ;

WSS-MAIN
