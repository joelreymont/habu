\ Roundtrip the supported checker graph through the real 17-section file.
\ Clear capture buffers and retired source effects before importing metadata.
\ Runtime bodies remain in this process; fresh native-image execution is a
\ separate acceptance boundary, and exceptional quotes are refused explicitly.
s" src/habu/layout.f" provided
s" src/core/checker-owner-abi.f" provided
require src/habu/aot-arm.f
require src/arch/arm64/asm.f
require src/arch/arm64/icode.f
require src/habu/aot-decl.f
require src/habu/xref.f
require src/habu/aot-capture.f
require src/habu/aot-ident.f
require src/habu/fdio.f
require src/habu/aot-file.f

package TFAM
public

\ The old rows must not rescue an importer that publishes counts but fails to
\ copy records. Bounds and current marks are checked before clearing them.
TRUSTED: ERASE-REGISTRY-DELTA ( ptr u8 n -- ) {: src:ptr u:n :}
   src u REG-AOT-TABLE-CHECK
   REG-AOT-N 0 ?do
      src i REG-AOT-ROW@ drop {: base:n count:n :}
      i REG-AOT-COUNT base <> IF 79 throw THEN
      i REG-AOT-BASE-PTR base i REG-AOT-WIDTH * +
      count i REG-AOT-WIDTH * ASIG-GRAPH-ZERO
   loop ;

;package

package GRAPH-ROUNDTRIP
using AOT-BUF
using AOT-ARM
using AOT-CAPTURE
using AOT-IDENT
$20000 constant CAP
create POOL CAP allot
variable USED
variable MARK
create KEY 32 allot
public

: EQ ( n n -- )
   2dup <> IF s" graph mismatch got/expected: " type swap . . 79 throw THEN 2drop ;
: ART$ ( -- ptr u8 n )
   s" HABU_PAYLOAD_TEST_ARTIFACT" GETENV
   dup 0= IF s" graph roundtrip: missing artifact path" 79 die THEN ;

TRUSTED: SAVE ( -- )
   AOT-SIG-BUF@ AOT-SIG-N @ SIG-ROW * AOT-SIG-STR-BUF@ AOT-SIG-STR-LEN @ {: rows:ptr rowu:n str:ptr stru:n :}
   3 POOL !
   56 POOL 8 + ! rowu POOL 16 + !
   56 rowu + POOL 24 + ! stru POOL 32 + !
   56 rowu + stru + {: regoff:n :}
   regoff POOL 40 + !
   AOT-REG-LEN @ {: regu:n :}
   regu CAP regoff - > IF 79 throw THEN
   AOT-REG-BUF@ POOL regoff + regu USIGS-COPY
   regu POOL 48 + !
   rows POOL 56 + rowu USIGS-COPY
   str POOL 56 rowu + + stru USIGS-COPY
   regoff regu + USED ! ;

TRUSTED: MODE? ( ptr u8 n -- bool )
   s" HABU_PAYLOAD_TEST_MODE" GETENV CORE-STR= ;

TRUSTED: GRAPH ( n -- ptr u8 )
   4 CK-AOT-FIELD POOL CK-AOT-S-STR CK-AOT-OFF + + ;

TRUSTED: DIN ( ptr u8 -- ptr u8 ) dup ER.DIN @ + ;
TRUSTED: DOUT-TYPE ( ptr u8 -- ptr u8 ) {: graph:ptr :}
   graph ER.DOUT @ graph + EN.A @ graph + ;

TRUSTED: CORRUPT ( -- )
   s" length" MODE? IF $7FFFFFFFFFFFFFFF 0 GRAPH ER.NEXT ! THEN
   s" cycle" MODE? IF 0 GRAPH dup ER.DIN @ swap DIN EN.B ! THEN
   s" tag" MODE? IF 99 0 GRAPH DIN EN.TAG ! THEN
   s" variables" MODE? IF
      2 GRAPH dup ER.TVN @ swap ER.RVN @ + 0 > 0= IF 79 throw THEN
      0 2 GRAPH ER.TVN ! 0 2 GRAPH ER.RVN !
   THEN
   s" family" MODE? IF
      3 GRAPH DOUT-TYPE dup EN.TAG @ EN-PARAM EQ
      0 swap EN.H !
   THEN
   s" exception" MODE? IF
      4 GRAPH DOUT-TYPE dup EN.TAG @ EN-QUOT EQ
      -1 swap EN.E !
   THEN
   s" HABU_PAYLOAD_TEST_MODE" GETENV nip IF
      s" graph corruption applied: " type s" HABU_PAYLOAD_TEST_MODE" GETENV type cr
   THEN ;

TRUSTED: ARM ( -- ) CHECKER-SCOPE-START UEND @ MARK ! WINDOW-OPEN ;
TRUSTED: INSTALL ( -- )
   WINDOW-CLOSE
   R0 @ D0 @ PRELUDE-MARK
   PAYLOAD-CAPTURE
   WINDOW$ CAPTURE
   RESET
   s" src/core/checker.f" PATH+
   s" bin/hb" KEY SHA256-FILE 0 <> IF 79 throw THEN
   \ File READ/WRITE remain qualified because the OS primitives have these names.
   KEY ART$ AOT-FILE:WRITE
   AOT-SIG-BUF@ AOT-SIG-N @ SIG-ROW * ASIG-GRAPH-ZERO
   AOT-SIG-STR-BUF@ AOT-SIG-STR-LEN @ ASIG-GRAPH-ZERO
   AOT-REG-BUF@ AOT-REG-LEN @ ASIG-GRAPH-ZERO
   0 AOT-SIG-N ! 0 AOT-SIG-STR-LEN ! 0 AOT-REG-LEN !
   KEY ART$ AOT-FILE:READ
   SAVE
   UEND @ {: source-end:n :}
   CHECKER-SCOPE-DONE
   UEND @ MARK @ EQ
   USIGS MARK @ + source-end MARK @ - ASIG-GRAPH-ZERO
   POOL POOL 40 + @ + POOL 48 + @ TFAM:ERASE-REGISTRY-DELTA
   POOL CK-AOT-SIG-POOL-FIELD !
   USED @ data-base CK-AOT-SIG-LEN-OFF + !
   0 CK-AOT-STATE !
   CORRUPT
   CK-AOT-REG-INSTALL
   CK-AOT-ROWS 0 ?do i CK-AOT-TAKE loop
   CK-AOT-ROWS 6 EQ ;      \ the six ordinary window definitions below

ARM
;package

: ROW-ADD ( R -- R ) 1+ ;
: PAYLOAD-FIXED ( n -- n ) 1+ ;
: PAYLOAD-QUANT ( R [ R -- S ] -- S ) execute ;
NEWTYPE payload-tag 0
: PAYLOAD-TAG ( payload-tag -- payload-tag ) ;
: ANON-PROVIDER [: 1+ ;] ;
: RETURN-PROVIDER ( n | -- | n ) >r ;

package GRAPH-ROUNDTRIP
INSTALL
s" ROUND-GOOD ( n -- n ) ROW-ADD" CHECK! -1 EQ
s" ROUND-BAD ( ptr u8 -- ptr u8 ) ROW-ADD" CHECK! 0 EQ
s" ROUND-QUANT ( n -- n ) [: 1+ ;] PAYLOAD-QUANT" CHECK! -1 EQ
s" ROUND-QUANT-PTR ( ptr u8 -- ptr u8 ) [: ;] PAYLOAD-QUANT" CHECK! -1 EQ

s" ROUND-TAG ( payload-tag -- payload-tag ) PAYLOAD-TAG" CHECK! -1 EQ
s" ROUND-TAG-BAD ( n -- n ) PAYLOAD-TAG" CHECK! 0 EQ
s" ROUND-ANON-BAD ( ptr u8 -- ptr u8 ) ANON-PROVIDER execute" CHECK! 0 EQ
s" ROUND-RETURN ( n | -- | n ) RETURN-PROVIDER" CHECK! -1 EQ
\ Checked consumers execute after the source effect bytes have been erased.
: GRAPH-JIT-USE ( n -- n ) ANON-PROVIDER execute ;
17 GRAPH-JIT-USE 18 EQ
1 set-tier
: GRAPH-NATIVE-USE ( n -- n ) [: 1+ ;] PAYLOAD-QUANT ;
0 set-tier
: GRAPH-JIT-RETURN ( n -- n ) RETURN-PROVIDER r> ;
17 GRAPH-NATIVE-USE 18 EQ
17 GRAPH-JIT-RETURN 17 EQ
s" graph metadata file roundtrip: ok" type cr
;package
