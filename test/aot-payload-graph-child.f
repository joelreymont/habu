\ Roundtrip the supported checker graph through the real 17-section file.
\ Clear capture buffers and retired source effects before importing metadata.
\ Runtime bodies remain in this process. The companion native producer,
\ reader and consumer test execution across process exits in the same suite.
\ Exceptional quotes are refused explicitly.
s" lib/prelude.f" provided
s" src/habu/layout.f" provided
s" src/core/checker-owner-abi.f" provided
require src/habu/aot-arm.f
require src/arch/arm64/asm.f
require src/arch/arm64/icode.f
require src/habu/aot-decl.f
require src/habu/code-span.f
require src/habu/xref.f
require src/habu/aot-capture.f
require src/habu/aot-ident.f
require src/habu/fdio.f
require src/habu/aot-file.f
require src/core/generated-declaration-dictionary.f
require src/core/generated-declaration-protection.f

package GRAPH-PUBLICATION
$800000 constant CAP
create SNAPSHOT CAP allot
variable CURSOR variable USED variable COMPARING
variable SCALAR
public

TRUSTED: BYTES ( ptr u8 n -- ) {: src:ptr u:n :}
   u CAP CURSOR @ - > IF 79 throw THEN
   SNAPSHOT CURSOR @ + {: saved:ptr :}
   COMPARING @ IF
      src u saved u CORE-STR= 0= IF 79 throw THEN
   ELSE src saved u USIGS-COPY THEN
   u CURSOR +! ;

TRUSTED: VALUE ( n -- ) SCALAR ! SCALAR CELL BYTES ;
TRUSTED: START ( bool -- ) COMPARING ! 0 CURSOR ! ;
TRUSTED: FINISH ( -- )
   COMPARING @ IF CURSOR @ USED @ <> IF 79 throw THEN
   ELSE CURSOR @ USED ! THEN ;

\ Published effects, symbols, constructors and their effect-index heads.
TRUSTED: CORE ( -- )
   UEND @ VALUE USIGS UEND @ BYTES
   SYM-N @ VALUE SYMS SYM-N @ SYM-REC * BYTES
   SYM-STR-U @ VALUE SYM-STR SYM-STR-U @ BYTES
   CTN @ VALUE CT-STR-U @ VALUE CT-STR CT-STR-U @ BYTES
   CT-NAME-A BYTE-VIEW CTN @ cells BYTES
   CT-NAME-U BYTE-VIEW CTN @ cells BYTES
   CT-CLASS BYTE-VIEW CTN @ cells BYTES
   CT-WIDTH BYTE-VIEW CTN @ cells BYTES
   CT-SIGN BYTE-VIEW CTN @ cells BYTES
   USX-GEN @ VALUE USX-HI @ VALUE
   SYM-N @ 1 ?do i USX@ VALUE loop ;

;package

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

TRUSTED: PREPARE-GRAPH-STATE ( -- ) TFX-ENSURE SVX-ENSURE ;
TRUSTED: GRAPH-STATE ( -- )
   REG-AOT-N 0 ?do
      i REG-AOT-COUNT GRAPH-PUBLICATION:VALUE
      i REG-AOT-BASE-PTR i REG-AOT-COUNT i REG-AOT-WIDTH * GRAPH-PUBLICATION:BYTES
   loop
   TFX-READY @ GRAPH-PUBLICATION:VALUE TFX-HI @ GRAPH-PUBLICATION:VALUE
   TFX-CAP @ GRAPH-PUBLICATION:VALUE TFX-SLOTS GRAPH-PUBLICATION:VALUE
   SVX-GEN @ GRAPH-PUBLICATION:VALUE SVX-HI @ GRAPH-PUBLICATION:VALUE
   TFX-BASE BYTE-VIEW TFX-SLOTS cells GRAPH-PUBLICATION:BYTES
   SYM-N @ 1 ?do i SVX@ GRAPH-PUBLICATION:VALUE loop ;

;package

package GRAPH-ROUNDTRIP
using AOT-BUF
using AOT-ARM
using AOT-CAPTURE
using AOT-IDENT
using STRUCTURE-DECL
using ENUM-DECL
$20000 constant CAP
create POOL CAP allot
variable USED
variable MARK
variable SAVED-CHECK
variable SAVED-TIER
create KEY 32 allot
create FSHA-CTX SHA256-FILE-CTX-BYTES allot   \ this fixture's file-digest context
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

TRUSTED: DIN ( ptr u8 -- ptr u8 ) dup EW.DIN @ + ;
TRUSTED: DOUT-TYPE ( ptr u8 -- ptr u8 ) {: graph:ptr :}
   graph EW.DOUT @ graph + EN.A @ graph + ;

TRUSTED: NAMED-GRAPH ( ptr u8 n -- ptr u8 ) {: name:ptr u:n :}
   CK-AOT-ROWS 0 ?do
      i 0 CK-AOT-FIELD CK-AOT-STR$ name u CORE-STR=CI IF i GRAPH unloop EXIT THEN
   loop 79 throw NULL-PTR ;

TRUSTED: CORRUPT ( -- )
   s" authority-bits" MODE? IF $10 0 GRAPH EW.SYM ! THEN
   s" length" MODE? IF $7FFFFFFFFFFFFFFF 0 GRAPH EW.NEXT ! THEN
   s" cycle" MODE? IF 0 GRAPH dup EW.DIN @ swap DIN EN.B ! THEN
   s" tag" MODE? IF 99 0 GRAPH DIN EN.TAG ! THEN
   s" variables" MODE? IF
      2 GRAPH dup EW.TVN @ swap EW.RVN @ + 0 > 0= IF 79 throw THEN
      0 2 GRAPH EW.TVN ! 0 2 GRAPH EW.RVN !
   THEN
   s" family" MODE? IF
      3 GRAPH DOUT-TYPE dup EN.TAG @ EN-PARAM EQ
      0 swap EN.H !
   THEN
   s" exception" MODE? IF
      4 GRAPH DOUT-TYPE dup EN.TAG @ EN-QUOT EQ
      -1 swap EN.E !
   THEN
   s" scalar-zero" MODE? IF
      1 GRAPH DIN EN.C @ 2 EQ
      1 1 GRAPH DIN EN.C !
      0 1 GRAPH EW.MINI !
   THEN
   s" wide-width" MODE? IF
      s" PAYLOAD-WIDE-USE" NAMED-GRAPH {: graph:ptr :}
      graph DIN EN.C @ 2 EQ
      graph DIN EN.A @ graph + dup EN.TAG @ EN-PARAM EQ
      dup EN.E @ 0 > 0= IF 79 throw THEN
      EN.E @ 2 EQ
      graph DIN dup EN.C @ 1+ swap EN.C !
      graph EW.MINI dup @ 1+ swap !
   THEN
   s" logical-width" MODE? IF
      s" PAYLOAD-POLY-USE" NAMED-GRAPH {: graph:ptr :}
      graph DIN EN.C @ 3 EQ
      graph DIN EN.A @ graph + dup EN.TAG @ EN-PARAM EQ
      EN.E @ 0 EQ
      2 graph DIN EN.C ! 1 graph EW.MINI !
   THEN
   s" HABU_PAYLOAD_TEST_MODE" GETENV nip IF
      s" graph corruption applied: " type s" HABU_PAYLOAD_TEST_MODE" GETENV type cr
   THEN ;

TRUSTED: WIDTH-MODE? ( -- bool )
   s" scalar-zero" MODE? s" wide-width" MODE? or s" logical-width" MODE? or ;

\ Exercise the private throwing validator below the public process refusal,
\ then compare every published store before rerunning the real install path.
TRUSTED: WIDTH-REFUSAL-ATOMIC ( -- )
   WIDTH-MODE? 0= IF EXIT THEN
   TFAM:PREPARE-GRAPH-STATE
   RES-FALSE GRAPH-PUBLICATION:START
   GRAPH-PUBLICATION:CORE TFAM:GRAPH-STATE GRAPH-PUBLICATION:FINISH
   [: CK-AOT-CONTENTS? ;] catch 76 EQ
   CK-GRAPH-WIDTH-BAD @ -1 EQ
   CK-GRAPH-RELEASE
   CK-AOT-STATE @ 0 EQ
   RES-TRUE GRAPH-PUBLICATION:START
   GRAPH-PUBLICATION:CORE TFAM:GRAPH-STATE GRAPH-PUBLICATION:FINISH
   s" graph width refusal preserved publication state" type cr ;

TRUSTED: ARM ( -- ) CHECKER-SCOPE-START UEND @ MARK ! WINDOW-OPEN ;
TRUSTED: ABI-BEGIN ( -- )
   check@ SAVED-CHECK ! tier@ SAVED-TIER ! 0 set-check 1 set-tier ;
TRUSTED: ABI-END ( -- ) SAVED-CHECK @ set-check SAVED-TIER @ set-tier ;
TRUSTED: SOURCE-MIN ( ptr u8 n -- n ) EFFECT-EXTERNAL-MIN-IN ;
TRUSTED: DECLARE-WIDE ( -- )
   s" payload-wide" s" 0 FIELD left n FIELD right r ;STRUCTURE" SD-REPLAY
   s" payload-empty" s" 0 ;STRUCTURE" SD-REPLAY
   s" payload-option" s" 1 VARIANT full FIELD value a ;VARIANT VARIANT empty ;VARIANT ;ENUM" ED-REPLAY ;
TRUSTED: CORRUPT-SOURCE ( -- )
   s" producer-scalar-zero" MODE? IF
      s" PAYLOAD-FIXED" FIND-SIG -1 EQ
      FEP @ {: rec:ptr :}
      rec E-DIN@ E-PTR EN.C @ 2 EQ
      1 rec E-DIN@ E-PTR EN.C ! 0 rec E-CONTENT EC.MINI !
      s" graph corruption applied: producer-scalar-zero" type cr
   THEN ;
TRUSTED: INSTALL ( -- )
   CORRUPT-SOURCE
   WINDOW-CLOSE
   R0 @ D0 @ PRELUDE-MARK
   PAYLOAD-CAPTURE
   WINDOW$ CAPTURE
   RESET
   s" src/core/checker.f" PATH+
   FSHA-CTX s" bin/hb" KEY SHA256-FILE-IN 0 <> IF 79 throw THEN
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
   WIDTH-REFUSAL-ATOMIC
   CK-AOT-REG-INSTALL
   CK-AOT-ROWS 0 ?do i CK-AOT-TAKE loop
   CK-AOT-ROWS 11 EQ ;

ARM
;package

: ROW-ADD ( R -- R ) 1+ ;
: PAYLOAD-FIXED ( n -- n ) 1+ ;
: PAYLOAD-QUANT ( R [ R -- S ] -- S ) execute ;
NEWTYPE payload-tag 0
: PAYLOAD-TAG ( payload-tag -- payload-tag ) ;
: ANON-PROVIDER [: 1+ ;] ;
: RETURN-PROVIDER ( n | -- | n ) >r ;
GRAPH-ROUNDTRIP:DECLARE-WIDE
: PAYLOAD-WIDE-USE ( payload-wide -- payload-wide ) ;
: PAYLOAD-NESTED-USE ( payload-option<payload-wide> -- payload-option<payload-wide> ) ;
: PAYLOAD-POLY-USE ( payload-option<a> -- payload-option<a> ) ;
: PAYLOAD-ZERO-ARG-USE ( payload-option<payload-empty> -- payload-option<payload-empty> ) ;
GRAPH-ROUNDTRIP:ABI-BEGIN
: PAYLOAD-ABI ( n -- n ) ;
GRAPH-ROUNDTRIP:ABI-END

package GRAPH-ROUNDTRIP
INSTALL
s" PAYLOAD-FIXED" SOURCE-MIN 1 EQ
s" PAYLOAD-ABI" EFFECT-QUERY -1 EQ
EFFECT-DIN-CELLS 1 EQ EFFECT-DOUT-CELLS 1 EQ
s" PAYLOAD-ABI" SOURCE-MIN -1 EQ
s" PAYLOAD-ABI" CHECKER-RESOLVES? 0 EQ
s" ROUND-ABI ( n -- n ) PAYLOAD-ABI" CHECK! 0 EQ
s" PAYLOAD-WIDE-USE" EFFECT-QUERY -1 EQ
EFFECT-DIN-N 2 EQ EFFECT-DIN-CELLS 2 EQ
s" PAYLOAD-NESTED-USE" EFFECT-QUERY -1 EQ
EFFECT-DIN-N 3 EQ EFFECT-DIN-CELLS 3 EQ
s" PAYLOAD-POLY-USE" EFFECT-QUERY -1 EQ
EFFECT-DIN-N 1 EQ EFFECT-DIN-CELLS 2 EQ
s" PAYLOAD-ZERO-ARG-USE" EFFECT-QUERY -1 EQ
EFFECT-DIN-N 1 EQ EFFECT-DIN-CELLS 1 EQ
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
