\ A real frozen graph arena larger than the old text-only pool. Every byte
\ survives file/owned transfer, merge and the source writer's contiguous span.
package EFFECT-POOL-TEST
ndict@ here variable PRE-R variable PRE-D PRE-D ! PRE-R !
;package

require lib/test.f
require lib/test/outcome.f
require lib/test/subject.f
require lib/fs-mutate.f
require lib/engine-id.f
require tools/native-emit.f
require src/habu/aot-arm.f
require src/habu/aot-capture.f

package AOT-FILE
public
\ Each row is in bounds and the table fills this actual owned allocation.
\ Only the aggregate is invalid, and IMPORT must reject it before any copy.
: EFFECT-TEST-OVERBUDGET ( -- AOT-OWNED:capture )
   STAGE BUILD-TABLE
   SEC-N ROW-BYTES * CUR !
   SEC-N 0 ?do
      i S-SIGSTR = if AOT-SECTION-CAP else i ROW-LEN@ then {: bytes:n :}
      CUR @ bytes i ROW! CUR @ bytes + CUR !
   loop
   CUR @ MEM-ALLOC-BYTES {: dst:ptr size:n :}
   TBL dst SEC-N ROW-BYTES * BYTE-COPY
   dst size -1 AOT--OWNED-CAPTURE:MAKE ;

: EFFECT-TEST-HEADER ( ptr u8 n -- )
   AOT-SECTION-CAP HDR O-PAYLEN + U64!
   HDR HDR-BYTES WRITE-ALL ;

;package

package EFFECT-POOL-TEST
using AOT-BUF
using AOT-WINDOW

1536 constant WORDS
create ROOT FS-PATH-CAP allot variable ROOT-U
create SOURCE FS-PATH-CAP allot variable SOURCE-U
create ART FS-PATH-CAP allot variable ART-U
create BAD-ART FS-PATH-CAP allot variable BAD-ART-U
create KEY 32 allot
create FSHA-CTX SHA256-FILE-CTX-BYTES allot   \ this fixture's file-digest context

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;
: SOURCE$ ( -- ptr u8 n ) SOURCE SOURCE-U @ ;
: ART$ ( -- ptr u8 n ) ART ART-U @ ;
: BAD-ART$ ( -- ptr u8 n ) BAD-ART BAD-ART-U @ ;

: NAME+ ( n -- ) {: k:n :}
   4 0 ?do k 3 i - 4 * rshift $F and s" 0123456789ABCDEF" drop + c@ SB-APPEND-C loop ;

public
: GENERATE ( -- )
   T-RESET CLEANUP-RESET
   s" habu-effect-pool" HB-TMP-MKDIR {: path:ptr u:n :}
   path ROOT u BYTE-COPY u ROOT-U ! ROOT$ CLEANUP-TREE+
   ROOT$ s" definitions.f" SOURCE JOIN-PATH SOURCE-U !
   ROOT$ s" effects.aot" ART JOIN-PATH ART-U !
   ROOT$ s" bad-header.aot" BAD-ART JOIN-PATH BAD-ART-U !
   SOURCE$ S\" package EFFECT-POOL-WINDOW public\nNEWTYPE effect-tag 0\n" WRITE-ALL
   WORDS 0 ?do
      SB-RESET s" : EFFECT-" SB-APPEND i NAME+
      S\"  ( n n n n -- n n n n ) 1+ ;\n" SB-APPEND
      SOURCE$ SB$ APPEND-FILE
   loop
   SOURCE$ S\" ;package\n" APPEND-FILE ;

: DEFINITIONS$ ( -- ptr u8 n ) SOURCE$ ;

;using
;using
;package

EFFECT-POOL-TEST:GENERATE
AOT-ARM:WINDOW-OPEN
EFFECT-POOL-TEST:DEFINITIONS$ included
AOT-ARM:WINDOW-CLOSE

package EFFECT-POOL-TEST
using AOT-BUF
using AOT-WINDOW

DYNAMIC-BUFFER EXPECTED n
variable EXPECTED-U
create EXPECTED-ROWS WORDS SIG-ROW * allot
create EXPECTED-REG AOT-REG-CAP allot variable EXPECTED-REG-U
$1000 constant IO-CAP
create OUT IO-CAP allot create ERR IO-CAP allot

: EXPECTED$ ( -- ptr u8 n ) 0 EXPECTED BYTE-VIEW EXPECTED-U @ ;
: POOL$ ( -- ptr u8 n ) AOT-SIG-STR-BUF@ AOT-SIG-STR-LEN @ ;
: U32@ ( ptr u8 -- n ) {: p:ptr :}
   p c@ p 1+ c@ 8 lshift or p 2 + c@ 16 lshift or p 3 + c@ 24 lshift or ;
: U64@ ( ptr u8 -- n ) {: p:ptr :}
   0 8 0 ?do 8 lshift p 7 i - + c@ or loop ;

: WORD-TIER ( ptr u8 n -- )
   XREF-FIND {: rec:ptr :}
   rec XREF-FOUND? 0= if 79 throw then
   rec XREF-START dup rec XREF-LEN + code-origin tier@ T= ;

: SOURCE-TIER ( -- )
   s" EFFECT-POOL-WINDOW:EFFECT-0000" WORD-TIER
   s" EFFECT-POOL-WINDOW:EFFECT-05FF" WORD-TIER
   s" AOT-SIG-PAYLOAD:BUILD" WORD-TIER
   tier@ 1 = if AOT-ARM:B0 @ AOT-ARM:B1 @ code-origin 1 T= then ;

: CAPTURE ( -- )
   PRE-R @ PRE-D @ AOT-CAPTURE:PRELUDE-MARK
   AOT-ARM:WINDOW$ AOT-CAPTURE:CAPTURE
   AOT-IDENT:RESET SOURCE$ AOT-IDENT:PATH+
   FSHA-CTX ENGINE-ID:PATH$ KEY SHA256-FILE-IN 0 T=
   AOT-SIG-N @ WORDS T=
   AOT-SIG-STR-LEN @ $40000 > TTRUE ;

: SAVE ( -- )
   AOT-SIG-STR-LEN @ dup EXPECTED-U ! CELL 1- + CELL / EXPECTED-RESERVE
   AOT-SIG-STR-BUF@ EXPECTED$ BYTE-COPY
   AOT-SIG-BUF@ EXPECTED-ROWS WORDS SIG-ROW * BYTE-COPY
   AOT-REG-LEN @ EXPECTED-REG-U !
   AOT-REG-BUF@ EXPECTED-REG AOT-REG-LEN @ BYTE-COPY ;

: CHECK ( -- )
   AOT-SIG-N @ WORDS T=
   POOL$ EXPECTED$ STR= TTRUE
   AOT-SIG-BUF@ WORDS SIG-ROW * EXPECTED-ROWS WORDS SIG-ROW * STR= TTRUE ;

: RELEASE ( -- )
   0 AOT-SIG-N ! 0 AOT-SIG-STR-LEN ! SIG-STR-STORAGE-RELEASE ;

: TRANSFER ( AOT-OWNED:capture -- )
   RELEASE dup AOT-FILE:IMPORT CHECK AOT-OWNED:CLOSE ;

\ Start with the row-only byte courier shape to exercise an empty pool, then
\ grow that staging mapping to the complete real graph arena. Graph intake is
\ tested elsewhere; this boundary must copy exact section bytes and offsets.
: STAGING ( -- )
   AOT-REG-LEN @ {: regu:n :}
   AOT-SIG-PAYLOAD:STORAGE-RELEASE
   RELEASE 0 AOT-REG-LEN !
   AOT-SIG-PAYLOAD:BUILD AOT-SIG-PAYLOAD:LEN @ 0 T=
   1 AOT-SIG-N ! AOT-SIG-PAYLOAD:BUILD
   AOT-SIG-PAYLOAD:LEN @ 56 SIG-ROW + T=
   EXPECTED-U @ AOT-SIG-STR-RESERVE
   EXPECTED$ {: saved:ptr savedu:n :}
   saved AOT-SIG-STR-BUF@ savedu BYTE-COPY
   WORDS AOT-SIG-N ! EXPECTED-U @ AOT-SIG-STR-LEN !
   regu AOT-REG-LEN !
   AOT-SIG-PAYLOAD:BUILD
   AOT-SIG-PAYLOAD:LEN @ $A0038 > TTRUE  \ former combined static capacity
   AOT-SIG-PAYLOAD:BUF@ {: p:ptr :}
   p U64@ 3 T=
   p 8 + U64@ 56 T= p 16 + U64@ WORDS SIG-ROW * T=
   p 24 + U64@ 56 WORDS SIG-ROW * + T=
   p 32 + U64@ EXPECTED-U @ T=
   p 40 + U64@ 56 WORDS SIG-ROW * + EXPECTED-U @ + T=
   p 48 + U64@ regu T=
   p 56 + WORDS SIG-ROW * EXPECTED-ROWS WORDS SIG-ROW * STR= TTRUE
   p p 24 + U64@ + EXPECTED-U @ EXPECTED$ STR= TTRUE
   p p 40 + U64@ + regu AOT-REG-BUF@ regu STR= TTRUE
   AOT-SIG-PAYLOAD:LEN @ 56 WORDS SIG-ROW * + EXPECTED-U @ + regu + T= ;

: MERGE ( -- )
   \ These two copies share the same frozen registry. Carry it once, from the
   \ incoming artifact; the negative control below retains the two-owner refusal.
   \ This is a section courier check, not execution of duplicated dictionaries.
   AOT-REG-LEN @ {: regu:n :}
   EXPECTED-U @ 7 + -8 and {: base:n :}
   EXPECTED-U @ base < TTRUE
   0 AOT-REG-LEN !
   KEY ART$ AOT-FILE:MERGE
   AOT-REG-LEN @ regu T=
   AOT-SIG-N @ WORDS 2 * T=
   AOT-SIG-STR-LEN @ base EXPECTED-U @ + T=
   AOT-SIG-STR-BUF@ EXPECTED-U @ EXPECTED$ STR= TTRUE
   AOT-SIG-STR-BUF@ base + EXPECTED-U @ EXPECTED$ STR= TTRUE
   base EXPECTED-U @ ?do AOT-SIG-STR-BUF@ i + c@ 0 T= loop
   AOT-SIG-BUF@ WORDS SIG-ROW * EXPECTED-ROWS WORDS SIG-ROW * STR= TTRUE
   WORDS 0 ?do
      AOT-SIG-BUF@ WORDS i + SIG-ROW * + {: row:ptr :}
      EXPECTED-ROWS i SIG-ROW * + {: old:ptr :}
      row U32@ old U32@ base + T=
      row 4 + U32@ old 4 + U32@ base + T=
      row 4 + U32@ 7 and 0 T=
      row 8 + U32@ old 8 + U32@ base + T=
      row 12 + U32@ old 12 + U32@ T=
   loop ;

public
: BAD-NEGATIVE ( -- ) -1 AOT-SIG-STR-RESERVE ;
: BAD-OVERFLOW ( -- ) $7FFFFFFFFFFFFFFF AOT-SIG-STR-RESERVE ;
: BAD-LIMIT ( -- ) AOT-SIG-STR-CAP 1+ AOT-SIG-STR-RESERVE ;
: BAD-WRITE ( -- ) AOT-SIG-STR-CAP AOT-SIG-STR-LEN ! KEY ART$ AOT-FILE:WRITE ;
: BAD-OWN ( -- ) AOT-SIG-STR-CAP AOT-SIG-STR-LEN ! AOT-FILE:OWN AOT-OWNED:CLOSE ;
: BAD-IMPORT ( -- ) AOT-FILE:EFFECT-TEST-OVERBUDGET AOT-FILE:IMPORT ;
: BAD-READ ( -- )
   BAD-ART$ AOT-FILE:EFFECT-TEST-HEADER KEY BAD-ART$ AOT-FILE:READ ;
: BAD-MERGE ( -- )
   0 AOT-REG-LEN !
   AOT-SIG-STR-CAP EXPECTED-U @ - -8 and AOT-SIG-STR-LEN ! KEY ART$ AOT-FILE:MERGE ;
: BAD-REGISTRY ( -- ) KEY ART$ AOT-FILE:MERGE ;
: BAD-REGISTRY-LIVE ( -- )
   \ Closing a later empty window changes the live serializer's delta to zero,
   \ but the original captured buffer must still refuse a competing payload.
   AOT-ARM:WINDOW-OPEN AOT-ARM:WINDOW-CLOSE
   KEY ART$ AOT-FILE:MERGE ;
: BAD-REGISTRY-SHORT ( -- )
   8 AOT-REG-LEN ! KEY ART$ AOT-FILE:MERGE ;
: BAD-REGISTRY-BOUNDS ( -- )
   $7FFFFFFFFFFFFFFF AOT-REG-BUF@ 8 + CELL-VIEW !
   KEY ART$ AOT-FILE:MERGE ;
: MAKE-PREFIX ( -- )
   AOT-ARM:WINDOW-OPEN AOT-ARM:WINDOW-CLOSE
   AOT-REG-BUF@ AOT-REG-CAP CHECKER-REG-AOT-SAVE AOT-REG-LEN ! ;
: CHECK-REGISTRY ( -- )
   AOT-REG-BUF@ AOT-REG-LEN @ EXPECTED-REG EXPECTED-REG-U @ STR= 0= if 79 throw then
   WORDS 2 * AOT-SIG-N @ <> if 79 throw then ;
: PREFIX-REGISTRY ( -- )
   MAKE-PREFIX
   KEY ART$ AOT-FILE:MERGE
   CHECK-REGISTRY
   s" prefix registry merge: ok" type cr ;
: HOST-DELTA-REGISTRY ( -- )
   MAKE-PREFIX KEY BAD-ART$ AOT-FILE:WRITE
   EXPECTED-REG AOT-REG-BUF@ EXPECTED-REG-U @ BYTE-COPY
   EXPECTED-REG-U @ AOT-REG-LEN !
   KEY BAD-ART$ AOT-FILE:MERGE CHECK-REGISTRY
   s" host delta merge: ok" type cr ;
: BAD-REGISTRY-PREFIX ( -- )
   MAKE-PREFIX
   \ First family record: alter an identity field without changing its extent.
   AOT-REG-BUF@ 200 + dup c@ 1 xor swap c!
   KEY ART$ AOT-FILE:MERGE ;
: CANONICAL-REGISTRY ( -- )
   MAKE-PREFIX
   \ TF.TAILNEXT is a rebuilt link, not part of captured family identity.
   AOT-REG-BUF@ 200 152 + + dup c@ 1 xor swap c!
   KEY ART$ AOT-FILE:MERGE CHECK-REGISTRY
   s" canonical registry merge: ok" type cr ;
: BAD-STAGING ( -- ) AOT-SIG-STR-CAP AOT-SIG-STR-LEN ! AOT-SIG-PAYLOAD:BUILD ;

private
: REJECT ( ptr u8 n n ptr u8 n -- ) {: code:n message:ptr u:n :}
   OUT IO-CAP >LEN ERR IO-CAP >LEN 5000 >MS SUBJECT:RUN
   code T-OUTCOME-EXITED= {: outu:len erru:len :}
   OUT outu LEN>N message u CONTAINS? ERR erru LEN>N message u CONTAINS? or TTRUE
   CHECK ;

: REJECT-RESERVE ( ptr u8 n -- )
   74 s" aot: effect pool exceeds the section byte budget" REJECT ;
: REJECT-BUDGET ( ptr u8 n -- )
   75 s" aot-file: encoded sections exceed their byte budget" REJECT ;

: REFUSALS ( -- )
   s" EFFECT-POOL-TEST:BAD-NEGATIVE" REJECT-RESERVE
   s" EFFECT-POOL-TEST:BAD-OVERFLOW" REJECT-RESERVE
   s" EFFECT-POOL-TEST:BAD-LIMIT" REJECT-RESERVE
   s" EFFECT-POOL-TEST:BAD-WRITE" REJECT-BUDGET
   s" EFFECT-POOL-TEST:BAD-OWN" REJECT-BUDGET
   s" EFFECT-POOL-TEST:BAD-IMPORT" REJECT-BUDGET
   s" EFFECT-POOL-TEST:BAD-READ" REJECT-BUDGET
   s" EFFECT-POOL-TEST:BAD-MERGE" REJECT-BUDGET
   s" EFFECT-POOL-TEST:BAD-REGISTRY" 76
   s" tfam: two type-registry deltas cannot share one base" REJECT
   s" EFFECT-POOL-TEST:BAD-REGISTRY-LIVE" 76
   s" tfam: two type-registry deltas cannot share one base" REJECT
   s" EFFECT-POOL-TEST:BAD-REGISTRY-SHORT" 76
   s" tfam: captured registry is shorter than its table" REJECT
   s" EFFECT-POOL-TEST:BAD-REGISTRY-BOUNDS" 76
   s" tfam: captured registry prefix exceeds its bytes" REJECT
   s" EFFECT-POOL-TEST:BAD-REGISTRY-PREFIX" 76
   s" tfam: captured registries have incompatible prefixes" REJECT
   s" EFFECT-POOL-TEST:PREFIX-REGISTRY" 0 s" prefix registry merge: ok" REJECT
   s" EFFECT-POOL-TEST:HOST-DELTA-REGISTRY" 0 s" host delta merge: ok" REJECT
   s" EFFECT-POOL-TEST:CANONICAL-REGISTRY" 0 s" canonical registry merge: ok" REJECT
   s" EFFECT-POOL-TEST:BAD-STAGING" 72 s" aot: encoded sections exceed their byte budget" REJECT ;

: RUN ( -- )
   SOURCE-TIER CAPTURE SAVE CHECK
   AOT-REG-LEN @ 0 > TTRUE
   s" complete real graph pool survives owned source release" T-LABEL
   AOT-FILE:OWN TRANSFER
   s" complete real graph pool survives file source release" T-LABEL
   KEY ART$ AOT-FILE:WRITE RELEASE KEY ART$ AOT-FILE:READ CHECK
   s" source writer stages empty and grown contiguous payloads" T-LABEL
   STAGING CHECK
   s" malformed extents fail before allocation or copy" T-LABEL
   REFUSALS
   s" merge preserves both complete pools and all rebased signature offsets" T-LABEL
   MERGE
   s" effect-pool: rows=" type WORDS . s" bytes=" type EXPECTED-U @ . cr
   EXPECTED-RELEASE SIG-STR-STORAGE-RELEASE AOT-SIG-PAYLOAD:STORAGE-RELEASE
   CLEANUP-RUN T-REPORT ;

RUN
;using
;using
;package
