\ Signature-string section storage and its bounds, using an inert code blob.
\ No host instructions are scanned or executed.
require lib/test.f
require lib/string.f
require lib/test/outcome.f
require lib/test/subject.f
require lib/fs-mutate.f
require lib/fmt.f
require src/os/script-argv.f
require src/habu/layout.f
require src/habu/aot-decl.f
require src/habu/aot-arm.f
require src/habu/aot-ident.f
require src/habu/fdio.f
require src/habu/aot-owned.f
require src/habu/aot-sig-payload.f
require lib/fs.f

package AOT-FILE
public
: SIGSTR-TEST-HEADER ( ptr u8 n -- )
   AOT-SECTION-CAP HDR O-PAYLEN + U64!
   HDR HDR-BYTES WRITE-ALL ;

: SIGSTR-TEST-OWNED ( -- AOT-OWNED:capture )
   STAGE BUILD-TABLE
   SEC-N ROW-BYTES * CUR !
   SEC-N 0 ?do
      i S-SIGSTR = if AOT-SECTION-CAP else i ROW-LEN@ then {: bytes:n :}
      CUR @ bytes i ROW! CUR @ bytes + CUR !
   loop
   CUR @ MEM-ALLOC-BYTES {: dst:ptr size:n :}
   TBL dst SEC-N ROW-BYTES * BYTE-COPY
   dst size -1 AOT--OWNED-CAPTURE:MAKE ;
;package

package AOT-SIGSTR-STORAGE-TEST
using AOT-BUF
using AOT-WINDOW

create KEY 32 allot
1536 constant WORDS
create ROOT FS-PATH-CAP allot variable ROOT-U
create DEFS FS-PATH-CAP allot variable DEFS-U
create HOST-ART FS-PATH-CAP allot variable HOST-ART-U
DYNAMIC-BUFFER EXPECTED n
variable EXPECTED-U
create EXPECTED-ROWS WORDS SIG-ROW * allot
create EXPECTED-REG AOT-REG-CAP allot variable EXPECTED-REG-U
$1000 constant IO-CAP
create OUT IO-CAP allot create ERR IO-CAP allot

: CASE? ( ptr u8 n -- bool ) 1 SCRIPT-ARGV$ STR= ;
: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;
: DEFS$ ( -- ptr u8 n ) DEFS DEFS-U @ ;
: ART$ ( -- ptr u8 n ) 0 SCRIPT-ARGV$ ;
: HOST-ART$ ( -- ptr u8 n ) HOST-ART HOST-ART-U @ ;
: EXPECTED$ ( -- ptr u8 n ) 0 EXPECTED BYTE-VIEW EXPECTED-U @ ;
: POOL$ ( -- ptr u8 n ) AOT-SIG-STR-BUF@ AOT-SIG-STR-LEN @ ;

: NAME+ ( n -- ) {: k:n :}
   4 0 ?do k 3 i - 4 * rshift $F and s" 0123456789ABCDEF" drop + c@ SB-APPEND-C loop ;

: GENERATE ( -- )
   s" habu-sigstr-storage" HB-TMP-MKDIR {: path:ptr u:n :}
   path ROOT u BYTE-COPY u ROOT-U ! ROOT$ CLEANUP-TREE+
   ROOT$ s" definitions.f" DEFS JOIN-PATH DEFS-U !
   ROOT$ s" host.aot" HOST-ART JOIN-PATH HOST-ART-U !
   DEFS$ S\" package SIGSTR-STORAGE-WINDOW public\nNEWTYPE effect-tag 0\n" WRITE-ALL
   WORDS 0 ?do
      SB-RESET s" : EFFECT-" SB-APPEND i NAME+
      S\"  ( n n n n -- n n n n ) 1+ ;\n" SB-APPEND
      DEFS$ SB$ APPEND-FILE
   loop
   DEFS$ S\" ;package\n" APPEND-FILE ;

: U32@ ( ptr u8 -- n ) {: p:ptr :}
   p c@ p 1+ c@ 8 lshift or p 2 + c@ 16 lshift or p 3 + c@ 24 lshift or ;
: U64@ ( ptr u8 -- n ) {: p:ptr :}
   0 8 0 ?do 8 lshift p 7 i - + c@ or loop ;

: SOURCE ( -- )
   AOT-IDENT:RESET s" src/habu/aot-decl.f" AOT-IDENT:PATH+
   4 AOT-BLOB-LEN ! $01020304 AOT-BLOB-BUF@ CELL-VIEW !
   0 AOT-REC-N ! 0 AOT-SITE-N ! 0 AOT-NAMES-LEN !
   0 AOT-DSITE-N ! 0 AOT-CSITE-N !
   0 AOT-CODE-B0 ! 0 AOT-DATA-D0 ! 0 AOT-DATA-SIZE !
   0 AOT-WID-W0 ! 0 AOT-WID-SPAN !
   WINDOW-RESET 0 XTOFF-N !
   0 AOT-XTSITE:N ! 0 AOT-BOOTRUN-LEN ! 0 AOT-PWIN-N !
   0 AOT-SIG-N ! 0 AOT-SIG-STR-LEN ! 0 AOT-REG-LEN ! ;

: WRITE ( -- ) KEY 0 SCRIPT-ARGV$ AOT-FILE:WRITE ;
: READ ( -- ) KEY 0 SCRIPT-ARGV$ AOT-FILE:READ ;
: CHECK ( -- )
   AOT-SIG-STR-LEN @ 3 T=
   AOT-SIG-STR-BUF@ 3 s" abc" STR= TTRUE ;

: LARGE-SOURCE ( -- )
   SOURCE GENERATE
   3 AOT-SIG-STR-RESERVE
   s" abc" drop AOT-SIG-STR-BUF@ 3 BYTE-COPY
   AOT-ARM:WINDOW-OPEN-UNARMED
   DEFS$ included
   AOT-ARM:WINDOW-CLOSE
   AOT-ARM:PAYLOAD-SPANS {: rows:ptr rowu:n strings:ptr stru:n :}
   rowu WORDS SIG-ROW * T=
   stru $40000 > TTRUE
   stru AOT-SIG-STR-RESERVE
   rows AOT-SIG-BUF@ rowu BYTE-COPY
   strings AOT-SIG-STR-BUF@ stru BYTE-COPY
   WORDS AOT-SIG-N ! stru AOT-SIG-STR-LEN !
   AOT-REG-BUF@ AOT-REG-CAP AOT-ARM:PAYLOAD-REG-SAVE AOT-REG-LEN !
   AOT-REG-LEN @ 0 > TTRUE
   DEFS$ AOT-IDENT:PATH+ ;

: SAVE-LARGE ( -- )
   AOT-SIG-STR-LEN @ dup EXPECTED-U ! CELL 1- + CELL / EXPECTED-RESERVE
   AOT-SIG-STR-BUF@ EXPECTED$ BYTE-COPY
   AOT-SIG-BUF@ EXPECTED-ROWS WORDS SIG-ROW * BYTE-COPY
   AOT-REG-LEN @ EXPECTED-REG-U !
   AOT-REG-BUF@ EXPECTED-REG AOT-REG-LEN @ BYTE-COPY ;

: CHECK-LARGE ( -- )
   AOT-SIG-N @ WORDS T=
   POOL$ EXPECTED$ STR= TTRUE
   AOT-SIG-BUF@ WORDS SIG-ROW * EXPECTED-ROWS WORDS SIG-ROW * STR= TTRUE
   AOT-REG-BUF@ AOT-REG-LEN @ EXPECTED-REG EXPECTED-REG-U @ STR= TTRUE ;

: RELEASE-LARGE ( -- )
   0 AOT-SIG-N ! 0 AOT-SIG-STR-LEN ! 0 AOT-REG-LEN !
   SIG-STR-STORAGE-RELEASE ;

: TRANSFER-LARGE ( AOT-OWNED:capture -- )
   RELEASE-LARGE dup AOT-FILE:IMPORT CHECK-LARGE AOT-OWNED:CLOSE ;

: STAGING ( -- )
   AOT-SIG-PAYLOAD:STORAGE-RELEASE
   RELEASE-LARGE
   AOT-SIG-PAYLOAD:BUILD AOT-SIG-PAYLOAD:LEN @ 0 T=
   1 AOT-SIG-N ! AOT-SIG-PAYLOAD:BUILD
   AOT-SIG-PAYLOAD:LEN @ 56 SIG-ROW + T=
   EXPECTED-U @ AOT-SIG-STR-RESERVE
   EXPECTED$ {: saved:ptr savedu:n :}
   saved AOT-SIG-STR-BUF@ savedu BYTE-COPY
   WORDS AOT-SIG-N ! EXPECTED-U @ AOT-SIG-STR-LEN !
   EXPECTED-REG AOT-REG-BUF@ EXPECTED-REG-U @ BYTE-COPY
   EXPECTED-REG-U @ AOT-REG-LEN !
   AOT-SIG-PAYLOAD:BUILD
   AOT-SIG-PAYLOAD:LEN @ $A0038 > TTRUE
   AOT-SIG-PAYLOAD:BUF@ {: p:ptr :}
   p U64@ 3 T=
   p 8 + U64@ 56 T= p 16 + U64@ WORDS SIG-ROW * T=
   p 24 + U64@ 56 WORDS SIG-ROW * + T=
   p 32 + U64@ EXPECTED-U @ T=
   p 40 + U64@ 56 WORDS SIG-ROW * + EXPECTED-U @ + T=
   p 48 + U64@ EXPECTED-REG-U @ T=
   p 56 + WORDS SIG-ROW * EXPECTED-ROWS WORDS SIG-ROW * STR= TTRUE
   p p 24 + U64@ + EXPECTED-U @ EXPECTED$ STR= TTRUE
   p p 40 + U64@ + EXPECTED-REG-U @ EXPECTED-REG EXPECTED-REG-U @ STR= TTRUE
   AOT-SIG-PAYLOAD:LEN @ 56 WORDS SIG-ROW * + EXPECTED-U @ + EXPECTED-REG-U @ + T= ;

: MERGE-LARGE ( -- )
   EXPECTED-U @ 7 + -8 and {: base:n :}
   EXPECTED-U @ base < TTRUE
   0 AOT-REG-LEN ! 1 AOT-REC-N !
   KEY ART$ AOT-FILE:MERGE
   AOT-REG-LEN @ EXPECTED-REG-U @ T=
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
: BAD-STAGING ( -- )
   AOT-SIG-STR-CAP AOT-SIG-STR-LEN ! AOT-SIG-PAYLOAD:BUILD ;
: BAD-REGISTRY ( -- )
   1 AOT-REC-N ! KEY ART$ AOT-FILE:MERGE ;
: BAD-REGISTRY-LIVE ( -- )
   AOT-ARM:WINDOW-OPEN-UNARMED AOT-ARM:WINDOW-CLOSE
   BAD-REGISTRY ;
: BAD-REGISTRY-SHORT ( -- )
   8 AOT-REG-LEN ! BAD-REGISTRY ;
: BAD-REGISTRY-BOUNDS ( -- )
   $7FFFFFFFFFFFFFFF AOT-REG-BUF@ 8 + CELL-VIEW !
   BAD-REGISTRY ;
: MAKE-PREFIX ( -- )
   AOT-ARM:WINDOW-OPEN-UNARMED AOT-ARM:WINDOW-CLOSE
   AOT-REG-BUF@ AOT-REG-CAP AOT-ARM:PAYLOAD-REG-SAVE AOT-REG-LEN ! ;
: CHECK-REGISTRY ( -- )
   AOT-REG-BUF@ AOT-REG-LEN @ EXPECTED-REG EXPECTED-REG-U @ STR= 0= if 79 throw then
   WORDS 2 * AOT-SIG-N @ <> if 79 throw then ;
: PREFIX-REGISTRY ( -- )
   MAKE-PREFIX
   1 AOT-REC-N ! KEY ART$ AOT-FILE:MERGE
   CHECK-REGISTRY s" prefix registry merge: ok" type cr ;
: HOST-DELTA-REGISTRY ( -- )
   MAKE-PREFIX KEY HOST-ART$ AOT-FILE:WRITE
   EXPECTED-REG AOT-REG-BUF@ EXPECTED-REG-U @ BYTE-COPY
   EXPECTED-REG-U @ AOT-REG-LEN !
   1 AOT-REC-N ! KEY HOST-ART$ AOT-FILE:MERGE
   CHECK-REGISTRY s" host delta merge: ok" type cr ;
: BAD-REGISTRY-PREFIX ( -- )
   MAKE-PREFIX
   AOT-REG-BUF@ 200 + dup c@ 1 xor swap c!
   1 AOT-REC-N ! KEY ART$ AOT-FILE:MERGE ;
: CANONICAL-REGISTRY ( -- )
   MAKE-PREFIX
   \ TF.TAILNEXT is rebuilt, not part of the captured family identity.
   AOT-REG-BUF@ 200 152 + + dup c@ 1 xor swap c!
   1 AOT-REC-N ! KEY ART$ AOT-FILE:MERGE
   CHECK-REGISTRY s" canonical registry merge: ok" type cr ;

private
: REJECT ( ptr u8 n n ptr u8 n -- ) {: src:ptr srcu:n code:n message:ptr u:n :}
   src srcu OUT IO-CAP >LEN ERR IO-CAP >LEN 5000 >MS SUBJECT:RUN {: outu:len erru:len oc :}
   src srcu OUT outu LEN>N ERR erru LEN>N oc code T-OUTCOME-EXITED=
   OUT outu LEN>N message u CONTAINS? ERR erru LEN>N message u CONTAINS? or TTRUE
   CHECK-LARGE ;

: REFUSALS ( -- )
   s" AOT-SIGSTR-STORAGE-TEST:BAD-STAGING" 72
   s" aot: encoded sections exceed their byte budget" REJECT
   s" AOT-SIGSTR-STORAGE-TEST:BAD-REGISTRY" 76
   s" tfam: two type-registry deltas cannot share one base" REJECT
   s" AOT-SIGSTR-STORAGE-TEST:BAD-REGISTRY-LIVE" 76
   s" tfam: two type-registry deltas cannot share one base" REJECT
   s" AOT-SIGSTR-STORAGE-TEST:BAD-REGISTRY-SHORT" 76
   s" tfam: captured registry is shorter than its table" REJECT
   s" AOT-SIGSTR-STORAGE-TEST:BAD-REGISTRY-BOUNDS" 76
   s" tfam: captured registry prefix exceeds its bytes" REJECT
   s" AOT-SIGSTR-STORAGE-TEST:BAD-REGISTRY-PREFIX" 76
   s" tfam: captured registries have incompatible prefixes" REJECT
   s" AOT-SIGSTR-STORAGE-TEST:PREFIX-REGISTRY" 0
   s" prefix registry merge: ok" REJECT
   s" AOT-SIGSTR-STORAGE-TEST:HOST-DELTA-REGISTRY" 0
   s" host delta merge: ok" REJECT
   s" AOT-SIGSTR-STORAGE-TEST:CANONICAL-REGISTRY" 0
   s" canonical registry merge: ok" REJECT ;

: LARGE ( -- )
   T-RESET CLEANUP-RESET
   LARGE-SOURCE SAVE-LARGE CHECK-LARGE
   s" large real graph pool survives owned transfer" T-LABEL
   AOT-FILE:OWN TRANSFER-LARGE
   s" large real graph pool survives file transfer" T-LABEL
   WRITE RELEASE-LARGE READ CHECK-LARGE
   s" source writer stages one contiguous graph payload" T-LABEL
   STAGING CHECK-LARGE
   s" source stage and registry merge refusals" T-LABEL
   REFUSALS
   s" merge aligns the graph pool and rebases every row" T-LABEL
   MERGE-LARGE
   s" sigstr-storage: rows=" type WORDS FMT:.INT
   s"  bytes=" type EXPECTED-U @ FMT:.INT cr
   EXPECTED-RELEASE SIG-STR-STORAGE-RELEASE AOT-SIG-PAYLOAD:STORAGE-RELEASE
   CLEANUP-RUN T-REPORT s" aot-sigstr-large: ok" type cr ;

public
: RUN ( -- )
   s" large" CASE? if LARGE exit then
   T-RESET
   s" reserve-negative" CASE? if -1 AOT-SIG-STR-RESERVE exit then
   s" reserve-overflow" CASE? if $7FFFFFFFFFFFFFFF AOT-SIG-STR-RESERVE exit then
   s" reserve-limit" CASE? if AOT-SIG-STR-CAP 1+ AOT-SIG-STR-RESERVE exit then
   SOURCE
   s" budget-write" CASE? if AOT-SIG-STR-CAP AOT-SIG-STR-LEN ! WRITE exit then
   s" budget-owned" CASE? if
      AOT-SIG-STR-CAP AOT-SIG-STR-LEN ! AOT-FILE:OWN AOT-OWNED:CLOSE exit then
   s" budget-import" CASE? if
      AOT-FILE:SIGSTR-TEST-OWNED dup AOT-FILE:IMPORT AOT-OWNED:CLOSE exit then
   3 AOT-SIG-STR-RESERVE
   s" abc" drop AOT-SIG-STR-BUF@ 3 BYTE-COPY
   3 AOT-SIG-STR-LEN ! CHECK WRITE
   s" budget-read" CASE? if
      0 SCRIPT-ARGV$ AOT-FILE:SIGSTR-TEST-HEADER READ exit then
   s" budget-merge" CASE? if
      AOT-SIG-STR-CAP 3 - -8 and AOT-SIG-STR-LEN !
      1 AOT-REC-N ! KEY 0 SCRIPT-ARGV$ AOT-FILE:MERGE exit then
   0 AOT-SIG-STR-LEN ! SIG-STR-STORAGE-RELEASE
   READ CHECK
   AOT-FILE:OWN
   0 AOT-SIG-STR-LEN ! SIG-STR-STORAGE-RELEASE
   dup AOT-FILE:IMPORT CHECK AOT-OWNED:CLOSE
   SIG-STR-STORAGE-RELEASE
   T-REPORT s" aot-sigstr-storage: ok" type cr ;

;using
;using
;package

AOT-SIGSTR-STORAGE-TEST:RUN
