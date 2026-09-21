\ Actual row append/WRITE/READ/IMPORT/MERGE beyond the former fixed limit.
\ The synthetic rows are valid fixed-location null pointers; no code executes.
require lib/test.f
require src/os/script-argv.f
require src/arch/arm64/asm.f
require src/arch/arm64/icode.f
require src/habu/layout.f
require src/habu/aot-decl.f
require src/habu/aot-arm.f
require src/habu/aot-capture.f
require src/habu/aot-ident.f
require src/habu/fdio.f
require src/habu/aot-owned.f
require lib/fs.f

package AOT-CAPTURE
public
: ADDRESS-TEST+ ( n n -- ) ACAP-ADD-XTOFF ;
;package

package AOT-FILE
public
: ADDRESS-TEST-HEADER ( ptr u8 n -- )
   AOT-SECTION-CAP HDR O-PAYLEN + U64!
   HDR HDR-BYTES WRITE-ALL ;

\ Every section has a valid individual width/cap and the table fills this real
\ allocation. The aggregate must refuse before a destination buffer is touched.
: ADDRESS-TEST-OWNED ( -- AOT-OWNED:capture )
   STAGE BUILD-TABLE
   SEC-N ROW-BYTES * CUR !
   SEC-N 0 ?do
      i S-XTOFFS = if AOT-SECTION-CAP else i ROW-LEN@ then {: bytes:n :}
      CUR @ bytes i ROW! CUR @ bytes + CUR !
   loop
   CUR @ MEM-ALLOC-BYTES {: dst:ptr size:n :}
   TBL dst SEC-N ROW-BYTES * BYTE-COPY
   dst size -1 AOT--OWNED-CAPTURE:MAKE ;
;package

package AOT-ADDRESS-CELLS-TEST
using AOT-BUF
using AOT-WINDOW
75900 constant ROWS
create KEY 32 allot
: CASE? ( ptr u8 n -- bool ) 1 SCRIPT-ARGV$ STR= ;
: ROW ( n -- ptr n ) cells XTOFF-BUF@ + CELL-VIEW ;
: VALUE ( n -- n ) {: k:n :}
   $100000 k cells + k 1 and 63 lshift or ;
: SOURCE ( n -- ) {: count:n :}
   AOT-IDENT:RESET s" src/habu/aot-decl.f" AOT-IDENT:PATH+
   \ One AArch64 RET instruction gives this nonexecuted fixture a valid code blob.
   4 AOT-BLOB-LEN ! $D65F03C0 AOT-BLOB-BUF@ CELL-VIEW !
   0 AOT-REC-N ! 0 AOT-SITE-N ! 0 AOT-NAMES-LEN !
   0 AOT-DSITE-N ! 0 AOT-CSITE-N !
   0 AOT-CODE-B0 ! 0 AOT-DATA-D0 ! 0 AOT-DATA-SIZE !
   0 AOT-WID-W0 ! 0 AOT-WID-SPAN !
   WINDOW-RESET 0 XTOFF-N !
   0 AOT-XTSITE:N ! 0 AOT-BOOTRUN-LEN ! 0 AOT-PWIN-N !
   0 AOT-SIG-N ! 0 AOT-SIG-STR-LEN ! 0 AOT-REG-LEN !
   count 0 ?do $100000 i cells + i 1 and 31 lshift AOT-CAPTURE:ADDRESS-TEST+ loop ;
: CHECK ( n -- ) {: first:n :}
   XTOFF-N @ ROWS first + T=
   ROWS 0 ?do first i + ROW @ i VALUE T= loop ;
: RELEASE ( -- ) 0 XTOFF-N ! XTOFF-STORAGE-RELEASE ;
: TRANSFER ( AOT-OWNED:capture -- )
   RELEASE dup AOT-FILE:IMPORT 0 CHECK AOT-OWNED:CLOSE ;
: WRITE ( -- ) KEY 0 SCRIPT-ARGV$ AOT-FILE:WRITE ;
: READ ( -- ) KEY 0 SCRIPT-ARGV$ AOT-FILE:READ ;
: MERGE ( -- ) KEY 0 SCRIPT-ARGV$ AOT-FILE:MERGE ;
: RUN ( -- )
   T-RESET
   s" reserve-negative" CASE? if -1 XTOFF-RESERVE exit then
   s" reserve-overflow" CASE? if $7FFFFFFFFFFFFFFF XTOFF-RESERVE exit then
   s" reserve-limit" CASE? if XTOFF-MAX 1+ XTOFF-RESERVE exit then
   0 SOURCE
   s" budget-write" CASE? if XTOFF-MAX XTOFF-N ! WRITE exit then
   s" budget-owned" CASE? if XTOFF-MAX XTOFF-N ! AOT-FILE:OWN AOT-OWNED:CLOSE exit then
   s" budget-read" CASE? if
      WRITE 0 SCRIPT-ARGV$ AOT-FILE:ADDRESS-TEST-HEADER READ exit then
   s" budget-import" CASE? if
      AOT-FILE:ADDRESS-TEST-OWNED dup AOT-FILE:IMPORT AOT-OWNED:CLOSE exit then
   ROWS SOURCE 0 CHECK WRITE
   s" budget-merge" CASE? if
      XTOFF-MAX ROWS - XTOFF-N ! 1 AOT-REC-N ! MERGE exit then
   RELEASE READ 0 CHECK
   AOT-FILE:OWN TRANSFER
   1 SOURCE 1 AOT-REC-N ! $F00000 0 ROW !
   MERGE 1 CHECK
   0 ROW @ $F00000 T=
   XTOFF-STORAGE-RELEASE
   T-REPORT s" aot-address-cells: ok" type cr ;
RUN
;using
;using
;package
