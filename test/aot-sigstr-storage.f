\ Signature-string section storage and its bounds, using an inert code blob.
\ No host instructions are scanned or executed.
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

: CASE? ( ptr u8 n -- bool ) 1 SCRIPT-ARGV$ STR= ;

: SOURCE ( -- )
   AOT-IDENT:RESET s" src/habu/aot-decl.f" AOT-IDENT:PATH+
   4 AOT-BLOB-LEN ! $D65F03C0 AOT-BLOB-BUF@ CELL-VIEW !
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

: RUN ( -- )
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

RUN
;using
;using
;package
