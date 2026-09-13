\ A frozen capture passed from host readers to a later source-bound writer.
\ These are the existing artifact sections in memory, without a disk stage.
\ OWN copies every used byte; later capture/writer storage cannot change it.
require lib/memory.f
require src/habu/aot-file.f

package AOT-OWNED
public
STRUCTURE capture 0
   FIELD bytes ptr u8
   FIELD size n
;STRUCTURE
;package

package AOT-FILE
using AOT-BUF
using AOT-WINDOW

\ The file reader and this in-process transfer share the section shape, bounds,
\ row widths, and count restoration. No second interpretation of a row lives here.
: COPY-OWNED ( ptr u8 -- ) {: dst:ptr :}
   TBL dst SEC-N ROW-BYTES * BYTE-COPY
   SEC-N 0 ?do
      i ROW-LEN@ 0 > if
         i SEC-PTR i BASE@ + dst i ROW-OFF@ + i ROW-LEN@ BYTE-COPY
      then
   loop ;

: IMPORT-SECTION ( ptr u8 n -- ) {: src:ptr k:n :}
   k ROW-LEN@ 0= if exit then
   k S-NAMES = if k ROW-LEN@ AOT-NAMES-RESERVE then
   src k ROW-OFF@ + k SEC-PTR k BASE@ + k ROW-LEN@ BYTE-COPY ;

public

: OWN ( -- AOT-OWNED:capture )
   STAGE BUILD-TABLE BASES-ALONE
   SEC-N 0 ?do i ?ROOM loop
   PAYLEN @ MEM-ALLOC-BYTES {: dst:ptr size:n :}
   dst COPY-OWNED
   dst size AOT--OWNED-CAPTURE:MAKE ;

: IMPORT ( AOT-OWNED:capture -- )
   AOT--OWNED-CAPTURE:UNMAKE {: src:ptr size:n :}
   size SEC-N ROW-BYTES * < if s" aot-owned: capture has no section table" DIE then
   -1 FD !
   size PAYLEN !
   src TBL SEC-N ROW-BYTES * BYTE-COPY
   ?TABLE
   S-SCALARS SCAL-BYTES ?EXACT
   BASES-ALONE
   SEC-N 0 ?do i ?ROOM loop
   SEC-N 0 ?do src i IMPORT-SECTION loop
   SCAL 32 + U64@ {: span:n :}
   span ?SPAN
   0 S-WDATA ROW-LEN@ 8 / span ?RUNS
   XTOFF-BUF@ S-XTOFFS ROW-LEN@ XTOFF-ROW /
   span S-BLOB ROW-LEN@ ?XTOFFS
   RESTORE-COUNTS
   S-CLOSURE ROW-LEN@ CLEN !
   RESTORE-CLOSURE ;

;using
;using
;package

package AOT-OWNED
public

: CLOSE ( capture -- )
   AOT--OWNED-CAPTURE:UNMAKE munmap 0<> if E-MEM-UNMAP throw then ;

;package
