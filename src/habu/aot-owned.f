\ A frozen capture passed from host readers to a later source-bound writer.
\ These are the existing artifact sections in memory, without a disk stage.
\ OWN copies every used byte; later capture/writer storage cannot change it.
require src/lib/errors.f
require src/lib/memory.f
require src/habu/aot-decl.f
require src/habu/aot-file.f

package AOT-OWNED
public
STRUCTURE capture
   FIELD bytes ptr u8
   FIELD size n
;STRUCTURE

: BYTES$ ( capture -- ptr u8 n ) AOT--OWNED-CAPTURE:UNMAKE ;
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
   k RESERVE-SECTION
   k ROW-LEN@ 0= if exit then
   src k ROW-OFF@ + k SEC-PTR k BASE@ + k ROW-LEN@ BYTE-COPY ;

public

: OWN ( -- AOT-OWNED:capture )
   STAGE BUILD-TABLE BASES-ALONE
   SEC-N 0 ?do i ?ROOM loop
   0 0 <> ?BUDGET
   ?ADDRESS-ROWS
   PAYLEN @ MEM-ALLOC-BYTES {: dst:ptr size:n :}
   dst COPY-OWNED
   dst size AOT--OWNED-CAPTURE:MAKE ;

\ Called immediately after the real capture audits, with its frozen live code
\ bounds and the compactor's exact copied extent.
: OWN-WINDOW ( n n n -- AOT-OWNED:capture )
   {: first:n end:n copied:n :}
   first 0 < end first < or if
      s" aot-owned: invalid live code window" DIE then
   copied 0 < copied end first - > or copied AOT-BLOB-LEN @ <> or if
      s" aot-owned: copied code differs from the compacted window extent" DIE then
   OWN ;

: IMPORT ( AOT-OWNED:capture -- )
   AOT-OWNED:BYTES$ {: src:ptr size:n :}
   size SEC-N ROW-BYTES * < if s" aot-owned: capture has no section table" DIE then
   -1 FD !
   size PAYLEN !
   src TBL SEC-N ROW-BYTES * BYTE-COPY
   ?TABLE
   S-SCALARS SCAL-BYTES ?EXACT
   BASES-ALONE
   SEC-N 0 ?do i ?ROOM loop
   0 0 <> ?BUDGET
   SEC-N 0 ?do src i IMPORT-SECTION loop
   SCAL 32 + U64@ {: span:n :}
   span ?SPAN
   0 S-WDATA ROW-LEN@ span ?CELLS
   ?ADDRESS-ROWS
   RESTORE-COUNTS
   S-CLOSURE ROW-LEN@ CLEN !
   RESTORE-CLOSURE ;

;using
;using
;package

package AOT-OWNED
public

: CLOSE ( capture -- )
   BYTES$ munmap 0<> if E-MEM-UNMAP throw then ;

;package
