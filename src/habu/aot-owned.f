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
   FIELD origin n
;STRUCTURE

\ Same refusal as ENGINE-ERROR:IMAGE-CODE-ORIGIN. This module also loads in a
\ bootstrap host that predates that engine primitive and its named constant.
100 constant ORIGIN-RC
: ORIGIN-REFUSE ( ptr u8 n -- ) ORIGIN-RC die ;

: BYTES$ ( capture -- ptr u8 n ) AOT--OWNED-CAPTURE:UNMAKE drop ;
: ORIGIN@ ( capture -- n ) AOT--OWNED-CAPTURE:UNMAKE nip nip ;
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

: OWN-AS ( n -- AOT-OWNED:capture ) {: origin:n :}
   STAGE BUILD-TABLE BASES-ALONE
   SEC-N 0 ?do i ?ROOM loop
   PAYLEN @ MEM-ALLOC-BYTES {: dst:ptr size:n :}
   dst COPY-OWNED
   dst size origin AOT--OWNED-CAPTURE:MAKE ;

public

\ Disk reads and ordinary copies supply no evidence about the generating tier.
: OWN ( -- AOT-OWNED:capture ) -1 OWN-AS ;

\ Called immediately after the real capture audits, with its frozen live code
\ bounds. Only the explicit bootstrap entry can carry an unknown origin; it
\ cannot turn a JIT or invalid answer into a native claim.
: OWN-WINDOW ( n n [ n n -- n ] bool -- AOT-OWNED:capture )
   {: first:n end:n query bootstrap:bool :}
   first 0 < end first < or if
      S\" aot-owned: invalid live code window\n" AOT-OWNED:ORIGIN-REFUSE then
   end first - AOT-BLOB-LEN @ <> if
      S\" aot-owned: copied code differs from the frozen window extent\n" AOT-OWNED:ORIGIN-REFUSE then
   first end query execute {: origin:n :}
   origin 1 = if 1 OWN-AS exit then
   origin -1 = bootstrap and if OWN exit then
   S\" aot-owned: captured code lacks native provenance\n" AOT-OWNED:ORIGIN-REFUSE ;

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
   BYTES$ munmap 0<> if E-MEM-UNMAP throw then ;

;package
