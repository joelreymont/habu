\ Exact address-row IO and MERGE through files, with independently chosen fields.
\ Run with an artifact path and case name; the registered capture suite owns both.
require test/aot-artifact-roundtrip.f

package AOT-FILE

\ Forge only the old version and half-row cases, with honest lengths and digest.
: ROW-TEST-DIGEST ( -- )
   SHA256-RESET
   TBL SEC-N ROW-BYTES * SHA256-UPDATE
   SEC-N 0 ?do i SEC-PTR i BASE@ + i ROW-LEN@ SHA256-UPDATE loop
   PAYSHA SHA256-FINAL ;


: ROW-TEST-SHORTEN ( -- )
   S-XTOFFS ROW-OFF@ 4 S-XTOFFS ROW!
   SEC-N S-XTOFFS 1+ ?do
      i ROW-OFF@ 4 - i ROW-LEN@ i ROW!
   loop
   PAYLEN @ 4 - PAYLEN ! ;


public

: ROW-TEST-WRITE ( ptr u8 ptr u8 n bool -- )
   {: prod path pathu:n short:bool :}
   STAGE BUILD-TABLE BASES-ALONE
   short if ROW-TEST-SHORTEN then
   ROW-TEST-DIGEST
   DERIVED AOT-IDENT:CHAIN-DIGEST
   prod BUILD-HEADER
   short 0= if 8 HDR O-VERSION + U64! then
   path pathu PATH0 1537 493 open FD !
   FD @ 0 < if s" artifact-row-test: cannot write forged file" DIE then
   SHA256-RESET
   HDR HDR-BYTES PUT
   TBL SEC-N ROW-BYTES * PUT
   SEC-N 0 ?do i SEC-PTR i BASE@ + i ROW-LEN@ PUT loop
   FD @ close ;

;package

package AOT-ROW-TEST
using AOT-BUF
using AOT-WINDOW
using AOTRT
using AOT-FILE

8 constant ROWS
$40 constant HOST-BLOB
$23 constant HOST-DATA
variable HOST-D0
variable ART-D0

variable ART-BLOB
variable ART-DATA
variable MERGED-DATA
variable RAW-INDEX
variable RAW-SITE
variable RAW-VALUE
variable CHAIN-INDEX
variable CHAIN-SITE
variable CHAIN-VALUE

: CASE$ ( -- ptr u8 n ) 1 SCRIPT-ARGV$ ;


: CASE? ( ptr u8 n -- bool )
   CASE$ STR= ;


: ASSERT ( bool -- )
   if exit then
   s" artifact-row-test: exact address row mismatch" $4C die ;


: U32@ ( ptr u8 -- n )
   {: p :}
   p c@ p 1+ c@ 8 lshift or p 2 + c@ 16 lshift or p 3 + c@ 24 lshift or ;


: U32! ( n ptr u8 -- )
   {: value:n p :}
   4 0 ?do value i 8 * rshift p i + c! loop ;


: U64@ ( ptr u8 -- n )
   {: p :}
   0 8 0 ?do 8 lshift p 7 i - + c@ or loop ;


: DSITE@ ( n -- n )
   4 * AOT-DSITE-BUF@ + U32@ ;


: SAVE-DSITES ( -- )
   -1 RAW-INDEX ! -1 CHAIN-INDEX !
   AOT-DSITE-N @ 0 ?do
      i DSITE@ {: site:n :}
      site AOT-DSITE-CELL and 0<> if
         i RAW-INDEX ! site RAW-SITE !
      else
         i CHAIN-INDEX ! site CHAIN-SITE !
      then
   loop
   RAW-INDEX @ 0 >= CHAIN-INDEX @ 0 >= and ASSERT
   AOT-BLOB-BUF@ RAW-SITE @ AOT-DSITE-OFF-MASK and + U64@ RAW-VALUE !
   AOT-BLOB-BUF@ CHAIN-SITE @ + SNAP-RELOC:CHAINV CHAIN-VALUE ! ;


: ROW-AT ( n -- ptr u8 )
   XTOFF-ROW * XTOFF-BUF@ + ;


: ROW! ( n n n -- )
   {: loc:n target:n index:n :}
   loc index ROW-AT U32!
   target index ROW-AT 4 + U32! ;


: ROW= ( n n n -- )
   {: loc:n target:n index:n :}
   index ROW-AT U32@ loc = ASSERT
   index ROW-AT 4 + U32@ target = ASSERT ;


: MATRIX! ( -- )
   ROWS XTOFF-RESERVE
   XTOFF-WINDOW-TAG       1                              0 ROW!
   XTOFF-WINDOW-TAG 8 +   XTOFF-DATA-TAG 1+               1 ROW!
   XTOFF-WINDOW-TAG 16 +  0                              2 ROW!
   XTOFF-WINDOW-TAG 24 +  XTOFF-DATA-TAG                  3 ROW!
   $1000                 ART-BLOB @                     4 ROW!
   $1008                 ART-DATA @ XTOFF-DATA-TAG or    5 ROW!
   $1010                 0                              6 ROW!
   $1018                 XTOFF-DATA-TAG                  7 ROW!
   ROWS XTOFF-N ! ;


: MATRIX= ( -- )
   XTOFF-N @ ROWS = ASSERT
   XTOFF-WINDOW-TAG       1                              0 ROW=
   XTOFF-WINDOW-TAG 8 +   XTOFF-DATA-TAG 1+               1 ROW=
   XTOFF-WINDOW-TAG 16 +  0                              2 ROW=
   XTOFF-WINDOW-TAG 24 +  XTOFF-DATA-TAG                  3 ROW=
   $1000                 ART-BLOB @                     4 ROW=
   $1008                 ART-DATA @ XTOFF-DATA-TAG or    5 ROW=
   $1010                 0                              6 ROW=
   $1018                 XTOFF-DATA-TAG                  7 ROW= ;


: CAPTURE ( -- )
   KEY!
   CLOSURE!
   AOTRT:CAPTURE
   AOT-BLOB-LEN @ ART-BLOB !
   AOT-DATA-SIZE @ ART-DATA !
   AOT-DATA-D0 @ ART-D0 !
   SAVE-DSITES
   MATRIX! ;


: READ-ARTIFACT ( -- )
   CLEAR-XTOFFS
   FORGET-COUNTS
   AOTRT:KEY ART$ AOT-FILE:READ ;


: WRITE-ARTIFACT ( -- )
   AOTRT:KEY ART$ AOT-FILE:WRITE ;


: HOST! ( -- )
   FORGET-COUNTS
   1 AOT-REC-N !
   HOST-BLOB AOT-BLOB-LEN !
   HOST-DATA AOT-DATA-SIZE !
   ART-D0 @ $FF - dup HOST-D0 ! AOT-DATA-D0 !
   XTOFF-WINDOW-TAG XTOFF-DATA-TAG 1+ 0 ROW!
   1 XTOFF-N ! ;


: MERGED= ( -- )
   XTOFF-N @ ROWS 1+ = ASSERT
   WDATA-BASE MERGED-DATA !
   MERGED-DATA @ HOST-DATA >= ASSERT
   MERGED-DATA @ HOST-DATA - 8 < ASSERT
   HOST-D0 @ MERGED-DATA @ + ART-D0 @ - 7 and 0= ASSERT
   MERGED-DATA @ HOST-DATA > ASSERT
   AOT-BLOB-LEN @ ART-BLOB @ HOST-BLOB + = ASSERT
   AOT-DATA-SIZE @ ART-DATA @ MERGED-DATA @ + = ASSERT
   XTOFF-WINDOW-TAG XTOFF-DATA-TAG 1+ 0 ROW=
   XTOFF-WINDOW-TAG MERGED-DATA @ +
   HOST-BLOB 1+ 1 ROW=
   XTOFF-WINDOW-TAG MERGED-DATA @ + 8 +
   XTOFF-DATA-TAG MERGED-DATA @ + 1+ 2 ROW=
   XTOFF-WINDOW-TAG MERGED-DATA @ + 16 + 0 3 ROW=
   XTOFF-WINDOW-TAG MERGED-DATA @ + 24 + XTOFF-DATA-TAG 4 ROW=
   $1000 ART-BLOB @ HOST-BLOB + 5 ROW=
   $1008 ART-DATA @ MERGED-DATA @ + XTOFF-DATA-TAG or 6 ROW=
   $1010 0 7 ROW=
   $1018 XTOFF-DATA-TAG 8 ROW= ;


: REBASED-DATA ( n -- n )
   ART-D0 @ - HOST-D0 @ + MERGED-DATA @ + ;


: MERGED-DSITES= ( -- )
   RAW-INDEX @ DSITE@ RAW-SITE @ HOST-BLOB + = ASSERT
   CHAIN-INDEX @ DSITE@ CHAIN-SITE @ HOST-BLOB + = ASSERT
   AOT-BLOB-BUF@ RAW-SITE @ AOT-DSITE-OFF-MASK and + HOST-BLOB + U64@
   RAW-VALUE @ REBASED-DATA = ASSERT
   AOT-BLOB-BUF@ CHAIN-SITE @ + HOST-BLOB + SNAP-RELOC:CHAINV
   CHAIN-VALUE @ REBASED-DATA = ASSERT ;


: MATRIX-ROUNDTRIP ( -- )
   WRITE-ARTIFACT READ-ARTIFACT MATRIX=
   HOST!
   AOTRT:KEY ART$ MERGE
   MERGED=
   MERGED-DSITES=
   s" address-rows: merge=ok" type cr ;


: FRESH ( -- )
   KEY!
   READ-ARTIFACT
   AOT-BLOB-LEN @ ART-BLOB !
   AOT-DATA-SIZE @ ART-DATA !
   MATRIX=
   s" address-rows: fresh=ok" type cr ;


: FORGE-ROW ( -- )
   s" bad-window" CASE? if
      XTOFF-WINDOW-TAG ART-DATA @ + 7 - 0 0 ROW! exit
   then
   s" bad-fixed" CASE? if
      SNAP-RELOC:XTCELL-OFF-MAX 1+ 0 0 ROW! exit
   then
   s" bad-data" CASE? if
      XTOFF-WINDOW-TAG ART-DATA @ 1+ XTOFF-DATA-TAG or 0 ROW! exit
   then
   s" bad-code" CASE? if
      XTOFF-WINDOW-TAG ART-BLOB @ 1+ 0 ROW! exit
   then
   s" artifact-row-test: unknown case" $4C die ;


: FORGE-SITE ( -- bool )
   s" bad-data-site" CASE? if
      ART-BLOB @ 7 - AOT-DSITE-CELL or
      RAW-INDEX @ 4 * AOT-DSITE-BUF@ + U32! true exit
   then
   s" bad-chain-site" CASE? if
      ART-BLOB @ SNAP-RELOC:ADDR-CHAIN-BYTES - 1+
      CHAIN-INDEX @ 4 * AOT-DSITE-BUF@ + U32! true exit
   then
   false ;


public

: RUN ( -- )
   s" fresh" CASE? if FRESH exit then
   CAPTURE
   s" matrix" CASE? if MATRIX-ROUNDTRIP exit then
   s" old-version" CASE? if
      AOTRT:KEY ART$ false ROW-TEST-WRITE READ-ARTIFACT exit
   then
   s" short-row" CASE? if
      1 XTOFF-N !
      AOTRT:KEY ART$ true ROW-TEST-WRITE READ-ARTIFACT exit
   then
   FORGE-SITE if
      WRITE-ARTIFACT HOST!
      AOTRT:KEY ART$ MERGE exit
   then
   FORGE-ROW
   WRITE-ARTIFACT READ-ARTIFACT ;

;using
;using
;using
;using
;package

AOT-ROW-TEST:RUN
