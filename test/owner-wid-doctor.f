\ owner-wid-doctor.f - malformed persisted owner-image fixtures.

require lib/errors.f
require lib/string.f
require lib/adt/option.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/codesign.f
require test/owner-wid-image.f

package OWNER-WID-DOCTOR

create AOT-BAD-BUF FS-PATH-CAP allot
create AOT-MAL-BUF FS-PATH-CAP allot
create SNAP-OLD-BUF FS-PATH-CAP allot
create SNAP-BAD-BUF FS-PATH-CAP allot
create SNAP-MAL-BUF FS-PATH-CAP allot
create SNAP-MAG-BUF FS-PATH-CAP allot
create SNAP-WID1-BUF FS-PATH-CAP allot
create SNAP-WID2-BUF FS-PATH-CAP allot
create SNAP-DUP-BUF FS-PATH-CAP allot
create SNAP-PTR-BUF FS-PATH-CAP allot
variable AOT-BAD-U
variable AOT-MAL-U
variable SNAP-OLD-U
variable SNAP-BAD-U
variable SNAP-MAL-U
variable SNAP-MAG-U
variable SNAP-WID1-U
variable SNAP-WID2-U
variable SNAP-DUP-U
variable SNAP-PTR-U
variable IMG-A
variable IMG-U
variable SCAN-I
variable SCAN-LAST

: IMG-A-FIELD ( -- ptr ptr u8 )
   IMG-A 0 ptr-field ;

: IMG ( -- ptr u8 )
   IMG-A-FIELD @ ;

: FIND-AFTER ( ptr u8 n n ptr u8 n -- option<idx> )
   {: a:ptr u:n start:n needle:ptr nu:n :}
   start 0 < if OPTION:NONE exit then
   start u >= if OPTION:NONE exit then
   a start BYTE+ u start - needle nu FIND-SUB MATCH option
     none OF OPTION:NONE ENDOF
     some OF IDX>N start + >IDX OPTION:SOME ENDOF
   ;MATCH ;

: MAGIC-STEP ( ptr u8 n -- bool ) {: magic:ptr magicu:n :}
   IMG IMG-U @ SCAN-I @ magic magicu FIND-AFTER MATCH option
     none OF 0 0= 0= exit ENDOF
     some OF IDX>N ENDOF
   ;MATCH {: idx:n :}
   idx SCAN-LAST !
   idx 1+ SCAN-I !
   0 0= ;

: LAST-MAGIC ( ptr u8 n -- n ) {: magic:ptr magicu:n :}
   0 SCAN-I !
   -1 SCAN-LAST !
   begin magic magicu MAGIC-STEP 0= until
   SCAN-LAST @ dup 0 < if
      drop s" owner-WID image marker missing" 74 die
   then ;

: LOAD-IMAGE ( ptr u8 n -- ) {: path:ptr pathu:n :}
   path pathu FILE-SIZE {: size:n :}
   size MEM-ALLOC-BYTES drop IMG-A-FIELD !
   path pathu IMG size READ-ALL IMG-U !
   IMG-U @ size <> if s" owner-WID image short read" 74 die then ;

: U32@ ( n -- n ) {: off:n :}
   IMG off BYTE+ c@
   IMG off 1+ BYTE+ c@ 8 lshift or
   IMG off 2 + BYTE+ c@ 16 lshift or
   IMG off 3 + BYTE+ c@ 24 lshift or ;

: U32! ( n n -- ) {: val:n off:n :}
   val IMG off BYTE+ c!
   val 8 rshift IMG off 1+ BYTE+ c!
   val 16 rshift IMG off 2 + BYTE+ c!
   val 24 rshift IMG off 3 + BYTE+ c! ;

: WRITE-IMAGE ( ptr u8 n -- ) {: path:ptr pathu:n :}
   path pathu IMG IMG-U @ WRITE-ALL
   path pathu CHMOD-X
   path pathu CODESIGN-FORCE ;

: AOT-MAGIC$ ( -- ptr u8 n )
   s" RIAPDIWO" ;

: SNAP-MAGIC$ ( -- ptr u8 n )
   s" !SNAPSBH" ;

: AOT-COUNT-OFF ( -- n )
   AOT-MAGIC$ LAST-MAGIC 16 + ;

: AOT-ROW-OFF ( -- n )
   AOT-COUNT-OFF 16 + ;

: SNAP-TRAILER-OFF ( -- n )
   SNAP-MAGIC$ LAST-MAGIC ;

: SNAP-DATA-OFF ( n -- n ) {: trailer:n :}
   trailer trailer 32 + U32@ - ;

: SNAP-REGION-OFF ( n -- n ) {: trailer:n :}
   trailer SNAP-DATA-OFF trailer 24 + U32@ - ;

: OWNER-REC-OFF ( n -- n ) {: trailer:n :}
   trailer SNAP-DATA-OFF OWNER-WID-OFF + {: row:n :}
   row OWNER-WID-PUB + U32@ {: pub:n :}
   row OWNER-WID-PRI + U32@ {: pri:n :}
   trailer SNAP-REGION-OFF {: region:n :}
   trailer 16 + U32@ 0 ?do
      region i DREC * + {: rec:n :}
      rec 40 + U32@ $FFFFFFFF =
      rec U32@ pub = and
      rec 8 + U32@ pri = and if rec unloop exit then
   loop
   s" owner-WID package record missing" 74 die ;

: BUILD-AOT-BAD ( -- )
   OWNER-WID-IMAGE:AOT-HB$ LOAD-IMAGE
   OWNER-WID-MAX 1+ AOT-COUNT-OFF U32!
   AOT-BAD-BUF AOT-BAD-U @ WRITE-IMAGE ;

: BUILD-AOT-MAL ( -- )
   OWNER-WID-IMAGE:AOT-HB$ LOAD-IMAGE
   0 AOT-ROW-OFF AOT-OWNER-SOURCE-PUB + U32!
   AOT-MAL-BUF AOT-MAL-U @ WRITE-IMAGE ;

: BUILD-SNAP-OLD ( -- )
   OWNER-WID-IMAGE:SNAP-HB$ LOAD-IMAGE
   2 SNAP-TRAILER-OFF 40 + U32!
   SNAP-OLD-BUF SNAP-OLD-U @ WRITE-IMAGE ;

: BUILD-SNAP-BAD ( -- )
   OWNER-WID-IMAGE:SNAP-HB$ LOAD-IMAGE
   SNAP-TRAILER-OFF {: trailer:n :}
   OWNER-WID-MAX 1+ trailer SNAP-DATA-OFF OWNER-WID-N-CELL + U32!
   SNAP-BAD-BUF SNAP-BAD-U @ WRITE-IMAGE ;

: BUILD-SNAP-MAL ( -- )
   OWNER-WID-IMAGE:SNAP-HB$ LOAD-IMAGE
   SNAP-TRAILER-OFF SNAP-DATA-OFF {: data:n :}
   0 data OWNER-WID-OFF + OWNER-WID-PUB + U32!
   SNAP-MAL-BUF SNAP-MAL-U @ WRITE-IMAGE ;

: BUILD-SNAP-MAG ( -- )
   OWNER-WID-IMAGE:SNAP-HB$ LOAD-IMAGE
   0 SNAP-TRAILER-OFF U32!
   SNAP-MAG-BUF SNAP-MAG-U @ WRITE-IMAGE ;

: BUILD-SNAP-WID ( n ptr u8 n -- )
   {: widn:n out:ptr outu:n :}
   OWNER-WID-IMAGE:SNAP-HB$ LOAD-IMAGE
   SNAP-TRAILER-OFF SNAP-DATA-OFF {: data:n :}
   widn data WIDN-CELL + U32!
   out outu WRITE-IMAGE ;

: BUILD-SNAP-DUP ( -- )
   OWNER-WID-IMAGE:SNAP-HB$ LOAD-IMAGE
   SNAP-TRAILER-OFF {: trailer:n :}
   trailer 16 + U32@ {: ndict:n :}
   trailer OWNER-REC-OFF {: src:n :}
   trailer SNAP-REGION-OFF ndict DREC * + {: dst:n :}
   IMG src BYTE+ IMG dst BYTE+ DREC BYTE-COPY
   $7FFFFFF0 dst U32!
   $7FFFFFF1 dst 8 + U32!
   ndict 1+ trailer 16 + U32!
   SNAP-DUP-BUF SNAP-DUP-U @ WRITE-IMAGE ;

: BUILD-SNAP-PTR ( -- )
   OWNER-WID-IMAGE:SNAP-HB$ LOAD-IMAGE
   SNAP-TRAILER-OFF OWNER-REC-OFF {: rec:n :}
   0 rec 24 + U32!
   0 rec 28 + U32!
   SNAP-PTR-BUF SNAP-PTR-U @ WRITE-IMAGE ;

public

: AOT-BAD$ ( -- ptr u8 n )
   OWNER-WID-IMAGE:ROOT s" hb-aot-owner-bad" AOT-BAD-BUF JOIN-PATH AOT-BAD-U !
   AOT-BAD-BUF AOT-BAD-U @ ;

: AOT-MAL$ ( -- ptr u8 n )
   OWNER-WID-IMAGE:ROOT s" hb-aot-owner-mal" AOT-MAL-BUF JOIN-PATH AOT-MAL-U !
   AOT-MAL-BUF AOT-MAL-U @ ;

: SNAP-OLD$ ( -- ptr u8 n )
   OWNER-WID-IMAGE:ROOT s" hb-snap-owner-old" SNAP-OLD-BUF JOIN-PATH SNAP-OLD-U !
   SNAP-OLD-BUF SNAP-OLD-U @ ;

: SNAP-BAD$ ( -- ptr u8 n )
   OWNER-WID-IMAGE:ROOT s" hb-snap-owner-bad" SNAP-BAD-BUF JOIN-PATH SNAP-BAD-U !
   SNAP-BAD-BUF SNAP-BAD-U @ ;

: SNAP-MAL$ ( -- ptr u8 n )
   OWNER-WID-IMAGE:ROOT s" hb-snap-owner-mal" SNAP-MAL-BUF JOIN-PATH SNAP-MAL-U !
   SNAP-MAL-BUF SNAP-MAL-U @ ;

: SNAP-MAG$ ( -- ptr u8 n )
   OWNER-WID-IMAGE:ROOT s" hb-snap-owner-magic" SNAP-MAG-BUF JOIN-PATH SNAP-MAG-U !
   SNAP-MAG-BUF SNAP-MAG-U @ ;

: SNAP-WID1$ ( -- ptr u8 n )
   OWNER-WID-IMAGE:ROOT s" hb-snap-owner-wid1" SNAP-WID1-BUF JOIN-PATH SNAP-WID1-U !
   SNAP-WID1-BUF SNAP-WID1-U @ ;

: SNAP-WID2$ ( -- ptr u8 n )
   OWNER-WID-IMAGE:ROOT s" hb-snap-owner-wid2" SNAP-WID2-BUF JOIN-PATH SNAP-WID2-U !
   SNAP-WID2-BUF SNAP-WID2-U @ ;

: SNAP-DUP$ ( -- ptr u8 n )
   OWNER-WID-IMAGE:ROOT s" hb-snap-owner-dup" SNAP-DUP-BUF JOIN-PATH SNAP-DUP-U !
   SNAP-DUP-BUF SNAP-DUP-U @ ;

: SNAP-PTR$ ( -- ptr u8 n )
   OWNER-WID-IMAGE:ROOT s" hb-snap-owner-ptr" SNAP-PTR-BUF JOIN-PATH SNAP-PTR-U !
   SNAP-PTR-BUF SNAP-PTR-U @ ;

: BUILD ( -- )
   AOT-BAD$ 2drop
   AOT-MAL$ 2drop
   SNAP-OLD$ 2drop
   SNAP-BAD$ 2drop
   SNAP-MAL$ 2drop
   SNAP-MAG$ 2drop
   SNAP-WID1$ 2drop
   SNAP-WID2$ 2drop
   SNAP-DUP$ 2drop
   SNAP-PTR$ 2drop
   BUILD-AOT-BAD
   BUILD-AOT-MAL
   BUILD-SNAP-OLD
   BUILD-SNAP-BAD
   BUILD-SNAP-MAL
   BUILD-SNAP-MAG
   1 SNAP-WID1$ BUILD-SNAP-WID
   2 SNAP-WID2$ BUILD-SNAP-WID
   BUILD-SNAP-DUP
   BUILD-SNAP-PTR ;

;package
