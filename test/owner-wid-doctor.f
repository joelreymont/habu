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
variable AOT-BAD-U
variable AOT-MAL-U
variable SNAP-OLD-U
variable SNAP-BAD-U
variable SNAP-MAL-U
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

: BUILD ( -- )
   AOT-BAD$ 2drop
   AOT-MAL$ 2drop
   SNAP-OLD$ 2drop
   SNAP-BAD$ 2drop
   SNAP-MAL$ 2drop
   BUILD-AOT-BAD
   BUILD-AOT-MAL
   BUILD-SNAP-OLD
   BUILD-SNAP-BAD
   BUILD-SNAP-MAL ;

;package
