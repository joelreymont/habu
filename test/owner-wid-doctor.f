\ owner-wid-doctor.f - malformed persisted owner-image fixtures.

require lib/errors.f
require lib/string.f
require lib/adt/option.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/codesign.f
require test/owner-wid-image.f

\ White-box CAD-NUM role reader (precedent: lib/string-test.f STR-T-IX>RAW):
\ reopen the unsealed CAD-NUM package to project the typed STR:FIND-SUB index
\ back to its raw cell, keeping FIND-AFTER byte-identical. A plain checked word
\ over the audited private INDEX>N projection - not a new boundary.
package CAD-NUM
public
: OWD-IX>RAW ( CAD-NUM:index -- n ) INDEX>N ;
;package

package OWNER-WID-DOCTOR

create AOT-BAD-BUF FS-PATH-CAP allot
create AOT-MAL-BUF FS-PATH-CAP allot
create AOT-LIMIT-BUF FS-PATH-CAP allot
create SNAP-OLD-BUF FS-PATH-CAP allot
create SNAP-BAD-BUF FS-PATH-CAP allot
create SNAP-MAL-BUF FS-PATH-CAP allot
create SNAP-MAG-BUF FS-PATH-CAP allot
create SNAP-WID1-BUF FS-PATH-CAP allot
create SNAP-WID2-BUF FS-PATH-CAP allot
create SNAP-WID-HI-BUF FS-PATH-CAP allot
create SNAP-PAIR-CAP-BUF FS-PATH-CAP allot
create SNAP-WL-CAP-BUF FS-PATH-CAP allot
create SNAP-DUP-BUF FS-PATH-CAP allot
create SNAP-ZERO-BUF FS-PATH-CAP allot
create SNAP-RSVD-BUF FS-PATH-CAP allot
create SNAP-ALIAS-BUF FS-PATH-CAP allot
create SNAP-REUSE-BUF FS-PATH-CAP allot
create SNAP-PROT-BUF FS-PATH-CAP allot
create SNAP-XPTR-BUF FS-PATH-CAP allot
create SNAP-PTR-BUF FS-PATH-CAP allot
create SNAP-DICT-EXT-BUF FS-PATH-CAP allot
create SNAP-LEAD-BUF FS-PATH-CAP allot
create SNAP-TRAIL-BUF FS-PATH-CAP allot
create SNAP-DBL-BUF FS-PATH-CAP allot
create SNAP-TYPE-OK-BUF FS-PATH-CAP allot
create SNAP-TYPE-PRI-BUF FS-PATH-CAP allot
create SNAP-TYPE-ALIAS-BUF FS-PATH-CAP allot
create SNAP-TYPE-MISS-BUF FS-PATH-CAP allot
create SNAP-CROSS-DUP-BUF FS-PATH-CAP allot
create SNAP-TYPE-WIDN-BUF FS-PATH-CAP allot
create SNAP-LIVE-BUF FS-PATH-CAP allot
create CAP-SRC-BUF FS-PATH-CAP allot
variable AOT-BAD-U
variable AOT-MAL-U
variable AOT-LIMIT-U
variable SNAP-OLD-U
variable SNAP-BAD-U
variable SNAP-MAL-U
variable SNAP-MAG-U
variable SNAP-WID1-U
variable SNAP-WID2-U
variable SNAP-WID-HI-U
variable SNAP-PAIR-CAP-U
variable SNAP-WL-CAP-U
variable SNAP-DUP-U
variable SNAP-ZERO-U
variable SNAP-RSVD-U
variable SNAP-ALIAS-U
variable SNAP-REUSE-U
variable SNAP-PROT-U
variable SNAP-XPTR-U
variable SNAP-PTR-U
variable SNAP-DICT-EXT-U
variable SNAP-LEAD-U
variable SNAP-TRAIL-U
variable SNAP-DBL-U
variable SNAP-TYPE-OK-U
variable SNAP-TYPE-PRI-U
variable SNAP-TYPE-ALIAS-U
variable SNAP-TYPE-MISS-U
variable SNAP-CROSS-DUP-U
variable SNAP-TYPE-WIDN-U
variable SNAP-LIVE-U
variable CAP-SRC-U
PTR-VARIABLE IMG-A
variable IMG-U
variable SCAN-I
variable SCAN-LAST

: IMG ( -- ptr u8 )
   IMG-A @ ;

: FIND-AFTER ( ptr u8 n n ptr u8 n -- option<idx> )
   {: a:ptr u:n start:n needle:ptr nu:n :}
   start 0 < if OPTION:NONE exit then
   start u >= if OPTION:NONE exit then
   a start BYTE+ u start - STR:LENGTH needle nu STR:LENGTH STR:FIND-SUB MATCH option
     none OF OPTION:NONE ENDOF
     some OF CAD-NUM:OWD-IX>RAW start + >IDX OPTION:SOME ENDOF
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
   size MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop IMG-A !
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

: U64@ ( n -- n ) {: off:n :}
   off U32@ off 4 + U32@ 32 lshift or ;

: U64! ( n n -- ) {: val:n off:n :}
   val off U32!
   val 32 rshift off 4 + U32! ;

: WRITE-IMAGE ( ptr u8 n -- ) {: path:ptr pathu:n :}
   path pathu IMG IMG-U @ WRITE-ALL
   path pathu CHMOD-X
   path pathu CODESIGN:FORCE ;

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

: APPEND-PACKAGE ( ptr u8 n n n n -- n )
   {: name:ptr nameu:n pub:n pri:n trailer:n :}
   trailer 16 + U32@ {: ndict:n :}
   trailer OWNER-REC-OFF {: src:n :}
   trailer SNAP-REGION-OFF ndict DREC * + {: dst:n :}
   IMG src BYTE+ IMG dst BYTE+ DREC BYTE-COPY
   pub dst U32!
   pri dst 8 + U32!
   nameu dst 16 + U64!
   name IMG dst 24 + BYTE+ nameu BYTE-COPY
   ndict 1+ trailer 16 + U32!
   dst ;

: APPEND-TYPE ( ptr u8 n n n n -- n )
   APPEND-PACKAGE
   dup 16 + dup U64@
   NAMESPACE:KIND-TYPE 52 lshift or swap U64! ;

: NEXT-WID@ ( n -- n )
   SNAP-DATA-OFF WIDN-CELL + U32@ ;

: NEXT-WID! ( n n -- ) {: wid:n trailer:n :}
   wid trailer SNAP-DATA-OFF WIDN-CELL + U32! ;

: APPEND-PROT ( n n -- ) {: wid:n trailer:n :}
   trailer SNAP-DATA-OFF {: data:n :}
   data PROT-WID-N-CELL + U32@ {: count:n :}
   count PROT-WID-MAX >= if s" owner-WID protected fixture full" 74 die then
   wid data PROT-WID-OFF + count 4 * + U32!
   count 1+ data PROT-WID-N-CELL + U32! ;

: BUILD-AOT-BAD ( -- )
   OWNER-WID-IMAGE:AOT-HB$ LOAD-IMAGE
   OWNER-WID-MAX 1+ AOT-COUNT-OFF U32!
   AOT-BAD-BUF AOT-BAD-U @ WRITE-IMAGE ;

: BUILD-AOT-MAL ( -- )
   OWNER-WID-IMAGE:AOT-HB$ LOAD-IMAGE
   0 AOT-ROW-OFF AOT-OWNER-SOURCE-PUB + U32!
   AOT-MAL-BUF AOT-MAL-U @ WRITE-IMAGE ;

: BUILD-AOT-LIMIT ( -- )
   OWNER-WID-IMAGE:AOT-HB$ LOAD-IMAGE
   OWNER-WID-LIMIT AOT-ROW-OFF AOT-OWNER-SOURCE-PUB + U32!
   AOT-LIMIT-BUF AOT-LIMIT-U @ WRITE-IMAGE ;

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
   trailer NEXT-WID@ {: wid:n :}
   trailer 16 + U32@ {: ndict:n :}
   trailer OWNER-REC-OFF {: src:n :}
   trailer SNAP-REGION-OFF ndict DREC * + {: dst:n :}
   IMG src BYTE+ IMG dst BYTE+ DREC BYTE-COPY
   wid dst U32!
   wid 1+ dst 8 + U32!
   ndict 1+ trailer 16 + U32!
   wid 2 + trailer NEXT-WID!
   SNAP-DUP-BUF SNAP-DUP-U @ WRITE-IMAGE ;

: BUILD-SNAP-ZERO ( -- )
   OWNER-WID-IMAGE:SNAP-HB$ LOAD-IMAGE
   SNAP-TRAILER-OFF {: trailer:n :}
   trailer NEXT-WID@ {: wid:n :}
   s" BAD-ZERO" wid 0 trailer APPEND-PACKAGE drop
   wid 1+ trailer NEXT-WID!
   SNAP-ZERO-BUF SNAP-ZERO-U @ WRITE-IMAGE ;

: BUILD-SNAP-RSVD ( -- )
   OWNER-WID-IMAGE:SNAP-HB$ LOAD-IMAGE
   SNAP-TRAILER-OFF {: trailer:n :}
   trailer NEXT-WID@ {: wid:n :}
   s" BAD-RSVD" FIRST-DYNAMIC-WID 1- wid trailer APPEND-PACKAGE drop
   wid 1+ trailer NEXT-WID!
   SNAP-RSVD-BUF SNAP-RSVD-U @ WRITE-IMAGE ;

: BUILD-SNAP-ALIAS ( -- )
   OWNER-WID-IMAGE:SNAP-HB$ LOAD-IMAGE
   SNAP-TRAILER-OFF {: trailer:n :}
   trailer SNAP-DATA-OFF OWNER-WID-OFF + OWNER-WID-PUB + U32@ {: pub:n :}
   trailer NEXT-WID@ {: wid:n :}
   s" BAD-ALIAS" pub wid trailer APPEND-PACKAGE drop
   wid 1+ trailer NEXT-WID!
   SNAP-ALIAS-BUF SNAP-ALIAS-U @ WRITE-IMAGE ;

: BUILD-SNAP-REUSE ( -- )
   OWNER-WID-IMAGE:SNAP-HB$ LOAD-IMAGE
   SNAP-TRAILER-OFF {: trailer:n :}
   trailer NEXT-WID@ {: wid:n :}
   s" BAD-REUSE-A" wid wid 1+ trailer APPEND-PACKAGE drop
   s" BAD-REUSE-B" wid wid 2 + trailer APPEND-PACKAGE drop
   wid 3 + trailer NEXT-WID!
   SNAP-REUSE-BUF SNAP-REUSE-U @ WRITE-IMAGE ;

: BUILD-SNAP-PROT ( -- )
   OWNER-WID-IMAGE:SNAP-HB$ LOAD-IMAGE
   SNAP-TRAILER-OFF {: trailer:n :}
   trailer NEXT-WID@ {: wid:n :}
   wid trailer APPEND-PROT
   s" BAD-PROT" wid wid 1+ trailer APPEND-PACKAGE drop
   wid 2 + trailer NEXT-WID!
   SNAP-PROT-BUF SNAP-PROT-U @ WRITE-IMAGE ;

: BUILD-SNAP-XPTR ( -- )
   OWNER-WID-IMAGE:SNAP-HB$ LOAD-IMAGE
   SNAP-TRAILER-OFF {: trailer:n :}
   trailer NEXT-WID@ {: wid:n :}
   s" BAD-XPTR" wid wid 1+ trailer APPEND-PACKAGE {: rec:n :}
   DNAME-INL 1+ DNAME-EXT or rec 16 + U64!
   0 rec 24 + U64!
   wid 2 + trailer NEXT-WID!
   SNAP-XPTR-BUF SNAP-XPTR-U @ WRITE-IMAGE ;

: BUILD-SNAP-PTR ( -- )
   OWNER-WID-IMAGE:SNAP-HB$ LOAD-IMAGE
   SNAP-TRAILER-OFF OWNER-REC-OFF {: rec:n :}
   0 rec 24 + U32!
   0 rec 28 + U32!
   SNAP-PTR-BUF SNAP-PTR-U @ WRITE-IMAGE ;

: BUILD-SNAP-DICT-EXT ( -- )
   OWNER-WID-IMAGE:SNAP-HB$ LOAD-IMAGE
   SNAP-TRAILER-OFF {: trailer:n :}
   trailer NEXT-WID@ {: wid:n :}
   s" BAD-DICT-EXT" wid wid 1+ trailer APPEND-PACKAGE {: rec:n :}
   trailer 16 + U32@ {: ndict:n :}
   trailer SNAP-REGION-OFF ndict DREC * + {: name:n :}
   s" BAD-DICTIONARY-EXT" {: text:ptr textu:n :}
   text IMG name BYTE+ textu BYTE-COPY
   textu DNAME-EXT or rec 16 + U64!
   RBASE-VA name trailer SNAP-REGION-OFF - + rec 24 + U64!
   wid 2 + trailer NEXT-WID!
   SNAP-DICT-EXT-BUF SNAP-DICT-EXT-U @ WRITE-IMAGE ;

: BUILD-SNAP-PATH ( ptr u8 n ptr u8 n -- )
   {: name:ptr nameu:n out:ptr outu:n :}
   OWNER-WID-IMAGE:SNAP-HB$ LOAD-IMAGE
   SNAP-TRAILER-OFF {: trailer:n :}
   trailer NEXT-WID@ {: wid:n :}
   name nameu wid wid 1+ trailer APPEND-PACKAGE drop
   wid 2 + trailer NEXT-WID!
   out outu WRITE-IMAGE ;

: BUILD-SNAP-LEAD ( -- )
   s" :BAD-LEAD" SNAP-LEAD-BUF SNAP-LEAD-U @ BUILD-SNAP-PATH ;

: BUILD-SNAP-TRAIL ( -- )
   s" BAD-TRAIL:" SNAP-TRAIL-BUF SNAP-TRAIL-U @ BUILD-SNAP-PATH ;

: BUILD-SNAP-DBL ( -- )
   s" BAD::DOUBLE" SNAP-DBL-BUF SNAP-DBL-U @ BUILD-SNAP-PATH ;

: BUILD-SNAP-TYPE-OK ( -- )
   OWNER-WID-IMAGE:SNAP-HB$ LOAD-IMAGE
   SNAP-TRAILER-OFF {: trailer:n :}
   trailer NEXT-WID@ {: wid:n :}
   wid trailer APPEND-PROT
   s" TYPE-OK" wid 0 trailer APPEND-TYPE drop
   wid 1+ trailer NEXT-WID!
   SNAP-TYPE-OK-BUF SNAP-TYPE-OK-U @ WRITE-IMAGE ;

: BUILD-SNAP-TYPE-PRI ( -- )
   OWNER-WID-IMAGE:SNAP-HB$ LOAD-IMAGE
   SNAP-TRAILER-OFF {: trailer:n :}
   trailer NEXT-WID@ {: wid:n :}
   wid trailer APPEND-PROT
   s" TYPE-PRI" wid wid 1+ trailer APPEND-TYPE drop
   wid 2 + trailer NEXT-WID!
   SNAP-TYPE-PRI-BUF SNAP-TYPE-PRI-U @ WRITE-IMAGE ;

: BUILD-SNAP-TYPE-ALIAS ( -- )
   OWNER-WID-IMAGE:SNAP-HB$ LOAD-IMAGE
   SNAP-TRAILER-OFF {: trailer:n :}
   trailer NEXT-WID@ {: wid:n :}
   wid trailer APPEND-PROT
   s" TYPE-ALIAS-A" wid 0 trailer APPEND-TYPE drop
   s" TYPE-ALIAS-B" wid 0 trailer APPEND-TYPE drop
   wid 1+ trailer NEXT-WID!
   SNAP-TYPE-ALIAS-BUF SNAP-TYPE-ALIAS-U @ WRITE-IMAGE ;

: BUILD-SNAP-TYPE-MISS ( -- )
   OWNER-WID-IMAGE:SNAP-HB$ LOAD-IMAGE
   SNAP-TRAILER-OFF {: trailer:n :}
   trailer NEXT-WID@ {: wid:n :}
   s" TYPE-MISS" wid 0 trailer APPEND-TYPE drop
   wid 1+ trailer NEXT-WID!
   SNAP-TYPE-MISS-BUF SNAP-TYPE-MISS-U @ WRITE-IMAGE ;

: BUILD-SNAP-CROSS-DUP ( -- )
   OWNER-WID-IMAGE:SNAP-HB$ LOAD-IMAGE
   SNAP-TRAILER-OFF {: trailer:n :}
   trailer NEXT-WID@ {: wid:n :}
   wid 2 + trailer APPEND-PROT
   s" CROSS-DUP" wid wid 1+ trailer APPEND-PACKAGE drop
   s" cross-dup" wid 2 + 0 trailer APPEND-TYPE drop
   wid 3 + trailer NEXT-WID!
   SNAP-CROSS-DUP-BUF SNAP-CROSS-DUP-U @ WRITE-IMAGE ;

: BUILD-SNAP-TYPE-WIDN ( -- )
   OWNER-WID-IMAGE:SNAP-HB$ LOAD-IMAGE
   SNAP-TRAILER-OFF {: trailer:n :}
   trailer NEXT-WID@ {: wid:n :}
   wid trailer APPEND-PROT
   s" TYPE-WIDN" wid 0 trailer APPEND-TYPE drop
   wid trailer NEXT-WID!
   SNAP-TYPE-WIDN-BUF SNAP-TYPE-WIDN-U @ WRITE-IMAGE ;

\ Persisted process-mode state must never select the next invocation's exit
\ path. Forge all three live cells nonzero so the batch mode matrix proves
\ startup owns their values after snapshot restore.
: BUILD-SNAP-LIVE ( -- )
   OWNER-WID-IMAGE:SNAP-HB$ LOAD-IMAGE
   SNAP-TRAILER-OFF SNAP-DATA-OFF {: data:n :}
   1 data REPLH-CELL + U64!
   1 data AOT-SEED-DONE-CELL + U64!
   1 data AOT-SEED-ARM-CELL + U64!
   SNAP-LIVE-BUF SNAP-LIVE-U @ WRITE-IMAGE ;

public

: AOT-BAD$ ( -- ptr u8 n )
   OWNER-WID-IMAGE:ROOT s" hb-aot-owner-bad" AOT-BAD-BUF JOIN-PATH AOT-BAD-U !
   AOT-BAD-BUF AOT-BAD-U @ ;

: AOT-MAL$ ( -- ptr u8 n )
   OWNER-WID-IMAGE:ROOT s" hb-aot-owner-mal" AOT-MAL-BUF JOIN-PATH AOT-MAL-U !
   AOT-MAL-BUF AOT-MAL-U @ ;

: AOT-LIMIT$ ( -- ptr u8 n )
   OWNER-WID-IMAGE:ROOT s" hb-aot-owner-limit" AOT-LIMIT-BUF JOIN-PATH AOT-LIMIT-U !
   AOT-LIMIT-BUF AOT-LIMIT-U @ ;

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

: SNAP-WID-HI$ ( -- ptr u8 n )
   OWNER-WID-IMAGE:ROOT s" hb-snap-owner-wid-hi" SNAP-WID-HI-BUF JOIN-PATH SNAP-WID-HI-U !
   SNAP-WID-HI-BUF SNAP-WID-HI-U @ ;

: SNAP-PAIR-CAP$ ( -- ptr u8 n )
   OWNER-WID-IMAGE:ROOT s" hb-snap-owner-pair-cap" SNAP-PAIR-CAP-BUF JOIN-PATH SNAP-PAIR-CAP-U !
   SNAP-PAIR-CAP-BUF SNAP-PAIR-CAP-U @ ;

: SNAP-WL-CAP$ ( -- ptr u8 n )
   OWNER-WID-IMAGE:ROOT s" hb-snap-owner-wl-cap" SNAP-WL-CAP-BUF JOIN-PATH SNAP-WL-CAP-U !
   SNAP-WL-CAP-BUF SNAP-WL-CAP-U @ ;

: SNAP-DUP$ ( -- ptr u8 n )
   OWNER-WID-IMAGE:ROOT s" hb-snap-owner-dup" SNAP-DUP-BUF JOIN-PATH SNAP-DUP-U !
   SNAP-DUP-BUF SNAP-DUP-U @ ;

: SNAP-ZERO$ ( -- ptr u8 n )
   OWNER-WID-IMAGE:ROOT s" hb-snap-owner-zero" SNAP-ZERO-BUF JOIN-PATH SNAP-ZERO-U !
   SNAP-ZERO-BUF SNAP-ZERO-U @ ;

: SNAP-RSVD$ ( -- ptr u8 n )
   OWNER-WID-IMAGE:ROOT s" hb-snap-owner-rsvd" SNAP-RSVD-BUF JOIN-PATH SNAP-RSVD-U !
   SNAP-RSVD-BUF SNAP-RSVD-U @ ;

: SNAP-ALIAS$ ( -- ptr u8 n )
   OWNER-WID-IMAGE:ROOT s" hb-snap-owner-alias" SNAP-ALIAS-BUF JOIN-PATH SNAP-ALIAS-U !
   SNAP-ALIAS-BUF SNAP-ALIAS-U @ ;

: SNAP-REUSE$ ( -- ptr u8 n )
   OWNER-WID-IMAGE:ROOT s" hb-snap-owner-reuse" SNAP-REUSE-BUF JOIN-PATH SNAP-REUSE-U !
   SNAP-REUSE-BUF SNAP-REUSE-U @ ;

: SNAP-PROT$ ( -- ptr u8 n )
   OWNER-WID-IMAGE:ROOT s" hb-snap-owner-prot" SNAP-PROT-BUF JOIN-PATH SNAP-PROT-U !
   SNAP-PROT-BUF SNAP-PROT-U @ ;

: SNAP-XPTR$ ( -- ptr u8 n )
   OWNER-WID-IMAGE:ROOT s" hb-snap-owner-xptr" SNAP-XPTR-BUF JOIN-PATH SNAP-XPTR-U !
   SNAP-XPTR-BUF SNAP-XPTR-U @ ;

: SNAP-PTR$ ( -- ptr u8 n )
   OWNER-WID-IMAGE:ROOT s" hb-snap-owner-ptr" SNAP-PTR-BUF JOIN-PATH SNAP-PTR-U !
   SNAP-PTR-BUF SNAP-PTR-U @ ;

: SNAP-DICT-EXT$ ( -- ptr u8 n )
   OWNER-WID-IMAGE:ROOT s" hb-snap-owner-dict-ext" SNAP-DICT-EXT-BUF JOIN-PATH SNAP-DICT-EXT-U !
   SNAP-DICT-EXT-BUF SNAP-DICT-EXT-U @ ;

: SNAP-LEAD$ ( -- ptr u8 n )
   OWNER-WID-IMAGE:ROOT s" hb-snap-owner-lead" SNAP-LEAD-BUF JOIN-PATH SNAP-LEAD-U !
   SNAP-LEAD-BUF SNAP-LEAD-U @ ;

: SNAP-TRAIL$ ( -- ptr u8 n )
   OWNER-WID-IMAGE:ROOT s" hb-snap-owner-trail" SNAP-TRAIL-BUF JOIN-PATH SNAP-TRAIL-U !
   SNAP-TRAIL-BUF SNAP-TRAIL-U @ ;

: SNAP-DBL$ ( -- ptr u8 n )
   OWNER-WID-IMAGE:ROOT s" hb-snap-owner-double" SNAP-DBL-BUF JOIN-PATH SNAP-DBL-U !
   SNAP-DBL-BUF SNAP-DBL-U @ ;

: SNAP-TYPE-OK$ ( -- ptr u8 n )
   OWNER-WID-IMAGE:ROOT s" hb-snap-owner-type-ok" SNAP-TYPE-OK-BUF JOIN-PATH SNAP-TYPE-OK-U !
   SNAP-TYPE-OK-BUF SNAP-TYPE-OK-U @ ;

: SNAP-TYPE-PRI$ ( -- ptr u8 n )
   OWNER-WID-IMAGE:ROOT s" hb-snap-owner-type-pri" SNAP-TYPE-PRI-BUF JOIN-PATH SNAP-TYPE-PRI-U !
   SNAP-TYPE-PRI-BUF SNAP-TYPE-PRI-U @ ;

: SNAP-TYPE-ALIAS$ ( -- ptr u8 n )
   OWNER-WID-IMAGE:ROOT s" hb-snap-owner-type-alias" SNAP-TYPE-ALIAS-BUF JOIN-PATH SNAP-TYPE-ALIAS-U !
   SNAP-TYPE-ALIAS-BUF SNAP-TYPE-ALIAS-U @ ;

: SNAP-TYPE-MISS$ ( -- ptr u8 n )
   OWNER-WID-IMAGE:ROOT s" hb-snap-owner-type-miss" SNAP-TYPE-MISS-BUF JOIN-PATH SNAP-TYPE-MISS-U !
   SNAP-TYPE-MISS-BUF SNAP-TYPE-MISS-U @ ;

: SNAP-CROSS-DUP$ ( -- ptr u8 n )
   OWNER-WID-IMAGE:ROOT s" hb-snap-owner-cross-dup" SNAP-CROSS-DUP-BUF JOIN-PATH SNAP-CROSS-DUP-U !
   SNAP-CROSS-DUP-BUF SNAP-CROSS-DUP-U @ ;

: SNAP-TYPE-WIDN$ ( -- ptr u8 n )
   OWNER-WID-IMAGE:ROOT s" hb-snap-owner-type-widn" SNAP-TYPE-WIDN-BUF JOIN-PATH SNAP-TYPE-WIDN-U !
   SNAP-TYPE-WIDN-BUF SNAP-TYPE-WIDN-U @ ;

: QUAL-CAP-SRC$ ( -- ptr u8 n )
   OWNER-WID-IMAGE:ROOT s" hb-owner-qual-cap.f" CAP-SRC-BUF JOIN-PATH CAP-SRC-U !
   CAP-SRC-BUF CAP-SRC-U @ ;

: PKG-CAP-SRC$ ( -- ptr u8 n )
   OWNER-WID-IMAGE:ROOT s" hb-owner-pkg-cap.f" CAP-SRC-BUF JOIN-PATH CAP-SRC-U !
   CAP-SRC-BUF CAP-SRC-U @ ;

: WL-CAP-SRC$ ( -- ptr u8 n )
   OWNER-WID-IMAGE:ROOT s" hb-owner-wl-cap.f" CAP-SRC-BUF JOIN-PATH CAP-SRC-U !
   CAP-SRC-BUF CAP-SRC-U @ ;

: GEN-CAP-SRC$ ( -- ptr u8 n )
   OWNER-WID-IMAGE:ROOT s" hb-owner-gen-cap.f" CAP-SRC-BUF JOIN-PATH CAP-SRC-U !
   CAP-SRC-BUF CAP-SRC-U @ ;

: SNAP-LIVE$ ( -- ptr u8 n )
   OWNER-WID-IMAGE:ROOT s" hb-snap-owner-live" SNAP-LIVE-BUF JOIN-PATH SNAP-LIVE-U !
   SNAP-LIVE-BUF SNAP-LIVE-U @ ;

: BUILD ( -- )
   AOT-BAD$ 2drop
   AOT-MAL$ 2drop
   AOT-LIMIT$ 2drop
   SNAP-OLD$ 2drop
   SNAP-BAD$ 2drop
   SNAP-MAL$ 2drop
   SNAP-MAG$ 2drop
   SNAP-WID1$ 2drop
   SNAP-WID2$ 2drop
   SNAP-WID-HI$ 2drop
   SNAP-PAIR-CAP$ 2drop
   SNAP-WL-CAP$ 2drop
   SNAP-DUP$ 2drop
   SNAP-ZERO$ 2drop
   SNAP-RSVD$ 2drop
   SNAP-ALIAS$ 2drop
   SNAP-REUSE$ 2drop
   SNAP-PROT$ 2drop
   SNAP-XPTR$ 2drop
   SNAP-PTR$ 2drop
   SNAP-DICT-EXT$ 2drop
   SNAP-LEAD$ 2drop
   SNAP-TRAIL$ 2drop
   SNAP-DBL$ 2drop
   SNAP-TYPE-OK$ 2drop
   SNAP-TYPE-PRI$ 2drop
   SNAP-TYPE-ALIAS$ 2drop
   SNAP-TYPE-MISS$ 2drop
   SNAP-CROSS-DUP$ 2drop
   SNAP-TYPE-WIDN$ 2drop
   SNAP-LIVE$ 2drop
   QUAL-CAP-SRC$ 2drop
   PKG-CAP-SRC$ 2drop
   WL-CAP-SRC$ 2drop
   GEN-CAP-SRC$ 2drop
   BUILD-AOT-BAD
   BUILD-AOT-MAL
   BUILD-AOT-LIMIT
   BUILD-SNAP-OLD
   BUILD-SNAP-BAD
   BUILD-SNAP-MAL
   BUILD-SNAP-MAG
   1 SNAP-WID1$ BUILD-SNAP-WID
   2 SNAP-WID2$ BUILD-SNAP-WID
   OWNER-WID-LIMIT 1+ SNAP-WID-HI$ BUILD-SNAP-WID
   OWNER-WID-LIMIT 1- SNAP-PAIR-CAP$ BUILD-SNAP-WID
   OWNER-WID-LIMIT SNAP-WL-CAP$ BUILD-SNAP-WID
   BUILD-SNAP-DUP
   BUILD-SNAP-ZERO
   BUILD-SNAP-RSVD
   BUILD-SNAP-ALIAS
   BUILD-SNAP-REUSE
   BUILD-SNAP-PROT
   BUILD-SNAP-XPTR
   BUILD-SNAP-PTR
   BUILD-SNAP-DICT-EXT
   BUILD-SNAP-LEAD
   BUILD-SNAP-TRAIL
   BUILD-SNAP-DBL
   BUILD-SNAP-TYPE-OK
   BUILD-SNAP-TYPE-PRI
   BUILD-SNAP-TYPE-ALIAS
   BUILD-SNAP-TYPE-MISS
   BUILD-SNAP-CROSS-DUP
   BUILD-SNAP-TYPE-WIDN
   BUILD-SNAP-LIVE
   QUAL-CAP-SRC$ s" : LIMIT-QUAL:WORD ( -- ) ;" WRITE-ALL
   PKG-CAP-SRC$ s" package LIMIT-PACKAGE" WRITE-ALL
   WL-CAP-SRC$ s" wordlist drop" WRITE-ALL
   GEN-CAP-SRC$ s" PRODUCT LIMIT-GEN 0 FIELD x n ;PRODUCT" WRITE-ALL ;

;package
