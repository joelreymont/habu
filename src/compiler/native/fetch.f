\ Copy source-bound frozen fetch descriptors before another checker scan runs.
require lib/prelude.f
require src/compiler/digest.f
require src/compiler/native/fetch-owner.f
require src/compiler/native/string.f
require src/compiler/native/fetch-check.f

package NFETCH
public

STRUCTURE fact 0
   FIELD offset n
   FIELD address n
   FIELD bytes n
   FIELD width n
;STRUCTURE

private

DYNAMIC-BUFFER FACTS fact
variable COUNT

: BAD ( -- )
   s" ncomp: malformed or stale fetch certificate" 76 die ;

: CELL@ ( ptr u8 n n -- n ) {: data:ptr count:n index:n :}
   index 0 < index count >= or if BAD then
   data index CDIGEST:SLOT@ ;

: RANGE-CK ( n n n -- ) {: start:n length:n total:n :}
   start 0 < start total > or length 0 < or if BAD then
   length total start - > if BAD then ;

: HASH ( ptr u8 n -- n ) {: source:ptr size:n :}
   LOWER-CERT:FNV-OFFSET
   size 0 ?do source i + c@ xor LOWER-CERT:FNV-PRIME * loop ;

: HEADER-CK ( ptr u8 n n -- ) {: data:ptr bytes:n source:n :}
   bytes LOWER-CERT:HEADER-CELLS cells < bytes CELL mod 0<> or if BAD then
   bytes CELL / {: count:n :}
   data count LOWER-CERT:MAGIC-CELL CELL@ LOWER-CERT:MAGIC <> if BAD then
   data count LOWER-CERT:VERSION-CELL CELL@ LOWER-CERT:VERSION <> if BAD then
   data count LOWER-CERT:TOTAL-BYTES-CELL CELL@ bytes <> if BAD then
   data count LOWER-CERT:BODY-LEN-CELL CELL@ source <> if BAD then ;

: TABLE-END ( n n n n -- n ) {: start:n rows:n stride:n total:n :}
   rows 0 < start 0 < or start total > or if BAD then
   rows total start - stride / > if BAD then
   start rows stride * + ;

: FETCH-START ( ptr u8 n -- n ) {: data:ptr count:n :}
   LOWER-CERT:HEADER-CELLS
   data count LOWER-CERT:WF-COUNT-CELL CELL@ LOWER-CERT:WF-CELLS count TABLE-END
   data count LOWER-CERT:BIND-COUNT-CELL CELL@ 1 count TABLE-END ;

: STREAM-START ( ptr u8 n -- n ) {: data:ptr count:n :}
   data count FETCH-START
   data count LOWER-CERT:FETCH-COUNT-CELL CELL@ LOWER-CERT:FETCH-CELLS count TABLE-END ;

: TABLES-CK ( ptr u8 n -- ) {: data:ptr count:n :}
   data count STREAM-START
   data count LOWER-CERT:FETCH-DATA-CELLS-CELL CELL@ 1 count TABLE-END
   count <> if BAD then ;

: WIDTH-FOR ( ptr u8 n n -- n ) {: data:ptr count:n offset:n :}
   data count LOWER-CERT:WF-COUNT-CELL CELL@ 0 ?do
      LOWER-CERT:HEADER-CELLS i LOWER-CERT:WF-CELLS * + {: row:n :}
      data count row CELL@ offset = if
         data count row 3 + CELL@ LOWER-CERT:FETCH-FLAG and 0<> if
            data count row 2 + CELL@ unloop exit
         then
      then
   loop
   BAD ;

: KEEP ( n ptr u8 n n -- ) {: offset:n data:ptr bytes:n width:n :}
   data bytes width NFETCH-CHECK:SHAPE
   data 0 CDIGEST:SLOT@ 0= if exit then
   COUNT @ 1+ FACTS-RESERVE
   offset data bytes NSTR:INTERN bytes width NFETCH-FACT:MAKE COUNT @ FACTS !
   1 COUNT +! ;

: COPY-ROW ( ptr u8 n n n n -- )
   {: data:ptr count:n index:n source:n base:n :}
   data count index CELL@ {: offset:n :}
   offset 0 < offset source >= or if BAD then
   data count index 1+ CELL@ {: start:n :}
   data count index 2 + CELL@ {: length:n :}
   start data count STREAM-START < if BAD then
   start length count RANGE-CK
   offset base + data start cells + length cells data count offset WIDTH-FOR KEEP ;

public

: RELEASE ( -- )
   0 COUNT !
   CHECKER-OWNER:RELEASE-CERTIFICATE
   FACTS-RELEASE ;

\ Each split body contributes its own complete snapshot under its source base.
\ Only NSTR-owned immutable bytes survive this call; no live certificate or
\ checker mapping is retained by the compiler or the emitted routine.
: CAPTURE ( ptr u8 n n -- ) {: source:ptr size:n base:n :}
   source size HASH CHECKER-OWNER:CERTIFICATE {: data:ptr bytes:n :}
   data bytes size HEADER-CK
   bytes CELL / {: count:n :}
   data count TABLES-CK
   data count FETCH-START {: start:n :}
   data count LOWER-CERT:FETCH-COUNT-CELL CELL@ 0 ?do
      data count start i LOWER-CERT:FETCH-CELLS * + size base COPY-ROW
   loop ;

: AT ( n -- n n n ) {: offset:n :}
   COUNT @ 0 ?do
      i FACTS @ NFETCH-FACT:UNMAKE {: at:n address:n bytes:n width:n :}
      at offset = if address bytes width unloop exit then
   loop
   0 0 0 ;

: CHECKED? ( n -- bool )
   AT 0 > nip nip ;

;package
