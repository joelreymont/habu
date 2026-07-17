\ diff-side-content-read.f - strict side-content artifact reader.

require tools/diff-side-content.f
require lib/memory.f

package DIFF-CONTENT
private

PTR-VARIABLE VIEW-A
variable VIEW-U
variable VIEW-N
variable VIEW-CAP
variable PAYLOAD-U
variable TRAILER-OFF
variable CURSOR
variable GROW-CAP
variable SELECT-I
variable SELECT-NEXT
variable SELECT-OLD
variable SELECT-NEW
variable SELECT-STEPS
variable VIEW-VALID

create CHECK-DIGEST $20 allot

: VIEW-A@ ( -- ptr u8 )
   VIEW-A @ ;

: REQUIRE-VALID ( -- )
   VIEW-VALID @ 0= if E-SYNTAX throw then ;

: RANGE ( n n -- ptr u8 ) {: off:n need:n :}
   off 0 < need 0 < or if E-CAPACITY throw then
   off need CHECK-ADD VIEW-U @ > if E-CAPACITY throw then
   VIEW-A@ off + ;

: BYTE@ ( n -- n )
   1 RANGE c@ ;

: U64@ ( n -- n )
   8 RANGE FS-U64@ dup 0 < if E-CAPACITY throw then ;

: BYTES$ ( n n -- ptr u8 n ) {: off:n u:n :}
   off u RANGE u ;

: ZERO-RANGE? ( n n -- bool ) {: off:n u:n :}
   0 begin dup u < while
      off over + BYTE@ 0<> if drop false exit then
      1+
   repeat drop
   true ;

: MAGIC? ( n ptr u8 n -- bool ) {: off:n a:ptr u:n :}
   off u BYTES$ a u STR= ;

: BOOL-BYTE? ( n -- bool )
   dup 0= swap 1 = or ;

: SIDE-TOTAL ( n -- n ) {: off:n :}
   off 16 + U64@ SIDE-HEAD-U swap CHECK-ADD ;

: SIDE-PATH$ ( n -- ptr u8 n ) {: off:n :}
   off 16 + U64@ {: pathu:n :}
   off SIDE-HEAD-U + pathu BYTES$ ;

: SIDE-DIGEST$ ( n -- ptr u8 n )
   32 + $20 BYTES$ ;

: SIDE-CANONICAL ( n -- ) {: off:n :}
   off 1+ BYTE@ {: present:n :}
   off 2 + BYTE@ BYTE>KIND {: kind:content-kind :}
   off 3 + BYTE@ {: binary:n :}
   off 8 + U64@ {: size:n :}
   off 16 + U64@ {: pathu:n :}
   present BOOL-BYTE? 0= binary BOOL-BYTE? 0= or if E-SYNTAX throw then
   present 0= if
      kind KIND-ABSENT? 0= pathu 0<> or size 0<> or binary 0<> or if E-SYNTAX throw then
   else
      kind KIND-ABSENT? if E-SYNTAX throw then
      off SIDE-PATH$ PATH-SAFE? 0= if E-SYNTAX throw then
   then
   kind KIND-FILE? if
      size 0= binary 0<> and if E-SYNTAX throw then
   else
      binary 0<> if E-SYNTAX throw then
   then
   kind KIND-GITLINK? kind KIND-ABSENT? or if
      size 0<> binary 0<> or if E-SYNTAX throw then
      off SIDE-DIGEST$ ZERO-DIGEST $20 STR= 0= if E-DIGEST throw then
   else
      size 0= if
         s" " CHECK-DIGEST SHA256
         off SIDE-DIGEST$ CHECK-DIGEST $20 STR= 0= if E-DIGEST throw then
      then
   then ;

: SIDE-CHECK ( n n -- ) {: off:n sideu:n :}
   sideu SIDE-HEAD-U < if E-SYNTAX throw then
   off sideu RANGE drop
   off s" S" MAGIC? 0= if E-SYNTAX throw then
   off 4 + SIDE-RESERVED-U ZERO-RANGE? 0= if E-SYNTAX throw then
   off 24 + U64@ 0<> if E-SYNTAX throw then
   off SIDE-TOTAL sideu <> if E-SYNTAX throw then
   off SIDE-CANONICAL ;

: ROW-CHECK ( n -- ) {: ordinal:n :}
   CURSOR @ {: off:n :}
   off ROW-HEAD-U RANGE drop
   off BYTE@ ROW-C <> if E-SYNTAX throw then
   off 1+ HEAD-RESERVED-U ZERO-RANGE? 0= if E-SYNTAX throw then
   off 8 + U64@ ordinal <> if E-SYNTAX throw then
   off 16 + U64@ {: rowu:n :}
   off 24 + U64@ {: oldu:n :}
   off 32 + U64@ {: newu:n :}
   rowu ROW-HEAD-U < if E-SYNTAX throw then
   ROW-HEAD-U oldu CHECK-ADD newu CHECK-ADD rowu <> if E-SYNTAX throw then
   off rowu CHECK-ADD TRAILER-OFF @ > if E-CAPACITY throw then
   off ROW-HEAD-U + oldu SIDE-CHECK
   off ROW-HEAD-U + oldu + newu SIDE-CHECK
   off rowu + CURSOR ! ;

: HEADER-CHECK ( -- )
   0 s" HABUSIDE" MAGIC? 0= if E-SYNTAX throw then
   8 BYTE@ VERSION <> if E-SYNTAX throw then
   9 HEAD-RESERVED-U ZERO-RANGE? 0= if E-SYNTAX throw then
   16 U64@ VIEW-N !
   24 U64@ PAYLOAD-U !
   104 8 ZERO-RANGE? 0= if E-SYNTAX throw then
   HEADER-U PAYLOAD-U @ CHECK-ADD TRAILER-OFF !
   TRAILER-OFF @ TRAILER-U CHECK-ADD VIEW-U @ <> if E-CAPACITY throw then ;

: TRAILER-CHECK ( -- )
   TRAILER-OFF @ {: off:n :}
   off s" EDISUBAH" MAGIC? 0= if E-SYNTAX throw then
   off 8 + BYTE@ VERSION <> if E-SYNTAX throw then
   off 9 + HEAD-RESERVED-U ZERO-RANGE? 0= if E-SYNTAX throw then
   off 16 + U64@ VIEW-N @ <> if E-SYNTAX throw then
   off 24 + U64@ VIEW-U @ <> if E-SYNTAX throw then
   32 $20 BYTES$ off 32 + $20 BYTES$ STR= 0= if E-DIGEST throw then ;

: DIGEST-CHECK ( -- )
   SHA256-RESET
   VIEW-A@ 32 SHA256-UPDATE
   ZERO-DIGEST $20 SHA256-UPDATE
   VIEW-A@ 64 + TRAILER-OFF @ 64 - SHA256-UPDATE
   CHECK-DIGEST SHA256-FINAL
   CHECK-DIGEST $20 32 $20 BYTES$ STR= 0= if E-DIGEST throw then ;

: ROWS-CHECK ( -- )
   HEADER-U CURSOR !
   0 begin dup VIEW-N @ < while
      dup ROW-CHECK
      1+
   repeat drop
   CURSOR @ TRAILER-OFF @ <> if E-SYNTAX throw then ;

: ENSURE-VIEW ( n -- ) {: need:n :}
   need VIEW-CAP @ <= if exit then
   VIEW-CAP @ 0= if MEM-64K else VIEW-CAP @ then GROW-CAP !
   begin GROW-CAP @ need < while
      GROW-CAP @ GROW-CAP @ CHECK-ADD GROW-CAP !
   repeat
   GROW-CAP @ MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop VIEW-A !
   GROW-CAP @ VIEW-CAP ! ;

: SELECT-RESET ( -- )
   -1 SELECT-I ! HEADER-U SELECT-NEXT !
   0 SELECT-OLD ! 0 SELECT-NEW ! 0 SELECT-STEPS ! ;

: SELECTED ( -- )
   REQUIRE-VALID
   SELECT-I @ 0 < if E-SYNTAX throw then ;

: SELECT-NEXT? ( -- bool )
   SELECT-I @ 1+ VIEW-N @ >= if false exit then
   SELECT-NEXT @ {: row:n :}
   row 24 + U64@ {: oldu:n :}
   row 16 + U64@ {: rowu:n :}
   row ROW-HEAD-U + SELECT-OLD !
   row ROW-HEAD-U + oldu + SELECT-NEW !
   row rowu + SELECT-NEXT !
   SELECT-I @ 1+ SELECT-I !
   SELECT-STEPS @ 1+ SELECT-STEPS !
   true ;

: SELECT ( n -- ) {: wanted:n :}
   wanted 0 < wanted VIEW-N @ >= or if E-SYNTAX throw then
   wanted SELECT-I @ < if SELECT-RESET then
   begin SELECT-I @ wanted < while
      SELECT-NEXT? 0= if E-SYNTAX throw then
   repeat ;

: SIDE-PATH-COPY ( n ptr u8 n -- n ) {: off:n dst:ptr cap:n :}
   off SIDE-PATH$ {: a:ptr u:n :}
   u cap > if E-CAPACITY throw then
   a dst u BYTE-COPY
   u ;

: SIDE-DIGEST-COPY ( n ptr u8 -- ) {: off:n dst:ptr :}
   off SIDE-DIGEST$ drop dst $20 BYTE-COPY ;

public

: VALIDATE ( ptr u8 n -- n ) {: a:ptr u:n :}
   false VIEW-VALID !
   u HEADER-U TRAILER-U + < if E-CAPACITY throw then
   u ENSURE-VIEW
   a VIEW-A@ u BYTE-COPY
   u VIEW-U !
   HEADER-CHECK
   TRAILER-CHECK
   DIGEST-CHECK
   ROWS-CHECK
   SELECT-RESET
   true VIEW-VALID !
   VIEW-N @ ;

: VALIDATE-BINDING ( ptr u8 n ptr u8 n -- n )
   {: artifact:ptr artifactu:n meta:ptr metau:n :}
   artifact artifactu VALIDATE {: count:n :}
   false VIEW-VALID !
   64 U64@ metau <> if E-DIGEST throw then
   meta metau CHECK-DIGEST SHA256
   CHECK-DIGEST $20 72 $20 BYTES$ STR= 0= if E-DIGEST throw then
   true VIEW-VALID !
   count ;

: COUNT ( -- n )
   REQUIRE-VALID
   VIEW-N @ ;

: TRAVERSAL-STEPS ( -- n )
   REQUIRE-VALID
   SELECT-STEPS @ ;

: ROW-SELECT ( n -- )
   REQUIRE-VALID
   SELECT ;

: ROW-NEXT? ( -- bool )
   REQUIRE-VALID
   SELECT-NEXT? ;

: METADATA-SIZE ( -- n )
   REQUIRE-VALID
   64 U64@ ;

: METADATA-DIGEST ( ptr u8 -- )
   REQUIRE-VALID
   72 $20 BYTES$ drop swap $20 BYTE-COPY ;

: OLD-PRESENT? ( -- bool )
   SELECTED
   SELECT-OLD @ 1+ BYTE@ 0<> ;

: NEW-PRESENT? ( -- bool )
   SELECTED
   SELECT-NEW @ 1+ BYTE@ 0<> ;

: OLD-KIND ( -- content-kind )
   SELECTED
   SELECT-OLD @ 2 + BYTE@ BYTE>KIND ;

: NEW-KIND ( -- content-kind )
   SELECTED
   SELECT-NEW @ 2 + BYTE@ BYTE>KIND ;

: OLD-BINARY? ( -- bool )
   SELECTED
   SELECT-OLD @ 3 + BYTE@ 0<> ;

: NEW-BINARY? ( -- bool )
   SELECTED
   SELECT-NEW @ 3 + BYTE@ 0<> ;

: OLD-CONTENT-SIZE ( -- n )
   SELECTED
   SELECT-OLD @ 8 + U64@ ;

: NEW-CONTENT-SIZE ( -- n )
   SELECTED
   SELECT-NEW @ 8 + U64@ ;

: OLD-PATH ( ptr u8 n -- n ) {: dst:ptr cap:n :}
   SELECTED
   SELECT-OLD @ dst cap SIDE-PATH-COPY ;

: NEW-PATH ( ptr u8 n -- n ) {: dst:ptr cap:n :}
   SELECTED
   SELECT-NEW @ dst cap SIDE-PATH-COPY ;

: OLD-DIGEST ( ptr u8 -- )
   SELECTED
   SELECT-OLD @ swap SIDE-DIGEST-COPY ;

: NEW-DIGEST ( ptr u8 -- )
   SELECTED
   SELECT-NEW @ swap SIDE-DIGEST-COPY ;

;package
