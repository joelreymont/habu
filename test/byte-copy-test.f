require lib/test.f

package BYTE-COPY-TEST

512 constant CAP
create ACTUAL CAP allot
create EXPECT CAP allot

: INIT ( -- )
   CAP 0 ?do i 251 mod dup ACTUAL i + c! EXPECT i + c! loop ;

\ Deliberately bytewise: pin forward overlap semantics independently of chunks.
: REFERENCE ( ptr u8 ptr u8 n -- ) {: src dst count :}
   count 0 ?do src i + c@ dst i + c! loop ;

: CHECK-COPY ( n n n -- ) {: source dest count :}
   INIT
   EXPECT source + EXPECT dest + count REFERENCE
   ACTUAL source + ACTUAL dest + count BYTE-COPY
   ACTUAL CAP EXPECT CAP T$= ;

: SMALL ( -- )
   81 0 ?do i {: count :}
      8 0 ?do i {: offset :}
         8 0 ?do 16 offset + 256 i + count CHECK-COPY loop
      loop
   loop ;

: OVERLAPS ( -- )
   81 0 ?do
      65 0 ?do 96 64 i + j CHECK-COPY loop
   loop ;

4096 constant LARGE
create BIG-SRC LARGE 16 + allot
create BIG-DST LARGE 16 + allot

: LARGE-CASE ( n n -- ) {: src-offset dst-offset :}
   LARGE 16 + 0 ?do
      i 251 mod BIG-SRC i + c! 253 BIG-DST i + c!
   loop
   BIG-SRC src-offset + BIG-DST dst-offset + LARGE BYTE-COPY
   BIG-SRC src-offset + LARGE BIG-DST dst-offset + LARGE T$=
   dst-offset 0 ?do BIG-DST i + c@ 253 T= loop
   LARGE 16 + LARGE dst-offset + ?do BIG-DST i + c@ 253 T= loop ;

: RUN ( -- )
   T-RESET SMALL OVERLAPS
   0 0 LARGE-CASE 1 7 LARGE-CASE
   \ A typed empty copy must not touch either address.
   NULL-PTR NULL-PTR 0 >LEN BYTE-COPY-LEN
   T-REPORT ;
RUN
;package
