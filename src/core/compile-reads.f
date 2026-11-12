\ compile-reads.f - what a compile reads besides its source: the roots it looked
\ up and what they resolved to, and the environment variables it read.
\ A recording is one mapping of reads. Each read is three cells (kind, first
\ length, second length), the first bytes, the second bytes, then zero bytes to
\ the next cell boundary: ROOT-READ holds an absolute spelling and its canonical
\ directory, ENV-READ a name and its value, ENV-ABSENT a name and no bytes.
package COMPILE-READS
public

1 constant ROOT-READ
2 constant ENV-READ
3 constant ENV-ABSENT

private

$1000 constant ROOM-MIN                    \ a first mapping's bytes

\ The process's record: active flag, mapping, its capacity, bytes used. Readers
\ reach it by SRCLOC:READS-CELL, the running hb's in a build.
0 constant REC-ACTIVE
CELL constant REC-MAP
2 cells constant REC-CAP
3 cells constant REC-USED
4 cells constant REC-BYTES
create READS-REC REC-BYTES SCRATCH-ALLOT   \ a saved program starts with it empty
SRCLOC:READS-CELL RESERVED-PTR-U8-CELL READS-CELL
READS-REC READS-CELL ptr-cell!             \ a build's store goes to its image
CAST: REC-CELLS ( ptr u8 -- ptr ptr u8 )
: REC ( -- ptr u8 ) READS-CELL @ ;
: ACTIVE ( -- ptr n ) REC REC-ACTIVE + CELL-VIEW ;
: MAP ( -- ptr ptr u8 ) REC REC-CELLS REC-MAP CELL / ptr-field ;
: CAP ( -- ptr n ) REC REC-CAP + CELL-VIEW ;
: USED ( -- ptr n ) REC REC-USED + CELL-VIEW ;

: SAVED ( -- n ptr u8 n n ) ACTIVE @ MAP @ CAP @ USED @ ;
: RESTORE ( n ptr u8 n n -- ) USED ! CAP ! MAP ! ACTIVE ! ;

: UNMAP ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0= if exit then
   a u munmap 0 < if s" compile reads: cannot release a recording" INCLUDE-IO-RC die then ;

: RELEASE ( -- )
   MAP @ CAP @ UNMAP
   NULL-PTR MAP !  0 CAP !  0 USED ! ;

: SCOPE ( n [ -- ] -- )
   {: on:n q :}
   SAVED {: old-on:n old-map:ptr old-cap:n old-used:n :}
   on ACTIVE !  NULL-PTR MAP !  0 CAP !  0 USED !
   q catch {: rc:n :}
   RELEASE
   old-on old-map old-cap old-used RESTORE
   rc 0= 0= if rc throw then ;

: PAD ( n -- n ) CELL 1- + CELL / CELL * ;
: AT ( n -- ptr u8 ) MAP @ + ;
: CELL@ ( n -- n ) AT CELL-VIEW @ ;

\ The bytes from the start of the read at off to the start of the next.
: SPAN ( n -- n ) {: off:n :}
   off CELL + CELL@ off 2 cells + CELL@ + PAD 3 cells + ;

: SAME? ( n n ptr u8 n ptr u8 n -- bool )
   {: off:n kind:n a:ptr u:n b:ptr v:n :}
   off CELL@ kind =  off CELL + CELL@ u = and  off 2 cells + CELL@ v = and if
      off 3 cells + AT u a u CORE-STR=
      off 3 cells + u + AT v b v CORE-STR= and exit
   then
   0 0= 0= ;

: SEEN? ( n ptr u8 n ptr u8 n -- bool )
   {: kind:n a:ptr u:n b:ptr v:n :}
   0 begin dup USED @ < while
      dup kind a u b v SAME? if drop 0 0= exit then
      dup SPAN +
   repeat drop 0 0= 0= ;

: ROOM ( n -- ) {: need:n :}
   need CAP @ <= if exit then
   CAP @ 2 * need max ROOM-MIN max {: cap:n :}
   cap map-anon 0= 0= if drop s" compile reads: cannot map a recording" INCLUDE-IO-RC die then
   {: fresh:ptr :}
   MAP @ fresh USED @ BYTE-COPY
   MAP @ CAP @ UNMAP
   fresh MAP !
   cap CAP ! ;

: APPEND ( n ptr u8 n ptr u8 n -- )
   {: kind:n a:ptr u:n b:ptr v:n :}
   ACTIVE @ 0= if exit then
   u 0 < v 0 < or if s" compile reads: negative length" INCLUDE-IO-RC die then
   kind a u b v SEEN? if exit then
   USED @ {: off:n :}
   u v + PAD 3 cells + {: size:n :}
   off size + ROOM
   kind off AT CELL-VIEW !
   u off CELL + AT CELL-VIEW !
   v off 2 cells + AT CELL-VIEW !
   a off 3 cells + AT u BYTE-COPY
   b off 3 cells + u + AT v BYTE-COPY
   size 3 cells - u v + ?do 0 off 3 cells + i + AT c! loop
   off size + USED ! ;

public

\ Runs q with a fresh, empty, active recording; recordings nest, and an inner
\ one's reads are not the outer one's.
: RECORD ( [ -- ] -- ) {: q :} 1 q SCOPE ;

\ Runs q with no recording active.
: UNRECORDED ( [ -- ] -- ) {: q :} 0 q SCOPE ;

\ The active recording's bytes, valid until its next append or its end.
: READS$ ( -- ptr u8 n )
   ACTIVE @ 0= if NULL-PTR 0 exit then
   MAP @ USED @ ;

\ A root's absolute spelling and its canonical directory.
: ROOT+ ( ptr u8 n ptr u8 n -- ) {: a:ptr u:n b:ptr v:n :}
   ROOT-READ a u b v APPEND ;

\ An environment variable's name and value; a NULL-PTR value is an absent one.
: ENV+ ( ptr u8 n ptr u8 n -- ) {: a:ptr u:n b:ptr v:n :}
   b NULL-PTR - 0= if ENV-ABSENT a u b 0 APPEND exit then
   ENV-READ a u b v APPEND ;

;package
