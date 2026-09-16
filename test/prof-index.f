\ The profiler's pc index answers exactly what an exhaustive scan answers.
\
\ `prof-on` builds a pc-sorted live-range index of every dictionary record that
\ owns code; the SIGALRM handler searches it instead of walking the dictionary,
\ and `prof-pc>rec` runs that same search, so this fixture tests the attribution
\ the handler performs rather than a copy of it.
\
\ THE RULE BOTH SIDES STATE: the live record with the greatest (start, record
\ index) at or below the pc, reported only when the pc is below that record's
\ end. LINREF below states it by scanning every record in dictionary order;
\ CHECK-PC compares the two answers. Records with no code of their own - a
\ package row, a retired row, an empty body - are in neither answer.
require lib/errors.f
require lib/test.f

1 set-tier

\ ---- the synthetic tail --------------------------------------------------
\ Records defined here land at the top of the dictionary, so the exhaustive
\ sweep below covers bodies of every shape this engine emits: short and long
\ colon words, a created cell with no code, a `does>` child whose span is a
\ suffix of its parent's, a word retired after it was defined, and the gaps
\ between them.
package PROF-INDEX-FIXTURE
public
: PIF-TINY ( -- n ) 1 ;
: PIF-SMALL ( n -- n ) 1+ ;
: PIF-MED ( n -- n ) {: a:n :} a a * a + a 3 * - 7 + ;
: PIF-LONG ( n -- n ) {: a:n :}
   a 0 begin dup 8 < while over + swap 1+ swap repeat drop
   dup 3 * over - swap 5 + + ;
: PIF-CALLER ( n -- n ) PIF-SMALL PIF-MED PIF-LONG ;
create PIF-CELL 8 allot
: PIF-MAKE ( n -- ) create , does> ( -- n ) @ ;
7 PIF-MAKE PIF-SEVEN
;package

\ A retired record keeps its code but leaves the index: neither answer names it.
: PIF-DOOMED ( -- n ) 42 ;
undefine PIF-DOOMED

package PROF-INDEX-TEST
private

variable BEST        \ the reference scan's best record so far
variable BESTS       \ that record's start address
variable GAP-PC      \ a pc in a gap between two spans, 0 when none was found
variable PROBES

: REC-CODE-BYTES ( n -- n ) {: i:n :}   \ 0 when record i owns no code
   i XREF-REC {: rec:ptr :}
   rec XREF-RETIRED? if 0 exit then
   rec XREF-START 0= if 0 exit then
   rec XREF-CODE-BYTES ;

: REC-START ( n -- n )
   XREF-REC XREF-START ;

: REF-ONE ( n n -- ) {: pc:n i:n :}
   i REC-CODE-BYTES 0= if exit then
   i REC-START {: st:n :}
   st pc > if exit then
   st BESTS @ < if exit then          \ ties keep the later record, as a stable sort does
   i BEST !  st BESTS ! ;

: LINREF ( n -- n ) {: pc:n :}
   -1 BEST !  0 BESTS !
   ndict@ 0 ?do pc i REF-ONE loop
   BEST @ {: b:n :}
   b 0 < if -1 exit then
   BESTS @ b REC-CODE-BYTES + pc > if b exit then
   -1 ;

: CHECK-PC ( n -- ) {: pc:n :}
   PROBES @ 1+ PROBES !
   pc prof-pc>rec  pc LINREF  T= ;

: CHECK-REC ( n -- ) {: i:n :}   \ every boundary of record i's span
   i REC-CODE-BYTES {: len:n :}
   len 0= if exit then
   i REC-START {: st:n :}
   st 4 - CHECK-PC
   st CHECK-PC
   st 4 + CHECK-PC
   st len 2 / + CHECK-PC
   st len + 4 - CHECK-PC
   st len + CHECK-PC ;

\ ---- the whole dictionary, then every record of the synthetic tail ---------
: SWEEP-STRIDE ( n -- ) {: stride:n :}
   ndict@ 0 ?do i CHECK-REC stride +loop ;

: SWEEP-TAIL ( n -- ) {: n:n :}
   ndict@ n - 0 max {: from:n :}
   ndict@ from ?do i CHECK-REC loop ;

\ A pc between two spans belongs to no record. Take the first gap above the
\ lowest record's end rather than assuming one exists at a fixed place.
: FIND-GAP ( -- )
   0 GAP-PC !
   ndict@ 1- 0 ?do
      GAP-PC @ 0= if
         i REC-CODE-BYTES 0<> if
            i REC-START i REC-CODE-BYTES + {: e:n :}
            i 1+ REC-CODE-BYTES 0<> if
               i 1+ REC-START e > if e GAP-PC ! then
            then
         then
      then
   loop ;

: CHECK-OUTSIDE ( -- )
   0 CHECK-PC
   4 CHECK-PC
   -1 1 rshift CHECK-PC                \ far above every span
   FIND-GAP
   GAP-PC @ 0<> if GAP-PC @ CHECK-PC then ;

: CHECK-UNARMED ( -- )   \ no index yet: the search answers "no record", never a guess
   ndict@ 1- REC-START prof-pc>rec -1 T=
   ndict@ 1- REC-START 4 + prof-pc>rec -1 T= ;

: RUN ( -- )
   T-RESET
   0 PROBES !
   CHECK-UNARMED
   0 prof-on                           \ builds the index; the limit never fires
   CHECK-OUTSIDE
   200 SWEEP-TAIL
   97 SWEEP-STRIDE
   s" prof-index probes " type PROBES @ . cr
   T-REPORT ;

public
: MAIN ( -- ) RUN ;
;package

PROF-INDEX-TEST:MAIN
