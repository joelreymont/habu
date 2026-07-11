\ map-test.f - focused tests for fixed-capacity map layout helpers.
\ Run: cat lib/errors.f lib/string.f lib/map.f lib/map-test.f | bin/hb

require lib/errors.f
require lib/string.f
require lib/map.f
require test/checker-assert.f

1 constant MT-EX-FAIL
8 constant MT-CAP
3 constant MT-FULL-CAP

variable MT-CASE
variable MT-FAIL
variable MT-EACH-COUNT
variable MT-EACH-SUM
variable MT-EACH-LEN-SUM

create MT-MAP MT-CAP >COUNT MAP-CELLS COUNT>N cells allot
create MT-FULL-MAP MT-FULL-CAP >COUNT MAP-CELLS COUNT>N cells allot
create MT-EACH-KEYS MT-CAP cells allot
create MT-EACH-LENS MT-CAP cells allot
create MT-EACH-VALUES MT-CAP cells allot
create MT-KEY 97 c, 98 c, 99 c,
create MT-KEY-A 97 c,
create MT-KEY-B 98 c,
create MT-KEY-C 99 c,
create MT-KEY-D 100 c,
create MT-KEY-I 105 c,
create MT-KEY-Q 113 c,
create MT-KEY-Z 122 c,

: MT-ASSERT ( bool -- )
   MT-CASE @ 1 + MT-CASE !
   0= if
      s" map-test: assertion " type MT-CASE @ . s" failed" type cr
      MT-FAIL @ 1 + MT-FAIL !
   then ;

: MT= ( n n -- )
   = MT-ASSERT ;

: MT-RANGE ( n n -- ) {: ix cap :}
   ix 0 >= ix cap < and MT-ASSERT ;

: MT-MAP-CHECK-CAP ( n -- )
   >COUNT MAP-CHECK-CAP ;

: MT-MAP-CHECK-LEN ( n -- )
   >LEN MAP-CHECK-LEN ;

: MT-MAP-CELLS ( n -- n )
   >COUNT MAP-CELLS COUNT>N ;

: MT-MAP-CAP@ ( ptr a -- n )
   MAP-CAP@ COUNT>N ;

: MT-MAP-CAP! ( n ptr a -- ) {: cap m:ptr :}
   cap >COUNT m MAP-CAP! ;

: MT-MAP-CHECK-HANDLE ( ptr a n -- ) {: m:ptr cap :}
   m cap >COUNT MAP-CHECK-HANDLE ;

: MT-MAP-COUNT@ ( ptr a -- n )
   MAP-COUNT@ COUNT>N ;

: MT-MAP-DELETED@ ( ptr a -- n )
   MAP-DELETED@ COUNT>N ;

: MT-MAP-COUNT! ( n ptr a -- ) {: count m:ptr :}
   count >COUNT m MAP-COUNT! ;

: MT-MAP-DELETED! ( n ptr a -- ) {: deleted m:ptr :}
   deleted >COUNT m MAP-DELETED! ;

: MT-MAP-CHECK-INDEX ( ptr a n -- ) {: m:ptr ix :}
   m ix >IDX MAP-CHECK-INDEX ;

: MT-MAP-SLOT ( ptr a n -- ptr a ) {: m:ptr ix :}
   m ix >IDX MAP-SLOT ;

: MT-MAP-SLOT-FIELD ( ptr a n n -- ptr a ) {: m:ptr ix off :}
   m ix >IDX off >OFF MAP-SLOT-FIELD ;

: MT-MAP-SLOT-STATE@ ( ptr a n -- slot-state ) {: m:ptr ix:n :}
   m ix >IDX MAP-SLOT-STATE@ ;

: MT-MAP-SLOT-STATE! ( slot-state ptr a n -- ) {: m:ptr ix:n :}   \ enum stays on stack (no enum locals)
   m ix >IDX MAP-SLOT-STATE! ;

: MT-MAP-SLOT-HASH@ ( ptr a n -- n ) {: m:ptr ix :}
   m ix >IDX MAP-SLOT-HASH@ ;

: MT-MAP-SLOT-HASH! ( n ptr a n -- ) {: hash m:ptr ix :}
   hash m ix >IDX MAP-SLOT-HASH! ;

: MT-MAP-SLOT-KEY-A@ ( ptr a n -- ptr u8 ) {: m:ptr ix :}
   m ix >IDX MAP-SLOT-KEY-A@ ;

: MT-MAP-SLOT-KEY-A! ( ptr u8 ptr a n -- ) {: key:ptr m:ptr ix :}
   key m ix >IDX MAP-SLOT-KEY-A! ;

: MT-MAP-SLOT-KEY-U@ ( ptr a n -- n ) {: m:ptr ix :}
   m ix >IDX MAP-SLOT-KEY-U@ LEN>N ;

: MT-MAP-SLOT-KEY-U! ( n ptr a n -- ) {: len m:ptr ix :}
   len >LEN m ix >IDX MAP-SLOT-KEY-U! ;

: MT-MAP-SLOT-VALUE@ ( ptr a n -- a ) {: m:ptr ix :}
   m ix >IDX MAP-SLOT-VALUE@ ;

: MT-MAP-SLOT-VALUE! ( a ptr a n -- ) {: value m:ptr ix :}
   value m ix >IDX MAP-SLOT-VALUE! ;

: MT-MAP-SLOT-CLEAR ( ptr a n -- ) {: m:ptr ix :}
   m ix >IDX MAP-SLOT-CLEAR ;

: MT-MAP-INIT ( ptr a n -- ) {: m:ptr cap :}
   m cap >COUNT MAP-INIT ;

: MT-MAP-HASH ( ptr u8 n -- n ) {: a:ptr u :}
   a u >LEN MAP-HASH ;

: MT-MAP-INDEX ( n n -- n ) {: hash cap :}
   hash cap >COUNT MAP-INDEX IDX>N ;

: MT-MAP-PROBE ( n n n -- n ) {: hash step cap :}
   hash step >COUNT cap >COUNT MAP-PROBE IDX>N ;

: MT-MAP-SLOT-MATCH? ( ptr a n n ptr u8 n -- bool ) {: m:ptr ix hash key:ptr len :}
   m ix >IDX hash key len >LEN MAP-SLOT-MATCH? ;

: MT-MAP-LOCATE-SLOT ( n ptr a n ptr u8 n n -- n map-loc ) {: fm:n m:ptr ix:n key:ptr len:n hash:n :}
   fm m ix >IDX key len >LEN hash MAP-LOCATE-SLOT ;

: MT-MAP-LOCATE ( ptr a n ptr u8 n -- map-loc n ) {: m:ptr cap:n key:ptr len:n :}
   m cap >COUNT key len >LEN MAP-LOCATE ;

: MT-LOC-KIND ( map-loc -- n )                       \ 0 full, 1 free, 2 found
   MATCH map-loc
     full OF 0 ENDOF
     free OF drop 1 ENDOF
     found OF drop 2 ENDOF
   ;MATCH ;

: MT-LOC-IDX ( map-loc -- n )                        \ carried slot index, -1 for full
   MATCH map-loc
     full OF -1 ENDOF
     free OF IDX>N ENDOF
     found OF IDX>N ENDOF
   ;MATCH ;

: MT-ASSERT-LOC ( map-loc n n -- ) {: kind:n ix:n :} \ verdict kind + carried idx
   dup MT-LOC-KIND kind MT=
   MT-LOC-IDX ix MT= ;

: MT-MAP-SLOT-INSERT ( a ptr a n n ptr u8 n -- ) {: value m:ptr ix hash key:ptr len :}
   value m ix >IDX hash key len >LEN MAP-SLOT-INSERT ;

: MT-MAP-GET ( ptr a n ptr u8 n -- option<n> ) {: m:ptr cap:n key:ptr len:n :}
   m cap >COUNT key len >LEN MAP-GET ;

: MT-MAP-HAS? ( ptr a n ptr u8 n -- bool ) {: m:ptr cap key:ptr len :}
   m cap >COUNT key len >LEN MAP-HAS? ;

: MT-MAP-SET ( n ptr a n ptr u8 n -- ) {: value m:ptr cap key:ptr len :}
   value m cap >COUNT key len >LEN MAP-SET ;

: MT-MAP-EACH ( ptr a n [ ptr u8 len n -- ] -- ) {: m:ptr cap q :}
   m cap >COUNT q MAP-EACH ;

: MT-FILL-STORAGE ( -- )
   MT-CAP MT-MAP-CELLS 0 ?do
      777 MT-MAP i cells + !
   loop ;

: MT-EACH-CLEAR-CELLS ( ptr a -- ) {: a:ptr :}
   MT-CAP 0 ?do
      0 a i cells + !
   loop ;

: MT-EACH-RESET ( -- )
   0 MT-EACH-COUNT !
   0 MT-EACH-SUM !
   0 MT-EACH-LEN-SUM !
   MT-EACH-KEYS MT-EACH-CLEAR-CELLS
   MT-EACH-LENS MT-EACH-CLEAR-CELLS
   MT-EACH-VALUES MT-EACH-CLEAR-CELLS ;

: MT-EACH-KEY-CODE ( ptr u8 len -- n ) {: key:ptr len :}
   len LEN>N 0 > if key c@ else 0 then ;

: MT-EACH-RECORD ( ptr u8 len n -- ) {: key:ptr len value :}
   MT-EACH-COUNT @ {: ix :}
   key len MT-EACH-KEY-CODE MT-EACH-KEYS ix cells + !
   len LEN>N MT-EACH-LENS ix cells + !
   value MT-EACH-VALUES ix cells + !
   ix 1 + MT-EACH-COUNT !
   MT-EACH-SUM @ value + MT-EACH-SUM !
   MT-EACH-LEN-SUM @ len LEN>N + MT-EACH-LEN-SUM ! ;

: MT-ASSERT-CLEAR-SLOT ( n -- ) {: ix :}
   MT-MAP ix MT-MAP-SLOT-STATE@ MAP-EMPTY? MT-ASSERT
   MT-MAP ix MT-MAP-SLOT-HASH@ 0 MT=
   MT-MAP ix MT-MAP-SLOT-KEY-A@ NULL$ drop = MT-ASSERT
   MT-MAP ix MT-MAP-SLOT-KEY-U@ 0 MT=
   MT-MAP ix MT-MAP-SLOT-VALUE@ 0 MT= ;

: MT-BAD-CAP-ZERO ( -- )
   0 MT-MAP-CHECK-CAP ;

: MT-BAD-CAP-NEG ( -- )
   -1 MT-MAP-CHECK-CAP ;

: MT-BAD-CAP-HIGH ( -- )
   MAP-MAX-CAP 1 + MT-MAP-CHECK-CAP ;

: MT-GOOD-CAP ( -- )
   1 MT-MAP-CHECK-CAP ;

: MT-BAD-COUNT-NEG ( -- )
   MT-MAP MT-CAP MT-MAP-INIT
   -1 MT-MAP MT-MAP-COUNT! ;

: MT-BAD-COUNT-HIGH ( -- )
   MT-MAP MT-CAP MT-MAP-INIT
   MT-CAP 1 + MT-MAP MT-MAP-COUNT! ;

: MT-BAD-COUNT-SUM ( -- )
   MT-MAP MT-CAP MT-MAP-INIT
   MT-CAP 1 - MT-MAP MT-MAP-DELETED!
   2 MT-MAP MT-MAP-COUNT! ;

: MT-BAD-DELETED-NEG ( -- )
   MT-MAP MT-CAP MT-MAP-INIT
   -1 MT-MAP MT-MAP-DELETED! ;

: MT-BAD-DELETED-HIGH ( -- )
   MT-MAP MT-CAP MT-MAP-INIT
   MT-CAP 1 + MT-MAP MT-MAP-DELETED! ;

: MT-BAD-DELETED-SUM ( -- )
   MT-MAP MT-CAP MT-MAP-INIT
   MT-CAP 1 - MT-MAP MT-MAP-COUNT!
   2 MT-MAP MT-MAP-DELETED! ;

: MT-BAD-INDEX-NEG ( -- )
   MT-MAP -1 MT-MAP-CHECK-INDEX ;

: MT-BAD-INDEX-HIGH ( -- )
   MT-MAP MT-CAP MT-MAP-CHECK-INDEX ;

: MT-BAD-FIELD-HIGH ( -- )
   MT-MAP 0 MAP-SLOT-CELLS MT-MAP-SLOT-FIELD drop ;

: MT-BAD-HASH ( -- )
   -1 MT-MAP 0 MT-MAP-SLOT-HASH! ;

: MT-BAD-KEY-U ( -- )
   -1 MT-MAP 0 MT-MAP-SLOT-KEY-U! ;

: MT-BAD-STATE-TAG ( -- )
   MT-MAP MT-CAP MT-MAP-INIT
   3 MT-MAP 0 MAP-SLOT-STATE-OFF MT-MAP-SLOT-FIELD !
   MT-MAP 0 MT-MAP-SLOT-STATE@ drop ;

: MT-TEST-CONSTANTS ( -- )
   MAP-CAP-OFF 0 MT=
   MAP-COUNT-OFF 1 MT=
   MAP-DELETED-OFF 2 MT=
   MAP-HEADER-CELLS 3 MT=
   MAP-SLOT-STATE-OFF 0 MT=
   MAP-SLOT-HASH-OFF 1 MT=
   MAP-SLOT-KEY-A-OFF 2 MT=
   MAP-SLOT-KEY-U-OFF 3 MT=
   MAP-SLOT-VALUE-OFF 4 MT=
   MAP-SLOT-CELLS 5 MT= ;

\ Full constructor x predicate matrix for the slot-state enum.
: MT-TEST-STATES ( -- )
   SLOT--STATE:EMPTY MAP-EMPTY? MT-ASSERT
   SLOT--STATE:DELETED MAP-DELETED? MT-ASSERT
   SLOT--STATE:OCCUPIED MAP-OCCUPIED? MT-ASSERT
   SLOT--STATE:EMPTY MAP-DELETED? 0= MT-ASSERT
   SLOT--STATE:EMPTY MAP-OCCUPIED? 0= MT-ASSERT
   SLOT--STATE:DELETED MAP-EMPTY? 0= MT-ASSERT
   SLOT--STATE:DELETED MAP-OCCUPIED? 0= MT-ASSERT
   SLOT--STATE:OCCUPIED MAP-EMPTY? 0= MT-ASSERT
   SLOT--STATE:OCCUPIED MAP-DELETED? 0= MT-ASSERT ;

\ Negative checked regressions: a raw n can no longer pose as a slot state,
\ a slot state cannot launder back to n, and enums do not compare with `=`.
\ The positive baseline proves the rejects are type errors, not noise.
: MT-TEST-STATE-TYPES ( -- )
   s" MTP1 ( slot-state ptr a idx -- ) MAP-SLOT-STATE!" CHECK-QUIET-CANDIDATE! -1 MT=
   s" MTN1 ( n ptr a idx -- ) MAP-SLOT-STATE!" CHECK-QUIET-CANDIDATE! 0 MT=
   s" MTN2 ( ptr a idx -- n ) MAP-SLOT-STATE@" CHECK-QUIET-CANDIDATE! 0 MT=
   s" MTN3 ( slot-state slot-state -- bool ) =" CHECK-QUIET-CANDIDATE! 0 MT=
   s" MTN4 ( ptr a idx -- ptr slot-state ) MAP-SLOT-STATE-OFF >OFF MAP-SLOT-FIELD" CHECK-QUIET-CANDIDATE! 0 MT= ;

: MT-TEST-CHECKS ( -- )
   [: MT-GOOD-CAP ;] catch 0 MT=
   [: MT-BAD-CAP-ZERO ;] catch E-MAP-BAD-CAP MT=
   [: MT-BAD-CAP-NEG ;] catch E-MAP-BAD-CAP MT=
   [: MT-BAD-CAP-HIGH ;] catch E-MAP-BAD-CAP MT=
   MT-MAP MT-CAP MT-MAP-INIT
   MT-CAP MT-MAP MT-MAP-COUNT!  MT-MAP MT-MAP-COUNT@ MT-CAP MT=
   0 MT-MAP MT-MAP-COUNT!
   MT-CAP MT-MAP MT-MAP-DELETED!  MT-MAP MT-MAP-DELETED@ MT-CAP MT=
   [: MT-BAD-COUNT-NEG ;] catch E-MAP-BAD-CAP MT=
   [: MT-BAD-COUNT-HIGH ;] catch E-MAP-FULL MT=
   [: MT-BAD-COUNT-SUM ;] catch E-MAP-FULL MT=
   [: MT-BAD-DELETED-NEG ;] catch E-MAP-BAD-CAP MT=
   [: MT-BAD-DELETED-HIGH ;] catch E-MAP-FULL MT=
   [: MT-BAD-DELETED-SUM ;] catch E-MAP-FULL MT=
   MT-MAP 0 MT-MAP-CHECK-INDEX
   MT-MAP MT-CAP 1 - MT-MAP-CHECK-INDEX
   [: MT-BAD-INDEX-NEG ;] catch E-MAP-BAD-CAP MT=
   [: MT-BAD-INDEX-HIGH ;] catch E-MAP-BAD-CAP MT=
   [: MT-BAD-FIELD-HIGH ;] catch E-MAP-BAD-CAP MT=
   [: MT-BAD-HASH ;] catch E-MAP-BAD-CAP MT=
   [: MT-BAD-KEY-U ;] catch E-MAP-BAD-CAP MT=
   [: MT-BAD-STATE-TAG ;] catch E-BAD-TAG MT= ;

: MT-TEST-LAYOUT ( -- )
   MT-CAP MT-MAP-CELLS MAP-HEADER-CELLS MT-CAP MAP-SLOT-CELLS * + MT=
   MT-MAP MT-CAP MT-MAP-INIT
   MT-MAP MT-MAP-CAP@ MT-CAP MT=
   MT-MAP MT-MAP-COUNT@ 0 MT=
   MT-MAP MT-MAP-DELETED@ 0 MT=
   MT-MAP MAP-SLOTS MT-MAP MAP-HEADER-CELLS cells + = MT-ASSERT
   MT-MAP 0 MT-MAP-SLOT MT-MAP MAP-SLOTS = MT-ASSERT
   MT-MAP 1 MT-MAP-SLOT MT-MAP MAP-SLOTS MAP-SLOT-CELLS cells + = MT-ASSERT
   MT-MAP 2 MAP-SLOT-STATE-OFF MT-MAP-SLOT-FIELD MT-MAP 2 MT-MAP-SLOT = MT-ASSERT
   MT-MAP 2 MAP-SLOT-VALUE-OFF MT-MAP-SLOT-FIELD
      MT-MAP 2 MT-MAP-SLOT 4 cells + = MT-ASSERT ;

: MT-TEST-SLOT-FIELDS ( -- )
   MT-MAP MT-CAP MT-MAP-INIT
   SLOT--STATE:DELETED MT-MAP 2 MT-MAP-SLOT-STATE!
   MT-MAP 2 MAP-SLOT-STATE-OFF MT-MAP-SLOT-FIELD @ 1 MT=
   12345 MT-MAP 2 MT-MAP-SLOT-HASH!
   MT-KEY MT-MAP 2 MT-MAP-SLOT-KEY-A!
   3 MT-MAP 2 MT-MAP-SLOT-KEY-U!
   99 MT-MAP 2 MT-MAP-SLOT-VALUE!
   MT-MAP 2 MT-MAP-SLOT-STATE@ MAP-DELETED? MT-ASSERT
   MT-MAP 2 MT-MAP-SLOT-HASH@ 12345 MT=
   MT-MAP 2 MT-MAP-SLOT-KEY-A@ MT-KEY = MT-ASSERT
   MT-MAP 2 MT-MAP-SLOT-KEY-U@ 3 MT=
   MT-MAP 2 MT-MAP-SLOT-VALUE@ 99 MT=
   MT-MAP 2 MT-MAP-SLOT-CLEAR
   MT-MAP 2 MAP-SLOT-STATE-OFF MT-MAP-SLOT-FIELD @ 0 MT=
   2 MT-ASSERT-CLEAR-SLOT ;

: MT-TEST-INIT-CLEAR ( -- )
   MT-FILL-STORAGE
   MT-MAP MT-CAP MT-MAP-INIT
   MT-MAP MT-MAP-CAP@ MT-CAP MT=
   MT-MAP MT-MAP-COUNT@ 0 MT=
   MT-MAP MT-MAP-DELETED@ 0 MT=
   MT-CAP 0 ?do i MT-ASSERT-CLEAR-SLOT loop
   SLOT--STATE:OCCUPIED MT-MAP 3 MT-MAP-SLOT-STATE!
   88 MT-MAP 3 MT-MAP-SLOT-VALUE!
   4 MT-MAP MT-MAP-COUNT!
   2 MT-MAP MT-MAP-DELETED!
   MT-MAP MAP-CLEAR
   MT-MAP MT-MAP-COUNT@ 0 MT=
   MT-MAP MT-MAP-DELETED@ 0 MT=
   MT-CAP 0 ?do i MT-ASSERT-CLEAR-SLOT loop ;

: MT-PROBE-RANGE ( ptr u8 n n n -- ) {: a:ptr u step cap :}
   a u MT-MAP-HASH step cap MT-MAP-PROBE cap MT-RANGE ;

: MT-ASSERT-HIT ( ptr a n ptr u8 n n -- ) {: m:ptr cap:n key:ptr len:n want:n :}
   m cap key len MT-MAP-HAS? MT-ASSERT                  \ present -> HAS? true
   m cap key len MT-MAP-GET MATCH option
     none OF 0 0= 0= MT-ASSERT ENDOF                    \ hit expected, got NONE -> fail
     some OF want MT= ENDOF                             \ SOME -> value must equal want
   ;MATCH ;

: MT-ASSERT-MISS ( ptr a n ptr u8 n -- ) {: m:ptr cap:n key:ptr len:n :}
   m cap key len MT-MAP-HAS? 0= MT-ASSERT               \ absent -> HAS? false
   m cap key len MT-MAP-GET MATCH option
     none OF ENDOF                                      \ absent -> NONE, ok
     some OF drop 0 0= 0= MT-ASSERT ENDOF               \ got SOME -> fail
   ;MATCH ;

: MT-ASSERT-COLLISION-SLOT ( n ptr u8 -- ) {: step want:ptr :}
   MT-MAP MT-KEY-A 1 MT-MAP-HASH step MT-CAP MT-MAP-PROBE MT-MAP-SLOT-KEY-A@
      want = MT-ASSERT ;

: MT-ASSERT-EACH-ROW ( n n n n -- ) {: ix key len value :}
   MT-EACH-KEYS ix cells + @ key MT=
   MT-EACH-LENS ix cells + @ len MT=
   MT-EACH-VALUES ix cells + @ value MT= ;

: MT-FULL-INSERT ( -- )
   44 MT-FULL-MAP MT-FULL-CAP MT-KEY-D 1 MT-MAP-SET ;

: MT-TEST-HASH-PROBE ( -- )
   s" " MT-MAP-HASH MAP-HASH-SEED MT=
   s" alpha" MT-MAP-HASH s" alpha" MT-MAP-HASH MT=
   s" alpha" MT-MAP-HASH 0 >= MT-ASSERT
   s" beta" MT-MAP-HASH MT-CAP MT-MAP-INDEX MT-CAP MT-RANGE
   -1 MT-CAP MT-MAP-INDEX MT-CAP 1 - MT=
   s" alpha" 0 MT-CAP MT-PROBE-RANGE
   s" alpha" 1 MT-CAP MT-PROBE-RANGE
   s" alpha" MT-CAP MT-CAP MT-PROBE-RANGE
   s" alpha" -1 MT-CAP MT-PROBE-RANGE ;

: MT-TEST-MAP-SET-GET ( -- )
   MT-MAP MT-CAP MT-MAP-INIT
   MT-MAP MT-CAP MT-KEY-Z 1 MT-ASSERT-MISS
   11 MT-MAP MT-CAP MT-KEY-A 1 MT-MAP-SET
   MT-MAP MT-MAP-COUNT@ 1 MT=
   MT-MAP MT-CAP MT-KEY-A 1 MT-MAP-HAS? MT-ASSERT
   MT-MAP MT-CAP MT-KEY-A 1 11 MT-ASSERT-HIT
   MT-MAP MT-CAP MT-KEY-Z 1 MT-ASSERT-MISS
   22 MT-MAP MT-CAP MT-KEY-A 1 MT-MAP-SET
   MT-MAP MT-MAP-COUNT@ 1 MT=
   MT-MAP MT-CAP MT-KEY-A 1 22 MT-ASSERT-HIT ;

: MT-TEST-MAP-COLLISIONS ( -- )
   MT-MAP MT-CAP MT-MAP-INIT
   MT-KEY-A 1 MT-MAP-HASH MT-CAP MT-MAP-INDEX
      MT-KEY-I 1 MT-MAP-HASH MT-CAP MT-MAP-INDEX MT=
   MT-KEY-A 1 MT-MAP-HASH MT-CAP MT-MAP-INDEX
      MT-KEY-Q 1 MT-MAP-HASH MT-CAP MT-MAP-INDEX MT=
   11 MT-MAP MT-CAP MT-KEY-A 1 MT-MAP-SET
   22 MT-MAP MT-CAP MT-KEY-I 1 MT-MAP-SET
   33 MT-MAP MT-CAP MT-KEY-Q 1 MT-MAP-SET
   MT-MAP MT-MAP-COUNT@ 3 MT=
   0 MT-KEY-A MT-ASSERT-COLLISION-SLOT
   1 MT-KEY-I MT-ASSERT-COLLISION-SLOT
   2 MT-KEY-Q MT-ASSERT-COLLISION-SLOT
   MT-MAP MT-CAP MT-KEY-A 1 11 MT-ASSERT-HIT
   MT-MAP MT-CAP MT-KEY-I 1 22 MT-ASSERT-HIT
   MT-MAP MT-CAP MT-KEY-Q 1 33 MT-ASSERT-HIT
   44 MT-MAP MT-CAP MT-KEY-I 1 MT-MAP-SET
   MT-MAP MT-MAP-COUNT@ 3 MT=
   MT-MAP MT-CAP MT-KEY-A 1 11 MT-ASSERT-HIT
   MT-MAP MT-CAP MT-KEY-I 1 44 MT-ASSERT-HIT
   MT-MAP MT-CAP MT-KEY-Q 1 33 MT-ASSERT-HIT ;

: MT-TEST-MAP-FULL ( -- )
   MT-FULL-MAP MT-FULL-CAP MT-MAP-INIT
   11 MT-FULL-MAP MT-FULL-CAP MT-KEY-A 1 MT-MAP-SET
   22 MT-FULL-MAP MT-FULL-CAP MT-KEY-B 1 MT-MAP-SET
   33 MT-FULL-MAP MT-FULL-CAP MT-KEY-C 1 MT-MAP-SET
   MT-FULL-MAP MT-MAP-COUNT@ MT-FULL-CAP MT=
   [: MT-FULL-INSERT ;] catch E-MAP-FULL MT=
   MT-FULL-MAP MT-MAP-COUNT@ MT-FULL-CAP MT=
   44 MT-FULL-MAP MT-FULL-CAP MT-KEY-B 1 MT-MAP-SET
   MT-FULL-MAP MT-MAP-COUNT@ MT-FULL-CAP MT=
   MT-FULL-MAP MT-FULL-CAP MT-KEY-B 1 44 MT-ASSERT-HIT
   MT-FULL-MAP MT-FULL-CAP MT-KEY-D 1 MT-ASSERT-MISS ;

\ Tombstone transitions: occupied -> deleted skips the slot in lookups but
\ keeps the probe chain alive, and a later insert of the tombstoned key
\ reclaims the slot through MAP-SLOT-INSERT's deleted branch.
: MT-TEST-TOMBSTONE ( -- )
   MT-MAP MT-CAP MT-MAP-INIT
   11 MT-MAP MT-CAP MT-KEY-A 1 MT-MAP-SET
   22 MT-MAP MT-CAP MT-KEY-I 1 MT-MAP-SET
   MT-KEY-A 1 MT-MAP-HASH 0 MT-CAP MT-MAP-PROBE {: ixa:n :}
   SLOT--STATE:DELETED MT-MAP ixa MT-MAP-SLOT-STATE!
   1 MT-MAP MT-MAP-COUNT!
   1 MT-MAP MT-MAP-DELETED!
   MT-MAP ixa MT-MAP-SLOT-STATE@ MAP-DELETED? MT-ASSERT
   MT-MAP MT-CAP MT-KEY-A 1 MT-ASSERT-MISS               \ deleted -> lookup miss
   MT-MAP MT-CAP MT-KEY-I 1 22 MT-ASSERT-HIT             \ probe walks past the tombstone
   33 MT-MAP MT-CAP MT-KEY-A 1 MT-MAP-SET                \ reinsert reclaims the tombstone
   MT-MAP MT-MAP-DELETED@ 0 MT=
   MT-MAP MT-MAP-COUNT@ 2 MT=
   MT-MAP ixa MT-MAP-SLOT-STATE@ MAP-OCCUPIED? MT-ASSERT
   MT-MAP MT-CAP MT-KEY-A 1 33 MT-ASSERT-HIT ;

\ Direct MAP-LOCATE-SLOT matrix: all four slot-state arms of the map-loc
\ verdict, with the free memo asserted below the verdict.
: MT-TEST-LOCATE-SLOT ( -- )
   MT-MAP MT-CAP MT-MAP-INIT
   MT-KEY-A 1 MT-MAP-HASH {: ha:n :}
   ha 0 MT-CAP MT-MAP-PROBE {: home:n :}
   -1 MT-MAP home MT-KEY-A 1 ha MT-MAP-LOCATE-SLOT
      1 home MT-ASSERT-LOC home MT=                     \ empty -> free home, memo home
   11 MT-MAP MT-CAP MT-KEY-A 1 MT-MAP-SET
   -1 MT-MAP home MT-KEY-A 1 ha MT-MAP-LOCATE-SLOT
      2 home MT-ASSERT-LOC -1 MT=                       \ key match -> found home, memo kept
   -1 MT-MAP home MT-KEY-I 1 MT-KEY-I 1 MT-MAP-HASH MT-MAP-LOCATE-SLOT
      0 -1 MT-ASSERT-LOC -1 MT=                         \ occupied non-match -> full, memo kept
   SLOT--STATE:DELETED MT-MAP home MT-MAP-SLOT-STATE!
   0 MT-MAP MT-MAP-COUNT!
   1 MT-MAP MT-MAP-DELETED!
   -1 MT-MAP home MT-KEY-A 1 ha MT-MAP-LOCATE-SLOT
      0 -1 MT-ASSERT-LOC home MT= ;                     \ deleted -> full, memo remembers slot

\ MAP-LOCATE end-to-end: every verdict with its carried index payload.
: MT-TEST-LOCATE ( -- )
   MT-MAP MT-CAP MT-MAP-INIT
   MT-KEY-A 1 MT-MAP-HASH 0 MT-CAP MT-MAP-PROBE {: home:n :}
   MT-MAP MT-CAP MT-KEY-A 1 MT-MAP-LOCATE drop
      1 home MT-ASSERT-LOC                              \ empty table -> free at home
   11 MT-MAP MT-CAP MT-KEY-A 1 MT-MAP-SET
   MT-MAP MT-CAP MT-KEY-A 1 MT-MAP-LOCATE drop
      2 home MT-ASSERT-LOC                              \ present -> found at home
   MT-MAP MT-CAP MT-KEY-I 1 MT-MAP-LOCATE drop
      1 MT-KEY-I 1 MT-MAP-HASH 1 MT-CAP MT-MAP-PROBE MT-ASSERT-LOC   \ collision -> free one past home
   22 MT-MAP MT-CAP MT-KEY-I 1 MT-MAP-SET
   SLOT--STATE:DELETED MT-MAP home MT-MAP-SLOT-STATE!
   1 MT-MAP MT-MAP-COUNT!
   1 MT-MAP MT-MAP-DELETED!
   MT-MAP MT-CAP MT-KEY-A 1 MT-MAP-LOCATE drop
      1 home MT-ASSERT-LOC                              \ tombstoned key -> free reclaims home
   MT-FULL-MAP MT-FULL-CAP MT-MAP-INIT
   11 MT-FULL-MAP MT-FULL-CAP MT-KEY-A 1 MT-MAP-SET
   22 MT-FULL-MAP MT-FULL-CAP MT-KEY-B 1 MT-MAP-SET
   33 MT-FULL-MAP MT-FULL-CAP MT-KEY-C 1 MT-MAP-SET
   MT-FULL-MAP MT-FULL-CAP MT-KEY-D 1 MT-MAP-LOCATE drop
      0 -1 MT-ASSERT-LOC ;                              \ full table -> full, no index

\ Negative checked regressions: the verdict is not three loose ints, does not
\ compare with `=`, and a raw n cannot pose as one. Positive baseline first.
: MT-TEST-LOC-TYPES ( -- )
   s" MTLP ( ptr a count ptr u8 len -- map-loc n ) MAP-LOCATE" CHECK-QUIET-CANDIDATE! -1 MT=
   s" MTLN1 ( ptr a count ptr u8 len -- n n n ) MAP-LOCATE" CHECK-QUIET-CANDIDATE! 0 MT=
   s" MTLN2 ( map-loc map-loc -- bool ) =" CHECK-QUIET-CANDIDATE! 0 MT=
   s" MTLN3 ( n -- map-loc )" CHECK-QUIET-CANDIDATE! 0 MT= ;

: MT-TEST-MAP-EACH-EMPTY ( -- )
   MT-MAP MT-CAP MT-MAP-INIT
   MT-EACH-RESET
   MT-MAP MT-CAP [: MT-EACH-RECORD ;] MT-MAP-EACH
   MT-EACH-COUNT @ 0 MT=
   MT-EACH-SUM @ 0 MT=
   MT-EACH-LEN-SUM @ 0 MT= ;

: MT-TEST-MAP-EACH-INSERTS ( -- )
   MT-MAP MT-CAP MT-MAP-INIT
   11 MT-MAP MT-CAP MT-KEY-A 1 MT-MAP-SET
   22 MT-MAP MT-CAP MT-KEY-B 1 MT-MAP-SET
   MT-EACH-RESET
   MT-MAP MT-CAP [: MT-EACH-RECORD ;] MT-MAP-EACH
   MT-EACH-COUNT @ 2 MT=
   MT-EACH-SUM @ 33 MT=
   MT-EACH-LEN-SUM @ 2 MT=
   0 [char] a 1 11 MT-ASSERT-EACH-ROW
   1 [char] b 1 22 MT-ASSERT-EACH-ROW ;

: MT-TEST-MAP-EACH-UPDATES ( -- )
   MT-MAP MT-CAP MT-MAP-INIT
   11 MT-MAP MT-CAP MT-KEY-A 1 MT-MAP-SET
   22 MT-MAP MT-CAP MT-KEY-B 1 MT-MAP-SET
   33 MT-MAP MT-CAP MT-KEY-A 1 MT-MAP-SET
   MT-EACH-RESET
   MT-MAP MT-CAP [: MT-EACH-RECORD ;] MT-MAP-EACH
   MT-EACH-COUNT @ 2 MT=
   MT-EACH-SUM @ 55 MT=
   0 [char] a 1 33 MT-ASSERT-EACH-ROW
   1 [char] b 1 22 MT-ASSERT-EACH-ROW ;

: MT-TEST-MAP-EACH-COLLISIONS ( -- )
   MT-MAP MT-CAP MT-MAP-INIT
   11 MT-MAP MT-CAP MT-KEY-A 1 MT-MAP-SET
   22 MT-MAP MT-CAP MT-KEY-I 1 MT-MAP-SET
   33 MT-MAP MT-CAP MT-KEY-Q 1 MT-MAP-SET
   MT-EACH-RESET
   MT-MAP MT-CAP [: MT-EACH-RECORD ;] MT-MAP-EACH
   MT-EACH-COUNT @ 3 MT=
   MT-EACH-SUM @ 66 MT=
   0 [char] q 1 33 MT-ASSERT-EACH-ROW
   1 [char] a 1 11 MT-ASSERT-EACH-ROW
   2 [char] i 1 22 MT-ASSERT-EACH-ROW ;

: MT-REPORT ( -- )
   MT-FAIL @ 0= if s" map-test: ok" type cr exit then
   MT-FAIL @ . s" map-test: failures" type cr
   s" map-test: failures" MT-EX-FAIL die ;

: MT-MAIN ( -- )
   MT-TEST-CONSTANTS
   MT-TEST-STATES
   MT-TEST-STATE-TYPES
   MT-TEST-CHECKS
   MT-TEST-LAYOUT
   MT-TEST-SLOT-FIELDS
   MT-TEST-INIT-CLEAR
   MT-TEST-HASH-PROBE
   MT-TEST-MAP-SET-GET
   MT-TEST-MAP-COLLISIONS
   MT-TEST-MAP-FULL
   MT-TEST-TOMBSTONE
   MT-TEST-LOCATE-SLOT
   MT-TEST-LOCATE
   MT-TEST-LOC-TYPES
   MT-TEST-MAP-EACH-EMPTY
   MT-TEST-MAP-EACH-INSERTS
   MT-TEST-MAP-EACH-UPDATES
   MT-TEST-MAP-EACH-COLLISIONS
   MT-REPORT ;

MT-MAIN
