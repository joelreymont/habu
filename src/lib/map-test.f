\ map-test.f - map operations, ownership, capacity and corrupt-state refusals.
\ Run: bin/hb --load src/lib/map-test.f

require src/lib/errors.f
require src/lib/string.f
require src/lib/map.f
require src/lib/fmt.f                   \ FMT:.INT - report numbers inline

\ White-box test: reopen the module's package so the fixtures reach its private
\ layout, probe and locate words by their bare package-local names, and so the
\ checker candidates below resolve them while the package is open.
package MAP

1 constant MT-EX-FAIL
8 constant MT-CAP
3 constant MT-FULL-CAP
32 constant MT-SPREAD-CAP                \ 26 letters with room to spread

variable MT-CASE
variable MT-FAIL
variable MT-EACH-COUNT
variable MT-EACH-SUM
variable MT-EACH-LEN-SUM
variable MT-EACH-LAST                   \ slot of the entry EACH visited last
variable MT-COL-N

create MT-MAP MT-CAP >COUNT CELL-COUNT COUNT>N cells allot
create MT-FULL-MAP MT-FULL-CAP >COUNT CELL-COUNT COUNT>N cells allot
create MT-SPREAD-A MT-SPREAD-CAP >COUNT CELL-COUNT COUNT>N cells allot
create MT-SPREAD-B MT-SPREAD-CAP >COUNT CELL-COUNT COUNT>N cells allot
create MT-COLS 3 cells allot            \ letter indices sharing one home slot
create MT-LETTERS 26 allot              \ the single-byte keys a..z
create MT-ORDER 52 allot                \ the letters EACH visits: map A, then map B
create MT-KEY 97 c, 98 c, 99 c,
create MT-KEY-A 97 c,
create MT-KEY-B 98 c,
create MT-KEY-C 99 c,
create MT-KEY-D 100 c,
create MT-KEY-Z 122 c,

: MT-ASSERT ( bool -- )
   MT-CASE @ 1 + MT-CASE !
   0= if
      s" map-test: assertion " type MT-CASE @ FMT:.INT s"  failed" type cr
      MT-FAIL @ 1 + MT-FAIL !
   then ;

: MT= ( n n -- )
   = MT-ASSERT ;

: MT-RANGE ( n n -- ) {: ix cap :}
   ix 0 >= ix cap < and MT-ASSERT ;

: MT-MAP-CHECK-CAP ( n -- )
   >COUNT CHECK-CAP ;

: MT-MAP-CELLS ( n -- n )
   >COUNT CELL-COUNT COUNT>N ;

: MT-MAP-CAP@ ( ptr a -- n )
   CAP@ COUNT>N ;

: MT-MAP-COUNT@ ( ptr a -- n )
   COUNT@ COUNT>N ;

: MT-MAP-COUNT! ( n ptr a -- ) {: count m:ptr :}
   count >COUNT m COUNT! ;

: MT-MAP-CHECK-INDEX ( ptr a n -- ) {: m:ptr ix :}
   m ix >IDX CHECK-INDEX ;

: MT-MAP-SLOT-FIELD ( ptr a n n -- ptr a ) {: m:ptr ix off :}
   m ix >IDX off >OFF SLOT-FIELD ;

: MT-MAP-SLOT-STATE@ ( ptr a n -- slot-state ) {: m:ptr ix:n :}
   m ix >IDX SLOT-STATE@ ;

: MT-MAP-SLOT-STATE! ( slot-state ptr a n -- ) {: m:ptr ix:n :}   \ enum stays on stack (no enum locals)
   m ix >IDX SLOT-STATE! ;

: MT-MAP-SLOT-HASH@ ( ptr a n -- n ) {: m:ptr ix :}
   m ix >IDX SLOT-HASH@ ;

: MT-MAP-SLOT-HASH! ( n ptr a n -- ) {: hash m:ptr ix :}
   hash m ix >IDX SLOT-HASH! ;

: MT-MAP-SLOT-KEY-A@ ( ptr a n -- ptr u8 ) {: m:ptr ix :}
   m ix >IDX SLOT-KEY-A@ ;

: MT-MAP-SLOT-KEY-U@ ( ptr a n -- n ) {: m:ptr ix :}
   m ix >IDX SLOT-KEY-U@ LEN>N ;

: MT-MAP-SLOT-KEY-U! ( n ptr a n -- ) {: len m:ptr ix :}
   len >LEN m ix >IDX SLOT-KEY-U! ;

: MT-MAP-SLOT-VALUE@ ( ptr a n -- a ) {: m:ptr ix :}
   m ix >IDX SLOT-VALUE@ ;

: MT-MAP-SLOT-VALUE! ( a ptr a n -- ) {: value m:ptr ix :}
   value m ix >IDX SLOT-VALUE! ;

: MT-MAP-INIT ( ptr a n -- ) {: m:ptr cap :}
   m cap >COUNT INIT ;

: MT-MAP-HASH ( ptr a ptr u8 n -- n ) {: m:ptr a:ptr u :}
   m a u >LEN HASH ;

: MT-MAP-INDEX ( n n -- n ) {: hash cap :}
   hash cap >COUNT INDEX IDX>N ;

: MT-MAP-PROBE ( n n n -- n ) {: hash step cap :}
   hash step >COUNT cap >COUNT PROBE IDX>N ;

: MT-MAP-LOCATE-SLOT ( ptr a n ptr u8 n n -- loc ) {: m:ptr ix:n key:ptr len:n hash:n :}
   m ix >IDX key len >LEN hash LOCATE-SLOT ;

: MT-MAP-LOCATE ( ptr a n ptr u8 n -- loc n ) {: m:ptr cap:n key:ptr len:n :}
   m cap >COUNT key len >LEN LOCATE ;

: MT-LOC-KIND ( loc -- n )                           \ 0 full, 1 free, 2 found
   MATCH loc
     full OF 0 ENDOF
     free OF drop 1 ENDOF
     found OF drop 2 ENDOF
   ;MATCH ;

: MT-LOC-IDX ( loc -- n )                            \ carried slot index, -1 for full
   MATCH loc
     full OF -1 ENDOF
     free OF IDX>N ENDOF
     found OF IDX>N ENDOF
   ;MATCH ;

: MT-ASSERT-LOC ( loc n n -- ) {: kind:n ix:n :}     \ verdict kind + carried idx
   dup MT-LOC-KIND kind MT=
   MT-LOC-IDX ix MT= ;

: MT-MAP-GET ( ptr n n ptr u8 n -- option<n> ) {: m:ptr cap:n key:ptr len:n :}
   m cap >COUNT key len >LEN GET ;

: MT-MAP-HAS? ( ptr n n ptr u8 n -- bool ) {: m:ptr cap key:ptr len :}
   m cap >COUNT key len >LEN HAS? ;

: MT-MAP-SET ( n ptr n n ptr u8 n -- ) {: value m:ptr cap key:ptr len :}
   value m cap >COUNT key len >LEN SET ;

: MT-MAP-EACH ( ptr n n [ ptr u8 len n -- ] -- ) {: m:ptr cap q :}
   m cap >COUNT q EACH ;

: MT-FILL-STORAGE ( -- )
   MT-CAP MT-MAP-CELLS 0 ?do
      777 MT-MAP i cells + !
   loop ;

: MT-EACH-RESET ( -- )
   0 MT-EACH-COUNT !
   0 MT-EACH-SUM !
   0 MT-EACH-LEN-SUM !
   -1 MT-EACH-LAST ! ;

\ EACH runs over MT-MAP: each visit is an entry the map finds with that value,
\ in a slot past the one visited before it.
: MT-EACH-RECORD ( ptr u8 len n -- ) {: key:ptr len value :}
   MT-MAP MT-CAP key len LEN>N MT-MAP-LOCATE drop
   dup MT-LOC-KIND 2 MT=
   MT-LOC-IDX {: slot :}
   slot MT-EACH-LAST @ > MT-ASSERT
   slot MT-EACH-LAST !
   MT-MAP slot MT-MAP-SLOT-VALUE@ value MT=
   MT-EACH-COUNT @ 1 + MT-EACH-COUNT !
   MT-EACH-SUM @ value + MT-EACH-SUM !
   MT-EACH-LEN-SUM @ len LEN>N + MT-EACH-LEN-SUM ! ;

: MT-ASSERT-CLEAR-SLOT ( n -- ) {: ix :}
   MT-MAP ix MT-MAP-SLOT-STATE@ OCCUPIED? 0= MT-ASSERT
   MT-MAP ix MT-MAP-SLOT-HASH@ 0 MT=
   MT-MAP ix MT-MAP-SLOT-KEY-A@ NULL$ drop = MT-ASSERT
   MT-MAP ix MT-MAP-SLOT-KEY-U@ 0 MT=
   MT-MAP ix MT-MAP-SLOT-VALUE@ 0 MT= ;

: MT-BAD-CAP-ZERO ( -- )
   0 MT-MAP-CHECK-CAP ;

: MT-BAD-CAP-NEG ( -- )
   -1 MT-MAP-CHECK-CAP ;

: MT-BAD-CAP-HIGH ( -- )
   MAX-CAP 1 + MT-MAP-CHECK-CAP ;

: MT-GOOD-CAP ( -- )
   1 MT-MAP-CHECK-CAP ;

: MT-BAD-COUNT-NEG ( -- )
   MT-MAP MT-CAP MT-MAP-INIT
   -1 MT-MAP MT-MAP-COUNT! ;

: MT-BAD-COUNT-HIGH ( -- )
   MT-MAP MT-CAP MT-MAP-INIT
   MT-CAP 1 + MT-MAP MT-MAP-COUNT! ;

: MT-BAD-INDEX-NEG ( -- )
   MT-MAP -1 MT-MAP-CHECK-INDEX ;

: MT-BAD-INDEX-HIGH ( -- )
   MT-MAP MT-CAP MT-MAP-CHECK-INDEX ;

: MT-BAD-FIELD-HIGH ( -- )
   MT-MAP 0 SLOT-CELLS MT-MAP-SLOT-FIELD drop ;

: MT-BAD-HASH ( -- )
   -1 MT-MAP 0 MT-MAP-SLOT-HASH! ;

: MT-BAD-KEY-U ( -- )
   -1 MT-MAP 0 MT-MAP-SLOT-KEY-U! ;

: MT-BAD-STATE-TAG ( -- )
   MT-MAP MT-CAP MT-MAP-INIT
   2 MT-MAP 0 SLOT-STATE-OFF MT-MAP-SLOT-FIELD !
   MT-MAP 0 MT-MAP-SLOT-STATE@ drop ;

\ Negative checked cases: a raw n cannot pose as a slot state, a slot state cannot
\ launder back to n, and enums do not compare with `=`. The positive baseline
\ proves the rejects are type errors, not noise.
: MT-TEST-STATE-TYPES ( -- )
   s" MTP1 ( slot-state ptr a idx -- ) SLOT-STATE!" CHECK-QUIET-CANDIDATE! -1 MT=
   s" MTN1 ( n ptr a idx -- ) SLOT-STATE!" CHECK-QUIET-CANDIDATE! 0 MT=
   s" MTN2 ( ptr a idx -- n ) SLOT-STATE@" CHECK-QUIET-CANDIDATE! 0 MT=
   s" MTN3 ( slot-state slot-state -- bool ) =" CHECK-QUIET-CANDIDATE! 0 MT=
   s" MTN4 ( ptr a idx -- ptr slot-state ) SLOT-STATE-OFF >OFF SLOT-FIELD" CHECK-QUIET-CANDIDATE! 0 MT= ;

: MT-TEST-CHECKS ( -- )
   [: MT-GOOD-CAP ;] catch 0 MT=
   [: MT-BAD-CAP-ZERO ;] catch E-MAP-BAD-CAP MT=
   [: MT-BAD-CAP-NEG ;] catch E-MAP-BAD-CAP MT=
   [: MT-BAD-CAP-HIGH ;] catch E-MAP-BAD-CAP MT=
   MT-MAP MT-CAP MT-MAP-INIT
   MT-CAP MT-MAP MT-MAP-COUNT!  MT-MAP MT-MAP-COUNT@ MT-CAP MT=
   0 MT-MAP MT-MAP-COUNT!
   [: MT-BAD-COUNT-NEG ;] catch E-MAP-BAD-CAP MT=
   [: MT-BAD-COUNT-HIGH ;] catch E-MAP-FULL MT=
   MT-MAP 0 MT-MAP-CHECK-INDEX
   MT-MAP MT-CAP 1 - MT-MAP-CHECK-INDEX
   [: MT-BAD-INDEX-NEG ;] catch E-MAP-BAD-CAP MT=
   [: MT-BAD-INDEX-HIGH ;] catch E-MAP-BAD-CAP MT=
   [: MT-BAD-FIELD-HIGH ;] catch E-MAP-BAD-CAP MT=
   [: MT-BAD-HASH ;] catch E-MAP-BAD-CAP MT=
   [: MT-BAD-KEY-U ;] catch E-MAP-BAD-CAP MT=
   [: MT-BAD-STATE-TAG ;] catch ENGINE-ERROR:BAD-TAG MT= ;

: MT-TEST-INIT-CLEAR ( -- )
   MT-FILL-STORAGE
   MT-MAP MT-CAP MT-MAP-INIT
   MT-MAP MT-MAP-CAP@ MT-CAP MT=
   MT-MAP MT-MAP-COUNT@ 0 MT=
   MT-CAP 0 ?do i MT-ASSERT-CLEAR-SLOT loop
   MAP-SLOT--STATE:OCCUPIED MT-MAP 3 MT-MAP-SLOT-STATE!
   88 MT-MAP 3 MT-MAP-SLOT-VALUE!
   4 MT-MAP MT-MAP-COUNT!
   MT-MAP CLEAR
   MT-MAP MT-MAP-COUNT@ 0 MT=
   MT-CAP 0 ?do i MT-ASSERT-CLEAR-SLOT loop ;

: MT-PROBE-RANGE ( ptr u8 n n n -- ) {: a:ptr u step cap :}
   MT-MAP a u MT-MAP-HASH step cap MT-MAP-PROBE cap MT-RANGE ;

: MT-FILL-LETTERS ( -- )
   26 0 ?do 97 i + MT-LETTERS i + c! loop ;

: MT-LETTER ( n -- ptr u8 )
   MT-LETTERS + ;

: MT-HOME ( n -- n )
   MT-MAP swap MT-LETTER 1 MT-MAP-HASH MT-CAP MT-MAP-INDEX ;

\ Collision key k: one of three letters sharing a home slot under MT-MAP's key.
: MT-COL ( n -- ptr u8 )
   cells MT-COLS + @ MT-LETTER ;

\ The key is secret, so the colliding keys are found, not chosen: 26 letters in
\ MT-CAP slots put at least three in one slot.
: MT-FIND-COLLISIONS ( -- )
   MT-FILL-LETTERS
   MT-CAP 0 ?do
      0 MT-COL-N !
      26 0 ?do
         i MT-HOME j =  MT-COL-N @ 3 <  and if
            i MT-COLS MT-COL-N @ cells + !
            MT-COL-N @ 1 + MT-COL-N !
         then
      loop
      MT-COL-N @ 3 = if unloop exit then
   loop
   0 0= 0= MT-ASSERT ;

: MT-ASSERT-HIT ( ptr n n ptr u8 n n -- ) {: m:ptr cap:n key:ptr len:n want:n :}
   m cap key len MT-MAP-HAS? MT-ASSERT
   m cap key len MT-MAP-GET MATCH option
     none OF 0 0= 0= MT-ASSERT ENDOF
     some OF want MT= ENDOF
   ;MATCH ;

: MT-ASSERT-MISS ( ptr n n ptr u8 n -- ) {: m:ptr cap:n key:ptr len:n :}
   m cap key len MT-MAP-HAS? 0= MT-ASSERT
   m cap key len MT-MAP-GET MATCH option
     none OF ENDOF
     some OF drop 0 0= 0= MT-ASSERT ENDOF
   ;MATCH ;

: MT-ASSERT-COLLISION-SLOT ( n ptr u8 -- ) {: step want:ptr :}
   MT-MAP MT-MAP 0 MT-COL 1 MT-MAP-HASH step MT-CAP MT-MAP-PROBE MT-MAP-SLOT-KEY-A@
      want = MT-ASSERT ;

: MT-FULL-INSERT ( -- )
   44 MT-FULL-MAP MT-FULL-CAP MT-KEY-D 1 MT-MAP-SET ;

: MT-TEST-HASH-PROBE ( -- )
   MT-MAP MT-CAP MT-MAP-INIT
   MT-MAP s" alpha" MT-MAP-HASH 0 >= MT-ASSERT
   MT-MAP s" beta" MT-MAP-HASH MT-CAP MT-MAP-INDEX MT-CAP MT-RANGE
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
   MT-FIND-COLLISIONS
   11 MT-MAP MT-CAP 0 MT-COL 1 MT-MAP-SET
   22 MT-MAP MT-CAP 1 MT-COL 1 MT-MAP-SET
   33 MT-MAP MT-CAP 2 MT-COL 1 MT-MAP-SET
   MT-MAP MT-MAP-COUNT@ 3 MT=
   0 0 MT-COL MT-ASSERT-COLLISION-SLOT
   1 1 MT-COL MT-ASSERT-COLLISION-SLOT
   2 2 MT-COL MT-ASSERT-COLLISION-SLOT
   MT-MAP MT-CAP 0 MT-COL 1 11 MT-ASSERT-HIT
   MT-MAP MT-CAP 1 MT-COL 1 22 MT-ASSERT-HIT
   MT-MAP MT-CAP 2 MT-COL 1 33 MT-ASSERT-HIT
   44 MT-MAP MT-CAP 1 MT-COL 1 MT-MAP-SET
   MT-MAP MT-MAP-COUNT@ 3 MT=
   MT-MAP MT-CAP 0 MT-COL 1 11 MT-ASSERT-HIT
   MT-MAP MT-CAP 1 MT-COL 1 44 MT-ASSERT-HIT
   MT-MAP MT-CAP 2 MT-COL 1 33 MT-ASSERT-HIT ;

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

: MT-TEST-LOCATE-SLOT ( -- )
   MT-MAP MT-CAP MT-MAP-INIT
   MT-MAP MT-KEY-A 1 MT-MAP-HASH {: ha:n :}
   ha 0 MT-CAP MT-MAP-PROBE {: home:n :}
   MT-MAP home MT-KEY-A 1 ha MT-MAP-LOCATE-SLOT
      1 home MT-ASSERT-LOC                              \ empty -> free home
   11 MT-MAP MT-CAP MT-KEY-A 1 MT-MAP-SET
   MT-MAP home MT-KEY-A 1 ha MT-MAP-LOCATE-SLOT
      2 home MT-ASSERT-LOC                              \ key match -> found home
   MT-MAP home MT-KEY-B 1 MT-MAP MT-KEY-B 1 MT-MAP-HASH MT-MAP-LOCATE-SLOT
      0 -1 MT-ASSERT-LOC ;                              \ occupied non-match -> full

: MT-TEST-LOCATE ( -- )
   MT-MAP MT-CAP MT-MAP-INIT
   MT-FIND-COLLISIONS
   MT-MAP 0 MT-COL 1 MT-MAP-HASH 0 MT-CAP MT-MAP-PROBE {: home:n :}
   MT-MAP MT-CAP 0 MT-COL 1 MT-MAP-LOCATE drop
      1 home MT-ASSERT-LOC                              \ empty table -> free at home
   11 MT-MAP MT-CAP 0 MT-COL 1 MT-MAP-SET
   MT-MAP MT-CAP 0 MT-COL 1 MT-MAP-LOCATE drop
      2 home MT-ASSERT-LOC                              \ present -> found at home
   MT-MAP MT-CAP 1 MT-COL 1 MT-MAP-LOCATE drop
      1 MT-MAP 1 MT-COL 1 MT-MAP-HASH 1 MT-CAP MT-MAP-PROBE MT-ASSERT-LOC   \ collision -> free one past home
   MT-FULL-MAP MT-FULL-CAP MT-MAP-INIT
   11 MT-FULL-MAP MT-FULL-CAP MT-KEY-A 1 MT-MAP-SET
   22 MT-FULL-MAP MT-FULL-CAP MT-KEY-B 1 MT-MAP-SET
   33 MT-FULL-MAP MT-FULL-CAP MT-KEY-C 1 MT-MAP-SET
   MT-FULL-MAP MT-FULL-CAP MT-KEY-D 1 MT-MAP-LOCATE drop
      0 -1 MT-ASSERT-LOC ;                              \ full table -> full, no index

\ Negative checked regressions: the verdict is not three loose ints, does not
\ compare with `=`, and a raw n cannot pose as one. Positive baseline first.
: MT-TEST-LOC-TYPES ( -- )
   s" MTLP ( ptr a count ptr u8 len -- loc n ) LOCATE" CHECK-QUIET-CANDIDATE! -1 MT=
   s" MTLN1 ( ptr a count ptr u8 len -- n n n ) LOCATE" CHECK-QUIET-CANDIDATE! 0 MT=
   s" MTLN2 ( loc loc -- bool ) =" CHECK-QUIET-CANDIDATE! 0 MT=
   s" MTLN3 ( n -- loc )" CHECK-QUIET-CANDIDATE! 0 MT= ;

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
   MT-EACH-LEN-SUM @ 2 MT= ;

: MT-TEST-MAP-EACH-UPDATES ( -- )
   MT-MAP MT-CAP MT-MAP-INIT
   11 MT-MAP MT-CAP MT-KEY-A 1 MT-MAP-SET
   22 MT-MAP MT-CAP MT-KEY-B 1 MT-MAP-SET
   33 MT-MAP MT-CAP MT-KEY-A 1 MT-MAP-SET
   MT-EACH-RESET
   MT-MAP MT-CAP [: MT-EACH-RECORD ;] MT-MAP-EACH
   MT-EACH-COUNT @ 2 MT=
   MT-EACH-SUM @ 55 MT=
   MT-MAP MT-CAP MT-KEY-A 1 33 MT-ASSERT-HIT ;

: MT-TEST-MAP-EACH-COLLISIONS ( -- )
   MT-MAP MT-CAP MT-MAP-INIT
   MT-FIND-COLLISIONS
   11 MT-MAP MT-CAP 0 MT-COL 1 MT-MAP-SET
   22 MT-MAP MT-CAP 1 MT-COL 1 MT-MAP-SET
   33 MT-MAP MT-CAP 2 MT-COL 1 MT-MAP-SET
   MT-EACH-RESET
   MT-MAP MT-CAP [: MT-EACH-RECORD ;] MT-MAP-EACH
   MT-EACH-COUNT @ 3 MT=
   MT-EACH-SUM @ 66 MT= ;

: MT-ORDER-RECORD ( ptr u8 len n -- )
   drop drop c@ MT-ORDER MT-EACH-COUNT @ + c!
   MT-EACH-COUNT @ 1 + MT-EACH-COUNT ! ;

: MT-SPREAD-FILL ( ptr n -- ) {: m:ptr :}
   m MT-SPREAD-CAP MT-MAP-INIT
   26 0 ?do i m MT-SPREAD-CAP i MT-LETTER 1 MT-MAP-SET loop ;

\ Each map hashes under its own secret key, so two maps given the same keys in the
\ same order place them apart; one fixed hash would make EACH visit both in one
\ order. With 26 keys an equal order by chance is negligible.
: MT-TEST-MAP-KEYED ( -- )
   MT-FILL-LETTERS
   MT-SPREAD-A MT-SPREAD-FILL
   MT-SPREAD-B MT-SPREAD-FILL
   0 MT-EACH-COUNT !
   MT-SPREAD-A MT-SPREAD-CAP [: MT-ORDER-RECORD ;] MT-MAP-EACH
   MT-SPREAD-B MT-SPREAD-CAP [: MT-ORDER-RECORD ;] MT-MAP-EACH
   MT-EACH-COUNT @ 52 MT=
   0 0= 0=
   26 0 ?do MT-ORDER i + c@ MT-ORDER 26 + i + c@ <> or loop
   MT-ASSERT ;

: MT-REPORT ( -- )
   MT-FAIL @ 0= if s" map-test: ok" type cr exit then
   s" map-test: " type MT-FAIL @ FMT:.INT s"  failure(s)" type cr
   s" map-test: failures" MT-EX-FAIL die ;

: MT-MAIN ( -- )
   MT-TEST-STATE-TYPES
   MT-TEST-CHECKS
   MT-TEST-INIT-CLEAR
   MT-TEST-HASH-PROBE
   MT-TEST-MAP-SET-GET
   MT-TEST-MAP-COLLISIONS
   MT-TEST-MAP-FULL
   MT-TEST-LOCATE-SLOT
   MT-TEST-LOCATE
   MT-TEST-LOC-TYPES
   MT-TEST-MAP-EACH-EMPTY
   MT-TEST-MAP-EACH-INSERTS
   MT-TEST-MAP-EACH-UPDATES
   MT-TEST-MAP-EACH-COLLISIONS
   MT-TEST-MAP-KEYED
   MT-REPORT ;

MT-MAIN

;package
