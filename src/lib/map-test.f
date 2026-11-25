\ map-test.f - map operations, removal, ownership and capacity.
\ Run: bin/hb --load src/lib/map-test.f

require src/lib/error.f
require src/lib/string.f
require src/lib/map.f
require src/lib/fmt.f                   \ FMT:.INT - report numbers inline
require src/lib/siphash.f
require src/lib/property.f              \ PROP - the seeded mix of SET and REMOVE
require src/core/exit-status.f
require src/lib/map-core.f

\ White-box test: reopen the module's package so the fixtures reach its private
\ layout, probe and locate words by their bare package-local names.
package MAP

8 constant MT-CAP
3 constant MT-FULL-CAP
32 constant MT-SPREAD-CAP                \ 26 letters with room to spread
13 constant MT-MIX-CAP                   \ half the 26 letters: the mix runs full
26 constant MT-MIX-KEYS
10000 constant MT-MIX-OPS

variable MT-CASE
variable MT-FAIL
variable MT-EACH-COUNT
variable MT-EACH-SUM
variable MT-EACH-LEN-SUM
variable MT-EACH-LAST                   \ slot of the entry EACH visited last
variable MT-COL-N
TYPED-VARIABLE MT-ROOM-MAP ^n

create MT-MAP MT-CAP >COUNT CELL-COUNT COUNT>N cells allot
create MT-FULL-MAP MT-FULL-CAP >COUNT CELL-COUNT COUNT>N cells allot
create MT-SPREAD-A MT-SPREAD-CAP >COUNT CELL-COUNT COUNT>N cells allot
create MT-SPREAD-B MT-SPREAD-CAP >COUNT CELL-COUNT COUNT>N cells allot
create MT-MIX-MAP MT-MIX-CAP >COUNT CELL-COUNT COUNT>N cells allot
create MT-MIX-LIST MT-MIX-KEYS cells allot  \ each letter's value, -1 when absent
create MT-COLS 3 cells allot            \ letter indices sharing one home slot
create MT-LETTERS 26 allot              \ the single-byte keys a..z
create MT-ORDER 52 allot                \ the letters EACH visits: map A, then map B
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

: MT-MAP-CELLS ( n -- n )
   >COUNT CELL-COUNT COUNT>N ;

: MT-MAP-CAP@ ( ^a -- n )
   CAP@ COUNT>N ;

: MT-MAP-COUNT@ ( ^a -- n )
   COUNT@ COUNT>N ;

: MT-MAP-COUNT! ( n ^a -- ) {: count m:^ :}
   count >COUNT m COUNT! ;

: MT-MAP-SLOT-STATE@ ( ^a n -- slot-state ) {: m:^ ix:n :}
   m ix >IDX SLOT-STATE@ ;

: MT-MAP-SLOT-STATE! ( slot-state ^a n -- ) {: m:^ ix:n :}   \ enum stays on stack (no enum locals)
   m ix >IDX SLOT-STATE! ;

: MT-MAP-SLOT-HASH@ ( ^a n -- n ) {: m:^ ix :}
   m ix >IDX SLOT-HASH@ ;

: MT-MAP-SLOT-KEY-A@ ( ^a n -- ^u8 ) {: m:^ ix :}
   m ix >IDX SLOT-KEY-A@ ;

: MT-MAP-SLOT-KEY-U@ ( ^a n -- n ) {: m:^ ix :}
   m ix >IDX SLOT-KEY-U@ ;

: MT-MAP-SLOT-VALUE@ ( ^a n -- a ) {: m:^ ix :}
   m ix >IDX SLOT-VALUE@ ;

: MT-MAP-SLOT-VALUE! ( a ^a n -- ) {: value m:^ ix :}
   value m ix >IDX SLOT-VALUE! ;

: MT-MAP-INIT ( ^a n -- ) {: m:^ cap :}
   m cap >COUNT INIT ;

: MT-MAP-HASH ( ^a str -- n ) {: m:^ s:str :}
   m s HASH ;

: MT-MAP-INDEX ( n n -- n ) {: hash cap :}
   hash cap >COUNT INDEX IDX>N ;

: MT-MAP-PROBE ( n n n -- n ) {: hash step cap :}
   hash step >COUNT cap >COUNT PROBE IDX>N ;

: MT-MAP-LOCATE ( ^a n str -- loc n ) {: m:^ cap:n key:str :}
   m cap >COUNT key LOCATE ;

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
: MT-EACH-RECORD ( str n -- ) {: key:str value :}
   MT-MAP MT-CAP key MT-MAP-LOCATE drop
   dup MT-LOC-KIND 2 MT=
   MT-LOC-IDX {: slot :}
   slot MT-EACH-LAST @ > MT-ASSERT
   slot MT-EACH-LAST !
   MT-MAP slot MT-MAP-SLOT-VALUE@ value MT=
   MT-EACH-COUNT @ 1 + MT-EACH-COUNT !
   MT-EACH-SUM @ value + MT-EACH-SUM !
   MT-EACH-LEN-SUM @ key STR:LEN + MT-EACH-LEN-SUM ! ;

: MT-ASSERT-CLEAR-SLOT ( n -- ) {: ix :}
   MT-MAP ix MT-MAP-SLOT-STATE@ OCCUPIED? 0= MT-ASSERT
   MT-MAP ix MT-MAP-SLOT-HASH@ 0 MT=
   MT-MAP ix MT-MAP-SLOT-KEY-A@ NULL$ STR:DATA = MT-ASSERT
   MT-MAP ix MT-MAP-SLOT-KEY-U@ 0 MT=
   MT-MAP ix MT-MAP-SLOT-VALUE@ 0 MT= ;

false [if] \ off until dot habu-add-the-checks-a49d054d
: MT-MAP-CHECK-CAP ( n -- )
   >COUNT CHECK-CAP ;

: MT-MAP-CHECK-INDEX ( ^a n -- ) {: m:^ ix :}
   m ix >IDX CHECK-INDEX ;

: MT-MAP-SLOT-FIELD ( ^a n n -- ^a ) {: m:^ ix off :}
   m ix >IDX off >OFF SLOT-FIELD ;

: MT-MAP-SLOT-HASH! ( n ^a n -- ) {: hash m:^ ix :}
   hash m ix >IDX SLOT-HASH! ;

: MT-MAP-SLOT-KEY-U! ( n ^a n -- ) {: len m:^ ix :}
   len m ix >IDX SLOT-KEY-U! ;

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
   [: MT-BAD-STATE-TAG ;] catch E-MAP-BAD-CAP MT= ;
[then]

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

: MT-FILL-LETTERS ( -- )
   26 0 ?do 97 i + MT-LETTERS i + c! loop ;

: MT-LETTER ( n -- ^u8 )
   MT-LETTERS + ;

: MT-HOME ( n -- n )
   MT-MAP swap MT-LETTER 1 >str MT-MAP-HASH MT-CAP MT-MAP-INDEX ;

\ Collision key k: one of three letters sharing a home slot under MT-MAP's key.
: MT-COL ( n -- ^u8 )
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

: MT-ASSERT-HIT ( ^n str n -- ) {: m:^ key:str want:n :}
   m key HAS? MT-ASSERT
   m key GET MATCH option
     none OF 0 0= 0= MT-ASSERT ENDOF
     some OF want MT= ENDOF
   ;MATCH ;

: MT-ASSERT-MISS ( ^n str -- ) {: m:^ key:str :}
   m key HAS? 0= MT-ASSERT
   m key GET MATCH option
     none OF ENDOF
     some OF drop 0 0= 0= MT-ASSERT ENDOF
   ;MATCH ;

: MT-ASSERT-COLLISION-SLOT ( n ^u8 -- ) {: step want:^ :}
   MT-MAP MT-MAP 0 MT-COL 1 >str MT-MAP-HASH step MT-CAP
   MT-MAP-PROBE MT-MAP-SLOT-KEY-A@
      want = MT-ASSERT ;

: MT-FULL-INSERT ( -- )
   44 MT-FULL-MAP MT-KEY-D 1 >str SET ;

: MT-TEST-MAP-SET-GET ( -- )
   MT-MAP MT-CAP MT-MAP-INIT
   MT-MAP MT-KEY-Z 1 >str MT-ASSERT-MISS
   11 MT-MAP MT-KEY-A 1 >str SET
   MT-MAP MT-MAP-COUNT@ 1 MT=
   MT-MAP MT-KEY-A 1 >str HAS? MT-ASSERT
   MT-MAP MT-KEY-A 1 >str 11 MT-ASSERT-HIT
   MT-MAP MT-KEY-Z 1 >str MT-ASSERT-MISS
   22 MT-MAP MT-KEY-A 1 >str SET
   MT-MAP MT-MAP-COUNT@ 1 MT=
   MT-MAP MT-KEY-A 1 >str 22 MT-ASSERT-HIT ;

: MT-TEST-MAP-COLLISIONS ( -- )
   MT-MAP MT-CAP MT-MAP-INIT
   MT-FIND-COLLISIONS
   11 MT-MAP 0 MT-COL 1 >str SET
   22 MT-MAP 1 MT-COL 1 >str SET
   33 MT-MAP 2 MT-COL 1 >str SET
   MT-MAP MT-MAP-COUNT@ 3 MT=
   0 0 MT-COL MT-ASSERT-COLLISION-SLOT
   1 1 MT-COL MT-ASSERT-COLLISION-SLOT
   2 2 MT-COL MT-ASSERT-COLLISION-SLOT
   MT-MAP 0 MT-COL 1 >str 11 MT-ASSERT-HIT
   MT-MAP 1 MT-COL 1 >str 22 MT-ASSERT-HIT
   MT-MAP 2 MT-COL 1 >str 33 MT-ASSERT-HIT
   44 MT-MAP 1 MT-COL 1 >str SET
   MT-MAP MT-MAP-COUNT@ 3 MT=
   MT-MAP 0 MT-COL 1 >str 11 MT-ASSERT-HIT
   MT-MAP 1 MT-COL 1 >str 44 MT-ASSERT-HIT
   MT-MAP 2 MT-COL 1 >str 33 MT-ASSERT-HIT ;

: MT-TEST-MAP-FULL ( -- )
   MT-FULL-MAP MT-FULL-CAP MT-MAP-INIT
   11 MT-FULL-MAP MT-KEY-A 1 >str SET
   22 MT-FULL-MAP MT-KEY-B 1 >str SET
   33 MT-FULL-MAP MT-KEY-C 1 >str SET
   MT-FULL-MAP MT-MAP-COUNT@ MT-FULL-CAP MT=
   [: MT-FULL-INSERT ;] catch E-MAP-FULL MT=
   MT-FULL-MAP MT-MAP-COUNT@ MT-FULL-CAP MT=
   44 MT-FULL-MAP MT-KEY-B 1 >str SET
   MT-FULL-MAP MT-MAP-COUNT@ MT-FULL-CAP MT=
   MT-FULL-MAP MT-KEY-B 1 >str 44 MT-ASSERT-HIT
   MT-FULL-MAP MT-KEY-D 1 >str MT-ASSERT-MISS ;

: MT-TEST-MAP-EACH-EMPTY ( -- )
   MT-MAP MT-CAP MT-MAP-INIT
   MT-EACH-RESET
   MT-MAP [: MT-EACH-RECORD ;] EACH
   MT-EACH-COUNT @ 0 MT=
   MT-EACH-SUM @ 0 MT=
   MT-EACH-LEN-SUM @ 0 MT= ;

: MT-TEST-MAP-EACH-INSERTS ( -- )
   MT-MAP MT-CAP MT-MAP-INIT
   11 MT-MAP MT-KEY-A 1 >str SET
   22 MT-MAP MT-KEY-B 1 >str SET
   MT-EACH-RESET
   MT-MAP [: MT-EACH-RECORD ;] EACH
   MT-EACH-COUNT @ 2 MT=
   MT-EACH-SUM @ 33 MT=
   MT-EACH-LEN-SUM @ 2 MT= ;

: MT-TEST-MAP-EACH-UPDATES ( -- )
   MT-MAP MT-CAP MT-MAP-INIT
   11 MT-MAP MT-KEY-A 1 >str SET
   22 MT-MAP MT-KEY-B 1 >str SET
   33 MT-MAP MT-KEY-A 1 >str SET
   MT-EACH-RESET
   MT-MAP [: MT-EACH-RECORD ;] EACH
   MT-EACH-COUNT @ 2 MT=
   MT-EACH-SUM @ 55 MT=
   MT-MAP MT-KEY-A 1 >str 33 MT-ASSERT-HIT ;

: MT-TEST-MAP-EACH-COLLISIONS ( -- )
   MT-MAP MT-CAP MT-MAP-INIT
   MT-FIND-COLLISIONS
   11 MT-MAP 0 MT-COL 1 >str SET
   22 MT-MAP 1 MT-COL 1 >str SET
   33 MT-MAP 2 MT-COL 1 >str SET
   MT-EACH-RESET
   MT-MAP [: MT-EACH-RECORD ;] EACH
   MT-EACH-COUNT @ 3 MT=
   MT-EACH-SUM @ 66 MT= ;

: MT-ORDER-RECORD ( str n -- )
   drop STR:DATA c@ MT-ORDER MT-EACH-COUNT @ + c!
   MT-EACH-COUNT @ 1 + MT-EACH-COUNT ! ;

: MT-SPREAD-FILL ( ^n -- ) {: m:^ :}
   m MT-SPREAD-CAP MT-MAP-INIT
   26 0 ?do i m i MT-LETTER 1 >str SET loop ;

\ Each map hashes under its own secret key, so two maps given the same keys in the
\ same order place them apart; one fixed hash would make EACH visit both in one
\ order. With 26 keys an equal order by chance is negligible.
: MT-TEST-MAP-KEYED ( -- )
   MT-FILL-LETTERS
   MT-SPREAD-A MT-SPREAD-FILL
   MT-SPREAD-B MT-SPREAD-FILL
   0 MT-EACH-COUNT !
   MT-SPREAD-A [: MT-ORDER-RECORD ;] EACH
   MT-SPREAD-B [: MT-ORDER-RECORD ;] EACH
   MT-EACH-COUNT @ 52 MT=
   0 0= 0=
   26 0 ?do MT-ORDER i + c@ MT-ORDER 26 + i + c@ <> or loop
   MT-ASSERT ;

: MT-KEYED-FILL ( ^n -- ) {: m:^ :}
   m MT-SPREAD-CAP >COUNT $5EED $C0DE INIT-KEYED
   26 0 ?do i m i MT-LETTER 1 >str SET loop ;

\ A map keyed by INIT-KEYED hashes under the caller's k0 and k1, and two maps
\ keyed alike given the same SETs in the same order visit alike.
: MT-TEST-INIT-KEYED ( -- )
   MT-FILL-LETTERS
   MT-SPREAD-A MT-KEYED-FILL
   MT-SPREAD-B MT-KEYED-FILL
   MT-SPREAD-A 2 MT-LETTER 1 >str 2 MT-ASSERT-HIT
   MT-SPREAD-A 0 MT-LETTER 1 >str MT-MAP-HASH
   $5EED $C0DE 0 MT-LETTER 1 >str SIPHASH:BYTES HASH-MASK and MT=
   0 MT-EACH-COUNT !
   MT-SPREAD-A [: MT-ORDER-RECORD ;] EACH
   MT-SPREAD-B [: MT-ORDER-RECORD ;] EACH
   MT-EACH-COUNT @ 52 MT=
   0 0=
   26 0 ?do MT-ORDER i + c@ MT-ORDER 26 + i + c@ = and loop
   MT-ASSERT ;

: MT-LIST@ ( n -- n )
   cells MT-MIX-LIST + @ ;

: MT-LIST! ( n n -- )
   cells MT-MIX-LIST + ! ;

: MT-LIST-COUNT ( -- n )
   0 MT-MIX-KEYS 0 ?do i MT-LIST@ 0 >= if 1 + then loop ;

: MT-LIST-MASK ( -- n )
   0 MT-MIX-KEYS 0 ?do i MT-LIST@ 0 >= if 1 i lshift or then loop ;

: MT-MIX-CHECK-KEY ( n -- ) {: k :}
   k MT-LIST@ {: want :}
   want 0 < if MT-MIX-MAP k MT-LETTER 1 >str MT-ASSERT-MISS exit then
   MT-MIX-MAP k MT-LETTER 1 >str want MT-ASSERT-HIT ;

\ A SET of a new key into a full map throws and leaves the list as it was.
: MT-MIX-SET ( n n -- ) {: v k :}
   k MT-LIST@ 0 <  MT-LIST-COUNT MT-MIX-CAP =  and if
      [: v MT-MIX-MAP k MT-LETTER 1 >str SET ;] catch
      E-MAP-FULL MT= exit
   then
   v MT-MIX-MAP k MT-LETTER 1 >str SET
   v k MT-LIST! ;

: MT-MIX-REMOVE ( n -- ) {: k :}
   MT-MIX-MAP k MT-LETTER 1 >str REMOVE bool>n
   k MT-LIST@ 0 >= bool>n MT=
   -1 k MT-LIST! ;

: MT-MIX-OP ( -- )
   MT-MIX-KEYS PROP:RND% {: k :}
   3 PROP:RND% case
      0 of 1000000 PROP:RND% k MT-MIX-SET endof
      1 of k MT-MIX-REMOVE endof
      k MT-MIX-CHECK-KEY
   endcase ;

\ Counts EACH's visits and ORs their letters' bits: with the count equal to the
\ list's, the mask equal to the list's means each entry was visited once.
: MT-MIX-RECORD ( str n -- ) {: key:str value :}
   key STR:LEN 1 MT=
   key STR:DATA c@ 97 - {: k :}
   k MT-LIST@ value MT=
   MT-EACH-SUM @ 1 k lshift or MT-EACH-SUM !
   MT-EACH-COUNT @ 1 + MT-EACH-COUNT ! ;

: MT-MIX-SWEEP ( -- )
   MT-MIX-KEYS 0 ?do i MT-MIX-CHECK-KEY loop
   MT-EACH-RESET
   MT-MIX-MAP [: MT-MIX-RECORD ;] EACH
   MT-EACH-COUNT @ MT-LIST-COUNT MT=
   MT-EACH-SUM @ MT-LIST-MASK MT=
   MT-MIX-MAP MT-MAP-COUNT@ MT-LIST-COUNT MT= ;

\ A seeded mix of SET, REMOVE and GET over 26 keys in 13 slots, checked after
\ every step against a plain list of the entries present.
: MT-TEST-MAP-MIX ( -- )
   MT-FILL-LETTERS
   MT-MIX-MAP MT-MIX-CAP >COUNT $5EED $C0DE INIT-KEYED
   MT-MIX-KEYS 0 ?do -1 i MT-LIST! loop
   $3A7 MT-MIX-OPS PROP:RUN-RESET
   PROP:COUNT@ 0 ?do MT-MIX-OP MT-MIX-SWEEP loop ;

\ ROOM makes a null map with capacity 8 and doubles it as SETs fill it past
\ half: nine keys end at 32, each still found.
: MT-TEST-ROOM ( -- )
   MT-FILL-LETTERS
   ARENA:NEW {: a:^ :}
   NULL-PTR MT-ROOM-MAP !
   MT-ROOM-MAP a ROOM
   MT-ROOM-MAP @ MT-MAP-CAP@ 8 MT=
   9 0 ?do
      MT-ROOM-MAP a ROOM
      i 100 + MT-ROOM-MAP @ i MT-LETTER 1 >str SET
   loop
   MT-ROOM-MAP @ MT-MAP-CAP@ 32 MT=
   9 0 ?do
      MT-ROOM-MAP @ i MT-LETTER 1 >str i 100 + MT-ASSERT-HIT
   loop
   a ARENA:RELEASE ;

: MT-REPORT ( -- )
   MT-FAIL @ 0= if s" map-test: ok" type cr exit then
   s" map-test: " type MT-FAIL @ FMT:.INT s"  failure(s)" type cr
   s" map-test: failures" EXIT-STATUS:FAILURE die ;

: MT-MAIN ( -- )
   MT-TEST-INIT-CLEAR
   MT-TEST-MAP-SET-GET
   MT-TEST-MAP-COLLISIONS
   MT-TEST-MAP-FULL
   MT-TEST-MAP-EACH-EMPTY
   MT-TEST-MAP-EACH-INSERTS
   MT-TEST-MAP-EACH-UPDATES
   MT-TEST-MAP-EACH-COLLISIONS
   MT-TEST-MAP-KEYED
   MT-TEST-INIT-KEYED
   MT-TEST-MAP-MIX
   MT-TEST-ROOM
   MT-REPORT ;

MT-MAIN

;package
