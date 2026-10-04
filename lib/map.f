\ map.f - fixed-capacity open-addressed string-key map.
\
\ The module lives in `package MAP`. The caller owns the storage: MAP:CELL-COUNT
\ answers how many cells a capacity needs, MAP:INIT lays the map out in them and
\ MAP:CLEAR empties it. MAP:SET, MAP:GET, MAP:HAS? and MAP:EACH are the map
\ itself, and MAP:CAP@ / MAP:COUNT@ read the header. The sizing word is
\ CELL-COUNT and not CELLS because a public CELLS would shadow the built-in
\ `cells` for every later body in the package. The header and slot layout, the
\ hash, the probe sequence and the locate step are package-private;
\ lib/map-test.f reopens the package to reach them.
\
\ The two families are public because only a public family gets generated
\ constructors (MAP-SLOT--STATE:EMPTY, MAP-LOC:FOUND); the words that produce
\ and consume them are private.

require lib/errors.f
require lib/string.f
require lib/adt/option.f                      \ option<n> for GET (switchover wave A)

package MAP
public

\ slot-state - the per-slot lifecycle tag (switchover wave C). An ENUM instead
\ of raw 0/-1/1 sentinel ints: the checker forces every consumer through MATCH
\ or the predicates below, and a plain n can no longer pose as a slot state.
ENUM slot-state
  empty
  deleted
  occupied
;ENUM

\ loc - the three-way LOCATE verdict (switchover wave C). A payload sum instead
\ of 0/1/2 constants plus a -1 idx placeholder: free/found carry the slot index,
\ full carries nothing, and the checker forces every consumer through
\ exhaustive MATCH.
SUMTYPE loc 0
  VARIANT full ;VARIANT
  VARIANT free idx ;VARIANT
  VARIANT found idx ;VARIANT
;SUMTYPE

private

0 constant CAP-OFF
1 constant COUNT-OFF
2 constant DELETED-OFF
3 constant HEADER-CELLS

0 constant SLOT-STATE-OFF
1 constant SLOT-HASH-OFF
2 constant SLOT-KEY-A-OFF
3 constant SLOT-KEY-U-OFF
4 constant SLOT-VALUE-OFF
5 constant SLOT-CELLS

5381 constant HASH-SEED
33 constant HASH-MUL
$7FFFFFFFFFFFFFFF constant HASH-MASK
HASH-MASK HEADER-CELLS - SLOT-CELLS / constant MAX-CAP

: CHECK-CAP ( count -- ) {: cap :}
   cap COUNT>N 0 <= if E-MAP-BAD-CAP throw then
   cap COUNT>N MAX-CAP > if E-MAP-BAD-CAP throw then ;

: CHECK-LEN ( len -- ) {: len :}
   len LEN>N 0 < if E-MAP-BAD-CAP throw then ;

public

\ Cells the caller allots for a map of this capacity.
: CELL-COUNT ( count -- count ) {: cap :}
   cap CHECK-CAP
   HEADER-CELLS cap COUNT>N SLOT-CELLS * + >COUNT ;

private

: EMPTY? ( slot-state -- bool )
   MATCH slot-state
     empty OF 0 0= ENDOF
     deleted OF 0 0= 0= ENDOF
     occupied OF 0 0= 0= ENDOF
   ;MATCH ;

: DELETED? ( slot-state -- bool )
   MATCH slot-state
     empty OF 0 0= 0= ENDOF
     deleted OF 0 0= ENDOF
     occupied OF 0 0= 0= ENDOF
   ;MATCH ;

: OCCUPIED? ( slot-state -- bool )
   MATCH slot-state
     empty OF 0 0= 0= ENDOF
     deleted OF 0 0= 0= ENDOF
     occupied OF 0 0= ENDOF
   ;MATCH ;

: HEADER-FIELD ( ptr a n -- ptr n )
   cells + BYTE-VIEW CELL-VIEW ;

public

: CAP@ ( ptr a -- count )
   CAP-OFF HEADER-FIELD @ >COUNT ;

\ Occupied entries.
: COUNT@ ( ptr a -- count )
   COUNT-OFF HEADER-FIELD @ >COUNT ;

private

: CAP! ( count ptr a -- ) {: cap m:ptr :}
   cap CHECK-CAP
   cap COUNT>N m CAP-OFF HEADER-FIELD ! ;

: CHECK-HANDLE ( ptr a count -- ) {: m:ptr cap :}
   cap CHECK-CAP
   m CAP@ COUNT>N cap COUNT>N <> if E-MAP-BAD-CAP throw then ;

: DELETED@ ( ptr a -- count )
   DELETED-OFF HEADER-FIELD @ >COUNT ;

: COUNT! ( count ptr a -- ) {: count m:ptr :}
   count COUNT>N 0 < if E-MAP-BAD-CAP throw then
   count COUNT>N m CAP@ COUNT>N m DELETED@ COUNT>N - > if E-MAP-FULL throw then
   count COUNT>N m COUNT-OFF HEADER-FIELD ! ;

: DELETED! ( count ptr a -- ) {: deleted m:ptr :}
   deleted COUNT>N 0 < if E-MAP-BAD-CAP throw then
   deleted COUNT>N m CAP@ COUNT>N m COUNT@ COUNT>N - > if E-MAP-FULL throw then
   deleted COUNT>N m DELETED-OFF HEADER-FIELD ! ;

: SLOTS ( ptr a -- ptr a )
   HEADER-CELLS cells + ;

: CHECK-INDEX ( ptr a idx -- ) {: m:ptr ix :}
   ix IDX>N 0 < if E-MAP-BAD-CAP throw then
   ix IDX>N m CAP@ COUNT>N >= if E-MAP-BAD-CAP throw then ;

: SLOT ( ptr a idx -- ptr a ) {: m:ptr ix :}
   m ix CHECK-INDEX
   m SLOTS ix IDX>N SLOT-CELLS * cells + ;

: SLOT-FIELD ( ptr a idx off -- ptr a ) {: m:ptr ix off :}
   off OFF>N 0 < if E-MAP-BAD-CAP throw then
   off OFF>N SLOT-CELLS >= if E-MAP-BAD-CAP throw then
   m ix SLOT off OFF>N cells + ;

: NUM-FIELD ( ptr a idx off -- ptr n )
   SLOT-FIELD BYTE-VIEW CELL-VIEW ;

: SLOT-STATE@ ( ptr a idx -- slot-state )
   SLOT-STATE-OFF >OFF NUM-FIELD @ case
      0 of MAP-SLOT--STATE:EMPTY endof
      1 of MAP-SLOT--STATE:DELETED endof
      2 of MAP-SLOT--STATE:OCCUPIED endof
      ENGINE-ERROR:BAD-TAG throw
   endcase ;

: SLOT-STATE! ( slot-state ptr a idx -- ) {: m:ptr ix:idx :}   \ enum stays on stack; the checker owns state validity
   MATCH slot-state
     empty OF 0 ENDOF
     deleted OF 1 ENDOF
     occupied OF 2 ENDOF
   ;MATCH
   m ix SLOT-STATE-OFF >OFF NUM-FIELD ! ;

: SLOT-HASH@ ( ptr a idx -- n )
   SLOT-HASH-OFF >OFF NUM-FIELD @ ;

: SLOT-HASH! ( n ptr a idx -- ) {: hash m:ptr ix :}
   hash 0 < if E-MAP-BAD-CAP throw then
   hash m ix SLOT-HASH-OFF >OFF NUM-FIELD ! ;

: SLOT-KEY-A@ ( ptr a idx -- ptr u8 )
   SLOT-KEY-A-OFF >OFF SLOT-FIELD 0 ptr-field @ ;

: SLOT-KEY-A! ( ptr u8 ptr a idx -- ) {: key:ptr m:ptr ix :}
   key m ix SLOT-KEY-A-OFF >OFF SLOT-FIELD 0 ptr-field ! ;

: SLOT-KEY-U@ ( ptr a idx -- len )
   SLOT-KEY-U-OFF >OFF NUM-FIELD @ >LEN ;

: SLOT-KEY-U! ( len ptr a idx -- ) {: len m:ptr ix :}
   len LEN>N 0 < if E-MAP-BAD-CAP throw then
   len LEN>N m ix SLOT-KEY-U-OFF >OFF NUM-FIELD ! ;

: SLOT-VALUE@ ( ptr a idx -- a )
   SLOT-VALUE-OFF >OFF SLOT-FIELD @ ;

: SLOT-VALUE! ( a ptr a idx -- ) {: value m:ptr ix :}
   value m ix SLOT-VALUE-OFF >OFF SLOT-FIELD ! ;

: SLOT-CLEAR ( ptr a idx -- ) {: m:ptr ix :}
   0 m ix SLOT-HASH!
   NULL$ drop m ix SLOT-KEY-A!
   0 >LEN m ix SLOT-KEY-U!
   0 m ix SLOT-VALUE-OFF >OFF NUM-FIELD !
   MAP-SLOT--STATE:EMPTY m ix SLOT-STATE! ;

public

: CLEAR ( ptr a -- ) {: m:ptr :}
   m CAP@ dup CHECK-CAP {: cap :}
   0 m COUNT-OFF HEADER-FIELD !
   0 m DELETED-OFF HEADER-FIELD !
   cap COUNT>N 0 ?do
      m i >IDX SLOT-CLEAR
   loop ;

: INIT ( ptr a count -- ) {: m:ptr cap :}
   cap m CAP!
   m CLEAR ;

private

: HASH ( ptr u8 len -- n ) {: a:ptr u :}
   u CHECK-LEN
   HASH-SEED
   u LEN>N 0 ?do
      HASH-MUL * a i + c@ + HASH-MASK and
   loop ;

: INDEX ( n count -- idx ) {: hash cap :}
   cap CHECK-CAP
   hash cap COUNT>N mod dup 0 < if cap COUNT>N + then >IDX ;

: PROBE ( n count count -- idx ) {: hash step cap :}
   hash cap INDEX IDX>N
   step COUNT>N cap INDEX IDX>N
   {: base inc :}
   cap COUNT>N 1 - base - inc < if
      inc cap COUNT>N base - -
   else
      base inc +
   then >IDX ;

: SLOT-MATCH? ( ptr a idx n ptr u8 len -- bool ) {: m:ptr ix hash key:ptr len :}
   m ix SLOT-STATE@ OCCUPIED? 0= if 0 0= 0= exit then
   m ix SLOT-HASH@ hash <> if 0 0= 0= exit then
   m ix SLOT-KEY-A@ m ix SLOT-KEY-U@ LEN>N key len LEN>N STR= ;

: REMEMBER-FREE ( n idx -- n ) {: free ix :}
   free 0 < if ix IDX>N else free then ;

: LOCATE-SLOT ( n ptr a idx ptr u8 len n -- n loc ) {: fm:n m:ptr ix:idx key:ptr len:len hash:n :}
   m ix SLOT-STATE@ MATCH slot-state
     empty OF fm ix REMEMBER-FREE dup >IDX MAP-LOC:FREE ENDOF
     deleted OF fm ix REMEMBER-FREE MAP-LOC:FULL ENDOF
     occupied OF
        m ix hash key len SLOT-MATCH? if
           fm ix MAP-LOC:FOUND
        else
           fm MAP-LOC:FULL
        then
     ENDOF
   ;MATCH ;

: LOCATE ( ptr a count ptr u8 len -- loc n ) {: m:ptr cap:count key:ptr len:len :}
   m cap CHECK-HANDLE
   len CHECK-LEN
   key len HASH {: hash:n :}
   -1
   cap COUNT>N 0 ?do
      m hash i >COUNT cap PROBE key len hash LOCATE-SLOT
      dup MATCH loc                            \ full = keep probing; free/found terminate
        full OF 0 0= 0= ENDOF
        free OF drop 0 0= ENDOF
        found OF drop 0 0= ENDOF
      ;MATCH if
         nip hash unloop exit                  \ drop the free memo, return verdict + hash
      then
      drop
   loop
   dup 0 < if drop MAP-LOC:FULL else >IDX MAP-LOC:FREE then hash ;

: SLOT-INSERT ( a ptr a idx n ptr u8 len -- ) {: value m:ptr ix hash key:ptr len :}
   m ix SLOT-STATE@ MATCH slot-state
     empty OF ENDOF
     deleted OF m DELETED@ COUNT>N 1 - >COUNT m DELETED! ENDOF
     occupied OF E-MAP-FULL throw ENDOF
   ;MATCH
   m COUNT@ COUNT>N 1 + >COUNT m COUNT!
   hash m ix SLOT-HASH!
   key m ix SLOT-KEY-A!
   len m ix SLOT-KEY-U!
   value m ix SLOT-VALUE!
   MAP-SLOT--STATE:OCCUPIED m ix SLOT-STATE! ;

public

: GET ( ptr n count ptr u8 len -- option<n> ) {: m:ptr cap:count key:ptr len:len :}   \ SOME value if the key is present, else NONE
   m cap key len LOCATE drop MATCH loc
     full OF OPTION:NONE ENDOF
     free OF drop OPTION:NONE ENDOF
     found OF m swap SLOT-VALUE@ OPTION:SOME ENDOF
   ;MATCH ;

: HAS? ( ptr n count ptr u8 len -- bool )
   GET MATCH option
     none OF 0 0= 0= ENDOF
     some OF drop 0 0= ENDOF
   ;MATCH ;

: SET ( n ptr n count ptr u8 len -- ) {: value:n m:ptr cap:count key:ptr len:len :}
   m cap key len LOCATE {: hash:n :}
   MATCH loc
     full OF E-MAP-FULL throw ENDOF
     free OF value m rot hash key len SLOT-INSERT ENDOF
     found OF value m rot SLOT-VALUE! ENDOF
   ;MATCH ;

\ Visit occupied entries in ascending storage-slot order.
: EACH ( ptr n count [ ptr u8 len n -- ] -- ) {: m:ptr cap q :}
   m cap CHECK-HANDLE
   cap COUNT>N 0 ?do
      m i >IDX SLOT-STATE@ OCCUPIED? if
         m i >IDX SLOT-KEY-A@
         m i >IDX SLOT-KEY-U@
         m i >IDX SLOT-VALUE@
         q execute
      then
   loop ;

;package
