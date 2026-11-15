\ map-core.f - package MAP: fixed-capacity open-addressed string-key map in
\ caller cells (CELL-COUNT sizes them; not CELLS, which would shadow `cells` in
\ later bodies). Layout, hash, probe and locate are private; src/lib/map-test.f
\ reopens the package. Every call takes the capacity INIT or INIT-KEYED was
\ given; a bad capacity, malformed storage or a full table throws E-MAP-*.
\ STORAGE CLASS. CALLER-OWNED: the caller's cells; no module state.

require src/lib/error.f
require src/lib/string.f
require src/lib/siphash.f
require src/lib/adt/option.f

package MAP
public

\ Public because only a public family gets generated constructors
\ (MAP-SLOT--STATE:EMPTY, MAP-LOC:FOUND); the words using both are private.
ENUM slot-state
  empty
  occupied
;ENUM

SUMTYPE loc
  VARIANT full ;VARIANT
  VARIANT free idx ;VARIANT
  VARIANT found idx ;VARIANT
;SUMTYPE

private

0 constant CAP-OFF
1 constant COUNT-OFF
2 constant HASH-K0-OFF                \ the SipHash key, k0 then k1
3 constant HASH-K1-OFF
4 constant HEADER-CELLS

0 constant SLOT-STATE-OFF
1 constant SLOT-HASH-OFF
2 constant SLOT-KEY-A-OFF
3 constant SLOT-KEY-U-OFF
4 constant SLOT-VALUE-OFF
5 constant SLOT-CELLS

$7FFFFFFFFFFFFFFF constant HASH-MASK
HASH-MASK HEADER-CELLS - SLOT-CELLS / constant MAX-CAP

: CHECK-CAP ( count -- ) {: cap :}
   cap COUNT>N 0 <= if E-MAP-BAD-CAP throw then
   cap COUNT>N MAX-CAP > if E-MAP-BAD-CAP throw then ;

: CHECK-LEN ( len -- ) {: len :}
   len LEN>N 0 < if E-MAP-BAD-CAP throw then ;

public

: CELL-COUNT ( count -- count ) {: cap :}
   cap CHECK-CAP
   HEADER-CELLS cap COUNT>N SLOT-CELLS * + >COUNT ;

private

: OCCUPIED? ( slot-state -- bool )
   MATCH slot-state
     empty OF 0 0= 0= ENDOF
     occupied OF 0 0= ENDOF
   ;MATCH ;

: HEADER-FIELD ( ^a n -- ^n )
   cells + BYTE-VIEW CELL-VIEW ;

public

: CAP@ ( ^a -- count )
   CAP-OFF HEADER-FIELD @ >COUNT ;

: COUNT@ ( ^a -- count )
   COUNT-OFF HEADER-FIELD @ >COUNT ;

private

: CAP! ( count ^a -- ) {: cap m:^ :}
   cap CHECK-CAP
   cap COUNT>N m CAP-OFF HEADER-FIELD ! ;

: CHECK-HANDLE ( ^a count -- ) {: m:^ cap :}
   cap CHECK-CAP
   m CAP@ COUNT>N cap COUNT>N <> if E-MAP-BAD-CAP throw then ;

: COUNT! ( count ^a -- ) {: count m:^ :}
   count COUNT>N 0 < if E-MAP-BAD-CAP throw then
   count COUNT>N m CAP@ COUNT>N > if E-MAP-FULL throw then
   count COUNT>N m COUNT-OFF HEADER-FIELD ! ;

: SLOTS ( ^a -- ^a )
   HEADER-CELLS cells + ;

: CHECK-INDEX ( ^a idx -- ) {: m:^ ix :}
   ix IDX>N 0 < if E-MAP-BAD-CAP throw then
   ix IDX>N m CAP@ COUNT>N >= if E-MAP-BAD-CAP throw then ;

: SLOT ( ^a idx -- ^a ) {: m:^ ix :}
   m ix CHECK-INDEX
   m SLOTS ix IDX>N SLOT-CELLS * cells + ;

: SLOT-FIELD ( ^a idx off -- ^a ) {: m:^ ix off :}
   off OFF>N 0 < if E-MAP-BAD-CAP throw then
   off OFF>N SLOT-CELLS >= if E-MAP-BAD-CAP throw then
   m ix SLOT off OFF>N cells + ;

: NUM-FIELD ( ^a idx off -- ^n )
   SLOT-FIELD BYTE-VIEW CELL-VIEW ;

: SLOT-STATE@ ( ^a idx -- slot-state )
   SLOT-STATE-OFF >OFF NUM-FIELD @ case
      0 of MAP-SLOT--STATE:EMPTY endof
      1 of MAP-SLOT--STATE:OCCUPIED endof
      E-MAP-BAD-CAP throw
   endcase ;

: SLOT-STATE! ( slot-state ^a idx -- ) {: m:^ ix:idx :}
   MATCH slot-state
     empty OF 0 ENDOF
     occupied OF 1 ENDOF
   ;MATCH
   m ix SLOT-STATE-OFF >OFF NUM-FIELD ! ;

: SLOT-HASH@ ( ^a idx -- n )
   SLOT-HASH-OFF >OFF NUM-FIELD @ ;

: SLOT-HASH! ( n ^a idx -- ) {: hash m:^ ix :}
   hash 0 < if E-MAP-BAD-CAP throw then
   hash m ix SLOT-HASH-OFF >OFF NUM-FIELD ! ;

: SLOT-KEY-A@ ( ^a idx -- ^u8 )
   SLOT-KEY-A-OFF >OFF SLOT-FIELD 0 ptr-field @ ;

: SLOT-KEY-A! ( ^u8 ^a idx -- ) {: key:^ m:^ ix :}
   key m ix SLOT-KEY-A-OFF >OFF SLOT-FIELD 0 ptr-field ! ;

: SLOT-KEY-U@ ( ^a idx -- len )
   SLOT-KEY-U-OFF >OFF NUM-FIELD @ >LEN ;

: SLOT-KEY-U! ( len ^a idx -- ) {: len m:^ ix :}
   len LEN>N 0 < if E-MAP-BAD-CAP throw then
   len LEN>N m ix SLOT-KEY-U-OFF >OFF NUM-FIELD ! ;

: SLOT-VALUE@ ( ^a idx -- a )
   SLOT-VALUE-OFF >OFF SLOT-FIELD @ ;

: SLOT-VALUE! ( a ^a idx -- ) {: value m:^ ix :}
   value m ix SLOT-VALUE-OFF >OFF SLOT-FIELD ! ;

: SLOT-CLEAR ( ^a idx -- ) {: m:^ ix :}
   0 m ix SLOT-HASH!
   NULL$ drop m ix SLOT-KEY-A!
   0 >LEN m ix SLOT-KEY-U!
   0 m ix SLOT-VALUE-OFF >OFF NUM-FIELD !
   MAP-SLOT--STATE:EMPTY m ix SLOT-STATE! ;

public

: CLEAR ( ^a -- ) {: m:^ :}
   m CAP@ dup CHECK-CAP {: cap :}
   0 m COUNT-OFF HEADER-FIELD !
   cap COUNT>N 0 ?do
      m i >IDX SLOT-CLEAR
   loop ;

\ Maps of one capacity and key given the same SETs in the same order store them
\ alike; a map whose keys come from outside the program takes INIT's secret key.
: INIT-KEYED ( ^a count n n -- )
   {: m:^ cap k0 k1 :}
   k0 m HASH-K0-OFF HEADER-FIELD !
   k1 m HASH-K1-OFF HEADER-FIELD !
   cap m CAP!
   m CLEAR ;

private

: HASH ( ^a ^u8 len -- n ) {: m:^ a:^ u :}
   u CHECK-LEN
   m HASH-K0-OFF HEADER-FIELD @  m HASH-K1-OFF HEADER-FIELD @
   a u LEN>N SIPHASH:BYTES HASH-MASK and ;

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

: SLOT-MATCH? ( ^a idx n ^u8 len -- bool ) {: m:^ ix hash key:^ len :}
   m ix SLOT-HASH@ hash <> if 0 0= 0= exit then
   m ix SLOT-KEY-A@ m ix SLOT-KEY-U@ LEN>N key len LEN>N STR= ;

: LOCATE-SLOT ( ^a idx ^u8 len n -- loc ) {: m:^ ix:idx key:^ len:len hash:n :}
   m ix SLOT-STATE@ MATCH slot-state
     empty OF ix MAP-LOC:FREE ENDOF
     occupied OF
        m ix hash key len SLOT-MATCH? if
           ix MAP-LOC:FOUND
        else
           MAP-LOC:FULL
        then
     ENDOF
   ;MATCH ;

: LOCATE ( ^a count ^u8 len -- loc n ) {: m:^ cap:count key:^ len:len :}
   m cap CHECK-HANDLE
   len CHECK-LEN
   m key len HASH {: hash:n :}
   cap COUNT>N 0 ?do
      m hash i >COUNT cap PROBE key len hash LOCATE-SLOT
      dup MATCH loc
        full OF 0 0= 0= ENDOF
        free OF drop 0 0= ENDOF
        found OF drop 0 0= ENDOF
      ;MATCH if
         hash unloop exit
      then
      drop
   loop
   MAP-LOC:FULL hash ;

: SLOT-INSERT ( a ^a idx n ^u8 len -- ) {: value m:^ ix hash key:^ len :}
   m COUNT@ COUNT>N 1 + >COUNT m COUNT!
   hash m ix SLOT-HASH!
   key m ix SLOT-KEY-A!
   len m ix SLOT-KEY-U!
   value m ix SLOT-VALUE!
   MAP-SLOT--STATE:OCCUPIED m ix SLOT-STATE! ;

public

: GET ( ^n count ^u8 len -- option<n> ) {: m:^ cap:count key:^ len:len :}
   m cap key len LOCATE drop MATCH loc
     full OF OPTION:NONE ENDOF
     free OF drop OPTION:NONE ENDOF
     found OF m swap SLOT-VALUE@ OPTION:SOME ENDOF
   ;MATCH ;

: HAS? ( ^n count ^u8 len -- bool )
   GET MATCH option
     none OF 0 0= 0= ENDOF
     some OF drop 0 0= ENDOF
   ;MATCH ;

\ Stores the key's pointer and length, not a copy; a replacement keeps the first
\ pointer: the key bytes stay alive and unchanged while the entry exists.
: SET ( n ^n count ^u8 len -- ) {: value:n m:^ cap:count key:^ len:len :}
   m cap key len LOCATE {: hash:n :}
   MATCH loc
     full OF E-MAP-FULL throw ENDOF
     free OF value m rot hash key len SLOT-INSERT ENDOF
     found OF value m rot SLOT-VALUE! ENDOF
   ;MATCH ;

\ Visits in storage order, which follows the map's hash key.
: EACH ( ^n count [ ^u8 len n -- ] -- ) {: m:^ cap q :}
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
