\ map.f - package MAP: fixed-capacity open-addressed string-key map in caller
\ cells (CELL-COUNT sizes them; not CELLS, which would shadow `cells` in later
\ bodies). Layout, hash, probe and locate are private; src/lib/map-test.f
\ reopens the package. Every call takes the capacity INIT was given; a bad
\ capacity, malformed storage or a full table throws E-MAP-*.
\ STORAGE CLASS. CALLER-OWNED: the caller's cells; no module state.

require src/lib/errors.f
require src/lib/string.f
require src/lib/siphash.f
require src/lib/entropy.f
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
2 constant HASH-K0-OFF                \ the SipHash key, k0 then k1, drawn by INIT
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

: HEADER-FIELD ( ptr a n -- ptr n )
   cells + BYTE-VIEW CELL-VIEW ;

public

: CAP@ ( ptr a -- count )
   CAP-OFF HEADER-FIELD @ >COUNT ;

: COUNT@ ( ptr a -- count )
   COUNT-OFF HEADER-FIELD @ >COUNT ;

private

: CAP! ( count ptr a -- ) {: cap m:ptr :}
   cap CHECK-CAP
   cap COUNT>N m CAP-OFF HEADER-FIELD ! ;

: CHECK-HANDLE ( ptr a count -- ) {: m:ptr cap :}
   cap CHECK-CAP
   m CAP@ COUNT>N cap COUNT>N <> if E-MAP-BAD-CAP throw then ;

: COUNT! ( count ptr a -- ) {: count m:ptr :}
   count COUNT>N 0 < if E-MAP-BAD-CAP throw then
   count COUNT>N m CAP@ COUNT>N > if E-MAP-FULL throw then
   count COUNT>N m COUNT-OFF HEADER-FIELD ! ;

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
      1 of MAP-SLOT--STATE:OCCUPIED endof
      ENGINE-ERROR:BAD-TAG throw
   endcase ;

: SLOT-STATE! ( slot-state ptr a idx -- ) {: m:ptr ix:idx :}
   MATCH slot-state
     empty OF 0 ENDOF
     occupied OF 1 ENDOF
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
   cap COUNT>N 0 ?do
      m i >IDX SLOT-CLEAR
   loop ;

\ Draws the SipHash-2-4 key into the header from the OS generator
\ (src/lib/entropy.f), so crafted keys cannot aim at one probe chain: chains are
\ expected short over the secret, not guaranteed. The key lives in the map's
\ cells, so a map INITed while building an image carries it into every run; a
\ map for untrusted keys is INITed at run time.
: INIT ( ptr a count -- ) {: m:ptr cap :}
   m HASH-K0-OFF cells + BYTE-VIEW 2 cells ENTROPY:FILL
   cap m CAP!
   m CLEAR ;

private

: HASH ( ptr a ptr u8 len -- n ) {: m:ptr a:ptr u :}
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

: SLOT-MATCH? ( ptr a idx n ptr u8 len -- bool ) {: m:ptr ix hash key:ptr len :}
   m ix SLOT-HASH@ hash <> if 0 0= 0= exit then
   m ix SLOT-KEY-A@ m ix SLOT-KEY-U@ LEN>N key len LEN>N STR= ;

: LOCATE-SLOT ( ptr a idx ptr u8 len n -- loc ) {: m:ptr ix:idx key:ptr len:len hash:n :}
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

: LOCATE ( ptr a count ptr u8 len -- loc n ) {: m:ptr cap:count key:ptr len:len :}
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

: SLOT-INSERT ( a ptr a idx n ptr u8 len -- ) {: value m:ptr ix hash key:ptr len :}
   m COUNT@ COUNT>N 1 + >COUNT m COUNT!
   hash m ix SLOT-HASH!
   key m ix SLOT-KEY-A!
   len m ix SLOT-KEY-U!
   value m ix SLOT-VALUE!
   MAP-SLOT--STATE:OCCUPIED m ix SLOT-STATE! ;

public

: GET ( ptr n count ptr u8 len -- option<n> ) {: m:ptr cap:count key:ptr len:len :}
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

\ Stores the caller's key pointer and length, not a copy; a replacement keeps
\ the first key's pointer: the key bytes stay alive and unchanged while the
\ entry exists.
: SET ( n ptr n count ptr u8 len -- ) {: value:n m:ptr cap:count key:ptr len:len :}
   m cap key len LOCATE {: hash:n :}
   MATCH loc
     full OF E-MAP-FULL throw ENDOF
     free OF value m rot hash key len SLOT-INSERT ENDOF
     found OF value m rot SLOT-VALUE! ENDOF
   ;MATCH ;

\ Storage order follows the keyed hash, so it differs between maps, and between
\ runs for maps INITed at run time.
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
