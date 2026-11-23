split-str
\ map-core.f - package MAP: fixed-capacity open-addressed string-key map in
\ caller cells (CELL-COUNT sizes them; not CELLS, which would shadow `cells` in
\ later bodies). Layout, hash, probe and locate are private; src/lib/map-test.f
\ reopens the package. Every call takes the capacity INIT or INIT-KEYED was
\ given; a SET into a full table throws E-MAP-FULL.
\ Probing is linear from a key's home slot (its hash mod the capacity), and no
\ empty slot lies between an entry's home and its slot, so LOCATE stops at the
\ first empty one.
\ REMOVE keeps that by backward-shift deletion, with no tombstones: each entry
\ after the hole, up to the next empty slot, moves back into the hole when the
\ hole lies on its path from home, and leaves a new hole. A full table has no
\ empty slot, so REMOVE's walk ends after capacity-1 slots; that suffices
\ because the slot SET filled last lies on no other entry's path.
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

public

: CELL-COUNT ( count -- count ) {: cap :}
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
   cap COUNT>N m CAP-OFF HEADER-FIELD ! ;

: COUNT! ( count ^a -- ) {: count m:^ :}
   count COUNT>N m COUNT-OFF HEADER-FIELD ! ;

: SLOTS ( ^a -- ^a )
   HEADER-CELLS cells + ;

: SLOT ( ^a idx -- ^a ) {: m:^ ix :}
   m SLOTS ix IDX>N SLOT-CELLS * cells + ;

: SLOT-FIELD ( ^a idx off -- ^a ) {: m:^ ix off :}
   m ix SLOT off OFF>N cells + ;

: NUM-FIELD ( ^a idx off -- ^n )
   SLOT-FIELD BYTE-VIEW CELL-VIEW ;

: SLOT-STATE@ ( ^a idx -- slot-state )
   SLOT-STATE-OFF >OFF NUM-FIELD @ 0= if
      MAP-SLOT--STATE:EMPTY
   else
      MAP-SLOT--STATE:OCCUPIED
   then ;

: SLOT-STATE! ( slot-state ^a idx -- ) {: m:^ ix:idx :}
   MATCH slot-state
     empty OF 0 ENDOF
     occupied OF 1 ENDOF
   ;MATCH
   m ix SLOT-STATE-OFF >OFF NUM-FIELD ! ;

: SLOT-HASH@ ( ^a idx -- n )
   SLOT-HASH-OFF >OFF NUM-FIELD @ ;

: SLOT-HASH! ( n ^a idx -- ) {: hash m:^ ix :}
   hash m ix SLOT-HASH-OFF >OFF NUM-FIELD ! ;

: SLOT-KEY-A@ ( ^a idx -- ^u8 )
   SLOT-KEY-A-OFF >OFF SLOT-FIELD 0 ptr-field @ ;

: SLOT-KEY-A! ( ^u8 ^a idx -- ) {: key:^ m:^ ix :}
   key m ix SLOT-KEY-A-OFF >OFF SLOT-FIELD 0 ptr-field ! ;

: SLOT-KEY-U@ ( ^a idx -- n )
   SLOT-KEY-U-OFF >OFF NUM-FIELD @ ;

: SLOT-KEY-U! ( n ^a idx -- ) {: len m:^ ix :}
   len m ix SLOT-KEY-U-OFF >OFF NUM-FIELD ! ;

: SLOT-VALUE@ ( ^a idx -- a )
   SLOT-VALUE-OFF >OFF SLOT-FIELD @ ;

: SLOT-VALUE! ( a ^a idx -- ) {: value m:^ ix :}
   value m ix SLOT-VALUE-OFF >OFF SLOT-FIELD ! ;

: SLOT-CLEAR ( ^a idx -- ) {: m:^ ix :}
   0 m ix SLOT-HASH!
   NULL$ drop m ix SLOT-KEY-A!
   0 m ix SLOT-KEY-U!
   0 m ix SLOT-VALUE-OFF >OFF NUM-FIELD !
   MAP-SLOT--STATE:EMPTY m ix SLOT-STATE! ;

public

: CLEAR ( ^a -- ) {: m:^ :}
   m CAP@ {: cap :}
   0 m COUNT-OFF HEADER-FIELD !
   cap COUNT>N 0 ?do
      m i >IDX SLOT-CLEAR
   loop ;

\ Maps of one capacity and key given the same SETs and REMOVEs in one order
\ store them alike; a map of keys from outside takes INIT's secret key.
: INIT-KEYED ( ^a count n n -- )
   {: m:^ cap k0 k1 :}
   k0 m HASH-K0-OFF HEADER-FIELD !
   k1 m HASH-K1-OFF HEADER-FIELD !
   cap m CAP!
   m CLEAR ;

private

: HASH ( ^a str -- n ) {: m:^ a:^ u :}
   m HASH-K0-OFF HEADER-FIELD @  m HASH-K1-OFF HEADER-FIELD @
   a u SIPHASH:BYTES HASH-MASK and ;

: INDEX ( n count -- idx ) {: hash cap :}
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

: SLOT-MATCH? ( ^a idx n str -- bool ) {: m:^ ix hash key:^ len :}
   m ix SLOT-HASH@ hash <> if 0 0= 0= exit then
   m ix SLOT-KEY-A@ m ix SLOT-KEY-U@ key len STR= ;

: LOCATE-SLOT ( ^a idx str n -- loc ) {: m:^ ix:idx key:^ len:n hash:n :}
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

: LOCATE ( ^a count str -- loc n ) {: m:^ cap:count key:^ len:n :}
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

: SLOT-INSERT ( a ^a idx n str -- ) {: value m:^ ix hash key:^ len :}
   m COUNT@ COUNT>N 1 + >COUNT m COUNT!
   hash m ix SLOT-HASH!
   key m ix SLOT-KEY-A!
   len m ix SLOT-KEY-U!
   value m ix SLOT-VALUE!
   MAP-SLOT--STATE:OCCUPIED m ix SLOT-STATE! ;

\ Forward probe steps from slot a to slot b.
: GAP ( idx idx count -- n ) {: a b cap :}
   b IDX>N a IDX>N - cap COUNT>N + cap COUNT>N mod ;

: SLOT-MOVE ( ^a idx idx -- ) {: m:^ from to :}
   m from SLOT-HASH@ m to SLOT-HASH!
   m from SLOT-KEY-A@ m to SLOT-KEY-A!
   m from SLOT-KEY-U@ m to SLOT-KEY-U!
   m from SLOT-VALUE@ m to SLOT-VALUE!
   MAP-SLOT--STATE:OCCUPIED m to SLOT-STATE! ;

\ Moves the entry in slot ix into the hole when the hole is on its probe path;
\ answers the hole left.
: SHIFT-BACK ( idx ^a count idx -- idx ) {: hole m:^ cap ix :}
   m ix SLOT-HASH@ cap INDEX {: home :}
   home ix cap GAP  hole ix cap GAP  < if hole exit then
   m ix hole SLOT-MOVE ix ;

\ Slot `step` past the removed slot ix: false at an empty slot, which ends the
\ chain that could probe through the hole.
: SHIFT-STEP ( idx ^a count idx n -- idx bool ) {: hole m:^ cap ix step :}
   ix IDX>N step >COUNT cap PROBE {: at :}
   m at SLOT-STATE@ OCCUPIED? 0= if hole false exit then
   hole m cap at SHIFT-BACK true ;

: SLOT-REMOVE ( ^a count idx -- ) {: m:^ cap ix :}
   m COUNT@ COUNT>N 1 - >COUNT m COUNT!
   ix
   cap COUNT>N 1 ?do
      m cap ix i SHIFT-STEP 0= if leave then
   loop
   m swap SLOT-CLEAR ;

public

: GET ( ^n count str -- option<n> ) {: m:^ cap:count key:^ len:n :}
   m cap key len LOCATE drop MATCH loc
     full OF OPTION:NONE ENDOF
     free OF drop OPTION:NONE ENDOF
     found OF m swap SLOT-VALUE@ OPTION:SOME ENDOF
   ;MATCH ;

: HAS? ( ^n count str -- bool )
   GET MATCH option
     none OF 0 0= 0= ENDOF
     some OF drop 0 0= ENDOF
   ;MATCH ;

\ Stores the key's pointer and length, not a copy; a replacement keeps the first
\ pointer: the key bytes stay alive and unchanged while the entry exists.
: SET ( n ^n count str -- ) {: value:n m:^ cap:count key:^ len:n :}
   m cap key len LOCATE {: hash:n :}
   MATCH loc
     full OF E-MAP-FULL throw ENDOF
     free OF value m rot hash key len SLOT-INSERT ENDOF
     found OF value m rot SLOT-VALUE! ENDOF
   ;MATCH ;

\ Removes the key's entry; true when there was one.
: REMOVE ( ^n count str -- bool ) {: m:^ cap:count key:^ len:n :}
   m cap key len LOCATE drop MATCH loc
     full OF false ENDOF
     free OF drop false ENDOF
     found OF m cap rot SLOT-REMOVE true ENDOF
   ;MATCH ;

\ Visits in storage order, which follows the map's hash key. q must not REMOVE
\ from this map: a shift back can make EACH skip an entry or visit one twice.
: EACH ( ^n count [ str n -- ] -- ) {: m:^ cap q :}
   cap COUNT>N 0 ?do
      m i >IDX SLOT-STATE@ OCCUPIED? if
         m i >IDX SLOT-KEY-A@
         m i >IDX SLOT-KEY-U@
         m i >IDX SLOT-VALUE@
         q execute
      then
   loop ;

;package
