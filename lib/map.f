\ map.f - fixed-capacity open-addressed string-key map layout.

require lib/adt/option.f                      \ option<n> for MAP-GET (switchover wave A)

\ slot-state - the per-slot lifecycle tag (switchover wave C). An ENUM instead
\ of raw 0/-1/1 sentinel ints: the checker forces every consumer through MATCH
\ or the predicates below, and a plain n can no longer pose as a slot state.
ENUM slot-state
  empty
  deleted
  occupied
;ENUM

0 constant MAP-CAP-OFF
1 constant MAP-COUNT-OFF
2 constant MAP-DELETED-OFF
3 constant MAP-HEADER-CELLS

0 constant MAP-SLOT-STATE-OFF
1 constant MAP-SLOT-HASH-OFF
2 constant MAP-SLOT-KEY-A-OFF
3 constant MAP-SLOT-KEY-U-OFF
4 constant MAP-SLOT-VALUE-OFF
5 constant MAP-SLOT-CELLS

0 constant MAP-LOC-FULL
1 constant MAP-LOC-FREE
2 constant MAP-LOC-FOUND

5381 constant MAP-HASH-SEED
33 constant MAP-HASH-MUL
$7FFFFFFFFFFFFFFF constant MAP-HASH-MASK
MAP-HASH-MASK MAP-HEADER-CELLS - MAP-SLOT-CELLS / constant MAP-MAX-CAP

: MAP-CHECK-CAP ( count -- ) {: cap :}
   cap COUNT>N 0 <= if E-MAP-BAD-CAP throw then
   cap COUNT>N MAP-MAX-CAP > if E-MAP-BAD-CAP throw then ;

: MAP-CHECK-LEN ( len -- ) {: len :}
   len LEN>N 0 < if E-MAP-BAD-CAP throw then ;

: MAP-CELLS ( count -- count ) {: cap :}
   cap MAP-CHECK-CAP
   MAP-HEADER-CELLS cap COUNT>N MAP-SLOT-CELLS * + >COUNT ;

: MAP-EMPTY? ( slot-state -- bool )
   MATCH slot-state
     empty OF 0 0= ENDOF
     deleted OF 0 0= 0= ENDOF
     occupied OF 0 0= 0= ENDOF
   ;MATCH ;

: MAP-DELETED? ( slot-state -- bool )
   MATCH slot-state
     empty OF 0 0= 0= ENDOF
     deleted OF 0 0= ENDOF
     occupied OF 0 0= 0= ENDOF
   ;MATCH ;

: MAP-OCCUPIED? ( slot-state -- bool )
   MATCH slot-state
     empty OF 0 0= 0= ENDOF
     deleted OF 0 0= 0= ENDOF
     occupied OF 0 0= ENDOF
   ;MATCH ;

: MAP-CAP@ ( ptr a -- count )
   MAP-CAP-OFF cells + @ >COUNT ;

: MAP-CAP! ( count ptr a -- ) {: cap m:ptr :}
   cap MAP-CHECK-CAP
   cap COUNT>N m MAP-CAP-OFF cells + ! ;

: MAP-CHECK-HANDLE ( ptr a count -- ) {: m:ptr cap :}
   cap MAP-CHECK-CAP
   m MAP-CAP@ COUNT>N cap COUNT>N <> if E-MAP-BAD-CAP throw then ;

: MAP-COUNT@ ( ptr a -- count )
   MAP-COUNT-OFF cells + @ >COUNT ;

: MAP-DELETED@ ( ptr a -- count )
   MAP-DELETED-OFF cells + @ >COUNT ;

: MAP-COUNT! ( count ptr a -- ) {: count m:ptr :}
   count COUNT>N 0 < if E-MAP-BAD-CAP throw then
   count COUNT>N m MAP-CAP@ COUNT>N m MAP-DELETED@ COUNT>N - > if E-MAP-FULL throw then
   count COUNT>N m MAP-COUNT-OFF cells + ! ;

: MAP-DELETED! ( count ptr a -- ) {: deleted m:ptr :}
   deleted COUNT>N 0 < if E-MAP-BAD-CAP throw then
   deleted COUNT>N m MAP-CAP@ COUNT>N m MAP-COUNT@ COUNT>N - > if E-MAP-FULL throw then
   deleted COUNT>N m MAP-DELETED-OFF cells + ! ;

: MAP-SLOTS ( ptr a -- ptr a )
   MAP-HEADER-CELLS cells + ;

: MAP-CHECK-INDEX ( ptr a idx -- ) {: m:ptr ix :}
   ix IDX>N 0 < if E-MAP-BAD-CAP throw then
   ix IDX>N m MAP-CAP@ COUNT>N >= if E-MAP-BAD-CAP throw then ;

: MAP-SLOT ( ptr a idx -- ptr a ) {: m:ptr ix :}
   m ix MAP-CHECK-INDEX
   m MAP-SLOTS ix IDX>N MAP-SLOT-CELLS * cells + ;

: MAP-SLOT-FIELD ( ptr a idx off -- ptr a ) {: m:ptr ix off :}
   off OFF>N 0 < if E-MAP-BAD-CAP throw then
   off OFF>N MAP-SLOT-CELLS >= if E-MAP-BAD-CAP throw then
   m ix MAP-SLOT off OFF>N cells + ;

: MAP-SLOT-STATE-PTR ( ptr a idx -- ptr slot-state )
   MAP-SLOT-STATE-OFF >OFF MAP-SLOT-FIELD ;

: MAP-SLOT-STATE@ ( ptr a idx -- slot-state )
   MAP-SLOT-STATE-PTR @ ;

: MAP-SLOT-STATE! ( slot-state ptr a idx -- ) {: m:ptr ix:idx :}   \ enum stays on stack; the checker owns state validity
   m ix MAP-SLOT-STATE-PTR ! ;

: MAP-SLOT-HASH@ ( ptr a idx -- n )
   MAP-SLOT-HASH-OFF >OFF MAP-SLOT-FIELD @ ;

: MAP-SLOT-HASH! ( n ptr a idx -- ) {: hash m:ptr ix :}
   hash 0 < if E-MAP-BAD-CAP throw then
   hash m ix MAP-SLOT-HASH-OFF >OFF MAP-SLOT-FIELD ! ;

: MAP-SLOT-KEY-A@ ( ptr a idx -- ptr u8 )
   MAP-SLOT-KEY-A-OFF >OFF MAP-SLOT-FIELD @ ;

: MAP-SLOT-KEY-A! ( ptr u8 ptr a idx -- ) {: key:ptr m:ptr ix :}
   key m ix MAP-SLOT-KEY-A-OFF >OFF MAP-SLOT-FIELD ! ;

: MAP-SLOT-KEY-U@ ( ptr a idx -- len )
   MAP-SLOT-KEY-U-OFF >OFF MAP-SLOT-FIELD @ >LEN ;

: MAP-SLOT-KEY-U! ( len ptr a idx -- ) {: len m:ptr ix :}
   len LEN>N 0 < if E-MAP-BAD-CAP throw then
   len LEN>N m ix MAP-SLOT-KEY-U-OFF >OFF MAP-SLOT-FIELD ! ;

: MAP-SLOT-VALUE@ ( ptr a idx -- a )
   MAP-SLOT-VALUE-OFF >OFF MAP-SLOT-FIELD @ ;

: MAP-SLOT-VALUE! ( a ptr a idx -- ) {: value m:ptr ix :}
   value m ix MAP-SLOT-VALUE-OFF >OFF MAP-SLOT-FIELD ! ;

: MAP-SLOT-CLEAR ( ptr a idx -- ) {: m:ptr ix :}
   0 m ix MAP-SLOT-HASH!
   NULL$ drop m ix MAP-SLOT-KEY-A!
   0 >LEN m ix MAP-SLOT-KEY-U!
   0 m ix MAP-SLOT-VALUE!
   SLOT--STATE:EMPTY m ix MAP-SLOT-STATE! ;

: MAP-CLEAR ( ptr a -- ) {: m:ptr :}
   m MAP-CAP@ dup MAP-CHECK-CAP {: cap :}
   0 m MAP-COUNT-OFF cells + !
   0 m MAP-DELETED-OFF cells + !
   cap COUNT>N 0 ?do
      m i >IDX MAP-SLOT-CLEAR
   loop ;

: MAP-INIT ( ptr a count -- ) {: m:ptr cap :}
   cap m MAP-CAP!
   m MAP-CLEAR ;

: MAP-HASH ( ptr u8 len -- n ) {: a:ptr u :}
   u MAP-CHECK-LEN
   MAP-HASH-SEED
   u LEN>N 0 ?do
      MAP-HASH-MUL * a i + c@ + MAP-HASH-MASK and
   loop ;

: MAP-INDEX ( n count -- idx ) {: hash cap :}
   cap MAP-CHECK-CAP
   hash cap COUNT>N mod dup 0 < if cap COUNT>N + then >IDX ;

: MAP-PROBE ( n count count -- idx ) {: hash step cap :}
   hash cap MAP-INDEX IDX>N
   step COUNT>N cap MAP-INDEX IDX>N
   {: base inc :}
   cap COUNT>N 1 - base - inc < if
      inc cap COUNT>N base - -
   else
      base inc +
   then >IDX ;

: MAP-SLOT-MATCH? ( ptr a idx n ptr u8 len -- bool ) {: m:ptr ix hash key:ptr len :}
   m ix MAP-SLOT-STATE@ MAP-OCCUPIED? 0= if 0 0= 0= exit then
   m ix MAP-SLOT-HASH@ hash <> if 0 0= 0= exit then
   m ix MAP-SLOT-KEY-A@ m ix MAP-SLOT-KEY-U@ LEN>N key len LEN>N STR= ;

: MAP-REMEMBER-FREE ( n idx -- n ) {: free ix :}
   free 0 < if ix IDX>N else free then ;

: MAP-LOCATE-SLOT ( n ptr a idx ptr u8 len n -- n n n ) {: free m:ptr ix key:ptr len hash :}
   m ix MAP-SLOT-STATE@ MATCH slot-state
     empty OF free ix MAP-REMEMBER-FREE dup MAP-LOC-FREE ENDOF
     deleted OF free ix MAP-REMEMBER-FREE -1 MAP-LOC-FULL ENDOF
     occupied OF
        m ix hash key len MAP-SLOT-MATCH? if
           free ix IDX>N MAP-LOC-FOUND
        else
           free -1 MAP-LOC-FULL
        then
     ENDOF
   ;MATCH ;

: MAP-LOCATE ( ptr a count ptr u8 len -- n n n ) {: m:ptr cap key:ptr len :}
   m cap MAP-CHECK-HANDLE
   len MAP-CHECK-LEN
   key len MAP-HASH {: hash :}
   -1
   cap COUNT>N 0 ?do
      m hash i >COUNT cap MAP-PROBE key len hash MAP-LOCATE-SLOT
      dup MAP-LOC-FULL <> if
         rot drop hash unloop exit
      then
      drop drop
   loop
   dup 0 < if drop -1 MAP-LOC-FULL hash else MAP-LOC-FREE hash then ;

: MAP-SLOT-INSERT ( a ptr a idx n ptr u8 len -- ) {: value m:ptr ix hash key:ptr len :}
   m ix MAP-SLOT-STATE@ MATCH slot-state
     empty OF ENDOF
     deleted OF m MAP-DELETED@ COUNT>N 1 - >COUNT m MAP-DELETED! ENDOF
     occupied OF E-MAP-FULL throw ENDOF
   ;MATCH
   m MAP-COUNT@ COUNT>N 1 + >COUNT m MAP-COUNT!
   hash m ix MAP-SLOT-HASH!
   key m ix MAP-SLOT-KEY-A!
   len m ix MAP-SLOT-KEY-U!
   value m ix MAP-SLOT-VALUE!
   SLOT--STATE:OCCUPIED m ix MAP-SLOT-STATE! ;

: MAP-GET ( ptr a count ptr u8 len -- option<n> ) {: m:ptr cap:count key:ptr len:len :}   \ SOME value if the key is present, else NONE
   m cap key len MAP-LOCATE {: ix:n loc:n hash:n :}
   loc MAP-LOC-FOUND = if
      m ix >IDX MAP-SLOT-VALUE@ OPTION:SOME
   else
      OPTION:NONE
   then ;

: MAP-HAS? ( ptr a count ptr u8 len -- bool )
   MAP-GET MATCH option
     none OF 0 0= 0= ENDOF
     some OF drop 0 0= ENDOF
   ;MATCH ;

: MAP-SET ( n ptr a count ptr u8 len -- ) {: value m:ptr cap key:ptr len :}
   m cap key len MAP-LOCATE {: ix loc hash :}
   loc MAP-LOC-FOUND = if
      value m ix >IDX MAP-SLOT-VALUE!
      exit
   then
   loc MAP-LOC-FREE <> if E-MAP-FULL throw then
   value m ix >IDX hash key len MAP-SLOT-INSERT ;

\ Visit occupied entries in ascending storage-slot order.
: MAP-EACH ( ptr a count [ ptr u8 len n -- ] -- ) {: m:ptr cap q :}
   m cap MAP-CHECK-HANDLE
   cap COUNT>N 0 ?do
      m i >IDX MAP-SLOT-STATE@ MAP-OCCUPIED? if
         m i >IDX MAP-SLOT-KEY-A@
         m i >IDX MAP-SLOT-KEY-U@
         m i >IDX MAP-SLOT-VALUE@
         q execute
      then
   loop ;
