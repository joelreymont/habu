\ map.f - fixed-capacity open-addressed string-key map layout.

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

0 constant MAP-EMPTY
-1 constant MAP-DELETED
1 constant MAP-OCCUPIED

0 constant MAP-LOC-FULL
1 constant MAP-LOC-FREE
2 constant MAP-LOC-FOUND

5381 constant MAP-HASH-SEED
33 constant MAP-HASH-MUL
$7FFFFFFFFFFFFFFF constant MAP-HASH-MASK

: MAP-CHECK-CAP ( n -- ) {: cap :}
   cap 0 <= if E-MAP-BAD-CAP throw then ;

: MAP-CHECK-LEN ( n -- ) {: len :}
   len 0 < if E-MAP-BAD-CAP throw then ;

: MAP-CELLS ( n -- n ) {: cap :}
   cap MAP-CHECK-CAP
   MAP-HEADER-CELLS cap MAP-SLOT-CELLS * + ;

: MAP-EMPTY? ( n -- bool )
   MAP-EMPTY = ;

: MAP-DELETED? ( n -- bool )
   MAP-DELETED = ;

: MAP-OCCUPIED? ( n -- bool )
   MAP-OCCUPIED = ;

: MAP-CAP@ ( ptr a -- n )
   MAP-CAP-OFF cells + @ ;

: MAP-CAP! ( n ptr a -- ) {: cap m:ptr :}
   cap MAP-CHECK-CAP
   cap m MAP-CAP-OFF cells + ! ;

: MAP-CHECK-HANDLE ( ptr a n -- ) {: m:ptr cap :}
   cap MAP-CHECK-CAP
   m MAP-CAP@ cap <> if E-MAP-BAD-CAP throw then ;

: MAP-COUNT@ ( ptr a -- n )
   MAP-COUNT-OFF cells + @ ;

: MAP-DELETED@ ( ptr a -- n )
   MAP-DELETED-OFF cells + @ ;

: MAP-COUNT! ( n ptr a -- ) {: count m:ptr :}
   count 0 < if E-MAP-BAD-CAP throw then
   count m MAP-CAP@ m MAP-DELETED@ - > if E-MAP-FULL throw then
   count m MAP-COUNT-OFF cells + ! ;

: MAP-DELETED! ( n ptr a -- ) {: deleted m:ptr :}
   deleted 0 < if E-MAP-BAD-CAP throw then
   deleted m MAP-CAP@ m MAP-COUNT@ - > if E-MAP-FULL throw then
   deleted m MAP-DELETED-OFF cells + ! ;

: MAP-SLOTS ( ptr a -- ptr a )
   MAP-HEADER-CELLS cells + ;

: MAP-CHECK-INDEX ( ptr a n -- ) {: m:ptr ix :}
   ix 0 < if E-MAP-BAD-CAP throw then
   ix m MAP-CAP@ >= if E-MAP-BAD-CAP throw then ;

: MAP-SLOT ( ptr a n -- ptr a ) {: m:ptr ix :}
   m ix MAP-CHECK-INDEX
   m MAP-SLOTS ix MAP-SLOT-CELLS * cells + ;

: MAP-SLOT-FIELD ( ptr a n n -- ptr a ) {: m:ptr ix off :}
   off 0 < if E-MAP-BAD-CAP throw then
   off MAP-SLOT-CELLS >= if E-MAP-BAD-CAP throw then
   m ix MAP-SLOT off cells + ;

: MAP-SLOT-STATE@ ( ptr a n -- n )
   MAP-SLOT-STATE-OFF MAP-SLOT-FIELD @ ;

: MAP-SLOT-STATE! ( n ptr a n -- ) {: state m:ptr ix :}
   state MAP-EMPTY? state MAP-DELETED? or state MAP-OCCUPIED? or 0= if
      E-MAP-BAD-CAP throw
   then
   state m ix MAP-SLOT-STATE-OFF MAP-SLOT-FIELD ! ;

: MAP-SLOT-HASH@ ( ptr a n -- n )
   MAP-SLOT-HASH-OFF MAP-SLOT-FIELD @ ;

: MAP-SLOT-HASH! ( n ptr a n -- ) {: hash m:ptr ix :}
   hash 0 < if E-MAP-BAD-CAP throw then
   hash m ix MAP-SLOT-HASH-OFF MAP-SLOT-FIELD ! ;

: MAP-SLOT-KEY-A@ ( ptr a n -- ptr u8 )
   MAP-SLOT-KEY-A-OFF MAP-SLOT-FIELD @ ;

: MAP-SLOT-KEY-A! ( ptr u8 ptr a n -- ) {: key:ptr m:ptr ix :}
   key m ix MAP-SLOT-KEY-A-OFF MAP-SLOT-FIELD ! ;

: MAP-SLOT-KEY-U@ ( ptr a n -- n )
   MAP-SLOT-KEY-U-OFF MAP-SLOT-FIELD @ ;

: MAP-SLOT-KEY-U! ( n ptr a n -- ) {: len m:ptr ix :}
   len 0 < if E-MAP-BAD-CAP throw then
   len m ix MAP-SLOT-KEY-U-OFF MAP-SLOT-FIELD ! ;

: MAP-SLOT-VALUE@ ( ptr a n -- a )
   MAP-SLOT-VALUE-OFF MAP-SLOT-FIELD @ ;

: MAP-SLOT-VALUE! ( a ptr a n -- ) {: value m:ptr ix :}
   value m ix MAP-SLOT-VALUE-OFF MAP-SLOT-FIELD ! ;

: MAP-SLOT-CLEAR ( ptr a n -- ) {: m:ptr ix :}
   0 m ix MAP-SLOT-HASH!
   NULL$ drop m ix MAP-SLOT-KEY-A!
   0 m ix MAP-SLOT-KEY-U!
   0 m ix MAP-SLOT-VALUE!
   MAP-EMPTY m ix MAP-SLOT-STATE! ;

: MAP-CLEAR ( ptr a -- ) {: m:ptr :}
   m MAP-CAP@ dup MAP-CHECK-CAP {: cap :}
   0 m MAP-COUNT-OFF cells + !
   0 m MAP-DELETED-OFF cells + !
   cap 0 ?do
      m i MAP-SLOT-CLEAR
   loop ;

: MAP-INIT ( ptr a n -- ) {: m:ptr cap :}
   cap m MAP-CAP!
   m MAP-CLEAR ;

: MAP-HASH ( ptr u8 n -- n ) {: a:ptr u :}
   u MAP-CHECK-LEN
   MAP-HASH-SEED
   u 0 ?do
      MAP-HASH-MUL * a i + c@ + MAP-HASH-MASK and
   loop ;

: MAP-INDEX ( n n -- n ) {: hash cap :}
   cap MAP-CHECK-CAP
   hash cap mod dup 0 < if cap + then ;

: MAP-PROBE ( n n n -- n ) {: hash step cap :}
   hash cap MAP-INDEX
   step cap MAP-INDEX
   {: base inc :}
   cap 1 - base - inc < if
      inc cap base - -
   else
      base inc +
   then ;

: MAP-SLOT-MATCH? ( ptr a n n ptr u8 n -- bool ) {: m:ptr ix hash key:ptr len :}
   m ix MAP-SLOT-STATE@ MAP-OCCUPIED? 0= if 0 0= 0= exit then
   m ix MAP-SLOT-HASH@ hash <> if 0 0= 0= exit then
   m ix MAP-SLOT-KEY-A@ m ix MAP-SLOT-KEY-U@ key len STR= ;

: MAP-REMEMBER-FREE ( n n -- n ) {: free ix :}
   free 0 < if ix else free then ;

: MAP-LOCATE-SLOT ( n ptr a n ptr u8 n n -- n n n ) {: free m:ptr ix key:ptr len hash :}
   m ix MAP-SLOT-STATE@ {: state :}
   state MAP-EMPTY? if free ix MAP-REMEMBER-FREE dup MAP-LOC-FREE exit then
   state MAP-DELETED? if free ix MAP-REMEMBER-FREE -1 MAP-LOC-FULL exit then
   m ix hash key len MAP-SLOT-MATCH? if free ix MAP-LOC-FOUND exit then
   free -1 MAP-LOC-FULL ;

: MAP-LOCATE ( ptr a n ptr u8 n -- n n n ) {: m:ptr cap key:ptr len :}
   m cap MAP-CHECK-HANDLE
   len MAP-CHECK-LEN
   key len MAP-HASH {: hash :}
   -1
   cap 0 ?do
      m hash i cap MAP-PROBE key len hash MAP-LOCATE-SLOT
      dup MAP-LOC-FULL <> if
         rot drop hash unloop exit
      then
      drop drop
   loop
   dup 0 < if drop -1 MAP-LOC-FULL hash else MAP-LOC-FREE hash then ;

: MAP-SLOT-INSERT ( a ptr a n n ptr u8 n -- ) {: value m:ptr ix hash key:ptr len :}
   m ix MAP-SLOT-STATE@ {: state :}
   state MAP-OCCUPIED? if E-MAP-FULL throw then
   state MAP-DELETED? if m MAP-DELETED@ 1 - m MAP-DELETED! then
   m MAP-COUNT@ 1 + m MAP-COUNT!
   hash m ix MAP-SLOT-HASH!
   key m ix MAP-SLOT-KEY-A!
   len m ix MAP-SLOT-KEY-U!
   value m ix MAP-SLOT-VALUE!
   MAP-OCCUPIED m ix MAP-SLOT-STATE! ;

: MAP-GET ( ptr a n ptr u8 n -- n bool ) {: m:ptr cap key:ptr len :}
   m cap key len MAP-LOCATE {: ix loc hash :}
   loc MAP-LOC-FOUND = if
      m ix MAP-SLOT-VALUE@ 0 0=
   else
      0 0 0= 0=
   then ;

: MAP-HAS? ( ptr a n ptr u8 n -- bool )
   MAP-GET nip ;

: MAP-SET ( n ptr a n ptr u8 n -- ) {: value m:ptr cap key:ptr len :}
   m cap key len MAP-LOCATE {: ix loc hash :}
   loc MAP-LOC-FOUND = if
      value m ix MAP-SLOT-VALUE!
      exit
   then
   loc MAP-LOC-FREE <> if E-MAP-FULL throw then
   value m ix hash key len MAP-SLOT-INSERT ;

\ Visit occupied entries in ascending storage-slot order.
: MAP-EACH ( ptr a n [ ptr u8 n n -- ] -- ) {: m:ptr cap q :}
   m cap MAP-CHECK-HANDLE
   cap 0 ?do
      m i MAP-SLOT-STATE@ MAP-OCCUPIED? if
         m i MAP-SLOT-KEY-A@
         m i MAP-SLOT-KEY-U@
         m i MAP-SLOT-VALUE@
         q execute
      then
   loop ;
