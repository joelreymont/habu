\ Pure image DATA codec. Storage and image framing belong to the callers.
\ A presence bit names each 64-byte group of the flat cell bitmap; stored
\ groups follow in order, then one unsigned LEB128 per present cell. The last
\ stored group is zero-padded. No process state or fixed buffer cap lives here.
package IMAGE-CELLS
public

8 constant CELL-BYTES
8 constant CELL-BITS
CELL-BYTES CELL-BITS * constant BM-BYTE-SPAN
64 constant GROUP-BYTES
GROUP-BYTES CELL-BITS * constant GROUP-CELLS
GROUP-BYTES BM-BYTE-SPAN * constant GROUP-SPAN
10 constant VMAX

: PMAP-BYTES ( n -- n ) CELL-BITS 1- + CELL-BITS / ;

: BIT? ( ptr u8 n -- bool ) {: map:ptr bit:n :}
   map bit CELL-BITS / + c@ bit CELL-BITS mod rshift 1 and 0<> ;

: BIT! ( ptr u8 n -- ) {: map:ptr bit:n :}
   map bit CELL-BITS / + {: p:ptr :}
   p c@ 1 bit CELL-BITS mod lshift or p c! ;

\ The caller supplies room for ceil(groups/8) + groups*GROUP-BYTES bytes.
\ Return the group count and stored bitmap bytes; the map length is derived.
\ The flat bitmap ends at its last nonzero byte, or is empty for all-zero DATA.
: BM! ( ptr u8 n ptr u8 -- n n ) {: bm:ptr len:n buf:ptr :}
   len GROUP-BYTES 1- + GROUP-BYTES / {: groups:n :}
   groups PMAP-BYTES {: pm:n :}
   pm 0 ?do 0 buf i + c! loop
   len 0 ?do bm i + c@ 0<> if buf i GROUP-BYTES / BIT! then loop
   0 groups GROUP-BYTES * 0 ?do
      buf i GROUP-BYTES / BIT? if
         {: stored:n :}
         i len < if bm i + c@ else 0 then
         buf pm + stored + c!
         stored 1+
      then
   loop
   groups swap ;

: CELL-VLEN ( n -- n ) {: v:n :}
   VMAX 1 ?do
      v i 7 * rshift 0= if i unloop exit then
   loop
   VMAX ;

: CELL-V! ( n ptr u8 -- n ) {: v:n p:ptr :}
   v CELL-VLEN {: w:n :}
   w 0 ?do
      v i 7 * rshift $7F and {: group:n :}
      i 1+ w < if group $80 or else group then p i + c!
   loop
   w ;

private
: CELL-VW@ ( ptr u8 n -- n ) {: p:ptr avail:n :}
   avail 0 <= if 0 exit then
   avail VMAX min 0 ?do
      p i + c@ $80 and 0= if
         i 1+ {: w:n :}
         w 1 > if p w 1- + c@ 0= if 0 unloop exit then then
         w unloop exit
      then
   loop
   0 ;

: CELL-VV@ ( ptr u8 n -- n ) {: p:ptr w:n :}
   0 w 0 ?do p i + c@ $7F and i 7 * lshift or loop ;

public
\ Width zero refuses a truncated, overflowing or nonminimal encoding. Never
\ read beyond avail, including when only a one-byte value is available.
: CELL-V@ ( ptr u8 n -- n n ) {: p:ptr avail:n :}
   p avail CELL-VW@ {: w:n :}
   w 0= if 0 0 exit then
   w VMAX = if p VMAX 1- + c@ 1 > if 0 0 exit then then
   p w CELL-VV@ w ;

\ A final partial cell has zero high bytes; a complete cell uses one load.
: CELL@ ( ptr u8 n -- n ) {: p:ptr avail:n :}
   avail CELL-BYTES >= if p CELL-VIEW @ exit then
   0 avail 0 ?do p i + c@ i 8 * lshift or loop ;

\ Room for the flat bitmap is ceil(len / BM-BYTE-SPAN). Answer its used
\ length and the exact value-stream length; all-zero tails have no map bits.
: BITMAP! ( ptr u8 n ptr u8 -- n n ) {: src:ptr len:n map:ptr :}
   len BM-BYTE-SPAN 1- + BM-BYTE-SPAN / 0 ?do 0 map i + c! loop
   0 0 len CELL-BYTES 1- + CELL-BYTES / 0 ?do
      src i cells + len i cells - CELL@ {: v:n :}
      v 0<> if
         {: used:n values:n :}
         map i BIT!
         i CELL-BITS / 1+ values v CELL-VLEN +
      then
   loop ;

\ The value output is sized from BITMAP!'s exact second result.
: VALUES! ( ptr u8 n ptr u8 -- n ) {: src:ptr len:n dst:ptr :}
   0 len CELL-BYTES 1- + CELL-BYTES / 0 ?do
      src i cells + len i cells - CELL@ {: v:n :}
      v 0<> if {: used:n :} v dst used + CELL-V! used + then
   loop ;

: U32@ ( ptr u8 -- n ) {: p:ptr :}
   0 4 0 ?do p i + c@ i 8 * lshift or loop ;

: U32! ( n ptr u8 -- ) {: v:n p:ptr :}
   4 0 ?do v i 8 * rshift p i + c! loop ;

private
\ Store only the decoded extent, and refuse a partial cell with high bits.
: CELL! ( n ptr u8 n -- bool ) {: v:n p:ptr avail:n :}
   avail CELL-BYTES >= if v p CELL-VIEW ! true exit then
   v avail 8 * rshift 0<> if false exit then
   avail 0 ?do v i 8 * rshift p i + c! loop true ;

: GROUP-NONZERO? ( ptr u8 -- bool ) {: p:ptr :}
   GROUP-BYTES 0 ?do p i + c@ 0<> if true unloop exit then loop false ;

\ Decode a stored bitmap group into scratch; -1 means malformed input.
: GROUP@ ( ptr u8 ptr u8 n ptr u8 n -- n )
   {: bm:ptr values:ptr avail:n dst:ptr room:n :}
   bm GROUP-NONZERO? 0= if -1 exit then
   0 GROUP-CELLS 0 ?do
      bm i BIT? if
         {: used:n :}
         i cells room >= if -1 unloop exit then
         values used + avail used - CELL-V@ {: v:n w:n :}
         w 0= v 0= or if -1 unloop exit then
         v dst i cells + room i cells - CELL! 0= if -1 unloop exit then
         used w +
      then
   loop ;

public
\ Read [G:u32][S:u32][presence map][S bitmap bytes][values]. Callers bound
\ room to their DATA extent and supply zeroed scratch, never live storage:
\ a refusal may have written earlier cells. Return bytes consumed, or zero
\ for any malformed header, map, cell or varint. Outer padding is separate.
: READ ( ptr u8 n ptr u8 n -- n ) {: src:ptr size:n dst:ptr room:n :}
   size 8 < room 0 < or if 0 exit then
   src U32@ {: groups:n :}
   src 4 + U32@ {: stored:n :}
   room GROUP-SPAN / room GROUP-SPAN mod 0<> if 1+ then
      groups < if 0 exit then
   stored GROUP-BYTES mod 0<> if 0 exit then
   groups PMAP-BYTES {: pm:n :}
   pm size 8 - > if 0 exit then
   stored size 8 - pm - > if 0 exit then
   src 8 + {: map:ptr :}
   groups 0<> if
      map groups 1- BIT? 0= if 0 exit then
      groups CELL-BITS mod {: tail:n :}
      tail 0<> if map pm + 1- c@ tail rshift 0<> if 0 exit then then
   then
   map pm + {: bm:ptr :}
   bm stored + {: values:ptr :}
   size 8 - pm - stored - {: avail:n :}
   0 0 groups 0 ?do
      {: b:n v:n :}
      map i BIT? if
         stored b - GROUP-BYTES < if 0 unloop exit then
         bm b + values v + avail v -
            dst i GROUP-SPAN * + room i GROUP-SPAN * - GROUP@ {: w:n :}
         w 0 < if 0 unloop exit then
         b GROUP-BYTES + v w +
      else b v then
   loop
   {: b:n v:n :}
   b stored <> if 0 exit then
   8 pm + stored + v + ;

;package
