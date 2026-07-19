\ hashmap.f - open-addressing integer-key hash probe over caller-supplied arrays.
\ Like lib/sort.f SORT:SORT!, the map operates on arrays the caller owns: a keys[]
\ cell array and a used[] cell array (0 = empty slot), both of `cap` cells where cap
\ is a power of two. HM:PROBE returns the slot a key lives in, or the first empty slot
\ to insert it at (linear probing); the caller decides insert vs. found by testing
\ used[slot]. This turns O(n) key lookups into O(1) amortized. Core only.
\ cap MUST be a power of two and load kept < 1 (the probe assumes an empty slot
\ exists); sizing cap above the expected key count is the caller's responsibility.
\
\ The module lives in `package HM`. External callers use the qualified public API
\ (HM:HASH64, HM:PROBE, HM:CLEAR); the probe cursor state is package-private.

package HM

variable SLOT  variable DONE  variable IX     \ probe cursor / loop state (private)

public

\ splitmix-style mix; identity for small sequential keys (ideal for frame indices)
: HASH64 ( n -- n ) {: x:n :} x  x 33 rshift xor ;

\ slot where key already lives, or the first empty slot for insertion
: PROBE ( ptr a ptr a n n -- n ) {: keys:ptr used:ptr cap:n key:n :}
   key HASH64 cap 1- and SLOT !
   0 DONE !
   begin DONE @ 0= while
      used SLOT @ cells + @ 0= if -1 DONE ! else
         keys SLOT @ cells + @ key = if -1 DONE ! else
            SLOT @ 1+ cap 1- and SLOT !
         then
      then
   repeat
   SLOT @ ;

\ zero a used[] array (mark all slots empty)
: CLEAR ( ptr a n -- ) {: used:ptr cap:n :}
   0 IX !
   begin IX @ cap < while  0 used IX @ cells + !  IX @ 1+ IX !  repeat ;

;package
