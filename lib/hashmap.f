\ hashmap.f - open-addressing integer-key hash probe over caller-supplied arrays.
\ Like lib/sort.f SORT!, the map operates on arrays the caller owns: a keys[] cell
\ array and a used[] cell array (0 = empty slot), both of `cap` cells where cap is a
\ power of two. HM-PROBE returns the slot a key lives in, or the first empty slot to
\ insert it at (linear probing); the caller decides insert vs. found by testing
\ used[slot]. This turns O(n) key lookups into O(1) amortized. Core only.
\ cap MUST be a power of two and load kept < 1 (the probe assumes an empty slot
\ exists); sizing cap above the expected key count is the caller's responsibility.

variable HM-SLOT  variable HM-DONE  variable HM-I

\ splitmix-style mix; identity for small sequential keys (ideal for frame indices)
: HASH64 ( n -- n ) {: x :} x  x 33 rshift xor ;

\ slot where key already lives, or the first empty slot for insertion
: HM-PROBE ( ptr a ptr a n n -- n ) {: keys:ptr used:ptr cap key :}
   key HASH64 cap 1- and HM-SLOT !
   0 HM-DONE !
   begin HM-DONE @ 0= while
      used HM-SLOT @ cells + @ 0= if -1 HM-DONE ! else
         keys HM-SLOT @ cells + @ key = if -1 HM-DONE ! else
            HM-SLOT @ 1+ cap 1- and HM-SLOT !
         then
      then
   repeat
   HM-SLOT @ ;

\ zero a used[] array (mark all slots empty)
: HM-CLEAR ( ptr a n -- ) {: used:ptr cap :}
   0 HM-I !
   begin HM-I @ cap < while  0 used HM-I @ cells + !  HM-I @ 1+ HM-I !  repeat ;
