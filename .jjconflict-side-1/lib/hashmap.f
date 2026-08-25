\ hashmap.f - open-addressing integer-key hash probe over caller-supplied arrays.
\ Like lib/sort.f SORT:SORT!, the map operates on arrays the caller owns: a keys[]
\ cell array and a used[] cell array (0 = empty slot), both of `cap` cells where cap
\ is a power of two. HM:PROBE returns the slot a key lives in, or the first empty slot
\ to insert it at (linear probing); the caller decides insert vs. found by testing
\ used[slot]. This turns O(n) key lookups into O(1) amortized. Core only.
\ cap MUST be a power of two and load kept < 1 (the probe assumes an empty slot
\ exists); sizing cap above the expected key count is the caller's responsibility.
\ Both invariants are enforced, not merely documented: every entry that takes cap
\ throws E-HM-CAP when cap is not a nonzero power of two (a non-power-of-two mask
\ would probe a subset of slots and cap=0 makes the mask an identity that returns
\ an out-of-bounds slot), and PROBE bounds its scan at cap steps, throwing E-HM-FULL
\ on a full table rather than looping forever on an absent key.
\
\ The module lives in `package HM`. External callers use the qualified public API
\ (HM:HASH64, HM:PROBE, HM:CLEAR); the probe cursor state is package-private.

require lib/errors.f

package HM

variable SLOT  variable DONE  variable IX  variable TRIES  \ probe cursor / loop state (private)

\ cap must be a nonzero power of two: the probe masks the slot index with (cap-1),
\ so cap-1 must be an all-ones bit run. Structural check, not a value range.
: CAP-OK ( n -- ) {: cap:n :}
   cap 0 <= if E-HM-CAP throw then
   cap  cap 1- and  0= 0= if E-HM-CAP throw then ;   \ (cap & (cap-1)) nonzero => not a power of two

public

\ splitmix-style mix; identity for small sequential keys (ideal for frame indices)
: HASH64 ( n -- n ) {: x:n :} x  x 33 rshift xor ;

\ slot where key already lives, or the first empty slot for insertion
: PROBE ( ptr a ptr a n n -- n ) {: keys:ptr used:ptr cap:n key:n :}
   cap CAP-OK
   key HASH64 cap 1- and SLOT !
   0 DONE !  0 TRIES !
   begin DONE @ 0= while
      TRIES @ cap >= if E-HM-FULL throw then         \ every slot scanned, none empty/matching
      used SLOT @ cells + @ 0= if -1 DONE ! else
         keys SLOT @ cells + @ key = if -1 DONE ! else
            SLOT @ 1+ cap 1- and SLOT !
            TRIES @ 1+ TRIES !
         then
      then
   repeat
   SLOT @ ;

\ zero a used[] array (mark all slots empty)
: CLEAR ( ptr a n -- ) {: used:ptr cap:n :}
   cap CAP-OK
   0 IX !
   begin IX @ cap < while  0 used IX @ cells + !  IX @ 1+ IX !  repeat ;

;package
