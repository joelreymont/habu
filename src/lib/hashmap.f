\ hashmap.f - open-addressing integer-key hash probe over caller-supplied arrays.
\ Like src/lib/sort.f SORT:SORT!, the map operates on arrays the caller owns: a keys[]
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
\ Keys go in as they are. HASH64 mixes the whole 64-bit key before PROBE masks
\ it, so no caller has to pre-mix, pre-fold or spread a key to get a usable slot
\ distribution; a fold that turns a non-integer key (a string, a byte range)
\ into one cell is a separate job and stays the caller's.
\
\ Storage class: caller-owned. PROBE and CLEAR keep traversal state in each
\ invocation; distinct caller-owned tables may be used by concurrent Tasks.
\ The module lives in `package HM`. External callers use the qualified public API
\ (HM:HASH64, HM:PROBE, HM:CLEAR).

require src/lib/errors.f

package HM

\ cap must be a nonzero power of two: the probe masks the slot index with (cap-1),
\ so cap-1 must be an all-ones bit run. Structural check, not a value range.
: CAP-OK ( n -- ) {: cap:n :}
   cap 0 <= if E-HM-CAP throw then
   cap  cap 1- and  0= 0= if E-HM-CAP throw then ;   \ (cap & (cap-1)) nonzero => not a power of two

\ fmix64's two multipliers, in the spec's hex spelling.
$FF51AFD7ED558CCD constant FMIX-MUL1
$C4CEB9FE1A85EC53 constant FMIX-MUL2

\ fmix64's xor-shift step: fold the high half of the cell down onto the low half,
\ which is what carries a high key's entropy into the bits PROBE masks.
: XOR-FOLD ( n -- n ) {: x:n :} x  x 33 rshift xor ;

public

\ fmix64, murmur3's 64-bit finalizer (splitmix64's finalizer is as good; this one
\ is the more widely published, so HM-RUN can pin known answers recomputed from
\ the spec). It is a bijection whose every output bit depends on every input bit,
\ so the low bits PROBE masks carry the whole key, not a fragment of it. The
\ multiplies must wrap to mix, and Habu's `*` wraps.
: HASH64 ( n -- n ) XOR-FOLD FMIX-MUL1 *  XOR-FOLD FMIX-MUL2 *  XOR-FOLD ;

\ slot where key already lives, or the first empty slot for insertion
: PROBE ( ptr n ptr n n n -- n )
   {: keys:ptr used:ptr cap:n key:n :}
   cap CAP-OK
   key HASH64 cap 1- and
   cap 0 ?do
      dup cells used + @ 0= if unloop exit then
      dup cells keys + @ key = if unloop exit then
      1+ cap 1- and
   loop
   drop E-HM-FULL throw ;

\ zero a used[] array (mark all slots empty)
: CLEAR ( ptr n n -- )
   {: used:ptr cap:n :}
   cap CAP-OK
   cap 0 ?do 0 used i cells + ! loop ;

;package
