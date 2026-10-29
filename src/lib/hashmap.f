\ hashmap.f - open-addressing integer-key hash probe (package HM) over
\ caller-owned keys[] and used[] cell arrays (0 = empty slot) of cap cells.
\ PROBE answers the slot a key lives in or the first empty one, and throws
\ E-HM-FULL after cap steps; cap is a nonzero power of two, else E-HM-CAP.
\ HASH64 mixes the whole key; folding a non-integer key is the caller's job.
\ STORAGE CLASS. CALLER-OWNED: distinct tables may be used by concurrent Tasks.

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

\ fmix64, murmur3's 64-bit finalizer, the more widely published one, so
\ hashmap-test.f pins known answers from the spec. It is a bijection whose every
\ output bit depends on every input bit; the multiplies must wrap to mix, and
\ Habu's `*` wraps.
: HASH64 ( n -- n ) XOR-FOLD FMIX-MUL1 *  XOR-FOLD FMIX-MUL2 *  XOR-FOLD ;

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

: CLEAR ( ptr n n -- )
   {: used:ptr cap:n :}
   cap CAP-OK
   cap 0 ?do 0 used i cells + ! loop ;

;package
