\ property.f - checked helpers for property-based tests in `package PROP`.
\ The PRNG is a deterministic LCG, so a seed reproduces a run.
\ A property that runs generated checked source owns its own audited `evaluate`
\ boundary; generators stay checked.
\ STORAGE CLASS. PROCESS-WIDE: seed, count and the source buffer are static;
\ one task runs a property at a time.

require src/lib/error.f

package PROP

public

1 constant DEFAULT-SEED
250 constant DEFAULT-COUNT
50000 constant MAX-COUNT

private

$7FFFFFFF constant PROP-SEED-MASK
1103515245 constant PROP-LCG-A
12345 constant PROP-LCG-C

public

4096 constant BUF-CAP

private

255 constant PROP-BYTE-MAX
48 constant PROP-ZERO

variable PROP-SEED
variable PROP-COUNT

create PROP-BUF BUF-CAP allot
variable PROP-BUF-LEN

: PROP-CHECK-SEED ( n -- ) {: seed :}
   seed 0 < if E-PROP-SEED throw then
   seed PROP-SEED-MASK > if E-PROP-SEED throw then ;

: PROP-CHECK-COUNT ( n -- ) {: count :}
   count 0 < if E-PROP-SEED throw then
   count MAX-COUNT > if E-PROP-SEED throw then ;

public

: DEFAULTS ( -- n n )
   DEFAULT-SEED DEFAULT-COUNT ;

: RUN-RESET ( n n -- ) {: seed:n count:n :}
   seed PROP-CHECK-SEED
   count PROP-CHECK-COUNT
   seed PROP-SEED !
   count PROP-COUNT ! ;

: SEED! ( n -- )
   dup PROP-CHECK-SEED
   PROP-SEED ! ;

: SEED@ ( -- n )
   PROP-SEED @ ;

: COUNT@ ( -- n )
   PROP-COUNT @ ;

: RND ( -- n )
   PROP-SEED @ PROP-LCG-A * PROP-LCG-C + PROP-SEED-MASK and
   dup PROP-SEED ! ;

: RND% ( n -- n ) {: bound:n :}
   bound 0 <= if E-PROP-GENERATOR throw then
   \ Fold the high bits into the short-cycling low bits before bounding.
   \ This permutation keeps the full 31-bit range and the seed sequence.
   RND dup 16 rshift xor bound mod ;

: BUF-CHECK-ROOM ( n -- ) {: add:n :}
   add 0 < if E-PROP-CAPACITY throw then
   add BUF-CAP PROP-BUF-LEN @ - > if E-PROP-CAPACITY throw then ;

: BUF-RESET ( -- )
   0 PROP-BUF-LEN ! ;

: BUF+ ( ptr u8 n -- ) {: a:ptr u:n :}
   u BUF-CHECK-ROOM
   0 begin dup u < while
      dup a + c@ PROP-BUF PROP-BUF-LEN @ + c!
      PROP-BUF-LEN @ 1+ PROP-BUF-LEN !
      1+
   repeat drop ;

: BUF-C+ ( n -- ) {: c:n :}
   c 0 < if E-PROP-GENERATOR throw then
   c PROP-BYTE-MAX > if E-PROP-GENERATOR throw then
   1 BUF-CHECK-ROOM
   c PROP-BUF PROP-BUF-LEN @ + c!
   PROP-BUF-LEN @ 1+ PROP-BUF-LEN ! ;

: DIGIT+ ( n -- ) {: digit:n :}
   digit 0 < if E-PROP-GENERATOR throw then
   digit 9 > if E-PROP-GENERATOR throw then
   digit PROP-ZERO + BUF-C+ ;

: BUF$ ( -- ptr u8 n )
   PROP-BUF PROP-BUF-LEN @ ;

;package
