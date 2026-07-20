\ property.f - checked helpers for property-based tests.
\
\ The module lives in `package PROP`. External callers use the qualified public
\ property-testing DSL (PROP:RUN-RESET, PROP:SEED!/SEED@, PROP:COUNT@, PROP:RND/
\ RND%, PROP:BUF-RESET/BUF+/BUF-C+/BUF$/BUF-CHECK-ROOM, PROP:DIGIT+, PROP:GEN-START/
\ GEN-STEP/GEN-DEPTH@, PROP:TRIM-TRAIL, PROP:DROP-LAST, PROP:SHRINK, PROP:DEFAULTS
\ and the PROP:DEFAULT-SEED/DEFAULT-COUNT/MAX-COUNT/BUF-CAP constants); the LCG
\ constants, PRNG/buffer state cells, and seed/count validators are package-private.

require lib/errors.f

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
32 constant PROP-SPACE
48 constant PROP-ZERO

variable PROP-SEED
variable PROP-COUNT
variable PROP-GEN-DEPTH
variable PROP-GEN-NEXT
\ the shrink predicate is stored and re-run across the shrink loop; a typed xt
\ cell keeps its ( -- bool ) effect through store+fetch so execute stays checked
\ (an untyped variable would launder it -> E-EXEC-OPAQUE-XT).
TYPED-VARIABLE PROP-SHRINK-PRED [ -- bool ]
variable PROP-SHRINK-SAVE

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
   RND bound mod ;

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

: GEN-DEPTH@ ( -- n )
   PROP-GEN-DEPTH @ ;

: GEN-START ( n -- ) {: depth:n :}
   depth 0 < if E-PROP-GENERATOR throw then
   BUF-RESET
   depth PROP-GEN-DEPTH ! ;

: GEN-STEP ( ptr u8 n n n -- ) {: a:ptr u:n need:n delta:n :}
   need 0 < if E-PROP-GENERATOR throw then
   PROP-GEN-DEPTH @ need < if E-PROP-GENERATOR throw then
   PROP-GEN-DEPTH @ delta + PROP-GEN-NEXT !
   PROP-GEN-NEXT @ 0 < if E-PROP-GENERATOR throw then
   u BUF-CHECK-ROOM
   a u BUF+
   PROP-GEN-NEXT @ PROP-GEN-DEPTH ! ;

: TRIM-TRAIL ( -- )
   begin PROP-BUF-LEN @ 0 > while
      PROP-BUF PROP-BUF-LEN @ 1- + c@ PROP-SPACE = if
         PROP-BUF-LEN @ 1- PROP-BUF-LEN !
      else
         exit
      then
   repeat ;

: DROP-LAST ( -- bool )
   TRIM-TRAIL
   PROP-BUF-LEN @ 0= if 0 0= 0= exit then
   begin PROP-BUF-LEN @ 0 > while
      PROP-BUF PROP-BUF-LEN @ 1- + c@ PROP-SPACE <> if
         PROP-BUF-LEN @ 1- PROP-BUF-LEN !
      else
         0 0= exit
      then
   repeat
   0 0= ;

: SHRINK ( [ -- bool ] -- )
   PROP-SHRINK-PRED !
   PROP-SHRINK-PRED @ execute 0= if E-PROP-SHRINK throw then
   begin
      PROP-BUF-LEN @ PROP-SHRINK-SAVE !
      DROP-LAST if
         PROP-SHRINK-PRED @ execute if
            0 0=
         else
            PROP-SHRINK-SAVE @ PROP-BUF-LEN !
            0 0= 0=
         then
      else
         0 0= 0=
      then
   while repeat ;

;package
