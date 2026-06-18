\ property.f - checked helpers for property-based tests.

1 constant PROP-DEFAULT-SEED
250 constant PROP-DEFAULT-COUNT
50000 constant PROP-MAX-COUNT

$7FFFFFFF constant PROP-SEED-MASK
1103515245 constant PROP-LCG-A
12345 constant PROP-LCG-C

4096 constant PROP-BUF-CAP
255 constant PROP-BYTE-MAX
32 constant PROP-SPACE
48 constant PROP-ZERO

variable PROP-SEED
variable PROP-COUNT
variable PROP-GEN-DEPTH
variable PROP-GEN-NEXT
variable PROP-SHRINK-PRED
variable PROP-SHRINK-SAVE

create PROP-BUF PROP-BUF-CAP allot
variable PROP-BUF-LEN

: PROP-CHECK-SEED ( n -- ) {: seed :}
   seed 0 < if E-PROP-SEED throw then
   seed PROP-SEED-MASK > if E-PROP-SEED throw then ;

: PROP-CHECK-COUNT ( n -- ) {: count :}
   count 0 < if E-PROP-SEED throw then
   count PROP-MAX-COUNT > if E-PROP-SEED throw then ;

: PROP-DEFAULTS ( -- n n )
   PROP-DEFAULT-SEED PROP-DEFAULT-COUNT ;

: PROP-RUN-RESET ( n n -- ) {: seed count :}
   seed PROP-CHECK-SEED
   count PROP-CHECK-COUNT
   seed PROP-SEED !
   count PROP-COUNT ! ;

: PROP-SEED! ( n -- )
   dup PROP-CHECK-SEED
   PROP-SEED ! ;

: PROP-SEED@ ( -- n )
   PROP-SEED @ ;

: PROP-COUNT@ ( -- n )
   PROP-COUNT @ ;

: PROP-RND ( -- n )
   PROP-SEED @ PROP-LCG-A * PROP-LCG-C + PROP-SEED-MASK and
   dup PROP-SEED ! ;

: PROP-RND% ( n -- n ) {: bound :}
   bound 0 <= if E-PROP-GENERATOR throw then
   PROP-RND bound mod ;

: PROP-BUF-CHECK-ROOM ( n -- ) {: add :}
   add 0 < if E-PROP-CAPACITY throw then
   add PROP-BUF-CAP PROP-BUF-LEN @ - > if E-PROP-CAPACITY throw then ;

: PROP-BUF-RESET ( -- )
   0 PROP-BUF-LEN ! ;

: PROP-BUF+ ( ptr u8 n -- ) {: a:ptr u :}
   u PROP-BUF-CHECK-ROOM
   0 begin dup u < while
      dup a + c@ PROP-BUF PROP-BUF-LEN @ + c!
      PROP-BUF-LEN @ 1+ PROP-BUF-LEN !
      1+
   repeat drop ;

: PROP-BUF-C+ ( n -- ) {: c :}
   c 0 < if E-PROP-GENERATOR throw then
   c PROP-BYTE-MAX > if E-PROP-GENERATOR throw then
   1 PROP-BUF-CHECK-ROOM
   c PROP-BUF PROP-BUF-LEN @ + c!
   PROP-BUF-LEN @ 1+ PROP-BUF-LEN ! ;

: PROP-DIGIT+ ( n -- ) {: digit :}
   digit 0 < if E-PROP-GENERATOR throw then
   digit 9 > if E-PROP-GENERATOR throw then
   digit PROP-ZERO + PROP-BUF-C+ ;

: PROP-BUF$ ( -- ptr u8 n )
   PROP-BUF PROP-BUF-LEN @ ;

: PROP-GEN-DEPTH@ ( -- n )
   PROP-GEN-DEPTH @ ;

: PROP-GEN-START ( n -- ) {: depth :}
   depth 0 < if E-PROP-GENERATOR throw then
   PROP-BUF-RESET
   depth PROP-GEN-DEPTH ! ;

: PROP-GEN-STEP ( ptr u8 n n n -- ) {: a:ptr u need delta :}
   need 0 < if E-PROP-GENERATOR throw then
   PROP-GEN-DEPTH @ need < if E-PROP-GENERATOR throw then
   PROP-GEN-DEPTH @ delta + PROP-GEN-NEXT !
   PROP-GEN-NEXT @ 0 < if E-PROP-GENERATOR throw then
   u PROP-BUF-CHECK-ROOM
   a u PROP-BUF+
   PROP-GEN-NEXT @ PROP-GEN-DEPTH ! ;

: PROP-TRIM-TRAIL ( -- )
   begin PROP-BUF-LEN @ 0 > while
      PROP-BUF PROP-BUF-LEN @ 1- + c@ PROP-SPACE = if
         PROP-BUF-LEN @ 1- PROP-BUF-LEN !
      else
         exit
      then
   repeat ;

: PROP-DROP-LAST ( -- bool )
   PROP-TRIM-TRAIL
   PROP-BUF-LEN @ 0= if 0 0= 0= exit then
   begin PROP-BUF-LEN @ 0 > while
      PROP-BUF PROP-BUF-LEN @ 1- + c@ PROP-SPACE <> if
         PROP-BUF-LEN @ 1- PROP-BUF-LEN !
      else
         0 0= exit
      then
   repeat
   0 0= ;

: PROP-SHRINK ( [ -- bool ] -- )
   PROP-SHRINK-PRED !
   PROP-SHRINK-PRED @ execute 0= if E-PROP-SHRINK throw then
   begin
      PROP-BUF-LEN @ PROP-SHRINK-SAVE !
      PROP-DROP-LAST if
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
