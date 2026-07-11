\ layout-buffer.f — generative typed storage for closed ADT layouts.
\
\ LAYOUT-BUFFER is the only public introduction form for `ptr layout`. It owns
\ allocation, zero-image initialization, stride, and bounds; the checker arms a
\ single generated-accessor authorization instead of allowing ptr variables to
\ acquire layout identity through ordinary unification.

$1000 constant LBUF-GEN-CAP
$7FFFFFFFFFFFFFFF constant LBUF-N-MAX
7121 constant E-LAYOUT-BUFFER
7122 constant E-LAYOUT-BOUNDS
78 constant E-DUP-DEFINITION
0 constant LBUF-FALSE
-1 constant LBUF-TRUE

create LBUF-GEN LBUF-GEN-CAP allot
variable LBUF-GEN-U
variable LBUF-I
variable LBUF-N
variable LBUF-W
variable LBUF-BYTES

: LBUF-CLEAR ( -- )
   0 LBUF-GEN-U ! ;

: LBUF-C, ( n -- ) {: c:n :}
   LBUF-GEN-U @ LBUF-GEN-CAP >= IF E-LAYOUT-BUFFER throw THEN
   c LBUF-GEN LBUF-GEN-U @ + c!
   LBUF-GEN-U @ 1 + LBUF-GEN-U ! ;

: LBUF-APP ( ptr u8 n -- ) {: a:ptr u:n :}
   0 LBUF-I !
   BEGIN LBUF-I @ u < WHILE
      a LBUF-I @ + c@ LBUF-C,
      LBUF-I @ 1 + LBUF-I !
   REPEAT ;

: LBUF-DEC, ( n -- ) {: n:n :}
   n 10 >= IF n 10 / RECURSE THEN
   n 10 mod 48 + LBUF-C, ;

: LBUF-COUNT? ( ptr u8 n -- n bool ) {: a:ptr u:n :}
   u 0= IF 0 LBUF-FALSE EXIT THEN
   0 LBUF-N !
   0 LBUF-I !
   BEGIN LBUF-I @ u < WHILE
      a LBUF-I @ + c@ {: c:n :}
      c 48 < c 58 >= or IF 0 LBUF-FALSE EXIT THEN
      c 48 - {: d:n :}
      LBUF-N @ LBUF-N-MAX d - 10 / > IF 0 LBUF-FALSE EXIT THEN
      LBUF-N @ 10 * d + LBUF-N !
      LBUF-I @ 1 + LBUF-I !
   REPEAT
   LBUF-N @ 0 <= IF 0 LBUF-FALSE EXIT THEN
   LBUF-N @ LBUF-TRUE ;

: LBUF-EXTENT? ( n n -- n bool ) {: count:n width:n :}
   count 0 <= width 0 <= or IF 0 LBUF-FALSE EXIT THEN
   count LBUF-N-MAX width / > IF 0 LBUF-FALSE EXIT THEN
   count width * {: cellsn:n :}
   cellsn LBUF-N-MAX CELL / > IF 0 LBUF-FALSE EXIT THEN
   cellsn cells LBUF-TRUE ;

: LBUF-VALIDATE ( ptr u8 n ptr u8 n -- ) {: type:ptr typeu:n count:ptr countu:n :}
   type typeu CHECKER-LAYOUT-INFO 0= IF 2drop E-LAYOUT-BUFFER throw THEN
   LBUF-W ! drop
   count countu LBUF-COUNT? 0= IF drop E-LAYOUT-BUFFER throw THEN
   LBUF-N !
   LBUF-N @ LBUF-W @ LBUF-EXTENT? 0= IF drop E-LAYOUT-BUFFER throw THEN
   LBUF-BYTES ! ;

: LBUF-NAME-GUARD ( ptr u8 n -- ) {: name:ptr nameu:n :}
   name nameu CHECKER-DEFINED? IF E-DUP-DEFINITION throw THEN
   name nameu get-current search-wl 0 <> IF E-DUP-DEFINITION throw THEN ;

: LBUF-ZERO ( ptr a n -- ) {: base:ptr bytes:n :}
   0 LBUF-I !
   BEGIN LBUF-I @ bytes < WHILE
      0 base LBUF-I @ + !
      LBUF-I @ CELL + LBUF-I !
   REPEAT ;

: LBUF-NAME, ( ptr u8 n -- ptr u8 n ) {: name:ptr nameu:n :}
   LBUF-GEN-U @ {: start:n :}
   name nameu LBUF-APP
   LBUF-GEN start + nameu ;

: LBUF-SOURCE ( ptr u8 n ptr u8 n n -- ptr u8 n ptr u8 n )
   {: name:ptr nameu:n type:ptr typeu:n off:n :}
   LBUF-CLEAR
   s" : " LBUF-APP
   name nameu LBUF-NAME, {: pna:ptr pnu:n :}
   s"  ( n -- ptr " LBUF-APP  type typeu LBUF-APP
   s"  ) {: i:n :} i 0 < if " LBUF-APP
   E-LAYOUT-BOUNDS LBUF-DEC,
   s"  throw then i " LBUF-APP
   LBUF-N @ LBUF-DEC,
   s"  >= if " LBUF-APP
   E-LAYOUT-BOUNDS LBUF-DEC,
   s"  throw then data-base " LBUF-APP
   off LBUF-DEC,
   s"  + i " LBUF-APP
   LBUF-W @ cells LBUF-DEC,
   s"  * + ;" LBUF-APP
   LBUF-GEN LBUF-GEN-U @ pna pnu ;

PTR-VARIABLE LBUF-EVAL-A
variable LBUF-EVAL-U

: LBUF-EVAL-RUN ( -- )
   LBUF-EVAL-A 0 ptr-field @ LBUF-EVAL-U @ TDECL-EVAL-XT @ execute ;

: LBUF-EVAL ( ptr u8 n ptr u8 n -- n )
   {: src:ptr srcu:n name:ptr nameu:n :}
   TDECL-EVAL-XT @ 0= IF E-LAYOUT-BUFFER throw THEN
   name nameu LBUF-PEND!
   src LBUF-EVAL-A 0 ptr-field !  srcu LBUF-EVAL-U !
   [: LBUF-EVAL-RUN ;] catch
   LBUF-PEND-CLEAR ;

: LBUF-ROLLBACK ( n -- ) {: rc:n :}
   LBUF-BYTES @ negate allot
   rc throw ;

: LAYOUT-BUFFER ( -- )
   parse-name {: name:ptr nameu:n :}
   parse-name {: type:ptr typeu:n :}
   parse-name {: count:ptr countu:n :}
   nameu 0= IF E-LAYOUT-BUFFER throw THEN
   TDECL-EVAL-XT @ 0= IF E-LAYOUT-BUFFER throw THEN
   name nameu LBUF-NAME-GUARD
   type typeu count countu LBUF-VALIDATE
   here {: base:ptr :}
   base data-base - {: off:n :}
   name nameu type typeu off LBUF-SOURCE {: src:ptr srcu:n pna:ptr pnu:n :}
   LBUF-BYTES @ allot
   base LBUF-BYTES @ LBUF-ZERO
   src srcu pna pnu LBUF-EVAL
   dup 0 <> IF LBUF-ROLLBACK THEN
   drop ;
