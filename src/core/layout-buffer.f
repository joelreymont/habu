\ layout-buffer.f — generative typed fixed-capacity storage.
\
\ LAYOUT-BUFFER is the only public introduction form for `ptr layout`. It owns
\ allocation, zero-image initialization, stride, and bounds; the checker arms a
\ single generated-accessor authorization instead of allowing ptr variables to
\ acquire layout identity through ordinary unification.
\
\ TYPED-VARIABLE and TYPED-BUFFER (dot habu-nominal-storage-typed) are the
\ convenience definers built on the SAME generative boundary: a single typed
\ cell, and a typed fixed-capacity buffer. They reuse LAYOUT-BUFFER's armed
\ generated-accessor window (LBUF-EVAL / LBUF-PEND) and admit a broader
\ CHECKER-STORAGE-INFO type surface — nominal scalars, closed non-linear layout
\ families, AND closed typed pointers — without weakening LAYOUT-BUFFER, whose
\ own narrower CHECKER-LAYOUT-INFO gate is unchanged.

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
   LBUF-GEN-U @ LBUF-GEN-CAP >= if E-LAYOUT-BUFFER throw then
   c LBUF-GEN LBUF-GEN-U @ + c!
   LBUF-GEN-U @ 1 + LBUF-GEN-U ! ;

: LBUF-APP ( ptr u8 n -- ) {: a:ptr u:n :}
   0 LBUF-I !
   begin LBUF-I @ u < while
      a LBUF-I @ + c@ LBUF-C,
      LBUF-I @ 1 + LBUF-I !
   repeat ;

: LBUF-DEC, ( n -- ) {: n:n :}
   n 10 >= if n 10 / recurse then
   n 10 mod 48 + LBUF-C, ;

: LBUF-EXTENT? ( n n -- n bool ) {: count:n width:n :}
   count 0 <= width 0 <= or if 0 LBUF-FALSE exit then
   count LBUF-N-MAX width / > if 0 LBUF-FALSE exit then
   count width * {: cellsn:n :}
   cellsn LBUF-N-MAX CELL / > if 0 LBUF-FALSE exit then
   cellsn cells LBUF-TRUE ;

: LBUF-VALIDATE ( n ptr u8 n -- ) {: count:n type:ptr typeu:n :}
   type typeu CHECKER-LAYOUT-INFO 0= if 2drop E-LAYOUT-BUFFER throw then
   LBUF-W ! drop
   count LBUF-N !
   count LBUF-W @ LBUF-EXTENT? 0= if drop E-LAYOUT-BUFFER throw then
   LBUF-BYTES ! ;

: LBUF-NAME-GUARD ( ptr u8 n -- ) {: name:ptr nameu:n :}
   name nameu CHECKER-LBUF-NAME-GUARD
   name nameu CHECKER-DEFINED? if E-DUP-DEFINITION throw then
   name nameu get-current search-wl 0 <> if E-DUP-DEFINITION throw then ;

: LBUF-ZERO ( ptr a n -- ) {: base:ptr bytes:n :}
   0 LBUF-I !
   begin LBUF-I @ bytes < while
      0 base LBUF-I @ + !
      LBUF-I @ CELL + LBUF-I !
   repeat ;

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
   LBUF-EVAL-A 0 ptr-field @ LBUF-EVAL-U @ TDECL-EVAL-XT ;

: LBUF-EVAL ( ptr u8 n ptr u8 n -- n )
   {: src:ptr srcu:n name:ptr nameu:n :}
   TDECL-EVAL-ARMED @ 0= if E-LAYOUT-BUFFER throw then
   name nameu LBUF-PEND!
   src LBUF-EVAL-A 0 ptr-field !  srcu LBUF-EVAL-U !
   [: LBUF-EVAL-RUN ;] catch
   LBUF-PEND-CLEAR ;

: LBUF-ROLLBACK ( n -- ) {: rc:n :}
   LBUF-BYTES @ negate allot
   rc throw ;

: LAYOUT-BUFFER ( n -- ) {: count:n :}
   parse-name {: name:ptr nameu:n :}
   parse-name {: type:ptr typeu:n :}
   nameu 0= if E-LAYOUT-BUFFER throw then
   TDECL-EVAL-ARMED @ 0= if E-LAYOUT-BUFFER throw then
   name nameu LBUF-NAME-GUARD
   count type typeu LBUF-VALIDATE
   here {: base:ptr :}
   base data-base - {: off:n :}
   name nameu type typeu off LBUF-SOURCE {: src:ptr srcu:n pna:ptr pnu:n :}
   LBUF-BYTES @ allot
   base LBUF-BYTES @ LBUF-ZERO
   src srcu pna pnu LBUF-EVAL
   dup 0 <> if LBUF-ROLLBACK then
   drop ;

\ LAYOUT-BUFFER is the public top-level introduction form: it consumes the
\ count operand and parses its own name + type tokens. The axiom keeps it
\ checker-known so the seal-time internal-word marking pass
\ (src/core/internal-mark.f) leaves it executable at top level (dot
\ habu-hb-crash-bare-c5be6634). UNSAFE-TOK? rejects `layout-buffer` inside
\ checked bodies (it evaluates generated accessor source via LBUF-EVAL), so
\ the axiom adds no checked-code capability.
PRIM: LAYOUT-BUFFER PE-N PE-IN PRIM;

\ ---- TYPED-VARIABLE / TYPED-BUFFER convenience definers ----------------------
\ Same generative machinery as LAYOUT-BUFFER (name guard, allocation, zero image,
\ generated-accessor evaluation under the armed window, transactional rollback),
\ gated by the broader CHECKER-STORAGE-INFO admissibility. TYPED-BUFFER reuses
\ LBUF-SOURCE (the indexed `( n -- ptr type )` accessor); TYPED-VARIABLE emits a
\ single-cell `( -- ptr type )` accessor. Both parse a `ptr* base` stored type so
\ closed typed pointers (`ptr TARGET`, `ptr res<n,n>`) are expressible.

variable STGT-A
variable STGT-U
variable STGT-START

: STORAGE-PTR-TOK? ( ptr u8 n -- bool )   \ token is the pointer constructor `ptr`
   s" ptr" CORE-STR= ;

: STORAGE-QUOT-OPEN? ( ptr u8 n -- bool )   \ token is the quotation opener `[`
   s" [" CORE-STR= ;

: STORAGE-QUOT-CLOSE? ( ptr u8 n -- bool )   \ token is the quotation closer `]`
   s" ]" CORE-STR= ;

\ Consume a spaced `[ in -- out ]` xt<effect> quotation type token by token, up
\ to and including the closer, so the returned span is the whole quotation.
: STORAGE-PARSE-QUOT ( -- )
   begin STGT-A @ STGT-U @ STORAGE-QUOT-CLOSE? 0= while
      parse-name STGT-U !  STGT-A !
      STGT-U @ 0= if E-LAYOUT-BUFFER throw then
   repeat ;

: STORAGE-PARSE-TYPE ( -- ptr u8 n )   \ capture a `ptr* base` or `[ in -- out ]` stored-type source span
   parse-name STGT-U !  STGT-A !
   STGT-U @ 0= if E-LAYOUT-BUFFER throw then
   STGT-A @ STGT-START !
   begin STGT-A @ STGT-U @ STORAGE-PTR-TOK? while
      parse-name STGT-U !  STGT-A !
      STGT-U @ 0= if E-LAYOUT-BUFFER throw then
   repeat
   STGT-A @ STGT-U @ STORAGE-QUOT-OPEN? if STORAGE-PARSE-QUOT then
   STGT-START @  STGT-A @ STGT-U @ + STGT-START @ - ;

: STORAGE-VALIDATE ( n ptr u8 n -- ) {: count:n type:ptr typeu:n :}
   type typeu CHECKER-STORAGE-INFO 0= if drop E-LAYOUT-BUFFER throw then
   LBUF-W !
   count LBUF-N !
   count LBUF-W @ LBUF-EXTENT? 0= if drop E-LAYOUT-BUFFER throw then
   LBUF-BYTES ! ;

: TYPED-VAR-SOURCE ( ptr u8 n ptr u8 n n -- ptr u8 n ptr u8 n )
   {: name:ptr nameu:n type:ptr typeu:n off:n :}
   LBUF-CLEAR
   s" : " LBUF-APP
   name nameu LBUF-NAME, {: pna:ptr pnu:n :}
   s"  ( -- ptr " LBUF-APP  type typeu LBUF-APP
   s"  ) data-base " LBUF-APP
   off LBUF-DEC,
   s"  + ;" LBUF-APP
   LBUF-GEN LBUF-GEN-U @ pna pnu ;

: TYPED-BUFFER ( n -- ) {: count:n :}
   parse-name {: name:ptr nameu:n :}
   STORAGE-PARSE-TYPE {: type:ptr typeu:n :}
   nameu 0= if E-LAYOUT-BUFFER throw then
   TDECL-EVAL-ARMED @ 0= if E-LAYOUT-BUFFER throw then
   name nameu LBUF-NAME-GUARD
   count type typeu STORAGE-VALIDATE
   here {: base:ptr :}
   base data-base - {: off:n :}
   name nameu type typeu off LBUF-SOURCE {: src:ptr srcu:n pna:ptr pnu:n :}
   LBUF-BYTES @ allot
   base LBUF-BYTES @ LBUF-ZERO
   src srcu pna pnu LBUF-EVAL
   dup 0 <> if LBUF-ROLLBACK then
   drop ;

: TYPED-VARIABLE ( -- )
   parse-name {: name:ptr nameu:n :}
   STORAGE-PARSE-TYPE {: type:ptr typeu:n :}
   nameu 0= if E-LAYOUT-BUFFER throw then
   TDECL-EVAL-ARMED @ 0= if E-LAYOUT-BUFFER throw then
   name nameu LBUF-NAME-GUARD
   1 type typeu STORAGE-VALIDATE
   here {: base:ptr :}
   base data-base - {: off:n :}
   name nameu type typeu off TYPED-VAR-SOURCE {: src:ptr srcu:n pna:ptr pnu:n :}
   LBUF-BYTES @ allot
   base LBUF-BYTES @ LBUF-ZERO
   src srcu pna pnu LBUF-EVAL
   dup 0 <> if LBUF-ROLLBACK then
   drop ;

\ Axioms keep the two definers checker-known so the seal-time internal-word pass
\ leaves them executable at top level (like LAYOUT-BUFFER); UNSAFE-TOK? rejects
\ `typed-buffer`/`typed-variable` inside checked bodies (they evaluate generated
\ accessor source), so the axioms add no checked-code capability.
PRIM: TYPED-BUFFER PE-N PE-IN PRIM;
PRIM: TYPED-VARIABLE PRIM;
