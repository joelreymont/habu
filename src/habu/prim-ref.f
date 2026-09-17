\ prim-ref.f — reference implementations for the engine primitive table.
\
\ src/habu/prims.f names one of these words in a row's `REF` clause, and
\ test/prim-parity.f runs the reference beside the backend's own body over the
\ same case set. A disagreement is the gate's red: either the backend's
\ primitive is wrong or the reference is, and the two were written from the
\ specification independently.
\
\ THE HONESTY RULE. A reference implements its row's semantics WITHOUT the
\ primitive it answers for. `SUB` may call `+`, `ADD` may not; `SHL` computes
\ its shift by doubling through `*`, `SHR` by halving through `/`, and the
\ divider builds its quotient from left shifts and subtraction so it never
\ reaches `/` or `mod` at all. A primitive whose semantics cannot be written
\ that way — `0=` (every composition needs a branch, and a branch needs `0=`),
\ pointer difference, the memory and float primitives whose edges (byte order,
\ signed zero, NaN) no composition reproduces — gets no reference, and its row
\ carries no `REF` clause. A reference is never the primitive renamed.
\
\ WHAT IT DOES NOT SAY. The references share a small base: `and`, `xor`, `0=`,
\ comparison and the branch. A backend that breaks one of those turns several
\ rows red at once; the gate names each, and the base itself is pinned by
\ expected-output cases rather than by a reference.
\
\ NAMES. No word here shadows a primitive spelling, case-insensitively, so
\ `dup`'s reference is `S-DUP` and `abs`'s is `ABSOLUTE` (docs/forth.md, "Do
\ not shadow native primitive names"). The `REF` clause spells the qualified
\ name, `PRIM-REF:S-DUP`, and the gate asserts every spelling resolves.
\
\ LOAD PATH. This file is NOT baked: it appears in no engine prefix source list
\ and runs as ordinary checked Habu after the check hook is installed. The
\ parity gate requires it. Reference loops keep their cursors in package
\ variables because a local cannot be reassigned (src/habu/prims.f does the
\ same for its own walkers), so no word here is reentrant.

package PRIM-REF
private

$4C constant REF-RC                     \ EX_PROTOCOL, as prims.f uses for its own wall
$8000000000000000 constant SIGN-BIT
$4000000000000000 constant SIGN-HALF    \ the sign bit one place down
$3F constant SHIFT-MASK                 \ shift counts are taken modulo the cell width
1 constant CHAR-SIZE

variable UD-T   variable UD-K   variable UD-Q   variable UD-R
variable UD-I   variable UD-STEP
variable ML-ACC variable ML-X   variable ML-B

\ ---- unsigned order, for the divider ----------------------------------------
\ Flipping the sign bit turns the signed order into the unsigned one over the
\ whole cell, which is what a magnitude comparison needs: a magnitude of 2^63
\ (MIN-N's) is a negative cell.
: U< ( n n -- bool ) {: a:n b:n :}
   a SIGN-BIT xor  b SIGN-BIT xor  < ;

: U<= ( n n -- bool ) {: a:n b:n :}
   b a U< 0= ;

public

\ The wordlist these references are defined in, captured while it is the current
\ one. A `REF` spelling in the table is qualified (`PRIM-REF:S-DUP`) and
\ `search-wl` resolves a tail in a wordlist, so the parity gate splits the
\ qualifier off and looks the tail up here to prove every clause names a word
\ that exists.
get-current constant WID

\ ---- stack shufflers ---------------------------------------------------------
\ Each is the row's effect written with typed locals, which name the cells the
\ primitive moves instead of moving them.
: S-DUP ( a -- a a ) {: x:a :}
   x x ;

: S-DROP ( a -- ) {: x:a :}
;

: S-SWAP ( a b -- b a ) {: x:a y:b :}
   y x ;

: S-OVER ( a b -- a b a ) {: x:a y:b :}
   x y x ;

: S-NIP ( a b -- b ) {: x:a y:b :}
   y ;

: S-TUCK ( a b -- b a b ) {: x:a y:b :}
   y x y ;

: S-ROT ( a b c -- b c a ) {: x:a y:b z:c :}
   y z x ;

: S-UNROT ( a b c -- c a b ) {: x:a y:b z:c :}
   z x y ;

: S-2DUP ( a b -- a b a b ) {: x:a y:b :}
   x y x y ;

: S-2DROP ( a b -- ) {: x:a y:b :}
;

: S-2SWAP ( a b c d -- c d a b ) {: x:a y:b z:c w:d :}
   z w x y ;

: S-2OVER ( a b c d -- a b c d a b ) {: x:a y:b z:c w:d :}
   x y z w x y ;

\ ---- bit logic ---------------------------------------------------------------
: BNOT ( n -- n )                       \ invert
   -1 xor ;

: BAND ( n n -- n ) {: a:n b:n :}       \ and, by De Morgan over or
   a invert b invert or invert ;

: BOR ( n n -- n ) {: a:n b:n :}        \ or, by De Morgan over and
   a invert b invert and invert ;

: BXOR ( n n -- n ) {: a:n b:n :}       \ xor = (a or b) and not (a and b)
   a b or  a b and invert  and ;

\ The boolean overloads of the same three rows: `invert` is a numeric row, so
\ the complement of a flag is `0=`.
: BAND-F ( bool bool -- bool ) {: a:bool b:bool :}
   a 0= b 0= or 0= ;

: BOR-F ( bool bool -- bool ) {: a:bool b:bool :}
   a 0= b 0= and 0= ;

: BXOR-F ( bool bool -- bool ) {: a:bool b:bool :}
   a b or  a b and 0=  and ;

\ ---- shifts ------------------------------------------------------------------
\ Measured on the arm64 engine and pinned by the gate: the count is taken
\ modulo the cell width (`1 64 lshift` is 1, not 0) and `rshift` is logical.
: SHL ( n n -- n ) {: x:n k:n :}
   x  k SHIFT-MASK and 0 ?do 2 * loop ;

private

: SHR1 ( n -- n ) {: x:n :}             \ one logical right shift
   x SIGN-BIT invert and 2 /
   x 0< IF SIGN-HALF + THEN ;

public

: SHR ( n n -- n ) {: x:n k:n :}
   x  k SHIFT-MASK and 0 ?do SHR1 loop ;

\ ---- arithmetic --------------------------------------------------------------
\ Ripple carry: the sum without carries is `xor`, the carries are `and` moved
\ up one place, and the recursion ends when no carry is left (at most 64 turns).
: ADD ( n n -- n ) {: a:n b:n :}
   b 0= IF a EXIT THEN
   a b xor   a b and 2 *   RECURSE ;

: NEG ( n -- n )                        \ negate; MIN-N negates to itself, as the primitive does
   invert 1+ ;

: SUB ( n n -- n ) {: a:n b:n :}
   a b NEG + ;

: INC ( n -- n )                        \ 1+
   1 + ;

: DEC ( n -- n )                        \ 1-
   1 - ;

\ Shift-and-add: the multiplicand doubles while the multiplier is consumed one
\ bit at a time, so nothing here multiplies.
: MUL ( n n -- n ) {: a:n b:n :}
   0 ML-ACC !  a ML-X !  b ML-B !
   BEGIN ML-B @ 0= 0= WHILE
      ML-B @ 1 and 0= 0= IF ML-ACC @ ML-X @ + ML-ACC ! THEN
      ML-X @ ML-X @ + ML-X !
      ML-B @ SHR1 ML-B !
   REPEAT
   ML-ACC @ ;

private

: MAGN ( n -- n ) {: x:n :}             \ magnitude as an unsigned pattern; MIN-N gives 2^63
   x 0< IF x NEG EXIT THEN
   x ;

\ Unsigned long division over magnitudes, quotient built from left shifts.
\ Phase one doubles the divisor while that neither overflows nor passes the
\ numerator; phase two walks the same shifts back down, subtracting where the
\ shifted divisor fits. Both operands are magnitudes of signed cells, so the
\ divisor never exceeds 2^63 and no intermediate wraps.
: UDIVMOD ( n n -- n n ) {: num:n den:n :}
   den UD-T !  0 UD-K !
   BEGIN
      UD-T @ SIGN-BIT and 0=
      UD-T @ 2 * num U<= and
   WHILE
      UD-T @ 2 * UD-T !
      UD-K @ 1+ UD-K !
   REPEAT
   0 UD-Q !  num UD-R !  UD-K @ UD-I !
   BEGIN UD-I @ 0 >= WHILE
      den UD-I @ SHL UD-STEP !
      UD-STEP @ UD-R @ U<= IF
         UD-R @ UD-STEP @ - UD-R !
         UD-Q @ 1 UD-I @ SHL + UD-Q !
      THEN
      UD-I @ 1- UD-I !
   REPEAT
   UD-Q @ UD-R @ ;

public

\ `/mod`'s order, measured: remainder under quotient. Division truncates toward
\ zero, so the remainder carries the dividend's sign.
\
\ A zero divisor has no value semantics to reference: the engine does not refuse
\ it by name, it terminates the process (measured: SIGTRAP crash dump, exit
\ 134), so this word dies rather than inventing a quotient. The gate cannot
\ hold that case in process and says so.
: DIVREM ( n n -- n n ) {: num:n den:n :}
   den 0= IF s" prim-ref: division by zero" REF-RC die THEN
   num MAGN den MAGN UDIVMOD {: uq:n ur:n :}
   ur num 0< IF NEG THEN
   uq num 0< den 0< xor IF NEG THEN ;

: DIV ( n n -- n )
   DIVREM nip ;

: REM ( n n -- n )                      \ mod
   DIVREM drop ;

: ABSOLUTE ( n -- n ) {: x:n :}         \ abs; MIN-N answers itself, as the primitive does
   x 0< IF x NEG EXIT THEN
   x ;

: MINIMUM ( n n -- n ) {: a:n b:n :}    \ min
   a b < IF a EXIT THEN
   b ;

: MAXIMUM ( n n -- n ) {: a:n b:n :}    \ max
   a b < IF b EXIT THEN
   a ;

\ ---- order -------------------------------------------------------------------
: ZERO-NEG? ( n -- bool )               \ 0<
   SIGN-BIT and 0= 0= ;

: EQ ( n n -- bool ) {: a:n b:n :}      \ = ; the difference is zero only for equals
   a b - 0= ;

: NEQ ( n n -- bool ) {: a:n b:n :}     \ <>
   a b - 0= 0= ;

\ Subtraction alone cannot order two cells: `a b -` wraps when the signs differ.
\ Different signs decide the order outright, and equal signs make the difference
\ exact.
: LT ( n n -- bool ) {: a:n b:n :}      \ <
   a 0< b 0< xor IF a 0< EXIT THEN
   a b - 0< ;

: GT ( n n -- bool ) {: a:n b:n :}      \ >
   b a LT ;

: LE ( n n -- bool ) {: a:n b:n :}      \ <=
   b a LT 0= ;

: GE ( n n -- bool ) {: a:n b:n :}      \ >=
   a b LT 0= ;

\ ---- address arithmetic on plain integers ------------------------------------
\ The numeric rows of `cells`, `chars`, `cell+` and `char+`; their pointer rows
\ have no reference, because only `+` moves a pointer.
: CELL-BYTES ( n -- n )                 \ cells
   CELL * ;

: CHAR-BYTES ( n -- n )                 \ chars
   CHAR-SIZE * ;

: CELL-STEP ( n -- n )                  \ cell+
   CELL + ;

: CHAR-STEP ( n -- n )                  \ char+
   CHAR-SIZE + ;

\ ---- memory ------------------------------------------------------------------
: ADD-TO ( n ptr n -- )                 \ +!
   dup @ rot + swap ! ;

: COUNTED$ ( ptr u8 -- ptr u8 n )       \ count
   dup char+ swap c@ ;

;package
