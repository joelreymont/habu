\ lib.f - shared scanner foundation for native lint tools.

require tools/lint/text.f
require tools/lint/token.f

\ Source span used by the REPL lint file driver.
TYPED-VARIABLE P2A ptr u8
variable P2U
: P2A@ ( -- ptr u8 ) P2A @ ;
: P2A! ( ptr u8 -- ) P2A ! ;

\ ---- attributed CLI failure -------------------------------------------------
\ A lint CLI catches its strict entry and calls `s" tool" code LINT-MAIN`, which
\ prints `tool: threw <code> (<name>)` through LINT-OUT-WRITE and rethrows the
\ same code, so the exit status is unchanged.

24 constant LINT-MAIN-NCAP
create LINT-MAIN-NBUF LINT-MAIN-NCAP allot
create LINT-MAIN-LFB 1 allot
variable LINT-MAIN-NI

: LINT-MAIN-N$ ( n -- ptr u8 n ) {: v:n :}
   LINT-MAIN-NCAP LINT-MAIN-NI !
   v 0= IF s" 0" exit THEN
   v 0 < IF 0 v - ELSE v THEN
   begin dup 0 > while
      dup 10 mod 48 +
      LINT-MAIN-NI @ 1- LINT-MAIN-NI !
      LINT-MAIN-NBUF LINT-MAIN-NI @ + c!
      10 /
   repeat drop
   v 0 < IF
      LINT-MAIN-NI @ 1- LINT-MAIN-NI !
      45 LINT-MAIN-NBUF LINT-MAIN-NI @ + c!
   THEN
   LINT-MAIN-NBUF LINT-MAIN-NI @ +  LINT-MAIN-NCAP LINT-MAIN-NI @ - ;

: LINT-MAIN-OUT ( ptr u8 n -- ) {: a:ptr u:n :}
   1 a u LINT-OUT-WRITE ;

: LINT-MAIN-LF ( -- )
   10 LINT-MAIN-LFB c!
   LINT-MAIN-LFB 1 LINT-MAIN-OUT ;

: LINT-THREW-NAME. ( n -- )   \ append " (E-LINT-...)" when the code is known
   LINT-CODE-FIND dup 0 < IF drop exit THEN
   s"  (" LINT-MAIN-OUT
   LINT-CODE-NAME$ LINT-MAIN-OUT
   s" )" LINT-MAIN-OUT ;

: LINT-MAIN ( ptr u8 n n -- ) {: name:ptr nu:n code:n :}
   code 0= IF exit THEN
   name nu LINT-MAIN-OUT
   s" : threw " LINT-MAIN-OUT
   code LINT-MAIN-N$ LINT-MAIN-OUT
   code LINT-THREW-NAME.
   LINT-MAIN-LF
   code throw ;

\ Downstream scanner modules that need an unchecked boundary must declare and
\ test that boundary locally instead of inheriting one from this shared library.
