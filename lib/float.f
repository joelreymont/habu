\ float.f - checked decimal string -> IEEE-double parsing.
\
\ STR>FLOAT parses an optionally-signed decimal with an optional fraction and an
\ optional `e`/`E` exponent: [+|-] digits [. digits] [ (e|E) [+|-] digits ].
\ The significand is accumulated in a double and scaled by a power of ten, so it
\ is exact for values whose significand fits in 2^53 and within double's range
\ (every sensor/JSON float the application emits); it is not a bit-exact arbitrary-precision
\ parser. Depends on lib/string.f (digit predicates, STR>NUMBER?).
\
\ Habu locals discipline: a local may not be bound after an `exit` has appeared
\ on a path, so every word here binds all its locals up front (after the
\ producing calls / value-only if-else) and keeps a single trailing exit guard.

require lib/string.f                         \ STR-DIGITS? / STR-DIGIT-VALUE / STR-MINUS / STR-PLUS
require lib/adt/option.f                      \ option<idx> for FL-FIND-E (switchover wave A)

46 constant FL-DOT
101 constant FL-E-LOWER
69 constant FL-E-UPPER
400 constant FL-EXP-MAX                     \ clamp: |exp| beyond this is inf/0 anyway

variable FL-IX                              \ digit-loop index (leaf, single-threaded)
variable FL-EXPV                            \ parsed exponent value
variable FL-VALID                           \ exponent validity flag

\ ---- powers of ten --------------------------------------------------------
: POW10+ ( n -- r ) {: k :}                 \ 10^k for k >= 0, iterative
   1.0  0 FL-IX !
   begin FL-IX @ k < while
      10.0 f*
      FL-IX @ 1+ FL-IX !
   repeat ;
: FL-CLAMP-EXP ( n -- n ) {: k :}
   k FL-EXP-MAX > if FL-EXP-MAX exit then
   k FL-EXP-MAX negate < if FL-EXP-MAX negate exit then
   k ;
: POW10 ( n -- r ) FL-CLAMP-EXP {: k :}     \ 10^k for any signed k
   k 0 >= if k POW10+ exit then
   0 k - POW10+ 1.0 swap f/ ;

\ ---- digit string -> double -----------------------------------------------
\ Unsigned run of decimal digits. Empty is valid and yields 0.0; any non-digit
\ byte is rejected.
: FL-DIGITS>F ( ptr u8 n -- option<r> ) {: a:ptr u:n :}   \ SOME digit-run value (empty -> SOME 0.0), NONE on a non-digit
   u 0= if 0.0 OPTION:SOME exit then
   a u STR-DIGITS? 0= if OPTION:NONE exit then
   0.0  0 FL-IX !
   begin FL-IX @ u < while
      10.0 f*  a FL-IX @ + c@ STR-DIGIT-VALUE s>f f+
      FL-IX @ 1+ FL-IX !
   repeat  OPTION:SOME ;

\ ---- field splitting ------------------------------------------------------
: FL-STRIP-SIGN ( ptr u8 n -- ptr u8 n bool ) {: a:ptr u :}
   u 0= if a 0 0 0= 0= exit then
   a c@ STR-MINUS = if a 1+ u 1- 0 0= exit then
   a c@ STR-PLUS  = if a 1+ u 1- 0 0= 0= exit then
   a u 0 0= 0= ;
: FL-FIND-E ( ptr u8 n -- option<idx> ) {: a:ptr u:n :}   \ SOME index of e/E, else NONE
   a u FL-E-LOWER INDEX-OF dup 0 >= if >IDX OPTION:SOME exit then drop
   a u FL-E-UPPER INDEX-OF dup 0 >= if >IDX OPTION:SOME else drop OPTION:NONE then ;

\ ---- significand (no sign, no exponent) -----------------------------------
\ Split the mantissa at the dot, parse both halves, combine. Requires at least
\ one digit across the two halves, so "" and "." are rejected.
: FL-SIG ( ptr u8 n -- option<r> ) {: a:ptr u:n :}   \ SOME significand, NONE if no digits / bad
   a u FL-DOT INDEX-OF {: dpos :}
   dpos 0 >= if dpos else u then {: ilen :}
   dpos 0 >= if a dpos 1+ + else a u + then {: fa:ptr :}
   dpos 0 >= if u dpos 1+ - else 0 then {: flen :}
   ilen flen + 0= if OPTION:NONE exit then       \ no digits at all: "" and "." rejected
   a ilen FL-DIGITS>F MATCH option
     none OF OPTION:NONE exit ENDOF
     some OF ENDOF                                \ int value (ival) left on the stack
   ;MATCH
   fa flen FL-DIGITS>F MATCH option
     none OF drop OPTION:NONE exit ENDOF          \ drop ival, reject a bad fraction
     some OF ENDOF                                \ stack: ival fval
   ;MATCH
   flen POW10 f/ f+ OPTION:SOME ;                 \ SOME (ival + fval/10^flen)

\ ---- exponent -------------------------------------------------------------
\ FL-EXP-AT parses the exponent text after position epos, records it, and
\ returns the mantissa length (epos). FL-PARSE-EXP handles the no-exponent case.
: FL-EXP-AT ( ptr u8 n n -- n ) {: a:ptr u epos :}
   a epos 1+ + u epos 1+ - STR>NUMBER? {: ok :}
   ok if FL-EXPV ! else drop 0 FL-VALID ! then
   epos ;
: FL-PARSE-EXP ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u FL-FIND-E MATCH option
     none OF 0 FL-EXPV ! u ENDOF                    \ no exponent: exp 0, mantissa = whole string
     some OF IDX>N {: epos:n :} a u epos FL-EXP-AT ENDOF
   ;MATCH ;

\ ---- public entry ---------------------------------------------------------
: STR>FLOAT ( ptr u8 n -- r bool ) {: a0:ptr u0 :}
   -1 FL-VALID !
   a0 u0 FL-STRIP-SIGN {: a:ptr u neg :}
   a u FL-PARSE-EXP {: mlen :}
   a mlen FL-SIG MATCH option
     none OF 0.0 0 0= 0= exit ENDOF                 \ bad significand -> 0.0, false
     some OF ENDOF                                  \ SOME significand left on the stack
   ;MATCH
   u 0=  FL-VALID @ 0= or
   if drop 0.0 0 0= 0= exit then
   FL-EXPV @ POW10 f*
   neg if fnegate then
   0 0= ;
