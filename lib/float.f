\ float.f - checked decimal string -> IEEE-double parsing.
\
\ STR>FLOAT parses an optionally-signed decimal with an optional fraction and an
\ optional `e`/`E` exponent: [+|-] digits [. digits] [ (e|E) [+|-] digits ].
\ Conversion keeps 18 significant decimal digits and combines the remaining
\ digit positions with the exponent before scaling. Binary64 arithmetic rounds
\ the result; this is not a correctly rounded arbitrary-precision parser.
\ Overflow yields signed infinity, underflow signed zero, and zero stays zero.
\ Exponents must fit signed i64; engine-shaped decimals retain engine admission.

require lib/string.f                         \ STR-DIGITS? / STR-DIGIT-VALUE / STR-MINUS / STR-PLUS
require lib/adt/option.f                      \ option<CAD-NUM:index> for STR:INDEX-OF (switchover wave A)

46 constant FL-DOT
101 constant FL-E-LOWER
69 constant FL-E-UPPER
400 constant FL-EXP-MAX                     \ enough for any bounded significand
18 constant FL-SIG-DIGITS                   \ decimal prefix fits a signed cell
22 constant FL-SCALE-CHUNK                  \ 10^0 through 10^22 are exact binary64
$7FFFFFFFFFFFFFFF constant FL-MAX-I64
$8000000000000000 constant FL-MIN-I64

variable FL-EXPV                            \ parsed exponent value
variable FL-VALID                           \ exponent validity flag
variable FL-MANT                            \ retained significant prefix
variable FL-KEPT                            \ retained digit count
variable FL-SCALE                           \ omitted digits minus fraction length

\ ---- pinned-raw residual: STR:INDEX-OF returns a checked option<CAD-NUM:index>,
\ but the found position immediately drives raw pointer/length arithmetic
\ (a+dpos, u-dpos, epos as a substring bound), so it is projected to a bare n
\ through the existing private CAD-NUM INDEX>N (no new TRUSTED). Retire with
\ TVK-RAW (habu-nominal-storage-raw-a3430ef2).
package CAD-NUM
public
: FL-IX>N ( CAD-NUM:index -- n ) INDEX>N ;
;package

\ ---- powers of ten --------------------------------------------------------
: POW10+ ( n -- r ) {: k:n :}
   \ Preserve the public helper's empty product for negative exponents.
   1.0 k 0 max 0 ?do
      10.0 f*
   loop ;


: FL-CLAMP-EXP ( n -- n ) {: k :}
   k FL-EXP-MAX > if FL-EXP-MAX exit then
   k FL-EXP-MAX negate < if FL-EXP-MAX negate exit then
   k ;


: FL-SCALE-UP ( r n -- r ) {: k:n :}
   k FL-SCALE-CHUNK / 0 ?do FL-SCALE-CHUNK POW10+ f* loop
   k FL-SCALE-CHUNK mod POW10+ f* ;


: FL-SCALE-DOWN ( r n -- r ) {: k:n :}
   k FL-SCALE-CHUNK / 0 ?do FL-SCALE-CHUNK POW10+ f/ loop
   k FL-SCALE-CHUNK mod POW10+ f/ ;


: FL-SCALE-F ( r n -- r ) FL-CLAMP-EXP {: k:n :}
   dup 0.0 f= if exit then
   k 0 >= if k FL-SCALE-UP else k negate FL-SCALE-DOWN then ;


: POW10 ( n -- r )
   1.0 swap FL-SCALE-F ;


\ Saturate only after combining the exponent with the full digit displacement.
: FL-ADD-EXP ( n n -- n ) {: exponent:n shift:n :}
   shift 0 > if
      exponent FL-MAX-I64 shift - > if FL-EXP-MAX exit then
   then
   shift 0 < if
      exponent FL-MIN-I64 shift - < if FL-EXP-MAX negate exit then
   then
   exponent shift + FL-CLAMP-EXP ;


: FL-RESET-SIG ( n -- )
   FL-SCALE ! 0 FL-MANT ! 0 FL-KEPT ! ;


: FL-KEEP-DIGIT ( n -- ) {: digit:n :}
   FL-MANT @ 0= digit 0= and if exit then
   FL-KEPT @ FL-SIG-DIGITS < if
      FL-MANT @ 10 * digit + FL-MANT !
      1 FL-KEPT +!
   else
      1 FL-SCALE +!
   then ;


: FL-KEEP-DIGITS ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 ?do a i + c@ STR-DIGIT-VALUE FL-KEEP-DIGIT loop ;


: FL-SIG-VALUE ( n -- r )
   FL-SCALE @ FL-ADD-EXP
   FL-MANT @ s>f swap FL-SCALE-F ;

\ ---- digit string -> double -----------------------------------------------
\ Unsigned run of decimal digits. Empty is valid and yields 0.0; any non-digit
\ byte is rejected.
: FL-DIGITS>F ( ptr u8 n -- option<r> ) {: a:ptr u:n :}   \ SOME digit-run value (empty -> SOME 0.0), NONE on a non-digit
   u 0= if 0.0 OPTION:SOME exit then
   a u STR-DIGITS? 0= if OPTION:NONE exit then
   0 FL-RESET-SIG
   a u FL-KEEP-DIGITS
   0 FL-SIG-VALUE OPTION:SOME ;

\ ---- field splitting ------------------------------------------------------
: FL-STRIP-SIGN ( ptr u8 n -- ptr u8 n bool ) {: a:ptr u :}
   u 0= if a 0 0 0= 0= exit then
   a c@ STR-MINUS = if a 1+ u 1- 0 0= exit then
   a c@ STR-PLUS  = if a 1+ u 1- 0 0= 0= exit then
   a u 0 0= 0= ;
: FL-FIND-E ( ptr u8 n -- option<CAD-NUM:index> ) {: a:ptr u:n :}   \ SOME index of e/E, else NONE
   a u STR:LENGTH FL-E-LOWER STR:INDEX-OF MATCH option
     none OF a u STR:LENGTH FL-E-UPPER STR:INDEX-OF ENDOF
     some OF OPTION:SOME ENDOF
   ;MATCH ;

\ STR>FLOAT accepts signs, exponent notation and a trailing dot beyond the
\ engine literal grammar. For the spellings that are engine-shaped decimals,
\ ask the engine's own reader whether its cell accumulators admit the value.
: FL-ENGINE-DECIMAL? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 0= if 0 0= 0= exit then
   a c@ STR-PLUS = if 0 0= 0= exit then
   a u FL-FIND-E MATCH option
     none OF 0 0= 0= ENDOF
     some OF drop 0 0= ENDOF
   ;MATCH if 0 0= 0= exit then
   a u STR:LENGTH FL-DOT STR:INDEX-OF MATCH option
     none OF 0 0= 0= ENDOF
     some OF CAD-NUM:FL-IX>N u 1- <> ENDOF
   ;MATCH ;

: FL-ENGINE-DECIMAL-ADMITTED? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u FL-ENGINE-DECIMAL? 0= if 0 0= exit then
   a u num-parse and nip ;

\ ---- significand (no sign, no exponent) -----------------------------------
\ Validate both halves before keeping their significant prefix and net scale.
: FL-SCALED-SIG ( ptr u8 n n -- option<r> ) {: a:ptr u:n exponent:n :}
   a u STR:LENGTH FL-DOT STR:INDEX-OF MATCH option    \ split at the dot: ilen fa flen
     none OF u  a u +  0 ENDOF                        \ no dot: int = whole string, empty fraction
     some OF CAD-NUM:FL-IX>N {: dpos:n :} dpos  a dpos 1+ +  u dpos 1+ - ENDOF
   ;MATCH {: ilen:n fa:ptr flen:n :}
   ilen flen + 0= if OPTION:NONE exit then       \ no digits at all: "" and "." rejected
   ilen 0 > if a ilen STR-DIGITS? 0= if OPTION:NONE exit then then
   flen 0 > if fa flen STR-DIGITS? 0= if OPTION:NONE exit then then
   flen negate FL-RESET-SIG
   a ilen FL-KEEP-DIGITS
   fa flen FL-KEEP-DIGITS
   exponent FL-SIG-VALUE OPTION:SOME ;


: FL-SIG ( ptr u8 n -- option<r> )
   0 FL-SCALED-SIG ;

\ ---- exponent -------------------------------------------------------------
\ FL-EXP-AT parses the exponent text after position epos, records it, and
\ returns the mantissa length (epos). FL-PARSE-EXP handles the no-exponent case.
: FL-EXP-AT ( ptr u8 n n -- n ) {: a:ptr u epos :}
   a epos 1+ + u epos 1+ - STR>NUMBER? MATCH option
     none OF 0 FL-VALID ! ENDOF
     some OF FL-EXPV ! ENDOF
   ;MATCH
   epos ;
: FL-PARSE-EXP ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u FL-FIND-E MATCH option
     none OF 0 FL-EXPV ! u ENDOF                    \ no exponent: exp 0, mantissa = whole string
     some OF CAD-NUM:FL-IX>N {: epos:n :} a u epos FL-EXP-AT ENDOF
   ;MATCH ;

\ ---- public entry ---------------------------------------------------------
: STR>FLOAT ( ptr u8 n -- option<r> ) {: a0:ptr u0:n :}
   a0 u0 FL-ENGINE-DECIMAL-ADMITTED? {: admitted:bool :}
   -1 FL-VALID !
   a0 u0 FL-STRIP-SIGN {: a:ptr u neg :}
   a u FL-PARSE-EXP {: mlen :}
   admitted 0= if OPTION:NONE exit then
   u 0= FL-VALID @ 0= or if OPTION:NONE exit then
   a mlen FL-EXPV @ FL-SCALED-SIG MATCH option
     none OF OPTION:NONE exit ENDOF                 \ bad significand -> NONE
     some OF ENDOF                                  \ SOME significand left on the stack
   ;MATCH
   neg if fnegate then
   OPTION:SOME ;
