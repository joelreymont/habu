\ float.f - checked decimal string -> IEEE-double parsing of
\ [+|-] digits [. digits] [ (e|E) [+|-] digits ]. It keeps 18 significant digits
\ and folds the rest into the exponent before scaling; binary64 arithmetic
\ rounds the result, so this is not a correctly rounded parser. Exponents must
\ fit signed i64; engine-shaped decimals keep the engine's admission.

require src/lib/string.f
require src/lib/string-option.f
require src/lib/adt/option.f

46 constant FL-DOT
101 constant FL-E-LOWER
69 constant FL-E-UPPER
400 constant FL-EXP-MAX                     \ enough for any bounded significand
18 constant FL-SIG-DIGITS                   \ decimal prefix fits a signed cell
18 constant FL-ENGINE-FRAC-DIGITS           \ 10^18 fits signed cell; 10^19 does not
22 constant FL-SCALE-CHUNK                  \ 10^0 through 10^22 are exact binary64
$7FFFFFFFFFFFFFFF constant FL-MAX-I64
$8000000000000000 constant FL-MIN-I64

variable FL-EXPV
variable FL-VALID
variable FL-MANT
variable FL-KEPT
variable FL-SCALE                           \ omitted digits minus fraction length

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


: FL-KEEP-DIGITS ( str -- ) {: s:str :}
   s STR:LEN 0 ?do s STR:DATA i + c@ STR:DIGIT-VALUE FL-KEEP-DIGIT loop ;


: FL-SIG-VALUE ( n -- r )
   FL-SCALE @ FL-ADD-EXP
   FL-MANT @ s>f swap FL-SCALE-F ;

\ ---- field splitting ------------------------------------------------------
: FL-STRIP-SIGN ( str -- str bool ) {: s:str :}
   s STR:LEN 0= if s 0 0= 0= exit then
   s STR:DATA c@ STR:MINUS = if s STR:DATA 1+ s STR:LEN 1- >str 0 0= exit then
   s STR:DATA c@ STR:PLUS  = if s STR:DATA 1+ s STR:LEN 1- >str 0 0= 0= exit then
   s 0 0= 0= ;
: FL-FIND-E ( str -- option<idx> ) {: s:str :}
   s FL-E-LOWER STR:INDEX-OF MATCH option
     none OF s FL-E-UPPER STR:INDEX-OF ENDOF
     some OF OPTION:SOME ENDOF
   ;MATCH ;

\ STR>FLOAT accepts signs, exponent notation and a trailing dot beyond the
\ engine literal grammar. For the spellings that are engine-shaped decimals,
\ ask the engine's own reader whether its cell accumulators admit the value.
: FL-ENGINE-DECIMAL? ( str -- bool ) {: s:str :}
   s STR:LEN 0= if 0 0= 0= exit then
   s STR:DATA c@ STR:PLUS = if 0 0= 0= exit then
   s FL-FIND-E MATCH option
     none OF 0 0= 0= ENDOF
     some OF drop 0 0= ENDOF
   ;MATCH if 0 0= 0= exit then
   s FL-DOT STR:INDEX-OF MATCH option
     none OF 0 0= 0= ENDOF
     some OF IDX>N s STR:LEN 1- <> ENDOF
   ;MATCH ;

\ EMIT-NUM's decimal literal limits: the integer magnitude and fractional
\ numerator must fit signed i64, and the 10^fraction scale must fit too. The
\ scale admits at most 18 fractional digits. A native literal's overflow is
\ rejected even when STR>FLOAT's more general exponent parser could scale it.
: FL-ADMIT-INT? ( str -- bool )
   {: s:str :}
   0
   s STR:LEN 0 ?do
      s STR:DATA i + c@ {: c:n :}
      c [char] 0 < c [char] 9 > or if drop false unloop exit then
      c [char] 0 - {: digit:n :}
      dup FL-MAX-I64 digit - 10 / > if drop false unloop exit then
      10 * digit +
   loop drop true ;

: FL-ENGINE-DECIMAL-ADMITTED? ( str -- bool )
   {: a0:str :}
   a0 FL-ENGINE-DECIMAL? 0= if true exit then
   a0 FL-STRIP-SIGN drop {: s:str :}
   s FL-DOT STR:INDEX-OF MATCH option
      none OF false exit ENDOF
      some OF IDX>N ENDOF
   ;MATCH {: dot:n :}
   s STR:DATA dot >str FL-ADMIT-INT? 0= if false exit then
   s STR:LEN dot 1+ - {: frac:n :}
   frac 0= frac FL-ENGINE-FRAC-DIGITS > or if false exit then
   s STR:DATA dot 1+ + frac >str FL-ADMIT-INT? ;

\ ---- significand (no sign, no exponent) -----------------------------------
: FL-SCALED-SIG ( str n -- option<r> ) {: s:str exponent:n :}
   s FL-DOT STR:INDEX-OF MATCH option                  \ split at the dot: ilen fa flen
     none OF s STR:LEN  s STR:DATA s STR:LEN +  0 ENDOF                        \ no dot: int = whole string, empty fraction
     some OF IDX>N {: dpos:n :} dpos  s STR:DATA dpos 1+ +  s STR:LEN dpos 1+ - ENDOF
   ;MATCH {: ilen:n fa:^ flen:n :}
   ilen flen + 0= if OPTION:NONE exit then       \ no digits at all: "" and "." rejected
   ilen 0 > if s STR:DATA ilen >str STR:DIGITS? 0= if OPTION:NONE exit then then
   flen 0 > if fa flen >str STR:DIGITS? 0= if OPTION:NONE exit then then
   flen negate FL-RESET-SIG
   s STR:DATA ilen >str FL-KEEP-DIGITS
   fa flen >str FL-KEEP-DIGITS
   exponent FL-SIG-VALUE OPTION:SOME ;

\ ---- exponent -------------------------------------------------------------
: FL-EXP-AT ( str n -- n ) {: s:str epos :}
   s STR:DATA epos 1+ + s STR:LEN epos 1+ - >str STR:>NUMBER? MATCH option
     none OF 0 FL-VALID ! ENDOF
     some OF FL-EXPV ! ENDOF
   ;MATCH
   epos ;
: FL-PARSE-EXP ( str -- n ) {: s:str :}
   s FL-FIND-E MATCH option
     none OF 0 FL-EXPV ! s STR:LEN ENDOF                    \ no exponent: exp 0, mantissa = whole string
     some OF IDX>N {: epos:n :} s epos FL-EXP-AT ENDOF
   ;MATCH ;

\ ---- public entry ---------------------------------------------------------
: STR>FLOAT ( str -- option<r> ) {: a0:str :}
   a0 FL-ENGINE-DECIMAL-ADMITTED? {: admitted:bool :}
   -1 FL-VALID !
   a0 FL-STRIP-SIGN {: s:str neg :}
   s FL-PARSE-EXP {: mlen :}
   admitted 0= if OPTION:NONE exit then
   s STR:LEN 0= FL-VALID @ 0= or if OPTION:NONE exit then
   s STR:DATA mlen >str FL-EXPV @ FL-SCALED-SIG MATCH option
     none OF OPTION:NONE exit ENDOF
     some OF ENDOF
   ;MATCH
   neg if fnegate then
   OPTION:SOME ;
