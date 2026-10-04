\ native-div-image.f - the stripped application test/compiler/native-div-refusal.f
\ builds with tools/hb-build.f. The build compiles it at tier 1, so QUOT's zero
\ guard branches to the engine's (DIV-ZERO) helper, and the image holds that
\ helper only if the stripped closure followed the branch. MAIN prints the code
\ it caught: an uncaught -6400 would exit 0, a multiple of 256, and say nothing.
\ The same executable checks signed literal-two remainder, a stack alias,
\ a divisor chosen at a control-flow join, and a parameter divisor.
package NDIV-IMAGE
private
variable DIVISOR
: QUOT ( n -- n ) DIVISOR @ / ;
: REM2 ( n -- n ) 2 mod ;
: ALIAS ( n -- n n ) dup 2 mod ;
: CHOICE ( n bool -- n ) if 2 else 3 then mod ;
: DYNAMIC ( n n -- n ) mod ;
public
: RUN ( -- )
   0 DIVISOR ! [: 7 QUOT drop ;] catch .
   -7 REM2 . -8 REM2 . 0 REM2 . 7 REM2 . 8 REM2 .
   $8000000000000000 REM2 . $7FFFFFFFFFFFFFFF REM2 .
   -7 ALIAS . .
   -8 true CHOICE . -8 false CHOICE .
   -7 2 DYNAMIC . ;
;package
: MAIN ( -- ) NDIV-IMAGE:RUN ;
