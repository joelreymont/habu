\ native-div-image.f - the stripped application test/compiler/native-div-refusal.f
\ builds with tools/hb-build.f. The build compiles it at tier 1, so QUOT's zero
\ guard branches to the engine's (DIV-ZERO) helper, and the image holds that
\ helper only if the stripped closure followed the branch. MAIN prints the code
\ it caught: an uncaught -6400 would exit 0, a multiple of 256, and say nothing.
package NDIV-IMAGE
private
variable DIVISOR
: QUOT ( n -- n ) DIVISOR @ / ;
public
: RUN ( -- ) 0 DIVISOR ! [: 7 QUOT drop ;] catch . ;
;package
: MAIN ( -- ) NDIV-IMAGE:RUN ;
