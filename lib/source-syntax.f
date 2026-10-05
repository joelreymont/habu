\ Pure source token classes shared by the loader and the native outer reader.
require lib/string.f

package SOURCE-SYNTAX
public

\ A parsing word takes the next whitespace-delimited token raw. A body checks
\ for a live local before asking this question.
: PARSING-KEYWORD? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" char" STR=CI if STR-TRUE exit then
   a u s" [char]" STR=CI if STR-TRUE exit then
   a u s" '" STR= if STR-TRUE exit then
   a u s" [']" STR= ;

\ Block tokens alter local lifetime only after byte-exact local lookup.
: BLOCK-OPENER? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" if" STR=CI if STR-TRUE exit then
   a u s" begin" STR=CI if STR-TRUE exit then
   a u s" do" STR=CI if STR-TRUE exit then
   a u s" ?do" STR=CI if STR-TRUE exit then
   a u s" case" STR=CI if STR-TRUE exit then
   a u s" of" STR=CI if STR-TRUE exit then
   a u s" match" STR=CI if STR-TRUE exit then
   a u s" [:" STR= ;

: BLOCK-CLOSER? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" then" STR=CI if STR-TRUE exit then
   a u s" until" STR=CI if STR-TRUE exit then
   a u s" repeat" STR=CI if STR-TRUE exit then
   a u s" again" STR=CI if STR-TRUE exit then
   a u s" loop" STR=CI if STR-TRUE exit then
   a u s" +loop" STR=CI if STR-TRUE exit then
   a u s" endof" STR=CI if STR-TRUE exit then
   a u s" endcase" STR=CI if STR-TRUE exit then
   a u s" ;match" STR=CI if STR-TRUE exit then
   a u s" ;]" STR= ;

;package
