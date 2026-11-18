split-str
\ Pure source token classes shared by the loader and the interpreter's reader.
\ STORAGE CLASS. CALLER-OWNED: pure predicates over the caller's bytes; no
\ module state.
require src/lib/string.f

package SOURCE-SYNTAX
public

: PARSING-KEYWORD? ( str -- bool ) {: a:^ u:n :}
   a u s" char" STR=CI if STR-TRUE exit then
   a u s" [char]" STR=CI if STR-TRUE exit then
   a u s" '" STR= if STR-TRUE exit then
   a u s" [']" STR= ;

\ In a body only the bracketed keywords and `postpone` parse, once no live
\ local claims the token; `char` and `'` there are names.
: BODY-PARSING-KEYWORD? ( str -- bool ) {: a:^ u:n :}
   a u s" [char]" STR=CI if STR-TRUE exit then
   a u s" postpone" STR=CI if STR-TRUE exit then
   a u s" [']" STR= ;

\ The engine opens a definition body at these.
: BODY-OPENER? ( str -- bool ) {: a:^ u:n :}
   a u s" :" STR= if STR-TRUE exit then
   a u s" TRUSTED:" STR=CI if STR-TRUE exit then
   a u s" kernel:" STR=CI ;

\ Block tokens alter local lifetime only after byte-exact local lookup.
: BLOCK-OPENER? ( str -- bool ) {: a:^ u:n :}
   a u s" if" STR=CI if STR-TRUE exit then
   a u s" begin" STR=CI if STR-TRUE exit then
   a u s" do" STR=CI if STR-TRUE exit then
   a u s" ?do" STR=CI if STR-TRUE exit then
   a u s" case" STR=CI if STR-TRUE exit then
   a u s" of" STR=CI if STR-TRUE exit then
   a u s" match" STR=CI if STR-TRUE exit then
   a u s" [:" STR= ;

: BLOCK-CLOSER? ( str -- bool ) {: a:^ u:n :}
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

: ROW-OPENER? ( str -- bool ) {: a:^ u:n :}
   a u s" EPRIM:" STR=CI if STR-TRUE exit then
   a u s" EPPRIM:" STR=CI ;

\ Both closers write the open row, so either ends a row of either family.
: ROW-CLOSER? ( str -- bool ) {: a:^ u:n :}
   a u s" EPRIM;" STR=CI if STR-TRUE exit then
   a u s" ECLOSE-PRIVATE" STR=CI ;

: ROW-ATOM? ( str -- bool ) {: a:^ u:n :}
   u 3 >= if a 3 s" PE-" STR=CI if STR-TRUE exit then then
   u 4 >= if a 4 s" ;PE-" STR=CI exit then
   STR-FALSE ;

: ROW-REF? ( str -- bool ) s" REF" STR=CI ;

;package
