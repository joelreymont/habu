\ Pure source token classes shared by the loader and the native outer reader.
\ STORAGE CLASS. CALLER-OWNED: pure predicates over the caller's bytes; no
\ module state.
require src/lib/string.f

package SOURCE-SYNTAX
public

\ A parsing word takes the next whitespace-delimited token raw.
: PARSING-KEYWORD? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" char" STR=CI if STR-TRUE exit then
   a u s" [char]" STR=CI if STR-TRUE exit then
   a u s" '" STR= if STR-TRUE exit then
   a u s" [']" STR= ;

\ In a body only the bracketed keywords parse, once no live local claims the
\ token; `char` and `'` there are names.
: BODY-PARSING-KEYWORD? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" [char]" STR=CI if STR-TRUE exit then
   a u s" [']" STR= ;

\ The engine opens a definition body at these (src/habu/habu2.f
\ EM-INTERPRET-COLON, C-TRUSTED): `:` byte for byte, the others case-folded.
: BODY-OPENER? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" :" STR= if STR-TRUE exit then
   a u s" TRUSTED:" STR=CI if STR-TRUE exit then
   a u s" kernel:" STR=CI ;

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
