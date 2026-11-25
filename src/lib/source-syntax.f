\ Pure source token classes shared by the loader and the interpreter's reader.
\ STORAGE CLASS. CALLER-OWNED: pure predicates over the caller's bytes; no
\ module state.
require src/lib/string.f

package SOURCE-SYNTAX
public

: PARSING-KEYWORD? ( str -- bool ) {: s:str :}
   s s" char" STR:=CI if STR:TRUE exit then
   s s" [char]" STR:=CI if STR:TRUE exit then
   s s" '" STR:= if STR:TRUE exit then
   s s" [']" STR:= ;

\ In a body only the bracketed keywords and `postpone` parse, once no live
\ local claims the token; `char` and `'` there are names.
: BODY-PARSING-KEYWORD? ( str -- bool ) {: s:str :}
   s s" [char]" STR:=CI if STR:TRUE exit then
   s s" postpone" STR:=CI if STR:TRUE exit then
   s s" [']" STR:= ;

\ The engine opens a definition body at these.
: BODY-OPENER? ( str -- bool ) {: s:str :}
   s s" :" STR:= if STR:TRUE exit then
   s s" TRUSTED:" STR:=CI if STR:TRUE exit then
   s s" kernel:" STR:=CI ;

\ Keywords only while the engine interprets: in a body `create` is a call and
\ the others are undefined, so a scanner asks these outside a definition.
: DEFINER-KEYWORD? ( str -- bool ) {: s:str :}
   s s" cast:" STR:=CI if STR:TRUE exit then
   s s" linear:" STR:=CI if STR:TRUE exit then
   s s" defer" STR:=CI if STR:TRUE exit then
   s s" create" STR:=CI if STR:TRUE exit then
   s s" variable" STR:=CI if STR:TRUE exit then
   s s" constant" STR:=CI ;

\ Block tokens alter local lifetime only after byte-exact local lookup.
: BLOCK-OPENER? ( str -- bool ) {: s:str :}
   s s" if" STR:=CI if STR:TRUE exit then
   s s" begin" STR:=CI if STR:TRUE exit then
   s s" do" STR:=CI if STR:TRUE exit then
   s s" ?do" STR:=CI if STR:TRUE exit then
   s s" case" STR:=CI if STR:TRUE exit then
   s s" of" STR:=CI if STR:TRUE exit then
   s s" match" STR:=CI if STR:TRUE exit then
   s s" [:" STR:= ;

: BLOCK-CLOSER? ( str -- bool ) {: s:str :}
   s s" then" STR:=CI if STR:TRUE exit then
   s s" until" STR:=CI if STR:TRUE exit then
   s s" repeat" STR:=CI if STR:TRUE exit then
   s s" again" STR:=CI if STR:TRUE exit then
   s s" loop" STR:=CI if STR:TRUE exit then
   s s" +loop" STR:=CI if STR:TRUE exit then
   s s" endof" STR:=CI if STR:TRUE exit then
   s s" endcase" STR:=CI if STR:TRUE exit then
   s s" ;match" STR:=CI if STR:TRUE exit then
   s s" ;]" STR:= ;

: ROW-OPENER? ( str -- bool ) {: s:str :}
   s s" EPRIM:" STR:=CI if STR:TRUE exit then
   s s" EPPRIM:" STR:=CI ;

\ Both closers write the open row, so either ends a row of either family.
: ROW-CLOSER? ( str -- bool ) {: s:str :}
   s s" EPRIM;" STR:=CI if STR:TRUE exit then
   s s" ECLOSE-PRIVATE" STR:=CI ;

: ROW-ATOM? ( str -- bool ) {: s:str :}
   s STR:LEN 3 >= if s STR:DATA 3 >str s" PE-" STR:=CI if STR:TRUE exit then then
   s STR:LEN 4 >= if s STR:DATA 4 >str s" ;PE-" STR:=CI if STR:TRUE exit then then
   s s" ETRUSTED-ONLY" STR:=CI if STR:TRUE exit then
   s s" EIMMEDIATE" STR:=CI if STR:TRUE exit then
   s s" EFIXED-ABI" STR:=CI ;

: ROW-REF? ( str -- bool ) s" REF" STR:=CI ;

;package
