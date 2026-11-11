\ create-axiom-test.f - `create`'s effect row (src/habu/prims.f `EPRIM: create
\ EPRIM;`) is empty: `create` parses a name and defines it (src/habu/habu1.f
\ BCREATE); the word gets `-- ptr a` from its definer (src/habu/interpreter.f
\ DEF-ADDR-PUBLISH). Restoring `PE-PTR-A PE-OUT` reds INFER-CASE's inferred effect
\ and both callers.

require src/lib/test.f
require src/habu/verify-source.f
require src/compiler/native/compiler.f

package CREATE-AXIOM-TEST
private

0 constant LOADED               \ what `catch` answers for a source that loaded
70 constant REJECT-RC           \ src/core/check-hook.f CHECK CHECK-RC (private there)

\ `evaluate-closed` loads a definition as a source file loads one. At top level
\ what these publish is global, the position the inference case needs.

\ Controls: `here` is measured beside it; a word `create` made keeps `-- ptr a`;
\ `create` in a string or a comment certifies as if absent.
\ The parser-only definer regression calls the production source verifier
\ directly.
\ NDICT:SPELL-ARITY resolves a spelling's active effect through the checker's
\ effect-read API and answers the pair in stack cells: asking it is asking the
\ row.
\ The word `create` makes keeps `-- ptr a`: its definer publishes it against the
\ new name as it emits the body.
: MADE-WORD ( -- )
   s" create CAX-DAT 8 allot" evaluate-closed ;

: MADE-USE ( -- )
   s" : CAX-USE ( -- n ) CAX-DAT @ ;" evaluate-closed ;

: MADE-CASE ( -- )
   s" a word `create` made still pushes one address" T-LABEL
   [: MADE-WORD ;] LOADED TTHROWSQ
   s" CAX-DAT" NDICT:SPELL-ARITY {: din:n dout:n :}
   din 0 T=  dout 1 T=

   s" and a checked body may still take it as one" T-LABEL
   [: MADE-USE ;] LOADED TTHROWSQ
   s" 41 CAX-DAT !  CAX-USE" TEST-EVAL:N 41 T= ;

: LOAD-EMPTY ( -- )      s" : CAX-B1 ( -- ) create ;" evaluate-closed ;
: LOAD-PTR-A ( -- )      s" : CAX-B2 ( -- ptr a ) create ;" evaluate-closed ;
: LOAD-N ( -- )          s" : CAX-B3 ( -- n ) create ;" evaluate-closed ;
: LOAD-ENUM-HALF ( -- )  s" : CAX-B4 ( n -- n ) dup create , 1 + ;" evaluate-closed ;
: LOAD-IN-STRING ( -- )  s\" : CAX-B5 ( -- ptr u8 n ) s\q create\q ;" evaluate-closed ;
: LOAD-IN-COMMENT ( -- ) s" : CAX-B6 ( -- ) ( create ) ;" evaluate-closed ;
\ `here` hands back a raw storage address, so its wrapper declares `ptr n`: a
\ parametric pointee over raw storage is E-NONPARAMETRIC-EFFECT (docs/forth.md
\ "Real types, not reflexive `n`."). What this case measures is the arity.
: LOAD-HERE ( -- )       s" : CAX-B7 ( -- ptr n ) here ;" evaluate-closed ;
\ A body that allots after `create` is the sharpest form of `create` moving no
\ stack cell.
: LOAD-BUFFER-HALF ( -- ) s" : CAX-D3 ( n -- ) create allot ;" evaluate-closed ;
\ The definer's runtime `create` gives the word it makes `-- ptr a`, so a checked
\ body can name it, and a package's public reader can name one its private
\ definer made.
: LOAD-CELL-READ ( -- )   s" : CAX-B8 ( -- n ) CAX-B4-CELL @ ;" evaluate-closed ;
: LOAD-CELL-N ( -- )      s" : CAX-B9 ( -- n ) CAX-B4-CELL ;" evaluate-closed ;
: LOAD-PKG-MADE ( -- )
   s" package CAX-PKG  : CAX-PMK ( -- ) create 0 , ;  CAX-PMK CAX-PW  public  : CAX-PRD ( -- n ) CAX-PW @ ;  ;package"
   evaluate-closed ;

: BODY-CASE ( -- )
   s" a body whose whole content is `create` declares nothing and loads" T-LABEL
   [: LOAD-EMPTY ;] LOADED TTHROWSQ

   s" and the machine agrees: running it leaves the cell under it on top" T-LABEL
   s" 99 CAX-B1 CAX-B1-NAME" TEST-EVAL:N 99 T=
   s" CAX-B1-NAME NULL-PTR -" TEST-EVAL:N 0 T<>

   s" declaring the address it used to claim is refused at the token" T-LABEL
   [: LOAD-PTR-A ;] REJECT-RC TTHROWSQ

   s" and so is any other output - the row states an effect, not a wildcard" T-LABEL
   [: LOAD-N ;] REJECT-RC TTHROWSQ

   s" the definer half src/core/enums.f ENUM+ ships balances" T-LABEL
   [: LOAD-ENUM-HALF ;] LOADED TTHROWSQ

   s" and running it defines the name and stores the counter in it" T-LABEL
   s" 5 CAX-B4 CAX-B4-CELL" TEST-EVAL:N 6 T=
   s" CAX-B4-CELL @" TEST-EVAL:N 5 T=

   s" the definer half that allots after `create` balances" T-LABEL
   [: LOAD-BUFFER-HALF ;] LOADED TTHROWSQ

   s" the word the definer made has its address row, so a checked body reads it" T-LABEL
   [: LOAD-CELL-READ ;] LOADED TTHROWSQ
   s" CAX-B8" TEST-EVAL:N 5 T=

   s" and the row is a typed address, not a wildcard" T-LABEL
   [: LOAD-CELL-N ;] REJECT-RC TTHROWSQ

   s" a word a definer makes inside a package is read by its public reader" T-LABEL
   [: LOAD-PKG-MADE ;] LOADED TTHROWSQ
   s" CAX-PKG:CAX-PRD" TEST-EVAL:N 0 T=

   s" `create` inside a string is a string" T-LABEL
   [: LOAD-IN-STRING ;] LOADED TTHROWSQ
   s\" CAX-B5 s\q create\q STR=" TEST-EVAL:FLAG TTRUE

   s" `create` inside a comment is a comment" T-LABEL
   [: LOAD-IN-COMMENT ;] LOADED TTHROWSQ
   s" CAX-B6" NDICT:SPELL-ARITY {: b6in:n b6out:n :}
   b6in 0 T=  b6out 0 T=

   s" and `here`, which really does push an address, still declares one" T-LABEL
   [: LOAD-HERE ;] LOADED TTHROWSQ
   s" CAX-B7" NDICT:SPELL-ARITY {: b7in:n b7out:n :}
   b7in 0 T=  b7out 1 T= ;

\ A signed body never reads the row (src/core/checker.f DEFINER-TOK models
\ `create` before TRY-PRIMS in DO-TOK). Its readers ask the effect store by
\ name: inference for an unsigned body, and src/compiler/native/dict.f
\ NDICT:SPELL-ARITY.
\ A body with no declared signature reaches the primitive row, the leg
\ DEFINER-TOK does not shadow, and its inferred effect is recorded under its
\ name for every later caller.
: INFER-MK ( -- )       s" : CAX-MK create ;" evaluate-closed ;
: INFER-BAD ( -- )      s" : CAX-C1 ( -- ptr a ) CAX-MK ;" evaluate-closed ;
: INFER-GOOD ( -- )     s" : CAX-C2 ( -- ) CAX-MK ;" evaluate-closed ;

: INFER-CASE ( -- )
   s" an unsigned body calling `create` records what it really leaves" T-LABEL
   [: INFER-MK ;] LOADED TTHROWSQ
   s" CAX-MK" NDICT:SPELL-ARITY {: mkin:n mkout:n :}
   mkin 0 T=  mkout 0 T=

   s" so a caller claiming a cell from it is refused, not certified" T-LABEL
   [: INFER-BAD ;] REJECT-RC TTHROWSQ

   s" and the caller that claims nothing certifies and runs" T-LABEL
   [: INFER-GOOD ;] LOADED TTHROWSQ
   s" CAX-C2 CAX-C2-CELL 8 allot  7 CAX-C2-CELL !  CAX-C2-CELL @" TEST-EVAL:N 7 T= ;

\ `variable` and `constant` are parser-only keywords, rejected in colon bodies
\ through E-UNDEFINED. `variable` needs no primitive row: its hook body uses
\ `create`.
\ `constant` keeps its one-cell input row because C-CONSTANT still checks the
\ synthetic body `NAME constant` before publishing the created word's effect.
\ `variable` and `constant` are parser-only defining keywords: the engine cannot
\ compile either spelling in a colon body. Exercise the production source
\ verifier, including inert spellings that would fool a text search.
: VC-VARIABLE ( -- )
   s" : CAX-V1 ( -- ) variable ;" VERIFY:SOURCE-BUF-IN-SCOPE ;

: VC-CONSTANT ( -- )
   s" : CAX-K1 constant ;" VERIFY:SOURCE-BUF-IN-SCOPE ;

: VC-INERT ( -- )
   s\" : CAX-V2 ( -- ptr u8 n ) s\q variable\q ;\n: CAX-K2 ( -- ) ( constant ) ;"
   VERIFY:SOURCE-BUF-IN-SCOPE ;

: VC-REJECT-CASE ( -- )
   s" parser-only definers are rejected in colon bodies" T-LABEL
   [: VC-VARIABLE ;] REJECT-RC TTHROWSQ
   [: VC-CONSTANT ;] REJECT-RC TTHROWSQ

   s" the same spellings stay inert in strings and comments" T-LABEL
   [: VC-INERT ;] LOADED TTHROWSQ ;

public

: RUN ( -- )
   T-RESET
   MADE-CASE
   BODY-CASE
   INFER-CASE
   VC-REJECT-CASE
   T-REPORT ;

;package

CREATE-AXIOM-TEST:RUN
