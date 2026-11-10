\ input-underflow-refusals.f - rejected programs for "a definition may not
\ consume below its declared inputs". The refusal names the consuming token and
\ both counts. Controls: a short and mistyped call keeps its own mismatch, and a
\ body spending exactly what it declared still certifies. The packet's declared
\ effect is the signature's rows, whatever the body bound. Cases run through
\ `evaluate-closed`, the path `bin/hb --load` takes; a refusal is a throw.

require src/lib/test.f
require src/lib/string.f

\ The consumers the evaluated programs call. They are file-global because the
\ programs below are compiled at run time, in the scope a real program writes
\ them in.
: UFC ( n -- ) drop ;
: UFC2 ( n n -- ) 2drop ;

\ The cell a speculative candidate refuses before the real row is applied.
variable IUF-V

package INPUT-UNDERFLOW-TEST

private

70 constant CHECK-RC                 \ the engine refusing a definition it cannot certify

create DIAG-BUF 8192 allot
8192 constant DIAG-CAP

: ARM ( -- )   \ capture the next diagnostic as JSON instead of printing it
   DIAG-BUF DIAG-CAP DIAG-BUFFER!  true DIAG-JSON! ;

: DISARM ( -- )
   false DIAG-JSON!  DIAG-BUFFER-OFF ;

: CODE$ ( -- ptr u8 n )
   S\" \"code\":\"E-INPUT-UNDERFLOW\"" ;

: REPAIR$ ( -- ptr u8 n )
   S\" \"repair_class\":\"supply_missing_input\"" ;

: MISMATCH$ ( -- ptr u8 n )
   S\" \"code\":\"E-MISMATCH\"" ;

: HAS? ( ptr u8 n -- )
   DIAG-BUFFER$ 2swap CONTAINS? TTRUE ;

: LACKS? ( ptr u8 n -- )
   DIAG-BUFFER$ 2swap CONTAINS? TFALSE ;

: TOKEN=? ( ptr u8 n -- bool )
   SB-RESET
   S\" \"token\":\"" SB-APPEND  SB-APPEND  34 SB-APPEND-C
   DIAG-BUFFER$ SB$ CONTAINS? ;

: TOKEN? ( ptr u8 n -- )
   TOKEN=? TTRUE ;

: NOT-TOKEN? ( ptr u8 n -- )
   TOKEN=? TFALSE ;

: SHORT-BY? ( ptr u8 n ptr u8 n -- ) {: needs:ptr needsu:n has:ptr hasu:n :}
   SB-RESET
   s" input underflow: the call takes more cells than the definition's declared inputs leave (needs " SB-APPEND
   needs needsu SB-APPEND  s" , has " SB-APPEND  has hasu SB-APPEND  s" )" SB-APPEND
   SB$ HAS? ;

: NAMED ( -- )
   CODE$ HAS?  REPAIR$ HAS? ;

: DECLARED? ( ptr u8 n -- )
   SB-RESET
   S\" \"declared_effect\":\"" SB-APPEND  SB-APPEND  34 SB-APPEND-C
   SB$ HAS? ;

: ROWS? ( ptr u8 n ptr u8 n -- )   \ the packet's expected and actual rows
   {: e:ptr eu:n a:ptr au:n :}
   SB-RESET
   S\" \"expected\":\"" SB-APPEND  e eu SB-APPEND
   S\" \",\"actual\":\"" SB-APPEND  a au SB-APPEND  34 SB-APPEND-C
   SB$ HAS? ;

\ ---- the shortfall itself ----------------------------------------------------

: CASE-BARE ( -- )
   s" a call with nothing on the stack is refused at the call" T-LABEL
   ARM
   [: s" : IUF-BARE ( -- n ) UFC 0 ;" evaluate-closed ;] CHECK-RC TTHROWSQ
   NAMED  s" UFC" TOKEN?  s" 1" s" 0" SHORT-BY?
   s" 0" NOT-TOKEN?
   DISARM ;

: CASE-TWO-DEEP ( -- )
   s" a two-cell call over one declared input names both depths" T-LABEL
   ARM
   [: s" : IUF-TWO ( n -- n ) UFC2 0 ;" evaluate-closed ;] CHECK-RC TTHROWSQ
   NAMED  s" UFC2" TOKEN?  s" 2" s" 1" SHORT-BY?
   DISARM ;

\ Binding the declared input into a local does not make another one appear; the
\ call still owns the refusal.
: CASE-AFTER-LOCALS ( -- )
   s" a call after a locals binding is still refused at the call" T-LABEL
   ARM
   [: s" : IUF-LOC ( n -- n ) {: x:n :} UFC x ;" evaluate-closed ;] CHECK-RC TTHROWSQ
   NAMED  s" UFC" TOKEN?  s" 1" s" 0" SHORT-BY?
   DISARM ;

\ The binder consumes too, so an underflowing `{: :}` group is refused at the
\ token that closes it rather than somewhere later in the body.
: CASE-LOCALS-BINDER ( -- )
   s" a locals group with nothing to bind is refused at its own closer" T-LABEL
   ARM
   [: s" : IUF-BIND ( -- ) {: x:n :} ;" evaluate-closed ;] CHECK-RC TTHROWSQ
   NAMED  s" :}" TOKEN?
   DISARM ;

\ A refusal raised on a candidate row the checker then abandons must not put its
\ name on this one. `IUF-V @ cell+` refuses the `ptr a -- ptr a` candidate
\ (raw cell, no address) before succeeding on `n -- n`, and the underflow two
\ tokens later is what the reader has to be sent to.
: CASE-OVER-ABANDONED-CANDIDATE ( -- )
   s" an abandoned candidate's refusal does not rename this one" T-LABEL
   ARM
   [: s" : IUF-SPEC ( -- n ) IUF-V @ cell+ drop drop 0 ;" evaluate-closed ;] CHECK-RC TTHROWSQ
   NAMED  s" drop" TOKEN?
   S\" \"code\":\"E-RAW-CELL-PTR\"" LACKS?
   DISARM ;

\ ---- the declared effect -----------------------------------------------------

\ A step that reaches under the declared inputs binds the row below them; the
\ packet still declares only the one input the signature wrote.
: CASE-DECLARED-ROWS ( -- )
   s" an underflow's declared effect is the signature as written" T-LABEL
   ARM
   [: s" : IUF-PLUS ( n -- n ) + ;" evaluate-closed ;] CHECK-RC TTHROWSQ
   NAMED  s" n -- n " DECLARED?
   DISARM ;

\ A body that specialises a declared variable binds it; the packet still
\ declares the variable.
: CASE-DECLARED-VARIABLE ( -- )
   s" a specialised variable stays a variable in the declared effect" T-LABEL
   ARM
   [: s" : IUF-PARAM ( a -- a ) 1 + ;" evaluate-closed ;] CHECK-RC TTHROWSQ
   s" a -- a " DECLARED?
   DISARM ;

\ A quotation's own rows hold no cells; the packet names them, in the
\ renderer's letters, where the signature wrote R and S.
: CASE-DECLARED-QUOTATION-ROWS ( -- )
   s" a quotation's own rows stay named in the declared effect" T-LABEL
   ARM
   [: s" : IUF-QROW ( [ R -- S ] -- [ R -- S ] ) swap ;" evaluate-closed ;] CHECK-RC TTHROWSQ
   s" [ A -- B ] -- [ A -- B ] " DECLARED?
   DISARM ;

\ A fresh name is one identity per spelling in a signature, so the packet
\ writes it as the signature does, with no ordinal.
: CASE-DECLARED-FRESH ( -- )
   s" a fresh name stays as written in the declared effect" T-LABEL
   ARM
   [: s" : IUF-FRESH ( n -- fresh-region-a ) + ;" evaluate-closed ;] CHECK-RC TTHROWSQ
   s" n -- fresh-region-a " DECLARED?
   DISARM ;

\ A quantifier after a free variable takes the next letter in both passes, not
\ the variable's.
: CASE-DECLARED-BINDER ( -- )
   s" a quantifier keeps its own name in the declared effect" T-LABEL
   ARM
   [: s" : IUF-FORALL ( a forall<p,[ read-view<p,p,u8> -- ]> -- a ) drop drop drop ;" evaluate-closed ;] CHECK-RC TTHROWSQ
   s" a forall<b,[ read-view<b,b,u8> -- ]> -- a " DECLARED?
   DISARM ;

\ ---- the refusal's rows ------------------------------------------------------

\ The drops after a refused `then` reach under the declared inputs and bind the
\ row below them; the refusal's rows are still those `then` saw.
: CASE-ROWS-AT-REFUSAL ( -- )
   s" a refusal's rows take no binding a later token makes" T-LABEL
   ARM
   [: s" : IUF-LATE ( ptr u8 bool -- ) if 1 then drop drop drop ;" evaluate-closed ;] CHECK-RC TTHROWSQ
   MISMATCH$ HAS?  s" then" TOKEN?
   s" ptr u8 " s" ptr u8 n " ROWS?
   DISARM ;

\ A row variable a later token specialises keeps its letter in the refusal's rows.
: CASE-ROWS-NAMES ( -- )
   s" a refusal's rows keep the letters a later token binds" T-LABEL
   ARM
   [: s" : IUF-NAMES ( a b bool -- ) if 1 else 0 0 = then rot 1 + drop drop ;" evaluate-closed ;] CHECK-RC TTHROWSQ
   s" a b bool " s" a b n " ROWS?
   DISARM ;

\ The repair class is read from the rows the refused call saw, not from the
\ deeper rows the drops after it bind.
: CASE-ROWS-REPAIR ( -- )
   s" a refusal's repair class reads the rows its token saw" T-LABEL
   ARM
   [: s" : IUF-LATE-CALL ( ptr u8 bool -- ) UFC2 drop drop drop drop ;" evaluate-closed ;] CHECK-RC TTHROWSQ
   s" n n " s" ptr u8 bool " ROWS?  S\" \"repair_class\":\"fix_type\"" HAS?
   DISARM ;

\ ---- the controls ------------------------------------------------------------

\ Short and mistyped: the type is the better answer and must not be swallowed by
\ the underflow.
: CASE-MISTYPED-CALL ( -- )
   s" a short call that is also mistyped keeps its own mismatch" T-LABEL
   ARM
   [: s" : IUF-TYPE ( r -- n ) UFC2 0 ;" evaluate-closed ;] CHECK-RC TTHROWSQ
   MISMATCH$ HAS?  CODE$ LACKS?  REPAIR$ LACKS?
   s" UFC2" TOKEN?
   DISARM ;

\ A call that takes exactly what the signature declared is not an underflow.
: CASE-EXACT ( -- )
   s" a call that spends exactly the declared inputs still certifies" T-LABEL
   [: s" : IUF-OK ( n -- n ) UFC 0 ;" evaluate-closed ;] 0 TTHROWSQ ;

\ Nor is a call under a value the body produced itself.
: CASE-PRODUCED ( -- )
   s" and a call over a value the body produced certifies too" T-LABEL
   [: s" : IUF-MADE ( -- n ) 0 UFC 0 ;" evaluate-closed ;] 0 TTHROWSQ ;

\ An UNSIGNED definition has no declared inputs to reach under: its base is
\ what the checker is inferring, so binding it is how inference works.
: CASE-UNSIGNED ( -- )
   s" an unsigned definition still infers its inputs from the body" T-LABEL
   [: s" : IUF-INFER UFC ;" evaluate-closed ;] 0 TTHROWSQ ;

\ A quotation owns a fresh base for the same reason, so consuming inside one is
\ the quotation taking an input, not the definition spending its caller's frame.
: CASE-QUOTATION ( -- )
   s" a quotation consuming inside the body takes its own input" T-LABEL
   [: s" : IUF-QUOT ( n -- n ) [: UFC 0 ;] execute ;" evaluate-closed ;] 0 TTHROWSQ ;

public

: RUN ( -- )
   T-RESET
   CASE-BARE
   CASE-TWO-DEEP
   CASE-AFTER-LOCALS
   CASE-LOCALS-BINDER
   CASE-OVER-ABANDONED-CANDIDATE
   CASE-DECLARED-ROWS
   CASE-DECLARED-VARIABLE
   CASE-DECLARED-QUOTATION-ROWS
   CASE-DECLARED-FRESH
   CASE-DECLARED-BINDER
   CASE-ROWS-AT-REFUSAL
   CASE-ROWS-NAMES
   CASE-ROWS-REPAIR
   CASE-MISTYPED-CALL
   CASE-EXACT
   CASE-PRODUCED
   CASE-UNSIGNED
   CASE-QUOTATION
   T-REPORT ;

;package

INPUT-UNDERFLOW-TEST:RUN
