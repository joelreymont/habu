\ input-underflow-refusals.f - the rejected programs for "a definition may not
\ consume below its declared inputs" (dot habu-name-an-input-45ee675e).
\
\ WHAT WAS WRONG. A call that took more cells than the signature left was
\ refused at the NEXT token and given no reason at all:
\ `: UFC ( n -- ) drop ;  : G ( -- n ) UFC 0 ;` answered `habu: in g: at '0'`
\ and stopped there. The call itself never failed -- the declared base is an
\ open row, so the step unified by binding it and checking carried on with a
\ stack that had grown under the definition's feet. Only CHECK-NO-BORROW, at
\ the boundary, noticed, by which time the pin sat on whatever token ran last.
\
\ WHAT IS PINNED HERE. The refusal names the token that consumes, and says how
\ far short it was: both counts, in the reason. The controls are the other half
\ of the rule -- a call that is short AND mistyped keeps its own mismatch
\ (naming the type is the better answer there, and that message must not move),
\ and a body that spends exactly what it declared still certifies.
\
\ These run against the live engine's own checker through `evaluate`, which is
\ the path `bin/hb --load` puts every definition through.

require lib/test.f
require lib/string.f

\ The consumers the evaluated programs call. They are file-global because the
\ programs below are compiled at run time, in the scope a real program writes
\ them in.
: UFC ( n -- ) drop ;
: UFC2 ( n n -- ) 2drop ;

\ The cell a speculative candidate refuses before the real row is applied.
variable IUF-V

package INPUT-UNDERFLOW-TEST

private

\ `evaluate` is the metaprogramming boundary the checker does not model, and it
\ is how a suite compiles a program that must be REFUSED: the refusal is a throw
\ out of the compile, not a value.
\ Retirement owner: habu-type-isolated-dynamic-244c0e2c.
TRUSTED: EV ( ptr u8 n -- ) evaluate ;

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

: TOKEN=? ( ptr u8 n -- bool )   \ is the diagnostic pinned to this token?
   SB-RESET
   S\" \"token\":\"" SB-APPEND  SB-APPEND  34 SB-APPEND-C
   DIAG-BUFFER$ SB$ CONTAINS? ;

: TOKEN? ( ptr u8 n -- )   \ the token the diagnostic is pinned to
   TOKEN=? TTRUE ;

: NOT-TOKEN? ( ptr u8 n -- )
   TOKEN=? TFALSE ;

\ Asserting the counts and not only the code is what keeps the reason honest:
\ a shortfall that says the wrong depth is a shortfall nobody can act on.
: SHORT-BY? ( ptr u8 n ptr u8 n -- ) {: needs:ptr needsu:n has:ptr hasu:n :}
   SB-RESET
   s" input underflow: the call takes more cells than the definition's declared inputs leave (needs " SB-APPEND
   needs needsu SB-APPEND  s" , has " SB-APPEND  has hasu SB-APPEND  s" )" SB-APPEND
   SB$ HAS? ;

: NAMED ( -- )
   CODE$ HAS?  REPAIR$ HAS? ;

\ ---- the shortfall itself ----------------------------------------------------

: CASE-BARE ( -- )
   s" a call with nothing on the stack is refused at the call" T-LABEL
   ARM
   [: s" : IUF-BARE ( -- n ) UFC 0 ;" EV ;] CHECK-RC TTHROWSQ
   NAMED  s" UFC" TOKEN?  s" 1" s" 0" SHORT-BY?
   DISARM ;

\ The token after the call used to own this diagnostic. It must not any more:
\ `0` is the innocent bystander that made the original report unreadable.
: CASE-NOT-THE-NEXT-TOKEN ( -- )
   s" and the token after the call is no longer blamed for it" T-LABEL
   ARM
   [: s" : IUF-NEXT ( -- n ) UFC 0 ;" EV ;] CHECK-RC TTHROWSQ
   s" 0" NOT-TOKEN?
   DISARM ;

: CASE-TWO-DEEP ( -- )
   s" a two-cell call over one declared input names both depths" T-LABEL
   ARM
   [: s" : IUF-TWO ( n -- n ) UFC2 0 ;" EV ;] CHECK-RC TTHROWSQ
   NAMED  s" UFC2" TOKEN?  s" 2" s" 1" SHORT-BY?
   DISARM ;

\ The locals case from the dot: binding the declared input into a local does not
\ make another one appear, and the call still owns the refusal.
: CASE-AFTER-LOCALS ( -- )
   s" a call after a locals binding is still refused at the call" T-LABEL
   ARM
   [: s" : IUF-LOC ( n -- n ) {: x:n :} UFC x ;" EV ;] CHECK-RC TTHROWSQ
   NAMED  s" UFC" TOKEN?  s" 1" s" 0" SHORT-BY?
   DISARM ;

\ The binder consumes too, so an underflowing `{: :}` group is refused at the
\ token that closes it rather than somewhere later in the body.
: CASE-LOCALS-BINDER ( -- )
   s" a locals group with nothing to bind is refused at its own closer" T-LABEL
   ARM
   [: s" : IUF-BIND ( -- ) {: x:n :} ;" EV ;] CHECK-RC TTHROWSQ
   NAMED  s" :}" TOKEN?
   DISARM ;

\ A refusal raised on a candidate row the checker then abandons must not put its
\ name on this one. `IUF-V @ cell+` refuses the `ptr a -- ptr a` candidate
\ (raw cell, no address) before succeeding on `n -- n`, and the underflow two
\ tokens later is what the reader has to be sent to.
: CASE-OVER-ABANDONED-CANDIDATE ( -- )
   s" an abandoned candidate's refusal does not rename this one" T-LABEL
   ARM
   [: s" : IUF-SPEC ( -- n ) IUF-V @ cell+ drop drop 0 ;" EV ;] CHECK-RC TTHROWSQ
   NAMED  s" drop" TOKEN?
   S\" \"code\":\"E-RAW-CELL-PTR\"" LACKS?
   DISARM ;

\ ---- the controls ------------------------------------------------------------

\ Short AND mistyped: the type is the better answer, and this message predates
\ the rule. If the underflow ever swallows it, the diagnostic got worse.
: CASE-MISTYPED-CALL ( -- )
   s" a short call that is also mistyped keeps its own mismatch" T-LABEL
   ARM
   [: s" : IUF-TYPE ( r -- n ) UFC2 0 ;" EV ;] CHECK-RC TTHROWSQ
   MISMATCH$ HAS?  CODE$ LACKS?  REPAIR$ LACKS?
   s" UFC2" TOKEN?
   DISARM ;

\ A call that takes exactly what the signature declared is not an underflow.
: CASE-EXACT ( -- )
   s" a call that spends exactly the declared inputs still certifies" T-LABEL
   [: s" : IUF-OK ( n -- n ) UFC 0 ;" EV ;] 0 TTHROWSQ ;

\ Nor is a call under a value the body produced itself.
: CASE-PRODUCED ( -- )
   s" and a call over a value the body produced certifies too" T-LABEL
   [: s" : IUF-MADE ( -- n ) 0 UFC 0 ;" EV ;] 0 TTHROWSQ ;

\ An UNSIGNED definition has no declared inputs to reach under: its base is
\ what the checker is inferring, so binding it is how inference works.
: CASE-UNSIGNED ( -- )
   s" an unsigned definition still infers its inputs from the body" T-LABEL
   [: s" : IUF-INFER UFC ;" EV ;] 0 TTHROWSQ ;

\ A quotation owns a fresh base for the same reason, so consuming inside one is
\ the quotation taking an input, not the definition spending its caller's frame.
: CASE-QUOTATION ( -- )
   s" a quotation consuming inside the body takes its own input" T-LABEL
   [: s" : IUF-QUOT ( n -- n ) [: UFC 0 ;] execute ;" EV ;] 0 TTHROWSQ ;

public

: RUN ( -- )
   T-RESET
   CASE-BARE
   CASE-NOT-THE-NEXT-TOKEN
   CASE-TWO-DEEP
   CASE-AFTER-LOCALS
   CASE-LOCALS-BINDER
   CASE-OVER-ABANDONED-CANDIDATE
   CASE-MISTYPED-CALL
   CASE-EXACT
   CASE-PRODUCED
   CASE-UNSIGNED
   CASE-QUOTATION
   T-REPORT ;

;package

INPUT-UNDERFLOW-TEST:RUN
