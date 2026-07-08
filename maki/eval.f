\ maki/eval.f - the LLM-target eval harness core (package EVAL).
\
\ The thesis is "checked kernels are a better LLM target": the CHECKER is the
\ correctness gate for a model's candidate kernels. This is that gate plus the
\ scoring - the harness scores each candidate by whether the checker certifies it,
\ and tallies pass@k. (The model-generation + compiler-in-loop repair arm is
\ external; this judges what it produces and is where the metrics come from. No
\ "better target" claim until the full matrix runs - PLAN.md.) maki -> habu only.
\
\ EVAL:CHECK-PASSES? is a metaprogramming boundary (it drives the checker over a
\ source string and reads the verdict) - hence TRUSTED:, like lib/array-test.f's
\ harness. It lives in maki/ (outside the trust root), so it needs no TRUSTED.md row.

package EVAL

public

TRUSTED: CHECK-PASSES? ( ptr u8 n -- bool )
   DIAGXT @ >r  0 DIAGXT !          \ silence reject diagnostics during scoring
   CHECK-CANDIDATE!  -1 =           \ verdict: -1 certified / 0 rejected / 1 uncheckable
   r> DIAGXT ! ;

variable PASS                       \ certified-candidate tally
variable TOTAL                      \ scored-candidate tally

: RESET ( -- )  0 PASS !  0 TOTAL ! ;

: PASS@1? ( -- bool )  PASS @ 0 > ;   \ at least one candidate certified

private

: RECORD ( bool -- )
   TOTAL @ 1+ TOTAL !
   if PASS @ 1+ PASS ! then ;

public

\ score one candidate and tally it
: SCORE ( ptr u8 n -- )  CHECK-PASSES? RECORD ;

end-package
