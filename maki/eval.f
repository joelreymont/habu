\ maki/eval.f - the LLM-target eval harness core.
\
\ The thesis is "checked kernels are a better LLM target": the CHECKER is the
\ correctness gate for a model's candidate kernels. This is that gate plus the
\ scoring - the harness scores each candidate by whether the checker certifies it,
\ and tallies pass@k. (The model-generation + compiler-in-loop repair arm is
\ external; this judges what it produces and is where the metrics come from. No
\ "better target" claim until the full matrix runs - PLAN.md.) maki -> habu only.
\
\ CHECK-PASSES? is a metaprogramming boundary (it drives the checker over a source
\ string and reads the verdict) - hence TRUSTED:, like lib/array-test.f's harness.
\ It lives in maki/ (outside the trust root), so it needs no TRUSTED.md row.

TRUSTED: CHECK-PASSES? ( ptr u8 n -- bool )
   DIAGXT @ >r  0 DIAGXT !          \ silence reject diagnostics during scoring
   CHECK!  -1 =                     \ CHECK! verdict: -1 certified / 0 rejected / 1 uncheckable
   r> DIAGXT ! ;

variable EV-PASS
variable EV-TOTAL

: EV-RESET ( -- )  0 EV-PASS !  0 EV-TOTAL ! ;

: EV-RECORD ( bool -- )
   EV-TOTAL @ 1+ EV-TOTAL !
   if EV-PASS @ 1+ EV-PASS ! then ;

: EV-PASS@1? ( -- bool )  EV-PASS @ 0 > ;   \ at least one candidate certified

\ score one candidate and tally it
: EV-SCORE ( ptr u8 n -- )  CHECK-PASSES? EV-RECORD ;
