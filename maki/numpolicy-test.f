\ maki/numpolicy-test.f - checked acceptance for the typed numeric-policy family
\ (maki/numpolicy.f). Covers the plan §22.6 Exit line: an approximate candidate
\ cannot satisfy an exact policy; composed error bounds are deterministic; and the
\ op-registry bridge / wire roundtrip are fail-closed. Every refusal is paired with a
\ resolving positive control (per LESSONS), so no TTHROWS is vacuous. The requested
\ policy is now DERIVED PER-OP by maki/sched-key.f REGION-POL (folding OP-DOM over the
\ region's ops), so the KEY-invalidation half of the dot (a different op mix => a
\ different honest policy => a different plan/artifact key) is proven in
\ maki/sched-key-test.f (skey's pol field + the per-op SK-KEY$ render), and the golden
\ RECORD carrying a relative result refusing an exact policy in
\ maki/evidence/policy-e2e-test.f.

require lib/test.f
require maki/numpolicy.f

\ ---- compact helpers (RANK is bijective with the domain, so rank pins the value) ---
: CMP    ( dom dom -- n )       NPOL:COMPOSE NPOL:RANK ;      \ compose, then rank
: SAT    ( dom dom -- bool )    NPOL:SATISFIES? ;
: OPDOM  ( MAKI:opkind -- n )   NPOL:OP-DOM NPOL:RANK ;       \ an op's achieved domain rank
: DOMRT  ( dom -- n )           NPOL:DOM>N NPOL:N>DOM NPOL:RANK ;   \ dom -> n -> dom roundtrip

\ ---- fail-closed probes ------------------------------------------------------
: TRY-NUM-N        ( -- )  MAKI:NUM-N NPOL:NUM>DOM drop ;     \ no op carries NUM-N
: TRY-NDOM-HI      ( -- )  4 NPOL:N>DOM drop ;
: TRY-NDOM-NEG     ( -- )  -1 NPOL:N>DOM drop ;

\ ---- fixtures: the checked refusal gate (ENFORCE) over the acceptance cases ----
\ TF32-vs-FP32 (the motivating confusion): FP32 FMA is the exact reference domain,
\ TF32 tensor-core is relative-error; approximate cannot satisfy exact.
: TF32-VS-FP32-NEG ( -- )  NPOL-DOM:RELATIVE NPOL-DOM:EXACT NPOL:ENFORCE ;
: TF32-VS-FP32-POS ( -- )  NPOL-DOM:EXACT    NPOL-DOM:EXACT NPOL:ENFORCE ;
\ GELU (approximate transcendental): registry numeric class relative; RELU is exact.
: GELU-NEG ( -- )  MAKI-OPKIND:GELU NPOL:OP-DOM NPOL-DOM:EXACT    NPOL:ENFORCE ;
: GELU-POS ( -- )  MAKI-OPKIND:GELU NPOL:OP-DOM NPOL-DOM:RELATIVE NPOL:ENFORCE ;
: RELU-POS ( -- )  MAKI-OPKIND:RELU NPOL:OP-DOM NPOL-DOM:EXACT    NPOL:ENFORCE ;
\ recompute (empirical): an empirical license satisfies only an empirical policy.
: RECOMPUTE-NEG-EXACT ( -- )  NPOL-DOM:EMPIRICAL NPOL-DOM:EXACT     NPOL:ENFORCE ;
: RECOMPUTE-NEG-REL   ( -- )  NPOL-DOM:EMPIRICAL NPOL-DOM:RELATIVE  NPOL:ENFORCE ;
: RECOMPUTE-POS       ( -- )  NPOL-DOM:EMPIRICAL NPOL-DOM:EMPIRICAL NPOL:ENFORCE ;
\ pipeline composition: a TF32 relative matmul + an exact elementwise stage yields
\ the WEAKEST domain (relative), deterministically.
: PIPE-TF32-EXACT ( -- n )
   MAKI-OPKIND:MATMUL NPOL:OP-DOM  MAKI-OPKIND:RELU NPOL:OP-DOM  NPOL:COMPOSE NPOL:RANK ;

T-RESET

\ ---- strength rank ordering (exact strongest 0 .. empirical weakest 3) ---------
NPOL-DOM:EXACT     NPOL:RANK 0 T=
NPOL-DOM:ULP       NPOL:RANK 1 T=
NPOL-DOM:RELATIVE  NPOL:RANK 2 T=
NPOL-DOM:EMPIRICAL NPOL:RANK 3 T=

\ ---- key tokens --------------------------------------------------------------
NPOL-DOM:EXACT     NPOL:NAME s" exact" T$=
NPOL-DOM:ULP       NPOL:NAME s" ulp"   T$=
NPOL-DOM:RELATIVE  NPOL:NAME s" rel"   T$=
NPOL-DOM:EMPIRICAL NPOL:NAME s" emp"   T$=

\ ---- composition table (weakest / higher-rank wins; commutative + idempotent) --
NPOL-DOM:EXACT     NPOL-DOM:EXACT     CMP 0 T=
NPOL-DOM:EXACT     NPOL-DOM:ULP       CMP 1 T=
NPOL-DOM:EXACT     NPOL-DOM:RELATIVE  CMP 2 T=
NPOL-DOM:EXACT     NPOL-DOM:EMPIRICAL CMP 3 T=
NPOL-DOM:ULP       NPOL-DOM:EXACT     CMP 1 T=
NPOL-DOM:ULP       NPOL-DOM:ULP       CMP 1 T=
NPOL-DOM:ULP       NPOL-DOM:RELATIVE  CMP 2 T=
NPOL-DOM:ULP       NPOL-DOM:EMPIRICAL CMP 3 T=
NPOL-DOM:RELATIVE  NPOL-DOM:EXACT     CMP 2 T=
NPOL-DOM:RELATIVE  NPOL-DOM:ULP       CMP 2 T=
NPOL-DOM:RELATIVE  NPOL-DOM:RELATIVE  CMP 2 T=
NPOL-DOM:RELATIVE  NPOL-DOM:EMPIRICAL CMP 3 T=
NPOL-DOM:EMPIRICAL NPOL-DOM:EXACT     CMP 3 T=
NPOL-DOM:EMPIRICAL NPOL-DOM:ULP       CMP 3 T=
NPOL-DOM:EMPIRICAL NPOL-DOM:RELATIVE  CMP 3 T=
NPOL-DOM:EMPIRICAL NPOL-DOM:EMPIRICAL CMP 3 T=
\ COMPOSE returns the actual weaker VALUE, not just its rank.
NPOL-DOM:RELATIVE NPOL-DOM:EXACT NPOL:COMPOSE NPOL-DOM:RELATIVE NPOL-DOM:EQ TTRUE

\ ---- satisfaction: evidence must be at least as STRONG as the requirement ------
NPOL-DOM:EXACT     NPOL-DOM:EXACT     SAT TTRUE
NPOL-DOM:EXACT     NPOL-DOM:EMPIRICAL SAT TTRUE     \ exact satisfies any requirement
NPOL-DOM:ULP       NPOL-DOM:EXACT     SAT TFALSE
NPOL-DOM:ULP       NPOL-DOM:RELATIVE  SAT TTRUE
NPOL-DOM:RELATIVE  NPOL-DOM:EXACT     SAT TFALSE    \ approximate cannot satisfy exact
NPOL-DOM:RELATIVE  NPOL-DOM:RELATIVE  SAT TTRUE
NPOL-DOM:EMPIRICAL NPOL-DOM:EXACT     SAT TFALSE
NPOL-DOM:EMPIRICAL NPOL-DOM:RELATIVE  SAT TFALSE
NPOL-DOM:EMPIRICAL NPOL-DOM:EMPIRICAL SAT TTRUE

\ ---- op-registry bridge: raw NUM-* class -> typed domain = the PER-OP request ---
\ OP-DOM is both an op's ACHIEVED domain and (folded over a region by sched-key.f
\ REGION-POL) its REQUESTED policy - the per-op axis that replaced the per-class
\ table, so a pure-gelu region requests relative while a pure-relu region stays exact.
MAKI-OPKIND:RELU    OPDOM 0 T=     \ NUM-EXACT
MAKI-OPKIND:CAST    OPDOM 0 T=     \ NUM-EXACT
MAKI-OPKIND:RESHAPE OPDOM 0 T=     \ movement: exact
MAKI-OPKIND:ADD     OPDOM 1 T=     \ NUM-ULP
MAKI-OPKIND:MUL     OPDOM 1 T=     \ NUM-ULP
MAKI-OPKIND:GELU    OPDOM 2 T=     \ NUM-RELTOL (approximate transcendental)
MAKI-OPKIND:MATMUL  OPDOM 2 T=     \ NUM-RELTOL (accumulated)
MAKI-OPKIND:LAYERNORM OPDOM 2 T=   \ NUM-RELTOL

\ ---- wire roundtrip (id == rank; inverse fails closed) ------------------------
NPOL-DOM:EXACT     DOMRT 0 T=
NPOL-DOM:ULP       DOMRT 1 T=
NPOL-DOM:RELATIVE  DOMRT 2 T=
NPOL-DOM:EMPIRICAL DOMRT 3 T=

\ ---- acceptance fixtures: refusals + positive controls (non-vacuous) -----------
' TF32-VS-FP32-NEG    E-NPOL-APPROX TTHROWS    \ TF32 relative result vs FP32 exact policy: refused
' TF32-VS-FP32-POS    0             TTHROWS    \ FP32 exact result vs FP32 exact policy: satisfied
' GELU-NEG            E-NPOL-APPROX TTHROWS    \ approximate GELU vs exact policy: refused
' GELU-POS            0             TTHROWS    \ GELU vs relative policy: satisfied
' RELU-POS            0             TTHROWS    \ exact RELU vs exact policy: satisfied (flipped verdict)
' RECOMPUTE-NEG-EXACT E-NPOL-APPROX TTHROWS    \ empirical recompute vs exact policy: refused
' RECOMPUTE-NEG-REL   E-NPOL-APPROX TTHROWS    \ empirical recompute vs relative policy: refused
' RECOMPUTE-POS       0             TTHROWS    \ empirical recompute vs empirical policy: satisfied
PIPE-TF32-EXACT 2 T=                           \ TF32 matmul + exact elementwise => relative (weakest)

\ ---- fail-closed throws ------------------------------------------------------
' TRY-NUM-N        E-NPOL-DOM   TTHROWS         \ NUM-N has no domain
' TRY-NDOM-HI      E-NPOL-DOM   TTHROWS         \ wire id out of range
' TRY-NDOM-NEG     E-NPOL-DOM   TTHROWS

\ § 23.9 numeric-policy-id: constructor + wire codec round-trip + fail-closed decode.
\ Reopen the owner package for LE-PUT / DOM-COUNT (an out-of-range wire raw is only
\ forgeable inside the owning package).
package NPOL

512 constant TT-WCAP
create TT-WBUF TT-WCAP allot

: TT-RT ( dom -- n )                            \ 0 = REGISTER->ID>WIRE->WIRE>ID keeps the dom
   dup {: d:dom :}
   REGISTER TT-WBUF TT-WCAP ID>WIRE {: len:n :}
   TT-WBUF len WIRE>ID
   MATCH id-result
      ok          OF POLICY-DOM RANK  d RANK  = if 0 else 1 then ENDOF
      wrong-width OF 2 ENDOF
      unknown     OF 3 ENDOF
   ;MATCH ;

: TT-WIRE-WIDTH ( -- n )                        \ a 4-byte buffer decodes as wrong-width
   TT-WBUF 4 WIRE>ID
   MATCH id-result  ok OF drop 8 ENDOF  wrong-width OF 2 ENDOF  unknown OF 3 ENDOF  ;MATCH ;

: TT-WIRE-UNKNOWN ( -- n )                      \ raw == DOM-COUNT (one past the closed range): unknown
   DOM-COUNT  TT-WBUF WIRE-BYTES LE-PUT
   TT-WBUF WIRE-BYTES WIRE>ID
   MATCH id-result  ok OF drop 9 ENDOF  wrong-width OF 2 ENDOF  unknown OF 3 ENDOF  ;MATCH ;

NPOL-DOM:EXACT     TT-RT 0 T=
NPOL-DOM:ULP       TT-RT 0 T=
NPOL-DOM:RELATIVE  TT-RT 0 T=
NPOL-DOM:EMPIRICAL TT-RT 0 T=
TT-WIRE-WIDTH 2 T=
TT-WIRE-UNKNOWN 3 T=

;package

T-REPORT
