\ maki/numpolicy.f - the typed numeric-policy proof-domain family (CAD-PLAN A3 +
\ section 22.6: "Precision is part of the plan and artifact key. A conversion or
\ approximate rewrite requires evidence in one of the exact, ULP, relative-error,
\ or empirically licensed domains").
\
\ V1 already tags every op with a RAW numeric class in maki/op-registry.f
\ (NUM-EXACT / NUM-ULP / NUM-RELTOL). Those are bare ints: nothing stops a
\ relative-tolerance TF32 tensor-core result from being compared against an exact
\ FP32-FMA reference under the same plan/artifact key (the motivating confusion,
\ MODEL-CAD-V2-PLAN.md:2804 "No result may mix arithmetic domains silently").
\
\ This file promotes that raw tag to a TYPED proof domain `dom` and gives it the
\ two operations the plan's Exit line requires:
\   - SATISFIES? / ENFORCE: an approximate candidate CANNOT satisfy a stricter
\     policy (E-NPOL-APPROX is the named checked refusal);
\   - COMPOSE: composing domains across a pipeline yields the WEAKEST domain
\     deterministically (a total-order max - commutative, associative, idempotent).
\ The domain also becomes part of the EXACT schedule/plan key: maki/sched-key.f
\ renders each region's REQUESTED policy (POL@, default exact) into SK-KEY / SK-KEY$,
\ so changing the requested policy invalidates plan/artifact/tuning key lookups
\ (same config, different policy => different key => no baseline pairing), and the
\ typed golden record carries the achieved domain (maki/evidence/schema.f).
\
\ The requested policy is an ambient per-region-class REQUEST, exactly like the
\ requested precision in maki/precision.f (PREC!/PREC@): an INPUT knob the plan
\ sets, not process-state provenance. The default is set HONESTLY per class by
\ CLASS-DEFAULT-POL (POL-RESET restores it): the accumulating classes (row-reduce,
\ matmul) and the decode/dequant chain request `relative` because their ops
\ genuinely accumulate / approximate in f32 / TF32, so an exact default would
\ refuse every legitimate golden (the "exact-and-always-refusing" trap); movement
\ (a layout rewrite, no compute) and elementwise request `exact`. Elementwise is a
\ MIXED class (exact relu/cast, ulp add/mul, transcendental gelu/silu); its class
\ default stays `exact` so a pure-elementwise key is unchanged, and any region that
\ fuses a transcendental/accumulating op ALSO carries a matmul/row-reduce/decode op
\ whose relative request drags REGION-POL to relative via COMPOSE. A per-OP honest
\ default (a pure-elementwise region of only gelu/silu requesting relative) needs a
\ per-op requested-policy axis that this per-class table cannot express; it is a
\ documented follow-on (a dedicated dot), not a regression - no such region is
\ promoted today. The bridge NUM>DOM / OP-DOM projects an op's raw registry numeric
\ class into the domain lattice (empirical has no per-op source - it is a
\ golden/recompute-level license, so NUM>DOM never yields it and fails closed on a
\ NUM-N tag).
\
\ Fail closed: an out-of-range wire id, region class, or raw numeric tag is a named
\ throw. maki -> habu only; numpolicy owns -5145..-5147.

require lib/prelude.f
require lib/errors.f
require maki/op-registry.f          \ NUM-* raw numeric class, CLASS-N, OPR-NUMERIC

-5145 constant E-NPOL-DOM       \ numeric-policy wire/ordinal id out of range
-5146 constant E-NPOL-APPROX    \ approximate evidence cannot satisfy a stricter numeric policy
-5147 constant E-NPOL-CLASS     \ region class id out of range (requested-policy table)

package NPOL
public

\ ---- the proof domain, ordered by STRENGTH (exact strongest .. empirical weakest) ----
\ DERIVE eq gives NPOL-DOM:EQ so `dom` can be an enum FIELD of the typed schedule
\ key (maki/sched-key.f skey) and the typed golden record (maki/evidence/schema.f).
\ Variants, strongest first (inline `\` notes inside an ENUM block are a parse error):
\   exact     - bit-exact / FP32 FMA reference: no approximation licensed
\   ulp       - ULP-bounded arithmetic
\   relative  - relative-error bounded (transcendental / accumulated / TF32 tensor-core)
\   empirical - empirically licensed against an independent golden dataset (recompute)
ENUM dom DERIVE eq
   exact
   ulp
   relative
   empirical
;ENUM

\ ---- strength lattice ---------------------------------------------------------
\ RANK is the single named ordinal boundary the lattice folds on; it doubles as the
\ stable wire/table id (exact=0 .. empirical=3). Strength DECREASES as rank rises.
: RANK ( dom -- n )
   MATCH dom
      exact     OF 0 ENDOF
      ulp       OF 1 ENDOF
      relative  OF 2 ENDOF
      empirical OF 3 ENDOF
   ;MATCH ;

\ SATISFIES?: does `have`-domain evidence meet the `need` policy? Evidence is
\ acceptable iff it is at least as STRONG as the requirement (rank <=). Exact
\ evidence satisfies anything; empirical evidence satisfies only an empirical need.
: SATISFIES? ( dom dom -- bool ) {: have:dom need:dom :}
   have RANK  need RANK  <= ;

\ COMPOSE: pipeline composition - the WEAKER (higher-rank) domain wins. A total-order
\ max, so composing a pipeline is deterministic, commutative, associative, idempotent
\ (e.g. TF32 relative matmul + exact elementwise => relative).
: COMPOSE ( dom dom -- dom ) {: a:dom b:dom :}
   a RANK  b RANK  >=  if a else b then ;

\ ENFORCE: the checked satisfaction GATE - a named refusal unless `have` evidence
\ satisfies the `need` policy. This is the "approximate candidate cannot satisfy an
\ exact policy" boundary; a golden/promotion carrying a relative or empirical result
\ throws E-NPOL-APPROX against an exact requirement.
: ENFORCE ( dom dom -- )  SATISFIES? 0= if E-NPOL-APPROX throw then ;

\ ---- durable key token (bare, positional pipe-field in SK-KEY$) ---------------
: NAME ( dom -- ptr u8 n )
   MATCH dom
      exact     OF s" exact" ENDOF
      ulp       OF s" ulp"   ENDOF
      relative  OF s" rel"   ENDOF
      empirical OF s" emp"   ENDOF
   ;MATCH ;

\ ---- wire projection (store row / ambient table id; inverse fails closed) ------
: DOM>N ( dom -- n )  RANK ;
: N>DOM ( n -- dom )
   case
      0 of NPOL-DOM:EXACT     endof
      1 of NPOL-DOM:ULP       endof
      2 of NPOL-DOM:RELATIVE  endof
      3 of NPOL-DOM:EMPIRICAL endof
      E-NPOL-DOM throw
   endcase ;

\ ---- bridge from op-registry's raw per-op numeric class -----------------------
\ NUM-EXACT/NUM-ULP/NUM-RELTOL project into the domain lattice; empirical has no
\ per-op source (it is a golden/recompute-level license), so a NUM-N tag fails closed.
: NUM>DOM ( n -- dom )
   case
      MAKI:NUM-EXACT  of NPOL-DOM:EXACT    endof
      MAKI:NUM-ULP    of NPOL-DOM:ULP      endof
      MAKI:NUM-RELTOL of NPOL-DOM:RELATIVE endof
      E-NPOL-DOM throw
   endcase ;
: OP-DOM ( MAKI:opkind -- dom )  MAKI:OPR-NUMERIC NUM>DOM ;   \ an op's achieved numeric domain

private

\ ---- ambient requested numeric policy per region class (mirror of PREC-REQ) ----
\ The plan's requested proof domain for each op class; the honest per-class default
\ is CLASS-DEFAULT-POL (POL-RESET restores). Stored as wire ids; POL@ lifts them
\ back through N>DOM (fail closed).
create NPOL-REQ MAKI:CLASS-N cells allot

: POL-CK-CLASS ( n -- ) {: c:n :}
   c 0 <  c MAKI:CLASS-N >=  or if E-NPOL-CLASS throw then ;

\ CLASS-DEFAULT-POL: the honest per-class requested proof domain the plan defaults
\ to. Accumulating (row-reduce / matmul) + decode -> relative (they accumulate or
\ approximate); movement + elementwise -> exact (see the header note on the mixed
\ elementwise class and the per-op follow-on dot). Fail closed on an unknown class.
: CLASS-DEFAULT-POL ( n -- dom )
   case
      MAKI:CLASS-EW         of NPOL-DOM:EXACT    endof
      MAKI:CLASS-ROW-REDUCE of NPOL-DOM:RELATIVE endof
      MAKI:CLASS-MATMUL     of NPOL-DOM:RELATIVE endof
      MAKI:CLASS-MOVEMENT   of NPOL-DOM:EXACT    endof
      MAKI:CLASS-DECODE     of NPOL-DOM:RELATIVE endof
      E-NPOL-CLASS throw
   endcase ;

public

: POL-RESET ( -- )
   MAKI:CLASS-N 0 ?do  i CLASS-DEFAULT-POL DOM>N  i cells NPOL-REQ + !  loop ;

\ POL!: request a numeric policy for a region class (the plan's declared contract;
\ it lands in the exact key via maki/sched-key.f REGION-POL, so a different request
\ is a different key). POL@: read it (default exact).
: POL! ( dom n -- ) {: pol:dom cls:n :}
   cls POL-CK-CLASS
   pol DOM>N  cls cells NPOL-REQ + ! ;
: POL@ ( n -- dom ) {: cls:n :}
   cls POL-CK-CLASS
   cls cells NPOL-REQ + @  N>DOM ;

POL-RESET

;package
