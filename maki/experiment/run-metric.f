\ maki/experiment/run-metric.f - typed metric POPULATIONS with the train/held-out
\ separation (MODEL-CAD-V2-PLAN.md § 23.4, plan:3310-3312 "Metrics are typed by
\ population, aggregation, units, and direction; an agent cannot compare unlike
\ populations or optimize a held-out test set as if it were training evidence"; dot
\ habu-v2-experiment-run-7c1d1906).
\
\ CONCERN: the typed metric value + the STATIC + dynamic separation that makes
\ "optimize a held-out test set as if it were training evidence" untypeable. A separate
\ concern from the run IDENTITY (maki/experiment/run.f): a metric is measured on a
\ population and never carries a run-id.
\
\ ---- STATIC LEG (the separation is a TYPE, not a runtime tag) -------------------------
\ Two DISTINCT nominal metric families that never unify (the maki/db/artifact.f
\ weight-artifact vs kernel-artifact "flat per-kind families" precedent):
\   report-metric    - a measurement on ANY population; the ordinary evaluation record.
\   objective-metric - a measurement ELIGIBLE as a training objective.
\ The ONLY producer of an objective-metric is PROMOTE-OBJECTIVE, which refines a
\ report-metric ONLY when its population is `train`. The training-loop consumer
\ AS-OBJECTIVE takes an objective-metric, so a held-out measurement (a report-metric)
\ CANNOT be passed to it: it is a compile-time signature mismatch (verdict 0), proven by
\ the run-metric-test.f verdict fixtures - the cad-kinds nominal-separation pattern. Thus
\ "held-out metric consumed as a training objective" is STATICALLY UNTYPEABLE.
\
\ ---- DYNAMIC LEG (the refinement is fail-closed) --------------------------------------
\ PROMOTE-OBJECTIVE is the only bridge from report to objective, and it returns the
\ `objective-result` custom sum: `ok objective-metric` only when the population is `train`,
\ else `not-training`. So a held-out (or validation) measurement dynamically REJECTS
\ promotion to an objective, never silently succeeding.
\
\ ---- COMPARE UNLIKE POPULATIONS (plan:3310) -------------------------------------------
\ COMPARABLE? is same-population AND same-direction AND same-aggregation; comparing across
\ unlike populations is false, the realised "an agent cannot compare unlike populations".
\
\ SCOPE: population / direction / aggregation are the modelled typed axes; a metric holds
\ a single scalar value and its three enum axes in a bounded ring pool (single-cell family
\ handles over the slot, the diff-suite pool precedent). UNITS (the fourth plan axis) needs
\ an open-vocabulary units owner and is a documented follow-up; it is NOT modelled here.
\ maki -> habu only; run-metric owns -5620.

require lib/prelude.f

-5620 constant E-RMETRIC-CAP        \ metric ring pool exhausted (should be unreachable; bounded live metrics)

package RUNMETRIC
public

\ ---- the typed metric axes (closed vocabularies) --------------------------------
\ population: which data population a metric was measured on. Only `train` licenses a
\ training objective; `validation` and `heldout` are evaluation-only (plan:3312).
ENUM population DERIVE eq
   train
   validation
   heldout
;ENUM

\ direction: the optimization sense an objective is improved along.
ENUM direction DERIVE eq
   maximize
   minimize
;ENUM

\ aggregation: how the scalar was reduced over its population.
ENUM aggregation DERIVE eq
   agg-mean
   agg-sum
   agg-min
   agg-max
   agg-last
;ENUM

\ ---- the two DISTINCT metric families (single-cell handles over a pool slot) -----
\ report-metric and objective-metric never unify: an objective consumer rejects a
\ report metric at compile time.
PRODUCT report-metric 0
   FIELD slot n
;PRODUCT

PRODUCT objective-metric 0
   FIELD slot n
;PRODUCT

\ ---- PROMOTE-OBJECTIVE outcome (custom-sum result, never value+flag) -------------
\ ok carries the eligible objective-metric; not-training rejects a held-out / validation
\ measurement promoted as a training objective.
SUMTYPE objective-result 0
   VARIANT ok objective-metric ;VARIANT
   VARIANT not-training ;VARIANT
;SUMTYPE

private

\ ---- readable wrappers over the generated constructor spellings ------------------
: >REP ( n -- report-metric )         RUNMETRIC-REPORT--METRIC:MAKE ;
: REP> ( report-metric -- n )         RUNMETRIC-REPORT--METRIC:UNMAKE ;
: >OBJ ( n -- objective-metric )      RUNMETRIC-OBJECTIVE--METRIC:MAKE ;
: OBJ> ( objective-metric -- n )      RUNMETRIC-OBJECTIVE--METRIC:UNMAKE ;
: OR-OK ( objective-metric -- objective-result )   RUNMETRIC-OBJECTIVE--RESULT:OK ;
: OR-NOTRAIN ( -- objective-result )               RUNMETRIC-OBJECTIVE--RESULT:NOT-TRAINING ;

\ ---- bounded ring metric pool (parallel per-slot columns) -----------------------
32 constant METRIC-CAP
create MVAL METRIC-CAP cells allot
METRIC-CAP TYPED-BUFFER MPOP population
METRIC-CAP TYPED-BUFFER MDIR direction
METRIC-CAP TYPED-BUFFER MAGG aggregation
variable MNEXT

: MSLOT ( -- n )   MNEXT @  dup 1+ METRIC-CAP mod MNEXT ! ;

: MVAL@ ( n -- n )              cells MVAL + @ ;
: MVAL! ( n n -- )              cells MVAL + ! ;
: MPOP@ ( n -- population )     MPOP @ ;
: MPOP! ( population n -- )     MPOP ! ;
: MDIR@ ( n -- direction )      MDIR @ ;
: MDIR! ( direction n -- )      MDIR ! ;
: MAGG@ ( n -- aggregation )    MAGG @ ;
: MAGG! ( aggregation n -- )    MAGG ! ;

: FILL-SLOT ( n direction aggregation population n -- ) {: v:n d:direction g:aggregation p:population s:n :}
   v s MVAL!  d s MDIR!  g s MAGG!  p s MPOP! ;

public

\ ---- MEASURE: record a metric on a population (the universal, total constructor) -
\ Any population yields a report-metric; only PROMOTE-OBJECTIVE can lift a train one to
\ an objective.
: MEASURE ( n direction aggregation population -- report-metric )
   {: v:n d:direction g:aggregation p:population :}
   MSLOT {: s:n :}
   v d g p s FILL-SLOT
   s >REP ;

\ ---- PROMOTE-OBJECTIVE: the ONLY bridge from a report metric to a training objective -
\ Refines to an objective-metric iff the population is `train`; a held-out / validation
\ measurement rejects `not-training` (the dynamic typed reject).
: PROMOTE-OBJECTIVE ( report-metric -- objective-result )
   REP> {: s:n :}
   s MPOP@ RUNMETRIC-POPULATION:TRAIN RUNMETRIC-POPULATION:EQ 0= if OR-NOTRAIN exit then
   s >OBJ OR-OK ;

\ ---- AS-OBJECTIVE: the training-loop consumer (the STATIC leg) -------------------
\ Takes an objective-metric ONLY; a report-metric (a held-out measurement) is a compile-
\ time signature mismatch. Yields the scalar and direction the training loop optimizes.
: AS-OBJECTIVE ( objective-metric -- n direction )
   OBJ> {: s:n :}
   s MVAL@  s MDIR@ ;

\ ---- report-metric read surface -------------------------------------------------
: VALUE@ ( report-metric -- n )              REP> MVAL@ ;
: POPULATION@ ( report-metric -- population ) REP> MPOP@ ;
: DIRECTION@ ( report-metric -- direction )   REP> MDIR@ ;
: AGGREGATION@ ( report-metric -- aggregation ) REP> MAGG@ ;

\ ---- COMPARABLE?: same population, direction, and aggregation (plan:3310) --------
: COMPARABLE? ( report-metric report-metric -- bool ) {: x:report-metric y:report-metric :}
   x REP> {: sx:n :}
   y REP> {: sy:n :}
   sx MPOP@ sy MPOP@ RUNMETRIC-POPULATION:EQ
   sx MDIR@ sy MDIR@ RUNMETRIC-DIRECTION:EQ and
   sx MAGG@ sy MAGG@ RUNMETRIC-AGGREGATION:EQ and ;

private

: RMETRIC-INIT ( -- )   0 MNEXT ! ;
RMETRIC-INIT

;package
