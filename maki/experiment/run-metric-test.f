\ maki/experiment/run-metric-test.f - acceptance for the typed metric populations
\ (maki/experiment/run-metric.f; dot habu-v2-experiment-run-7c1d1906).
\
\ Proves the plan:3310-3312 acceptance (all sum / product / enum values are produced and
\ consumed INSIDE colon words, never on the interpret-mode stack):
\   MVC-* : the STATIC leg - AS-OBJECTIVE takes an objective-metric ONLY, so a report-metric
\          (what a held-out measurement yields) is a compile-time signature mismatch
\          (verdict 0); the two metric families never unify in either direction; PROMOTE-
\          OBJECTIVE takes a report-metric only. Each paired with a certifying control (the
\          maki/cad-kinds-test.f verdict-fixture pattern).
\   OBJ-* : the DYNAMIC leg - PROMOTE-OBJECTIVE lifts a `train` measurement to an objective
\          (ok) but rejects a `heldout` or `validation` one (not-training).
\   AS-*  : a promoted objective yields the scalar + direction the training loop optimizes.
\   CMP-* : COMPARABLE? is true only for same population + direction + aggregation (an agent
\          cannot compare unlike populations).
\
\ The test reopens package RUNMETRIC (a friend) to reach the private OBJ> unwrapper and to
\ let the verdict-fixture candidates name the public metric types bare in the current
\ package context (the cad-kinds-test bare-type precedent).

require lib/test.f
require lib/string.f
require maki/experiment/run-metric.f

package RUNMETRIC

create VDIAG 4096 allot

: VCHECK ( ptr u8 n -- n )
   VDIAG 4096 DIAG-BUFFER!
   CHECK-CANDIDATE!
   DIAG-BUFFER-OFF ;

\ ---- metric constructors (dynamic-leg fixtures) ---------------------------------
: MK-TRAIN ( n -- report-metric ) {: v:n :}
   v RUNMETRIC-DIRECTION:MAXIMIZE RUNMETRIC-AGGREGATION:AGG-MEAN RUNMETRIC-POPULATION:TRAIN MEASURE ;
: MK-HELDOUT ( n -- report-metric ) {: v:n :}
   v RUNMETRIC-DIRECTION:MAXIMIZE RUNMETRIC-AGGREGATION:AGG-MEAN RUNMETRIC-POPULATION:HELDOUT MEASURE ;
: MK-VAL ( n -- report-metric ) {: v:n :}
   v RUNMETRIC-DIRECTION:MINIMIZE RUNMETRIC-AGGREGATION:AGG-MEAN RUNMETRIC-POPULATION:VALIDATION MEASURE ;

\ ---- DYNAMIC leg: only a train measurement promotes to an objective ---------------
: PROMOTE-CODE ( report-metric -- n )              \ 0 ok, 1 not-training
   PROMOTE-OBJECTIVE MATCH objective-result
      ok OF OBJ> drop 0 ENDOF
      not-training OF 1 ENDOF
   ;MATCH ;
: OBJ-TRAIN ( -- n )    5 MK-TRAIN PROMOTE-CODE ;
: OBJ-HELDOUT ( -- n )  5 MK-HELDOUT PROMOTE-CODE ;
: OBJ-VAL ( -- n )      5 MK-VAL PROMOTE-CODE ;

\ ---- AS-OBJECTIVE yields the scalar + direction the training loop optimizes -------
: OBJ-OPT-VALUE ( -- n )
   42 MK-TRAIN PROMOTE-OBJECTIVE MATCH objective-result
      ok OF AS-OBJECTIVE drop ENDOF
      not-training OF -777 throw ENDOF
   ;MATCH ;
: OBJ-OPT-DIR ( -- bool )
   42 MK-TRAIN PROMOTE-OBJECTIVE MATCH objective-result
      ok OF AS-OBJECTIVE nip RUNMETRIC-DIRECTION:MAXIMIZE RUNMETRIC-DIRECTION:EQ ENDOF
      not-training OF -777 throw ENDOF
   ;MATCH ;

\ ---- COMPARABLE?: unlike populations / directions cannot be compared --------------
: CMP-SAME ( -- bool )   5 MK-TRAIN 6 MK-TRAIN COMPARABLE? ;
: CMP-POP ( -- bool )    5 MK-TRAIN 6 MK-HELDOUT COMPARABLE? 0= ;
: CMP-DIR ( -- bool )
   5 RUNMETRIC-DIRECTION:MAXIMIZE RUNMETRIC-AGGREGATION:AGG-MEAN RUNMETRIC-POPULATION:VALIDATION MEASURE
   6 RUNMETRIC-DIRECTION:MINIMIZE RUNMETRIC-AGGREGATION:AGG-MEAN RUNMETRIC-POPULATION:VALIDATION MEASURE
   COMPARABLE? 0= ;

\ ---- read-back ------------------------------------------------------------------
: RB-VALUE ( -- n )    7 MK-TRAIN VALUE@ ;
: RB-POP ( -- bool )   7 MK-HELDOUT POPULATION@ RUNMETRIC-POPULATION:HELDOUT RUNMETRIC-POPULATION:EQ ;

T-RESET

\ ---- DYNAMIC leg ----------------------------------------------------------------
OBJ-TRAIN 0 T=            \ a train metric promotes to a training objective
OBJ-HELDOUT 1 T=          \ a held-out metric CANNOT be a training objective (not-training)
OBJ-VAL 1 T=              \ a validation metric is not a training objective either
OBJ-OPT-VALUE 42 T=       \ the objective carries its scalar
OBJ-OPT-DIR TTRUE         \ ...and its optimization direction

\ ---- COMPARABLE? ----------------------------------------------------------------
CMP-SAME TTRUE
CMP-POP TTRUE
CMP-DIR TTRUE

\ ---- read-back ------------------------------------------------------------------
RB-VALUE 7 T=
RB-POP TTRUE

\ ---- STATIC leg: held-out-as-objective is untypeable ----------------------------
\ AS-OBJECTIVE consumes an objective-metric; a report-metric (a held-out measurement) is a
\ compile-time reject, the control certifies.
s" MVC-OBJ ( objective-metric -- n direction ) AS-OBJECTIVE" VCHECK -1 T=
s" MVC-REP ( report-metric -- n direction ) AS-OBJECTIVE" VCHECK 0 T=
\ PROMOTE-OBJECTIVE consumes a report-metric; an objective-metric cannot be re-promoted.
s" MVC-PROM-OK ( report-metric -- objective-result ) PROMOTE-OBJECTIVE" VCHECK -1 T=
s" MVC-PROM-BAD ( objective-metric -- objective-result ) PROMOTE-OBJECTIVE" VCHECK 0 T=
\ the two metric families never unify, in either direction.
s" MVC-XF1 ( objective-metric -- report-metric )" VCHECK 0 T=
s" MVC-XF2 ( report-metric -- objective-metric )" VCHECK 0 T=
\ positive controls: identity on each family certifies.
s" MVC-OBJ-ID ( objective-metric -- objective-metric )" VCHECK -1 T=
s" MVC-REP-ID ( report-metric -- report-metric )" VCHECK -1 T=

T-REPORT

;package
