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

\ ---- same-shape twins for the metric records and the promotion outcome ----------
\ met-twin is a THIRD one-cell record with the same FIELD line as both report-metric and
\ objective-metric, and objr-twin repeats objective-result's exact shape - same arity, same
\ variants in the same order, same named payload field, and deliberately the SAME payload
\ type, so nothing but the family NAME differs. Together they prove the metric separation
\ is nominal rather than structural: a record of identical shape still refuses to unify
\ with either metric family, and an outcome of identical shape refuses to unify with
\ objective-result. They live in their own package, not in the reopened package RUNMETRIC,
\ because a test must not add public words to the production package's surface; and they
\ must be public, because a private family publishes no constructors at all, which would
\ let the negatives pass by being unresolvable rather than ill-typed.
package RUNMETRIC-TEST
public

STRUCTURE met-twin 0
   FIELD slot n
;STRUCTURE

ENUM objr-twin 0
   VARIANT ok FIELD metric RUNMETRIC:objective-metric ;VARIANT
   VARIANT not-training ;VARIANT
;ENUM

;package

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

\ ---- the generated record pair round-trips its one field ------------------------
\ report-metric and objective-metric are declared through the unified STRUCTURE front end;
\ these drive the generated MAKE / UNMAKE pair directly (not the >REP / >OBJ wrappers) so
\ the field a record carries is proven to survive construction and read-back.
\
\ The metric pool is a RING whose cursor starts at 0, and a field the constructor zeroed
\ also reads back as 0, so a round-trip riding slot 0 would pass on a dropped field.
\ MRT-ANCHOR burns a slot whenever the cursor is about to hand out 0, so every fixture
\ below rides a non-zero slot, and the NZ pins keep that checked rather than assumed.
: MRT-ANCHOR ( -- )
   MNEXT @ 0= if
      0 RUNMETRIC-DIRECTION:MAXIMIZE RUNMETRIC-AGGREGATION:AGG-MEAN
        RUNMETRIC-POPULATION:TRAIN MEASURE drop
   then ;

: MRT-SLOT# ( -- n )   MRT-ANCHOR MSLOT ;       \ a freshly allocated NON-ZERO pool slot

: MRT-REP-RT ( -- bool )                        \ the report-metric field survives MAKE/UNMAKE
   MRT-SLOT# {: s:n :}
   s RUNMETRIC-REPORT--METRIC:MAKE RUNMETRIC-REPORT--METRIC:UNMAKE s = ;
: MRT-OBJ-RT ( -- bool )                        \ the objective-metric field survives MAKE/UNMAKE
   MRT-SLOT# {: s:n :}
   s RUNMETRIC-OBJECTIVE--METRIC:MAKE RUNMETRIC-OBJECTIVE--METRIC:UNMAKE s = ;
: MRT-REP-NZ ( -- bool )   MRT-SLOT# 0 > ;      \ the compared slot is never 0
: MRT-OBJ-NZ ( -- bool )   MRT-SLOT# 0 > ;

\ ---- objective-result carries its metric through MATCH --------------------------
\ PROMOTE-OBJECTIVE reaches the ok arm only through a train measurement. These construct
\ the arms DIRECTLY through the production producers OR-OK / OR-NOTRAIN and match them
\ straight back, so the named payload FIELD is proven to bind: the ok arm binds the payload
\ to a TYPED local and reports the pool slot behind it, which is the metric's whole
\ representation, so a payload the constructor dropped or zeroed reads back as a different
\ slot instead of passing.
: MRT-OBJ-METRIC ( -- objective-metric )        \ a promoted objective on a non-zero slot
   MRT-ANCHOR
   7 MK-TRAIN PROMOTE-OBJECTIVE MATCH objective-result
      ok OF ENDOF
      not-training OF -777 throw ENDOF
   ;MATCH ;

: MRT-MK-OK ( objective-metric -- objective-result )   OR-OK ;
: MRT-MK-NT ( -- objective-result )                    OR-NOTRAIN ;

: MRT-CODE ( objective-result -- n )            \ 1 ok, 2 not-training
   MATCH objective-result
      ok OF drop 1 ENDOF
      not-training OF 2 ENDOF
   ;MATCH ;

: MRT-SLOT ( objective-result -- n )            \ ok payload's pool slot, else -1
   MATCH objective-result
      ok OF {: got:objective-metric :} got OBJ> ENDOF
      not-training OF -1 ENDOF
   ;MATCH ;

: MRT-OK-ARM ( -- n )     MRT-OBJ-METRIC MRT-MK-OK MRT-CODE ;
: MRT-NT-ARM ( -- n )     MRT-MK-NT MRT-CODE ;
: MRT-RT ( -- n )                               \ 0 = the metric came back on its own slot
   MRT-OBJ-METRIC dup OBJ> {: want:n :}
   MRT-MK-OK MRT-SLOT want = if 0 else 1 then ;
: MRT-RT-NZ ( -- bool )   MRT-OBJ-METRIC OBJ> 0 > ;
: MRT-NT-SLOT ( -- n )    MRT-MK-NT MRT-SLOT ;  \ the payloadless arm carries no slot

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

\ ---- the generated records and outcome round-trip their fields ------------------
MRT-REP-RT TTRUE          \ a report-metric's slot survives MAKE then UNMAKE
MRT-OBJ-RT TTRUE          \ an objective-metric's slot survives MAKE then UNMAKE
MRT-REP-NZ TTRUE          \ ...compared against a non-zero slot, so a zeroed field fails
MRT-OBJ-NZ TTRUE
MRT-OK-ARM 1 T=           \ a constructed ok dispatches to its own arm
MRT-NT-ARM 2 T=           \ not-training dispatches to its own arm
MRT-RT 0 T=               \ the ok payload carries its metric through unchanged
MRT-RT-NZ TTRUE           \ ...on a non-zero slot, so a zeroed payload fails
MRT-NT-SLOT -1 T=         \ the no-payload arm of MRT-SLOT is live

\ ---- the generated constructor spellings + exact effects ------------------------
\ The SPELLING is load-bearing: the checker answers 1 (uncheckable) for a name it cannot
\ resolve, so a -1 means it resolved EXACTLY this name, and a 0 means it resolved the name
\ and refused the types. The two 1-verdict rows are the controls that prove that split.
s" MC-REP-MK ( n -- report-metric ) RUNMETRIC-REPORT--METRIC:MAKE" VCHECK -1 T=
s" MC-REP-UN ( report-metric -- n ) RUNMETRIC-REPORT--METRIC:UNMAKE" VCHECK -1 T=
s" MC-OBJ-MK ( n -- objective-metric ) RUNMETRIC-OBJECTIVE--METRIC:MAKE" VCHECK -1 T=
s" MC-OBJ-UN ( objective-metric -- n ) RUNMETRIC-OBJECTIVE--METRIC:UNMAKE" VCHECK -1 T=
s" MC-REP-SPELL ( n -- report-metric ) RUNMETRIC-REPORT--METRICX:MAKE" VCHECK 1 T=
s" MC-OR-SPELL ( objective-metric -- objective-result ) RUNMETRIC-OBJECTIVE--RESULTX:OK" VCHECK 1 T=
\ Each record takes exactly one cell and yields exactly one: the declaration has one FIELD.
s" MC-REP-MK2 ( n n -- report-metric ) RUNMETRIC-REPORT--METRIC:MAKE" VCHECK 0 T=
s" MC-REP-UN2 ( report-metric -- n n ) RUNMETRIC-REPORT--METRIC:UNMAKE" VCHECK 0 T=
\ The generated pairs never cross between the two metric families.
s" MC-X-MK1 ( n -- objective-metric ) RUNMETRIC-REPORT--METRIC:MAKE" VCHECK 0 T=
s" MC-X-MK2 ( n -- report-metric ) RUNMETRIC-OBJECTIVE--METRIC:MAKE" VCHECK 0 T=
s" MC-X-UN1 ( objective-metric -- n ) RUNMETRIC-REPORT--METRIC:UNMAKE" VCHECK 0 T=
s" MC-X-UN2 ( report-metric -- n ) RUNMETRIC-OBJECTIVE--METRIC:UNMAKE" VCHECK 0 T=
\ objective-result's ok demands an objective-metric: not a raw cell, not a report metric,
\ not nothing, and the result is not a bare scalar.
s" MC-OR-OK ( objective-metric -- objective-result ) RUNMETRIC-OBJECTIVE--RESULT:OK" VCHECK -1 T=
s" MC-OR-NT ( -- objective-result ) RUNMETRIC-OBJECTIVE--RESULT:NOT-TRAINING" VCHECK -1 T=
s" MC-OR-RAW ( n -- objective-result ) RUNMETRIC-OBJECTIVE--RESULT:OK" VCHECK 0 T=
s" MC-OR-REP ( report-metric -- objective-result ) RUNMETRIC-OBJECTIVE--RESULT:OK" VCHECK 0 T=
s" MC-OR-NONE ( -- objective-result ) RUNMETRIC-OBJECTIVE--RESULT:OK" VCHECK 0 T=
s" MC-OR-BARE ( objective-metric -- n ) RUNMETRIC-OBJECTIVE--RESULT:OK" VCHECK 0 T=

\ ---- nominal identity against the same-shape twins -----------------------------
\ Positive controls build through each twin's own generated constructor, so the negatives
\ cannot pass by being unresolvable. A third record of identical shape does not unify with
\ either metric family, and an outcome of identical shape does not unify with
\ objective-result - in either direction.
s" MC-TWIN-MK ( n -- RUNMETRIC-TEST:met-twin ) RUNMETRIC--TEST-MET--TWIN:MAKE" VCHECK -1 T=
s" MC-TWIN-X1 ( n -- RUNMETRIC-TEST:met-twin ) RUNMETRIC-REPORT--METRIC:MAKE" VCHECK 0 T=
s" MC-TWIN-X2 ( n -- report-metric ) RUNMETRIC--TEST-MET--TWIN:MAKE" VCHECK 0 T=
s" MC-TWIN-X3 ( n -- objective-metric ) RUNMETRIC--TEST-MET--TWIN:MAKE" VCHECK 0 T=
s" MC-TWIN-X4 ( RUNMETRIC-TEST:met-twin -- report-metric )" VCHECK 0 T=
s" MC-TWIN-X5 ( report-metric -- RUNMETRIC-TEST:met-twin )" VCHECK 0 T=
s" MC-TWIN-X6 ( RUNMETRIC-TEST:met-twin -- objective-metric )" VCHECK 0 T=
s" MC-TWIN-OR ( objective-metric -- RUNMETRIC-TEST:objr-twin ) RUNMETRIC--TEST-OBJR--TWIN:OK" VCHECK -1 T=
s" MC-TWIN-OR-X1 ( objective-metric -- RUNMETRIC-TEST:objr-twin ) RUNMETRIC-OBJECTIVE--RESULT:OK" VCHECK 0 T=
s" MC-TWIN-OR-X2 ( objective-metric -- objective-result ) RUNMETRIC--TEST-OBJR--TWIN:OK" VCHECK 0 T=

T-REPORT

;package
