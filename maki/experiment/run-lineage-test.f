\ maki/experiment/run-lineage-test.f - acceptance for the per-run lineage log
\ (maki/experiment/run-lineage.f; dot habu-v2-experiment-run-7c1d1906).
\
\ Proves the plan:3296-3298 acceptance (all id / sum values are produced and consumed
\ INSIDE colon words, never on the interpret-mode stack):
\   LIN-RESUME    : two equal run-key builds resume the SAME lineage - an append after a
\                   rebuild accumulates under one identity (LINEAGE-COUNT == 2).
\   LIN-RESUME-EQ : the rebuild is the SAME interned run-id.
\   LIN-EVENTS-DISTINCT : lineage composes with the journal - two appends mint two DISTINCT
\                   monotonic audit events (never a content-collapsed set).
\   LIN-AT        : LINEAGE-AT returns each appended event in order, held as a typed id.
\   LIN-DISTINCT  : a different run key has an independent lineage bucket.
\
\ The test reopens package RUN (a friend) to build run keys and MATCH seal-result bare;
\ RLINEAGE and JOURNAL are consumed across their package boundary (qualified). Real ids are
\ minted through their owner constructors (ARTIFACT / NPOL / TARGET / CONFIG); names carry
\ the lin-test prefix.

require lib/test.f
require lib/string.f
require maki/experiment/run-lineage.f
require maki/artifact.f
require maki/numpolicy.f
require maki/target/target.f
require maki/config.f
require maki/journal.f

package RUN

\ populate a canonical run key parameterised by seed (a distinct seed = a distinct run).
: POP-RUN ( n -- ) {: seed:n :}
   NEW
   seed SEED!
   s" lin-test/rng" RNG
   s" lin-test/dataset" ARTIFACT:REGISTER DATASET
   s" lin-test/split" SPLIT
   s" lin-test/prep" PREPROCESS
   s" lin-test/model" ARTIFACT:REGISTER MODEL
   s" lin-test/opt" OPTIMIZER
   NPOL-DOM:EXACT NPOL:REGISTER NUMERIC
   TARGET:SM87 TARGET
   s" lin-test/compiler" CONFIG:REGISTER COMPILER
   s" lin-test/env" CONFIG:REGISTER ENVIRONMENT
   s" lin-test/lic" LICENSE
   s" lin-test/auth" AUTHORITY ;

: SEAL-RUN ( -- CAD-KIND:run-id )
   SEAL MATCH seal-result
      ok OF ENDOF
      incomplete OF -777 throw ENDOF
   ;MATCH ;

: BUILD-RUN ( -- CAD-KIND:run-id )      7 POP-RUN SEAL-RUN ;
: BUILD-RUN-ALT ( -- CAD-KIND:run-id )  99 POP-RUN SEAL-RUN ;

\ ---- ACCEPTANCE: equal keys resume the same lineage -----------------------------
: LIN-RESUME ( -- n )                              \ append across a rebuild -> one lineage
   BUILD-RUN {: id1:CAD-KIND:run-id :}
   id1 s" run/started" RLINEAGE:LINEAGE+ drop
   BUILD-RUN {: id2:CAD-KIND:run-id :}             \ rebuild equal key -> same interned id
   id2 s" run/epoch-1" RLINEAGE:LINEAGE+ drop
   id1 RLINEAGE:LINEAGE-COUNT ;
: LIN-RESUME-EQ ( -- bool )
   BUILD-RUN {: a:CAD-KIND:run-id :}
   BUILD-RUN {: b:CAD-KIND:run-id :}
   a b EQUAL? ;

\ ---- ACCEPTANCE: lineage composes with the append-only journal -------------------
: LIN-EVENTS-DISTINCT ( -- bool )                  \ two appends -> two distinct occurrences
   BUILD-RUN {: id:CAD-KIND:run-id :}
   id s" a" RLINEAGE:LINEAGE+ {: e1:CAD-KIND:audit-event-id :}
   id s" b" RLINEAGE:LINEAGE+ {: e2:CAD-KIND:audit-event-id :}
   e1 e2 JOURNAL:EQUAL? 0= ;
: LIN-AT ( -- bool )                               \ the appended event is retrievable in order
   BUILD-RUN {: id:CAD-KIND:run-id :}
   id RLINEAGE:LINEAGE-COUNT {: base:n :}
   id s" ev-x" RLINEAGE:LINEAGE+ {: ev:CAD-KIND:audit-event-id :}
   id base RLINEAGE:LINEAGE-AT ev JOURNAL:EQUAL? ;

\ ---- ACCEPTANCE: a different run has an independent lineage -----------------------
: LIN-DISTINCT ( -- bool )
   BUILD-RUN {: a:CAD-KIND:run-id :}
   a s" x" RLINEAGE:LINEAGE+ drop
   BUILD-RUN-ALT {: b:CAD-KIND:run-id :}
   b RLINEAGE:LINEAGE-COUNT 0= ;

\ ---- ACCEPTANCE: an out-of-range event index rejects fail-closed -----------------
\ (dot habu-bounds-check-action-39819fc1). LINEAGE-AT guarded only k>=COUNT; a negative k
\ read before the run's event region in LK-EVT and minted a bogus audit-event-id. Both
\ bounds now reject with E-RLINEAGE-CAP before the read and the mint.
: LIN-NEG ( -- )                                   \ k = -1: before the bucket's event region
   BUILD-RUN {: id:CAD-KIND:run-id :}
   id s" ev" RLINEAGE:LINEAGE+ drop
   id -1 RLINEAGE:LINEAGE-AT drop ;
: LIN-OVER ( -- )                                  \ k = COUNT: past the last recorded event
   BUILD-RUN {: id:CAD-KIND:run-id :}
   id s" ev" RLINEAGE:LINEAGE+ drop
   id id RLINEAGE:LINEAGE-COUNT RLINEAGE:LINEAGE-AT drop ;

T-RESET

LIN-RESUME 2 T=          \ both events accumulate under one resumed lineage
LIN-RESUME-EQ TTRUE      \ the rebuild is the same interned identity
LIN-EVENTS-DISTINCT TTRUE
LIN-AT TTRUE
LIN-DISTINCT TTRUE

\ event-index bounds (dot habu-bounds-check-action-39819fc1)
' LIN-NEG  E-RLINEAGE-CAP TTHROWS      \ negative index rejects (was OOB read + bogus event-id mint)
' LIN-OVER E-RLINEAGE-CAP TTHROWS      \ index == count still rejects (upper bound intact)

T-REPORT

;package
