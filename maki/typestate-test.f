\ typestate-test.f - R7 stage typestate acceptance suite (sub-dot
\ v2-typestate-stage-kinds, dot habu-v2-typestate-stage-a0eb43a2).
\
\ Each transition is pinned two ways: a positive control (the right-stage input
\ certifies) and a wrong-stage negative (a differently-staged value of the same
\ underlying kind is a checker reject, verdict 0). Wrong-order is a TYPE error,
\ not a runtime guard. A runnable pipeline (TS-PIPE) proves the transitions also
\ compose and execute, not just type-check. CHECK-QUIET-CANDIDATE! returns -1
\ certified / 0 rejected (test/checker-assert.f).

require lib/test.f
require test/checker-assert.f
require maki/typestate.f

\ Runnable positive control: the full MODEL->...->KIR pipeline composes and runs.
\ (CAND:EMIT / ART:BUILD need a CAD-KIND target/toolchain id, minted only inside
\ their owner packages, so the executable leg stops at KIR:verified; their type
\ order is pinned by the CHECK-QUIET candidates below.)
: TS-PIPE ( -- )
   MODEL:DECLARE MODEL:ELABORATE TIR:SOLVE RIR:LEGALIZE
   PLAN:DRAFT PLAN:FINISH
   KIR:DRAFT KIR:VERIFY
   drop ;

T-RESET

\ ---- positive controls: every transition certifies in the right order --------
s" TS-OK-ELAB ( MODEL:decl -- MODEL:elab ) MODEL:ELABORATE"
   CHECK-QUIET-CANDIDATE! -1 T=
s" TS-OK-SOLVE ( MODEL:elab -- TIR:solved ) TIR:SOLVE"
   CHECK-QUIET-CANDIDATE! -1 T=
s" TS-OK1 ( TIR:solved -- RIR:legal ) RIR:LEGALIZE"
   CHECK-QUIET-CANDIDATE! -1 T=
s" TS-OK-FINISH ( RIR:legal PLAN:draft -- PLAN:complete ) PLAN:FINISH"
   CHECK-QUIET-CANDIDATE! -1 T=
s" TS-OK2 ( PLAN:complete KIR:drafted -- KIR:verified ) KIR:VERIFY"
   CHECK-QUIET-CANDIDATE! -1 T=
s" TS-OK-EMIT ( KIR:verified CAD-KIND:target-id -- CAND:emitted ) CAND:EMIT"
   CHECK-QUIET-CANDIDATE! -1 T=
s" TS-OK-BUILD ( CAND:emitted CAD-KIND:toolchain-id -- ART:built ) ART:BUILD"
   CHECK-QUIET-CANDIDATE! -1 T=

\ ---- wrong-stage negatives: each must be a checker reject (verdict 0) ---------
\ 1. unconstrained Model IR cannot enter region planning (R7 acceptance 1).
s" TS-BAD-ORDER ( MODEL:elab -- RIR:legal ) RIR:LEGALIZE"
   CHECK-QUIET-CANDIDATE! 0 T=
\ 2. a draft plan cannot enter kernel lowering (R7 acceptance 2).
s" TS-BAD-PLAN ( PLAN:draft KIR:drafted -- KIR:verified ) KIR:VERIFY"
   CHECK-QUIET-CANDIDATE! 0 T=
\ 3. an unverified KIR cannot enter target emission (folded review negative).
s" TS-BAD-KIR ( KIR:drafted CAD-KIND:target-id -- CAND:emitted ) CAND:EMIT"
   CHECK-QUIET-CANDIDATE! 0 T=
\ 4. TIR:SOLVE consumes an elaborated model, not an already-solved one.
s" TS-BAD-SOLVE ( TIR:solved -- TIR:solved ) TIR:SOLVE"
   CHECK-QUIET-CANDIDATE! 0 T=
\ 5. a built artifact cannot be re-legalized (stage runs one way).
s" TS-BAD-REBUILD ( ART:built -- RIR:legal ) RIR:LEGALIZE"
   CHECK-QUIET-CANDIDATE! 0 T=

\ ---- executable positive: the pipeline composes and runs ---------------------
TS-PIPE
s" typestate: MODEL->TIR->RIR->PLAN->KIR pipeline ran" type cr

T-REPORT
