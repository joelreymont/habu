\ gate-validation-worker.f - candidate/baseline validation evidence runner.

package GATE-VALIDATION

GT-OUT-CAP constant EVIDENCE-CAP
3 constant WORKER-MAX
\ Existing validation uses 8 nested executions. The direct-lint shared row adds
\ 2 failure children on the candidate and 2 on the baseline: 8 + 2 + 2 = 12.
12 constant NESTED-EXEC-MAX

create CANDIDATE-EVIDENCE EVIDENCE-CAP allot
variable CANDIDATE-EVIDENCE-U
variable WORKERS

: RUN-WORKER ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: exe:ptr exeu:n mode:ptr modeu:n label:ptr labelu:n :}
   WORKERS @ 1+ WORKERS !
   GE-HB-RESET
   s" HABU_UNDER_TEST" >LEN exe exeu >LEN PROC-ENV+
   s" test/candidate-validation.f" GE-ARG+
   mode modeu GE-ARG+
   exe exeu GE-TIMEOUT-MS GE-RUN-ENV
   label labelu GE-EXPECT-OK
   GT-ERR$ nip 0 <> if label labelu GE-FAIL then ;

: SAVE-CANDIDATE ( -- )
   GT-OUT$ CANDIDATE-EVIDENCE swap
   dup CANDIDATE-EVIDENCE-U !
   BYTE-COPY ;

: CHECK-BASELINE ( -- )
   GT-OUT$ CANDIDATE-EVIDENCE CANDIDATE-EVIDENCE-U @ STR= 0= if
      s" candidate validation evidence differs from bin/hb" GE-FAIL
   then ;

: RUN-SHARED ( ptr u8 n -- ) {: candidate:ptr candidateu:n :}
   candidate candidateu s" shared" s" candidate validation shared" RUN-WORKER
   SAVE-CANDIDATE
   s" bin/hb" s" shared" s" baseline validation shared" RUN-WORKER
   CHECK-BASELINE
   CANDIDATE-EVIDENCE CANDIDATE-EVIDENCE-U @ type ;

: RUN-TOP-ROW ( ptr u8 n -- ) {: candidate:ptr candidateu:n :}
   candidate candidateu s" top-row" s" candidate validation top-row" RUN-WORKER
   GT-OUT$ type ;

: CHECK-WORKERS ( -- )
   WORKERS @ WORKER-MAX > if
      s" candidate validation workers=" type WORKERS @ .
      s" max-workers=" type WORKER-MAX . cr
      s" candidate validation worker ratchet exceeded" GE-FAIL
   then ;

: CHECK-NESTED ( -- )
   GS-ON? 0= if
      s" candidate validation ratchet requires HABU_GATE_STATS" GE-FAIL
   then
   GS-READ
   GS-BUF GS-U @ s" process-exec" s" candidate-validation"
      GATE-PROCESS:ROW-COUNT$ {: count:n :}
   count NESTED-EXEC-MAX > if
      s" candidate validation nested-exec=" type count .
      s" max-nested-exec=" type NESTED-EXEC-MAX . cr
      s" candidate validation nested process ratchet exceeded" GE-FAIL
   then ;

public

: RUN ( ptr u8 n -- ) {: candidate:ptr candidateu:n :}
   0 WORKERS !
   candidate candidateu RUN-SHARED
   candidate candidateu RUN-TOP-ROW
   CHECK-WORKERS
   CHECK-NESTED
   s" PASS: candidate validation process ratchet" type cr
   s" PASS: candidate validation evidence matches bin/hb" type cr ;

;package
