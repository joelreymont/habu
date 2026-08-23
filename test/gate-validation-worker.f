\ gate-validation-worker.f - candidate/baseline validation evidence runner.

package GATE-VALIDATION

GT-OUT-CAP constant EVIDENCE-CAP

create CANDIDATE-EVIDENCE EVIDENCE-CAP allot
variable CANDIDATE-EVIDENCE-U

: RUN-WORKER ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: exe:ptr exeu:n mode:ptr modeu:n label:ptr labelu:n :}
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

public

: RUN ( ptr u8 n -- ) {: candidate:ptr candidateu:n :}
   candidate candidateu RUN-SHARED
   candidate candidateu RUN-TOP-ROW
   s" PASS: candidate validation evidence matches bin/hb" type cr ;

;package
