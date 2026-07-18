\ maki/db/diff-runner-spawn.f - the SPAWN-ISOLATED subject adapter + the external-process
\ PyTorch reference adapter for the differential runner (maki/db/diff-runner.f; dot
\ habu-v2-differential-runner-13359019).
\
\ CONCERN: the process/fs isolation layer the pure runner core (maki/db/diff-runner.f)
\ deliberately keeps out. This file reopens package DIFFRUN and installs two concrete
\ adapters, both real, both outside the runner's checked semantics at the process boundary:
\
\   SPAWN-SRC / SPAWN-CASE : the ISOLATED subject adapter (the grader pattern,
\     maki/eval-device.f). Each case runs in a FRESH spawned bin/hb child, not in-process:
\     an untrusted / generated subject that crashes or hangs is a graded FAULT, never a
\     grader casualty, and a fresh exec image cannot corrupt this process's dictionary. The
\     child writes its scalar to stdout; the parent captures (outu erru outcome) with the
\     capture timeout and classifies through the core CLASSIFY-OUTCOME: exit(0) + a parseable
\     scalar -> produced; timeout (hung, SIGKILL-reaped) / signal death / nonzero exit /
\     unparseable output -> faulted. So a hanging/dying case reaps as its OWN taxonomy member
\     and can never be graded a wrong value - the EVN-DEVICE-FAULT vs WRONG precedent.
\     SPAWN-CASE's per-case driver is an identity scalar subject (prints the case parameter);
\     a real suite substitutes the subject-under-test source through the same isolation harness
\     (the subject-source-injection protocol is a follow-on, not the core).
\
\   TORCH-REFERENCE : the PyTorch reference adapter INTERFACE. A differential reference runs
\     OUTSIDE Habu semantics as a spawned external torch process (the maki/onnx/ort-ref-test.f
\     external-reference pattern). Off-device / without a torch toolchain (DIFFRUN_TORCH unset)
\     the leg records a SKIP - the runner's reference-skip verdict - so the in-gate suite is
\     deterministic without PyTorch; the deterministic scripted fake reference used by the
\     in-gate tests lives in maki/db/diff-runner-test.f. A real torch integration replaces the
\     TORCH-REFERENCE body with the external spawn + capture, gated by TORCH-AVAILABLE?.
\
\ maki -> habu only. No new error codes (reuses the runner's + lib/process's).

require lib/string.f
require lib/fmt.f
require lib/fs.f                    \ GETENV
require lib/fs-mutate.f            \ WRITE-ALL
require lib/process-argv.f         \ PROC-ARGV+ / RUN-ARGV-CAPTURE-OUTCOME
require maki/device-artifacts.f    \ MAKI-GRADE:PREPARE / DRIVER$ / CLEAN (private temp driver path)
require maki/db/diff-runner.f      \ package DIFFRUN: CLASSIFY-OUTCOME, run-result wrappers, installers

package DIFFRUN
private

$2000 constant SP-OUT-CAP
$1000 constant SP-ERR-CAP
$2000 constant SP-SRC-CAP
30000 constant SPAWN-MS             \ per-case capture deadline (30s class, like the grader's launch)
create SP-OUT SP-OUT-CAP allot
create SP-ERR SP-ERR-CAP allot
create SP-SRC SP-SRC-CAP allot      \ stable source copy (a caller may pass a shared-builder span)

\ ---- captured-stdout scalar parse: a clean exit with a parseable int is produced ---------
: PARSE-INT ( ptr u8 n -- run-result )
   STR>NUMBER? MATCH option
      none OF >FAULTED ENDOF        \ clean exit but no scalar on stdout: a protocol fault
      some OF >PRODUCED ENDOF       \ the parsed scalar -> produced
   ;MATCH ;

: SPAWN-ARGV ( -- )
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   MAKI-GRADE:DRIVER$ >LEN PROC-ARGV+ ;

\ a self-contained driver printing the subject's scalar for case n (identity placeholder).
\ The subject signals SUCCESS by clean natural completion (exit 0), never `bye`: a mid-load
\ `bye` exits with the FFI-file convention code (70), which the fault taxonomy would (rightly)
\ read as a nonzero-exit fault. A crashing/hanging subject still faults via die/throw/timeout.
: CASE-SRC ( n -- ptr u8 n ) {: n:n :}
   SB-RESET
   s" require lib/string.f" SB-APPEND  10 SB-APPEND-C
   s" require lib/fmt.f" SB-APPEND     10 SB-APPEND-C
   s" SB-RESET " SB-APPEND  n SB-INT  s"  SB-INT SB$ type" SB-APPEND  10 SB-APPEND-C
   SB$ ;

public

\ SPAWN-SRC runs source in a fresh bin/hb child under a capture timeout and classifies the
\ outcome into the runner's run-result taxonomy (clean exit + parseable scalar -> produced;
\ hang / signal / nonzero exit / unparseable -> faulted).
: SPAWN-SRC ( ptr u8 n n -- run-result ) {: a:ptr u:n ms:n :}
   u SP-SRC-CAP > if E-DIFFRUN-BUF throw then
   a SP-SRC u BYTE-COPY                        \ copy first: MAKI-GRADE:PREPARE may reuse the shared builder
   s" diffrun-spawn" MAKI-GRADE:PREPARE
   MAKI-GRADE:DRIVER$ SP-SRC u WRITE-ALL
   SPAWN-ARGV
   s" bin/hb" >LEN  SP-OUT SP-OUT-CAP >LEN  SP-ERR SP-ERR-CAP >LEN  ms >MS  RUN-ARGV-CAPTURE-OUTCOME
   CLASSIFY-OUTCOME {: outu:len erru:len clean:bool :}
   MAKI-GRADE:CLEAN
   clean if SP-OUT outu LEN>N PARSE-INT else >FAULTED then ;

: SPAWN-CASE ( n -- run-result )   CASE-SRC SPAWN-MS SPAWN-SRC ;
: SPAWN-SUBJECT! ( -- )            [: SPAWN-CASE ;] SUBJECT! ;

\ ---- external-process PyTorch reference adapter interface --------------------------------
: TORCH-AVAILABLE? ( -- bool )   s" DIFFRUN_TORCH" GETENV nip 0<> ;

\ Off-device / without a torch toolchain this reference leg records a SKIP (the reference
\ runs outside Habu semantics as a spawned external process; a real integration replaces this
\ body with that external spawn + capture, gated by TORCH-AVAILABLE?).
: TORCH-REFERENCE ( n -- ref-result )   drop >SKIP ;
: TORCH-REFERENCE! ( -- )               [: TORCH-REFERENCE ;] REFERENCE! ;

;package
