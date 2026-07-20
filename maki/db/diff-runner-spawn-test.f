\ maki/db/diff-runner-spawn-test.f - REAL spawn-isolation acceptance for the differential
\ runner's subject/reference adapters (maki/db/diff-runner-spawn.f; dot
\ habu-v2-differential-runner-13359019).
\
\ ACCEPTANCE (b), proven with ACTUAL spawned bin/hb children (not constructed outcomes):
\   PRODUCE-OK?    : a child that cleanly exits printing a scalar -> produced <value>.
\   HANG-FAULTED?  : a child that HANGS is SIGKILL-reaped at the capture deadline (timeout)
\                    -> faulted, NEVER a value. (the taxonomy member is its own, not wrong-value)
\   CRASH-FAULTED? : a child that DIES with a nonzero exit -> faulted, never a value.
\   CASE-PRODUCES? : the shipped SPAWN-CASE subject adapter runs a case in a fresh isolated
\                    child and produces the case scalar - each case is spawn-isolated.
\ ACCEPTANCE (c): the external-process PyTorch reference adapter records a SKIP off-device
\   (TORCH-AVAILABLE? false without DIFFRUN_TORCH), so the reference legs stay outside Habu
\   semantics and the in-gate suite is deterministic without PyTorch.
\
\ This is the real-process complement to the deterministic classifier proof in
\ maki/db/diff-runner-test.f (which pins the outcome->taxonomy mapping over CONSTRUCTED
\ outcomes). The child sources use S\" escaped literals (docs/forth.md). Reopens package
\ DIFFRUN (a friend) for the spawn adapter surface.

require lib/test.f
require lib/string.f
require maki/db/diff-runner-spawn.f

package DIFFRUN

\ ---- child driver sources -------------------------------------------------------------
: PRODUCE-SRC ( -- ptr u8 n )   \ prints its scalar and completes naturally (exit 0), no `bye`
   s\" require lib/string.f\nrequire lib/fmt.f\nSB-RESET 42 FMT:SB-INT SB$ type\n" ;
: HANG-SRC ( -- ptr u8 n )
   s\" : DR-HANG ( -- ) begin 0 0 = while repeat ;\nDR-HANG\n" ;
: CRASH-SRC ( -- ptr u8 n )
   s\" s\q x\q 7 die\n" ;

\ ---- (b): real spawn classification ---------------------------------------------------
: PRODUCE-OK? ( -- bool )
   PRODUCE-SRC 20000 SPAWN-SRC MATCH run-result
      produced OF 42 = ENDOF
      faulted  OF false ENDOF
   ;MATCH ;
: HANG-FAULTED? ( -- bool )   HANG-SRC 1500 SPAWN-SRC RUN-RESULT>N 1 = ;
: CRASH-FAULTED? ( -- bool )  CRASH-SRC 20000 SPAWN-SRC RUN-RESULT>N 1 = ;
: CASE-PRODUCES? ( -- bool )   \ the shipped isolated subject adapter runs a case in a child
   7 SPAWN-CASE MATCH run-result
      produced OF 7 = ENDOF
      faulted  OF false ENDOF
   ;MATCH ;

\ ---- (c): external-process reference records a skip off-device ------------------------
: TORCH-SKIP? ( -- bool )    5 TORCH-REFERENCE REF-RESULT>N 1 = ;
: TORCH-UNAVAIL? ( -- bool ) TORCH-AVAILABLE? 0= ;

T-RESET

PRODUCE-OK? TTRUE          \ clean exit + scalar -> produced
HANG-FAULTED? TTRUE        \ hung child (timeout, SIGKILL-reaped) -> faulted, never wrong-value
CRASH-FAULTED? TTRUE       \ dying child (nonzero exit) -> faulted, never wrong-value
CASE-PRODUCES? TTRUE       \ shipped spawn-isolated subject adapter produces the case scalar
TORCH-SKIP? TTRUE          \ off-device PyTorch reference records a skip
TORCH-UNAVAIL? TTRUE       \ no torch toolchain in-gate

T-REPORT

;package
