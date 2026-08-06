\ cg-mma-slot-neg-test.f - production-shaped falsification regressions for the
\ CHECKED single-buffer K-loop protocol (lib/ptx/cg-mma.f MMA-PIPE-KLOOP-SINGLE,
\ dot habu-wire-cppslot-typestate-ce2463df).
\
\ MMA-PIPE-KLOOP-SINGLE threads a cpp-slot typestate token through its emit-time
\ protocol: MMA-STAGE-ISSUE (the trusted cp.async issue mint, cpp-pending<p>) ->
\ CPPSLOT:COMMIT -> CPPSLOT:WAIT -> CPPSLOT:READ. These candidates use the REAL
\ production mint and protocol words in the loop body's exact shape, so the
\ enforcement is proven on the production vocabulary, not a fixture-only slot:
\   GOOD     the in-order protocol certifies (the shipping loop's shape).
\   BAD-WBC  wait-before-commit: swapping COMMIT/WAIT rejects (WAIT demands a
\            cpp-committed slot; the issued slot is cpp-pending).
\   BAD-DW   dropped wait/sync fence: COMMIT then READ rejects (READ demands a
\            cpp-ready slot; an unwaited slot is cpp-committed).
\   BAD-RBI  read straight after issue rejects (READ demands cpp-ready) - this
\            also PINS the mint's declared state: if MMA-STAGE-ISSUE were ever
\            laundered to return a committed/ready slot, this negative certifies
\            and the suite fails (the anti-tautology guard).
\ Requires cg-mma.f itself, so this file also fails closed if the threaded
\ MMA-PIPE-KLOOP-SINGLE ever stops certifying.

require lib/ptx/neg-test-lib.f
require lib/ptx/cg-mma.f

package MMASB
using PTXN
private

: MAIN ( -- )
   T-RESET
   256 %BLOCK

   \ the production single-buffer protocol shape certifies end to end
   s" GOOD ( -- ) 11 14 MMA-STAGE-ISSUE CPPSLOT:COMMIT CPPSLOT:WAIT CPPSLOT:READ"
   s" single-buffer in-order protocol certifies" ACCEPTS

   s" BAD-WBC ( -- ) 11 14 MMA-STAGE-ISSUE CPPSLOT:WAIT CPPSLOT:COMMIT CPPSLOT:READ"
   s" cpp-committed" s" single-buffer wait-before-commit negative" REJECTS
   s" NEG: wait-before-commit on the production issue mint rejected (needs cpp-committed)" type cr

   s" BAD-DW ( -- ) 11 14 MMA-STAGE-ISSUE CPPSLOT:COMMIT CPPSLOT:READ"
   s" cpp-ready" s" single-buffer dropped-wait negative" REJECTS
   s" NEG: dropped wait/sync fence rejected (READ needs cpp-ready)" type cr

   s" BAD-RBI ( -- ) 11 14 MMA-STAGE-ISSUE CPPSLOT:READ"
   s" cpp-ready" s" single-buffer read-after-issue negative (mint state pin)" REJECTS
   s" NEG: read straight after issue rejected (mint returns cpp-pending, not ready)" type cr

   T-REPORT ;

MAIN

;using
;package
