\ cuda-scope-leak-proof-test.f - host-only both-direction leak proof for the tools/ptx
\ CUDA-lifecycle migration (dot habu-migrate-ptx-tool-f57679ef).
\
\ The migrated launchers/gradchecks/device-tests and PTXBENCH transfer every acquired
\ CUDA resource into a CUDA-SCOPE frame (OWN-PRIMARY-CTX / OWN-MODULE / OWN-EVENT /
\ OWN-DEVPTR) and let the frame unwind them on return OR throw. This suite pins that the
\ migration actually removes the leak, using the SAME injection seam CUDA-SCOPE exposes:
\ recording fake release ops (REL-*!) that log each release handle in order, with NO GPU.
\
\ Two representative shapes, each proved BOTH ways:
\   LAUNCHER  (sum-launch / cuda-launch / acc / redadd shape): own ctx, module, and the
\             per-run device buffers; a throw at the launch/readback step.
\   BENCH     (tools/ptx/bench.f BENCH-GPU-NS shape): an outer frame owns ctx + module; a
\             nested per-burst frame owns the two timing events; a throw mid-burst.
\ For each shape the UNFIXED-BASE model runs the acquisitions then throws BEFORE its
\ open-coded release list (release never called -> the fake logs nothing = leak); the
\ MIGRATED model owns each resource then throws (the frame unwinds every owned row in
\ reverse acquisition order, exactly once = no leak). Handle scheme mirrors cuda-scope-test.f
\ (ctx=10, mod=20, evt/start=30, evt/stop=40 or buffers 30/40).

require lib/test.f
require lib/ptx/cuda-scope.f

package CUDA-SCOPE-LEAK-PROOF

-7777 constant E-PRIMARY                          \ a mid-pipeline body error distinct from E-CUDA (-5002)

\ ---- recording fake release table (no GPU; logs release handles in order) --------
64 constant RLOG-CAP
create RLOG RLOG-CAP cells allot
variable RLOG-N
: RLOG-RESET ( -- )  0 RLOG-N ! ;
: RLOG-PUT ( n -- )
   RLOG-N @ RLOG-CAP >= if E-CUDA throw then
   RLOG-N @ cells RLOG + !
   RLOG-N @ 1+ RLOG-N ! ;
: RLOG@ ( n -- n )  cells RLOG + @ ;
: RLOG-N@ ( -- n )  RLOG-N @ ;
: FREL ( n -- n )  {: h:n :}  h RLOG-PUT  0 ;      \ the injected release op for every kind: log, succeed
: INSTALL-FAKES ( -- )
   [: FREL ;] CUDA-SCOPE:REL-CTX!
   [: FREL ;] CUDA-SCOPE:REL-MOD!
   [: FREL ;] CUDA-SCOPE:REL-EVT!
   [: FREL ;] CUDA-SCOPE:REL-DEVPTR! ;
variable FAIL?                                     \ armed true at runtime; conditional so the checker
: HARNESS-RESET ( -- )  RLOG-RESET  CUDA-SCOPE:RESET  STR-TRUE FAIL? ! ;   \ keeps the base release list statically reachable

\ the mid-pipeline step (launch / sync / readback / timed-burst) that fails; conditional throw so
\ the checker sees the base's open-coded release list after it as reachable (runtime-skipped here).
: FAIL-STEP ( -- )  FAIL? @ if E-PRIMARY throw then ;

\ ================= LAUNCHER shape (own ctx, module, 2 buffers) =====================
\ Migrated: SETUP owns ctx+module, LAUNCH owns the two device buffers, then the launch throws.
: MIG-LAUNCH-BODY ( -- )
   10 >CUDA-DEV    CUDA-SCOPE:OWN-PRIMARY-CTX      \ ctx retained + owned
   20 >CUDA-MOD    CUDA-SCOPE:OWN-MODULE           \ module loaded + owned
   30 >CUDA-DEVPTR CUDA-SCOPE:OWN-DEVPTR           \ DIN allocated + owned
   40 >CUDA-DEVPTR CUDA-SCOPE:OWN-DEVPTR           \ DOUT allocated + owned
   FAIL-STEP ;                                     \ launch/sync/readback fails mid-pipeline
: MIG-LAUNCH ( -- )  [: MIG-LAUNCH-BODY ;] CUDA-SCOPE:SCOPE ;

\ Unfixed base: acquisitions happen, a throw at the launch step skips the open-coded release
\ list entirely (RS-RELEASE / the trailing CU-MEM-FREE list), so nothing is released.
: BASE-LAUNCH ( -- )
   \ (real base would CU-DEVICE-PRIMARY-CTX-RETAIN / CU-MODULE-LOAD / CU-MEM-ALLOC x2 here)
   FAIL-STEP                                       \ launch/sync/readback fails mid-pipeline...
   40 FREL drop  30 FREL drop                      \ ...so this open-coded release list never runs
   20 FREL drop  10 FREL drop ;

: T-LAUNCH-BASE-LEAKS ( -- )                       \ UNFIXED: mid-pipeline throw -> release never called
   HARNESS-RESET
   [: BASE-LAUNCH ;] E-PRIMARY TTHROWSQ            \ the primary error propagates
   RLOG-N@ 0 T=                                    \ ZERO releases logged = every resource leaked
   CUDA-SCOPE:DEPTH 0 T= ;

: T-LAUNCH-MIG-UNWINDS ( -- )                      \ MIGRATED: same throw -> full reverse-order unwind
   HARNESS-RESET
   [: MIG-LAUNCH ;] E-PRIMARY TTHROWSQ             \ primary error still wins after the frame unwinds
   RLOG-N@ 4 T=                                    \ all four released, exactly once each
   0 RLOG@ 40 T=  1 RLOG@ 30 T=  2 RLOG@ 20 T=  3 RLOG@ 10 T=   \ reverse acquisition order
   CUDA-SCOPE:DEPTH 0 T= ;                          \ ledger empty = no outstanding resource

\ ================= BENCH shape (outer ctx+module, nested per-burst events) =========
\ Migrated bench: an outer frame owns ctx + module; BENCH-GPU-NS opens a NESTED frame owning
\ the two timing events; a throw mid-burst unwinds the events (reverse) then the outer frame.
: MIG-BURST-BODY ( -- )
   30 >CUDA-EVENT CUDA-SCOPE:OWN-EVENT             \ start event created + owned (per-burst frame)
   40 >CUDA-EVENT CUDA-SCOPE:OWN-EVENT             \ stop event created + owned
   FAIL-STEP ;                                     \ a launch in the timed loop fails mid-burst
: MIG-BENCH-BODY ( -- )
   10 >CUDA-DEV CUDA-SCOPE:OWN-PRIMARY-CTX         \ outer frame: ctx
   20 >CUDA-MOD CUDA-SCOPE:OWN-MODULE              \ outer frame: module
   [: MIG-BURST-BODY ;] CUDA-SCOPE:SCOPE ;         \ nested per-burst frame (throws, unwinds its events, re-throws)
: MIG-BENCH ( -- )  [: MIG-BENCH-BODY ;] CUDA-SCOPE:SCOPE ;

\ Unfixed base bench: events created, a throw mid-burst skips EVENTS-DESTROY and the outer
\ UNLOAD/CLOSE, leaking both events + module + context.
: BASE-BENCH ( -- )
   \ (real base would retain ctx, load module, CU-EVENT-CREATE x2 here)
   FAIL-STEP                                       \ a launch in the timed loop fails mid-burst...
   40 FREL drop  30 FREL drop                      \ ...EVENTS-DESTROY never runs
   20 FREL drop  10 FREL drop ;                    \ ...UNLOAD/CLOSE never run

: T-BENCH-BASE-LEAKS ( -- )
   HARNESS-RESET
   [: BASE-BENCH ;] E-PRIMARY TTHROWSQ
   RLOG-N@ 0 T=                                    \ nothing released = events/module/ctx all leaked
   CUDA-SCOPE:DEPTH 0 T= ;

: T-BENCH-MIG-UNWINDS ( -- )
   HARNESS-RESET
   [: MIG-BENCH ;] E-PRIMARY TTHROWSQ
   RLOG-N@ 4 T=                                    \ events (nested) then module+ctx (outer), all released
   0 RLOG@ 40 T=  1 RLOG@ 30 T=                    \ nested per-burst frame first: stop then start event
   2 RLOG@ 20 T=  3 RLOG@ 10 T=                    \ outer frame next: module then ctx
   CUDA-SCOPE:DEPTH 0 T= ;

: RUN ( -- )
   T-RESET
   INSTALL-FAKES
   T-LAUNCH-BASE-LEAKS
   T-LAUNCH-MIG-UNWINDS
   T-BENCH-BASE-LEAKS
   T-BENCH-MIG-UNWINDS
   CUDA-SCOPE:USE-REAL-DRIVER                      \ restore the real release table for any later loader
   T-REPORT ;

RUN

;package
