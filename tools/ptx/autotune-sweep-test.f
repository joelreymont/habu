\ autotune-sweep-test.f - host-side coverage for the GB sweep harness's bounds
\ (package AUTOTUNE, tools/ptx/autotune-sweep.f). Device-free by design: the sweep
\ file is import-safe (it no longer self-runs the device smoke at load), so this
\ file `require`s it and exercises the candidate-storage bounds, the sweep-parameter
\ validation, and the zero-side-effect rejection witness entirely off-device. The
\ device smoke (AT-SWEEP-SMOKE) and the timed valid path are proven on the GB10.
\
\ Bounds under test:
\   candidate INDEX  - SW-CAND-DEF/COPY/SET reject index outside 0..SW-CANDS-CAP-1 (E-SW-CAND),
\                      because SW-CAND-ROW is package-private + bounds-checked.
\   candidate COUNT  - AT-SWEEP(-VALIDATE) rejects count outside 0..SW-CANDS-CAP (E-SW-COUNT);
\                      negative count is refused, never delegated to a raw ?do.
\   square EDGE      - AT-SWEEP(-VALIDATE) rejects an edge outside 1..MX-MAX (MX-E-CAP), so a
\                      positive tileable n>512 can no longer pass the per-config shape check and
\                      then over-run the MMA-EXACT host/packed buffers.
\   ZERO side effect - a rejected sweep throws the validator's named code (only AT-SWEEP-VALIDATE
\                      throws it), so control never reached MX-BUF-INIT / solo-wait / CUDA open /
\                      emit / launch; the candidate canary is intact and the host arena unallocated.

require lib/test.f
require tools/ptx/autotune-sweep.f

package SW-RT

\ ---- thin aliases + scratch config for staging ------------------------------
: DEF! ( n -- )        AUTOTUNE:SW-CAND-DEF ;
: SET! ( n n n -- )    AUTOTUNE:SW-CAND-SET ;
: COPY! ( ptr a n -- ) AUTOTUNE:SW-CAND-COPY ;
: CANDOK? ( n -- bool ) AUTOTUNE:SW-CAND-OK? ;
: CNTOK? ( n -- bool ) AUTOTUNE:SW-COUNT-OK? ;
create SRC AUTOTUNE:AT-CFG-N cells allot     \ a valid source config to copy

\ ---- (index) SW-CAND-* die named on an out-of-range candidate index ----------
: IDX-DEF-NEG ( -- )  -1 DEF! ;
: IDX-DEF-CAP ( -- )  AUTOTUNE:SW-CANDS-CAP DEF! ;             \ == SW-CANDS-CAP (33 rows would need index CAP; out of range)
: IDX-DEF-OVER ( -- ) AUTOTUNE:SW-CANDS-CAP 1+ DEF! ;
: IDX-DEF-HUGE ( -- ) $7FFFFFFFFFFFFFFF DEF! ;
: IDX-SET-CAP ( -- )  7 AUTOTUNE:SW-CANDS-CAP AUTOTUNE:AT-WARPS SET! ;
: IDX-COPY-CAP ( -- ) SRC AUTOTUNE:SW-CANDS-CAP COPY! ;
: CAND-INDEX-TESTS ( -- )
   \ predicate over the required boundary set: -1/0/31/32/33/huge
   -1 CANDOK? TFALSE   0 CANDOK? TTRUE   31 CANDOK? TTRUE
   AUTOTUNE:SW-CANDS-CAP CANDOK? TFALSE          \ 32 == capacity is NOT a valid index
   AUTOTUNE:SW-CANDS-CAP 1+ CANDOK? TFALSE   $7FFFFFFFFFFFFFFF CANDOK? TFALSE
   \ every staging word rejects a bad index with the named code
   [: IDX-DEF-NEG ;]  E-SW-CAND TTHROWSQ
   [: IDX-DEF-CAP ;]  E-SW-CAND TTHROWSQ
   [: IDX-DEF-OVER ;] E-SW-CAND TTHROWSQ
   [: IDX-DEF-HUGE ;] E-SW-CAND TTHROWSQ
   [: IDX-SET-CAP ;]  E-SW-CAND TTHROWSQ
   [: IDX-COPY-CAP ;] E-SW-CAND TTHROWSQ
   \ the in-range boundary indices stage cleanly (no throw)
   0 DEF!  31 DEF!  7 31 AUTOTUNE:AT-WARPS SET! ;

\ ---- (count) AT-SWEEP-VALIDATE bounds the candidate count 0..SW-CANDS-CAP ------
: CNT-NEG ( -- )  -1 512 AUTOTUNE:AT-SWEEP-VALIDATE ;
: CNT-OVER ( -- ) AUTOTUNE:SW-CANDS-CAP 1+ 512 AUTOTUNE:AT-SWEEP-VALIDATE ;
: CNT-HUGE ( -- ) $7FFFFFFFFFFFFFFF 512 AUTOTUNE:AT-SWEEP-VALIDATE ;
: COUNT-TESTS ( -- )
   -1 CNTOK? TFALSE   0 CNTOK? TTRUE   AUTOTUNE:SW-CANDS-CAP CNTOK? TTRUE   \ 0..CAP inclusive (CAP rows use index 0..CAP-1)
   AUTOTUNE:SW-CANDS-CAP 1+ CNTOK? TFALSE   $7FFFFFFFFFFFFFFF CNTOK? TFALSE
   [: CNT-NEG ;]  E-SW-COUNT TTHROWSQ            \ negative count refused, not handed to a raw ?do
   [: CNT-OVER ;] E-SW-COUNT TTHROWSQ
   [: CNT-HUGE ;] E-SW-COUNT TTHROWSQ
   0 512 AUTOTUNE:AT-SWEEP-VALIDATE              \ count 0 + valid edge: no throw
   AUTOTUNE:SW-CANDS-CAP 512 AUTOTUNE:AT-SWEEP-VALIDATE ;   \ count == capacity: no throw

\ ---- (edge) AT-SWEEP-VALIDATE bounds the square edge 1..MX-MAX ----------------
: EDGE-NEG ( -- )  1 -1 AUTOTUNE:AT-SWEEP-VALIDATE ;
: EDGE-ZERO ( -- ) 1 0 AUTOTUNE:AT-SWEEP-VALIDATE ;
: EDGE-513 ( -- )  1 513 AUTOTUNE:AT-SWEEP-VALIDATE ;
: EDGE-1024 ( -- ) 1 1024 AUTOTUNE:AT-SWEEP-VALIDATE ;         \ tileable by BN=64/BROWS but past the buffers
: EDGE-HUGE ( -- ) 1 $7FFFFFFFFFFFFFFF AUTOTUNE:AT-SWEEP-VALIDATE ;
: EDGE-TESTS ( -- )
   [: EDGE-NEG ;]  MMA-EXACT:MX-E-CAP TTHROWSQ
   [: EDGE-ZERO ;] MMA-EXACT:MX-E-CAP TTHROWSQ
   [: EDGE-513 ;]  MMA-EXACT:MX-E-CAP TTHROWSQ
   [: EDGE-1024 ;] MMA-EXACT:MX-E-CAP TTHROWSQ
   [: EDGE-HUGE ;] MMA-EXACT:MX-E-CAP TTHROWSQ
   1 1 AUTOTUNE:AT-SWEEP-VALIDATE                \ edge 1: no throw
   1 511 AUTOTUNE:AT-SWEEP-VALIDATE              \ edge 511: no throw
   1 MMA-EXACT:MX-MAX AUTOTUNE:AT-SWEEP-VALIDATE ; \ edge 512 (MX-MAX): no throw

\ ---- (canary) staging the boundary rows never crosses the arena guard cell ----
: CANARY-TESTS ( -- )
   AUTOTUNE:SW-CAND-CANARY-SEED
   0 DEF!  AUTOTUNE:SW-CANDS-CAP 1- DEF!         \ stage the first and LAST valid rows (index 0 and CAP-1)
   AUTOTUNE:SW-CAND-CANARY-INTACT? TTRUE ;       \ neither write reached the guard cell one past the arena

\ ---- (zero side effect) a rejected sweep does no allocation / device work -----
\ AT-SWEEP validates FIRST, so a bad count/edge throws the validator's own code
\ (E-SW-COUNT / MX-E-CAP) - control never reached MX-BUF-INIT, the solo-wait, the
\ CUDA open, the emit, or the launch. The host arena stays unallocated and the
\ candidate canary stays intact: the committed witnesses that the reject was inert.
: SWEEP-BADCOUNT ( -- )  AUTOTUNE:SW-CANDS-CAP 1+ 512 AUTOTUNE:AT-SWEEP ;
: SWEEP-BADEDGE ( -- )   1 1024 AUTOTUNE:AT-SWEEP ;
: ZERO-SIDE-EFFECT-TESTS ( -- )
   MMA-EXACT:MX-BUF-READY? TFALSE                \ allocation witness: host arena unallocated at entry
   AUTOTUNE:SW-CAND-CANARY-SEED
   [: SWEEP-BADCOUNT ;] E-SW-COUNT TTHROWSQ      \ rejected before MX-BUF-INIT / OPEN / emit / launch
   [: SWEEP-BADEDGE ;]  MMA-EXACT:MX-E-CAP TTHROWSQ
   MMA-EXACT:MX-BUF-READY? TFALSE                \ still unallocated: the rejections allocated nothing
   AUTOTUNE:SW-CAND-CANARY-INTACT? TTRUE ;       \ and wrote nothing into candidate storage

T-RESET
CAND-INDEX-TESTS
COUNT-TESTS
EDGE-TESTS
CANARY-TESTS
ZERO-SIDE-EFFECT-TESTS
T-REPORT

;package
