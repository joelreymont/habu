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
\                      emit / launch; the candidate canary is intact and MX-BUF-INIT's reach count
\                      is unchanged (the shared arena may already be held by an earlier member).

require lib/test.f
require tools/ptx/autotune-sweep.f

package SW-RT

\ ---- thin aliases + scratch config for staging ------------------------------
: DEF! ( n -- )        AUTOTUNE:SW-CAND-DEF ;
: SET! ( n n n -- )    AUTOTUNE:SW-CAND-SET ;
: COPY! ( ptr n n -- ) AUTOTUNE:SW-CAND-COPY ;
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
\ CUDA open, the emit, or the launch. MX-BUF-INIT's reach count stays unchanged and
\ the candidate canary stays intact: the committed witnesses that the reject was inert.
: SWEEP-BADCOUNT ( -- )  AUTOTUNE:SW-CANDS-CAP 1+ 512 AUTOTUNE:AT-SWEEP ;
: SWEEP-BADEDGE ( -- )   1 1024 AUTOTUNE:AT-SWEEP ;
: ZERO-SIDE-EFFECT-TESTS ( -- )
   MMA-EXACT:MX-BUF-INIT-CALLS {: init0:n :}     \ allocation witness: snapshot MX-BUF-INIT's reach count. The MMA-EXACT
                                                 \ host arena is a shared, process-lifetime resource an earlier suite
                                                 \ member may already hold, so absolute readiness is not this file's to
                                                 \ assert - the reach count is (unchanged => the reject never allocated).
   AUTOTUNE:SW-CAND-CANARY-SEED
   [: SWEEP-BADCOUNT ;] E-SW-COUNT TTHROWSQ      \ rejected in AT-SWEEP-VALIDATE, before MX-BUF-INIT / OPEN / emit / launch
   MMA-EXACT:MX-BUF-INIT-CALLS init0 T=          \ the bad-count reject never reached MX-BUF-INIT (allocated nothing)
   [: SWEEP-BADEDGE ;]  MMA-EXACT:MX-E-CAP TTHROWSQ
   MMA-EXACT:MX-BUF-INIT-CALLS init0 T=          \ the bad-edge reject never reached MX-BUF-INIT (allocated nothing)
   AUTOTUNE:SW-CAND-CANARY-INTACT? TTRUE ;       \ and wrote nothing into candidate storage

\ ============ (2) DEVICE EXCLUSIVITY: the injected PID-census fixture matrix =====
\ The census pipeline is device-free at its core: SW-CENSUS-PARSE turns raw nvidia-smi
\ compute-apps bytes + an expected owner into a typed `census` verdict with NO device.
\ So every adversarial / malformed nvidia-smi output the dot enumerates is injected here
\ as a byte string and MUST yield a NON-exclusive verdict (no timing row): a foreign PID,
\ a missing self, a changed set (contended), or a malformed / failed probe (probe-failed).
\ The one exclusive control (sole owner) proves the happy path still yields a row. The
\ clock parser is likewise proven to REJECT the overflowing digit run, so the poison
\ reading that wrapped AT-CLK-STABLE?'s compare can never reach the classifier.
4242 constant OWN     \ the expected sole-owner PID in the post-open fixtures (5555 is a foreign PID in the byte fixtures)

: CPARSE ( ptr u8 n n -- census )                AUTOTUNE:SW-CENSUS-PARSE ;
: CLKPARSE ( ptr u8 n -- result<n,n> )           AUTOTUNE:SW-PARSE-CLK ;
: SET-UUID ( -- )  s" GPU-test" AUTOTUNE:SW-DEV-UUID-SET ;

\ ---- census verdict inspectors (a census is multi-cell: MATCH it, never dup it) ----
: CKIND ( census -- n )     MATCH census exclusive OF drop 0 ENDOF contended OF 2drop 1 ENDOF probe-failed OF 2drop 2 ENDOF ;MATCH ;
: CEXCLN ( census -- n )    MATCH census exclusive OF ENDOF contended OF 2drop -1 ENDOF probe-failed OF 2drop -1 ENDOF ;MATCH ;
: CFGN ( census -- n )      MATCH census exclusive OF drop -1 ENDOF contended OF drop ENDOF probe-failed OF 2drop -1 ENDOF ;MATCH ;
: CTOT ( census -- n )      MATCH census exclusive OF drop -1 ENDOF contended OF nip ENDOF probe-failed OF 2drop -1 ENDOF ;MATCH ;
: CPFCODE ( census -- n )   MATCH census exclusive OF drop -1 ENDOF contended OF 2drop -1 ENDOF probe-failed OF drop ENDOF ;MATCH ;
: CLKOK? ( result<n,n> -- bool )  MATCH result ok OF drop STR-TRUE ENDOF err OF drop STR-FALSE ENDOF ;MATCH ;
: CLKVAL ( result<n,n> -- n )     MATCH result ok OF ENDOF err OF drop -1 ENDOF ;MATCH ;
: MKF ( -- result<pcap:captured,pcap:failed> )  0 >LEN 0 >LEN 1 >RC PCAP-FAILED:MAKE RESULT:ERR ;   \ a nonzero-exit capture

\ ---- verdicts: ownership is a SET IDENTITY, not a count -----------------------
: CENSUS-VERDICT-TESTS ( -- )
   SET-UUID
   s\" 4242, GPU-test\n" OWN CPARSE CKIND 0 T=                       \ sole owner -> EXCLUSIVE (the ONE row-producing case)
   s\" 4242, GPU-test\n" OWN CPARSE CEXCLN 1 T=                      \ exclusive owner count = 1
   s" " -1 CPARSE CKIND 0 T=                                         \ pre-open idle: empty -> EXCLUSIVE(0)
   s" " -1 CPARSE CEXCLN 0 T=
   s\" 999, GPU-test\n" -1 CPARSE CKIND 1 T=                         \ pre-open: a foreign PID present -> contended
   s" " OWN CPARSE CKIND 1 T=                                        \ own PID ABSENT (post-open, empty) -> contended, no row
   s\" 999, GPU-test\n" OWN CPARSE CKIND 1 T=                        \ foreign-only, count 1 -> contended, no row
   s\" 4242, GPU-test\n5555, GPU-test\n" OWN CPARSE CKIND 1 T=       \ own + foreign -> contended
   s\" 4242, GPU-test\n5555, GPU-test\n" OWN CPARSE CFGN 1 T=        \ ...foreign count = 1
   s\" 4242, GPU-test\n5555, GPU-test\n" OWN CPARSE CTOT 2 T= ;      \ ...total = 2

\ ---- probe-failed: malformed / adversarial output is INFRA, never contention --
: CENSUS-PROBEFAIL-TESTS ( -- )
   SET-UUID
   s" 4242" OWN CPARSE CPFCODE AUTOTUNE:SW-PF-BADROW T=              \ truncated: no "pid, uuid" separator
   s\" 4242 GPU-test\n" OWN CPARSE CPFCODE AUTOTUNE:SW-PF-BADROW T=  \ malformed: space where the comma must be
   s\" xyz, GPU-test\n" OWN CPARSE CPFCODE AUTOTUNE:SW-PF-BADROW T=  \ malformed: non-numeric pid
   s\" 4242, \n" OWN CPARSE CPFCODE AUTOTUNE:SW-PF-BADROW T=         \ malformed: empty uuid field
   s\" 4242, GPU-test\n4242, GPU-test\n" OWN CPARSE CPFCODE AUTOTUNE:SW-PF-DUPPID T=   \ duplicate pid
   s\" 999999999999999999999, GPU-test\n" OWN CPARSE CPFCODE AUTOTUNE:SW-PF-BADPID T=  \ overflowing pid
   s\" 4242, GPU-other\n" OWN CPARSE CPFCODE AUTOTUNE:SW-PF-DEVMISS T=                  \ device mismatch (foreign gpu_uuid)
   MKF OWN AUTOTUNE:SW-CENSUS-OF CPFCODE AUTOTUNE:SW-PF-EXIT T= ;    \ nvidia-smi nonzero exit / timeout-class -> probe-failed, NOT contention

\ ---- churn / loss-of-exclusivity across a burst: after-census catches it ------
: CENSUS-CHURN-TESTS ( -- )
   SET-UUID
   s\" 4242, GPU-test\n" OWN CPARSE CKIND 0 T=                       \ BEFORE the burst: sole owner (exclusive)
   s\" 4242, GPU-test\n5555, GPU-test\n" OWN CPARSE CKIND 1 T= ;     \ AFTER the burst: a foreign PID joined -> contended -> burst rejected

\ ---- over-cap set fails closed (short uuid keeps the fixture < SB-CAP) ---------
: CAP-BYTES ( -- ptr u8 n )                       \ SW-PIDS-CAP+1 distinct device rows
   SB-RESET
   AUTOTUNE:SW-PIDS-CAP 1+ 0 ?do
      i 1000 + FMT:SB-INT  s" , GX" SB-APPEND  10 SB-APPEND-C
   loop
   SB$ ;
: CENSUS-CAP-TESTS ( -- )
   s" GX" AUTOTUNE:SW-DEV-UUID-SET
   CAP-BYTES OWN CPARSE CPFCODE AUTOTUNE:SW-PF-CAP T= ;

\ ---- (3) bounded exact clock parse: the overflow poison is refused HERE --------
: CLK-PARSE-TESTS ( -- )
   s\" 208\n" CLKPARSE CLKOK? TTRUE
   s\" 208\n" CLKPARSE CLKVAL 208 T=                                 \ normal reading
   s" 2400" CLKPARSE CLKVAL 2400 T=                                  \ no trailing newline still parses
   s" " CLKPARSE CLKOK? TFALSE                                       \ empty -> err (never a silent 0)
   s\" abc\n" CLKPARSE CLKOK? TFALSE                                 \ non-digit -> err
   s\" 99999999999999\n" CLKPARSE CLKOK? TFALSE ;                    \ overflowing digit run (max-cell class) -> err; poison never reaches AT-CLK-STABLE?

\ ---- fail-closed gate: contended -> E-AT-CONTENDED; probe-failed -> E-SW-PROBE -
: GATE-EXCL ( -- )  1 CENSUS:EXCLUSIVE AUTOTUNE:SW-CENSUS-GATE ;
: GATE-CONT ( -- )  1 2 CENSUS:CONTENDED AUTOTUNE:SW-CENSUS-GATE ;
: GATE-PF ( -- )    AUTOTUNE:SW-PF-EXIT 1 CENSUS:PROBE-FAILED AUTOTUNE:SW-CENSUS-GATE ;
: GATE-TESTS ( -- )
   [: GATE-EXCL ;] 0 TTHROWSQ                                        \ exclusive: no throw (a row may be emitted)
   [: GATE-CONT ;] E-AT-CONTENDED TTHROWSQ                           \ contended: fail closed
   [: GATE-PF ;]   E-SW-PROBE TTHROWSQ ;                             \ probe-failed: infra propagated, NOT relabeled as contention

\ ---- device identity + compute-mode parse -------------------------------------
: DEVID-GOOD ( -- )  s\" GPU-xyz, Default\n" AUTOTUNE:SW-PARSE-DEVID ;
: DEVID-BAD ( -- )   s\" GPU-xyz-no-comma\n" AUTOTUNE:SW-PARSE-DEVID ;
: DEVID-TESTS ( -- )
   DEVID-GOOD
   AUTOTUNE:SW-DEV-UUID$ s" GPU-xyz" T$=                             \ uuid parsed + bound
   AUTOTUNE:SW-DEV-MODE$ s" Default" T$=                             \ compute mode recorded (detection-only under Default)
   [: DEVID-BAD ;] E-SW-PROBE TTHROWSQ ;                             \ malformed identity -> fail closed

T-RESET
CAND-INDEX-TESTS
COUNT-TESTS
EDGE-TESTS
CANARY-TESTS
ZERO-SIDE-EFFECT-TESTS
CENSUS-VERDICT-TESTS
CENSUS-PROBEFAIL-TESTS
CENSUS-CHURN-TESTS
CENSUS-CAP-TESTS
CLK-PARSE-TESTS
GATE-TESTS
DEVID-TESTS
T-REPORT

;package
