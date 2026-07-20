\ autotune-sweep.f - (3) STOPWATCH: the GB sweep harness under gate discipline.
\
\ dot habu-committed-autotune-menu-4321e05d item (3). Where Triton autotunes at
\ first call with a handful of noisy warmup reps, Habu sweeps OFFLINE under a
\ discipline strictly stronger than that: for each candidate config the harness
\   1. ELEMENT-EXACT PRECONDITION - runs the integer-fill element-exact check at
\      the sweep shape BEFORE timing; a config that is not bit-exact is EXCLUDED
\      from timing with a recorded reason (listed, never silently skipped).
\   2. SOLO GPU - waits (bounded, fail-closed) until no OTHER compute process is
\      on the device before it times; it never times under contention.
\   3. SUSTAINED-CLOCK VERIFICATION - reads the SM clock before/after every timed
\      burst and rejects/reruns a sample whose clock sagged past a stated
\      tolerance; the clock evidence is recorded per row.
\   4. BEST-OF-3 - the reported ns is the minimum over three clock-stable reps.
\   5. The rows are emitted as a CANDIDATE report in the perf-rows.tsv 12-field
\      layout for the maintainer to review; the harness NEVER writes perf-rows.tsv
\      itself (committed winners are reviewed data, per the dot's WINNERS rule).
\
\ The device-free decisions (clock classifier, exclusion bookkeeping, row
\ formatting) live in tools/ptx/autotune.f (package AUTOTUNE) and are unit-tested
\ in the gate's ptx-toolchain slice (tools/ptx/autotune-test.f). This file adds the
\ GPU-driving orchestration to the SAME package, so the entry point AT-SWEEP is a
\ named AUTOTUNE word. IMPORT-SAFE by construction: loading this file defines the
\ words but opens no libcuda, allocates nothing, assembles nothing, and runs no
\ sweep - the candidate staging + bounds validation are host-side and unit-tested
\ off-device (tools/ptx/autotune-sweep-test.f). AT-SWEEP-SMOKE is the explicit
\ device entry (it opens a CUDA context and drives the GB10); a human or a device
\ suite invokes it, and it self-skips off-device via CUDA:OPEN?.
\
\ ELEMENT-EXACT: the integer fill / f64 host reference / zero-tolerance compare / dtype
\ pack / device alloc-htod-params-dtoh / ptxas assemble are the import-safe library
\ tools/ptx/mma-exact-lib.f (package MMA-EXACT), shared with tools/ptx/mma-gemm-check.f -
\ loading it runs no campaign, so this harness reuses the canonical proof instead of
\ copying it (the copy this file once carried is gone).

require lib/errors.f
require lib/string.f
require lib/float.f
require lib/fmt.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require src/arch/ptx/emit.f
require lib/ptx/cg.f
require lib/ptx/cg-matmul.f
require lib/ptx/cg-mma.f
require lib/ptx/toolchain.f
require maki/eval/active-target.f
require maki/array.f
require tools/ptx/bench.f
require tools/ptx/mma-exact-lib.f
require tools/ptx/profile.f
require tools/ptx/autotune.f

-7315 constant E-SW-COUNT      \ sweep candidate count outside 0..SW-CANDS-CAP (would read config rows past SW-CANDS)
-7316 constant E-SW-CAND       \ staged candidate index outside 0..SW-CANDS-CAP-1 (would read/write a config row past SW-CANDS)

package AUTOTUNE
public

\ ---- per-candidate emit + element-exact precondition launch -------------------
\ The element-exact buffers / integer fill / f64 reference / zero-tolerance compare /
\ dtype pack, the device alloc/free/params/htod/dtoh, and the ptxas assemble are the
\ shared import-safe library tools/ptx/mma-exact-lib.f (package MMA-EXACT); this harness
\ composes them. NB: no PTXBENCH:RESET here - the sweep opens ONE context (AT-SWEEP) and
\ RESET would zero its DEV/CTX. CUBIN!/KERNEL!/LABEL! set their own lengths; MX-PARAMS sets
\ grid/block; BENCH-GPU-NS manages its own events. So per-config we only (re)point + reload.
private
: SW-EMIT-LOAD ( -- )
   s" habu-at-sweep" PTXTC:PREPARE
   PTX-CAPTURE-ON  EMIT-MATMUL-MMA  PTX-CAPTURE-OFF
   MMA-EXACT:MX-ASSEMBLE
   PTXTC:CUBIN$ PTXBENCH:CUBIN!
   s" MMM" PTXBENCH:KERNEL!  s" MMM" PTXBENCH:LABEL! ;

\ the element-exact precondition launch (real data, one launch): fill + host reference,
\ pack + upload A,B, zero C, launch + sync, read C back. Compare is MMA-EXACT:MX-COMPARE.
: SW-EXACT-LAUNCH ( -- )
   MMA-EXACT:MX-N @ dup * {: e:n :}
   MMA-EXACT:MX-FILL  MMA-EXACT:MX-REF
   e MMA-EXACT:MX-PACK-AB
   e MMA-EXACT:MX-HTOD-AB
   MMA-EXACT:MX-DC @ 0 e PTXBENCH:DEVICE-MEMSET32
   PTXBENCH:LAUNCH  PTXBENCH:SYNC
   e MMA-EXACT:MX-DTOH-C ;

\ ---- (2) SOLO GPU: fail-closed wait for an uncontended device -----------------
\ "Solo" = the compute-app count is within a self-limit: 0 before this process
\ opens its CUDA context (a truly idle GPU), 1 once it holds one (only itself).
\ The sweep opens ONE context and checks `1 SW-WAIT-SOLO` before EVERY timing pass,
\ so its own long-lived context is never mistaken for contention while any OTHER
\ process pushes the count past the limit and makes it wait. A competing kernel
\ that slips in mid-burst is independently caught by the clock-stability guard.
2000 constant SW-SMI-MS                     \ per nvidia-smi call timeout
40 constant SW-SOLO-TRIES                   \ bounded solo-wait retries (nvidia-smi latency is the backoff)
$400 constant SW-SMI-CAP    $400 constant SW-SMI-ECAP
create SW-SMI-OUT SW-SMI-CAP allot   create SW-SMI-ERR SW-SMI-ECAP allot
: SW-NL-COUNT ( ptr u8 n -- n ) {: a:ptr u:n :}   \ compute-app pids = newline-terminated lines
   0  u 0 ?do  a i + c@ 10 = if 1+ then  loop ;
: SW-COMPUTE-COUNT ( -- n )                 \ number of GPU compute apps, or -1 on a failed probe (fail closed)
   PROC-ARGV-RESET
   s" --query-compute-apps=pid" >LEN PROC-ARGV+
   s" --format=csv,noheader" >LEN PROC-ARGV+
   s" /usr/bin/nvidia-smi" >LEN  SW-SMI-OUT SW-SMI-CAP >LEN  SW-SMI-ERR SW-SMI-ECAP >LEN  SW-SMI-MS >MS  RUN-ARGV-CAPTURE
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE {: o:len e:len :} SW-SMI-OUT o LEN>N SW-NL-COUNT ENDOF
     err OF PCAP-FAILED:UNMAKE {: o:len e:len c:rc :} -1 ENDOF
   ;MATCH ;
: SW-SOLO-OK? ( n -- bool ) {: limit:n :}   \ compute apps within limit AND the probe succeeded
   SW-COMPUTE-COUNT dup 0 < if drop STR-FALSE exit then
   limit >  0= ;
: SW-WAIT-SOLO ( n -- ) {: limit:n :}
   SW-SOLO-TRIES 0 ?do
      limit SW-SOLO-OK? if unloop exit then
      s" at-sweep: GPU contended (>" type limit .U s"  compute app) - waiting" type cr
   loop
   limit SW-SOLO-OK? 0= if E-AT-CONTENDED throw then ;

\ ---- (3) SM clock probe (nvidia-smi nounits -> MHz; 0 on probe failure) --------
variable SW-ACC
: SW-PARSE-MHZ ( ptr u8 n -- n ) {: a:ptr u:n :}
   0 SW-ACC !
   u 0 ?do
      a i + c@ {: ch:n :}
      ch 48 >=  ch 58 <  and if
         SW-ACC @ 10 *  ch 48 -  +  SW-ACC !
      else
         SW-ACC @ 0 > if unloop SW-ACC @ exit then    \ stop at the first non-digit after digits
      then
   loop
   SW-ACC @ ;
: SW-SM-CLK ( -- n )
   PROC-ARGV-RESET
   s" --query-gpu=clocks.sm" >LEN PROC-ARGV+
   s" --format=csv,noheader,nounits" >LEN PROC-ARGV+
   s" /usr/bin/nvidia-smi" >LEN  SW-SMI-OUT SW-SMI-CAP >LEN  SW-SMI-ERR SW-SMI-ECAP >LEN  SW-SMI-MS >MS  RUN-ARGV-CAPTURE
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE {: o:len e:len :} SW-SMI-OUT o LEN>N SW-PARSE-MHZ ENDOF
     err OF PCAP-FAILED:UNMAKE {: o:len e:len c:rc :} 0 ENDOF
   ;MATCH ;

\ ---- (3)/(4) best-of-3 over clock-stable reps --------------------------------
5 constant SW-CLK-TOL                       \ permille clock-sag tolerance for a valid sample
3 constant SW-NEED                          \ clock-stable reps required (best-of-3)
9 constant SW-REPS-CAP                      \ max attempts to collect them (fail-closed to UNSTABLE)
50 constant SW-ITERS                        \ launches per rep (BENCH-GPU-NS averages the burst)
variable SW-BEST-NS  variable SW-CLK-MIN  variable SW-CLK-MAX  variable SW-VALID  variable SW-ATT
: SW-CLK-EVID ( n -- )                      \ fold one clock reading into the min/max evidence
   dup SW-CLK-MIN @ min SW-CLK-MIN !
       SW-CLK-MAX @ max SW-CLK-MAX ! ;
: SW-BEST3 ( -- bool )                      \ min ns over SW-NEED clock-stable reps; STR-FALSE if unattainable
   $7FFFFFFFFFFFFFFF SW-BEST-NS !  $7FFFFFFF SW-CLK-MIN !  0 SW-CLK-MAX !  0 SW-VALID !  0 SW-ATT !
   begin  SW-VALID @ SW-NEED <   SW-ATT @ SW-REPS-CAP <   and  while
      SW-SM-CLK {: c0:n :}
      PTXBENCH:BENCH-GPU-NS {: ns:n :}
      SW-SM-CLK {: c1:n :}
      SW-ATT @ 1+ SW-ATT !
      c0 c1 SW-CLK-TOL AT-CLK-STABLE? if
         ns SW-BEST-NS @ min SW-BEST-NS !
         c0 SW-CLK-EVID  c1 SW-CLK-EVID
         SW-VALID @ 1+ SW-VALID !
      then
   repeat
   SW-VALID @ SW-NEED >= ;

\ ---- (4) candidate-report row ------------------------------------------------
: SW-EMIT-ROW ( n n -- ) {: id:n n:n :}
   n n * {: work:n :}
   2 n dup * n * *  SW-ITERS *  {: fl:n :}                  \ 2*n^3*iters (total flops timed)
   fl SW-BEST-NS @ PTXPROF:GFLOPS-X1000 {: val:n :}
   SB-RESET
   s" AT-SWEEP-c" SB-APPEND  id SB-INT  AT-TAB
   MMA-EXACT:MX-N @ MMA-BN @ /   AT-FLD-N
   MMA-EXACT:MX-N @ MMA-BROWS /  AT-FLD-N
   16 AT-FLD-N
   MMA-NTHREADS 16 /   AT-FLD-N
   SW-ITERS AT-FLD-N
   work AT-FLD-N
   s" GFLOPS" AT-FLD$
   val AT-FLD-N
   s" spark-gb10-sm121a" AT-FLD$
   s" 2026-07-19" AT-FLD$
   s" BN=" SB-APPEND MMA-BN @ SB-INT  s"  MF=" SB-APPEND MMA-MFRAGS @ SB-INT
   s"  W=" SB-APPEND MMA-WARPS @ SB-INT  s"  BK=" SB-APPEND MMA-BK @ SB-INT
   s"  st=" SB-APPEND MMA-STAGES @ SB-INT  s"  clk=" SB-APPEND SW-CLK-MIN @ SB-INT
   s" -" SB-APPEND SW-CLK-MAX @ SB-INT  s" MHz best-of-3 solo element-exact" SB-APPEND
   SB$ type cr ;

\ ---- one candidate: element-exact precondition -> solo -> best-of-3 -> row -----
\ one candidate; the shared CUDA context is already open (AT-SWEEP owns it). Emits
\ + loads the module for this config, gates on element-exactness, then - after a
\ per-config solo re-check (limit 1 = only this process) - times best-of-3.
: SW-CANDIDATE ( ptr a n n -- ) {: c:ptr id:n n:n :}
   c AT-APPLY
   n MMA-EXACT:MX-SHAPE-OK? 0= if
      id AT-XR-PRUNED AT-XCL-ADD
      s" -- cfg " type id .U  s"  EXCLUDED: shape " type n .U  s"  not tileable by this config" type cr
      exit
   then
   n MMA-EXACT:MX-N !
   SW-EMIT-LOAD
   PTXBENCH:LOAD
   MMA-EXACT:MX-DEV-ALLOC  MMA-EXACT:MX-PARAMS
   SW-EXACT-LAUNCH  MMA-EXACT:MX-COMPARE {: bad:n :}
   bad 0 > if
      id AT-XR-INEXACT AT-XCL-ADD
      s" -- cfg " type id .U  s"  EXCLUDED inexact (mismatches=" type bad .U  s" ) - NOT timed" type cr
   else
      1 SW-WAIT-SOLO                          \ solo (only this process) before the timing pass
      SW-ITERS PTXBENCH:ITERS!
      SW-BEST3 if
         id n SW-EMIT-ROW
      else
         id AT-XR-UNSTABLE AT-XCL-ADD
         s" -- cfg " type id .U  s"  EXCLUDED clock-unstable (no " type SW-NEED .U  s"  stable reps) - NOT reported" type cr
      then
   then
   MMA-EXACT:MX-DEV-FREE
   PTXBENCH:UNLOAD  PTXTC:CLEAN ;

\ ---- the sweep + its exclusion listing ---------------------------------------
: SW-XCL-REPORT ( -- )
   AT-XCL-N 0= if s" -- no candidates excluded" type cr exit then
   s" -- EXCLUDED candidates (never timed; id : reason):" type cr
   AT-XCL-N 0 ?do
      s"    cfg " type  i AT-XCL-ID@ .U  s"  : " type  i AT-XCL-REASON@ AT-XR$ type cr
   loop ;

\ ---- candidate staging: contiguous records indexed by arithmetic (ptr a) ------
\ Candidates are prebuilt in SW-CANDS (row = AT-CFG-N cells); SW-CAND-ROW mirrors
\ AT-WIN-ROW's create-base + offset so each row types as ptr a. The caller stages a
\ menu subset here (copy a winner, set defaults, tweak a knob), then calls AT-SWEEP.
public
32 constant SW-CANDS-CAP                     \ max staged candidates (public: the candidate count/index bound)
private
1 constant SW-CAND-GUARD                     \ canary cell past the candidate arena (overflow tripwire)
create SW-CANDS SW-CANDS-CAP AT-CFG-N * cells  SW-CAND-GUARD cells  allot
$5A5A5A5A5A5A5A5A constant SW-CAND-CANARY-VAL
public
: SW-CAND-OK? ( n -- bool )  dup 0 >=  swap SW-CANDS-CAP <  and ;        \ valid candidate INDEX 0..SW-CANDS-CAP-1
: SW-COUNT-OK? ( n -- bool )  dup 0 >=  swap SW-CANDS-CAP <=  and ;       \ valid candidate COUNT 0..SW-CANDS-CAP
: SW-CAND-CANARY-SEED ( -- )    SW-CAND-CANARY-VAL  SW-CANDS SW-CANDS-CAP AT-CFG-N * cells +  ! ;
: SW-CAND-CANARY-INTACT? ( -- bool )  SW-CANDS SW-CANDS-CAP AT-CFG-N * cells +  @ SW-CAND-CANARY-VAL = ;
\ The candidate arena holds SW-CANDS-CAP config rows (AT-CFG-N cells each). Row access is the
\ owning boundary: SW-CAND-ROW is package-private and bounds-checked, so no external caller can
\ obtain a raw base+offset pointer and stage index -1 / SW-CANDS-CAP / huge into config storage.
private
: SW-CAND-ROW ( n -- ptr a )                                    \ bounds-checked row pointer (0..SW-CANDS-CAP-1)
   dup SW-CAND-OK? 0= if E-SW-CAND throw then
   AT-CFG-N * cells SW-CANDS + ;
public
: SW-CAND-DEF ( n -- )  SW-CAND-ROW AT-DEFAULTS ;               \ row = the cg-mma baseline record
: SW-CAND-COPY ( ptr a n -- ) {: src:ptr idx:n :}              \ row idx = a copy of the config at src
   idx SW-CAND-ROW {: dst:ptr :}
   AT-CFG-N 0 ?do  src i AT-CFG@  dst i AT-CFG!  loop ;
: SW-CAND-SET ( n n n -- ) {: field:n :}                       \ ( value idx field -- ) set one knob of a staged row, bounds-checked
   SW-CAND-ROW field AT-CFG! ;

\ AT-SWEEP - the entry point. Sweeps `count` config records prestaged in SW-CANDS
\ at square shape n. Each legal config is element-exact-gated then timed solo,
\ best-of-3, at the sustained clock; illegal configs are pruned via the emitter's
\ own guards (AT-LEGAL?) and listed. Rows go to stdout as a candidate report;
\ committing them into perf-rows.tsv is the maintainer's reviewed step.
\ Reject the sweep parameters BEFORE any allocation, emission, solo-wait, or CUDA open: a bad
\ candidate count would read config rows past SW-CANDS; a square edge outside 1..MX-MAX would
\ index the MMA-EXACT host/packed buffers out of bounds (a positive tileable n>512 otherwise
\ passes the per-config shape check and then over-runs fill/ref/compare). Both die named here,
\ so a rejected sweep does zero allocation, emit, assembler, context, or launch work.
: AT-SWEEP-VALIDATE ( n n -- ) {: count:n n:n :}
   count SW-COUNT-OK? 0= if E-SW-COUNT throw then
   n MMA-EXACT:MX-EDGE-OK? 0= if MMA-EXACT:MX-E-CAP throw then ;

: AT-SWEEP ( n n -- ) {: count:n n:n :}
   count n AT-SWEEP-VALIDATE                   \ validate count + edge before any side effect
   MMA-EXACT:MX-BUF-INIT
   AT-XCL-RESET
   s" # AT-SWEEP candidate report - perf-rows.tsv 12-field layout; NOT written to perf-rows.tsv (review before committing)" type cr
   S\" # kernel\tgrid\tgridy\tblock\tblocky\titers\twork\tmetric\tvalue_x1000\tdevice\tdate\tnote" type cr
   0 SW-WAIT-SOLO                              \ start solo: the GPU must be idle (no compute apps) before we open a context
   PTXBENCH:OPEN
   count 0 ?do
      i SW-CAND-ROW {: c:ptr :}
      c AT-LEGAL? if
         c i n SW-CANDIDATE
      else
         i AT-XR-PRUNED AT-XCL-ADD
         s" -- cfg " type i .U  s"  EXCLUDED pruned (AT-LEGAL? false) - NOT emitted" type cr
      then
   loop
   PTXBENCH:CLOSE
   SW-XCL-REPORT ;

\ ---- smoke: 3 configs at 512 (2 legal+timed, 1 illegal+pruned) ---------------
: AT-SWEEP-SMOKE ( -- )
   CUDA:OPEN? 0= if s" at-sweep: libcuda unavailable -> SKIPPED (off-device)" type cr exit then
   512 512 512 0 AT-SELECT 0 SW-CAND-COPY                  \ row0 = tf32 512 committed winner (legal -> exact -> timed)
   1 SW-CAND-DEF                                           \ row1 = tf32 8-warp baseline (legal -> exact -> timed)
   2 SW-CAND-DEF  16 2 AT-WARPS SW-CAND-SET               \ row2 = defaults + W=16 -> E-MMA-WARPS (pruned)
   3 512 AT-SWEEP ;

;package
