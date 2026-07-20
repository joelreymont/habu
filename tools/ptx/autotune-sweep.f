\ autotune-sweep.f - (3) STOPWATCH: the GB sweep harness under gate discipline.
\
\ dot habu-committed-autotune-menu-4321e05d item (3). Where Triton autotunes at
\ first call with a handful of noisy warmup reps, Habu sweeps OFFLINE under a
\ discipline strictly stronger than that: for each candidate config the harness
\   1. ELEMENT-EXACT PRECONDITION - runs the integer-fill element-exact check at
\      the sweep shape BEFORE timing; a config that is not bit-exact is EXCLUDED
\      from timing with a recorded reason (listed, never silently skipped).
\   2. DEVICE EXCLUSIVITY (PID census, NOT a count) - censuses the EXACT set of
\      compute PIDs on the bound device immediately BEFORE and AFTER every timed
\      burst and requires it to be exactly {our PID}; any foreign PID, missing self,
\      changed set, or failed/malformed probe fails closed (never a threshold, never
\      inferring absence-of-contention from a steady clock). No unprivileged exclusive-
\      process LEASE is obtainable here (needs root); the census DETECTS rather than
\      PREVENTS, and the compute mode is recorded per row (see the (2) header below).
\   3. SUSTAINED-CLOCK VERIFICATION - reads the SM clock (bounded exact parse, so the
\      stability compare cannot overflow) before/after every timed burst and rejects/
\      reruns a sample whose clock sagged past a stated tolerance; evidence recorded per row.
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
-7317 constant E-SW-PROBE      \ (2) an nvidia-smi census/clock/device probe failed or returned malformed evidence (infra - fail closed, NEVER relabeled as contention)

\ census - the typed verdict of ONE compute-PID census on the bound device. The
\ dot's core fix: ownership is a SET IDENTITY, not a count. `exclusive` carries the
\ owner PIDs observed (0 pre-open, 1 once our context holds the device); `contended`
\ carries (foreign-count, total-count) - a foreign PID present, our PID absent, or a
\ changed set; `probe-failed` carries (diag-code, child-rc) - a missing / erroring /
\ malformed nvidia-smi probe. The checker forces every consumer to MATCH all three,
\ so an INFRASTRUCTURE failure can never be silently read as contention (the old -1
\ sentinel's exact bug) and a count can never masquerade as ownership.
SUMTYPE census 0
  VARIANT exclusive    n ;VARIANT
  VARIANT contended    n n ;VARIANT
  VARIANT probe-failed n n ;VARIANT
;SUMTYPE

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

\ ---- (2) DEVICE EXCLUSIVITY: a PID census, never a count ----------------------
\ The dot's measurement-integrity fix. A COUNT of compute apps proves NOTHING about
\ ownership: one unrelated process satisfies "<= 1" while THIS context is absent from
\ the snapshot, and a competitor can start-and-finish between two samples. So we
\ census the EXACT set of compute PIDs on the BOUND device (nvidia-smi -i 0
\ --query-compute-apps=pid,gpu_uuid) and require it to equal exactly {our PID} for
\ EVERY timed burst - BEFORE and AFTER it - failing closed on any foreign PID, any
\ missing self, any changed set, or any malformed / failed probe. Ownership is a SET
\ IDENTITY, not a threshold; a clock that held is NOT evidence of no contention (it
\ is only a throttle guard, secondary to the census).
\
\ LEASE BOUNDARY (honest scope, dot SCOPE HONESTY): a truly ENFORCED exclusive lease
\ needs EXCLUSIVE_PROCESS compute mode (`nvidia-smi -c EXCLUSIVE_PROCESS`), which
\ requires root - and this box has NO passwordless sudo (user-local only). So an
\ unprivileged process CANNOT acquire a system-enforced lease; the honest MAXIMUM is
\ this census, which DETECTS contention rather than PREVENTING it. We read and RECORD
\ the compute mode (SW-DEV-MODE): under the default mode the row records detection-only;
\ if an operator set EXCLUSIVE_PROCESS out-of-band the row records that a real lease was
\ in force. The privileged lease is left as an ops-time decision, NOT faked here. Under
\ the detection model "lease collision / stale owner" reduces to the contended case (a
\ foreign PID in the census) - there is no privileged lease object to collide on.
\
\ Every probe is a plain nvidia-smi spawn OUTSIDE the CUDA-event-timed burst, so
\ census / clock overhead never lands in a reported ns (BENCH-GPU-NS times only the
\ launch loop). INFRASTRUCTURE failure (nvidia-smi missing, nonzero exit, timeout,
\ malformed output) is a distinct probe-failed outcome that propagates IMMEDIATELY
\ (E-SW-PROBE / a thrown E-PROC-TIMEOUT from the capture), never retried, never
\ relabeled as contention.
private
2000 constant SW-SMI-MS                      \ per nvidia-smi call timeout
40 constant SW-SOLO-TRIES                    \ bounded pre-open idle-wait retries
$400 constant SW-SMI-CAP    $400 constant SW-SMI-ECAP
$80 constant SW-DEV-CAP                      \ device uuid / compute-mode field buffer
public
64 constant SW-PIDS-CAP                      \ max compute PIDs one census tracks (public: the census fixture asserts the cap)
private
100000 constant SW-MHZ-CAP                   \ MHz ceiling: bounds the clock parse -> overflow-free stability compare
1073741824 constant SW-PID-CAP               \ PID ceiling (2^30): bounds the pid parse -> no accumulate overflow
create SW-SMI-OUT SW-SMI-CAP allot   create SW-SMI-ERR SW-SMI-ECAP allot
create SW-DEV-UUID SW-DEV-CAP allot  variable SW-DEV-UUID-U
create SW-DEV-MODE SW-DEV-CAP allot  variable SW-DEV-MODE-U
create SW-PIDS SW-PIDS-CAP cells allot   variable SW-PIDS-N
variable SW-OWN-PID                           \ our PID: the SOLE compute PID every timed burst must show
variable SW-ACC   variable SW-SI              \ shared parse accumulator + scan cursor
variable SW-US    variable SW-UE              \ current field [start,end) during a scan
variable SW-FGN                               \ foreign-PID counter during a verdict
public
\ probe-failed diagnostic codes (the census probe-failed payload; asserted by the fixtures)
1 constant SW-PF-EXIT       \ nvidia-smi completed nonzero (or was SIGKILL-reaped)
2 constant SW-PF-BADPID     \ a pid field is empty, non-numeric, or overflows SW-PID-CAP
3 constant SW-PF-BADROW     \ a row lacks the "pid, uuid" shape (truncated / malformed)
4 constant SW-PF-DUPPID     \ the same pid appears twice (anomalous - fail closed)
5 constant SW-PF-DEVMISS    \ a row's gpu_uuid != the bound device (device mismatch)
6 constant SW-PF-CAP        \ more compute pids than SW-PIDS-CAP (fail closed)
7 constant SW-PF-BADCLK     \ a clock reading is empty, non-numeric, or overflows SW-MHZ-CAP
private
\ ---- bounded byte helpers + the compute-PID set --------------------------------
: SW-DIGIT? ( n -- bool )  dup 48 >=  swap 58 <  and ;
: SW-WS? ( n -- bool )  dup 10 =  swap dup 13 =  swap 32 =  or  or ;      \ LF / CR / space
: SW-STORE-FIELD ( ptr u8 n n ptr u8 n -- n ) {: a:ptr s:n e:n dst:ptr cap:n :}   \ copy [s,e) into dst (capped), return len
   e s - cap min {: len:n :}
   0 begin dup len < while
      dup a s + + c@  over dst + c!
      1+
   repeat drop
   len ;
: SW-PIDS-RESET ( -- )  0 SW-PIDS-N ! ;
: SW-PIDS-HAS? ( n -- bool ) {: p:n :}
   0 begin dup SW-PIDS-N @ < while
      dup cells SW-PIDS + @ p = if drop STR-TRUE exit then
      1+
   repeat drop STR-FALSE ;

\ SW-CENSUS-SCAN - fill SW-PIDS from the compute-apps output; 0 ok, else a SW-PF-* code.
\ Each record is "<pid>, <uuid>\n"; a device-mismatched uuid, an unparsable / overflowing
\ pid, a missing separator, a duplicate pid, or an over-cap set all fail closed. Bounded
\ and exact - no arbitrary digit run, no accumulate overflow.
: SW-CENSUS-SCAN ( ptr u8 n -- n ) {: a:ptr u:n :}
   SW-PIDS-RESET
   0 SW-SI !
   begin SW-SI @ u < while
      a SW-SI @ + c@ 10 = if
         SW-SI @ 1+ SW-SI !                                    \ blank line / stray newline
      else
         a SW-SI @ + c@ SW-DIGIT? 0= if SW-PF-BADROW exit then
         0 SW-ACC !                                            \ --- bounded exact pid ---
         begin SW-SI @ u < if a SW-SI @ + c@ SW-DIGIT? else STR-FALSE then while
            SW-ACC @ 10 *  a SW-SI @ + c@ 48 -  +  SW-ACC !
            SW-ACC @ SW-PID-CAP > if SW-PF-BADPID exit then
            SW-SI @ 1+ SW-SI !
         repeat
         SW-SI @ u >= if SW-PF-BADROW exit then                \ separator required
         a SW-SI @ + c@ 44 <> if SW-PF-BADROW exit then        \ expect ','
         SW-SI @ 1+ SW-SI !
         begin SW-SI @ u < if a SW-SI @ + c@ 32 = else STR-FALSE then while
            SW-SI @ 1+ SW-SI !                                 \ skip spaces after ','
         repeat
         SW-SI @ SW-US !                                       \ uuid start
         begin SW-SI @ u < if a SW-SI @ + c@ 10 <> else STR-FALSE then while
            SW-SI @ 1+ SW-SI !
         repeat
         SW-SI @ SW-UE !                                       \ uuid end (before newline / EOF); trim trailing ws
         begin SW-UE @ SW-US @ > if a SW-UE @ 1- + c@ SW-WS? else STR-FALSE then while
            SW-UE @ 1- SW-UE !
         repeat
         SW-UE @ SW-US @ <= if SW-PF-BADROW exit then          \ empty uuid
         a SW-US @ +  SW-UE @ SW-US @ -  SW-DEV-UUID SW-DEV-UUID-U @  STR= 0= if SW-PF-DEVMISS exit then
         SW-ACC @ SW-PIDS-HAS? if SW-PF-DUPPID exit then       \ duplicate pid
         SW-PIDS-N @ SW-PIDS-CAP >= if SW-PF-CAP exit then     \ over cap
         SW-ACC @ SW-PIDS-N @ cells SW-PIDS + !
         SW-PIDS-N @ 1+ SW-PIDS-N !
      then
   repeat
   0 ;

\ SW-CENSUS-VERDICT - map the scanned SW-PIDS set to a verdict against the expected owner.
\ owner < 0 -> expect EMPTY (pre-open idle); owner >= 0 -> expect exactly {owner}. Exclusive
\ at BOTH burst endpoints => the set was identically {owner} at both, so churn that surfaces
\ at either endpoint is rejected (churn wholly inside a burst is unobservable by sampling -
\ the same boundary as clock sampling, documented).
: SW-CENSUS-VERDICT ( n -- census ) {: owner:n :}
   SW-PIDS-N @ {: tot:n :}
   owner 0 < if
      tot 0= if 0 CENSUS:EXCLUSIVE else tot tot CENSUS:CONTENDED then exit
   then
   0 SW-FGN !
   0 begin dup tot < while
      dup cells SW-PIDS + @ owner <> if SW-FGN @ 1+ SW-FGN ! then
      1+
   repeat drop
   tot 1 =  owner SW-PIDS-HAS?  and  SW-FGN @ 0=  and if
      1 CENSUS:EXCLUSIVE
   else
      SW-FGN @ tot CENSUS:CONTENDED
   then ;
public
\ ---- the PURE parsers (unit-tested off-device with injected nvidia-smi bytes) --
\ SW-CENSUS-PARSE turns raw compute-apps bytes + an expected owner into a verdict; it
\ opens no device and is the surface the fixture matrix drives. SW-PARSE-CLK is the
\ bounded-exact clock parser (rejects the overflowing digit run that let a max-cell
\ reading wrap AT-CLK-STABLE?'s compare - the value is refused HERE, so a poison clock
\ can never reach the classifier). SW-CENSUS-OF wraps a capture result into a census
\ (its err arm is probe-failed, never contention). SW-CENSUS-GATE is the fail-closed
\ throw point. SW-DEV-UUID-SET binds the device uuid the census matches rows against.
: SW-CENSUS-PARSE ( ptr u8 n n -- census ) {: owner:n :}
   SW-CENSUS-SCAN dup 0 <> if 0 CENSUS:PROBE-FAILED exit then drop
   owner SW-CENSUS-VERDICT ;
: SW-PARSE-CLK ( ptr u8 n -- result<n,n> ) {: a:ptr u:n :}    \ ok = bounded MHz; err = SW-PF-BADCLK
   u 0= if SW-PF-BADCLK RESULT:ERR exit then
   a c@ SW-DIGIT? 0= if SW-PF-BADCLK RESULT:ERR exit then
   0 SW-ACC !  0 SW-SI !
   begin SW-SI @ u < if a SW-SI @ + c@ SW-DIGIT? else STR-FALSE then while
      SW-ACC @ 10 *  a SW-SI @ + c@ 48 -  +  SW-ACC !
      SW-ACC @ SW-MHZ-CAP > if SW-PF-BADCLK RESULT:ERR exit then
      SW-SI @ 1+ SW-SI !
   repeat
   SW-ACC @ RESULT:OK ;
: SW-CENSUS-OF ( result<pcap:captured,pcap:failed> n -- census ) {: owner:n :}
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE {: o:len e:len :} SW-SMI-OUT o LEN>N owner SW-CENSUS-PARSE ENDOF
     err OF PCAP-FAILED:UNMAKE {: o:len e:len c:rc :} SW-PF-EXIT c RC>N CENSUS:PROBE-FAILED ENDOF
   ;MATCH ;
: SW-CENSUS-GATE ( census -- )                                \ exclusive -> ok; else fail closed
   MATCH census
     exclusive    OF drop ENDOF
     contended    OF {: fgn:n tot:n :}
        s" -- at-sweep: EXCLUSIVITY LOST - " type fgn FMT:.U s"  foreign of " type tot FMT:.U
        s"  compute PID(s) on the bound device (fail closed; NO row)" type cr
        E-AT-CONTENDED throw ENDOF
     probe-failed OF {: diag:n rc:n :}
        s" -- at-sweep: census PROBE FAILED code=" type diag FMT:.U s"  rc=" type rc FMT:.U
        s"  (infrastructure failure - propagated, NOT relabeled as contention)" type cr
        E-SW-PROBE throw ENDOF
   ;MATCH ;
: SW-DEV-UUID-SET ( ptr u8 n -- ) {: a:ptr u:n :}             \ bind the device uuid the census matches against
   a 0 u SW-DEV-UUID SW-DEV-CAP SW-STORE-FIELD SW-DEV-UUID-U ! ;
: SW-DEV-UUID$ ( -- ptr u8 n )  SW-DEV-UUID SW-DEV-UUID-U @ ;  \ bound device identity (recorded per row)
: SW-DEV-MODE$ ( -- ptr u8 n )  SW-DEV-MODE SW-DEV-MODE-U @ ;  \ bound device compute mode (recorded per row)
: SW-PARSE-DEVID ( ptr u8 n -- ) {: a:ptr u:n :}             \ "uuid, compute_mode" -> SW-DEV-UUID / SW-DEV-MODE; malformed -> E-SW-PROBE
   0 SW-SI !
   begin SW-SI @ u < if a SW-SI @ + c@ 44 <> else STR-FALSE then while
      SW-SI @ 1+ SW-SI !
   repeat
   SW-SI @ u >= if E-SW-PROBE throw then                      \ no comma -> malformed devid
   SW-SI @ {: cpos:n :}
   cpos SW-UE !                                               \ uuid = [0,cpos) trimmed
   begin SW-UE @ 0 > if a SW-UE @ 1- + c@ SW-WS? else STR-FALSE then while
      SW-UE @ 1- SW-UE !
   repeat
   SW-UE @ 0= if E-SW-PROBE throw then                        \ empty uuid
   a 0 SW-UE @ SW-DEV-UUID SW-DEV-CAP SW-STORE-FIELD SW-DEV-UUID-U !
   cpos 1+ SW-US !                                            \ mode = [cpos+1,u) leading-space-skipped, trailing-ws-trimmed
   begin SW-US @ u < if a SW-US @ + c@ 32 = else STR-FALSE then while
      SW-US @ 1+ SW-US !
   repeat
   u SW-UE !
   begin SW-UE @ SW-US @ > if a SW-UE @ 1- + c@ SW-WS? else STR-FALSE then while
      SW-UE @ 1- SW-UE !
   repeat
   a SW-US @ SW-UE @ SW-DEV-MODE SW-DEV-CAP SW-STORE-FIELD SW-DEV-MODE-U ! ;
private
\ ---- device probes (real nvidia-smi; NOT reached off-device) -------------------
: SW-APPS-PROBE ( -- result<pcap:captured,pcap:failed> )     \ device-0 compute PID census
   PROC-ARGV-RESET
   s" -i" >LEN PROC-ARGV+   s" 0" >LEN PROC-ARGV+
   s" --query-compute-apps=pid,gpu_uuid" >LEN PROC-ARGV+
   s" --format=csv,noheader" >LEN PROC-ARGV+
   s" /usr/bin/nvidia-smi" >LEN  SW-SMI-OUT SW-SMI-CAP >LEN  SW-SMI-ERR SW-SMI-ECAP >LEN  SW-SMI-MS >MS  RUN-ARGV-CAPTURE ;
: SW-CLK-PROBE ( -- result<pcap:captured,pcap:failed> )      \ device-0 SM clock (MHz, nounits)
   PROC-ARGV-RESET
   s" -i" >LEN PROC-ARGV+   s" 0" >LEN PROC-ARGV+
   s" --query-gpu=clocks.sm" >LEN PROC-ARGV+
   s" --format=csv,noheader,nounits" >LEN PROC-ARGV+
   s" /usr/bin/nvidia-smi" >LEN  SW-SMI-OUT SW-SMI-CAP >LEN  SW-SMI-ERR SW-SMI-ECAP >LEN  SW-SMI-MS >MS  RUN-ARGV-CAPTURE ;
: SW-DEVID-PROBE ( -- result<pcap:captured,pcap:failed> )    \ device-0 identity + compute mode
   PROC-ARGV-RESET
   s" -i" >LEN PROC-ARGV+   s" 0" >LEN PROC-ARGV+
   s" --query-gpu=uuid,compute_mode" >LEN PROC-ARGV+
   s" --format=csv,noheader" >LEN PROC-ARGV+
   s" /usr/bin/nvidia-smi" >LEN  SW-SMI-OUT SW-SMI-CAP >LEN  SW-SMI-ERR SW-SMI-ECAP >LEN  SW-SMI-MS >MS  RUN-ARGV-CAPTURE ;
: SW-CLK-VAL ( result<n,n> -- n )                            \ parsed clock, or propagate as E-SW-PROBE
   MATCH result  ok OF ENDOF  err OF drop E-SW-PROBE throw ENDOF  ;MATCH ;
: SW-SM-CLK ( -- n )                                         \ bounded SM MHz; probe / parse failure propagates - never a silent 0
   SW-CLK-PROBE MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE {: o:len e:len :} SW-SMI-OUT o LEN>N SW-PARSE-CLK SW-CLK-VAL ENDOF
     err OF PCAP-FAILED:UNMAKE {: o:len e:len c:rc :} E-SW-PROBE throw ENDOF
   ;MATCH ;
: SW-CENSUS ( n -- census ) {: owner:n :}  SW-APPS-PROBE owner SW-CENSUS-OF ;
: SW-REQUIRE-EXCLUSIVE ( n -- )  SW-CENSUS SW-CENSUS-GATE ;   \ probe + gate; throws unless the set is exactly {owner}
variable SW-IDLE?   variable SW-TRY
: SW-CENSUS-IDLE-STEP ( census -- )
   MATCH census
     exclusive    OF drop STR-TRUE SW-IDLE? ! ENDOF
     contended    OF 2drop s" at-sweep: device busy (foreign compute PID) - waiting for idle" type cr ENDOF
     probe-failed OF {: diag:n rc:n :}
        s" -- at-sweep: idle-probe FAILED code=" type diag FMT:.U s"  (infra - propagated)" type cr
        E-SW-PROBE throw ENDOF
   ;MATCH ;
: SW-WAIT-IDLE ( -- )                                        \ pre-open: bounded wait for an EMPTY device; probe-failure propagates now
   STR-FALSE SW-IDLE? !  0 SW-TRY !
   begin SW-IDLE? @ 0=  SW-TRY @ SW-SOLO-TRIES <  and while
      -1 SW-CENSUS SW-CENSUS-IDLE-STEP
      SW-TRY @ 1+ SW-TRY !
   repeat
   SW-IDLE? @ 0= if E-AT-CONTENDED throw then ;
: SW-DEV-BIND ( -- )                                         \ bind device 0 identity + record compute mode (no CUDA)
   SW-DEVID-PROBE MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE {: o:len e:len :} SW-SMI-OUT o LEN>N SW-PARSE-DEVID ENDOF
     err OF PCAP-FAILED:UNMAKE {: o:len e:len c:rc :} E-SW-PROBE throw ENDOF
   ;MATCH
   s" -- at-sweep: bound device 0 uuid=" type SW-DEV-UUID SW-DEV-UUID-U @ type
   s"  compute_mode=" type SW-DEV-MODE SW-DEV-MODE-U @ type
   SW-DEV-MODE SW-DEV-MODE-U @ s" Default" STR= if
      s"  [DETECTION-ONLY: no privileged EXCLUSIVE_PROCESS lease; census detects contention]" type
   else
      s"  [ENFORCED exclusive lease in force]" type
   then cr ;

\ ---- (3)/(4) best-of-3 over clock-stable reps, each burst bracketed by a census
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
      SW-OWN-PID @ SW-REQUIRE-EXCLUSIVE          \ census BEFORE the burst (outside the CUDA-event-timed interval)
      SW-SM-CLK {: c0:n :}
      PTXBENCH:BENCH-GPU-NS {: ns:n :}
      SW-SM-CLK {: c1:n :}
      SW-OWN-PID @ SW-REQUIRE-EXCLUSIVE          \ census AFTER the burst; a foreign PID either endpoint aborts (fail closed)
      SW-ATT @ 1+ SW-ATT !
      c0 c1 SW-CLK-TOL AT-CLK-STABLE? if         \ secondary throttle guard, on an already-exclusive sample
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
   s" AT-SWEEP-c" SB-APPEND  id FMT:SB-INT  AT-TAB
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
   s" BN=" SB-APPEND MMA-BN @ FMT:SB-INT  s"  MF=" SB-APPEND MMA-MFRAGS @ FMT:SB-INT
   s"  W=" SB-APPEND MMA-WARPS @ FMT:SB-INT  s"  BK=" SB-APPEND MMA-BK @ FMT:SB-INT
   s"  st=" SB-APPEND MMA-STAGES @ FMT:SB-INT  s"  clk=" SB-APPEND SW-CLK-MIN @ FMT:SB-INT
   s" -" SB-APPEND SW-CLK-MAX @ FMT:SB-INT  s" MHz best-of-3 element-exact" SB-APPEND
   s"  exclusivity[dev=" SB-APPEND SW-DEV-UUID SW-DEV-UUID-U @ SB-APPEND       \ (2) per-row exclusivity evidence
   s"  mode=" SB-APPEND SW-DEV-MODE SW-DEV-MODE-U @ SB-APPEND
   s"  owner-pid=" SB-APPEND SW-OWN-PID @ FMT:SB-INT  s"  census=sole-owner(before+after every burst)" SB-APPEND
   s"  ns=" SB-APPEND SW-BEST-NS @ FMT:SB-INT  s" ]" SB-APPEND
   SB$ type cr ;

\ ---- one candidate: element-exact precondition -> best-of-3 (per-burst census) -> row
\ one candidate; the shared CUDA context is already open (AT-SWEEP owns it). Emits
\ + loads the module for this config, gates on element-exactness, then times best-of-3.
\ Exclusivity is proven per burst INSIDE SW-BEST3 (a PID census immediately before and
\ after each timed burst, requiring the device's compute-PID set to be exactly {our PID}),
\ so no separate pre-timing solo-wait is needed - a foreign PID aborts the sweep fail-closed.
: SW-CANDIDATE ( ptr a n n -- ) {: c:ptr id:n n:n :}
   c AT-APPLY
   n MMA-EXACT:MX-SHAPE-OK? 0= if
      id AT-XR-PRUNED AT-XCL-ADD
      s" -- cfg " type id FMT:.U  s"  EXCLUDED: shape " type n FMT:.U  s"  not tileable by this config" type cr
      exit
   then
   n MMA-EXACT:MX-N !
   SW-EMIT-LOAD
   PTXBENCH:LOAD
   MMA-EXACT:MX-DEV-ALLOC  MMA-EXACT:MX-PARAMS
   SW-EXACT-LAUNCH  MMA-EXACT:MX-COMPARE {: bad:n :}
   bad 0 > if
      id AT-XR-INEXACT AT-XCL-ADD
      s" -- cfg " type id FMT:.U  s"  EXCLUDED inexact (mismatches=" type bad FMT:.U  s" ) - NOT timed" type cr
   else
      SW-ITERS PTXBENCH:ITERS!
      SW-BEST3 if
         id n SW-EMIT-ROW
      else
         id AT-XR-UNSTABLE AT-XCL-ADD
         s" -- cfg " type id FMT:.U  s"  EXCLUDED clock-unstable (no " type SW-NEED FMT:.U  s"  stable reps) - NOT reported" type cr
      then
   then
   MMA-EXACT:MX-DEV-FREE
   PTXBENCH:UNLOAD  PTXTC:CLEAN ;

\ ---- the sweep + its exclusion listing ---------------------------------------
: SW-XCL-REPORT ( -- )
   AT-XCL-N 0= if s" -- no candidates excluded" type cr exit then
   s" -- EXCLUDED candidates (never timed; id : reason):" type cr
   AT-XCL-N 0 ?do
      s"    cfg " type  i AT-XCL-ID@ FMT:.U  s"  : " type  i AT-XCL-REASON@ AT-XR$ type cr
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
   SW-DEV-BIND                                 \ bind device 0 identity + record compute mode (no CUDA yet)
   SW-WAIT-IDLE                                \ start idle: an EMPTY PID census (no foreign compute app) before we open a context
   PTXBENCH:OPEN
   getpid SW-OWN-PID !                         \ our PID: the SOLE compute PID every timed burst's census must show
   count 0 ?do
      i SW-CAND-ROW {: c:ptr :}
      c AT-LEGAL? if
         c i n SW-CANDIDATE
      else
         i AT-XR-PRUNED AT-XCL-ADD
         s" -- cfg " type i FMT:.U  s"  EXCLUDED pruned (AT-LEGAL? false) - NOT emitted" type cr
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
