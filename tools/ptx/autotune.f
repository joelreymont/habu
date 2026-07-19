\ autotune.f - committed autotune: the MENU (config record over the cg-mma knobs),
\ the PRUNE (mechanical legality via the emitter's own fail-closed guards), and the
\ WINNERS table (committed per-(shape-class,dtype) configs) + a plan-time SELECTOR.
\ dot habu-committed-autotune-menu-4321e05d, parity-plan phase 4 items (1)(2)(4).
\
\ Habu's answer to @triton.autotune, each mechanic upgraded to Habu discipline:
\   (1) MENU   - the candidate space is DATA, not decorator args: a fixed config
\                record over cg-mma.f's emit-time knobs (warps, MFRAGS, BK, pad,
\                stages, dyn, epilogue, dtype, LMODE/B-feed, BN, GROUP, SPLITK,
\                BLDM, BPAD), plus the per-axis candidate value lists.
\   (2) PRUNE  - the legal set is the menu run against the emitter's EXISTING
\                fail-closed guards (cg-mma.f MMA-CHECK-*). We do NOT weaken or
\                duplicate a guard - AT-CHECK CALLS them, in EMIT (MMA-BODY) order.
\   (4) WINNERS- a committed table keyed (shape-class, dtype), reviewed data next
\                to tools/ptx/perf-rows.tsv, read by AT-SELECT at PLAN time. Exact
\                class -> its config; unknown shape -> nearest committed class by a
\                geometric (M*N*K product) bucketing rule; a genuinely new class
\                outside all buckets -> E-AT-NEWCLASS (never a silent default or a
\                runtime bench). Every row cites its perf-rows.tsv source.
\
\ This is the HOST-side planning surface, so it lives in tools/ptx (beside
\ perf-registry.f / perf-rows.tsv) and not in the lib/ptx device-emit library:
\ it reads committed measurements and picks knobs; it emits no PTX itself. The
\ selector consulting the committed table is what gives Habu deterministic cold
\ start (no first-call runtime autotune stall) - a headline vs Triton.

require lib/errors.f
require lib/ptx/cg-mma.f

-7310 constant E-AT-NEWCLASS   \ shape is a genuinely new class outside every committed bucket (needs an OFFLINE sweep + review)
-7311 constant E-AT-DTYPE      \ tensor dtype key is not one of tf32(0)/fp16(1)/bf16(2)
-7312 constant E-AT-CONTENDED  \ (3) STOPWATCH: the GPU never freed within the solo-wait timeout - never time under contention
-7314 constant E-AT-XCLFULL    \ (3) STOPWATCH: more excluded candidates than the exclusion table holds (sweep misconfigured)

package AUTOTUNE
public

\ ============ (1) MENU - the config record over the cg-mma knobs ==============
\ A config is a flat 15-cell record; the field order is fixed and shared by the
\ winners table's config sub-record. DATA layout only - no logic here.
15 constant AT-CFG-N          \ cells per config record
0  constant AT-WARPS          \ warps/block (4 or 8)
1  constant AT-MFRAGS         \ M-fragments per warp (1/2/4)
2  constant AT-BK             \ staged K-tile depth
3  constant AT-PAD            \ As row pad floats
4  constant AT-STAGES         \ cp.async pipeline buffers (1/2/3)
5  constant AT-DYN            \ 1 = dynamic .shared
6  constant AT-EPILOG         \ 1 = smem coalesced C epilogue
7  constant AT-DTYPE          \ 0 tf32 / 1 fp16 / 2 bf16  (ALSO the winners table's dtype key)
8  constant AT-LMODE          \ fragment-load mode (0 scalar+cvt / 1 raw / 2 ldmatrix)
9  constant AT-BN             \ output-tile width (64/128/256)
10 constant AT-GROUP          \ grouped-raster group height (0 = off)
11 constant AT-SPLITK         \ split-K count (1 = off)
12 constant AT-BLDM           \ tf32 transposed-Bs B-ldmatrix (wide path)
13 constant AT-BTF16          \ fp16/bf16 transposed-Bs B feed
14 constant AT-BPAD           \ transposed-Bs (BT) row pad

public
: AT-CFG@ ( ptr a n -- n )  cells + @ ;
: AT-CFG! ( n ptr a n -- )  cells + ! ;

\ the byte-identical cg-mma baseline (8-warp tf32 BN=64 MFRAGS=1) as a config record
: AT-DEFAULTS ( ptr a -- ) {: c:ptr :}
   8 c AT-WARPS AT-CFG!   1 c AT-MFRAGS AT-CFG!  32 c AT-BK AT-CFG!   0 c AT-PAD AT-CFG!
   2 c AT-STAGES AT-CFG!  0 c AT-DYN AT-CFG!      0 c AT-EPILOG AT-CFG! 0 c AT-DTYPE AT-CFG!
   0 c AT-LMODE AT-CFG!  64 c AT-BN AT-CFG!       0 c AT-GROUP AT-CFG!  1 c AT-SPLITK AT-CFG!
   0 c AT-BLDM AT-CFG!    0 c AT-BTF16 AT-CFG!    0 c AT-BPAD AT-CFG! ;

\ per-axis candidate value lists (the MENU's search space, as DATA). These are the
\ documented axes an offline sweep draws from; PRUNE below filters any config drawn
\ from them against the guards. Length-prefixed cell arrays.
private
create AT-AXW  2 ,  4 , 8 ,
create AT-AXM  3 ,  1 , 2 , 4 ,
create AT-AXK  3 , 16 , 32 , 64 ,
create AT-AXS  3 ,  1 , 2 , 3 ,
create AT-AXN  3 , 64 , 128 , 256 ,
create AT-AXD  3 ,  0 , 1 , 2 ,
public
: AT-AXIS-N ( ptr a -- n )  @ ;
: AT-AXIS@ ( ptr a n -- n ) {: ax:ptr i:n :}  i 1+ cells ax + @ ;
: AT-AX-WARPS  ( -- ptr a )  AT-AXW ;
: AT-AX-MFRAGS ( -- ptr a )  AT-AXM ;
: AT-AX-BK     ( -- ptr a )  AT-AXK ;
: AT-AX-STAGES ( -- ptr a )  AT-AXS ;
: AT-AX-BN     ( -- ptr a )  AT-AXN ;
: AT-AX-DTYPE  ( -- ptr a )  AT-AXD ;

\ ============ (2) PRUNE - legality via the emitter's own fail-closed guards ====
\ AT-APPLY maps a config record onto cg-mma's live knobs; AT-CHECK then CALLS the
\ emitter's guards in MMA-BODY order. AT-SAVE/AT-RESTORE bracket the probe so it
\ never leaves the emitter's knob state mutated. We add no legality logic of our
\ own: a config is legal here iff it is legal for the emitter to emit.
private
create AT-SNAP AT-CFG-N cells allot

public
: AT-APPLY ( ptr a -- ) {: c:ptr :}                   \ set every cg-mma knob from a config record
   c AT-WARPS  AT-CFG@ MMA-WARPS !     c AT-MFRAGS AT-CFG@ MMA-MFRAGS !
   c AT-BK     AT-CFG@ MMA-BK !        c AT-PAD    AT-CFG@ MMA-PAD !
   c AT-STAGES AT-CFG@ MMA-STAGES !    c AT-DYN    AT-CFG@ MMA-DYNSMEM !
   c AT-EPILOG AT-CFG@ MMA-EPILOG !    c AT-DTYPE  AT-CFG@ MMA-DTYPE !
   c AT-LMODE  AT-CFG@ MMA-LMODE !     c AT-BN     AT-CFG@ MMA-BN !
   c AT-GROUP  AT-CFG@ MMA-GROUP !     c AT-SPLITK AT-CFG@ MMA-SPLITK !
   c AT-BLDM   AT-CFG@ MMA-BLDM !      c AT-BTF16  AT-CFG@ MMA-BTF16 !
   c AT-BPAD   AT-CFG@ MMA-BPAD ! ;

private
: AT-SAVE ( -- )                                      \ snapshot the live cg-mma knobs into AT-SNAP
   MMA-WARPS @  AT-SNAP AT-WARPS  AT-CFG!   MMA-MFRAGS @ AT-SNAP AT-MFRAGS AT-CFG!
   MMA-BK @     AT-SNAP AT-BK     AT-CFG!   MMA-PAD @    AT-SNAP AT-PAD    AT-CFG!
   MMA-STAGES @ AT-SNAP AT-STAGES AT-CFG!   MMA-DYNSMEM @ AT-SNAP AT-DYN   AT-CFG!
   MMA-EPILOG @ AT-SNAP AT-EPILOG AT-CFG!   MMA-DTYPE @  AT-SNAP AT-DTYPE  AT-CFG!
   MMA-LMODE @  AT-SNAP AT-LMODE  AT-CFG!   MMA-BN @     AT-SNAP AT-BN     AT-CFG!
   MMA-GROUP @  AT-SNAP AT-GROUP  AT-CFG!   MMA-SPLITK @ AT-SNAP AT-SPLITK AT-CFG!
   MMA-BLDM @   AT-SNAP AT-BLDM   AT-CFG!   MMA-BTF16 @  AT-SNAP AT-BTF16  AT-CFG!
   MMA-BPAD @   AT-SNAP AT-BPAD   AT-CFG! ;
: AT-RESTORE ( -- )  AT-SNAP AT-APPLY ;               \ restore the snapshotted knobs

public
\ CALL the emitter's guards, in MMA-BODY order. Throws the config's specific E-MMA-*
\ on the first illegal knob; returns cleanly when the config is legal to emit.
: AT-CHECK ( -- )
   MMA-CHECK-BN  MMA-CHECK-DTYPE  MMA-CHECK-BTF16  MMA-CHECK-SMEM
   MMA-CHECK-BLDM  MMA-CHECK-WARPS  MMA-CHECK-EPI  MMA-CHECK-REGS
   MMA-CHECK-GROUP  MMA-CHECK-SPLIT ;

\ apply + check a config; throws the specific E-MMA-* if the config is illegal.
: AT-CHECK-CFG ( ptr a -- )  AT-APPLY AT-CHECK ;

\ pure legality probe: never leaves the emitter's knob state mutated.
: AT-LEGAL? ( ptr a -- bool ) {: c:ptr :}
   AT-SAVE
   c AT-APPLY
   [: AT-CHECK ;] catch {: code:n :}
   AT-RESTORE
   code 0= ;

\ ============ (4) WINNERS - the committed table keyed (shape-class, dtype) =====
\ Reviewed data next to perf-rows.tsv. One row per (shape-class dim, dtype). Row =
\ 1 class-dim cell + a 15-cell config sub-record (field order = the AT-* menu). Each
\ row's note cites its tools/ptx/perf-rows.tsv source measurement (all GB10, sm_121a,
\ element-exact via tools/ptx/mma-gemm-check.f). Winners are the reviewed per-shape
\ bold configs of docs/eval-triton.md Rounds 3/6/7/10.
private
\ fields:      dim   W  MF  BK PAD ST DYN EPI DT LM  BN GRP SK BLDM BTF BPAD
create AT-WINNERS
\ --- tf32 (dtype 0) ---
   512 ,   4 , 4 , 32 , 8 , 2 , 1 , 1 , 0 , 2 ,  64 , 0 , 1 , 0 , 0 , 0 ,  \ MMM-W4-M4-S2-EPI 262144 16338128 (Round 3, 512 WINNER)
  1024 ,   4 , 4 , 32 , 8 , 1 , 0 , 1 , 0 , 2 ,  64 , 0 , 1 , 0 , 0 , 0 ,  \ MMM-W4-M4-S1-EPI 1048576 29922312 ptxas133 (Round 3, 1024 WINNER)
  2048 ,   4 , 4 , 32 , 8 , 1 , 0 , 1 , 0 , 2 ,  64 , 0 , 1 , 0 , 0 , 0 ,  \ MMM-W4-M4-S1-EPI 4194304 32560225 ptxas133 (Round 3, 2048)
  4096 ,   8 , 2 , 16 , 8 , 3 , 1 , 0 , 0 , 2 , 256 , 8 , 1 , 0 , 0 , 0 ,  \ MMM-BN256-BK16-S3-G8 16777216 33821967 ptxas133 (Round 10, 4096 WINNER)
\ --- fp16 (dtype 1) ---
   512 ,   4 , 4 , 32 , 8 , 2 , 1 , 0 , 1 , 0 ,  64 , 0 , 1 , 0 , 0 , 0 ,  \ MMM-F16-W4-M4-S2 262144 16326442 (Round 6, 512 WINNER k-major)
  1024 ,   4 , 4 , 32 , 8 , 2 , 1 , 0 , 1 , 0 ,  64 , 0 , 1 , 0 , 0 , 0 ,  \ MMM-F16-W4-M4-S2 1048576 36108183 (Round 6, 1024 WINNER k-major)
  2048 ,   4 , 4 , 32 , 8 , 2 , 1 , 0 , 1 , 0 ,  64 , 0 , 1 , 0 , 0 , 0 ,  \ MMM-F16-W4-M4-S2 4194304 46063324 (Round 6, 2048 k-major 46.1 > transposed 45.2)
  4096 ,   4 , 4 , 32 , 8 , 1 , 0 , 0 , 1 , 0 ,  64 , 0 , 1 , 0 , 1 , 8 ,  \ MMM-F16T-W4-M4-S1 16777216 47227116 (Round 6, 4096 WINNER transposed-Bs)
\ --- bf16 (dtype 2) ---
   512 ,   4 , 4 , 32 , 8 , 2 , 1 , 0 , 2 , 0 ,  64 , 0 , 1 , 0 , 0 , 0 ,  \ MMM-BF16-W4-M4-S2 262144 16346647 (Round 7, 512 WINNER k-major)
  1024 ,   4 , 4 , 32 , 8 , 2 , 1 , 0 , 2 , 0 ,  64 , 0 , 1 , 0 , 0 , 0 ,  \ MMM-BF16-W4-M4-S2 1048576 36111780 (Round 7, 1024 WINNER k-major)
  2048 ,   4 , 4 , 32 , 8 , 1 , 0 , 0 , 2 , 0 ,  64 , 0 , 1 , 0 , 1 , 8 ,  \ MMM-BF16T-W4-M4-S1 4194304 46344051 (Round 7, 2048 WINNER transposed-Bs)
  4096 ,   4 , 4 , 32 , 8 , 1 , 0 , 0 , 2 , 0 ,  64 , 0 , 1 , 0 , 1 , 8 ,  \ MMM-BF16T-W4-M4-S1 16777216 46899720 (Round 7, 4096 WINNER transposed-Bs)

public
12 constant AT-WIN#            \ committed rows
16 constant AT-ROW-N           \ cells per row (1 class-dim + AT-CFG-N config)
: AT-WIN-ROW ( n -- ptr a )  AT-ROW-N * cells AT-WINNERS + ;   \ row index 0..AT-WIN#-1
: AT-ROW-DIM@ ( ptr a -- n )   @ ;
: AT-ROW-CFG ( ptr a -- ptr a )  1 cells + ;                   \ 15-cell config sub-record

\ ---- bucketing rule (documented, integer-only, no overflow) -----------------
\ The shape's work W = M*N*K (the GEMM FLOP proxy). Each committed class is a cube
\ c^3; classes step by 8x in work, so "nearest" is nearest in GEOMETRIC (log) space
\ = the M*N*K-product class whose ratio max(W/c^3, c^3/W) is smallest. The interior
\ split between adjacent classes a<b is their geometric-mean work sqrt(a^3*b^3)
\ (= a^3*sqrt(8)); the committed midpoints below are those means (rounded). Outside
\ the covered band [512^3/8, 4096^3*8] (ratio > 8 from even the nearest class) the
\ shape is a genuinely NEW class -> E-AT-NEWCLASS (offline sweep + review), never a
\ silent default. All boundaries are committed integer constants (products fit 64b).
private
16777216      constant AT-FLOOR   \ 512^3 / 8  (new-class floor)
379625050     constant AT-MID01   \ geomean(512^3, 1024^3)   = 512^3 * sqrt(8)
3037000396    constant AT-MID12   \ geomean(1024^3, 2048^3)  = 1024^3 * sqrt(8)
24296003000   constant AT-MID23   \ geomean(2048^3, 4096^3)  = 2048^3 * sqrt(8)
549755813888  constant AT-CEIL    \ 4096^3 * 8  (new-class ceiling)

: AT-BUCKET ( n -- n ) {: w:n :}                      \ w -> nearest committed class dim (assumes in-band)
   w AT-MID01 <  if  512 exit  then
   w AT-MID12 <  if 1024 exit  then
   w AT-MID23 <  if 2048 exit  then
   4096 ;

public
\ ============ (4) SELECTOR - plan-time table lookup ==========================
\ AT-SELECT maps a GEMM (M,N,K,dtype) to a committed config record:
\   dtype not in {0,1,2}                  -> E-AT-DTYPE
\   W = M*N*K outside [512^3/8, 4096^3*8]  -> E-AT-NEWCLASS (offline sweep + review)
\   otherwise                             -> the winner for (nearest class, dtype)
\ Deterministic, side-effect free: the same shape always resolves to the same
\ committed config with no runtime bench. PRECEDENCE (for the integration seam):
\ an EXPLICIT caller-set config always wins - a caller that has already chosen the
\ cg-mma knobs simply does NOT call AT-AUTOTUNE. The table is consulted only when a
\ GEMM lowering carries NO explicit config; the selected config's fields then map
\ onto the knobs via AT-APPLY.
: AT-DTYPE-OK? ( n -- bool )  dup 0 >= swap 3 < and ;

: AT-SELECT ( n n n n -- ptr a ) {: dt:n :}           \ ( M N K dtype -- cfg )
   dt AT-DTYPE-OK? 0= if E-AT-DTYPE throw then
   * * {: w:n :}                                      \ W = M*N*K
   w AT-FLOOR <  w AT-CEIL >  or if E-AT-NEWCLASS throw then
   w AT-BUCKET {: dim:n :}
   AT-WIN# 0 ?do
      i AT-WIN-ROW {: row:ptr :}
      row AT-ROW-DIM@ dim =  row AT-ROW-CFG AT-DTYPE AT-CFG@ dt =  and
      if row AT-ROW-CFG unloop exit then
   loop
   E-AT-NEWCLASS throw ;                              \ (class,dtype) absent: table integrity - treat as new class

\ consult the table and APPLY the winner to the emitter's knobs (the callable
\ selector surface a config-less GEMM lowering uses).
: AT-AUTOTUNE ( n n n n -- )  AT-SELECT AT-APPLY ;

\ ============ (3) STOPWATCH - the sweep harness's device-free decision logic ===
\ The GB sweep harness lives in tools/ptx/autotune-sweep.f (it drives the GPU, so
\ it stays out of the resident gate image). The pure decisions it makes - is a
\ clock sample stable, which candidates were excluded and why, how a measured row
\ is laid out - live HERE so they are UNIT-TESTED in the gate's ptx-toolchain slice
\ (tools/ptx/autotune-test.f), not only proven by the device smoke run. Integer +
\ string logic only; no libcuda, no emit.

\ ---- (3a) clock-tolerance classifier -----------------------------------------
\ A best-of-3 sample is honest only if the SM clock HELD across the burst. Given
\ the SM clock (MHz) read just before and just after the timed burst, the sample
\ is STABLE iff the drop from the higher to the lower reading is within tol PERMILLE
\ of the higher: (hi-lo)*1000 <= tol*hi. Integer-exact, no divide. A zero reading
\ (clock query failed) is never stable - fail closed so a bad probe cannot pass.
: AT-CLK-STABLE? ( n n n -- bool ) {: before:n after:n tol:n :}
   before after min {: lo:n :}
   before after max {: hi:n :}
   hi 0= if STR-FALSE exit then
   tol hi *   hi lo - 1000 *   >= ;

\ ---- (3b) exclusion bookkeeping ----------------------------------------------
\ Every candidate the sweep does NOT time is recorded with its reason, so the
\ report LISTS the excluded configs - never a silent skip. Row = (cfg-id, reason).
0 constant AT-XR-TIMED          \ not excluded: timed and reported
1 constant AT-XR-PRUNED         \ AT-LEGAL? rejected it (emitter guard) - never emitted
2 constant AT-XR-INEXACT        \ element-exact precondition failed - emitted+checked, never timed
3 constant AT-XR-UNSTABLE       \ no clock-stable best-of-3 within the retry budget - never reported
64 constant AT-XCL-CAP          \ recorded-exclusion capacity
private
create AT-XCL-TAB AT-XCL-CAP 2 * cells allot
variable AT-XCL-CNT
public
: AT-XCL-RESET ( -- )  0 AT-XCL-CNT ! ;
: AT-XCL-N ( -- n )  AT-XCL-CNT @ ;
: AT-XCL-ADD ( n n -- ) {: id:n r:n :}
   AT-XCL-CNT @ {: k:n :}
   k AT-XCL-CAP >= if E-AT-XCLFULL throw then
   id  AT-XCL-TAB k 2 * cells + !
   r   AT-XCL-TAB k 2 * 1+ cells + !
   k 1+ AT-XCL-CNT ! ;
: AT-XCL-ID@ ( n -- n )         2 * cells AT-XCL-TAB + @ ;
: AT-XCL-REASON@ ( n -- n )        2 * 1+ cells AT-XCL-TAB + @ ;
: AT-XR$ ( n -- ptr u8 n )                      \ report label for a reason code
   dup AT-XR-PRUNED   = if drop s" pruned"   exit then
   dup AT-XR-INEXACT  = if drop s" inexact"  exit then
   dup AT-XR-UNSTABLE = if drop s" unstable" exit then
   drop s" timed" ;

\ ---- (3c) candidate-report row formatting ------------------------------------
\ The harness emits a candidate report in the perf-rows.tsv 12-field layout (kernel
\ grid gridy block blocky iters work metric value_x1000 device date note) for the
\ maintainer to review; it NEVER writes perf-rows.tsv itself (committed winners are
\ reviewed data). These append one tab-separated field into SB; the harness composes
\ a row from them and reads it back with SB$.
: AT-TAB ( -- )  9 SB-APPEND-C ;                     \ ASCII tab field separator
: AT-FLD-N ( n -- )  SB-INT AT-TAB ;                 \ integer field + tab
: AT-FLD$ ( ptr u8 n -- )  SB-APPEND AT-TAB ;        \ string field + tab

;package
