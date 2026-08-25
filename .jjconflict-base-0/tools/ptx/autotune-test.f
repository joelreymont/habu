\ autotune-test.f - checked fixtures for tools/ptx/autotune.f: the config-record
\ menu, the mechanical legality PRUNE (every committed winner legal; every
\ documented-illegal combo rejected with its specific E-MMA-*), and the plan-time
\ WINNERS selector (exact class, nearest-class bucketing, new-class + bad-dtype
\ errors, dtype keying, non-mutating legality probe). dot habu-committed-autotune-menu.

require lib/test.f
require tools/ptx/autotune.f

package AT-RT

\ ---- thin aliases into the module (keeps the assertions readable) -----------
: CFG@ ( ptr a n -- n )   AUTOTUNE:AT-CFG@ ;
: CFG! ( n ptr a n -- )   AUTOTUNE:AT-CFG! ;
: DEF! ( ptr a -- )       AUTOTUNE:AT-DEFAULTS ;
: SEL ( n n n n -- ptr a ) AUTOTUNE:AT-SELECT ;
: AUTO ( n n n n -- )     AUTOTUNE:AT-AUTOTUNE ;
: APPLY ( ptr a -- )      AUTOTUNE:AT-APPLY ;
: LEGAL? ( ptr a -- bool ) AUTOTUNE:AT-LEGAL? ;
: CHECKCFG ( ptr a -- )   AUTOTUNE:AT-CHECK-CFG ;
: WROW ( n -- ptr a )     AUTOTUNE:AT-WIN-ROW ;
: RCFG ( ptr a -- ptr a ) AUTOTUNE:AT-ROW-CFG ;
: RDIM ( ptr a -- n )     AUTOTUNE:AT-ROW-DIM@ ;
\ (3) STOPWATCH pure-logic aliases
: CLKSTABLE? ( n n n -- bool ) AUTOTUNE:AT-CLK-STABLE? ;
: XRESET ( -- )           AUTOTUNE:AT-XCL-RESET ;
: XADD ( n n -- )        AUTOTUNE:AT-XCL-ADD ;
: XN ( -- n )             AUTOTUNE:AT-XCL-N ;
: XID ( n -- n )       AUTOTUNE:AT-XCL-ID@ ;
: XREASON ( n -- n )    AUTOTUNE:AT-XCL-REASON@ ;
: XR$ ( n -- ptr u8 n )   AUTOTUNE:AT-XR$ ;
variable SW-CAND          \ candidate counter for the menu-iteration test

AUTOTUNE:AT-WARPS  constant kW    AUTOTUNE:AT-MFRAGS constant kMF
AUTOTUNE:AT-BK     constant kBK   AUTOTUNE:AT-PAD    constant kPAD
AUTOTUNE:AT-STAGES constant kST   AUTOTUNE:AT-DYN    constant kDYN
AUTOTUNE:AT-EPILOG constant kEPI  AUTOTUNE:AT-DTYPE  constant kDT
AUTOTUNE:AT-LMODE  constant kLM   AUTOTUNE:AT-BN     constant kBN
AUTOTUNE:AT-GROUP  constant kGRP
AUTOTUNE:AT-BLDM   constant kBLDM AUTOTUNE:AT-BTF16  constant kBTF
AUTOTUNE:AT-BPAD   constant kBPAD

create TC AUTOTUNE:AT-CFG-N cells allot     \ scratch config record for building candidates
: TC@ ( n -- n )  TC swap CFG@ ;

\ ---- (1) MENU: the config record + accessors ---------------------------------
: MENU-TESTS ( -- )
   AUTOTUNE:AT-CFG-N 14 T=
   TC DEF!                                   \ the byte-identical baseline record
   kW   TC@ 8 T=   kMF TC@ 1 T=   kBK TC@ 32 T=   kST TC@ 2 T=
   kDYN TC@ 0 T=   kDT TC@ 0 T=   kBN TC@ 64 T=
   4 TC kW CFG!   256 TC kBN CFG!            \ field writes read back
   kW TC@ 4 T=    kBN TC@ 256 T= ;

\ ---- (1) MENU: the per-axis candidate value lists (DATA) ---------------------
: AXES-TESTS ( -- )
   AUTOTUNE:AT-AX-WARPS AUTOTUNE:AT-AXIS-N 2 T=
   AUTOTUNE:AT-AX-WARPS 0 AUTOTUNE:AT-AXIS@ 4 T=
   AUTOTUNE:AT-AX-WARPS 1 AUTOTUNE:AT-AXIS@ 8 T=
   AUTOTUNE:AT-AX-MFRAGS AUTOTUNE:AT-AXIS-N 3 T=
   AUTOTUNE:AT-AX-BN 2 AUTOTUNE:AT-AXIS@ 256 T=
   AUTOTUNE:AT-AX-DTYPE AUTOTUNE:AT-AXIS-N 3 T= ;

\ ---- (4) WINNERS: table shape + spot-checked committed rows -------------------
: WINNERS-TESTS ( -- )
   AUTOTUNE:AT-WIN# 12 T=
   \ tf32 4096 winner = the BN=256 BK=16 stages=3 grouped-raster tile (Round 10)
   4096 4096 4096 0 SEL {: c:ptr :}
   c kBN CFG@ 256 T=   c kGRP CFG@ 8 T=   c kBK CFG@ 16 T=   c kST CFG@ 3 T=
   c kW CFG@ 8 T=      c kMF CFG@ 2 T=    c kLM CFG@ 2 T=
   \ tf32 512 winner = 4-warp MFRAGS=4 stages=2 dyn + smem epilogue (Round 3)
   512 512 512 0 SEL {: c2:ptr :}
   c2 kW CFG@ 4 T=   c2 kMF CFG@ 4 T=   c2 kST CFG@ 2 T=   c2 kEPI CFG@ 1 T=   c2 kBN CFG@ 64 T=
   \ fp16/bf16 4096 winners = the transposed-Bs feed (Rounds 6/7)
   4096 4096 4096 1 SEL kBTF CFG@ 1 T=
   4096 4096 4096 2 SEL kBTF CFG@ 1 T= ;

\ ---- (2) PRUNE: every committed winner is legal to emit ----------------------
: WINNER-LEGAL-TESTS ( -- )
   AUTOTUNE:AT-WIN# 0 ?do  i WROW RCFG LEGAL? TTRUE  loop ;

\ ---- (2) PRUNE: documented-illegal combos rejected with the specific E-MMA-* --
\ Each helper builds a config (baseline + the offending knob) and runs it through
\ the emitter's own guards via CHECKCFG; the guard, not this test, decides the code.
: BAD-REGS ( -- )   TC DEF!  4 TC kMF CFG!  256 TC kBN CFG!  1 TC kDYN CFG!  TC CHECKCFG ;
: BAD-W4M1 ( -- )   TC DEF!  4 TC kW CFG!   1 TC kMF CFG!    TC CHECKCFG ;
: BAD-W16 ( -- )    TC DEF!  16 TC kW CFG!  TC CHECKCFG ;
: BAD-BLDM ( -- )   TC DEF!  1 TC kBLDM CFG!  1 TC kMF CFG!  TC CHECKCFG ;
: BAD-BTF16 ( -- )  TC DEF!  1 TC kBTF CFG!   TC CHECKCFG ;
: BAD-BN96 ( -- )   TC DEF!  96 TC kBN CFG!   TC CHECKCFG ;
: BAD-BN32 ( -- )   TC DEF!  32 TC kBN CFG!   TC CHECKCFG ;
: BAD-GROUP ( -- )  TC DEF!  -1 TC kGRP CFG!  TC CHECKCFG ;
: BAD-DT-LM1 ( -- ) TC DEF!  1 TC kDT CFG!  1 TC kLM CFG!  TC CHECKCFG ;
: BAD-DT-BLDM ( -- ) TC DEF! 1 TC kDT CFG!  4 TC kMF CFG!  1 TC kBLDM CFG!  1 TC kDYN CFG!  TC CHECKCFG ;
: BAD-SMEM ( -- )   TC DEF!  2 TC kMF CFG!  256 TC kBN CFG!  0 TC kDYN CFG!  TC CHECKCFG ;
: BAD-EPI ( -- )    TC DEF!  4 TC kMF CFG!  256 TC kBN CFG!  1 TC kDYN CFG!  1 TC kEPI CFG!  TC CHECKCFG ;

: PRUNE-REJECT-TESTS ( -- )
   [: BAD-REGS ;]    E-MMA-REGS   TTHROWSQ
   [: BAD-W4M1 ;]    E-MMA-WARPS  TTHROWSQ
   [: BAD-W16 ;]     E-MMA-WARPS  TTHROWSQ
   [: BAD-BLDM ;]    E-MMA-BLDM   TTHROWSQ
   [: BAD-BTF16 ;]   E-MMA-BTF16  TTHROWSQ
   [: BAD-BN96 ;]    E-MMA-BN     TTHROWSQ
   [: BAD-BN32 ;]    E-MMA-BN     TTHROWSQ
   [: BAD-GROUP ;]   E-MMA-GROUP  TTHROWSQ
   [: BAD-DT-LM1 ;]  E-MMA-DTYPE  TTHROWSQ
   [: BAD-DT-BLDM ;] E-MMA-DTYPE  TTHROWSQ
   [: BAD-SMEM ;]    E-MMA-SMEM   TTHROWSQ
   [: BAD-EPI ;]     E-MMA-EPI    TTHROWSQ ;

\ ---- (4) SELECTOR: nearest-class bucketing (distinguishing fields) -----------
: BUCKET-TESTS ( -- )
   \ near 512^3 (below the 512/1024 geomean) -> the stages=2 512 tile
   600 600 600 0 SEL kST CFG@ 2 T=
   \ interior 1024 bucket -> stages=1, BN=64
   1200 1200 1200 0 SEL kST CFG@ 1 T=
   1200 1200 1200 0 SEL kBN CFG@ 64 T=
   \ near 4096^3 (above the 2048/4096 geomean) -> the BN=256 grouped tile
   3500 3500 3500 0 SEL kBN CFG@ 256 T=
   3500 3500 3500 0 SEL kGRP CFG@ 8 T=
   \ non-cube shape buckets on the M*N*K product: 2048x2048x512 (W~2.1e9) -> 1024 class
   2048 2048 512 0 SEL kBN CFG@ 64 T=
   2048 2048 512 0 SEL kST CFG@ 1 T= ;

\ ---- (4) SELECTOR: dtype keys the row (same shape, different tile) -----------
: DTYPE-KEY-TESTS ( -- )
   1024 1024 1024 0 SEL kDT CFG@ 0 T=
   1024 1024 1024 1 SEL kDT CFG@ 1 T=
   1024 1024 1024 2 SEL kDT CFG@ 2 T=
   \ tf32 1024 uses ldmatrix-A (LMODE=2); fp16 1024 uses the scalar k-major feed (LMODE=0)
   1024 1024 1024 0 SEL kLM CFG@ 2 T=
   1024 1024 1024 1 SEL kLM CFG@ 0 T= ;

\ ---- (4) SELECTOR: new-class + bad-dtype errors (never a silent default) ------
: SEL-64 ( -- )    64 64 64 0 SEL drop ;             \ W below the 512^3/8 floor
: SEL-BIG ( -- )   16384 16384 16384 0 SEL drop ;    \ W above the 4096^3*8 ceiling
: SEL-DT3 ( -- )   1024 1024 1024 3 SEL drop ;
: SEL-DTNEG ( -- ) 1024 1024 1024 -1 SEL drop ;
: SEL-ERROR-TESTS ( -- )
   [: SEL-64 ;]    E-AT-NEWCLASS TTHROWSQ
   [: SEL-BIG ;]   E-AT-NEWCLASS TTHROWSQ
   [: SEL-DT3 ;]   E-AT-DTYPE    TTHROWSQ
   [: SEL-DTNEG ;] E-AT-DTYPE    TTHROWSQ ;

\ ---- (2)/(4) PRUNE probe is non-mutating; SELECTOR precedence via AT-APPLY ----
\ AT-LEGAL? must leave the emitter's live knobs untouched, and AT-AUTOTUNE maps the
\ selected config onto them (an explicit caller simply does not call AT-AUTOTUNE).
: SENTINEL-KNOBS ( -- )  4 MMA-WARPS !  128 MMA-BN !  2 MMA-MFRAGS ! ;
: PROBE-TESTS ( -- )
   SENTINEL-KNOBS
   TC DEF!  16 TC kW CFG!  TC LEGAL? TFALSE       \ illegal probe (W16 -> E-MMA-WARPS)
   MMA-WARPS @ 4 T=   MMA-BN @ 128 T=  MMA-MFRAGS @ 2 T=   \ knobs restored, not mutated
   512 512 512 0 AUTO                             \ apply the 512 tf32 winner to the live knobs
   MMA-WARPS @ 4 T=   MMA-MFRAGS @ 4 T=  MMA-EPILOG @ 1 T=  MMA-BN @ 64 T=  MMA-STAGES @ 2 T=
   MMA-BN @ 128 <> TTRUE ;                        \ table selection overrode the sentinel

\ ---- (3a) STOPWATCH: clock-tolerance classifier -----------------------------
: CLK-TESTS ( -- )
   2400 2400 5 CLKSTABLE? TTRUE          \ no change -> stable
   2400 2388 5 CLKSTABLE? TTRUE          \ exactly 5 permille drop -> stable (boundary)
   2400 2387 5 CLKSTABLE? TFALSE         \ just past 5 permille -> unstable
   2280 2400 5 CLKSTABLE? TFALSE         \ order-independent: a 5% swing is rejected
   0 0 5 CLKSTABLE? TFALSE ;             \ a failed clock probe (0) is never stable

\ ---- (3b) STOPWATCH: exclusion bookkeeping + reason labels -------------------
: XCL-TESTS ( -- )
   XRESET  XN 0 T=
   7 AUTOTUNE:AT-XR-PRUNED XADD
   9 AUTOTUNE:AT-XR-INEXACT XADD
   XN 2 T=
   0 XID 7 T=   0 XREASON AUTOTUNE:AT-XR-PRUNED T=
   1 XID 9 T=   1 XREASON AUTOTUNE:AT-XR-INEXACT T=
   AUTOTUNE:AT-XR-PRUNED   XR$ s" pruned"   T$=
   AUTOTUNE:AT-XR-INEXACT  XR$ s" inexact"  T$=
   AUTOTUNE:AT-XR-UNSTABLE XR$ s" unstable" T$=
   AUTOTUNE:AT-XR-TIMED    XR$ s" timed"    T$= ;

\ ---- (3c) STOPWATCH: candidate-report row field formatting ------------------
: TSV-TESTS ( -- )
   SB-RESET
   s" a" AUTOTUNE:AT-FLD$   1 AUTOTUNE:AT-FLD-N   s" b" SB-APPEND
   SB$ S\" a\t1\tb" T$= ;

\ ---- (3) STOPWATCH: menu iteration + legality PRUNE partition ----------------
\ iterate the candidate set (the committed winners, all legal) plus one injected
\ illegal config; legal ones are timing candidates, the illegal one is recorded
\ PRUNED. Pure - AT-LEGAL? probes the emitter guards with no GPU.
: SWEEP-ITER-TESTS ( -- )
   XRESET  0 SW-CAND !
   AUTOTUNE:AT-WIN# 0 ?do
      i WROW RCFG LEGAL? if 1 SW-CAND +! else i AUTOTUNE:AT-XR-PRUNED XADD then
   loop
   TC DEF!  16 TC kW CFG!                          \ W=16 -> E-MMA-WARPS (illegal)
   TC LEGAL? if 999 SW-CAND +! else 999 AUTOTUNE:AT-XR-PRUNED XADD then
   SW-CAND @ AUTOTUNE:AT-WIN# T=                   \ all 12 winners are legal candidates
   XN 1 T=                                         \ exactly the injected illegal config was pruned
   0 XID 999 T=   0 XREASON AUTOTUNE:AT-XR-PRUNED T= ;

\ restore the byte-identical cg-mma baseline so a shared resident image sees defaults
: RESTORE-DEFAULTS ( -- )  TC DEF!  TC APPLY ;

T-RESET
MENU-TESTS
AXES-TESTS
WINNERS-TESTS
WINNER-LEGAL-TESTS
PRUNE-REJECT-TESTS
BUCKET-TESTS
DTYPE-KEY-TESTS
SEL-ERROR-TESTS
PROBE-TESTS
CLK-TESTS
XCL-TESTS
TSV-TESTS
SWEEP-ITER-TESTS
RESTORE-DEFAULTS
T-REPORT

;package
