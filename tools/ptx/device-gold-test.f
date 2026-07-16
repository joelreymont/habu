\ device-gold-test.f - HOST proof of the device-gold emit halves (no device, no ptxas).
\
\ tools/ptx/device-gold.f runs each flagship kernel's COMMITTED emitter in a spawned
\ bin/hb, then assembles + launches + golden-compares on the Orin. This test runs the
\ EMIT half only: it spawns each committed entry file and asserts the emitted PTX is the
\ right kernel body - SGEMM MM has the entry + the FMA accumulate + cp.async staging;
\ attention ATTN has the entry + the softmax ex2.approx + a bar.sync; fused relu(a*x+y)
\ has scale + bias + the relu clamp (max.f32); the bandwidth SAXPY has scale + bias but
\ NO clamp. The assemble + device launch/compare half is the on-device leg (device-gold
\ under the CUDA probe). Requires NO device FFI, so it is safe in the resident stdlib
\ runner (an INPROCESS ptx-toolchain member). Mirrors tools/ptx/fusion-emit-test.f.

require lib/test.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process-argv.f
require lib/ptx/toolchain.f
require lib/ptx/process-test-prelude.f

package DEVGOLDT

$10000 constant CAP-OUT-CAP                     \ emitted PTX (single MM ~28 KB)
$2000  constant CAP-ERR-CAP
30000  constant EMIT-TIMEOUT-MS

create CAP-OUT CAP-OUT-CAP allot  create CAP-ERR CAP-ERR-CAP allot
variable EMIT-U                                 \ bytes of the last emitted PTX

: ARGV-BASE ( -- )                              \ shared checked PTX-codegen load prefix
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   s" lib/errors.f" >LEN PROC-ARGV+  s" lib/string.f" >LEN PROC-ARGV+
   s" lib/float.f"  >LEN PROC-ARGV+  s" lib/fmt.f"    >LEN PROC-ARGV+
   s" src/arch/ptx/emit.f" >LEN PROC-ARGV+  s" lib/ptx/cg.f" >LEN PROC-ARGV+
   s" lib/ptx/header.f" >LEN PROC-ARGV+ ;
: ARGV-V4+ ( -- )                               \ v4 tile vocab for the SAXPY-family producers
   s" lib/ptx/cg-collective.f" >LEN PROC-ARGV+  s" lib/ptx/cg-vec.f" >LEN PROC-ARGV+
   s" lib/ptx/tile.f" >LEN PROC-ARGV+  s" lib/ptx/tile-v4.f" >LEN PROC-ARGV+ ;

: SPAWN-EMIT ( -- )                             \ argv built (entry last) -> capture PTX to CAP-OUT
   ENGINE-CANDIDATE:PATH$ >LEN  CAP-OUT CAP-OUT-CAP >LEN  CAP-ERR CAP-ERR-CAP >LEN  EMIT-TIMEOUT-MS >MS  RUN-ARGV-CAPTURE
   {: outu:len erru:len rc:rc :}
   CAP-ERR erru LEN>N  rc RC>N  PTXTC:EMIT-GUARD         \ missing producer / nonzero rc -> stderr + throw
   outu LEN>N EMIT-U ! ;
: EMITTED$ ( -- ptr u8 n )  CAP-OUT EMIT-U @ ;

: EMIT-GEMM ( -- )   ARGV-BASE  s" tools/ptx/matmul-cg.f"    >LEN PROC-ARGV+  SPAWN-EMIT ;
: EMIT-ATTN ( -- )   ARGV-BASE  s" tools/ptx/attention-cg.f" >LEN PROC-ARGV+  SPAWN-EMIT ;
: EMIT-FUSED ( -- )  ARGV-BASE ARGV-V4+  s" tools/ptx/fused-relu-cg.f" >LEN PROC-ARGV+  SPAWN-EMIT ;
: EMIT-BW ( -- )     ARGV-BASE ARGV-V4+  s" tools/ptx/saxpy-cg.f"       >LEN PROC-ARGV+  SPAWN-EMIT ;

: HAS ( ptr u8 n -- ) {: a:ptr u:n :}           \ emitted PTX contains the token
   EMITTED$ a u CONTAINS? TTRUE ;
: HAS-NOT ( ptr u8 n -- ) {: a:ptr u:n :}       \ emitted PTX omits the token
   EMITTED$ a u CONTAINS? 0= TTRUE ;

\ fail-closed: a missing entry makes the spawned bin/hb exit nonzero; EMIT-GUARD
\ surfaces the child stderr and throws E-PTX-EMIT, so a renamed/missing committed
\ producer can never yield a silent empty PTX (no device / ptxas needed to prove it).
: MISS-THROWS ( -- )
   [: ARGV-BASE  s" tools/ptx/does-not-exist-cg.f" >LEN PROC-ARGV+  SPAWN-EMIT ;] E-PTX-EMIT TTHROWSQ ;

T-RESET

\ SGEMM MM: entry + the FMA K-accumulate + cp.async shared staging + global store
EMIT-GEMM
EMIT-U @ 0 > TTRUE                              \ emit produced PTX bytes
s" .visible .entry MM" HAS
s" fma.rn.f32" HAS
s" cp.async" HAS
s" st.global.f32" HAS
s" ERROR" HAS-NOT                               \ no emit-error marker leaked into the PTX

\ attention ATTN: entry + softmax exp (ex2.approx) + phase barrier + global store
EMIT-ATTN
s" .visible .entry ATTN" HAS
s" ex2.approx.f32" HAS
s" bar.sync" HAS
s" st.global.f32" HAS

\ fused relu(a*x+y): scale (mul) + bias (add) + relu clamp (max ,0) all present
EMIT-FUSED
s" .visible .entry SAXPY" HAS
s" mul.rn.f32" HAS
s" add.rn.f32" HAS
s" max.f32" HAS

\ bandwidth SAXPY(a*x+y): scale + bias but NO relu clamp (a copy/scale kernel)
EMIT-BW
s" .visible .entry SAXPY" HAS
s" mul.rn.f32" HAS
s" add.rn.f32" HAS
s" max.f32" HAS-NOT

\ fail-closed: a missing committed producer throws E-PTX-EMIT, never an empty PTX
MISS-THROWS

T-REPORT
s" device-gold-test: ok" type cr

;package
