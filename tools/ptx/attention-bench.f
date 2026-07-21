\ attention-bench.f - CUDA-event benchmark of the fused ATTENTION kernel (ATTN).
\
\ The producer tools/ptx/attention-cg.f (lib/ptx/cg-attention.f) emits O = softmax(Q*K^T)*V
\ for one head, one query per block, block = N threads, D <= 64, N <= 128. This tool
\ SELF-EMITS that checked kernel to a PRIVATE per-run toolchain root and ptxas-assembles
\ it fail-closed via tools/ptx/fusion-emit.f (PTXFE:BUILD-KERNEL): a missing producer or a
\ nonzero emit/ptxas rc fails CLOSED with the named E-PTX-EMIT throw (child stderr surfaced
\ first), never a stale shared /tmp cubin. The fusion emit prelude already provides every
\ checked lib attention-cg requires (errors/string/float/fmt + emit/cg/header), so the emit
\ word is reused unchanged. The emit half needs no device and is proven host-side by
\ tools/ptx/attention-bench-test.f; assemble + launch need the CUDA toolchain, so the whole
\ device leg sits behind CUDA:OPEN? and is a recorded SKIP off-device (this file still
\ check-loads). ABI: pQ@0 pK@8 pV@16 pO@24 pN@32 pD@36; grid=N, block=N.
\
\ FLOP MODEL (per launch, one head, N queries x D dims):
\   QK^T   : each of N queries dots N keys over D  -> N*N*D multiply-adds = 2*N*N*D flops
\   softmax: per row max + (sub,exp,add) + reciprocal over N elements ~ 5*N*N flops
\   PV     : each of N*D outputs sums S[i]*V[i]    -> N*N*D multiply-adds = 2*N*N*D flops
\   total  = 4*N*N*D + 5*N*N flops   (the softmax term is a small correction to the two GEMMs)
\ Run on the Orin: scp to zed:Work/habu then `bin/hb --load tools/ptx/attention-bench.f`.

require lib/errors.f
require lib/string.f
require maki/eval/active-target.f
require tools/ptx/fusion-emit.f
require tools/ptx/bench.f

package ATTNBENCH

64 constant AB-D                       \ head dim D (<= 64), fixed across the shape ladder
40 constant AB-PARAM-BYTES             \ pQ,pK,pV,pO (8 B each) + pN,pD (4 B each) = 40
2000 constant AB-ITERS                 \ launches/shape, sized for stable CUDA-event timing on small launches
128 constant AB-NMAX                   \ max query rows N (kernel shared layout limit)
AB-NMAX AB-D * 4 * constant AB-BYTES-MAX   \ largest matrix in bytes = 128*64*4 = 32768

create AB-HOST AB-BYTES-MAX allot      \ host staging: cyclic f32 pattern, HTOD'd to Q/K/V
create AB-FILL $00000000 , $3DCCCCCD , $3E4CCCCD , $3E99999A , $3ECCCCCD , $3F000000 , $3F19999A , $3F333333 ,
8 constant AB-FILL-N                    \ 0.0 .. 0.7 step 0.1 as f32 bits; bounded so exp() stays finite

variable AB-DQ  variable AB-DK  variable AB-DV  variable AB-DO
variable AB-NV                         \ N as the u32 kernel param (pN)
variable AB-DP                         \ D as the u32 kernel param (pD)

: AB-INT. ( n -- )  SB-RESET FMT:SB-INT SB$ type ;

: AB-FBITS ( n n -- n ) {: seed:n idx:n :}    \ cyclic f32 bit pattern: deterministic, non-uniform, bounded
   seed idx + AB-FILL-N mod cells AB-FILL + @ ;

: AB-PUT ( n n -- ) {: v:n idx:n :}           \ little-endian f32 store of v at cell index idx in AB-HOST
   idx 4 * {: o:n :}
   v $FF and           AB-HOST o + c!
   v 8 rshift $FF and  AB-HOST o 1 + + c!
   v 16 rshift $FF and AB-HOST o 2 + + c!
   v 24 rshift $FF and AB-HOST o 3 + + c! ;

: AB-STAGE ( n n -- ) {: seed:n e:n :}        \ fill AB-HOST[0..e) with the seeded cyclic pattern
   e 0 ?do  seed i AB-FBITS  i AB-PUT  loop ;

: AB-ALLOC ( n -- ) {: n:n :}                 \ alloc Q/K/V/O (n x D f32) + stage non-uniform Q/K/V inputs
   n AB-D * {: e:n :}
   e 4 * {: by:n :}
   by AB-DQ PTXBENCH:DEVICE-ALLOC  AB-DQ PTXBENCH:OWN-DEV
   by AB-DK PTXBENCH:DEVICE-ALLOC  AB-DK PTXBENCH:OWN-DEV
   by AB-DV PTXBENCH:DEVICE-ALLOC  AB-DV PTXBENCH:OWN-DEV
   by AB-DO PTXBENCH:DEVICE-ALLOC  AB-DO PTXBENCH:OWN-DEV
   0 e AB-STAGE  AB-DQ @ AB-HOST by PTXBENCH:HTOD
   3 e AB-STAGE  AB-DK @ AB-HOST by PTXBENCH:HTOD
   5 e AB-STAGE  AB-DV @ AB-HOST by PTXBENCH:HTOD ;

: AB-PARAMS ( n -- ) {: n:n :}                 \ grid = N blocks, block = N threads (one query/block)
   n AB-NV !  AB-D AB-DP !
   n PTXBENCH:BLOCK!  1 PTXBENCH:BLOCKY!
   n PTXBENCH:GRID!   1 PTXBENCH:GRIDY!
   AB-PARAM-BYTES PTXBENCH:PARAM-BYTES!
   PTXBENCH:PREPARE-LAUNCH
   0  AB-DQ PTXBENCH:PARAM-PTR!
   8  AB-DK PTXBENCH:PARAM-PTR!
   16 AB-DV PTXBENCH:PARAM-PTR!
   24 AB-DO PTXBENCH:PARAM-PTR!
   32 AB-NV PTXBENCH:PARAM-U32!                \ pN
   36 AB-DP PTXBENCH:PARAM-U32! ;              \ pD

: AB-FLOPS ( n n -- n ) {: n:n it:n :}         \ (4*N*N*D + 5*N*N) per launch, x iters
   n n * {: nn:n :}
   nn AB-D * 4 *  nn 5 *  +  it * ;

: AB-BYTES ( n n -- n ) {: n:n it:n :}         \ Q+K+V+O tensor footprint (4 tensors, 4 B/f32), x iters
   n AB-D * 16 * it * ;

: AB-SHAPE ( n n -- ) {: n:n it:n :}
   it PTXBENCH:ITERS!
   n AB-ALLOC  n AB-PARAMS
   PTXBENCH:BENCH-GPU-NS {: ns:n :}
   s" ATTENTION N=" type n AB-INT. s"  D=" type AB-D AB-INT. s"  iters=" type it AB-INT. cr
   n AB-D * PTXBENCH:WORK!
   n it AB-BYTES  n it AB-FLOPS  ns PTXBENCH:REPORT-GPU ;   \ per-shape buffers owned by the run's scope (freed at UNWIND, not here)

: AB-SHAPES ( -- )
   64  AB-ITERS AB-SHAPE
   128 AB-ITERS AB-SHAPE ;

: AB-KERNEL ( -- )                             \ load ATTN from the private per-run cubin, bench the ladder
   PTXBENCH:RESET
   PTXTC:CUBIN$ PTXBENCH:CUBIN!
   s" ATTN" PTXBENCH:KERNEL!  s" ATTENTION" PTXBENCH:LABEL!
   [: PTXBENCH:OPEN  PTXBENCH:OWN-CTX  PTXBENCH:LOAD  PTXBENCH:OWN-MOD   \ one owning frame around the whole run: ctx+module load-once, per-shape buffers accumulate + unwind together
      AB-SHAPES ;] CUDA-SCOPE:SCOPE ;

: AB-RUN ( -- )                                \ device leg: self-emit+assemble, bench, drop the private root
   s" == ATTENTION fused (one query/block, serial softmax; correctness baseline) ==" type cr
   ATGT:LABEL$ PTXTC:TC-ARCH!                  \ assembler arch from the probed active target (BUILD-KERNEL's ptxas fails closed on the GB10 otherwise)
   s" habu-attention-bench" s" tools/ptx/attention-cg.f" PTXFE:BUILD-KERNEL
   AB-KERNEL
   PTXTC:CLEAN ;

public

: AB-ALL ( -- )
   CUDA:OPEN? 0= if s" attention-bench: libcuda unavailable -> SKIPPED (off-device)" type cr exit then
   AB-RUN ;

;package

ATTNBENCH:AB-ALL
