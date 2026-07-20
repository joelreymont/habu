\ mma-profile.f - emit ONE TF32 mma.sync GEMM config and launch MMM EXACTLY ONCE so an
\ external profiler (ncu / nsys) captures a single clean tensor-core kernel launch. This is
\ the profile-first harness for dot habu-close-mma-gemm: it isolates one cg-mma.f tile config
\ (BK / pad / stages / dyn-smem / fragment-load mode) at one shape, with NO timing loop and NO
\ warmup, so `ncu -k MMM --launch-count 1` profiles the intended kernel and nothing else.
\
\ Config-driven (all optional, left-to-right, defaults reproduce the current best swizzled config):
\   bin/hb --load tools/ptx/mma-profile.f -- BK PAD STAGES DYN MODE SHAPE
\   default: 64 8 2 1 2 1024  (BK=64 pad=8 stages=2 dynamic-.shared ldmatrix, 1024^3)
\ A=B=1.0, C=0 (values immaterial to a profile). Device-only: off the Orin (no libcuda) MP-RUN
\ prints SKIP and exits 0 so the file still check-loads. Correctness is owned by
\ tools/ptx/mma-gemm-check.f; timing by tools/ptx/gemm-bench.f. This harness only shapes the
\ launch for a profiler.

require lib/errors.f
require lib/string.f
require lib/float.f
require lib/fmt.f
require lib/fs.f
require lib/fs-mutate.f
require src/arch/ptx/emit.f
require lib/ptx/cg.f
require lib/ptx/cg-matmul.f
require lib/ptx/cg-mma.f
require lib/ptx/toolchain.f
require tools/ptx/bench.f

package MMAPROFILE

$3F800000 constant MP-ONE               \ 1.0f bit pattern (A/B fill)
create MP-QO $1000 allot  create MP-QE $1000 allot
variable MP-DA  variable MP-DB  variable MP-DC
variable MP-NV                          \ M=N=K (square) as the u32 kernel param

: MP-INT. ( n -- )  SB-RESET FMT:SB-INT SB$ type ;

: MP-ARG ( n n -- n )                   \ ( default index -- value ) SCRIPT-ARGV[i] as int, else default
   {: def:n i:n :}
   i SCRIPT-ARGC >= if def exit then
   i SCRIPT-ARGV$ STR>NUMBER? MATCH option
     none OF def ENDOF
     some OF ENDOF
   ;MATCH ;

: MP-ASSEMBLE ( -- )                    \ captured PTX -> kernel.ptx -> ptxas cubin (die on rc<>0)
   PTXTC:PTX$ PTX-CAPTURE$ WRITE-ALL
   MP-QO $1000 >LEN MP-QE $1000 >LEN PTXTC:ASSEMBLE PTXTC:ASM-REPORT {: rc:n :}
   rc 0= 0= if s" mma-profile: ptxas failed" 1 die then ;

: MP-ALLOC ( n -- ) {: s:n :}           \ alloc + fill A=B=1.0, C=0 for an s x s GEMM
   s s * {: e:n :}
   e 4 * MP-DA PTXBENCH:DEVICE-ALLOC
   e 4 * MP-DB PTXBENCH:DEVICE-ALLOC
   e 4 * MP-DC PTXBENCH:DEVICE-ALLOC
   MP-DA @ MP-ONE e PTXBENCH:DEVICE-MEMSET32
   MP-DB @ MP-ONE e PTXBENCH:DEVICE-MEMSET32
   MP-DC @ 0     e PTXBENCH:DEVICE-MEMSET32 ;

: MP-PARAMS ( n -- ) {: s:n :}          \ 2D grid = (s/64)^2 output tiles, 16x16 block
   s MP-NV !
   16 PTXBENCH:BLOCK!  16 PTXBENCH:BLOCKY!
   s 64 / PTXBENCH:GRID!  s 64 / PTXBENCH:GRIDY!
   36 PTXBENCH:PARAM-BYTES!
   MMA-DYNSMEM @ if MMA-SMEM PTXBENCH:SHARED! else 0 PTXBENCH:SHARED! then
   PTXBENCH:PREPARE-LAUNCH
   0  MP-DA PTXBENCH:PARAM-PTR!
   8  MP-DB PTXBENCH:PARAM-PTR!
   16 MP-DC PTXBENCH:PARAM-PTR!
   24 MP-NV PTXBENCH:PARAM-U32!  28 MP-NV PTXBENCH:PARAM-U32!  32 MP-NV PTXBENCH:PARAM-U32! ;

: MP-FREE ( -- )
   MP-DA @ 0 <> if MP-DA @ PTXBENCH:DEVICE-FREE then
   MP-DB @ 0 <> if MP-DB @ PTXBENCH:DEVICE-FREE then
   MP-DC @ 0 <> if MP-DC @ PTXBENCH:DEVICE-FREE then
   0 MP-DA !  0 MP-DB !  0 MP-DC ! ;

: MP-EMIT-LOAD ( -- )                   \ emit MMM at the active knobs, assemble, module-load
   s" habu-mma-profile" PTXTC:PREPARE
   PTX-CAPTURE-ON  EMIT-MATMUL-MMA  PTX-CAPTURE-OFF
   MP-ASSEMBLE
   PTXBENCH:RESET
   PTXTC:CUBIN$ PTXBENCH:CUBIN!
   s" MMM" PTXBENCH:KERNEL!  s" MMM" PTXBENCH:LABEL!
   PTXBENCH:OPEN  PTXBENCH:LOAD ;

: MP-CFG. ( n n n n n n -- )            \ echo the resolved config (does not consume)
   {: bk:n pad:n stages:n dyn:n mode:n shape:n :}
   s" mma-profile: BK=" type bk MP-INT.  s"  pad=" type pad MP-INT.
   s"  stages=" type stages MP-INT.  s"  dyn=" type dyn MP-INT.
   s"  frag=" type mode MP-INT.  s"  shape=" type shape MP-INT.
   s"  smem=" type MMA-SMEM MP-INT. s"  B" type cr ;

public

: MP-RUN ( -- )                         \ one config, one launch, for an external profiler
   64 0 MP-ARG  8 1 MP-ARG  2 2 MP-ARG  1 3 MP-ARG  2 4 MP-ARG  1024 5 MP-ARG
   {: bk:n pad:n stages:n dyn:n mode:n shape:n :}
   bk MMA-BK !  pad MMA-PAD !  stages MMA-STAGES !  dyn MMA-DYNSMEM !  mode MMA-LMODE !
   bk pad stages dyn mode shape MP-CFG.
   CUDA:OPEN? 0= if s" mma-profile: libcuda unavailable -> SKIPPED (off-device)" type cr
      32 MMA-BK !  0 MMA-PAD !  2 MMA-STAGES !  0 MMA-DYNSMEM !  0 MMA-LMODE !  exit then
   MP-EMIT-LOAD
   shape MP-ALLOC  shape MP-PARAMS
   PTXBENCH:LAUNCH  PTXBENCH:SYNC       \ EXACTLY ONE MMM launch (profiler target)
   MP-FREE
   PTXBENCH:UNLOAD  PTXBENCH:CLOSE
   PTXTC:CLEAN
   s" mma-profile: 1 MMM launch complete" type cr
   32 MMA-BK !  0 MMA-PAD !  2 MMA-STAGES !  0 MMA-DYNSMEM !  0 MMA-LMODE ! ;

;package

MMAPROFILE:MP-RUN
