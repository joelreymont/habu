\ residency-probe.f - THE UMA weight-residency measurement (dot
\ habu-infer-safetensors-loader-0b58e06a; the sanctioned timing lane; plan of
\ record docs/inference-engine-plan.md M1). On the idle GB10 it times one
\ grid-stride read-reduction kernel (maki/infer/resid-kernel.cu) over ~100 MiB and
\ compares four weight-residency options as a FAIR comparison. Direct mmap is an
\ OPTION, not a predetermined policy:
\   (1) file-backed mmap read DIRECTLY by the kernel (UMA/ATS coherence) - reported
\       COLD (first GPU touch: ATS page-fault-in) and STEADY (prefaulted) separately;
\   (2) mmap + cuMemHostRegister (DEVICEMAP), read via the device pointer;
\   (3) a cuMemAlloc device buffer POPULATED ONCE from the file (HtoD copy) - the
\       one-time copy cost is reported alongside the steady read bandwidth.
\ (4) A PACKED (pre-transposed/quantized) layout is a later candidate (model-pack
\   dot), not measured here. cuMemAdvise applies to cuMemAllocManaged unified
\   memory, not to file mmap+register, so it is out of this probe's scope.
\ PREFAULT METHODOLOGY: STEADY numbers come from BENCH-GPU-NS, which runs one full
\ grid-stride warmup launch (faulting every page in via ATS) before the timed
\ burst; COLD is a single un-warmed launch timed on the host clock, capturing the
\ first-touch page-fault cost (the file is page-cache-resident, so COLD reflects
\ GPU-side ATS mapping, not disk I/O). Device resources use an exception-safe
\ CUDA-SCOPE frame. Run manually on spark (NOT part of the cold gate):
\   nvcc -arch=sm_121a -cubin maki/infer/resid-kernel.cu -o /tmp/hb-resid.cubin
\   head -c 104857600 /dev/urandom > gpt2-model/resid-host.bin
\   bin/hb --load maki/infer/residency-probe.f
\ Off a GPU (no libcuda) it reports SKIPPED and exits clean.

require lib/errors.f
require lib/string.f
require lib/fmt.f
require lib/fs.f
require lib/ffi-abi.f
require lib/ptx/cuda-driver.f
require lib/ptx/cuda-scope.f
require tools/ptx/bench.f

package RESID

104857600 constant RP-BYTES        \ 100 MiB streamed per launch
50 constant RP-ITERS               \ timed launches per steady-state burst
8192 constant RP-GRID              \ blocks (grid-stride covers the whole buffer)
256 constant RP-BLOCK
2 constant RP-HR-DEVICEMAP         \ CU_MEMHOSTREGISTER_DEVICEMAP (READ_ONLY 0x08 = NOT_SUPPORTED on GB10)

variable RP-N                      \ float4 count (u32 kernel arg)
variable RP-DIN                    \ device input buffer
variable RP-DOUT                   \ device reduction output
variable RP-HADDR                  \ host mapping numeric address
variable RP-DPTR                   \ device pointer for the registered host mapping
variable RP-COLD-NS                \ (1) cold first-touch (host clock)
variable RP-HOST-NS                \ (1) steady, prefaulted
variable RP-REG-NS                 \ (2) steady, registered
variable RP-COPY-NS                \ (3) one-time HtoD populate
variable RP-DEV-NS                 \ (3) steady, device buffer

\ Retirement owner: habu-epic-gb10-uma-391d12e8.
TRUSTED: RP-N>PTR ( n -- ptr u8 ) ;   \ host mapping address -> byte pointer for HtoD

: RP-HOST-PATH ( -- ptr u8 n )  s" gpt2-model/resid-host.bin" ;
: RP-CUBIN-PATH ( -- ptr u8 n )  s" /tmp/hb-resid.cubin" ;

: RP-MMAP-HOST ( -- n )            \ mmap the host scratch file read-only, return its address
   RP-HOST-PATH FILE-SIZE {: sz :}
   RP-HOST-PATH FS-PATHZ 0 0 open {: fd :}
   fd 0 < if E-FS-OPEN throw then
   0 sz 1 2 fd 0 mmap {: base :}
   fd close
   base 0 < if E-MEM-MAP throw then
   base ;

: RP-SETUP ( -- )
   PTXBENCH:RESET
   RP-CUBIN-PATH PTXBENCH:CUBIN!
   s" resid_read" PTXBENCH:KERNEL!
   s" resid_read" PTXBENCH:LABEL!
   RP-GRID  PTXBENCH:GRID!
   RP-BLOCK PTXBENCH:BLOCK!
   RP-ITERS PTXBENCH:ITERS!
   RP-N @   PTXBENCH:WORK!
   20 PTXBENCH:PARAM-BYTES! ;

: RP-SET-PARAMS ( ptr a -- )       \ arg = variable holding the input pointer value
   0 swap PTXBENCH:PARAM-PTR!      \ param[0]  = input pointer
   8 RP-DOUT PTXBENCH:PARAM-PTR!   \ param[8]  = output pointer
   16 RP-N PTXBENCH:PARAM-U32!     \ param[16] = float4 count
   PTXBENCH:PREPARE-LAUNCH ;

: RP-STEADY ( ptr a -- n )  RP-SET-PARAMS PTXBENCH:BENCH-GPU-NS ;    \ warmup + 50 timed iters
: RP-COLD ( ptr a -- n )                                            \ single un-warmed launch (host clock)
   RP-SET-PARAMS
   mono-ns {: t0 :}  PTXBENCH:LAUNCH PTXBENCH:SYNC  mono-ns t0 - ;

: RP-BODY ( -- )                   \ runs under a CUDA-SCOPE frame (owns ctx/module/buffers)
   PTXBENCH:OWN-CTX PTXBENCH:OWN-MOD
   4 RP-DOUT PTXBENCH:DEVICE-ALLOC        RP-DOUT PTXBENCH:OWN-DEV
   RP-BYTES RP-DIN PTXBENCH:DEVICE-ALLOC  RP-DIN PTXBENCH:OWN-DEV
   \ (1) file-backed mmap read directly: cold first, then steady (prefaulted)
   RP-HADDR RP-COLD   RP-COLD-NS !
   RP-HADDR RP-STEADY RP-HOST-NS !
   \ (2) mmap + cuMemHostRegister (device-mapped), steady
   RP-HADDR @ RP-BYTES >LEN RP-HR-DEVICEMAP CUDA:CU-MEM-HOST-REGISTER CUDA:RC0
   RP-DPTR RP-HADDR @ 0 CUDA:CU-MEM-HOST-GET-DEVICE-POINTER CUDA:RC0
   RP-DPTR RP-STEADY RP-REG-NS !
   RP-HADDR @ CUDA:CU-MEM-HOST-UNREGISTER CUDA:RC0
   \ (3) cuMemAlloc device buffer POPULATED ONCE from the file: copy cost, then steady
   mono-ns {: t0 :}
   RP-DIN @ RP-HADDR @ RP-N>PTR RP-BYTES PTXBENCH:HTOD
   mono-ns t0 - RP-COPY-NS !
   RP-DIN RP-STEADY RP-DEV-NS ! ;

: RP-.3 ( n -- )                   \ zero-padded 3-digit fraction
   dup 100 < if s" 0" type then
   dup 10 < if s" 0" type then
   FMT:.U ;

: RP-GBS ( n -- )                  \ ns for one 100 MiB x 50-iter burst -> "GB/s=x.yyy"
   {: ns :}
   RP-BYTES RP-ITERS * 1000 * ns / {: gbs :}
   s" GB/s=" type gbs 1000 / FMT:.U s" ." type gbs 1000 mod RP-.3 ;

: RP-STEADY-LINE ( ptr u8 n n -- )   \ label, steady ns
   {: la:ptr lu ns :}
   la lu type ns RP-GBS s"   (ns=" type ns FMT:.U s" )" type cr ;

: RP-ONESHOT ( ptr u8 n n -- )     \ label, one-time 100 MiB op ns -> effective GB/s + ns
   {: la:ptr lu ns :}
   la lu type
   RP-BYTES 1000 * ns / {: gbs :}
   s" GB/s=" type gbs 1000 / FMT:.U s" ." type gbs 1000 mod RP-.3
   s"   (ns=" type ns FMT:.U s" )" type cr ;

: RP-MAIN ( -- )
   CUDA:OPEN? 0= if
      s" residency-probe: libcuda unavailable -> SKIPPED (device-only measurement)" type cr exit
   then
   RP-BYTES 16 / RP-N !               \ float4 count
   RP-MMAP-HOST RP-HADDR !
   RP-SETUP
   PTXBENCH:OPEN
   PTXBENCH:LOAD
   [: RP-BODY ;] CUDA-SCOPE:SCOPE
   cr s" === GB10 UMA weight-residency: GPU read bandwidth, 100 MiB, 50-iter steady ===" type cr
   s" (1) mmap direct, steady (prefault): " RP-HOST-NS @ RP-STEADY-LINE
   s" (2) mmap + cuMemHostRegister:       " RP-REG-NS  @ RP-STEADY-LINE
   s" (3) cuMemAlloc device buffer:       " RP-DEV-NS  @ RP-STEADY-LINE
   cr s" --- one-time costs (per 100 MiB) ---" type cr
   s" (1) mmap direct, COLD first-touch:  " RP-COLD-NS @ RP-ONESHOT
   s" (3) HtoD populate device buffer:    " RP-COPY-NS @ RP-ONESHOT ;

RP-MAIN

;package
