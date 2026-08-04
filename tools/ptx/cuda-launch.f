\ cuda-launch.f - CHECKED on-device proof: LAUNCH a checked-emitted scalar SAXPY
\ kernel on the Orin GPU and verify the result against the CPU golden.
\
\ Fully checked Habu via lib/ffi-abi.f (NO 0 set-check; only FFI:>CELL trusted). The
\ deprecated <=8-arg launch API (cuFuncSetBlockShape / cuParamSetv / cuLaunchGrid)
\ avoids cuLaunchKernel's 11 args; the real driver memory entry points are the
\ _v2 symbols (the earlier INVALID_CONTEXT was symbol versioning). Self-contained:
\ the cubin is SELF-EMITTED - a spawned bin/hb emits the checked SAXPY producer
\ (tools/ptx/saxpy-cg.f) to a PRIVATE per-run PTXTC PTX, ptxas-assembles it, and
\ this launches that private cubin. A missing producer or a nonzero emit/ptxas rc
\ fails CLOSED with the named E-PTX-EMIT throw (child stderr surfaced), never a
\ stale/absent /tmp/saxpy.cubin. A dropped copy-back fails closed via PTXSENT.
\ Data: x=2.0, y=0, n=4, ARBITRARY a marshalled through F32:NARROW (lib/float32.f)
\ => y' = a*x+y = 2a. The kernel name, block shape, cuParamSetSize total, and
\ every cuParamSetV offset/size are GENERATED from the kernel-ABI record
\ (lib/ptx/kernel-abi.f) - the same record that renders the kernel's entry and
\ param loads - and LL-ABI-CHECK pins them to the old hand literals. The
\ host-side marshalling + ABI assertions run unconditionally; the device leg
\ (emit+ptxas+launch) is SKIPPED off-Orin (libcuda absent).

require lib/errors.f
require lib/string.f
require lib/float.f
require lib/float32.f
require lib/fmt.f
require src/arch/ptx/emit.f
require lib/ptx/kernel-abi.f
require lib/ptx/cg.f
require lib/ffi-abi.f
require lib/ptx/toolchain.f
require lib/ptx/sentinel.f
require lib/ptx/cuda-driver.f
require lib/ptx/cuda-scope.f
require lib/test.f
require maki/eval/active-target.f

package CUDA-LAUNCH-TEST

private

create LL-PATH 64 allot
create LL-KN   32 allot          \ kernel name
create LL-OUT $8000 allot  create LL-ERR $1000 allot   \ spawned emit capture
create LL-QO  $1000 allot  create LL-QE  $1000 allot   \ ptxas capture
variable LL-DEV  variable LL-CTX  variable LL-MOD  variable LL-FUNC
variable LL-DX variable LL-DY variable LL-ABITS variable LL-NV variable LL-RBUF

\ missing producer / nonzero emit rc -> surface child stderr + throw; else write PTX, return bytes
: LL-EMIT-WRITE ( len len rc -- n ) {: o:len e:len c:rc :}
   LL-ERR e LEN>N  c RC>N  PTXTC:EMIT-GUARD
   PTXTC:PTX$ LL-OUT o LEN>N WRITE-ALL  o LEN>N ;

\ spawn bin/hb to emit the checked scalar SAXPY kernel to the private PTX
: LL-EMIT ( -- n )
   PROC-ARGV-RESET
   s" --load"               >LEN PROC-ARGV+
   s" lib/errors.f"         >LEN PROC-ARGV+  s" lib/string.f"  >LEN PROC-ARGV+
   s" lib/float.f"          >LEN PROC-ARGV+  s" lib/fmt.f"     >LEN PROC-ARGV+
   s" src/arch/ptx/emit.f"  >LEN PROC-ARGV+  s" lib/ptx/cg.f"  >LEN PROC-ARGV+
   s" lib/ptx/header.f"     >LEN PROC-ARGV+  s" lib/ptx/tile.f" >LEN PROC-ARGV+
   s" tools/ptx/saxpy-cg.f" >LEN PROC-ARGV+
   s" bin/hb" >LEN  LL-OUT $8000 >LEN  LL-ERR $1000 >LEN  20000 >MS  RUN-ARGV-CAPTURE
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE 0 >RC LL-EMIT-WRITE ENDOF
     err OF PCAP-FAILED:UNMAKE LL-EMIT-WRITE ENDOF
   ;MATCH ;

: LL-PTXAS ( -- n )
   ATGT:LABEL$ PTXTC:TC-ARCH!                        \ assembler arch from the probed active target
   LL-QO $1000 >LEN LL-QE $1000 >LEN PTXTC:ASSEMBLE ;

: LL-SETUP ( -- )                                 \ ctx + module (private cubin) + function
   CUDA:OPEN
   0 CUDA:CU-INIT CUDA:RC0
   LL-DEV 0 >IDX CUDA:CU-DEVICE-GET CUDA:RC0
   LL-CTX LL-DEV @ >CUDA-DEV CUDA:CU-DEVICE-PRIMARY-CTX-RETAIN CUDA:RC0
   LL-DEV @ >CUDA-DEV CUDA-SCOPE:OWN-PRIMARY-CTX
   LL-CTX @ >CUDA-CTX CUDA:CU-CTX-SET-CURRENT CUDA:RC0
   PTXTC:CUBIN$ LL-PATH FFI:CSTR
   LL-MOD LL-PATH CUDA:CU-MODULE-LOAD CUDA:RC0
   LL-MOD @ >CUDA-MOD CUDA-SCOPE:OWN-MODULE
   KABI:NAME$ LL-KN FFI:CSTR                         \ kernel entry name from the ABI record
   LL-FUNC LL-MOD @ >CUDA-MOD LL-KN CUDA:CU-MODULE-GET-FUNCTION CUDA:RC0 ;

\ record-driven .param packing: offset and size of one named field
: LL-POFF ( ptr u8 n -- idx )
   KABI:OFFSET-OF >IDX ;
: LL-PLEN ( ptr u8 n -- len )
   KABI:SIZE-OF >LEN ;

: LL-LAUNCH ( r -- )  {: a:r :}                   \ marshal a, alloc, launch, copy back, free
   LL-RBUF 4 PTXSENT:FILL                                                  \ poison readback: dropped copy-back fails closed
   LL-DX 16 >LEN CUDA:CU-MEM-ALLOC CUDA:RC0  LL-DX @ >CUDA-DEVPTR CUDA-SCOPE:OWN-DEVPTR
   LL-DY 16 >LEN CUDA:CU-MEM-ALLOC CUDA:RC0  LL-DY @ >CUDA-DEVPTR CUDA-SCOPE:OWN-DEVPTR
   LL-DX @ >CUDA-DEVPTR 2.0 F32:NARROW 4 >COUNT CUDA:CU-MEMSET-D32 CUDA:RC0   \ x = 2.0
   LL-DY @ >CUDA-DEVPTR 0 4 >COUNT CUDA:CU-MEMSET-D32 CUDA:RC0             \ y = 0
   a F32:NARROW LL-ABITS !  4 LL-NV !                                        \ arbitrary a, n = 4
   LL-FUNC @ >CUDA-FN KABI:BLOCK@ 1 1 CUDA:CU-FUNC-SET-BLOCK-SHAPE CUDA:RC0
   LL-FUNC @ >CUDA-FN KABI:TOTAL >LEN CUDA:CU-PARAM-SET-SIZE CUDA:RC0
   LL-FUNC @ >CUDA-FN s" x" LL-POFF LL-DX s" x" LL-PLEN CUDA:CU-PARAM-SET-V CUDA:RC0
   LL-FUNC @ >CUDA-FN s" y" LL-POFF LL-DY s" y" LL-PLEN CUDA:CU-PARAM-SET-V CUDA:RC0
   LL-FUNC @ >CUDA-FN s" a" LL-POFF LL-ABITS s" a" LL-PLEN CUDA:CU-PARAM-SET-V CUDA:RC0
   LL-FUNC @ >CUDA-FN s" n" LL-POFF LL-NV s" n" LL-PLEN CUDA:CU-PARAM-SET-V CUDA:RC0
   LL-FUNC @ >CUDA-FN 1 1 CUDA:CU-LAUNCH-GRID CUDA:RC0
   CUDA:CU-CTX-SYNCHRONIZE CUDA:RC0
   LL-RBUF LL-DY @ >CUDA-DEVPTR 4 >LEN CUDA:DTOH ;   \ DX/DY owned by the run scope; freed on unwind

: SAXPY-GPU-BITS ( -- n )  LL-RBUF @ $FFFFFFFF and PTXSENT:GUARD ;   \ read-back f32 bits (fail-closed on dropped copy-back)

\ host-side marshalling proof (runs off-device too): 3.0 still narrows to the old
\ hardcoded 0x40400000 (no behaviour change), 1.7 is an arbitrary scalar, and the
\ two CPU goldens (a*x = 2a for a in {3.0, 1.7}) are the f32 bits the device must
\ return.
: HOST-CHECK ( -- )
   3.0 F32:NARROW $40400000 T=            \ a=3.0 marshals to the previously hardcoded bits
   1.7 F32:NARROW $3FD9999A T=            \ a=1.7 arbitrary scalar marshals correctly
   3.0 2.0 f* F32:NARROW $40C00000 T=     \ a=3.0 CPU golden 6.0
   1.7 2.0 f* F32:NARROW $4059999A T= ;   \ a=1.7 CPU golden 3.4

\ pinned: the record-generated launch packing equals the old hand literals
\ (name SAXPY, block 256,1,1, cuParamSetSize 24, offsets 0/8/16/20, sizes 8/8/4/4)
: LL-ABI-CHECK ( -- )
   KABI:NAME$ s" SAXPY" T$=
   KABI:BLOCK@ 256 T=
   KABI:TOTAL 24 T=
   s" x" KABI:OFFSET-OF 0 T=    s" x" KABI:SIZE-OF 8 T=
   s" y" KABI:OFFSET-OF 8 T=    s" y" KABI:SIZE-OF 8 T=
   s" a" KABI:OFFSET-OF 16 T=   s" a" KABI:SIZE-OF 4 T=
   s" n" KABI:OFFSET-OF 20 T=   s" n" KABI:SIZE-OF 4 T= ;

: SAXPY-CHECK ( r -- )  {: a:r :}       \ device result == CPU golden f32(a*x+y), x=2 y=0
   SAXPY-GPU-BITS  a 2.0 f* F32:NARROW  T=
   s" SAXPY a*2 on GPU -> f32 bits " type SAXPY-GPU-BITS . cr ;

: RUN ( -- )
   T-RESET
   HOST-CHECK
   LL-ABI-CHECK
   CUDA:OPEN? 0= if
      s" cuda-launch: libcuda unavailable -> device leg SKIPPED (host marshalling proven)" type cr
      T-REPORT exit
   then
   s" habu-ptx-cuda-launch" PTXTC:PREPARE
   LL-EMIT drop                         \ self-emit the checked scalar SAXPY PTX (fail-closed)
   LL-PTXAS PTXTC:ASM-REPORT 0 T=       \ ptxas rc 0 (stderr surfaced on failure)
   [: LL-SETUP                          \ acquire+own ctx/module; per-launch buffers owned in the same frame
      3.0 LL-LAUNCH  3.0 SAXPY-CHECK    \ regression: a=3.0 golden 0x40C00000
      1.7 LL-LAUNCH  1.7 SAXPY-CHECK    \ arbitrary: a=1.7 golden 0x4059999A
   ;] CUDA-SCOPE:SCOPE
   PTXTC:CLEAN
   T-REPORT ;

RUN

;package
