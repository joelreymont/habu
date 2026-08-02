\ maki/gpu.f - run a maki tensor op on the GPU (the maki -> Habu-PTX lowering).
\
\ AXPY over float arrays: y[i] = a*x[i] + y[i], computed on the Orin via the
\ CHECKED SAXPY kernel (lib/ptx/...), with ARBITRARY float data marshalled through
\ F64>F32, and verified against the CPU. Fully checked Habu (no 0 set-check) via
\ the checked FFI (lib/ffi-abi.f) + F64>F32 (lib/ptx/cg.f). maki -> habu only.
\ Self-contained: SETUP emits the checked SAXPY kernel (tools/ptx/saxpy-cg.f) to a
\ PRIVATE per-run PTX under a toolchain root, ptxas-assembles it, and loads that
\ cubin - no shared /tmp/saxpy.cubin that could be stale/missing/wrong.

require lib/ptx/cuda-driver.f
require maki/cuda-run.f
require lib/float.f
require lib/fmt.f
require src/arch/ptx/emit.f
require lib/ptx/cg.f
require lib/ptx/sentinel.f
require lib/ptx/toolchain.f
require lib/engine-candidate.f
require maki/eval/active-target.f

\ package GPU owns the stateful launch machinery. The G- stem drops (GPU carries it:
\ GPU:SETUP / GPU:LAUNCH / GPU:SGD ...); the CUDA:/PTXSENT: bindings stay the driver's.
\ Launch state (GN/G*/host buffers) and the f32 pack/unpack helpers are private.
package GPU

private

4 constant GN                       \ vector length (demo)
create GPATH 64 allot
create GKN  32 allot
create GHX  32 allot                \ host x as packed f32 (GN*4 bytes)
create GHY  32 allot                \ host y as packed f32
variable GDEV variable GCTX variable GMOD variable GFUNC
variable GDX variable GDY variable GABITS variable GNVAR

create GEMIT-OUT $4000 allot        \ captured SAXPY PTX from the emit child
create GEMIT-ERR $1000 allot
create GQ-OUT $1000 allot            \ ptxas stdout/stderr capture
create GQ-ERR $1000 allot

\ pack/read a 32-bit value at element idx (4-byte stride, little-endian)
: F32! ( n ptr u8 n -- ) {: v buf idx :}
   idx 4 *  {: o :}
   v             $FF and  buf o     + c!
   v 8 rshift    $FF and  buf o 1 + + c!
   v 16 rshift   $FF and  buf o 2 + + c!
   v 24 rshift   $FF and  buf o 3 + + c! ;
: F32@ ( ptr u8 n -- n ) {: buf idx :}
   idx 4 *  {: o :}
   buf o     + c@
   buf o 1 + + c@  8  lshift or
   buf o 2 + + c@  16 lshift or
   buf o 3 + + c@  24 lshift or ;

\ spawn bin/hb to emit the checked SAXPY kernel to the private PTX; return PTX bytes
: EMIT-PTX ( -- n )
   PROC-ARGV-RESET
   s" --load"               >LEN PROC-ARGV+
   s" lib/errors.f"         >LEN PROC-ARGV+  s" lib/string.f"        >LEN PROC-ARGV+
   s" lib/float.f"          >LEN PROC-ARGV+  s" lib/fmt.f"           >LEN PROC-ARGV+
   s" src/arch/ptx/emit.f"  >LEN PROC-ARGV+  s" lib/ptx/cg.f"        >LEN PROC-ARGV+
   s" lib/ptx/header.f"     >LEN PROC-ARGV+  s" lib/ptx/cg-collective.f" >LEN PROC-ARGV+
   s" lib/ptx/tile.f"       >LEN PROC-ARGV+  s" lib/ptx/collective.f" >LEN PROC-ARGV+
   s" tools/ptx/saxpy-cg.f" >LEN PROC-ARGV+
   ENGINE-CANDIDATE:PATH$ >LEN  GEMIT-OUT $4000 >LEN  GEMIT-ERR $1000 >LEN  20000 >MS  RUN-ARGV-CAPTURE
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE 0 >RC ENDOF          \ clean exit -> rc 0
     err OF PCAP-FAILED:UNMAKE ENDOF                  \ nonzero child: (out err code) on stack
   ;MATCH
   {: outu:len erru:len rc:rc :}
   GEMIT-ERR erru LEN>N  rc RC>N  PTXTC:EMIT-GUARD          \ nonzero emit rc -> surface stderr, throw
   PTXTC:PTX$ GEMIT-OUT outu LEN>N WRITE-ALL  outu LEN>N ;

\ ptxas-assemble the private PTX to the private cubin (fail-closed on nonzero rc).
\ The arch comes from the probed active target, never a literal.
: ASSEMBLE-PTX ( -- )
   ATGT:LABEL$ PTXTC:TC-ARCH!
   GQ-OUT $1000 >LEN GQ-ERR $1000 >LEN PTXTC:ASSEMBLE
   PTXTC:ASM-REPORT 0= if exit then                        \ ptxas rc 0 -> assembled
   E-PTX-EMIT throw ;                                      \ else surface stderr (ASM-REPORT) + fail closed

\ self-emit + assemble SAXPY to a private per-run cubin (no shared /tmp)
: BUILD-CUBIN ( -- )
   s" habu-ptx-saxpy" PTXTC:PREPARE
   EMIT-PTX drop
   ASSEMBLE-PTX ;

\ acquire ctx + module into the module-level CUDA-SCOPE ledger (the open half of the
\ SETUP..RELEASE owner boundary). Every driver call goes through the injectable MKD
\ seam; ctx and module are transferred to the ledger the instant they are retained/
\ loaded, so a throw at any later acquisition unwinds the owned prefix (SETUP wraps).
: SETUP-CU ( -- )
   MKD:OPEN
   0 MKD:CUINIT CUDA:RC0
   GDEV 0 >IDX MKD:CUDEVICEGET CUDA:RC0
   GCTX GDEV @ >CUDA-DEV MKD:CUDEVICEPRIMARYCTXRETAIN CUDA:RC0
   GDEV @ >CUDA-DEV CUDA-SCOPE:OWN-PRIMARY-CTX
   GCTX @ >CUDA-CTX MKD:CUCTXSETCURRENT CUDA:RC0
   PTXTC:CUBIN$ GPATH FFI:CSTR
   GMOD GPATH MKD:CUMODULELOAD CUDA:RC0
   GMOD @ >CUDA-MOD CUDA-SCOPE:OWN-MODULE
   s" SAXPY" GKN FFI:CSTR
   GFUNC GMOD @ >CUDA-MOD GKN MKD:CUMODULEGETFUNCTION CUDA:RC0 ;

\ one launch's device buffers live in a per-call SCOPE frame: GDX/GDY are freed on
\ both return and throw, so repeated launches (gpu-train's epochs) never leak the
\ previous pair and a mid-launch throw releases them without touching ctx/module.
: LAUNCH-CORE ( -- )
   GN 4 *  {: bytes :}
   GDX bytes >LEN MKD:CUMEMALLOC CUDA:RC0  GDX @ >CUDA-DEVPTR CUDA-SCOPE:OWN-DEVPTR
   GDY bytes >LEN MKD:CUMEMALLOC CUDA:RC0  GDY @ >CUDA-DEVPTR CUDA-SCOPE:OWN-DEVPTR
   GDX @ >CUDA-DEVPTR GHX bytes >LEN MKD:CUMEMCPYHTOD CUDA:RC0
   GDY @ >CUDA-DEVPTR GHY bytes >LEN MKD:CUMEMCPYHTOD CUDA:RC0
   GFUNC @ >CUDA-FN 256 1 1 CUDA:CU-FUNC-SET-BLOCK-SHAPE CUDA:RC0
   GFUNC @ >CUDA-FN 24 >LEN CUDA:CU-PARAM-SET-SIZE CUDA:RC0
   GFUNC @ >CUDA-FN 0 >IDX  GDX 8 >LEN CUDA:CU-PARAM-SET-V CUDA:RC0
   GFUNC @ >CUDA-FN 8 >IDX  GDY 8 >LEN CUDA:CU-PARAM-SET-V CUDA:RC0
   GFUNC @ >CUDA-FN 16 >IDX GABITS 4 >LEN CUDA:CU-PARAM-SET-V CUDA:RC0
   GFUNC @ >CUDA-FN 20 >IDX GNVAR 4 >LEN CUDA:CU-PARAM-SET-V CUDA:RC0
   GFUNC @ >CUDA-FN 1 1 CUDA:CU-LAUNCH-GRID CUDA:RC0
   CUDA:CU-CTX-SYNCHRONIZE CUDA:RC0
   GHY bytes PTXSENT:FILL                                  \ poison before copy-back (y already on device)
   GHY GDY @ >CUDA-DEVPTR bytes >LEN MKD:CUMEMCPYDTOH CUDA:RC0 ;

\ atomic ctx+module acquire: a throw mid-acquisition unwinds the owned prefix
\ (primary error wins, cleanup retained) so the owner boundary is all-or-nothing.
: SETUP-OPEN ( -- )
   [: SETUP-CU ;] catch
   dup 0<> if [: CUDA-SCOPE:UNWIND ;] catch drop throw then
   drop ;

public

\ SETUP opens the ctx+module owner boundary (released by RELEASE = single UNWIND).
: SETUP ( -- )
   BUILD-CUBIN                                             \ self-emit -> private per-run cubin
   SETUP-OPEN ;

\ load element i of x and y from Habu floats into the host f32 buffers
: PUT ( r r n -- ) {: xv:r yv:r ix:n :}
   xv F64>F32 GHX ix F32!
   yv F64>F32 GHY ix F32! ;

: LAUNCH ( r -- )  {: a:r :}                        \ a = scalar; x,y already in GHX/GHY
   a F64>F32 GABITS !  GN GNVAR !                          \ stash into globals for the scope body
   [: LAUNCH-CORE ;] CUDA-SCOPE:SCOPE ;                    \ GDX/GDY owned + freed within this launch

: RELEASE ( -- )
   CUDA-SCOPE:UNWIND                                       \ release module then primary context (reverse of SETUP)
   PTXTC:CLEAN ;                                           \ remove the private per-run root

\ result element i (f32 bits) after the launch
: RESULT ( n -- n )  GHY swap F32@ PTXSENT:GUARD ;

\ tensor SGD step on the GPU: w[i] -= lr*g[i], lowered onto the SAXPY kernel
\ (a = -lr, x = grad, y = weight, so a*x+y = w - lr*g). Put grad as x and weight
\ as y via PUT, then SGD; RESULT i is the updated weight. Matches maki/array.f
\ T-SGD! on the f32-marshalled inputs - the optimizer step runs on device.
: SGD ( r -- )  fnegate LAUNCH ;

;package
