\ fusion-emit.f - self-emit the checked v4 SAXPY-family kernels to PRIVATE cubins.
\
\ The fusion benchmark (tools/ptx/fusion-compare.f) must not load shared
\ /tmp/*.cubin that can be stale/missing/wrong. Each kernel's cubin is built HERE:
\ the checked v4 producer (tools/ptx/{saxpy-v4,relu-v4,fused-relu}-cg.f) is emitted
\ to a PRIVATE per-run PTX under a toolchain root by a spawned bin/hb, then
\ ptxas-assembled to a private cubin. A missing producer or a nonzero emit/ptxas rc
\ fails CLOSED with the named E-PTX-EMIT throw (the child stderr is surfaced first),
\ never a silent or empty artifact. Emit needs no device and no ptxas, so the host
\ proves that half (tools/ptx/fusion-emit-test.f); assemble needs the CUDA toolchain
\ (ptxas), so a caller runs BUILD-KERNEL only where ptxas is present (the device
\ leg). This file requires NO device FFI, so it stays safe to load in-process.

require lib/errors.f
require lib/string.f
require lib/fs.f
require lib/process-argv.f
require lib/ptx/toolchain.f

package PTXFE

$8000 constant PTX-CAP                   \ captured kernel PTX
$1000 constant MSG-CAP                   \ emit / ptxas stderr capture
30000 constant EMIT-TIMEOUT-MS           \ spawn bin/hb + emit one v4 kernel

create PTX-OUT PTX-CAP allot
create EMIT-ERR MSG-CAP allot
create ASM-OUT MSG-CAP allot
create ASM-ERR MSG-CAP allot

variable PTX-U                           \ bytes of the last emitted PTX

private

: EMIT-PRELUDE ( -- )                    \ shared checked v4 codegen load list
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   s" lib/errors.f" >LEN PROC-ARGV+  s" lib/string.f" >LEN PROC-ARGV+
   s" lib/float.f"  >LEN PROC-ARGV+  s" lib/fmt.f"    >LEN PROC-ARGV+
   s" src/arch/ptx/emit.f" >LEN PROC-ARGV+  s" lib/ptx/cg.f" >LEN PROC-ARGV+
   s" lib/ptx/header.f" >LEN PROC-ARGV+  s" lib/ptx/cg-collective.f" >LEN PROC-ARGV+
   s" lib/ptx/cg-vec.f" >LEN PROC-ARGV+  s" lib/ptx/tile.f" >LEN PROC-ARGV+
   s" lib/ptx/tile-v4.f" >LEN PROC-ARGV+ ;

public

: EMIT-KERNEL ( ptr u8 n -- n )          \ producer path -> emit checked PTX to private PTX; return bytes
   {: prod:ptr produ:n :}
   EMIT-PRELUDE
   prod produ >LEN PROC-ARGV+
   s" bin/hb" >LEN  PTX-OUT PTX-CAP >LEN  EMIT-ERR MSG-CAP >LEN  EMIT-TIMEOUT-MS >MS  RUN-ARGV-CAPTURE
   {: outu:len erru:len rc:rc :}
   EMIT-ERR erru LEN>N  rc RC>N  PTXTC:EMIT-GUARD         \ missing producer / nonzero emit rc -> stderr + throw
   outu LEN>N PTX-U !
   PTXTC:PTX$ PTX-OUT PTX-U @ WRITE-ALL
   PTX-U @ ;

: EMITTED$ ( -- ptr u8 n )               \ the last emitted PTX bytes (for the host emit proof)
   PTX-OUT PTX-U @ ;

: ASSEMBLE-KERNEL ( -- )                 \ ptxas-assemble the private PTX fail-closed (needs ptxas)
   ASM-OUT MSG-CAP >LEN ASM-ERR MSG-CAP >LEN PTXTC:ASSEMBLE
   PTXTC:ASM-REPORT 0= if exit then                      \ ptxas rc 0 -> assembled
   E-PTX-EMIT throw ;                                     \ else surface stderr (ASM-REPORT) + fail closed

: BUILD-KERNEL ( ptr u8 n ptr u8 n -- )  \ root-name, producer path -> private per-run cubin at PTXTC:CUBIN$
   {: name:ptr nameu:n prod:ptr produ:n :}
   name nameu PTXTC:PREPARE
   prod produ EMIT-KERNEL drop
   ASSEMBLE-KERNEL ;

;package
