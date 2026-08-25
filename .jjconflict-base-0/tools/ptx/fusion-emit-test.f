\ fusion-emit-test.f - HOST proof of the fusion self-emit half (no device, no ptxas).
\
\ tools/ptx/fusion-compare.f self-emits each v4 kernel's cubin via
\ tools/ptx/fusion-emit.f instead of loading a shared /tmp/*.cubin. This test runs
\ the EMIT half only: it emits each checked v4 producer to a PRIVATE per-run PTX and
\ asserts the emitted text is the right kernel body - fused RELU has scale+bias+clamp,
\ plain SAXPY has scale+bias but no clamp, plain RELU has clamp but no scale. The
\ ptxas-assemble + device launch/compare half is the on-device leg (fusion-compare
\ under CUDA:OPEN?). This test requires NO device FFI, so it is safe to run
\ in-process in the resident stdlib runner. Mirrors maki/gpu-emit-test.f.

require lib/test.f
require tools/ptx/fusion-emit.f

package PTXFE

: GET-EMIT ( ptr u8 n -- )               \ producer path -> private per-run root -> emit PTX
   {: prod:ptr produ:n :}
   s" habu-ptx-fusion-emit-test" PTXTC:PREPARE
   prod produ EMIT-KERNEL drop
   PTXTC:CLEAN ;

: HAS ( ptr u8 n -- )                    \ emitted PTX contains the token
   {: a:ptr u:n :}
   EMITTED$ a u CONTAINS? TTRUE ;

: HAS-NOT ( ptr u8 n -- )                \ emitted PTX omits the token
   {: a:ptr u:n :}
   EMITTED$ a u CONTAINS? 0= TTRUE ;

\ fail-closed regression: a MISSING producer makes the spawned bin/hb exit nonzero;
\ EMIT-GUARD surfaces the child stderr ("hb: cannot open ...") and throws E-PTX-EMIT,
\ so a missing/renamed producer can never yield a silent empty PTX. No device/ptxas
\ needed - the child fails during load, before any assemble.
: MISS-THROWS ( -- )
   s" habu-ptx-fusion-emit-missing" PTXTC:PREPARE
   [: s" tools/ptx/does-not-exist-cg.f" EMIT-KERNEL drop ;] E-PTX-EMIT TTHROWSQ
   PTXTC:CLEAN ;

T-RESET

\ fused RELU(a*x+y): scale (mul) + bias (add) + clamp (max ,0) all present
s" tools/ptx/fused-relu-cg.f" GET-EMIT
EMITTED$ nip 0 > TTRUE                    \ emit produced PTX bytes
s" .visible .entry SAXPY" HAS             \ the kernel entry
s" mul.rn.f32" HAS                        \ a*x
s" add.rn.f32" HAS                        \ + y
s" max.f32" HAS                           \ relu clamp
s" ERROR" HAS-NOT                         \ no emit-error marker leaked into the PTX

\ plain v4 SAXPY(a*x+y): scale + bias but NO relu clamp
s" tools/ptx/saxpy-v4-cg.f" GET-EMIT
s" mul.rn.f32" HAS
s" add.rn.f32" HAS
s" max.f32" HAS-NOT

\ plain v4 RELU(x): clamp only, no scale
s" tools/ptx/relu-v4-cg.f" GET-EMIT
s" max.f32" HAS
s" mul.rn.f32" HAS-NOT

\ fail-closed: a missing producer throws E-PTX-EMIT (child stderr surfaced), never an empty PTX
MISS-THROWS

T-REPORT
s" fusion-emit-test: ok" type cr

;package
