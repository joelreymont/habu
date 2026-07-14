\ maki/gpu-emit-test.f - HOST proof of GPU:SETUP's self-emit half.
\
\ GPU:SETUP now self-emits the checked SAXPY kernel to a PRIVATE per-run cubin
\ instead of loading a shared /tmp/saxpy.cubin. This test runs the EMIT half only
\ (no device, no ptxas): it reopens package GPU to reach the private EMIT-PTX and
\ its capture buffer, then asserts the emitted text is the SAXPY kernel PTX. The
\ assemble + launch half is the Orin device gate (maki/gpu-test.f).

require lib/test.f
require maki/gpu.f

package GPU

variable GET-U                          \ emitted SAXPY PTX byte count

: GET-EMIT ( -- )                       \ private per-run root -> emit SAXPY PTX
   s" habu-ptx-saxpy-emit-test" PTXTC:PREPARE
   EMIT-PTX GET-U !
   PTXTC:CLEAN ;

: GET-HAS ( ptr u8 n -- )               \ emitted PTX contains the token
   {: a:ptr u:n :}
   GEMIT-OUT GET-U @ a u CONTAINS? TTRUE ;

: GET-HAS-NOT ( ptr u8 n -- )           \ emitted PTX omits the token
   {: a:ptr u:n :}
   GEMIT-OUT GET-U @ a u CONTAINS? 0= TTRUE ;

T-RESET
GET-EMIT
GET-U @ 0 > TTRUE                        \ emit produced PTX bytes
s" .visible .entry SAXPY" GET-HAS        \ the SAXPY kernel entry
s" mul.rn.f32" GET-HAS                   \ a*x
s" add.rn.f32" GET-HAS                   \ + y
s" st.global.f32" GET-HAS                \ store y
s" ERROR" GET-HAS-NOT                    \ no emit-error marker leaked into the PTX
T-REPORT
s" gpu-emit-test: ok" type cr

;package
