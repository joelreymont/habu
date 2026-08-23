\ gpt2-tensor-emit-test.f - host smoke for the GPT-2 tensor PTX emitter.

require lib/test.f
require maki/infer/gpt2-tensor-cg.f

package GPT2-TENSOR-EMIT-TEST

T-RESET

32 %BLOCK
PTX-CAPTURE-ON
GPT2-PTX:EMIT
PTX-CAPTURE-OFF

PTX-CAPTURE$ nip 0 > TTRUE
PTX-BLOCK@ 256 T=

T-REPORT

;package
