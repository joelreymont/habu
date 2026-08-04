\ cg-activation-test.f - direct-load smoke test for public activation emitters.

require lib/test.f
require lib/ptx/cg-activation.f

package PTX-ACT-TEST

using PTX-ACT

private

: CAPTURE-ADDC ( -- )
   PTX-CAPTURE-ON CG-RESET 1 0.5 EMIT-ADDC drop PTX-CAPTURE-OFF
   PTX-CAPTURE$ s" add.f32" CONTAINS? TTRUE ;

: CAPTURE-GELU ( -- )
   PTX-CAPTURE-ON CG-RESET 1 EMIT-GELU drop PTX-CAPTURE-OFF
   PTX-CAPTURE$ s" ex2.approx.f32" CONTAINS? TTRUE ;

: CAPTURE-SILU ( -- )
   PTX-CAPTURE-ON CG-RESET 1 EMIT-SILU drop PTX-CAPTURE-OFF
   PTX-CAPTURE$ s" div.rn.f32" CONTAINS? TTRUE ;

public

: RUN ( -- )
   T-RESET
   CAPTURE-ADDC
   CAPTURE-GELU
   CAPTURE-SILU
   T-REPORT ;

;using
;package

PTX-ACT-TEST:RUN
