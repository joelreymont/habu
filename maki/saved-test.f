\ maki/saved-test.f - checked tests for the save-vs-recompute decision (cad-9c).
\ The two byte-equivalent costs, the comparison, the matmul policy floor, the
\ model-input (not-recomputable) case, the calibration flop-byte ratio + its
\ default, the decision rows, and the fail-closed calibration parse.

require lib/test.f
require lib/string.f
require maki/cad.f
require maki/saved.f

package MAKI

\ ---- render containment helper ---------------------------------------------
variable SVT-VA  variable SVT-VU
: SVT-SAVE ( ptr u8 n -- )  SVT-VU ! SVT-VA ! ;
: SVT-IN ( ptr u8 n -- )  SVT-VA @ SVT-VU @ 2swap CONTAINS? TTRUE ;
: SVT-NODE-REF ( n -- MIR:operand-ref )  MIR-NODE-ID MIR-NODE-REF ;
: SVT-SLOT-REF ( n -- MIR:operand-ref )  MIR-SLOT-ID MIR-IN-REF ;

\ ---- fail-closed probe ------------------------------------------------------
: SVT-TRY-CALIB ( -- )  s" not-a-number" SV-FBR-PARSE drop ;

T-RESET

\ ---- flop-byte ratio: default when no calibration row, and the parser ---------
SAVED-FBR SAVED-FBR-DEFAULT T=
SAVED-FBR-DEFAULT 1 T=
s" 8" SV-FBR-PARSE 8 T=
s" 1" SV-FBR-PARSE 1 T=

\ ---- RELU GELU (2x4 f32): gelu saves relu's output; relu saves the model input --
\ n0 = relu(i0) ; n1 = gelu(n0). fbr = 1.
\   save(n0)      = 2 * 8elems * 4B = 64B
\   recompute(n0) = relu flops(1)*8 * 1  +  upstream i0 bytes(32) = 40B  -> RECOMPUTE
MODEL: RG ( x:2x4 -- y ) RELU GELU ;
0 SVT-NODE-REF SAVED-SAVE-COST      64 T=
0 SVT-NODE-REF SAVED-RECOMPUTE-COST 40 T=
0 SVT-NODE-REF false SAVED-DECIDE   SV-RECOMPUTE T=
\ relu's saved input is the model input i0 (ref -1): not recomputable -> SAVE
0 SVT-SLOT-REF false SAVED-DECIDE SV-SAVE T=
REPORT:NEW SAVED-INTO REPORT:RENDER SVT-SAVE
s" backward.recompute: n0 (save 64B > recompute 40B)" SVT-IN
s" backward.saved: i0 (model input; not recomputable)" SVT-IN

\ ---- SOFTMAX-ROW (4x8 f32): the adjoint needs the OUTPUT saved -----------------
\   save(n0)      = 2 * 32elems * 4B = 256B
\   recompute(n0) = softmax flops(5)*32 * 1  +  upstream i0 bytes(128) = 288B -> SAVE
MODEL: SMX ( x:4x8 -- y ) SOFTMAX-ROW ;
0 SVT-NODE-REF SAVED-SAVE-COST      256 T=
0 SVT-NODE-REF SAVED-RECOMPUTE-COST 288 T=
0 SVT-NODE-REF false SAVED-DECIDE   SV-SAVE T=
REPORT:NEW SAVED-INTO REPORT:RENDER SVT-SAVE
s" backward.saved: n0 (save 256B < recompute 288B)" SVT-IN

\ ---- MATMUL (2x3, 3x4): operands ALWAYS saved by the policy floor --------------
MODEL: MM ( x:2x3 w:3x4 -- y ) MATMUL ;
\ both operands are model inputs; the floor forces SAVE regardless of any comparison
0 SVT-SLOT-REF true SAVED-DECIDE SV-SAVE T=
1 SVT-SLOT-REF true SAVED-DECIDE SV-SAVE T=
REPORT:NEW SAVED-INTO REPORT:RENDER SVT-SAVE
s" backward.saved: i0 (matmul operand; policy floor)" SVT-IN
s" backward.saved: i1 (matmul operand; policy floor)" SVT-IN

\ ---- ADD (no save needed): the linear adjoint records nothing ------------------
MODEL: ADDM ( x:2x2 y:2x2 -- z ) ADD ;
REPORT:NEW SAVED-INTO REPORT:RENDER SVT-SAVE
s" backward.saved" SVT-VA @ SVT-VU @ 2swap CONTAINS? TFALSE   \ no save rows at all

\ ---- cad-9e: LINEAR saves every operand (matmul policy floor) ------------------
\ the linear adjoint reads x (dW) and w (dX); the bias-grad reads only the cotangent,
\ so b is over-saved by the matmul floor - conservative-correct (saving is always safe).
MODEL: LINM ( x:2x3 w:3x4 b:1x4 -- y ) LINEAR ;
REPORT:NEW SAVED-INTO REPORT:RENDER SVT-SAVE
s" backward.saved: i0 (matmul operand; policy floor)" SVT-IN
s" backward.saved: i2 (matmul operand; policy floor)" SVT-IN

\ ---- cad-9e: BIAS saves nothing (d-bias = OP-ROWSUM-BWD reads only the cotangent) --
MODEL: BIASM ( x:2x3 b:1x3 -- y ) BIAS ;
REPORT:NEW SAVED-INTO REPORT:RENDER SVT-SAVE
s" backward.saved" SVT-VA @ SVT-VU @ 2swap CONTAINS? TFALSE   \ SAVE-NONE: no save rows

\ ---- cad-9e: SCALE saves both operands (the 1x1 factor + the input) ------------
MODEL: SCS ( x:2x3 s:1x1 -- z ) SCALE ;
REPORT:NEW SAVED-INTO REPORT:RENDER SVT-SAVE
s" backward.saved: i0 (model input; not recomputable)" SVT-IN
s" backward.saved: i1 (model input; not recomputable)" SVT-IN

\ ---- fail closed: a non-numeric calibration value ------------------------------
' SVT-TRY-CALIB E-SV-CALIB TTHROWS

T-REPORT

;package
