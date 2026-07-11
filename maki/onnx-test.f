\ maki/onnx-test.f - ONNX op coverage lowers correctly and fails closed.
\ Reopens `package MAKI` for the op-kind FACTs (OP-RESHAPE ...) it asserts on;
\ the ONNX subsystem words are called across the package seam as ONNX:LOWER /
\ ONNX:MOVE-KIND (docs/forth.md "Qualify only across package boundaries").

require lib/test.f
require maki/onnx.f

package MAKI

T-RESET

\ supported ops lower to their maki/Habu-PTX entry
s" Add"     ONNX:LOWER s" ADD-F"        STR= TTRUE
s" Mul"     ONNX:LOWER s" MUL-F"        STR= TTRUE
s" Relu"    ONNX:LOWER s" RELU-F"       STR= TTRUE
s" Softmax" ONNX:LOWER s" SOFTMAX-ROWS" STR= TTRUE
s" Gemm"    ONNX:LOWER s" SAXPY"        STR= TTRUE

\ an unsupported op is REJECTED loudly (never silently approximated)
: BAD-ONNX ( -- )  s" Conv" ONNX:LOWER 2drop ;
' BAD-ONNX E-MK-ONNX TTHROWS

\ movement ops lower to their maki op-kind FACT (no kernel entry)
s" Reshape"   ONNX:MOVE-KIND OPKIND>N OP-RESHAPE   T=
s" Transpose" ONNX:MOVE-KIND OPKIND>N OP-TRANSPOSE T=
s" Slice"     ONNX:MOVE-KIND OPKIND>N OP-SLICE     T=
s" Concat"    ONNX:MOVE-KIND OPKIND>N OP-CONCAT    T=
s" Gather"    ONNX:MOVE-KIND OPKIND>N OP-GATHER    T=

\ a non-movement / unknown op is rejected by the movement lowering too
: BAD-MOVE ( -- )  s" Add"  ONNX:MOVE-KIND drop ;
: BAD-MOVE2 ( -- ) s" Conv" ONNX:MOVE-KIND drop ;
' BAD-MOVE  E-MK-ONNX TTHROWS
' BAD-MOVE2 E-MK-ONNX TTHROWS

T-REPORT

end-package
