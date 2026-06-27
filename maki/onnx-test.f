\ maki/onnx-test.f - ONNX op coverage lowers correctly and fails closed.

T-RESET

\ supported ops lower to their maki/Habu-PTX entry (MK:-qualified maki namespace)
s" Add"     MK:ONNX-LOWER s" ADD-F"        STR= TTRUE
s" Mul"     MK:ONNX-LOWER s" MUL-F"        STR= TTRUE
s" Relu"    MK:ONNX-LOWER s" RELU-F"       STR= TTRUE
s" Softmax" MK:ONNX-LOWER s" SOFTMAX-ROWS" STR= TTRUE
s" Gemm"    MK:ONNX-LOWER s" SAXPY"        STR= TTRUE

\ an unsupported op is REJECTED loudly (never silently approximated)
: BAD-ONNX ( -- )  s" Conv" MK:ONNX-LOWER 2drop ;
' BAD-ONNX E-MK-ONNX TTHROWS

T-REPORT
