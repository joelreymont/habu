\ maki/onnx-test.f - ONNX op coverage lowers correctly and fails closed.

T-RESET

\ supported ops lower to their maki/Habu-PTX entry (MAKI: package namespace)
s" Add"     MAKI:ONNX-LOWER s" ADD-F"        STR= TTRUE
s" Mul"     MAKI:ONNX-LOWER s" MUL-F"        STR= TTRUE
s" Relu"    MAKI:ONNX-LOWER s" RELU-F"       STR= TTRUE
s" Softmax" MAKI:ONNX-LOWER s" SOFTMAX-ROWS" STR= TTRUE
s" Gemm"    MAKI:ONNX-LOWER s" SAXPY"        STR= TTRUE

\ an unsupported op is REJECTED loudly (never silently approximated)
: BAD-ONNX ( -- )  s" Conv" MAKI:ONNX-LOWER 2drop ;
' BAD-ONNX E-MK-ONNX TTHROWS

T-REPORT
