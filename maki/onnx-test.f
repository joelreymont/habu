\ maki/onnx-test.f - ONNX op coverage lowers correctly and fails closed.

T-RESET

\ supported ops lower to their maki/Habu-PTX entry
s" Add"     ONNX-LOWER s" ADD-F"        STR= TTRUE
s" Mul"     ONNX-LOWER s" MUL-F"        STR= TTRUE
s" Relu"    ONNX-LOWER s" RELU-F"       STR= TTRUE
s" Softmax" ONNX-LOWER s" SOFTMAX-ROWS" STR= TTRUE
s" Gemm"    ONNX-LOWER s" SAXPY"        STR= TTRUE

\ an unsupported op is REJECTED loudly (never silently approximated)
: BAD-ONNX ( -- )  s" Conv" ONNX-LOWER 2drop ;
' BAD-ONNX E-MK-ONNX TTHROWS

T-REPORT
