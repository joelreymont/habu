\ maki/onnx.f - ONNX op import: supported-op coverage + lowering, fail-closed.
\
\ The core of ONNX import: map each ONNX operator to the maki/Habu-PTX kernel it
\ lowers onto, and REJECT (fail-closed, never silently approximate) any op outside
\ the supported set - the policy docs/maki/onnx.md mandates. (Graph traversal and
\ protobuf decoding are the larger follow-up; this is the op-coverage table that
\ decides what a model can lower to.) maki -> habu only; load after lib/string.f.

-5001 constant E-MK-ONNX   \ unsupported ONNX op (fail-closed import)

\ ONNX op name -> the maki/Habu-PTX entry it lowers onto. Defined in the `MK` wordlist
\ (maki namespace): callers use `MK:ONNX-LOWER`; a bare `ONNX-LOWER` does not resolve in
\ the global namespace, so maki words cannot collide with or be reached from habu core.
: MK:ONNX-LOWER ( ptr u8 n -- ptr u8 n )
   2dup s" Add"     STR= if 2drop s" ADD-F"        exit then
   2dup s" Mul"     STR= if 2drop s" MUL-F"        exit then
   2dup s" Relu"    STR= if 2drop s" RELU-F"       exit then
   2dup s" Softmax" STR= if 2drop s" SOFTMAX-ROWS" exit then
   2dup s" Gemm"    STR= if 2drop s" SAXPY"        exit then   \ affine y = a*x + b
   E-MK-ONNX throw ;
