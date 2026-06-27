\ maki/onnx.f - ONNX op import: supported-op coverage + lowering, fail-closed.
\
\ The core of ONNX import: map each ONNX operator to the maki/Habu-PTX kernel it
\ lowers onto, and REJECT (fail-closed, never silently approximate) any op outside
\ the supported set - the policy docs/maki/onnx.md mandates. (Graph traversal and
\ protobuf decoding are the larger follow-up; this is the op-coverage table that
\ decides what a model can lower to.) maki -> habu only; load after lib/string.f.
\
\ Wrapped in `package MAKI`: the public word exports as `MAKI:ONNX-LOWER`, so a bare
\ `ONNX-LOWER` does not resolve in the global/habu namespace - the one-way maki<-habu
\ seam enforced at the dictionary level (docs/forth.md "Packages"). The E-MK-ONNX error
\ code stays global (cross-cutting, like lib/errors.f's E-* codes); the package body
\ reaches it via the package's global-fallback lookup.

-5001 constant E-MK-ONNX   \ unsupported ONNX op (fail-closed import)

package MAKI
public

\ ONNX op name -> the maki/Habu-PTX entry it lowers onto.
: ONNX-LOWER ( ptr u8 n -- ptr u8 n )
   2dup s" Add"     STR= if 2drop s" ADD-F"        exit then
   2dup s" Mul"     STR= if 2drop s" MUL-F"        exit then
   2dup s" Relu"    STR= if 2drop s" RELU-F"       exit then
   2dup s" Softmax" STR= if 2drop s" SOFTMAX-ROWS" exit then
   2dup s" Gemm"    STR= if 2drop s" SAXPY"        exit then   \ affine y = a*x + b
   E-MK-ONNX throw ;

end-package
