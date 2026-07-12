\ maki/onnx.f - ONNX op import: supported-op coverage + lowering, fail-closed.
\
\ The core of ONNX import: map each ONNX operator to the maki/Habu-PTX kernel it
\ lowers onto, and REJECT (fail-closed, never silently approximate) any op outside
\ the supported set - the policy docs/maki/onnx.md mandates. (Graph traversal and
\ protobuf decoding are the larger follow-up; this is the op-coverage table that
\ decides what a model can lower to.) maki -> habu only; load after lib/string.f.
\
\ Wrapped in `package ONNX`: the public words export as `ONNX:LOWER` /
\ `ONNX:MOVE-KIND`, so a bare `LOWER` does not resolve in the global/habu namespace -
\ the one-way maki<-habu seam enforced at the dictionary level (docs/forth.md
\ "Packages"). The E-MK-ONNX error code stays global (cross-cutting, like
\ lib/errors.f's E-* codes); the package body reaches it via the package's
\ global-fallback lookup. The op-kind FACTs (MAKI:OP-RESHAPE ...) are qualified
\ across the ONNX<-MAKI package seam.

require lib/string.f
require maki/op-kind.f

-5001 constant E-MK-ONNX   \ unsupported ONNX op (fail-closed import)

package ONNX
public

\ ONNX op name -> the maki/Habu-PTX entry it lowers onto.
: LOWER ( ptr u8 n -- ptr u8 n )
   2dup s" Add"     STR= if 2drop s" ADD-F"        exit then
   2dup s" Mul"     STR= if 2drop s" MUL-F"        exit then
   2dup s" Relu"    STR= if 2drop s" RELU-F"       exit then
   2dup s" Softmax" STR= if 2drop s" SOFTMAX-ROWS" exit then
   2dup s" Gemm"    STR= if 2drop s" SAXPY"        exit then   \ affine y = a*x + b
   E-MK-ONNX throw ;

\ Movement ONNX ops carry no kernel; they lower to a maki movement op-kind
\ (the IR layout FACT the planner reasons over). Fail closed on any other op.
: MOVE-KIND ( ptr u8 n -- n )
   2dup s" Reshape"   STR= if 2drop MAKI:OP-RESHAPE   exit then
   2dup s" Transpose" STR= if 2drop MAKI:OP-TRANSPOSE exit then
   2dup s" Slice"     STR= if 2drop MAKI:OP-SLICE     exit then
   2dup s" Concat"    STR= if 2drop MAKI:OP-CONCAT    exit then
   2dup s" Gather"    STR= if 2drop MAKI:OP-GATHER    exit then
   2drop E-MK-ONNX throw ;

;package
