\ asm-collide-test.f - ONNX's own ENC-B beside the packaged ARM64 assembler.
\
\ maki/onnx/encode.f publishes a public `ENC-B ( n -- )`, the protobuf byte
\ appender. src/arch/arm64/asm.f publishes `ENC-B ( n -- n )`, the unconditional
\ branch encoder. While the assembler's names were GLOBAL the two could not be in
\ one image with ONNX imported: a bare `enc-b` under `using ONNX` was refused
\ with E-USING-SHADOW-GLOBAL (checker code 7141, "the global 'enc-b' (1 -- 1) and
\ used public 'onnx:enc-b' (1 -- 0) export the same name"). That refusal is the
\ checker working, and it is why the assembler could not join the boot prefix.
\
\ The assembler is package A64ASM now, so it owns that tail instead of everyone
\ owning it. This file is the inversion measured against the REAL encoder rather
\ than a stand-in: it loads both and uses both. It lives here because the
\ habu<-maki dependency guard lets maki name a src/ path and forbids the reverse;
\ test/compiler/asm-package-test.f measures the assembler's own boundary from the
\ habu side. The load itself is half the proof - a returned global ENC-B would
\ end this file at the `using ONNX` below with exit 67 - and the cases then pin
\ which word each name reaches.

require lib/test.f
require lib/string.f
require src/arch/arm64/asm.f
require maki/onnx/encode.f

package ONNX-ASM-COLLIDE-TEST

using ONNX

\ The unconditional branch with a zero displacement: what A64ASM:ENC-B answers,
\ and nothing the protobuf appender could produce.
$14000000 constant B-ZERO

public

: RUN ( -- )
   \ ONNX's public appender, bare, under the import that used to be refused.
   ENC-RESET  $2A ENC-B  $7F ENC-B
   ENC$ drop c@ $2A T=
   \ The assembler's encoder, qualified, in the same image and the same word.
   0 A64ASM:ENC-B B-ZERO T=
   \ The two really are different words: ONNX's took a byte and left nothing, so
   \ the buffer grew by exactly the two bytes appended above.
   ENC$ nip 2 T= ;

;using

;package

T-RESET
ONNX-ASM-COLLIDE-TEST:RUN
T-REPORT
