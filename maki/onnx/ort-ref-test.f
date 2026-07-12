\ maki/onnx/ort-ref-test.f - onnxruntime golden parity for the ONNX importer
\ (dots habu-maki-onnx-graph + habu-maki-onnx-import residue: external ref).
\
\ Two claims, both against the REAL exported artifact committed in
\ maki/onnx/ort-ref-data.f:
\   1. byte identity - the encode.f DSL fixture ORF-MODEL reproduces the
\      onnx-1.22.0-exported ModelProto byte-for-byte (ORF-REF$), so the
\      hand-encoded fixture IS the external artifact, not a lookalike;
\   2. runtime parity - ONNX:IMPORT of those exported bytes, host-executed
\      by EX-RUN on the committed input, matches onnxruntime 1.27.0's output
\      elementwise within ORF-TOL.
\
\ Tolerance: onnxruntime computes in f32, the host executor in f64 over the
\ SAME exact values (weights/inputs are 2^-10-grid binary fractions, exact in
\ both widths), so the gap is pure f32 rounding across the Gemm accumulation
\ chain (K<=8, values O(1)): measured 1.24e-7 max abs, bounded by a few f32
\ eps (1.19e-7). ORF-TOL = 1e-5 keeps ~80x margin and still fails on any
\ wrong element, op, or layout (those show >=1e-2 here). Load via maki/test.f.

require lib/test.f
require lib/string.f
require lib/float.f
require maki/onnx/ort-ref-data.f
require maki/onnx/import.f

package ONNX-ORT-TEST

: ORF-TOL ( -- r )  0.00001 ;   \ see header: f32-vs-f64 rounding floor is 1.24e-7

: ORF-CLOSE? ( r r -- bool )  f- fabs  ORF-TOL f< ;

: ORF-OUT@ ( n -- r )  ONNX:OUT-NODE@ MAKI:EX-OUT@ swap T-GET ;

T-RESET

\ ---- byte identity: the DSL fixture IS the exported artifact -------------------
ORF-MODEL
ONNX:ENC$ ORF-REF$ STR= TTRUE

\ ---- import the REAL exported bytes; structure facts ---------------------------
ORF-REF$ ONNX:IMPORT
MAKI:MIR-NAME$ s" EXTMLP" STR= TTRUE
MAKI:MIR-N@ 3 T=                               \ Gemm->linear, Relu, Gemm->linear
0 MAKI:MIR-OP@ MAKI:OPKIND>N MAKI:OP-LINEAR T=
1 MAKI:MIR-OP@ MAKI:OPKIND>N MAKI:OP-RELU T=
2 MAKI:MIR-OP@ MAKI:OPKIND>N MAKI:OP-LINEAR T=
ONNX:IN# 1 T=
0 ONNX:IN-NAME$ s" x" STR= TTRUE
ONNX:INIT# 4 T=
ONNX:OUT-NODE@ 2 T=
2 MAKI:MIR-ROWS@ ORF-BATCH T=  2 MAKI:MIR-COLS@ ORF-OUT T=

\ ---- run on the committed input; elementwise parity with onnxruntime -----------
MAKI:EX-RESET  ONNX:BIND-INITS
ORF-X 0 ONNX:IN-SLOT@ MAKI:EX-BIND
MAKI:EX-RUN
0 ORF-OUT@  ORF-Y 0 T-GET  ORF-CLOSE? TTRUE
1 ORF-OUT@  ORF-Y 1 T-GET  ORF-CLOSE? TTRUE
2 ORF-OUT@  ORF-Y 2 T-GET  ORF-CLOSE? TTRUE
3 ORF-OUT@  ORF-Y 3 T-GET  ORF-CLOSE? TTRUE

T-REPORT

;package
