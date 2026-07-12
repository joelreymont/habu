\ maki/onnx/ort-ref-data.f - committed onnxruntime golden fixture (external
\ reference data for the ONNX importer; dots habu-maki-onnx-graph +
\ habu-maki-onnx-import residue).
\
\ Provenance: onnx 1.22.0 helper built a REAL 2-layer f32 MLP ModelProto
\ (Gemm(x,w1,b1) -> Relu -> Gemm(a,w2,b2); x 2x4, w1 4x8, b1 [8], w2 8x2,
\ b2 [2], y 2x2 - the 2D f32 Gemm+Relu envelope maki/onnx/deploy-test.f
\ MODEL-A exercises), ir_version 8, default-domain opset 13, initializers as
\ raw_data; onnxruntime 1.27.0 (CPUExecutionProvider) ran it on the committed
\ input. ORF-REF$ holds those exported bytes EXACTLY (hex rows below, decoded
\ fail-closed at load); ORF-MODEL re-encodes the same ModelProto through the
\ maki/onnx/encode.f DSL, and maki/onnx/ort-ref-test.f proves the two
\ byte-identical - the repo convention stays "no binary blob files" without
\ weakening the fixture's authenticity. Weights/inputs are multiples of 2^-10
\ (numpy default_rng(20260712) grid): exact in f32, exact in the decimal
\ literals below, so both engines consume identical values. ORF-Y$ is ORT's
\ f32 output widened to its exact f64 decimals. ort-ref owns -5280..-5284.

require lib/prelude.f
require lib/string.f
require lib/float.f
require src/arch/ptx/emit.f
require maki/onnx/encode.f
require maki/onnx/proto.f
require maki/array.f

-5280 constant E-ORT-HEX    \ non-hex byte or odd digit count in a hex row
-5281 constant E-ORT-DATA   \ reference byte table over/underflow

package ONNX-ORT-TEST

\ ---- shapes (the fixture's committed dimensions) ------------------------------
2 constant ORF-BATCH
4 constant ORF-IN
8 constant ORF-HID
2 constant ORF-OUT
8 constant ORF-XN           \ ORF-BATCH * ORF-IN
4 constant ORF-YN           \ ORF-BATCH * ORF-OUT

\ ---- the exported ModelProto bytes (hex rows -> fail-closed byte table) --------
405 constant ORF-REF-LEN
create ORF-REF ORF-REF-LEN allot
variable ORF-REF-U

97 constant ORF-CH-A        \ lowercase hex rows only (the generator emits a-f)

: ORF-HEX-NIB ( n -- n ) {: c:n :}
   c STR-ZERO >=  c STR-ZERO 10 + <  and if c STR-ZERO - exit then
   c ORF-CH-A  >=  c ORF-CH-A 6 +  <  and if c ORF-CH-A - 10 + exit then
   E-ORT-HEX throw ;

: ORF-BYTE+ ( n -- )
   ORF-REF-U @ ORF-REF-LEN >= if E-ORT-DATA throw then
   ORF-REF ORF-REF-U @ + c!
   ORF-REF-U @ 1+ ORF-REF-U ! ;

: ORF-HEX+ ( ptr u8 n -- ) {: a:ptr u:n :}
   u 2 mod 0<> if E-ORT-HEX throw then
   u 2 / 0 ?do
      a i 2 * + c@ ORF-HEX-NIB 16 *
      a i 2 * 1+ + c@ ORF-HEX-NIB +
      ORF-BYTE+
   loop ;

: ORF-REF$ ( -- ptr u8 n )
   ORF-REF-U @ ORF-REF-LEN <> if E-ORT-DATA throw then
   ORF-REF ORF-REF-U @ ;

   s" 08083a8a030a140a01780a0277310a026231120168220447656d6d0a0c0a" ORF-HEX+
   s" 0168120161220452656c750a140a01610a0277320a026232120179220447" ORF-HEX+
   s" 656d6d12064558544d4c502a8d01080408081001420277314a800100c081" ORF-HEX+
   s" be00c0933e004063be0000debc000037bf00c00d3f0000963c0000fcbd00" ORF-HEX+
   s" c04cbe001033bf00c087be00f004bf00c076be0020293f0000903e00401e" ORF-HEX+
   s" 3f0020eebe00a0f5be007030bf00001cbc00004cbe00809dbe00303dbf00" ORF-HEX+
   s" 0034be00c0843e00603abf004095be00502f3f0020e2be0040b9be00208e" ORF-HEX+
   s" be00806e3e2a2a08081001420262314a2000004f3e0000adbd000095bd00" ORF-HEX+
   s" 80d7be0080bb3e0000e03e0080563e0080b63e2a4c080808021001420277" ORF-HEX+
   s" 324a400000fc3e0000f6bc00e0cdbe0040573e00e0053f0040c23e00005b" ORF-HEX+
   s" 3d00c06abe00407e3e0040ef3e0070213f00403c3e0040423e00c07f3e00" ORF-HEX+
   s" 60db3e008026be2a1208021001420262324a08008006be00809a3e5a130a" ORF-HEX+
   s" 0178120e0a0c080112080a0208020a02080462130a0179120e0a0c080112" ORF-HEX+
   s" 080a0208020a02080242040a00100d" ORF-HEX+

\ ---- the same ModelProto through the encode.f DSL (proved byte-identical) ------
: ORF-DIM+ ( n -- ) {: d:n :}                  \ one TensorShapeProto.Dimension
   1 ONNX:ENC-SUB  d 1 ONNX:ENC-INT  ONNX:;ENC-SUB ;

: ORF-VI+ ( ptr u8 n n n n -- ) {: a:ptr u:n fld:n rows:n cols:n :}   \ 2D f32 ValueInfo
   fld ONNX:ENC-SUB
      a u 1 ONNX:ENC-STR
      2 ONNX:ENC-SUB  1 ONNX:ENC-SUB
         1 1 ONNX:ENC-INT
         2 ONNX:ENC-SUB  rows ORF-DIM+  cols ORF-DIM+  ONNX:;ENC-SUB
      ONNX:;ENC-SUB  ONNX:;ENC-SUB
   ONNX:;ENC-SUB ;

: ORF-GEMM+ ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )   \ Gemm x w b -> out
   {: xa:ptr xu:n wa:ptr wu:n ba:ptr bu:n oa:ptr ou:n :}
   1 ONNX:ENC-SUB
      xa xu 1 ONNX:ENC-STR  wa wu 1 ONNX:ENC-STR  ba bu 1 ONNX:ENC-STR
      oa ou 2 ONNX:ENC-STR  s" Gemm" 4 ONNX:ENC-STR
   ONNX:;ENC-SUB ;

: ORF-RELU+ ( ptr u8 n ptr u8 n -- ) {: ia:ptr iu:n oa:ptr ou:n :}   \ Relu in -> out
   1 ONNX:ENC-SUB
      ia iu 1 ONNX:ENC-STR  oa ou 2 ONNX:ENC-STR  s" Relu" 4 ONNX:ENC-STR
   ONNX:;ENC-SUB ;

\ open a 2D f32 initializer (dims r c, raw_data payload follows; )ORF-INIT closes)
: ORF-INIT2( ( ptr u8 n n n -- ) {: a:ptr u:n r:n c:n :}
   5 ONNX:ENC-SUB
      r 1 ONNX:ENC-INT  c 1 ONNX:ENC-INT  1 2 ONNX:ENC-INT  a u 8 ONNX:ENC-STR
      9 ONNX:ENC-SUB ;

: ORF-INIT1( ( ptr u8 n n -- ) {: a:ptr u:n d:n :}   \ rank-1 f32 initializer (dims [d])
   5 ONNX:ENC-SUB
      d 1 ONNX:ENC-INT  1 2 ONNX:ENC-INT  a u 8 ONNX:ENC-STR
      9 ONNX:ENC-SUB ;

: )ORF-INIT ( -- )  ONNX:;ENC-SUB  ONNX:;ENC-SUB ;

\ opset_import { domain:"" version:13 } - onnx emits the EXPLICIT empty domain
: ORF-OPSET+ ( -- )
   8 ONNX:ENC-SUB
      1 ONNX:WT-LEN ONNX:ENC-TAG  0 ONNX:ENC-VARINT
      13 2 ONNX:ENC-INT
   ONNX:;ENC-SUB ;

: ORF-W1-DATA ( -- )                           \ w1 4x8 row-major raw_data
   -0.25341796875 ONNX:ENC-F32   0.28857421875 ONNX:ENC-F32
   -0.221923828125 ONNX:ENC-F32  -0.027099609375 ONNX:ENC-F32
   -0.71484375 ONNX:ENC-F32      0.5537109375 ONNX:ENC-F32
   0.018310546875 ONNX:ENC-F32   -0.123046875 ONNX:ENC-F32
   -0.199951171875 ONNX:ENC-F32  -0.699462890625 ONNX:ENC-F32
   -0.26513671875 ONNX:ENC-F32   -0.519287109375 ONNX:ENC-F32
   -0.240966796875 ONNX:ENC-F32  0.66064453125 ONNX:ENC-F32
   0.28125 ONNX:ENC-F32          0.6181640625 ONNX:ENC-F32
   -0.465087890625 ONNX:ENC-F32  -0.479736328125 ONNX:ENC-F32
   -0.689208984375 ONNX:ENC-F32  -0.009521484375 ONNX:ENC-F32
   -0.19921875 ONNX:ENC-F32      -0.3076171875 ONNX:ENC-F32
   -0.739013671875 ONNX:ENC-F32  -0.17578125 ONNX:ENC-F32
   0.25927734375 ONNX:ENC-F32    -0.72802734375 ONNX:ENC-F32
   -0.29150390625 ONNX:ENC-F32   0.684814453125 ONNX:ENC-F32
   -0.441650390625 ONNX:ENC-F32  -0.36181640625 ONNX:ENC-F32
   -0.277587890625 ONNX:ENC-F32  0.23291015625 ONNX:ENC-F32 ;

: ORF-B1-DATA ( -- )                           \ b1 [8] raw_data
   0.2021484375 ONNX:ENC-F32     -0.08447265625 ONNX:ENC-F32
   -0.07275390625 ONNX:ENC-F32   -0.4208984375 ONNX:ENC-F32
   0.3662109375 ONNX:ENC-F32     0.4375 ONNX:ENC-F32
   0.20947265625 ONNX:ENC-F32    0.3564453125 ONNX:ENC-F32 ;

: ORF-W2-DATA ( -- )                           \ w2 8x2 row-major raw_data
   0.4921875 ONNX:ENC-F32        -0.030029296875 ONNX:ENC-F32
   -0.402099609375 ONNX:ENC-F32  0.210205078125 ONNX:ENC-F32
   0.52294921875 ONNX:ENC-F32    0.37939453125 ONNX:ENC-F32
   0.053466796875 ONNX:ENC-F32   -0.229248046875 ONNX:ENC-F32
   0.248291015625 ONNX:ENC-F32   0.46728515625 ONNX:ENC-F32
   0.630615234375 ONNX:ENC-F32   0.183837890625 ONNX:ENC-F32
   0.189697265625 ONNX:ENC-F32   0.249755859375 ONNX:ENC-F32
   0.428466796875 ONNX:ENC-F32   -0.16259765625 ONNX:ENC-F32 ;

: ORF-B2-DATA ( -- )                           \ b2 [2] raw_data
   -0.13134765625 ONNX:ENC-F32   0.3017578125 ONNX:ENC-F32 ;

\ the whole exported ModelProto, field-for-field in onnx's serialization order
: ORF-MODEL ( -- )
   ONNX:ENC-RESET
   8 1 ONNX:ENC-INT                            \ ir_version 8
   7 ONNX:ENC-SUB                              \ graph
      s" x" s" w1" s" b1" s" h" ORF-GEMM+
      s" h" s" a" ORF-RELU+
      s" a" s" w2" s" b2" s" y" ORF-GEMM+
      s" EXTMLP" 2 ONNX:ENC-STR
      s" w1" 4 8 ORF-INIT2(  ORF-W1-DATA  )ORF-INIT
      s" b1" 8   ORF-INIT1(  ORF-B1-DATA  )ORF-INIT
      s" w2" 8 2 ORF-INIT2(  ORF-W2-DATA  )ORF-INIT
      s" b2" 2   ORF-INIT1(  ORF-B2-DATA  )ORF-INIT
      s" x" 11 ORF-BATCH ORF-IN  ORF-VI+
      s" y" 12 ORF-BATCH ORF-OUT ORF-VI+
   ONNX:;ENC-SUB
   ORF-OPSET+ ;

\ ---- committed input (the tensor onnxruntime consumed, exact in f32 and f64) ---
create ORF-X ORF-XN cells allot

0.408203125  ORF-X 0 T-SET   0.2265625    ORF-X 1 T-SET
-0.7607421875 ORF-X 2 T-SET  0.5068359375 ORF-X 3 T-SET
0.287109375  ORF-X 4 T-SET   -0.6904296875 ORF-X 5 T-SET
0.1611328125 ORF-X 6 T-SET   0.9951171875 ORF-X 7 T-SET

\ ---- onnxruntime's output (f32 run, widened to its exact f64 decimals) ---------
create ORF-Y ORF-YN cells allot

1.1908855438232422  ORF-Y 0 T-SET   0.5643671751022339  ORF-Y 1 T-SET
0.16488242149353027 ORF-Y 2 T-SET   0.13253964483737946 ORF-Y 3 T-SET

end-package
