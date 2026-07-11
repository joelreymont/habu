\ maki/onnx/import-test.f - checked end-to-end tests for the ONNX graph importer.
\
\ Each fixture is a complete ModelProto hand-encoded IN SOURCE by the checked
\ encoder DSL (maki/onnx/encode.f), imported by ONNX:IMPORT, and PROVEN by
\ host-executing the imported IR (MAKI:EX-RUN) against values computed by hand:
\   A: Gemm(x,w,b) -> Relu   raw_data initializers, rank-1 bias -> 1xN, OP-LINEAR
\   B: Add(x,c)              float_data initializer ALSO listed as a graph input
\                            (legacy export form), written/reimported via IMPORT-FILE
\   C: Mul -> Softmax(axis=-1)  negative-varint attr, OP-SOFTMAX-ROW
\   D: Gemm(x,w) two-input   -> OP-MATMUL
\   E: Add(x,bias 1x2)       numpy row broadcast -> OP-BIAS
\   F: Mul(x,scale 1x1)      numpy scalar broadcast -> OP-SCALE
\   RS: Reshape(x,[1,4])     target from an INT64 shape initializer -> OP-RESHAPE
\   TR: Transpose(x,[1,0])   2x3 -> 3x2 perm attribute -> OP-TRANSPOSE
\   CC: Concat(x,cc,axis 0)  2x2 + 1x2 -> 3x2 row-append -> OP-CONCAT
\   GTB: Gemm(x,wt,transB=1) TRANSPOSE + MATMUL (the PyTorch Linear export shape)
\   GAB: Gemm(x,w,b,a=2,b=0) MATMUL + SCALE (synthetic 1x1), C dropped
\ The fusion planner runs the imported IR too (FP-BUILD; Gemm->Relu fuses into
\ one region). Negative fixtures cover every importer throw: dynamic dim_param,
\ unsupported op (the ONNX:LOWER rejection), non-topological input, a foreign Gemm
\ attr / bad Softmax / foreign attrs, rank 3, non-f32 dtype, a 2x1 column and a ragged
\ 3x2-vs-2x2 Add (broadcast shapes outside the legal classes), a runtime-computed
\ Reshape shape, a non-transpose and a rank-3 Transpose perm, two graph outputs, output
\ not the last node, missing graph, SSA rebind, missing initializer payload, oversized
\ name, wrong arity, and truncated model bytes.

require lib/test.f
require lib/string.f
require lib/float.f
require lib/fs.f
require lib/fs-mutate.f
require maki/onnx/proto.f
require maki/onnx/encode.f
require maki/onnx/graph.f
require maki/onnx/import.f
require maki/executor.f
require maki/fusion-plan.f

package ONNX-IMPORT-TEST

\ ---- readers (executor-test pattern) -----------------------------------------
: >I ( ptr a n -- n )  T-GET 0.5 f+ f>s ;             \ cell as nearest int (>= 0)
: >M ( ptr a n -- n )  T-GET 1000.0 f* 0.5 f+ f>s ;   \ cell as milliunits (>= 0)

create XB 8 cells allot                        \ runtime input buffer
create X2B 8 cells allot

: ENC-I64 ( n -- ) {: v:n :}                   \ append 8 raw LE bytes of an int64 (constant payload)
   v $FF and ONNX:ENC-B          v 8  rshift $FF and ONNX:ENC-B
   v 16 rshift $FF and ONNX:ENC-B  v 24 rshift $FF and ONNX:ENC-B
   v 32 rshift $FF and ONNX:ENC-B  v 40 rshift $FF and ONNX:ENC-B
   v 48 rshift $FF and ONNX:ENC-B  v 56 rshift $FF and ONNX:ENC-B ;

\ ---- encoder helpers: ValueInfo / node / initializer builders ------------------
: DIM+ ( n -- ) {: d:n :}                      \ one TensorShapeProto.Dimension
   1 ONNX:ENC-SUB  d 1 ONNX:ENC-INT  ONNX:;ENC-SUB ;

: VI+ ( ptr u8 n n n n -- ) {: a:ptr u:n fld:n rows:n cols:n :}   \ 2D f32 ValueInfo
   fld ONNX:ENC-SUB
      a u 1 ONNX:ENC-STR
      2 ONNX:ENC-SUB  1 ONNX:ENC-SUB
         1 1 ONNX:ENC-INT
         2 ONNX:ENC-SUB  rows DIM+  cols DIM+  ONNX:;ENC-SUB
      ONNX:;ENC-SUB  ONNX:;ENC-SUB
   ONNX:;ENC-SUB ;

: MDL ( -- )  ONNX:ENC-RESET  8 1 ONNX:ENC-INT  7 ONNX:ENC-SUB ;  \ ir_version + open graph
: ;MDL ( -- )                                  \ close graph + a skipped opset_import
   ONNX:;ENC-SUB
   8 ONNX:ENC-SUB  13 2 ONNX:ENC-INT  ONNX:;ENC-SUB ;

: NODE1 ( ptr u8 n ptr u8 n ptr u8 n -- )      \ one-input node: op in out
   {: opa:ptr opu:n ia:ptr iu:n oa:ptr ou:n :}
   1 ONNX:ENC-SUB
      ia iu 1 ONNX:ENC-STR  oa ou 2 ONNX:ENC-STR  opa opu 4 ONNX:ENC-STR
   ONNX:;ENC-SUB ;

: NODE2 ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )   \ two-input node: op in0 in1 out
   {: opa:ptr opu:n ia:ptr iu:n ja:ptr ju:n oa:ptr ou:n :}
   1 ONNX:ENC-SUB
      ia iu 1 ONNX:ENC-STR  ja ju 1 ONNX:ENC-STR
      oa ou 2 ONNX:ENC-STR  opa opu 4 ONNX:ENC-STR
   ONNX:;ENC-SUB ;

: INIT-HDR ( ptr u8 n -- )                     \ dims [2,2] f32 + name (payload follows)
   {: a:ptr u:n :}
   2 1 ONNX:ENC-INT  2 1 ONNX:ENC-INT  1 2 ONNX:ENC-INT  a u 8 ONNX:ENC-STR ;

: INIT-W ( -- )                                \ w 2x2 = [[5,6],[7,8]] via raw_data
   5 ONNX:ENC-SUB
      s" w" INIT-HDR
      9 ONNX:ENC-SUB
         5.0 ONNX:ENC-F32  6.0 ONNX:ENC-F32  7.0 ONNX:ENC-F32  8.0 ONNX:ENC-F32
      ONNX:;ENC-SUB
   ONNX:;ENC-SUB ;

: INIT-B ( -- )                                \ b [2] = [-30,20] rank-1 via raw_data
   5 ONNX:ENC-SUB
      2 1 ONNX:ENC-INT  1 2 ONNX:ENC-INT  s" b" 8 ONNX:ENC-STR
      9 ONNX:ENC-SUB  -30.0 ONNX:ENC-F32  20.0 ONNX:ENC-F32  ONNX:;ENC-SUB
   ONNX:;ENC-SUB ;

: INIT-C ( -- )                                \ c 2x2 = [[1,2],[3,4]] via packed float_data
   5 ONNX:ENC-SUB
      s" c" INIT-HDR
      4 ONNX:ENC-SUB
         1.0 ONNX:ENC-F32  2.0 ONNX:ENC-F32  3.0 ONNX:ENC-F32  4.0 ONNX:ENC-F32
      ONNX:;ENC-SUB
   ONNX:;ENC-SUB ;

\ ---- fixture A: Gemm(x,w,b) -> Relu (the 2-node dot fixture) --------------------
: GEMM-NODE ( -- )                             \ Gemm x w b -> t
   1 ONNX:ENC-SUB
      s" x" 1 ONNX:ENC-STR  s" w" 1 ONNX:ENC-STR  s" b" 1 ONNX:ENC-STR
      s" t" 2 ONNX:ENC-STR  s" Gemm" 4 ONNX:ENC-STR
   ONNX:;ENC-SUB ;

: MODEL-A-OUT ( ptr u8 n -- ) {: oa:ptr ou:n :}   \ fixture A with a chosen graph output
   MDL
   s" GR2" 2 ONNX:ENC-STR
   GEMM-NODE
   s" Relu" s" t" s" y" NODE1
   INIT-W  INIT-B
   s" x" 11 2 2 VI+  oa ou 12 2 2 VI+
   ;MDL ;

: MODEL-A ( -- )  s" y" MODEL-A-OUT ;

\ ---- fixture B: Add(x,c), c = float_data initializer also listed as an input ----
: MODEL-B ( -- )
   MDL
   s" ADD1" 2 ONNX:ENC-STR
   s" Add" s" x" s" c" s" y" NODE2
   INIT-C
   s" x" 11 2 2 VI+  s" c" 11 2 2 VI+  s" y" 12 2 2 VI+
   ;MDL ;

\ ---- fixture C: Mul -> Softmax(axis=-1) ------------------------------------------
: MODEL-C ( -- )
   MDL
   s" MS" 2 ONNX:ENC-STR
   s" Mul" s" x" s" x2" s" m" NODE2
   1 ONNX:ENC-SUB
      s" m" 1 ONNX:ENC-STR  s" y" 2 ONNX:ENC-STR  s" Softmax" 4 ONNX:ENC-STR
      5 ONNX:ENC-SUB  s" axis" 1 ONNX:ENC-STR  -1 3 ONNX:ENC-INT  ONNX:;ENC-SUB
   ONNX:;ENC-SUB
   s" x" 11 1 2 VI+  s" x2" 11 1 2 VI+  s" y" 12 1 2 VI+
   ;MDL ;

\ ---- fixture D: two-input Gemm -> OP-MATMUL --------------------------------------
: MODEL-D ( -- )
   MDL
   s" MM" 2 ONNX:ENC-STR
   s" Gemm" s" x" s" w" s" y" NODE2
   INIT-W
   s" x" 11 2 2 VI+  s" y" 12 2 2 VI+
   ;MDL ;

\ ---- broadcast initializers (dims [r,c], f32 raw_data) --------------------------
: INIT-BR ( -- )                               \ bias row 1x2 = [10,20]
   5 ONNX:ENC-SUB
      1 1 ONNX:ENC-INT  2 1 ONNX:ENC-INT  1 2 ONNX:ENC-INT  s" br" 8 ONNX:ENC-STR
      9 ONNX:ENC-SUB  10.0 ONNX:ENC-F32  20.0 ONNX:ENC-F32  ONNX:;ENC-SUB
   ONNX:;ENC-SUB ;

: INIT-SC ( -- )                               \ scale scalar 1x1 = [3]
   5 ONNX:ENC-SUB
      1 1 ONNX:ENC-INT  1 1 ONNX:ENC-INT  1 2 ONNX:ENC-INT  s" sc" 8 ONNX:ENC-STR
      9 ONNX:ENC-SUB  3.0 ONNX:ENC-F32  ONNX:;ENC-SUB
   ONNX:;ENC-SUB ;

: INIT-COL ( -- )                              \ column 2x1 = [[5],[6]] (Rx1, an illegal Add class)
   5 ONNX:ENC-SUB
      2 1 ONNX:ENC-INT  1 1 ONNX:ENC-INT  1 2 ONNX:ENC-INT  s" col" 8 ONNX:ENC-STR
      9 ONNX:ENC-SUB  5.0 ONNX:ENC-F32  6.0 ONNX:ENC-F32  ONNX:;ENC-SUB
   ONNX:;ENC-SUB ;

: INIT-R2 ( -- )                               \ 2x2 = ones (ragged vs a 3x2 operand)
   5 ONNX:ENC-SUB
      2 1 ONNX:ENC-INT  2 1 ONNX:ENC-INT  1 2 ONNX:ENC-INT  s" r2" 8 ONNX:ENC-STR
      9 ONNX:ENC-SUB
         1.0 ONNX:ENC-F32  1.0 ONNX:ENC-F32  1.0 ONNX:ENC-F32  1.0 ONNX:ENC-F32
      ONNX:;ENC-SUB
   ONNX:;ENC-SUB ;

\ ---- fixture E: Add(x 2x2, bias 1x2) -> OP-BIAS (row broadcast) ------------------
: MODEL-E ( -- )
   MDL
   s" EB" 2 ONNX:ENC-STR
   s" Add" s" x" s" br" s" y" NODE2
   INIT-BR
   s" x" 11 2 2 VI+  s" y" 12 2 2 VI+
   ;MDL ;

\ ---- fixture F: Mul(x 2x2, scale 1x1) -> OP-SCALE (scalar broadcast) -------------
: MODEL-F ( -- )
   MDL
   s" FS" 2 ONNX:ENC-STR
   s" Mul" s" x" s" sc" s" y" NODE2
   INIT-SC
   s" x" 11 2 2 VI+  s" y" 12 2 2 VI+
   ;MDL ;

\ ---- movement fixtures (Reshape / Transpose / Concat) ---------------------------
: INIT-SH ( -- )                               \ int64 shape constant "sh" = [1,4] (rank-1 dims=[2])
   5 ONNX:ENC-SUB
      2 1 ONNX:ENC-INT  7 2 ONNX:ENC-INT  s" sh" 8 ONNX:ENC-STR   \ dims [2], data_type INT64
      9 ONNX:ENC-SUB  1 ENC-I64  4 ENC-I64  ONNX:;ENC-SUB          \ raw_data [1,4]
   ONNX:;ENC-SUB ;

: RESHAPE-NODE ( -- )                          \ Reshape x sh -> y
   1 ONNX:ENC-SUB
      s" x" 1 ONNX:ENC-STR  s" sh" 1 ONNX:ENC-STR
      s" y" 2 ONNX:ENC-STR  s" Reshape" 4 ONNX:ENC-STR
   ONNX:;ENC-SUB ;

: MODEL-RS ( -- )                              \ Reshape(x 2x2, sh [1,4]) -> y 1x4
   MDL
   s" RS" 2 ONNX:ENC-STR
   RESHAPE-NODE  INIT-SH
   s" x" 11 2 2 VI+  s" y" 12 1 4 VI+
   ;MDL ;

: TRANSPOSE-NODE ( -- )                        \ Transpose x -> y, perm [1,0] (unpacked ints)
   1 ONNX:ENC-SUB
      s" x" 1 ONNX:ENC-STR  s" y" 2 ONNX:ENC-STR  s" Transpose" 4 ONNX:ENC-STR
      5 ONNX:ENC-SUB  s" perm" 1 ONNX:ENC-STR
         1 8 ONNX:ENC-INT  0 8 ONNX:ENC-INT
      ONNX:;ENC-SUB
   ONNX:;ENC-SUB ;

: MODEL-TR ( -- )                              \ Transpose(x 2x3, perm [1,0]) -> y 3x2
   MDL
   s" TR" 2 ONNX:ENC-STR
   TRANSPOSE-NODE
   s" x" 11 2 3 VI+  s" y" 12 3 2 VI+
   ;MDL ;

: INIT-CC ( -- )                               \ float 1x2 = [10,20] (concat second operand)
   5 ONNX:ENC-SUB
      1 1 ONNX:ENC-INT  2 1 ONNX:ENC-INT  1 2 ONNX:ENC-INT  s" cc" 8 ONNX:ENC-STR
      9 ONNX:ENC-SUB  10.0 ONNX:ENC-F32  20.0 ONNX:ENC-F32  ONNX:;ENC-SUB
   ONNX:;ENC-SUB ;

: CONCAT-NODE ( -- )                           \ Concat x cc -> y, axis 0
   1 ONNX:ENC-SUB
      s" x" 1 ONNX:ENC-STR  s" cc" 1 ONNX:ENC-STR
      s" y" 2 ONNX:ENC-STR  s" Concat" 4 ONNX:ENC-STR
      5 ONNX:ENC-SUB  s" axis" 1 ONNX:ENC-STR  0 3 ONNX:ENC-INT  ONNX:;ENC-SUB
   ONNX:;ENC-SUB ;

: MODEL-CC ( -- )                              \ Concat(x 2x2, cc 1x2) axis 0 -> y 3x2
   MDL
   s" CC" 2 ONNX:ENC-STR
   CONCAT-NODE  INIT-CC
   s" x" 11 2 2 VI+  s" y" 12 3 2 VI+
   ;MDL ;

\ ---- Gemm attribute composition fixtures (transB / alpha+beta) -------------------
: INIT-WT ( -- )                               \ wt 2x3 = [[1,0,1],[0,1,0]] (a transB weight)
   5 ONNX:ENC-SUB
      2 1 ONNX:ENC-INT  3 1 ONNX:ENC-INT  1 2 ONNX:ENC-INT  s" wt" 8 ONNX:ENC-STR
      9 ONNX:ENC-SUB
         1.0 ONNX:ENC-F32  0.0 ONNX:ENC-F32  1.0 ONNX:ENC-F32
         0.0 ONNX:ENC-F32  1.0 ONNX:ENC-F32  0.0 ONNX:ENC-F32
      ONNX:;ENC-SUB
   ONNX:;ENC-SUB ;

: GEMM-TB-NODE ( -- )                          \ Gemm x wt -> y, transB=1
   1 ONNX:ENC-SUB
      s" x" 1 ONNX:ENC-STR  s" wt" 1 ONNX:ENC-STR  s" y" 2 ONNX:ENC-STR
      s" Gemm" 4 ONNX:ENC-STR
      5 ONNX:ENC-SUB  s" transB" 1 ONNX:ENC-STR  1 3 ONNX:ENC-INT  ONNX:;ENC-SUB
   ONNX:;ENC-SUB ;

: MODEL-GTB ( -- )                             \ Gemm(x 2x3, wt 2x3, transB=1) -> y 2x2 = x . wt^T
   MDL
   s" GTB" 2 ONNX:ENC-STR
   GEMM-TB-NODE  INIT-WT
   s" x" 11 2 3 VI+  s" y" 12 2 2 VI+
   ;MDL ;

: GEMM-AB-NODE ( -- )                          \ Gemm x w b -> y, alpha=2 beta=0
   1 ONNX:ENC-SUB
      s" x" 1 ONNX:ENC-STR  s" w" 1 ONNX:ENC-STR  s" b" 1 ONNX:ENC-STR
      s" y" 2 ONNX:ENC-STR  s" Gemm" 4 ONNX:ENC-STR
      5 ONNX:ENC-SUB  s" alpha" 1 ONNX:ENC-STR  2.0 2 ONNX:ENC-F32A  ONNX:;ENC-SUB
      5 ONNX:ENC-SUB  s" beta"  1 ONNX:ENC-STR  0.0 2 ONNX:ENC-F32A  ONNX:;ENC-SUB
   ONNX:;ENC-SUB ;

: MODEL-GAB ( -- )                             \ Gemm(x 2x2, w 2x2, b, alpha=2, beta=0) -> 2*(x . w)
   MDL
   s" GAB" 2 ONNX:ENC-STR
   GEMM-AB-NODE  INIT-W  INIT-B
   s" x" 11 2 2 VI+  s" y" 12 2 2 VI+
   ;MDL ;

\ ---- negative fixtures (each builds a model then imports it) ---------------------
: IMP! ( -- )  ONNX:ENC$ ONNX:IMPORT ;

: TRY-DYN ( -- )                               \ symbolic dim_param -> fail closed
   MDL
   s" Relu" s" x" s" y" NODE1
   11 ONNX:ENC-SUB
      s" x" 1 ONNX:ENC-STR
      2 ONNX:ENC-SUB  1 ONNX:ENC-SUB
         1 1 ONNX:ENC-INT
         2 ONNX:ENC-SUB
            1 ONNX:ENC-SUB  s" N" 2 ONNX:ENC-STR  ONNX:;ENC-SUB
            2 DIM+
         ONNX:;ENC-SUB
      ONNX:;ENC-SUB  ONNX:;ENC-SUB
   ONNX:;ENC-SUB
   s" y" 12 2 2 VI+
   ;MDL IMP! ;

: TRY-CONV ( -- )                              \ unsupported op: the LOWER table rejection
   MDL  s" Conv" s" x" s" y" NODE1
   s" x" 11 2 2 VI+  s" y" 12 2 2 VI+  ;MDL IMP! ;

: TRY-TOPO ( -- )                              \ node reads a LATER node's output
   MDL
   s" Relu" s" t" s" y" NODE1
   s" Relu" s" x" s" t" NODE1
   s" x" 11 2 2 VI+  s" y" 12 2 2 VI+  ;MDL IMP! ;

: TRY-GEMMATTR ( -- )                          \ Gemm with an axis attr: outside the Gemm attribute set
   MDL
   1 ONNX:ENC-SUB
      s" x" 1 ONNX:ENC-STR  s" w" 1 ONNX:ENC-STR  s" y" 2 ONNX:ENC-STR
      s" Gemm" 4 ONNX:ENC-STR
      5 ONNX:ENC-SUB  s" axis" 1 ONNX:ENC-STR  1 3 ONNX:ENC-INT  ONNX:;ENC-SUB
   ONNX:;ENC-SUB
   INIT-W  s" x" 11 2 2 VI+  s" y" 12 2 2 VI+  ;MDL IMP! ;

: TRY-AXIS ( -- )                              \ Softmax axis=0 is not the last axis
   MDL
   1 ONNX:ENC-SUB
      s" x" 1 ONNX:ENC-STR  s" y" 2 ONNX:ENC-STR  s" Softmax" 4 ONNX:ENC-STR
      5 ONNX:ENC-SUB  s" axis" 1 ONNX:ENC-STR  0 3 ONNX:ENC-INT  ONNX:;ENC-SUB
   ONNX:;ENC-SUB
   s" x" 11 2 2 VI+  s" y" 12 2 2 VI+  ;MDL IMP! ;

: TRY-ADDATTR ( -- )                           \ a recognized attr on an op that allows none
   MDL
   1 ONNX:ENC-SUB
      s" x" 1 ONNX:ENC-STR  s" x" 1 ONNX:ENC-STR  s" y" 2 ONNX:ENC-STR
      s" Add" 4 ONNX:ENC-STR
      5 ONNX:ENC-SUB  s" axis" 1 ONNX:ENC-STR  1 3 ONNX:ENC-INT  ONNX:;ENC-SUB
   ONNX:;ENC-SUB
   s" x" 11 2 2 VI+  s" y" 12 2 2 VI+  ;MDL IMP! ;

: TRY-BADATTR ( -- )                           \ an attr name the importer cannot honor
   MDL
   1 ONNX:ENC-SUB
      s" x" 1 ONNX:ENC-STR  s" y" 2 ONNX:ENC-STR  s" Relu" 4 ONNX:ENC-STR
      5 ONNX:ENC-SUB  s" pads" 1 ONNX:ENC-STR  1 3 ONNX:ENC-INT  ONNX:;ENC-SUB
   ONNX:;ENC-SUB
   s" x" 11 2 2 VI+  s" y" 12 2 2 VI+  ;MDL IMP! ;

: TRY-RANK3 ( -- )                             \ rank-3 input: the IR is 2D
   MDL
   s" Relu" s" x" s" y" NODE1
   11 ONNX:ENC-SUB
      s" x" 1 ONNX:ENC-STR
      2 ONNX:ENC-SUB  1 ONNX:ENC-SUB
         1 1 ONNX:ENC-INT
         2 ONNX:ENC-SUB  2 DIM+  2 DIM+  2 DIM+  ONNX:;ENC-SUB
      ONNX:;ENC-SUB  ONNX:;ENC-SUB
   ONNX:;ENC-SUB
   s" y" 12 2 2 VI+  ;MDL IMP! ;

: TRY-I64DT ( -- )                             \ elem_type INT64 (7): only FLOAT lowers
   MDL
   s" Relu" s" x" s" y" NODE1
   11 ONNX:ENC-SUB
      s" x" 1 ONNX:ENC-STR
      2 ONNX:ENC-SUB  1 ONNX:ENC-SUB
         7 1 ONNX:ENC-INT
         2 ONNX:ENC-SUB  2 DIM+  2 DIM+  ONNX:;ENC-SUB
      ONNX:;ENC-SUB  ONNX:;ENC-SUB
   ONNX:;ENC-SUB
   s" y" 12 2 2 VI+  ;MDL IMP! ;

: TRY-ADDCOL ( -- )                            \ Add second operand 2x1 column: not a legal Add class
   MDL
   s" Add" s" x" s" col" s" y" NODE2
   INIT-COL
   s" x" 11 2 2 VI+  s" y" 12 2 2 VI+  ;MDL IMP! ;

: TRY-ADDRAGGED ( -- )                         \ Add operands 3x2 vs 2x2 (rows differ, neither 1)
   MDL
   s" Add" s" x" s" r2" s" y" NODE2
   INIT-R2
   s" x" 11 3 2 VI+  s" y" 12 3 2 VI+  ;MDL IMP! ;

: TRY-RSDYN ( -- )                             \ Reshape shape is a runtime graph input, not a constant
   MDL
   RESHAPE-NODE
   s" x" 11 2 2 VI+  s" sh" 11 1 2 VI+  s" y" 12 1 4 VI+  ;MDL IMP! ;

: TRY-BADPERM ( -- )                           \ Transpose perm [0,1] is identity, not the 2D transpose
   MDL
   1 ONNX:ENC-SUB
      s" x" 1 ONNX:ENC-STR  s" y" 2 ONNX:ENC-STR  s" Transpose" 4 ONNX:ENC-STR
      5 ONNX:ENC-SUB  s" perm" 1 ONNX:ENC-STR  0 8 ONNX:ENC-INT  1 8 ONNX:ENC-INT  ONNX:;ENC-SUB
   ONNX:;ENC-SUB
   s" x" 11 2 2 VI+  s" y" 12 2 2 VI+  ;MDL IMP! ;

: TRY-PERM3 ( -- )                             \ a rank-3 perm on the 2D importer
   MDL
   1 ONNX:ENC-SUB
      s" x" 1 ONNX:ENC-STR  s" y" 2 ONNX:ENC-STR  s" Transpose" 4 ONNX:ENC-STR
      5 ONNX:ENC-SUB  s" perm" 1 ONNX:ENC-STR
         2 8 ONNX:ENC-INT  1 8 ONNX:ENC-INT  0 8 ONNX:ENC-INT
      ONNX:;ENC-SUB
   ONNX:;ENC-SUB
   s" x" 11 2 2 VI+  s" y" 12 2 2 VI+  ;MDL IMP! ;

: TRY-2OUT ( -- )                              \ two graph outputs: outside the v1 contract
   MDL
   GEMM-NODE  s" Relu" s" t" s" y" NODE1  INIT-W INIT-B
   s" x" 11 2 2 VI+  s" y" 12 2 2 VI+  s" t" 12 2 2 VI+
   ;MDL IMP! ;

: TRY-OUTMID ( -- )                            \ the graph output is not the last node
   s" t" MODEL-A-OUT IMP! ;

: TRY-NOGRAPH ( -- )                           \ ModelProto without a graph field
   ONNX:ENC-RESET  8 1 ONNX:ENC-INT  IMP! ;

: TRY-SSA ( -- )                               \ two nodes bind the same output name
   MDL
   s" Relu" s" x" s" y" NODE1
   s" Relu" s" x" s" y" NODE1
   s" x" 11 2 2 VI+  s" y" 12 2 2 VI+  ;MDL IMP! ;

: TRY-NODATA ( -- )                            \ initializer without a payload
   MDL
   s" Add" s" x" s" c" s" y" NODE2
   5 ONNX:ENC-SUB  s" c" INIT-HDR  ONNX:;ENC-SUB
   s" x" 11 2 2 VI+  s" y" 12 2 2 VI+  ;MDL IMP! ;

create LONGN 70 allot
: FILL-LONGN ( -- )  70 0 ?do  $61 LONGN i + c!  loop ;
FILL-LONGN

: TRY-LONGNAME ( -- )                          \ a 70-byte tensor name over the slot cap
   MDL
   s" Relu" s" x" LONGN 70 NODE1
   s" x" 11 2 2 VI+  s" y" 12 2 2 VI+  ;MDL IMP! ;

: TRY-ARITY ( -- )                             \ Add with one input
   MDL
   s" Add" s" x" s" y" NODE1
   s" x" 11 2 2 VI+  s" y" 12 2 2 VI+  ;MDL IMP! ;

: TRY-TRUNC ( -- )                             \ fixture A cut one byte short
   MODEL-A  ONNX:ENC$ 1- ONNX:IMPORT ;

\ ---- IMPORT-FILE round trip -------------------------------------------------------
: FIX-PATH$ ( -- ptr u8 n )  s" tmp/onnx-import-test.onnx" ;
: WRITE-FIX ( -- )  s" tmp" MAKE-DIRS  MODEL-B  FIX-PATH$ ONNX:ENC$ WRITE-ALL ;

T-RESET

\ ---- fixture A: import facts + host execution + fusion plan ----------------------
MODEL-A  ONNX:ENC$ ONNX:IMPORT
MAKI:MIR-N@ 2 T=
MAKI:MIR-IN-SLOTS@ 3 T=                        \ x + initializers w, b
MAKI:MIR-NAME$ s" GR2" STR= TTRUE
0 MAKI:MIR-OP@ MAKI:OPKIND>N MAKI:OP-LINEAR T=               \ 3-input Gemm -> linear
1 MAKI:MIR-OP@ MAKI:OPKIND>N MAKI:OP-RELU T=
1 MAKI:MIR-ROWS@ 2 T=  1 MAKI:MIR-COLS@ 2 T=
ONNX:IN# 1 T=
0 ONNX:IN-SLOT@ 0 T=
0 ONNX:IN-NAME$ s" x" STR= TTRUE
ONNX:OUT-NODE@ 1 T=
ONNX:INIT# 2 T=
0 ONNX:INIT-SLOT@ 1 T=  1 ONNX:INIT-SLOT@ 2 T=
0 ONNX:INIT-DATA@ 0 >I 5 T=                    \ w materialized from raw_data
0 ONNX:INIT-DATA@ 3 >I 8 T=

\ x = [[1,2],[3,4]]; x.w + b = [[-11,42],[13,70]]; relu -> [[0,42],[13,70]]
1.0 XB 0 T-SET  2.0 XB 1 T-SET  3.0 XB 2 T-SET  4.0 XB 3 T-SET
MAKI:EX-RESET  ONNX:BIND-INITS  XB 0 ONNX:IN-SLOT@ MAKI:EX-BIND  MAKI:EX-RUN
ONNX:OUT-NODE@ MAKI:EX-OUT@ 0 >I 0 T=
ONNX:OUT-NODE@ MAKI:EX-OUT@ 1 >I 42 T=
ONNX:OUT-NODE@ MAKI:EX-OUT@ 2 >I 13 T=
ONNX:OUT-NODE@ MAKI:EX-OUT@ 3 >I 70 T=

MAKI:FP-BUILD                                  \ the fusion planner runs the imported IR
MAKI:FP-REGION-COUNT 1 T=                      \ Gemm -> Relu fuses (matmul + EW epilogue)

\ ---- fixture B: float_data init, input-listed initializer, IMPORT-FILE ------------
WRITE-FIX
FIX-PATH$ ONNX:IMPORT-FILE                     \ the on-disk .onnx path round-trips
MAKI:MIR-N@ 1 T=
0 MAKI:MIR-OP@ MAKI:OPKIND>N MAKI:OP-ADD T=
ONNX:IN# 1 T=                                  \ c is initializer-bound, not a runtime input
MAKI:MIR-IN-SLOTS@ 2 T=
10.0 XB 0 T-SET  20.0 XB 1 T-SET  30.0 XB 2 T-SET  40.0 XB 3 T-SET
MAKI:EX-RESET  ONNX:BIND-INITS  XB 0 ONNX:IN-SLOT@ MAKI:EX-BIND  MAKI:EX-RUN
ONNX:OUT-NODE@ MAKI:EX-OUT@ 0 >I 11 T=
ONNX:OUT-NODE@ MAKI:EX-OUT@ 1 >I 22 T=
ONNX:OUT-NODE@ MAKI:EX-OUT@ 2 >I 33 T=
ONNX:OUT-NODE@ MAKI:EX-OUT@ 3 >I 44 T=
FIX-PATH$ REMOVE-FILE

\ ---- fixture C: Mul -> Softmax(axis=-1) --------------------------------------------
MODEL-C  ONNX:ENC$ ONNX:IMPORT
MAKI:MIR-N@ 2 T=
0 MAKI:MIR-OP@ MAKI:OPKIND>N MAKI:OP-MUL T=
1 MAKI:MIR-OP@ MAKI:OPKIND>N MAKI:OP-SOFTMAX-ROW T=
ONNX:IN# 2 T=
2.0 XB 0 T-SET  3.0 XB 1 T-SET
0.0 X2B 0 T-SET  0.0 X2B 1 T-SET
MAKI:EX-RESET  ONNX:BIND-INITS
XB 0 ONNX:IN-SLOT@ MAKI:EX-BIND  X2B 1 ONNX:IN-SLOT@ MAKI:EX-BIND  MAKI:EX-RUN
ONNX:OUT-NODE@ MAKI:EX-OUT@ 0 >M 500 T=        \ softmax([0,0]) = [0.5,0.5]
ONNX:OUT-NODE@ MAKI:EX-OUT@ 1 >M 500 T=

\ ---- fixture D: two-input Gemm -> matmul -------------------------------------------
MODEL-D  ONNX:ENC$ ONNX:IMPORT
MAKI:MIR-N@ 1 T=
0 MAKI:MIR-OP@ MAKI:OPKIND>N MAKI:OP-MATMUL T=
1.0 XB 0 T-SET  2.0 XB 1 T-SET  3.0 XB 2 T-SET  4.0 XB 3 T-SET
MAKI:EX-RESET  ONNX:BIND-INITS  XB 0 ONNX:IN-SLOT@ MAKI:EX-BIND  MAKI:EX-RUN
ONNX:OUT-NODE@ MAKI:EX-OUT@ 0 >I 19 T=         \ [[1,2],[3,4]] . [[5,6],[7,8]]
ONNX:OUT-NODE@ MAKI:EX-OUT@ 1 >I 22 T=
ONNX:OUT-NODE@ MAKI:EX-OUT@ 2 >I 43 T=
ONNX:OUT-NODE@ MAKI:EX-OUT@ 3 >I 50 T=

\ ---- fixture E: Add with a 1x2 bias -> OP-BIAS, row-broadcast ---------------------
MODEL-E  ONNX:ENC$ ONNX:IMPORT
MAKI:MIR-N@ 1 T=
0 MAKI:MIR-OP@ MAKI:OPKIND>N MAKI:OP-BIAS T=                  \ 1xC second operand maps to a bias node
ONNX:IN# 1 T=                                   \ br is initializer-bound, x is the runtime input
1.0 XB 0 T-SET  2.0 XB 1 T-SET  3.0 XB 2 T-SET  4.0 XB 3 T-SET
MAKI:EX-RESET  ONNX:BIND-INITS  XB 0 ONNX:IN-SLOT@ MAKI:EX-BIND  MAKI:EX-RUN
ONNX:OUT-NODE@ MAKI:EX-OUT@ 0 >I 11 T=          \ x + [10,20] broadcast over rows
ONNX:OUT-NODE@ MAKI:EX-OUT@ 1 >I 22 T=
ONNX:OUT-NODE@ MAKI:EX-OUT@ 2 >I 13 T=
ONNX:OUT-NODE@ MAKI:EX-OUT@ 3 >I 24 T=

\ ---- fixture F: Mul with a 1x1 scalar -> OP-SCALE --------------------------------
MODEL-F  ONNX:ENC$ ONNX:IMPORT
MAKI:MIR-N@ 1 T=
0 MAKI:MIR-OP@ MAKI:OPKIND>N MAKI:OP-SCALE T=                 \ 1x1 second operand maps to a scale node
1.0 XB 0 T-SET  2.0 XB 1 T-SET  3.0 XB 2 T-SET  4.0 XB 3 T-SET
MAKI:EX-RESET  ONNX:BIND-INITS  XB 0 ONNX:IN-SLOT@ MAKI:EX-BIND  MAKI:EX-RUN
ONNX:OUT-NODE@ MAKI:EX-OUT@ 0 >I 3 T=           \ x * 3 (scalar broadcast)
ONNX:OUT-NODE@ MAKI:EX-OUT@ 1 >I 6 T=
ONNX:OUT-NODE@ MAKI:EX-OUT@ 2 >I 9 T=
ONNX:OUT-NODE@ MAKI:EX-OUT@ 3 >I 12 T=

\ ---- Reshape: shape from the INT64 constant, host-executed ------------------------
MODEL-RS  ONNX:ENC$ ONNX:IMPORT
MAKI:MIR-N@ 1 T=
0 MAKI:MIR-OP@ MAKI:OPKIND>N MAKI:OP-RESHAPE T=
0 MAKI:MIR-ROWS@ 1 T=  0 MAKI:MIR-COLS@ 4 T=    \ target [1,4] read from the shape initializer
ONNX:IN# 1 T=                                   \ sh is an int64 constant, not a runtime input
1.0 XB 0 T-SET  2.0 XB 1 T-SET  3.0 XB 2 T-SET  4.0 XB 3 T-SET
MAKI:EX-RESET  ONNX:BIND-INITS  XB 0 ONNX:IN-SLOT@ MAKI:EX-BIND  MAKI:EX-RUN
ONNX:OUT-NODE@ MAKI:EX-OUT@ 0 >I 1 T=           \ row-major reshape preserves order
ONNX:OUT-NODE@ MAKI:EX-OUT@ 1 >I 2 T=
ONNX:OUT-NODE@ MAKI:EX-OUT@ 2 >I 3 T=
ONNX:OUT-NODE@ MAKI:EX-OUT@ 3 >I 4 T=

\ ---- Transpose: perm [1,0], 2x3 -> 3x2, host-executed -----------------------------
MODEL-TR  ONNX:ENC$ ONNX:IMPORT
MAKI:MIR-N@ 1 T=
0 MAKI:MIR-OP@ MAKI:OPKIND>N MAKI:OP-TRANSPOSE T=
0 MAKI:MIR-ROWS@ 3 T=  0 MAKI:MIR-COLS@ 2 T=
1.0 XB 0 T-SET  2.0 XB 1 T-SET  3.0 XB 2 T-SET
4.0 XB 3 T-SET  5.0 XB 4 T-SET  6.0 XB 5 T-SET
MAKI:EX-RESET  ONNX:BIND-INITS  XB 0 ONNX:IN-SLOT@ MAKI:EX-BIND  MAKI:EX-RUN
ONNX:OUT-NODE@ MAKI:EX-OUT@ 0 >I 1 T=           \ [[1,2,3],[4,5,6]]^T = [[1,4],[2,5],[3,6]]
ONNX:OUT-NODE@ MAKI:EX-OUT@ 1 >I 4 T=
ONNX:OUT-NODE@ MAKI:EX-OUT@ 2 >I 2 T=
ONNX:OUT-NODE@ MAKI:EX-OUT@ 3 >I 5 T=
ONNX:OUT-NODE@ MAKI:EX-OUT@ 4 >I 3 T=
ONNX:OUT-NODE@ MAKI:EX-OUT@ 5 >I 6 T=

\ ---- Concat: axis 0 row-append, 2x2 + 1x2 -> 3x2, host-executed -------------------
MODEL-CC  ONNX:ENC$ ONNX:IMPORT
MAKI:MIR-N@ 1 T=
0 MAKI:MIR-OP@ MAKI:OPKIND>N MAKI:OP-CONCAT T=
0 MAKI:MIR-ROWS@ 3 T=  0 MAKI:MIR-COLS@ 2 T=
1.0 XB 0 T-SET  2.0 XB 1 T-SET  3.0 XB 2 T-SET  4.0 XB 3 T-SET
MAKI:EX-RESET  ONNX:BIND-INITS  XB 0 ONNX:IN-SLOT@ MAKI:EX-BIND  MAKI:EX-RUN
ONNX:OUT-NODE@ MAKI:EX-OUT@ 0 >I 1 T=           \ x rows then the cc row [10,20]
ONNX:OUT-NODE@ MAKI:EX-OUT@ 1 >I 2 T=
ONNX:OUT-NODE@ MAKI:EX-OUT@ 2 >I 3 T=
ONNX:OUT-NODE@ MAKI:EX-OUT@ 3 >I 4 T=
ONNX:OUT-NODE@ MAKI:EX-OUT@ 4 >I 10 T=
ONNX:OUT-NODE@ MAKI:EX-OUT@ 5 >I 20 T=

\ ---- Gemm transB=1 (the common PyTorch Linear export): TRANSPOSE + MATMUL ---------
MODEL-GTB  ONNX:ENC$ ONNX:IMPORT
MAKI:MIR-N@ 2 T=
0 MAKI:MIR-OP@ MAKI:OPKIND>N MAKI:OP-TRANSPOSE T=            \ B is transposed by an inserted movement node
1 MAKI:MIR-OP@ MAKI:OPKIND>N MAKI:OP-MATMUL T=
ONNX:OUT-NODE@ 1 T=
1 MAKI:MIR-ROWS@ 2 T=  1 MAKI:MIR-COLS@ 2 T=
1.0 XB 0 T-SET  2.0 XB 1 T-SET  3.0 XB 2 T-SET
4.0 XB 3 T-SET  5.0 XB 4 T-SET  6.0 XB 5 T-SET
MAKI:EX-RESET  ONNX:BIND-INITS  XB 0 ONNX:IN-SLOT@ MAKI:EX-BIND  MAKI:EX-RUN
ONNX:OUT-NODE@ MAKI:EX-OUT@ 0 >I 4 T=          \ x . wt^T, wt=[[1,0,1],[0,1,0]]
ONNX:OUT-NODE@ MAKI:EX-OUT@ 1 >I 2 T=
ONNX:OUT-NODE@ MAKI:EX-OUT@ 2 >I 10 T=
ONNX:OUT-NODE@ MAKI:EX-OUT@ 3 >I 5 T=

\ ---- Gemm alpha=2 beta=0: MATMUL + SCALE (synthetic 1x1), C dropped ----------------
MODEL-GAB  ONNX:ENC$ ONNX:IMPORT
MAKI:MIR-N@ 2 T=
0 MAKI:MIR-OP@ MAKI:OPKIND>N MAKI:OP-MATMUL T=
1 MAKI:MIR-OP@ MAKI:OPKIND>N MAKI:OP-SCALE T=               \ alpha=2 -> an inserted scale node
ONNX:OUT-NODE@ 1 T=
1.0 XB 0 T-SET  2.0 XB 1 T-SET  3.0 XB 2 T-SET  4.0 XB 3 T-SET
MAKI:EX-RESET  ONNX:BIND-INITS  XB 0 ONNX:IN-SLOT@ MAKI:EX-BIND  MAKI:EX-RUN
ONNX:OUT-NODE@ MAKI:EX-OUT@ 0 >I 38 T=          \ 2 * (x . w), w=[[5,6],[7,8]]; beta=0 drops b
ONNX:OUT-NODE@ MAKI:EX-OUT@ 1 >I 44 T=
ONNX:OUT-NODE@ MAKI:EX-OUT@ 2 >I 86 T=
ONNX:OUT-NODE@ MAKI:EX-OUT@ 3 >I 100 T=

\ ---- fail closed -------------------------------------------------------------------
' TRY-DYN      E-ONNX-DYNSHAPE TTHROWS
' TRY-CONV     E-MK-ONNX       TTHROWS
' TRY-TOPO     E-ONNX-TOPO     TTHROWS
' TRY-GEMMATTR E-ONNX-ATTR     TTHROWS
' TRY-AXIS     E-ONNX-ATTR     TTHROWS
' TRY-ADDATTR  E-ONNX-ATTR     TTHROWS
' TRY-BADATTR  E-ONNX-ATTR     TTHROWS
' TRY-RANK3    E-ONNX-RANK     TTHROWS
' TRY-I64DT    E-ONNX-DTYPE    TTHROWS
' TRY-ADDCOL   E-ONNX-SHAPE    TTHROWS
' TRY-ADDRAGGED E-ONNX-SHAPE   TTHROWS
' TRY-RSDYN    E-ONNX-DYNSHAPE TTHROWS
' TRY-BADPERM  E-ONNX-ATTR     TTHROWS
' TRY-PERM3    E-ONNX-ATTR     TTHROWS
' TRY-2OUT     E-ONNX-OUTPUT   TTHROWS
' TRY-OUTMID   E-ONNX-OUTPUT   TTHROWS
' TRY-NOGRAPH  E-ONNX-NOGRAPH  TTHROWS
' TRY-SSA      E-ONNX-NAME     TTHROWS
' TRY-NODATA   E-ONNX-DATA     TTHROWS
' TRY-LONGNAME E-ONNX-CAP      TTHROWS
' TRY-ARITY    E-ONNX-ARITY    TTHROWS
' TRY-TRUNC    E-PB-TRUNC      TTHROWS

T-REPORT

end-package
