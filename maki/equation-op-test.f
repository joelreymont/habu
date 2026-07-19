\ maki/equation-op-test.f - the equation op-kind inside the trainable graph (dot
\ habu-equation-op-kind, docs/model-unified.md stage 1).
\
\ A SPEC:-declared einsum is ONE `equation` op-kind (maki/op-kind.f) whose attrs cell
\ carries the equation's spec-registry slot (maki/spec.f). This suite proves stage 1
\ end to end:
\   - registration: a composable (gather-free, <=2 free, <=3 plain-factor) einsum joins
\     the equation registry at SPEC: time (EQ-FIND);
\   - composition: the equation is referenced as a body op inside a MODEL: composition
\     (maki/cad.f), its operands extent-checked against the equation's per-factor
\     (rows,cols) (maki/plan-ops.f PLAN-EQUATION);
\   - execution: the captured equation node runs on the host executor (maki/executor.f
\     EX-EQUATION dispatches to the generated RUN word) and reproduces the attention
\     scores S = Q.Kᵀ of the maki/attention.f MM-NT golden, to exact numeric equality;
\   - training reject: an equation has no adjoint in stage 1, so building a backward pass
\     over an equation node is the named E-BW-NOADJ reject (stage-2 adjoints are another dot).
\
\ The composition-time extent check and the training reject are also driven directly as
\ red-first negatives. Own extents #EQA/#EQB/#EQK and EQO-* names keep this loading
\ cleanly into the shared maki/test.f image beside every other suite.

require lib/test.f
require lib/float.f
require maki/spec.f
require maki/cad.f
require maki/executor.f
require maki/backward.f
require maki/attention.f

T-RESET

package MAKI

\ L = 4 (query/key positions), d = 3 (head dim).
4 EXTENT: #EQA   4 EXTENT: #EQB   3 EXTENT: #EQK
TENSOR: EQO-Q ( #EQA #EQK )    \ Q : L x d
TENSOR: EQO-K ( #EQB #EQK )    \ K : L x d  (the transposed operand: shares the trailing d index)
TENSOR: EQO-S ( #EQA #EQB )    \ S : L x L  (attention scores)
ITENSOR: EQO-IX ( #EQA #EQA )  \ a gather witness (rows of Q), for the forward-only reject

SPEC: EQO-QK  EQO-S[eqa eqb] = EQO-Q[eqa eqk] EQO-K[eqb eqk] * +SUM eqk ;   \ S = Q.Kᵀ

\ ---- registration: the composable einsum joined the equation registry ------------
: EQO-REG? ( -- bool )
   s" EQO-QK" EQ-FIND MATCH option  none OF false ENDOF  some OF drop true ENDOF ;MATCH ;
EQO-REG? -1 T=

\ ---- composition: the equation is referenced as an op inside a MODEL: composition --
MODEL: EQO-ATTN ( q:4x3 k:4x3 -- s ) EQO-QK ;
MODEL-K 1 T=                                                        \ one node: the equation op
0 MIR-NODE-ID MIR-OP@ MAKI-OPKIND:EQUATION MAKI-OPKIND:EQ -1 T=     \ typed as the equation op-kind
0 MIR-NODE-ID MIR-ROWS@ ROWS-RAW 4 T=  0 MIR-NODE-ID MIR-COLS@ COLS-RAW 4 T=   \ output S = 4 x 4

\ ---- execution: the captured equation node reproduces the MM-NT golden exactly -----
create EQO-BQ #EQA #EQK * cells allot
create EQO-BK #EQB #EQK * cells allot
create EQO-REF #EQA #EQB * cells allot
: EQO-FILL ( -- )
   #EQA #EQK * 0 ?do  i 1+ s>f       EQO-BQ i T-SET  loop           \ Q = 1..12
   #EQB #EQK * 0 ?do  i 2 * 1+ s>f   EQO-BK i T-SET  loop ;         \ K = 1,3,5,...
EQO-FILL
EQO-BQ EQO-BK EQO-REF #EQA #EQB #EQK MM-NT                          \ golden scores S = Q.Kᵀ
EX-RESET  EQO-BQ 0 MIR-SLOT-ID EX-BIND  EQO-BK 1 MIR-SLOT-ID EX-BIND  EX-RUN
0 MIR-NODE-ID EX-OUT@ EQO-REF #EQA #EQB * T-DIST2  1000000.0 f* 0.5 f+ f>s  0 T=

\ ---- stage 2: the equation is now differentiable via derived adjoints --------------
\ The gather-free GEMM equation carries pre-derived adjoint equations (maki/spec.f
\ EQ-ADJ-DERIVE), so it is trainable (EQ-DIFF?) - and building the backward pass over the
\ captured EQO-ATTN graph now SUCCEEDS (was the E-BW-NOADJ reject in stage 1), emitting the
\ derived adjoints as ordinary equation nodes (maki/backward.f BW-STEP-EQUATION).
: EQO-DIFF? ( -- bool )
   s" EQO-QK" EQ-FIND MATCH option  none OF false ENDOF  some OF EQ-DIFF? ENDOF ;MATCH ;
EQO-DIFF? TTRUE
MODEL: EQO-ATTN ( q:4x3 k:4x3 -- s ) EQO-QK ;
BW-BUILD
BW-BWD-COUNT 0 > TTRUE                                              \ a backward region was emitted

\ ---- gather reject: a gather adjoint is scatter-add, not expressible (E-CAD-GRAD) ---
\ A gather equation registers forward-only; requesting its training adjoint is the named
\ E-CAD-GRAD reject - never a wrong gradient. The gather-free GEMM body validates cleanly.
: EQO-GATHER-ADJ ( -- )
   s" EQO-S[eqa eqb] = EQO-Q[ EQO-IX[eqa] eqk ] EQO-K[eqb eqk] * +SUM eqk" SPEC-ADJ-CHECK$ ;
' EQO-GATHER-ADJ  E-CAD-GRAD  TTHROWS
: EQO-GEMM-ADJ ( -- )
   s" EQO-S[eqa eqb] = EQO-Q[eqa eqk] EQO-K[eqb eqk] * +SUM eqk" SPEC-ADJ-CHECK$ ;
' EQO-GEMM-ADJ catch 0 T=                                          \ differentiable body: no reject

\ ---- composition-time extent-check negative (drive the composer directly) ----------
\ An operand whose contraction span (cols 5) differs from the equation's #EQK = 3 is the
\ named E-TV-SHAPE reject, exactly like the matmul inner-dim check.
: EQO-BADOP ( -- )
   TENSOR:TV-RESET
   #EQA 5 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW SPACE-HOST TENSOR:TV-DESC       \ Q operand: 4 x 5
   #EQB #EQK SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW SPACE-HOST TENSOR:TV-DESC    \ K operand: 4 x 3
   s" EQO-QK" EQ-FIND MATCH option
      none OF E-SPEC-SYNTAX throw ENDOF
      some OF PLAN-EQUATION ENDOF
   ;MATCH drop ;
' EQO-BADOP  E-TV-SHAPE  TTHROWS

;package

T-REPORT
