\ maki/mha-block-test.f - the trainable batched multi-head attention sublayer
\ (maki/mha.f) exercised in TRAINING: repeated-batch gradient accumulation, an
\ end-to-end block gradcheck, and a GPT-style transformer block (attention sublayer +
\ MLP sublayer, each with its own residual + LM head) that trains deterministically.
\ This is the integration proof the dot asks for in this lane's OWN file (distinct from
\ maki/examples/nanogpt/gptblock-attn-test.f, which the concurrent lane owns and which
\ uses the single-head FOLDED attention, not this batched multi-head sublayer).
\
\   (A) ACCUMULATION - the backward is linear in the seed and has no cross-batch mixing,
\       so the full-batch weight gradient equals the SUM of the per-batch gradients
\       (seed masked to one batch at a time). This is the microbatch/batch-loop trainer
\       idiom: per-batch grads accumulate additively.
\   (B) BLOCK GRADCHECK - the hand-composed block backward (LM head + MLP sublayer +
\       MHA-BWD) matches central FD on the block loss, for representative params.
\   (C) TRAINING - Adam over every block parameter more than HALVES the mean cross-
\       entropy on an integer copy task, deterministically (committed milli locks) and
\       run-twice bit-identical.
\
\ maki -> habu only.

require lib/test.f
require lib/float.f
require maki/gradcheck.f              \ GC-CLOSE? / GC-H
require maki/matmul.f                 \ MATMUL / MATMUL-DX / MATMUL-DW
require maki/gelu.f                   \ GELU-F / GELU-BWD
require maki/loss-tensor.f            \ TT-XENT / TT-XENT-SEED
require maki/optim-tensor.f           \ TT-ADAM!
require maki/mha.f

T-RESET

package MAKI

\ ---- block shapes: rows BT = B*T, channels C, MLP hidden HH, vocab VV -----------------
MHA-BT constant BKT                    \ B*T rows
#MC constant BKC                       \ channels C
8 constant BKH                         \ MLP hidden
6 constant BKV                         \ vocab
BKT BKC * constant BK-XN               \ (BT,C) elements
BKC BKC * constant BK-WN               \ (C,C) per-Q/K/V weight (reference block size)
BK-WN 3 * constant BK-WQKVN            \ (C,3C) fused c_attn projection weight
BKC 3 * constant BK-3C                 \ fused bias width 3C
BKT BKV * constant BK-ON               \ logit elements

\ ---- trainable params ----------------------------------------------------------------
create BK-X  BK-XN cells allot
create BK-WQKV BK-WQKVN cells allot   create BK-BQKV BK-3C cells allot   \ fused c_attn projection
create BK-WO BK-WN cells allot   create BK-BO BKC cells allot
create BK-W1 BKC BKH * cells allot   create BK-B1 BKH cells allot
create BK-W2 BKH BKC * cells allot   create BK-B2 BKC cells allot
create BK-WL BKC BKV * cells allot   create BK-BL BKV cells allot

\ ---- saved activations ---------------------------------------------------------------
create BK-H   BK-XN cells allot        \ attention sublayer output (BT,C)
create BK-HID BKT BKH * cells allot     \ MLP pre-gelu (BT,HH)
create BK-A   BKT BKH * cells allot     \ MLP post-gelu
create BK-M   BK-XN cells allot         \ MLP second linear (BT,C)
create BK-Z   BK-XN cells allot         \ MLP residual stream (BT,C)
create BK-LOG BK-ON cells allot         \ logits (BT,V)
create BK-TGT BKT cells allot           \ integer targets
create BK-SEED BK-ON cells allot        \ CE seed cotangent

\ ---- gradients -----------------------------------------------------------------------
create BK-DX  BK-XN cells allot
create BK-DWQKV BK-WQKVN cells allot   create BK-DBQKV BK-3C cells allot
create BK-DWO BK-WN cells allot   create BK-DBO BKC cells allot
create BK-DW1 BKC BKH * cells allot   create BK-DB1 BKH cells allot
create BK-DW2 BKH BKC * cells allot   create BK-DB2 BKC cells allot
create BK-DWL BKC BKV * cells allot   create BK-DBL BKV cells allot
create BK-DZ  BK-XN cells allot   create BK-DM BK-XN cells allot
create BK-DA  BKT BKH * cells allot   create BK-DHID BKT BKH * cells allot
create BK-DHM BK-XN cells allot   create BK-DH BK-XN cells allot

: BK-ROWBIAS ( ptr r n n ptr r -- ) {: yb:ptr rows:n cols:n bb:ptr :}
   rows 0 ?do  cols 0 ?do  yb j cols * i + T-GET  bb i T-GET f+  yb j cols * i + T-SET  loop  loop ;
: BK-COLSUM ( ptr r n n ptr r -- ) {: sb:ptr rows:n cols:n db:ptr :}
   cols 0 ?do  i {: c:n :}  0.0  rows 0 ?do  sb i cols * c + T-GET  f+  loop  db c T-SET  loop ;
: BK-COPY ( ptr r ptr r n -- ) {: s:ptr d:ptr n:n :}  n 0 ?do  s i T-GET  d i T-SET  loop ;
: BK-ADD ( ptr r ptr r ptr r n -- ) {: a:ptr b:ptr d:ptr n:n :}  n 0 ?do  a i T-GET  b i T-GET  f+  d i T-SET  loop ;

\ ---- deterministic LCG init ----------------------------------------------------------
variable BK-RNG
: BK-NEXT ( -- r )  BK-RNG @ 1664525 * 1013904223 + $FFFFFFFF and dup BK-RNG !  s>f 4294967296.0 f/ ;
: BK-UNIT ( -- r )  BK-NEXT 2.0 f* 1.0 f- ;
: BK-SMALL ( ptr r n -- ) {: p:ptr n:n :}  n 0 ?do  BK-UNIT 0.1 f*  p i T-SET  loop ;
\ fill the (C,C) column block at `base` of the fused (C,3C) weight with C*C draws in the
\ SAME row-major order BK-SMALL uses on a standalone (C,C) weight, so the Q/K/V blocks get
\ element-for-element the values the three separate weights got - the fused init is a bit-
\ identical rearrangement of the three-projection init (training trajectory is preserved).
: BK-SMALL-QKVBLK ( n -- ) {: base:n :}
   BKC 0 ?do  BKC 0 ?do  BK-UNIT 0.1 f*  BK-WQKV j BK-3C * base + i +  T-SET  loop  loop ;
: BK-SMALL-QKVBIAS ( n -- ) {: base:n :}
   BKC 0 ?do  BK-UNIT 0.1 f*  BK-BQKV base i +  T-SET  loop ;
: BK-INIT ( -- )
   $C0FFEE BK-RNG !
   BK-X BK-XN BK-SMALL
   0       BK-SMALL-QKVBLK  0       BK-SMALL-QKVBIAS      \ Q block draws, then Q bias (baseline WQ,BQ order)
   BKC     BK-SMALL-QKVBLK  BKC     BK-SMALL-QKVBIAS      \ K block, K bias
   BKC 2 * BK-SMALL-QKVBLK  BKC 2 * BK-SMALL-QKVBIAS      \ V block, V bias
   BK-WO BK-WN BK-SMALL  BK-BO BKC BK-SMALL
   BK-W1 BKC BKH * BK-SMALL  BK-B1 BKH BK-SMALL  BK-W2 BKH BKC * BK-SMALL  BK-B2 BKC BK-SMALL
   BK-WL BKC BKV * BK-SMALL  BK-BL BKV BK-SMALL
   BKT 0 ?do  i BKV mod s>f  BK-TGT i T-SET  loop ;

\ ---- forward: attention sublayer (mine) -> MLP sublayer -> LM head --------------------
: BK-FWD ( -- )
   BK-X BK-WQKV BK-BQKV BK-WO BK-BO BK-H MHA-FWD     \ H = attn(X)+X (fused c_attn projection)
   BK-H BK-W1 BK-HID  BKT BKC BKH  MATMUL   BK-HID BKT BKH BK-B1 BK-ROWBIAS
   BKT BKH * 0 ?do  BK-HID i T-GET GELU-F  BK-A i T-SET  loop
   BK-A BK-W2 BK-M  BKT BKH BKC  MATMUL   BK-M BKT BKC BK-B2 BK-ROWBIAS
   BK-M BK-H BK-Z BK-XN BK-ADD                                          \ Z = MLP(H) + H
   BK-Z BK-WL BK-LOG  BKT BKC BKV  MATMUL   BK-LOG BKT BKV BK-BL BK-ROWBIAS ;

: BK-INVR ( -- r )  1.0 BKT s>f f/ ;
\ seed the mean-scaled (softmax - onehot) cotangent; return mean CE
: BK-LOSS ( -- r )
   BK-LOG BK-TGT BK-SEED  BKT BKV BKT  LOSS:TT-XENT-SEED
   BK-ON 0 ?do  BK-SEED i T-GET BK-INVR f*  BK-SEED i T-SET  loop
   BK-LOG BKT BKV BK-TGT BKT  LOSS:TT-XENT  BK-INVR f* ;

\ ---- backward: LM head -> MLP sublayer -> MHA-BWD (reads its saved tape) --------------
: BK-BWD ( -- )
   \ LM head: logits = Z.WL + bL
   BK-Z BK-SEED BK-DWL  BKT BKC BKV  MATMUL-DW
   BK-SEED BKT BKV BK-DBL BK-COLSUM
   BK-SEED BK-WL BK-DZ  BKT BKC BKV  MATMUL-DX
   \ MLP sublayer: Z = (A.W2 + b2) + H ; A = gelu(H.W1 + b1)
   BK-DZ BK-DM BK-XN BK-COPY                                            \ dM = dZ
   BK-A BK-DM BK-DW2  BKT BKH BKC  MATMUL-DW
   BK-DM BKT BKC BK-DB2 BK-COLSUM
   BK-DM BK-W2 BK-DA  BKT BKH BKC  MATMUL-DX
   BKT BKH * 0 ?do  BK-DA i T-GET  BK-HID i T-GET  GELU-BWD  BK-DHID i T-SET  loop
   BK-H BK-DHID BK-DW1  BKT BKC BKH  MATMUL-DW
   BK-DHID BKT BKH BK-DB1 BK-COLSUM
   BK-DHID BK-W1 BK-DHM  BKT BKC BKH  MATMUL-DX
   BK-DZ BK-DHM BK-DH BK-XN BK-ADD                                      \ dH = dZ + dMLP (residual)
   \ attention sublayer
   BK-X BK-WQKV BK-WO BK-DH
   BK-DX BK-DWQKV BK-DBQKV BK-DWO BK-DBO MHA-BWD ;

\ ============ (A) repeated-batch gradient accumulation ================================
\ full-batch dWqkv == sum of the per-batch dWqkv (seed masked to one batch's rows at a time).
create BK-DWQKV-FULL BK-WQKVN cells allot   create BK-DWQKV-B0 BK-WQKVN cells allot
create BK-DWQKV-SUM  BK-WQKVN cells allot
#MQ BKV * constant BK-BATCH-ON          \ logit rows of one batch = T*V
: BK-MASK-SEED-B1 ( -- )  BK-BATCH-ON 0 ?do  0.0 BK-SEED BK-BATCH-ON i + T-SET  loop ;   \ zero batch 1 rows
: BK-MASK-SEED-B0 ( -- )  BK-BATCH-ON 0 ?do  0.0 BK-SEED i T-SET  loop ;                 \ zero batch 0 rows
: BK-ACCUM? ( -- bool )
   BK-INIT  BK-FWD  BK-LOSS drop
   BK-BWD  BK-DWQKV BK-DWQKV-FULL BK-WQKVN BK-COPY             \ full-batch dWqkv
   BK-FWD  BK-LOSS drop  BK-MASK-SEED-B1                        \ re-seed, keep only batch 0
   BK-Z BK-SEED BK-DWL BKT BKC BKV MATMUL-DW  BK-SEED BK-WL BK-DZ BKT BKC BKV MATMUL-DX
   BK-DZ BK-DM BK-XN BK-COPY  BK-A BK-DM BK-DW2 BKT BKH BKC MATMUL-DW  BK-DM BK-W2 BK-DA BKT BKH BKC MATMUL-DX
   BKT BKH * 0 ?do  BK-DA i T-GET BK-HID i T-GET GELU-BWD BK-DHID i T-SET  loop
   BK-DHID BK-W1 BK-DHM BKT BKC BKH MATMUL-DX  BK-DZ BK-DHM BK-DH BK-XN BK-ADD
   BK-X BK-WQKV BK-WO BK-DH  BK-DX BK-DWQKV BK-DBQKV BK-DWO BK-DBO MHA-BWD
   BK-DWQKV BK-DWQKV-B0 BK-WQKVN BK-COPY                       \ batch-0-only dWqkv
   BK-FWD  BK-LOSS drop  BK-MASK-SEED-B0                        \ re-seed, keep only batch 1
   BK-Z BK-SEED BK-DWL BKT BKC BKV MATMUL-DW  BK-SEED BK-WL BK-DZ BKT BKC BKV MATMUL-DX
   BK-DZ BK-DM BK-XN BK-COPY  BK-A BK-DM BK-DW2 BKT BKH BKC MATMUL-DW  BK-DM BK-W2 BK-DA BKT BKH BKC MATMUL-DX
   BKT BKH * 0 ?do  BK-DA i T-GET BK-HID i T-GET GELU-BWD BK-DHID i T-SET  loop
   BK-DHID BK-W1 BK-DHM BKT BKC BKH MATMUL-DX  BK-DZ BK-DHM BK-DH BK-XN BK-ADD
   BK-X BK-WQKV BK-WO BK-DH  BK-DX BK-DWQKV BK-DBQKV BK-DWO BK-DBO MHA-BWD
   BK-DWQKV-B0 BK-DWQKV BK-DWQKV-SUM BK-WQKVN BK-ADD           \ b0 + b1
   BK-DWQKV-FULL BK-DWQKV-SUM BK-WQKVN T-DIST2  1000000.0 f* 0.5 f+ f>s 0= ;
BK-ACCUM? TTRUE

\ ============ (B) block gradcheck: analytic vs central FD on representative params =====
: BK-FD ( ptr r n -- r ) {: pb:ptr e:n :}
   pb e T-GET {: base:r :}
   base GC-H f+ pb e T-SET  BK-FWD BK-LOSS {: lp:r :}
   base GC-H f- pb e T-SET  BK-FWD BK-LOSS {: lm:r :}
   base pb e T-SET  lp lm f- GC-H 2.0 f* f/ ;
: BK-GC? ( ptr r n ptr r -- bool ) {: pb:ptr n:n gb:ptr :}
   true  n 0 ?do  gb i T-GET  pb i BK-FD  GC-CLOSE? 0= if drop false leave then  loop ;
BK-INIT  BK-FWD  BK-LOSS drop  BK-BWD          \ analytic grads for the block loss
BK-X  BK-XN BK-DX  BK-GC? TTRUE                 \ dX flows through the whole block
BK-WL BKC BKV * BK-DWL BK-GC? TTRUE             \ LM head weight
BK-W1 BKC BKH * BK-DW1 BK-GC? TTRUE             \ MLP first linear
BK-WQKV BK-WQKVN BK-DWQKV BK-GC? TTRUE          \ fused attention QKV projection (through the block)
BK-BO BKC BK-DBO BK-GC? TTRUE                   \ attention output bias (through the block)

\ ============ (C) training: Adam more than halves mean CE, deterministic, run-twice ====
\ Adam state (step count + decay powers) + per-param moment buffers.
: BK-BET1 ( -- r ) 0.9 ;  : BK-BET2 ( -- r ) 0.999 ;  : BK-EPS ( -- r ) 0.00000001 ;
variable BK-T  variable BK-B1T  variable BK-B2T
: BK-C1 ( -- r ) 1.0 BK-B1T @ f- ;  : BK-C2 ( -- r ) 1.0 BK-B2T @ f- ;
: BK-TICK ( -- )  BK-T @ 1+ BK-T !  BK-B1T @ BK-BET1 f* BK-B1T !  BK-B2T @ BK-BET2 f* BK-B2T ! ;
: BK-LR ( -- r )  0.03 ;

\ 10 params (fused c_attn folds Q/K/V into one weight + one bias): buffer, grad, length, moment m, moment v.
create BK-MBUF 2400 cells allot   variable BK-MOFF
: BK-M+ ( n -- ptr r )  {: n:n :}  BK-MOFF @ {: o:n :}  o n + BK-MOFF !  BK-MBUF o cells + ;
create BK-VBUF 2400 cells allot   variable BK-VOFF
: BK-V+ ( n -- ptr r )  {: n:n :}  BK-VOFF @ {: o:n :}  o n + BK-VOFF !  BK-VBUF o cells + ;
\ the moment slabs (assigned once, in param order)
0 BK-MOFF !  0 BK-VOFF !
BK-XN BK-M+ constant MBX   BK-XN BK-V+ constant VBX
BK-WQKVN BK-M+ constant MBWQKV  BK-WQKVN BK-V+ constant VBWQKV   BK-3C BK-M+ constant MBBQKV  BK-3C BK-V+ constant VBBQKV
BK-WN BK-M+ constant MBWO  BK-WN BK-V+ constant VBWO   BKC BK-M+ constant MBBO  BKC BK-V+ constant VBBO
BKC BKH * BK-M+ constant MBW1  BKC BKH * BK-V+ constant VBW1   BKH BK-M+ constant MBB1  BKH BK-V+ constant VBB1
BKH BKC * BK-M+ constant MBW2  BKH BKC * BK-V+ constant VBW2   BKC BK-M+ constant MBB2  BKC BK-V+ constant VBB2
BKC BKV * BK-M+ constant MBWL  BKC BKV * BK-V+ constant VBWL   BKV BK-M+ constant MBBL  BKV BK-V+ constant VBBL

: BK-ARESET ( -- )
   0 BK-T !  1.0 BK-B1T !  1.0 BK-B2T !
   0.0 BK-MBUF BK-MOFF @ T-FILL  0.0 BK-VBUF BK-VOFF @ T-FILL ;
: BK-UPD ( ptr r ptr r ptr r ptr r n -- ) {: p:ptr g:ptr m:ptr v:ptr n:n :}
   BK-LR BK-BET1 BK-BET2 BK-EPS BK-C1 BK-C2  p g m v n  OPTIM:TT-ADAM! ;
: BK-STEP ( -- r )
   BK-FWD  BK-LOSS {: loss:r :}  BK-BWD  BK-TICK       \ X is the fixed block input (frozen); train the weights
   BK-WQKV BK-DWQKV MBWQKV VBWQKV BK-WQKVN BK-UPD   BK-BQKV BK-DBQKV MBBQKV VBBQKV BK-3C BK-UPD
   BK-WO BK-DWO MBWO VBWO BK-WN BK-UPD   BK-BO BK-DBO MBBO VBBO BKC BK-UPD
   BK-W1 BK-DW1 MBW1 VBW1 BKC BKH * BK-UPD   BK-B1 BK-DB1 MBB1 VBB1 BKH BK-UPD
   BK-W2 BK-DW2 MBW2 VBW2 BKH BKC * BK-UPD   BK-B2 BK-DB2 MBB2 VBB2 BKC BK-UPD
   BK-WL BK-DWL MBWL VBWL BKC BKV * BK-UPD   BK-BL BK-DBL MBBL VBBL BKV BK-UPD
   loss ;
: BK-MILLI ( r -- n )  1000.0 f* 0.5 f+ f>s ;
variable BK-IL   variable BK-FL
: BK-RUN ( n -- ) {: n:n :}
   n 0 ?do  BK-STEP {: l:r :}  i 0= if l BK-IL ! then  i n 1- = if l BK-FL ! then  loop ;

BK-INIT  BK-ARESET  40 BK-RUN
BK-FL @ BK-IL @ f< TTRUE                        \ loss decreased
BK-FL @ BK-IL @ 0.5 f* f< TTRUE                 \ at least halved
BK-IL @ BK-MILLI  1789 T=                        \ committed initial mean CE (determinism lock)
BK-FL @ BK-MILLI   532 T=                        \ committed final mean CE (determinism lock)
BK-INIT  BK-ARESET  40 BK-RUN  BK-FL @ BK-MILLI 532 T=    \ run-twice bit-identical

;package

T-REPORT
