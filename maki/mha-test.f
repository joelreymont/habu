\ maki/mha-test.f - checked tests for the multi-head causal self-attention sublayer
\ (maki/mha.f), authored as SPEC: contraction lines composed with named row ops.
\
\ The acceptance vehicle (derivation 1) is exact numeric parity: MHA-FWD is proven
\ equal, cell for cell, to a reference COMPOSED FROM THE EXISTING GOLDENS in this file
\ - per-head X.W projections and A.V via MATMUL (maki/matmul.f), scores via MM-NT
\ (maki/attention.f), the 1/sqrt(hd) scale (ATTN-SCALE!), the causal-masked row
\ softmax (maki/causal.f CAUSAL-SOFTMAX-ROWS), and a direct head-merge output-
\ projection loop. The SPEC:-generated contractions carry no reference of their own,
\ so the match validates them against hand-composed math.
\
\ Beyond parity: (a) per-head slicing is proven - the head-major MHA-O-SCRATCH blocks
\ match the per-head reference, and perturbing one head's weights leaves the other
\ head's output block untouched; (b) the causal mask is proven - perturbing a future
\ position leaves every earlier query's output identical (a masked key cannot
\ influence it); (c) every malformed spec is a NAMED throw (SPEC-CHECK$), including
\ the 3-free-index shape that is exactly why the head split cannot be one rank-3
\ SPEC output; (d) a swapped-operand candidate is a CHECKER reject (SPEC-CAND$), on
\ both the transposed Q.K^T and the rank-3 head-merge contraction.
\
\ Assertions run at top level (interpret mode) like maki/spec-attention-test.f, so
\ STR=/0= bools read directly against -1. Buffers carry an MHAT- prefix; the sublayer
\ owns extents #HQ/#HN/#HC/#HD/#HH, so this loads cleanly into the shared maki/test.f.

require lib/test.f
require test/checker-assert.f
require maki/mha.f

T-RESET

package MAKI

\ ---- test buffers (T=4, C=6, H=2, hd=3; head-major weights and activations) -------
create MHAT-X   #HQ #HC * cells allot          \ input (T,C)
create MHAT-WQ  #HH #HC * #HD * cells allot     \ head-major query weight (H,C,hd)
create MHAT-WK  #HH #HC * #HD * cells allot
create MHAT-WV  #HH #HC * #HD * cells allot
create MHAT-WO  #HH #HD * #HC * cells allot     \ head-major output weight (H,hd,C)
create MHAT-BO  #HC cells allot                 \ output bias (C)
create MHAT-Y   #HQ #HC * cells allot           \ sublayer output (T,C)
create MHAT-Y1  #HQ #HC * cells allot           \ causal test: baseline output
create MHAT-Y2  #HQ #HC * cells allot           \ causal test: perturbed output

create MHAT-QH  #HQ #HD * cells allot           \ reference per-head scratch
create MHAT-KH  #HN #HD * cells allot
create MHAT-VH  #HN #HD * cells allot
create MHAT-S   #HQ #HN * cells allot
create MHAT-A   #HQ #HN * cells allot
create MHAT-O   #HH #HQ * #HD * cells allot      \ reference head-major output (H,T,hd)
create MHAT-RY  #HQ #HC * cells allot            \ reference sublayer output (T,C)
create MHAT-OSAVE #HH #HQ * #HD * cells allot     \ per-head test: saved O snapshot

: MHAT-COPY ( ptr a ptr a n -- ) {: src:ptr dst:ptr n:n :}
   n 0 ?do  src i T-GET  dst i T-SET  loop ;

: MHAT-SCALE ( -- r )  1.0 #HD s>f fsqrt f/ ;   \ 1/sqrt(hd), matching maki/mha.f

: MHAT-FILL ( -- )
   #HQ #HC *        0 ?do  i 1 + s>f 23.0 f/  MHAT-X  i T-SET  loop
   #HH #HC * #HD *  0 ?do  i 2 + s>f 29.0 f/  MHAT-WQ i T-SET  loop
   #HH #HC * #HD *  0 ?do  i 3 + s>f 31.0 f/  MHAT-WK i T-SET  loop
   #HH #HC * #HD *  0 ?do  i 5 + s>f 37.0 f/  MHAT-WV i T-SET  loop
   #HH #HD * #HC *  0 ?do  i 7 + s>f 41.0 f/  MHAT-WO i T-SET  loop
   #HC              0 ?do  i 1 + s>f 43.0 f/  MHAT-BO i T-SET  loop ;

\ reference head-merge output projection: RY[q,c] = sum_{h,d} O[h,q,d] * WO[h,d,c].
: MHAT-REF-OUT-EL ( n n -- r ) {: q:n c:n :}
   0.0
   #HH 0 ?do
      #HD 0 ?do
         MHAT-O   j #HQ * q + #HD * i +   T-GET
         MHAT-WO  j #HD * i + #HC * c +   T-GET
         f* f+
      loop
   loop ;
: MHAT-REF-OUT ( -- )
   #HQ 0 ?do #HC 0 ?do
      j i MHAT-REF-OUT-EL  MHAT-RY  j #HC * i +  T-SET
   loop loop ;

\ full core reference (composed from existing goldens): per head, project Q/K/V, score
\ + scale + causal softmax, A.V into the head-major MHAT-O; then the merge projection.
: MHAT-REF ( -- )
   #HH 0 ?do
      i #HC #HD * *  {: woff:n :}
      i #HQ #HD * *  {: boff:n :}
      MHAT-X  MHAT-WQ woff T-AT  MHAT-QH  #HQ #HC #HD  MATMUL     \ Q_h = X . WQ_h
      MHAT-X  MHAT-WK woff T-AT  MHAT-KH  #HN #HC #HD  MATMUL     \ K_h = X . WK_h
      MHAT-X  MHAT-WV woff T-AT  MHAT-VH  #HN #HC #HD  MATMUL     \ V_h = X . WV_h
      MHAT-QH MHAT-KH MHAT-S  #HQ #HN #HD  MM-NT                  \ S_h = Q_h . K_h^T
      MHAT-S  #HQ #HN *  MHAT-SCALE  ATTN-SCALE!
      MHAT-S MHAT-A #HQ  CAUSAL-SOFTMAX-ROWS
      MHAT-A MHAT-VH  MHAT-O boff T-AT  #HQ #HN #HD  MATMUL       \ O_h = A_h . V_h
   loop
   MHAT-REF-OUT ;

\ --- derivation (1): MHA-FWD matches the composed golden reference exactly ----------
: MHAT-PARITY ( -- )
   MHAT-FILL
   MHAT-REF
   MHAT-X MHAT-WQ MHAT-WK MHAT-WV MHAT-WO MHAT-Y MHA-FWD
   MHAT-Y MHAT-RY #HQ #HC * T-DIST2  1000000.0 f* 0.5 f+ f>s  0 T= ;
MHAT-PARITY

\ --- per-head slicing: the head-major output blocks match the per-head reference ----
: MHAT-OSCR-PARITY ( -- )
   MHAT-FILL
   MHAT-REF
   MHAT-X MHAT-WQ MHAT-WK MHAT-WV MHAT-WO MHAT-Y MHA-FWD
   MHA-O-SCRATCH MHAT-O #HH #HQ * #HD * T-DIST2  1000000.0 f* 0.5 f+ f>s  0 T= ;
MHAT-OSCR-PARITY

\ --- per-head slicing: perturbing head 1's weights leaves head 0's output block
\ untouched (no cross-head leak), and changes head 1's own block. -------------------
: MHAT-HEAD-INDEP ( -- )
   MHAT-FILL
   MHAT-X MHAT-WQ MHAT-WK MHAT-WV MHAT-WO MHAT-Y MHA-FWD
   MHA-O-SCRATCH MHAT-OSAVE #HH #HQ * #HD * MHAT-COPY
   #HC #HD * 0 ?do                                              \ perturb head-1 WQ block only
      MHAT-WQ  #HC #HD * i +  T-GET  50.0 f+  MHAT-WQ  #HC #HD * i +  T-SET
   loop
   MHAT-X MHAT-WQ MHAT-WK MHAT-WV MHAT-WO MHAT-Y MHA-FWD
   MHA-O-SCRATCH MHAT-OSAVE #HQ #HD * T-DIST2  1000000.0 f* 0.5 f+ f>s  0 T=   \ head 0 unchanged
   MHA-O-SCRATCH #HQ #HD * T-AT   MHAT-OSAVE #HQ #HD * T-AT   #HQ #HD *  T-DIST2  1000000.0 f* 0.5 f+ f>s  0 T<> ; \ head 1 changed
MHAT-HEAD-INDEP

\ --- causal mask: perturbing the last position leaves every earlier query's output
\ identical (a future/masked key cannot influence an earlier query), and changes the
\ last query's own output (the perturbation is real, not frozen). --------------------
: MHAT-CAUSAL ( -- )
   MHAT-FILL
   MHAT-X MHAT-WQ MHAT-WK MHAT-WV MHAT-WO MHAT-Y1 MHA-FWD
   #HC 0 ?do                                                    \ perturb X at the last position T-1
      MHAT-X  #HQ 1- #HC * i +  T-GET  100.0 f+  MHAT-X  #HQ 1- #HC * i +  T-SET
   loop
   MHAT-X MHAT-WQ MHAT-WK MHAT-WV MHAT-WO MHAT-Y2 MHA-FWD
   MHAT-Y1 MHAT-Y2  #HQ 1- #HC *  T-DIST2  1000000.0 f* 0.5 f+ f>s  0 T=       \ rows 0..T-2 identical
   MHAT-Y1 #HQ 1- #HC * T-AT   MHAT-Y2 #HQ 1- #HC * T-AT   #HC  T-DIST2  1000000.0 f* 0.5 f+ f>s  0 T<> ; \ row T-1 differs
MHAT-CAUSAL

\ --- the full sublayer: core + output bias (broadcast) + residual (add X) ------------
: MHAT-SUBLAYER ( -- )
   MHAT-FILL
   MHAT-REF                                                     \ core reference into MHAT-RY
   #HQ 0 ?do #HC 0 ?do                                          \ + bias[c] + X[q,c]
      MHAT-RY j #HC * i +  T-GET
      MHAT-BO i T-GET  f+
      MHAT-X  j #HC * i +  T-GET  f+
      MHAT-RY j #HC * i +  T-SET
   loop loop
   MHAT-X MHAT-WQ MHAT-WK MHAT-WV MHAT-WO MHAT-BO MHAT-Y MHA-SUBLAYER-FWD
   MHAT-Y MHAT-RY #HQ #HC * T-DIST2  1000000.0 f* 0.5 f+ f>s  0 T= ;
MHAT-SUBLAYER

\ --- ROW-BIAS-ADD! in isolation: every row gains the same bias vector ----------------
: MHAT-BIAS-ADD ( -- )
   MHAT-FILL
   MHAT-X MHAT-Y #HQ #HC * MHAT-COPY               \ Y = X
   MHAT-Y #HQ #HC MHAT-BO ROW-BIAS-ADD!            \ Y[r,c] += BO[c]
   #HQ 0 ?do #HC 0 ?do                             \ reference RY[r,c] = X[r,c] + BO[c]
      MHAT-X j #HC * i + T-GET   MHAT-BO i T-GET  f+   MHAT-RY j #HC * i + T-SET
   loop loop
   MHAT-Y MHAT-RY #HQ #HC * T-DIST2  1000000.0 f* 0.5 f+ f>s  0 T= ;
MHAT-BIAS-ADD

\ --- negatives: every malformed spec is a NAMED throw, no code generated -------------
: MHAT-NOSTAR   ( -- ) s" MHA-SH[hq hn] = MHA-QH[hq hd] MHA-KH[hn hd] +SUM hd" SPEC-CHECK$ ;    \ two factors, no *
: MHAT-RANK     ( -- ) s" MHA-SH[hq hn] = MHA-QH[hq] MHA-KH[hn hd] * +SUM hd" SPEC-CHECK$ ;      \ MHA-QH rank 2, one index
: MHAT-OUTRANK  ( -- ) s" MHA-Y[hq hc] = MHA-O[hh hq] MHA-WO[hh hd hc] * +SUM hh hd" SPEC-CHECK$ ; \ MHA-O rank 3, two indices
: MHAT-FREE3    ( -- ) s" MHA-O[hh hq hd] = MHA-X[hq hc] MHA-WQ[hc hd] * +SUM hc" SPEC-CHECK$ ;   \ 3 free indices: the head split is NOT one rank-3 SPEC
: MHAT-UNKEXT   ( -- ) s" MHA-SH[hq zz] = MHA-QH[hq hd] MHA-KH[zz hd] * +SUM hd" SPEC-CHECK$ ;    \ zz -> #ZZ undeclared
: MHAT-UNKTENS  ( -- ) s" MHA-SH[hq hn] = MHA-QH[hq hd] ZZ[hn hd] * +SUM hd" SPEC-CHECK$ ;        \ tensor ZZ undeclared
: MHAT-OKSPEC   ( -- ) s" MHA-SH[hq hn] = MHA-QH[hq hd] MHA-KH[hn hd] * +SUM hd" SPEC-CHECK$ ;     \ valid: no throw
' MHAT-NOSTAR   E-SPEC-SYNTAX  TTHROWS
' MHAT-RANK     E-SPEC-ARITY   TTHROWS
' MHAT-OUTRANK  E-SPEC-ARITY   TTHROWS
' MHAT-FREE3    E-SPEC-ARITY   TTHROWS
' MHAT-UNKEXT   E-SPEC-EXTENT  TTHROWS
' MHAT-UNKTENS  E-SPEC-TENSOR  TTHROWS
' MHAT-OKSPEC   0              TTHROWS

\ --- a swapped operand is a CHECKER reject: writing a variable into the wrong extent
\ slot flips ix<...> where a different extent is demanded, so the derived accessor loop
\ scores 0 (reject); the correct order scores -1 (accept). Proven on the transposed
\ Q.K^T operand AND the rank-3 head-merge contraction. -------------------------------
SPEC-CAND: MHA-COK   MHA-SH[hq hn] = MHA-QH[hq hd] MHA-KH[hn hd] * +SUM hd ;
SPEC-CAND$ CHECK-QUIET-CANDIDATE! -1 T=
SPEC-CAND: MHA-CFLIP MHA-SH[hq hn] = MHA-QH[hq hd] MHA-KH[hd hn] * +SUM hd ;   \ MHA-KH extents swapped
SPEC-CAND$ CHECK-QUIET-CANDIDATE!  0 T=
SPEC-CAND: MHA-OCOK  MHA-Y[hq hc] = MHA-O[hh hq hd] MHA-WO[hh hd hc] * +SUM hh hd ;
SPEC-CAND$ CHECK-QUIET-CANDIDATE! -1 T=
SPEC-CAND: MHA-OFLIP MHA-Y[hq hc] = MHA-O[hh hd hq] MHA-WO[hh hd hc] * +SUM hh hd ;   \ MHA-O row/col swapped
SPEC-CAND$ CHECK-QUIET-CANDIDATE!  0 T=

;package

T-REPORT
