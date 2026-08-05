\ maki/segment.f - segment (block-diagonal) causal self-attention host reference +
\ its VJP. Rows = B*T with B outermost: sequence b is the contiguous row block
\ [b*T, b*T+T). Attention contracts strictly within each T-block and (optionally)
\ causally inside it, so a (B*T)x(B*T) score is never materialized - only a per-block
\ T x T scratch. Composes the checked primitives MM-NT (scores, maki/attention.f),
\ the causal / plain row softmax (maki/causal.f, maki/attention.f), and MATMUL
\ (maki/matmul.f), one block at a time over row offsets b*T*d. The block width T and
\ the causal flag ride the IR attrs cell; this file owns the pack/unpack and the
\ buffer references the executor / backward dispatch bind.
\
\ The forward mirrors ATTN-FWD block by block; the backward mirrors ATTN-BWD block
\ by block but RECOMPUTES the per-block attention A from Q,K (the fused op saves no
\ interior), then applies the causal softmax-jvp (maki/causal.f CAUSAL-SMBWD-ROWS)
\ when masked - so masked positions carry zero cotangent structurally, no stored
\ mask. The VJP writes dQ,dK,dV blocks into one combined [dQ;dK;dV] row buffer so
\ the IR backward slices one gradient per input.
\
\ Fail closed: T must be 1..SEG-TMAX (the host T x T scratch bound) and must divide
\ the row count exactly (a ragged block is a caller bug, not a maskable state).
\ maki -> habu only; segment owns -5138..-5139.

require maki/array.f
require maki/attention.f
require maki/causal.f
require maki/matmul.f

-5138 constant E-SEG-SHAPE   \ block width T < 1, > rows, or not an exact divisor of rows
-5139 constant E-SEG-CAP     \ block width T exceeds the host T x T scratch bound

package MAKI

\ ---- attrs cell: block width T (low 20 bits) + causal flag (bit 20) ----------
\ A compute op's attrs cell is free-form (movement ops pack MV-* here instead); the
\ segment op stores T and whether the within-block softmax is causal.
$FFFFF constant SEG-TMASK       \ 20-bit block width (max 1048575)
20     constant SEG-CAUSAL-SH   \ causal flag bit

public

: SEG-PACK ( n bool -- n ) {: t:n c:bool :}
   t SEG-TMASK and  c if 1 SEG-CAUSAL-SH lshift or then ;
: SEG-T@ ( n -- n )  SEG-TMASK and ;
: SEG-CAUSAL@ ( n -- bool )  SEG-CAUSAL-SH rshift 1 and 0= 0= ;

private

\ ---- per-block T x T scratch (host goldens are toy-scale; PROMOTE tiles real T) --
64 constant SEG-TMAX                       \ largest block width the host scratch holds
SEG-TMAX SEG-TMAX * constant SEG-TT        \ one T x T score block
create SEG-SCRATCH  SEG-TT 4 * cells allot \ sb | ab | dab | dsb
: SEG-SB  ( -- ptr a )  SEG-SCRATCH 0          T-AT ;   \ scores S
: SEG-AB  ( -- ptr a )  SEG-SCRATCH SEG-TT      T-AT ;  \ attention A (softmax of S)
: SEG-DAB ( -- ptr a )  SEG-SCRATCH SEG-TT 2 *  T-AT ;  \ dA
: SEG-DSB ( -- ptr a )  SEG-SCRATCH SEG-TT 3 *  T-AT ;  \ dS

\ block width must be a real prefix of the rows: 1..SEG-TMAX and an exact divisor
: SEG-GUARD ( n n -- ) {: rows:n t:n :}
   t 1 < t rows > or  rows t mod 0= 0= or if E-SEG-SHAPE throw then
   t SEG-TMAX > if E-SEG-CAP throw then ;

: SEG-1/SQRT ( n -- r ) {: d:n :}  1.0 d s>f fsqrt f/ ;   \ 1/sqrt(d) attention scale

\ one block's scaled + (causal) softmaxed scores A into SEG-AB (T x T)
: SEG-ATTN! ( ptr r ptr r n n bool -- ) {: qb:ptr kb:ptr t:n d:n causal:bool :}
   qb kb SEG-SB  t t d  MM-NT
   SEG-SB  t t *  d SEG-1/SQRT  ATTN-SCALE!
   causal if SEG-SB SEG-AB t CAUSAL-SOFTMAX-ROWS
          else SEG-SB SEG-AB t ATTN-SOFTMAX-ROWS then ;

public

\ O = segment attention over rows = B*T, block width T, head dim d. Each block
\ [b*T,b*T+T) is an independent single-block attention; nothing crosses a boundary.
: SEG-ATTN-FWD ( ptr r ptr r ptr r ptr r n n n bool -- )
   {: qb:ptr kb:ptr vb:ptr ob:ptr rows:n t:n d:n causal:bool :}
   rows t SEG-GUARD
   rows t / 0 ?do
      i t * d *  {: off:n :}
      qb off T-AT  kb off T-AT  t d causal  SEG-ATTN!
      SEG-AB  vb off T-AT  ob off T-AT  t t d  MATMUL
   loop ;

private

\ one block's VJP: recompute A, then dV=A^T.dO, dA=dO.V^T, dS=softmax-jvp(dA,A) (causal
\ when masked), dS/sqrt(d), dQ=dS.K, dK=dS^T.Q. Mirrors attention.f ATTN-BWD block by
\ block; masked positions carry zero cotangent structurally via CAUSAL-SMBWD-ROWS.
: SEG-BWD! ( ptr r ptr r ptr r ptr r  ptr r ptr r ptr r  n n bool -- )
   {: qb:ptr kb:ptr vb:ptr dob:ptr  dqb:ptr dkb:ptr dvb:ptr  t:n d:n causal:bool :}
   qb kb t d causal  SEG-ATTN!                       \ recompute A into SEG-AB
   SEG-AB dob dvb  t t d  MM-TN                        \ dV = A^T . dO
   dob vb SEG-DAB  t t d  MM-NT                        \ dA = dO . V^T
   causal if SEG-DAB SEG-AB SEG-DSB t CAUSAL-SMBWD-ROWS
          else SEG-DAB SEG-AB SEG-DSB t ATTN-SMBWD-ROWS then
   SEG-DSB  t t *  d SEG-1/SQRT  ATTN-SCALE!
   SEG-DSB kb dqb  t t d  MATMUL                       \ dQ = dS . K
   SEG-DSB qb dkb  t t d  MM-TN ;                      \ dK = dS^T . Q

public

\ VJP of SEG-ATTN-FWD. gb is the combined gradient buffer 3*rows x d holding
\ [dQ ; dK ; dV] as three row regions, so the IR backward slices one grad per input.
: SEG-ATTN-BWD ( ptr r ptr r ptr r ptr r  ptr r  n n n bool -- )
   {: qb:ptr kb:ptr vb:ptr dob:ptr  gb:ptr  rows:n t:n d:n causal:bool :}
   rows t SEG-GUARD
   gb                {: dqreg:ptr :}
   gb  rows d *      T-AT {: dkreg:ptr :}
   gb  rows d * 2 *  T-AT {: dvreg:ptr :}
   rows t / 0 ?do
      i t * d *  {: off:n :}
      qb off T-AT  kb off T-AT  vb off T-AT  dob off T-AT
      dqreg off T-AT  dkreg off T-AT  dvreg off T-AT
      t d causal  SEG-BWD!
   loop ;

;package
