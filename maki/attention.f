\ maki/attention.f - single-head scaled-dot-product attention forward (no batch,
\ no mask): the transformer attention block, composed from the checked primitives.
\ Q,K,V are L x d (row-major); S = Q.K^T / sqrt(d) (LxL); A = softmax_rows(S);
\ O = A.V (Lxd). The host reference the device flash-attention must match. Needs
\ maki/{array,matmul,softmax}.f (and their deps fmath/autograd). maki -> habu only.

\ S[i,j] = sum_p A[i,p]*B[j,p]   (A.B^T : both operands row-major, inner-contiguous)
: MM-NT-EL ( ptr a ptr a n n n -- r ) {: ab:ptr bb:ptr ix:n jx:n inner:n :}
   0.0  inner 0 ?do
      ab ix inner * i + T-GET   bb jx inner * i + T-GET   f*  f+
   loop ;
: MM-NT ( ptr a ptr a ptr a n n n -- ) {: ab:ptr bb:ptr cb:ptr rows:n rows2:n inner:n :}
   rows 0 ?do
      rows2 0 ?do                        \ row = j (outer), col = i (inner)
         ab bb  j i  inner  MM-NT-EL
         cb  j rows2 *  i +  T-SET
      loop
   loop ;

\ in-place scale of n elements by a scalar
: ATTN-SCALE! ( ptr a n r -- ) {: sb:ptr n:n inv:r :}
   n 0 ?do  sb i T-GET  inv f*  sb i T-SET  loop ;

\ per-row softmax: each length-L row of S -> A
: ATTN-SOFTMAX-ROWS ( ptr a ptr a n -- ) {: sb:ptr ab:ptr lc:n :}
   lc 0 ?do  sb i lc * cells +   ab i lc * cells +   lc  SM-FWD  loop ;

\ O = softmax(Q.K^T / sqrt(d)) . V   (sb = LxL score scratch, ab = LxL attn scratch)
: ATTN-FWD ( ptr a ptr a ptr a ptr a ptr a ptr a n n -- )
   {: qb:ptr kb:ptr vb:ptr sb:ptr ab:ptr ob:ptr lc:n d:n :}
   qb kb sb  lc lc d  MM-NT
   sb  lc lc *  1.0 d s>f fsqrt f/  ATTN-SCALE!
   sb ab lc  ATTN-SOFTMAX-ROWS
   ab vb ob  lc lc d  MATMUL ;

\ --- backward (given dO; uses the saved attention A): dQ, dK, dV ---

\ C[p,q] = sum_i A[i,p]*B[i,q]   (A^T.B : A is rows x ca, B is rows x cbc, C is ca x cbc)
: MM-TN-EL ( ptr a ptr a n n n n n -- r ) {: ab:ptr bb:ptr p:n q:n rows:n ca:n cbc:n :}
   0.0  rows 0 ?do
      ab i ca * p + T-GET   bb i cbc * q + T-GET   f* f+
   loop ;
: MM-TN ( ptr a ptr a ptr a n n n -- ) {: ab:ptr bb:ptr cb:ptr rows:n ca:n cbc:n :}
   ca 0 ?do
      cbc 0 ?do                          \ p = j (outer over ca), q = i (inner over cbc)
         ab bb  j i  rows ca cbc  MM-TN-EL
         cb  j cbc *  i +  T-SET
      loop
   loop ;

\ per-row softmax VJP: dS[i,:] = softmax_JVP(dA[i,:], A[i,:])
: ATTN-SMBWD-ROWS ( ptr a ptr a ptr a n -- ) {: dab:ptr ab:ptr dsb:ptr lc:n :}
   lc 0 ?do
      dab i lc * cells +   ab i lc * cells +   dsb i lc * cells +   lc  SM-BWD
   loop ;

\ dV=A^T.dO ; dA=dO.V^T ; dS=softmax_JVP_rows ; /sqrt(d) ; dQ=dS.K ; dK=dS^T.Q
\ scratch: dab,dsb are LxL ; dvb,dqb,dkb are Lxd ; ab is the saved attention.
: ATTN-BWD ( ptr a ptr a ptr a ptr a ptr a ptr a ptr a ptr a ptr a ptr a n n -- )
   {: dob:ptr qb:ptr kb:ptr vb:ptr ab:ptr dvb:ptr dab:ptr dsb:ptr dqb:ptr dkb:ptr lc:n d:n :}
   ab dob dvb  lc lc d  MM-TN
   dob vb dab  lc lc d  MM-NT
   dab ab dsb  lc  ATTN-SMBWD-ROWS
   dsb  lc lc *  1.0 d s>f fsqrt f/  ATTN-SCALE!
   dsb kb dqb  lc lc d  MATMUL
   dsb qb dkb  lc lc d  MM-TN ;
