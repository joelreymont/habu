\ maki/causal.f - causal (autoregressive) masking for attention scores, composed
\ with the checked row softmax (maki/softmax.f). A query at row i attends only to
\ keys j<=i, so score columns j>i are masked out before the row softmax. A GPT-style
\ decoder needs this so a token never attends to its own future.
\
\ "Mask to -inf, then softmax" is realized STRUCTURALLY, not with a sentinel float.
\ A column masked to -inf contributes exp(-inf)=0 to the row: it drops out of the
\ row max/sum and its output is 0. Concretely, row i's softmax runs over the valid
\ prefix [0,i+1) of its score row and the masked suffix [i+1,n) is set to 0. This
\ keeps every value that reaches FEXP finite - lib/fmath.f FEXP has no -inf branch
\ (its range reduction would form a non-finite exponent and an unbounded 2^k), so a
\ real -inf must never flow through SM-FWD; the prefix restriction is the exact,
\ finite equivalent. SM-FWD/SM-BWD (softmax.f) are reused UNCHANGED.
\
\ A causal row always includes key j=i, so the valid prefix length v=i+1 is >=1: a
\ fully-masked row cannot occur. A v outside [1,n] is a caller bug (E-CAUSAL-RANGE),
\ not a maskable state. Backward: masked positions carry ZERO cotangent structurally
\ (dx=0 there) with no stored mask - the prefix length is position-derived (i+1), so
\ the adjoint needs no saved state beyond the softmax output SM-BWD already reads.
\ Needs maki/{array,softmax}.f; the attention path (maki/attention.f MM-NT) builds
\ the scores this masks. maki -> habu only; causal owns -5121.

require maki/array.f
require maki/softmax.f

-5121 constant E-CAUSAL-RANGE  \ causal valid-prefix count outside [1,n]

package MAKI

private

\ zero the masked suffix yb[v..n) (columns j>i for query row i, valid count v=i+1)
: SM-ZERO-TAIL! ( ptr a n n -- ) {: yb:ptr n:n v:n :}
   n v ?do  0.0 yb i T-SET  loop ;

\ the valid prefix count must be a real prefix of the row: 1..n inclusive
: CAUSAL-GUARD ( n n -- ) {: n:n v:n :}
   v 1 < v n > or if E-CAUSAL-RANGE throw then ;

public

\ y = causal-masked row softmax: softmax over the valid prefix [0,v) of x, y[j]=0
\ for j in [v,n). v = i+1 for query row i. Composes the checked SM-FWD over the prefix.
: SM-FWD-CAUSAL ( ptr a ptr a n n -- ) {: xb:ptr yb:ptr n:n v:n :}
   n v CAUSAL-GUARD
   xb yb v SM-FWD
   yb n v SM-ZERO-TAIL! ;

\ VJP of SM-FWD-CAUSAL: dx over the valid prefix via SM-BWD (reads the saved y),
\ dx[j]=0 for j in [v,n). Masked positions carry zero cotangent with no stored mask.
: SM-BWD-CAUSAL ( ptr a ptr a ptr a n n -- ) {: dyb:ptr yb:ptr dxb:ptr n:n v:n :}
   n v CAUSAL-GUARD
   dyb yb dxb v SM-BWD
   dxb n v SM-ZERO-TAIL! ;

\ per-row causal softmax over an lc x lc score matrix (row-major): row i masks
\ columns j>i, i.e. runs softmax over its valid prefix i+1. Mirrors ATTN-SOFTMAX-ROWS.
: CAUSAL-SOFTMAX-ROWS ( ptr a ptr a n -- ) {: sb:ptr ab:ptr lc:n :}
   lc 0 ?do
      sb i lc * cells +   ab i lc * cells +   lc  i 1+  SM-FWD-CAUSAL
   loop ;

\ per-row causal softmax VJP over an lc x lc matrix: dS[i,:] = causal_softmax_JVP of
\ (dA[i,:], A[i,:]); masked columns j>i carry zero cotangent. Mirrors ATTN-SMBWD-ROWS.
: CAUSAL-SMBWD-ROWS ( ptr a ptr a ptr a n -- ) {: dab:ptr ab:ptr dsb:ptr lc:n :}
   lc 0 ?do
      dab i lc * cells +   ab i lc * cells +   dsb i lc * cells +   lc  i 1+  SM-BWD-CAUSAL
   loop ;

;package
