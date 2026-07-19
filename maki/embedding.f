\ maki/embedding.f - embedding lookup (gather) + its VJP (scatter-ADD), plus the
\ learned positional table (wpe) SLICE and the token+pos ADD composition.
\
\ E is a vocab x dim table; ids is a length-L vector of token ids (held as floats,
\ f>s to index). Forward gathers Y[i,:] = E[ids[i],:]. The VJP is a scatter-ADD:
\ dE[ids[i],:] += dY[i,:] - repeated ids ACCUMULATE (the default LOAD-adjoint rule,
\ habu-ad-scatter-add). dE must be zeroed by the caller first.
\
\ wpe (dot habu-learned-positional-embedding): positions are ALWAYS 0..T-1 and
\ contiguous, so the positional lookup is the row-SLICE wpe[0:T] of a MaxT x dim
\ table - exact, and it needs no index buffer (unlike a pos GATHER). Its VJP is the
\ slice adjoint (a PAD-SCATTER at row offset 0): dWpe[0:T] += dY, rows T..MaxT-1
\ untouched (caller pre-zeroes dWpe). Chosen over a pos GATHER because both adjoints
\ gradcheck today but this one is the simpler existing adjoint (a contiguous copy, no
\ index buffer, no repeated-index collisions). T past the table extent is a NAMED
\ reject (E-WPE-EXTENT), never a clamp. TOKPOS-EMBED composes the two lookups + the
\ elementwise ADD into the block input Y[i,:] = E[ids[i],:] + wpe[i,:]. Needs
\ maki/array.f. maki -> habu only; embedding owns -5169.

require maki/array.f

-5169 constant E-WPE-EXTENT   \ requested length T exceeds the positional table's row count

package MAKI

\ copy table row E[id] into output row Y[iy]
: EMB-ROW! ( ptr a ptr a n n n -- ) {: eb:ptr yb:ptr id:n iy:n dim:n :}
   dim 0 ?do
      eb  id dim *  i +  T-GET
      yb  iy dim *  i +  T-SET
   loop ;

\ Y[i,:] = E[ids[i],:]
public

: EMB-GATHER ( ptr a ptr a ptr a n n -- ) {: eb:ptr idsb:ptr yb:ptr lc:n dim:n :}
   lc 0 ?do
      eb yb  idsb i T-GET f>s  i  dim  EMB-ROW!
   loop ;

private

\ dE[id,:] += dY[iy,:]   (accumulate, so repeated ids sum)
: EMB-SCATTER-ROW! ( ptr a ptr a n n n -- ) {: deb:ptr dyb:ptr id:n iy:n dim:n :}
   dim 0 ?do
      deb  id dim *  i +  T-GET
      dyb  iy dim *  i +  T-GET  f+
      deb  id dim *  i +  T-SET
   loop ;

\ scatter-ADD the row cotangents back into the table gradient (dE pre-zeroed)
public

: EMB-SCATTER-ADD ( ptr a ptr a ptr a n n -- ) {: idsb:ptr dyb:ptr deb:ptr lc:n dim:n :}
   lc 0 ?do
      deb dyb  idsb i T-GET f>s  i  dim  EMB-SCATTER-ROW!
   loop ;

private

\ dst[i] += src[i] over the leading n elements (the slice image is contiguous from row 0)
: ROW-ADD ( ptr a ptr a n -- ) {: sb:ptr db:ptr n:n :}
   n 0 ?do  db i T-GET  sb i T-GET  f+  db i T-SET  loop ;

\ named reject: a sequence length past the positional table's extent (never a clamp)
: WPE-FIT ( n n -- ) {: t:n maxt:n :}  t maxt > if E-WPE-EXTENT throw then ;

\ wpe piece: forward positional lookup wpe[0:T] (rows 0..T-1, contiguous slice)
public

: WPE-SLICE ( ptr a ptr a n n n -- ) {: wb:ptr yb:ptr t:n maxt:n dim:n :}
   t maxt WPE-FIT
   t dim * 0 ?do  wb i T-GET  yb i T-SET  loop ;

\ wpe piece VJP: dWpe[0:T] += dY (slice adjoint / pad-scatter at offset 0; dWpe pre-zeroed)
: WPE-SLICE-ADD ( ptr a ptr a n n -- ) {: dyb:ptr dwb:ptr t:n dim:n :}
   dyb dwb  t dim *  ROW-ADD ;

\ token+pos composition: Y[i,:] = E[ids[i],:] + wpe[i,:] (the B*T x dim block input, B=1)
: TOKPOS-EMBED ( ptr a ptr a ptr a ptr a n n n -- )
   {: eb:ptr idsb:ptr wb:ptr yb:ptr t:n maxt:n dim:n :}
   t maxt WPE-FIT
   eb idsb yb  t dim  EMB-GATHER
   wb yb  t dim *  ROW-ADD ;

;package
