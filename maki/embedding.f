\ maki/embedding.f - embedding lookup (gather) + its VJP (scatter-ADD).
\
\ E is a vocab x dim table; ids is a length-L vector of token ids (held as floats,
\ f>s to index). Forward gathers Y[i,:] = E[ids[i],:]. The VJP is a scatter-ADD:
\ dE[ids[i],:] += dY[i,:] - repeated ids ACCUMULATE (the default LOAD-adjoint rule,
\ habu-ad-scatter-add). dE must be zeroed by the caller first. Needs maki/array.f.
\ maki -> habu only.

require maki/array.f

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

end-package
