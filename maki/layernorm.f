\ maki/layernorm.f - LayerNorm over a feature vector (no affine), gradcheckable
\ with only + - * / sqrt (no transcendentals). y_i = (x_i - mu)/sqrt(var+eps).
\
\ A transformer-block normalization (habu-autograd-transformer-block). The backward
\ is the standard simplified form dx_i = (1/std)(dy_i - mean(dy) - xhat_i*mean(dy*xhat)).
\ Stats are recomputed rather than juggled on the stack (a CPU reference; correctness
\ over speed). Needs maki/array.f. maki -> habu only.

require maki/array.f

\ layernorm owns -5432 (the affine golden's fail-closed feature-length guard).
-5432 constant E-LN-DIM   \ affine forward/VJP given a non-positive feature length

package MAKI

: LN-EPS ( -- r )  0.00001 ;

: LN-MEAN ( ptr r n -- r ) {: xb:ptr n:n :}  xb n T-SUM  n s>f f/ ;

\ variance = mean( (x-mu)^2 ) given mu
: LN-VAR ( ptr r n r -- r ) {: xb:ptr n:n mu:r :}
   0.0  n 0 ?do  xb i T-GET  mu f-  dup f*  f+  loop  n s>f f/ ;

\ standard deviation sqrt(var+eps)
: LN-STD ( ptr r n r -- r ) {: xb:ptr n:n mu:r :}
   xb n mu LN-VAR  LN-EPS f+  fsqrt ;

\ write normalized xhat_i = (x_i-mu)/std into yb
: LN-NORM! ( ptr r ptr r n r r -- ) {: xb:ptr yb:ptr n:n mu:r std:r :}
   n 0 ?do  xb i T-GET  mu f-  std f/  yb i T-SET  loop ;

\ forward (recomputes mu twice / std once; clean over fast)
public

: LN-FWD ( ptr r ptr r n -- ) {: xb:ptr yb:ptr n:n :}
   xb yb n  xb n LN-MEAN  xb n  xb n LN-MEAN  LN-STD  LN-NORM! ;

private

\ mean( dy_i * (x_i - mu) ) given mu
: LN-MEAN-DYC ( ptr r ptr r n r -- r ) {: dyb:ptr xb:ptr n:n mu:r :}
   0.0  n 0 ?do  dyb i T-GET  xb i T-GET mu f-  f*  f+  loop  n s>f f/ ;

\ dx_i = (dy_i - mdy - xhat_i*mdyx)/std   (mdyx = mean(dy*xhat), xhat=(x-mu)/std)
: LN-DX! ( ptr r ptr r ptr r n r r r r -- )
   {: dyb:ptr xb:ptr dxb:ptr n:n mu:r std:r mdy:r mdyx:r :}
   n 0 ?do
      dyb i T-GET  mdy f-
      xb i T-GET mu f- std f/  mdyx f*  f-
      std f/
      dxb i T-SET
   loop ;

\ backward (recomputes the stats; mdyx = mean(dy*(x-mu)) / std = mean(dy*xhat))
public

: LN-BWD ( ptr r ptr r ptr r n -- ) {: dyb:ptr xb:ptr dxb:ptr n:n :}
   dyb xb dxb n
   xb n LN-MEAN
   xb n  xb n LN-MEAN  LN-STD
   dyb n T-SUM  n s>f f/
   dyb xb n  xb n LN-MEAN  LN-MEAN-DYC   xb n  xb n LN-MEAN  LN-STD  f/
   LN-DX! ;

\ ---- affine LayerNorm (GPT-2 style): y = gamma*xhat + beta ------------------
\ gamma/beta are per-feature (length n), SHARED across rows; xhat is the existing
\ no-affine normalized value. Golden operates one ROW at a time; a multi-row caller
\ loops the rows and the VJP accumulates the per-row parameter gradients (dot
\ habu-affine-layernorm-gamma). Non-positive n is a fail-closed error.

\ forward: write xhat with LN-FWD, then scale-and-shift in place.
: LN-AFFINE-FWD ( ptr r ptr r ptr r ptr r n -- )
   {: xb:ptr yb:ptr gb:ptr bb:ptr n:n :}
   n 1 < if E-LN-DIM throw then
   xb yb n LN-FWD
   n 0 ?do  gb i T-GET  yb i T-GET f*  bb i T-GET f+  yb i T-SET  loop ;

\ VJP for one row: ACCUMULATE dgamma += dy*xhat and dbeta += dy (the row-sum the
\ affine params see), and write dx = LN-BWD(dy*gamma, x) - the upstream cotangent
\ into xhat is dy*gamma, threaded through the existing normalization backward. xhb
\ is caller scratch (length n): it first holds xhat (for dgamma), then is reused as
\ dxhat (= dy*gamma) fed to LN-BWD. The caller zeros dgb/dbb before the row loop.
: LN-AFFINE-BWD ( ptr r ptr r ptr r ptr r ptr r ptr r ptr r n -- )
   {: dyb:ptr xb:ptr gb:ptr dxb:ptr dgb:ptr dbb:ptr xhb:ptr n:n :}
   n 1 < if E-LN-DIM throw then
   xb xhb n LN-FWD
   n 0 ?do
      dgb i T-GET  dyb i T-GET  xhb i T-GET f*  f+  dgb i T-SET   \ dgamma += dy*xhat
      dbb i T-GET  dyb i T-GET  f+  dbb i T-SET                   \ dbeta  += dy
      dyb i T-GET  gb i T-GET f*  xhb i T-SET                     \ xhb := dxhat = dy*gamma
   loop
   xhb xb dxb n LN-BWD ;                                          \ dx = LN-BWD(dxhat, x)

;package
