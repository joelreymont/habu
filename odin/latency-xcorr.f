\ latency-xcorr.f - camera/IMU latency cross-correlation core, ported from
\ src/latency_xcorr.zig. The substance is the Pearson correlation over paired
\ (camera luminance-delta, IMU signal) samples at a candidate latency offset, the
\ 3-vector L2 norm used to reduce accel/gyro to a scalar, the nearest-IMU lookup
\ (binary search + neighbour check), ms->ns conversion, and the best-score pick.
\ src/latency_xcorr.zig has no inline tests, so these are checked against exact
\ hand-computable oracles (norm(3,4,0)=5; a perfectly-correlated set -> r=1.0;
\ ms->ns truncation). Floats in float cells behind F@/F!. Depends on lib/errors.f
\ lib/string.f lib/float.f odin/float-cell.f.

package XCORR
private
public
: NORM ( r r r -- r ) {: a:r b:r c:r :} a a f* b b f* f+ c c f* f+ fsqrt ;       \ L2 norm of a 3-vector
: MS>NS ( r -- i64 ) 1000000.0 f* f>s ;                                     \ @intFromFloat(ms*1e6)
: ABS-I64 ( i64 -- i64 ) {: v:i64 :} v 0 < if v negate else v then ;
private
: FMAX0 ( r -- r ) {: x:r :} x 0.0 f> if x else 0.0 then ;                    \ max(0, x)

\ Pearson correlation over n paired float samples xs[i], ys[i].
\ Returns ( correlation mean_x mean_y ). Operation order matches the Zig.
variable PX-SX variable PX-SY variable PX-SX2 variable PX-SY2 variable PX-SXY variable PX-I
public
: PEARSON ( ptr a ptr a n -- r r r ) {: xs:ptr ys:ptr n:n :}
   0.0 PX-SX F!  0.0 PX-SY F!  0.0 PX-SX2 F!  0.0 PX-SY2 F!  0.0 PX-SXY F!  0 PX-I !
   begin PX-I @ n < while
      PX-SX  F@ xs PX-I @ cells + F@ f+ PX-SX F!
      PX-SY  F@ ys PX-I @ cells + F@ f+ PX-SY F!
      PX-SX2 F@ xs PX-I @ cells + F@ xs PX-I @ cells + F@ f* f+ PX-SX2 F!
      PX-SY2 F@ ys PX-I @ cells + F@ ys PX-I @ cells + F@ f* f+ PX-SY2 F!
      PX-SXY F@ xs PX-I @ cells + F@ ys PX-I @ cells + F@ f* f+ PX-SXY F!
      PX-I @ 1+ PX-I !
   repeat
   n s>f {: nf:r :}
   PX-SX F@ nf f/ {: mx:r :}
   PX-SY F@ nf f/ {: my:r :}
   PX-SX2 F@  PX-SX F@ PX-SX F@ f* nf f/  f-  FMAX0 {: cx2:r :}
   PX-SY2 F@  PX-SY F@ PX-SY F@ f* nf f/  f-  FMAX0 {: cy2:r :}
   PX-SXY F@  PX-SX F@ PX-SY F@ f* nf f/  f-  {: cxy:r :}
   cx2 cy2 f* fsqrt {: den:r :}
   den 0.0 f> if cxy den f/ else 0.0 then  mx  my ;
end-package
