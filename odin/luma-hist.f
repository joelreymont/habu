\ luma-hist.f - 256-bin luminance histogram + mean/percentile.
\
\ Ported from Odin's src/low_light.zig ImageAggregate (histogram/meanLuminance/
\ percentileLuminance). Pure integer data: build a 256-bin histogram from a luma
\ byte plane (e.g. from odin/netpbm.f NP-DECODE), then read mean and rank-based
\ percentile luminance straight off the bins (no sort needed — the histogram is
\ pre-binned). Histograms are 256 count cells (ptr n). Depends only on core.

package LUMA
private
256 constant BINS

variable IX
variable RESULT
variable FOUND

public
: RESET ( ptr n -- ) {: h:ptr :}                \ zero a 256-bin histogram
   0 IX !
   begin IX @ BINS < while  0 h IX @ cells + !  IX @ 1+ IX !  repeat ;

: ADD ( ptr n ptr u8 n -- ) {: h:ptr luma:ptr n:n :}   \ count n luma bytes into h
   0 IX !
   begin IX @ n < while
      luma IX @ + c@ cells h +  dup @ 1+ swap !
      IX @ 1+ IX !
   repeat ;

: TOTAL ( ptr n -- n ) {: h:ptr :}              \ pixel count = sum of bins
   0  0 IX !
   begin IX @ BINS < while  h IX @ cells + @ +  IX @ 1+ IX !  repeat ;

private
: WSUM ( ptr n -- n ) {: h:ptr :}               \ sum(i * hist[i])
   0  0 IX !
   begin IX @ BINS < while
      IX @  h IX @ cells + @  *  +
      IX @ 1+ IX !
   repeat ;

public
: MEAN ( ptr n -- n ) {: h:ptr :}               \ mean luminance, 0 if empty
   h TOTAL {: total:n :}
   total 0= if 0 else h WSUM total / then ;

\ max(1, ceil(total*num/den)) — the 1-based rank of the target sample
private
: RANK ( n n n -- n ) {: total:n num:n den:n :}
   total num * den 1 - + den / {: rank:n :}
   rank 1 < if 1 else rank then ;

\ first bin whose cumulative count exceeds target, else 255
: SCAN ( ptr n n -- n ) {: h:ptr target:n :}
   255 RESULT !  0 FOUND !
   0  0 IX !
   begin IX @ BINS < while
      h IX @ cells + @ +
      dup target > FOUND @ 0= and if IX @ RESULT !  -1 FOUND ! then
      IX @ 1+ IX !
   repeat  drop  RESULT @ ;

\ numerator/denominator percentile luminance (e.g. 50 100 = median, 5 100 = p05)
public
: PCT ( ptr n n n -- n ) {: h:ptr num:n den:n :}
   h TOTAL {: total:n :}
   total 0= den 0= or if 0 exit then
   h  total num den RANK 1 -  SCAN ;
end-package
