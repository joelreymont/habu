\ stats.f - float-array summary statistics over (ptr r len).
\
\ Sum / mean / min / max / population variance / stddev, plus linear-interpolated
\ percentile and median (which require the array pre-sorted ascending — use
\ lib/sort.f FSORT! first). All over a float cell array `ptr r len`; len >= 1 for
\ the reductions. Depends on lib/sort.f (FX@ cell read).

variable STAT-I

: FMIN2 ( r r -- r ) {: a b :} a b f< if a else b then ;
: FMAX2 ( r r -- r ) {: a b :} a b f> if a else b then ;
: IMIN ( n n -- n ) {: x y :} x y < if x else y then ;

: FSUM ( ptr r n -- r ) {: a:ptr len :}
   0.0  0 STAT-I !
   begin STAT-I @ len < while
      a STAT-I @ FX@ f+
      STAT-I @ 1 + STAT-I !
   repeat ;
: FMEAN ( ptr r n -- r ) {: a:ptr len :}
   a len FSUM  len s>f f/ ;

: FMIN ( ptr r n -- r ) {: a:ptr len :}
   a 0 FX@  1 STAT-I !
   begin STAT-I @ len < while
      a STAT-I @ FX@ FMIN2
      STAT-I @ 1 + STAT-I !
   repeat ;
: FMAX ( ptr r n -- r ) {: a:ptr len :}
   a 0 FX@  1 STAT-I !
   begin STAT-I @ len < while
      a STAT-I @ FX@ FMAX2
      STAT-I @ 1 + STAT-I !
   repeat ;

: FVAR ( ptr r n -- r ) {: a:ptr len :}        \ population variance
   a len FMEAN {: m :}
   0.0  0 STAT-I !
   begin STAT-I @ len < while
      a STAT-I @ FX@ m f-  dup f*  f+
      STAT-I @ 1 + STAT-I !
   repeat
   len s>f f/ ;
: FSTDDEV ( ptr r n -- r ) {: a:ptr len :}
   a len FVAR fsqrt ;

\ p-th percentile (p in [0,1], clamped) of a SORTED array, linear interpolation.
: FPERCENTILE ( ptr r n r -- r ) {: a:ptr len p :}
   p 0.0 FMAX2 1.0 FMIN2 {: pc :}
   pc len 1 - s>f f* {: pos :}
   pos f>s {: lo :}
   lo 1 +  len 1 -  IMIN {: hi :}
   a lo FX@ {: alo :}
   a hi FX@ {: ahi :}
   pos lo s>f f- {: frac :}
   alo  ahi alo f-  frac f*  f+ ;
: FMEDIAN ( ptr r n -- r ) {: a:ptr len :}     \ array must be sorted ascending
   a len 0.5 FPERCENTILE ;
