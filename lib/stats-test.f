\ stats-test.f - checked float-stats coverage.
\ Run: cat lib/errors.f lib/test.f lib/sort.f lib/stats.f lib/stats-test.f | bin/hb

require lib/errors.f
require lib/test.f
require lib/sort.f
require lib/stats.f

create SA 8 cells allot
: SA! ( r n -- ) {: idx :} SA idx 8 * + ! ;
: FNEAR ( r r -- bool ) f- fabs 0.000001 f< ;
: TF ( r r -- ) FNEAR T-ASSERT ;

: STATS-RUN ( -- )
   T-RESET
   \ [1,2,3,4,5]: sum 15, mean 3, min 1, max 5, var 2, stddev sqrt2
   1.0 0 SA!  2.0 1 SA!  3.0 2 SA!  4.0 3 SA!  5.0 4 SA!
   SA 5 FSUM     15.0 TF
   SA 5 FMEAN     3.0 TF
   SA 5 FMIN      1.0 TF
   SA 5 FMAX      5.0 TF
   SA 5 FVAR      2.0 TF
   SA 5 FSTDDEV   2.0 fsqrt TF
   \ percentiles on the (already ascending) array
   SA 5 0.5 FPERCENTILE  3.0 TF        \ median
   SA 5 FMEDIAN          3.0 TF
   SA 5 0.0 FPERCENTILE  1.0 TF        \ min
   SA 5 1.0 FPERCENTILE  5.0 TF        \ max
   SA 5 0.25 FPERCENTILE 2.0 TF        \ pos=1 -> a[1]
   SA 5 0.9 FPERCENTILE  4.6 TF        \ pos=3.6 -> a[3]+0.6*(a[4]-a[3])
   \ unsorted input: sort first, then median
   5.0 0 SA!  3.0 1 SA!  1.0 2 SA!  4.0 3 SA!  2.0 4 SA!
   SA 5 FSORT!  SA 5 FMEDIAN  3.0 TF
   \ single element
   9.0 0 SA!  SA 1 FMEAN 9.0 TF  SA 1 FMIN 9.0 TF  SA 1 FMAX 9.0 TF ;

STATS-RUN
T-REPORT
