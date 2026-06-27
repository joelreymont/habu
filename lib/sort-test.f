\ sort-test.f - checked FSORT! coverage.
\ Run: cat lib/errors.f lib/test.f lib/sort.f lib/sort-test.f | bin/hb

create ST-A 8 cells allot

: ST! ( r n -- ) {: idx :} ST-A idx 8 * + ! ;     \ store float at slot
: ST@ ( n -- r ) {: idx :} ST-A idx FX@ ;
: ST-ASC? ( n -- bool ) {: len :}                 \ a[i] <= a[i+1] for all i?
   0 0= len 1 - 0 ?do
      ST-A i FX@  ST-A i 1 + FX@  f> if drop 0 0= 0= then
   loop ;

: SORT-RUN ( -- )
   T-RESET
   \ [3,1,4,1,5] -> [1,1,3,4,5]
   3.0 0 ST!  1.0 1 ST!  4.0 2 ST!  1.0 3 ST!  5.0 4 ST!
   ST-A 5 FSORT!
   0 ST@ 1.0 f= T-ASSERT   1 ST@ 1.0 f= T-ASSERT   2 ST@ 3.0 f= T-ASSERT
   3 ST@ 4.0 f= T-ASSERT   4 ST@ 5.0 f= T-ASSERT
   5 ST-ASC? T-ASSERT
   \ reverse-sorted -> ascending
   8.0 0 ST!  6.0 1 ST!  4.0 2 ST!  2.0 3 ST!  0.0 4 ST!
   ST-A 5 FSORT!  5 ST-ASC? T-ASSERT  0 ST@ 0.0 f= T-ASSERT  4 ST@ 8.0 f= T-ASSERT
   \ negatives + duplicates
   -1.0 0 ST!  -3.0 1 ST!  -1.0 2 ST!  2.0 3 ST!
   ST-A 4 FSORT!  4 ST-ASC? T-ASSERT  0 ST@ -3.0 f= T-ASSERT  3 ST@ 2.0 f= T-ASSERT
   \ len 1 and len 0 are no-ops (no crash)
   7.0 0 ST!  ST-A 1 FSORT!  0 ST@ 7.0 f= T-ASSERT
   ST-A 0 FSORT!  0 ST@ 7.0 f= T-ASSERT ;

SORT-RUN
T-REPORT
