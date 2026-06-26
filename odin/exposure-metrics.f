\ exposure-metrics.f - exposure-adaptation metric core, ported from
\ src/exposure_metrics.zig. The substance: BT.601 RGB->luma, IntegerStats/FloatStats
\ means (truncating / IEEE), ImageAggregate mean luminance and histogram percentile
\ luminance (ceil-rank), and writeRatio (percentage with 3 decimals). The .zig has
\ no inline tests; these are checked against exact hand-computed oracles.
\ Depends on lib/errors.f lib/string.f lib/float.f odin/float-cell.f lib/render.f.

package EXPOSURE
private
: MAX2 ( n n -- n ) {: a:n b:n :} a b > if a else b then ;

\ BT.601 luma: (r*2126 + g*7152 + b*722) / 10000  (truncating)
public
: LUMA-RGB ( n n n -- n ) {: r:n g:n b:n :} r 2126 *  g 7152 * +  b 722 * +  10000 / ;
: INT-MEAN ( i64 n -- i64 ) {: sum:i64 samp:n :} samp 0= if 0 else sum samp / then ;
: FLT-MEAN ( r n -- r ) {: fsum:r samp:n :} samp 0= if 0.0 else fsum samp s>f f/ then ;
: MEAN-LUM ( i64 n -- i64 ) {: lsum:i64 px:n :} px 0= if 0 else lsum px / then ;

\ percentileLuminance(num,den): ceil-rank over a 256-bin histogram
variable SEEN variable BI variable TGT
: PCTL ( ptr a n n n -- n ) {: hist:ptr px:n num:n den:n :}
   px 0= if 0 exit then
   den 0= if 0 exit then
   px num *  den 1- +  den /  1 MAX2  1-  TGT !
   0 SEEN !  0 BI !
   begin BI @ 256 < while
      SEEN @ hist BI @ cells + @ + SEEN !
      SEEN @ TGT @ > if BI @ exit then
      BI @ 1+ BI !
   repeat  255 ;
: CONTRAST ( ptr a n -- n ) {: hist:ptr px:n :}    \ p95 - p05
   hist px 95 100 PCTL  hist px 5 100 PCTL  - ;

\ writeRatio: percentage (num/den * 100) with exactly 3 decimals, "0.000" if den=0
: WRITE-RATIO ( i64 n -- ) {: num:i64 den:n :}
   den 0= if s" 0.000" RB+
   else num 100000 * den /  dup 1000 / RB#  46 RB-C  1000 mod RB-3 then ;
end-package
