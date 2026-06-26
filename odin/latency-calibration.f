\ latency-calibration.f - camera/IMU latency calibration stats core, ported from
\ src/latency_calibration.zig. The substance: median of a sorted i64 array
\ (odd -> middle; even -> divTrunc of the two middles), nearest-rank percentile of
\ a sorted u64 array (index = min((p*len+99)/100 - 1, len-1)), absolute difference,
\ ms->ns jitter threshold, and the per-run latency stats (mean via sum/len,
\ median, min, max, residual p95/max from |latency-median|, events outside jitter).
\ src/latency_calibration.zig has no inline tests; checked vs hand-computed oracles.
\ Depends on lib/errors.f lib/string.f lib/float.f lib/sort.f odin/float-cell.f.

package LATCAL
private
: MIN2 ( n n -- n ) {: a:n b:n :} a b < if a else b then ;
public
: ABS ( i64 -- i64 ) {: v:i64 :} v 0 < if v negate else v then ;
: MAXJITTER ( r -- i64 ) 1000000.0 f* f>s ;                 \ @intFromFloat(ms*1e6)

: MEDIAN-I64 ( ptr a n -- n ) {: base:ptr len:n :}
   len 2 / {: mid:n :}
   len 2 mod 1 = if base mid cells + @
   else base mid 1- cells + @  base mid cells + @  +  2 /  then ;

: PCTL-U64 ( ptr a n n -- n ) {: base:ptr len:n p:n :}
   len 0= if 0
   else p len * 99 + 100 / 1-  len 1- MIN2  cells base + @ then ;

variable MEAN variable MED variable MIN variable MAX
variable RP95 variable RMAX variable OUT
512 constant MAX-N
create RES MAX-N cells allot
variable SUM variable LI

\ latencyStats over a latency array (sorted in place); fills the LC-* result vars.
: LATSTATS ( ptr a n i64 -- ) {: base:ptr len:n mj:i64 :}
   base len [: < ;] SORT!
   0 SUM !  0 LI ! begin LI @ len < while  SUM @ base LI @ cells + @ + SUM !  LI @ 1+ LI ! repeat
   SUM @ len / MEAN !
   base len MEDIAN-I64 MED !
   base @ MIN !   base len 1- cells + @ MAX !
   0 OUT !  0 LI ! begin LI @ len < while
      base LI @ cells + @ MED @ - ABS  RES LI @ cells + !
      RES LI @ cells + @ mj > if OUT @ 1+ OUT ! then
      LI @ 1+ LI !
   repeat
   RES len [: < ;] SORT!
   RES len 95 PCTL-U64 RP95 !
   RES len 1- cells + @ RMAX ! ;
: MEAN@ ( -- n ) MEAN @ ;   : LC-MED@ ( -- n ) MED @ ;
: MIN@ ( -- n ) MIN @ ;     : LC-MAX@ ( -- n ) MAX @ ;
: RP95@ ( -- n ) RP95 @ ;   : LC-RMAX@ ( -- n ) RMAX @ ;  : LC-OUT@ ( -- n ) OUT @ ;
end-package
