\ perception-latency.f - perception latency metric core, ported from
\ src/perception_latency.zig (analyze's latency summary + percentileF64). The
\ analyzer collects per-record latency_ms samples (tick records if present, else
\ detection records), sorts them, and reports latency_samples + p50/p95/p99/max.
\ percentileF64 is nearest-rank: sorted[(len*pct+99)/100 - 1]; max is the last
\ element after the ascending sort. Beyond the latency core this also ports the
\ rate/queue/timing reductions from analyze(): QueueStats (QS-*), rateFromWindow
\ (RATE-WINDOW), summarizeTiming (TSUM/TS-*), and computeRateSummaries (RSUM/RS-*).
\
\ src/perception_latency.zig carries no inline test, so the oracle is the .zig
\ itself: percentileF64 was run directly (zig 0.16) on known sample sets to produce
\ the expected values the Habu port is checked against. Float samples sort via
\ lib/sort.f FSORT!. Signatures are type-keyword only. Depends on
\ lib/errors.f lib/string.f lib/float.f lib/sort.f lib/prelude.f and
\ odin/float-cell.f (the trusted F@/F! boundary).

package PERCEPTION
private
256 constant PL-MAX
create PL-LAT PL-MAX cells allot          \ latency_ms samples (float cells)
variable PL-N

public
: PL-RESET ( -- ) 0 PL-N ! ;
-6205 constant E-PL-FULL
: PL-ADD ( r -- ) {: x:r :}
   PL-N @ PL-MAX >= if E-PL-FULL throw then        \ guard: never silently drop a sample
   x PL-LAT PL-N @ cells + F!  PL-N @ 1+ PL-N ! ;
: PL-FINISH ( -- ) PL-LAT PL-N @ FSORT! ;  \ sort ascending in place

\ nearest-rank percentile over the sorted samples (percentileF64)
private
: PCTL-F ( ptr a n n -- r ) {: base:ptr len:n pct:n :}
   len pct * 99 + 100 /  1-  cells base + F@ ;

public
: PL-SAMPLES@ ( -- n ) PL-N @ ;
: PL-P50@ ( -- r ) PL-LAT PL-N @ 50 PCTL-F ;
: PL-P95@ ( -- r ) PL-LAT PL-N @ 95 PCTL-F ;
: PL-P99@ ( -- r ) PL-LAT PL-N @ 99 PCTL-F ;
: PL-MAX@ ( -- r ) PL-N @ 1- cells PL-LAT + F@ ;

\ ---------------------------------------------------------------------------
\ Queue depth, rates, timing summaries, and rate-summary reductions, ported
\ from the same analyze() reductions in src/perception_latency.zig.
\ ---------------------------------------------------------------------------

\ QueueStats: running samples / sum / max over observed queue depths.
\ mean() and max are null (signalled by QS-SAMPLES = 0) before any observation.
variable QS-N  variable QS-SUM  variable QS-MAX
: QS-RESET ( -- ) 0 QS-N !  0 QS-SUM !  0 QS-MAX ! ;
: QS-OBSERVE ( n -- ) {: d:n :}
   QS-N @ 1+ QS-N !
   QS-SUM @ d + QS-SUM !
   QS-N @ 1 = if d QS-MAX !
   else d QS-MAX @ > if d QS-MAX ! then then ;
: QS-SAMPLES ( -- n ) QS-N @ ;
: QS-SUM@ ( -- n ) QS-SUM @ ;
: QS-MAX@ ( -- n ) QS-MAX @ ;               \ valid only when QS-SAMPLES > 0
: QS-MEAN ( -- r bool ) QS-N @ 0= if 0.0 false exit then
   QS-SUM @ s>f QS-N @ s>f f/ true ;

\ rateFromWindow: (count-1) events over the (last-first) ns window, in Hz.
\ null (bool=false) when count<2 or the window is non-positive, exactly as the
\ Zig optional. Operation order matches the Zig: duration_s = (last-first)/1e9.
: RATE-WINDOW ( i64 i64 i64 -- r bool ) {: cnt:i64 fst:i64 lst:i64 :}
   lst fst - s>f  1000000000.0 f/  {: dur:r :}      \ bound before control (no bind-after-exit)
   cnt 2 < if 0.0 false
   else lst fst <= if 0.0 false
   else dur 0.0 f> 0= if 0.0 false
   else cnt 1- s>f dur f/ true
   then then then ;

\ summarizeTiming over a float array: samples + p50/p95/p99 (nearest-rank) and
\ max (last after the ascending sort). TS-SAMPLES = 0 means the empty/null case.
variable TS-N  variable TS-P50  variable TS-P95  variable TS-P99  variable TS-MAX
: TSUM ( ptr a n -- ) {: base:ptr len:n :}
   len TS-N !
   len 0= if exit then
   base len FSORT!
   base len 50 PCTL-F TS-P50 F!
   base len 95 PCTL-F TS-P95 F!
   base len 99 PCTL-F TS-P99 F!
   len 1- cells base + F@ TS-MAX F! ;
: TS-SAMPLES ( -- n ) TS-N @ ;
: TS-P50@ ( -- r ) TS-P50 F@ ;
: TS-P95@ ( -- r ) TS-P95 F@ ;
: TS-P99@ ( -- r ) TS-P99 F@ ;
: TS-MAX@ ( -- r ) TS-MAX F@ ;

\ computeRateSummaries: min / sum / mean / max over a set of valid per-camera
\ rates (caller supplies only the present ones, as the Zig skips null rates).
variable RS-N  variable RS-MIN  variable RS-MAX  variable RS-SUMF
variable RS-I  variable RS-R
: RSUM ( ptr a n -- ) {: base:ptr len:n :}
   0 RS-N !  0.0 RS-SUMF F!  0 RS-I !
   begin RS-I @ len < while
      base RS-I @ cells + F@ RS-R F!
      RS-N @ 0= if RS-R F@ RS-MIN F!  RS-R F@ RS-MAX F!
      else
         RS-R F@ RS-MIN F@ f< if RS-R F@ RS-MIN F! then
         RS-R F@ RS-MAX F@ f> if RS-R F@ RS-MAX F! then
      then
      RS-SUMF F@ RS-R F@ f+ RS-SUMF F!
      RS-N @ 1+ RS-N !  RS-I @ 1+ RS-I !
   repeat ;
: RS-COUNT ( -- n ) RS-N @ ;
: RS-MIN@ ( -- r ) RS-MIN F@ ;
: RS-MAX@ ( -- r ) RS-MAX F@ ;
: RS-SUM@ ( -- r ) RS-SUMF F@ ;             \ tracker_rate_hz aggregate = sum
: RS-MEAN ( -- r bool ) RS-N @ 0= if 0.0 false exit then
   RS-SUMF F@ RS-N @ s>f f/ true ;
end-package
