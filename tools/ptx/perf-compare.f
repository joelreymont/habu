\ perf-compare.f - perf-regression compare over kernel profile rows.
\ Higher is better for every metric (GBS, GFLOPS, PCT-ROOF); two rows compare
\ only when they share kernel+config+device+metric (PERF:KEY=). A new value
\ below the baseline by more than TOL-MILLI permille is a regression.

require lib/errors.f
require lib/string.f
require lib/fmt.f
require tools/ptx/perf-registry.f

package PERF

create SEEN ROW-MAX allot
variable SCAN-REGRESS

public

50 constant TOL-MILLI    \ +/- 5.0% comparison band, permille of the baseline

0 constant V-OK
1 constant V-IMPROVE
2 constant V-REGRESS
3 constant V-MISSING

: VERDICT ( n n -- n ) {: base:n new:n :}   \ baseline value, new value -> verdict
   base 1 < if E-PERF-ROW throw then
   new 1 < if E-PERF-ROW throw then
   new 1000 *  base 1000 TOL-MILLI - *  < if V-REGRESS exit then
   new 1000 *  base 1000 TOL-MILLI + *  > if V-IMPROVE exit then
   V-OK ;

: VERDICT$ ( n -- ptr u8 n ) {: v:n :}
   v V-OK = if s" OK" exit then
   v V-IMPROVE = if s" IMPROVE" exit then
   v V-REGRESS = if s" REGRESS" exit then
   s" MISSING" ;

: BASELINE ( n -- n ) {: i:n :}   \ nearest earlier row sharing i's key, -1 if none
   i 1- begin dup 0 < 0= while
      dup i KEY= if exit then
      1-
   repeat ;

: COMPARE-ROWS ( n n -- n ) {: base:n new:n :}   \ verdict of row new vs row base
   base new KEY= 0= if E-PERF-KEY throw then
   base WAIVER? new WAIVER? or if V-OK exit then
   base VALUE@ new VALUE@ VERDICT ;

: ROW-VERDICT ( n -- n ) {: i:n :}   \ verdict of row i vs its nearest baseline
   i WAIVER? if V-OK exit then
   i BASELINE {: b:n :}
   b 0 < if V-MISSING exit then
   b i COMPARE-ROWS ;

private

: SEEN-RESET ( -- )
   0 begin dup ROW-MAX < while
      0 over SEEN + c!
      1+
   repeat drop ;

: SEEN? ( n -- bool )
   SEEN + c@ 0 <> ;

: MARK-KEY ( n -- ) {: i:n :}   \ mark i and every earlier same-key row seen
   i begin dup 0 < 0= while
      dup i KEY= if 1 over SEEN + c! then
      1-
   repeat drop ;

: .PAIR ( n n n -- ) {: b:n i:n v:n :}
   s" perf-compare: kernel=" type i KERNEL$ type
   s"  metric=" type i METRIC@ METRIC$ type
   s"  device=" type i DEVICE$ type
   s"  base=" type b VALUE@ FMT:.U
   s"  new=" type i VALUE@ FMT:.U
   s"  verdict=" type v VERDICT$ type cr ;

: SCAN-ONE ( n -- ) {: i:n :}   \ compare the latest same-key pair ending at row i
   i MARK-KEY
   i WAIVER? if exit then
   i BASELINE {: b:n :}
   b 0 < if exit then
   b i COMPARE-ROWS {: v:n :}
   b i v .PAIR
   v V-REGRESS = if SCAN-REGRESS @ 1+ SCAN-REGRESS ! then ;

public

: SCAN ( -- n )   \ compare the latest pair per key; report pairs; count regressions
   0 SCAN-REGRESS !
   SEEN-RESET
   ROW# 1- begin dup 0 < 0= while
      dup SEEN? 0= if dup SCAN-ONE then
      1-
   repeat drop
   SCAN-REGRESS @ ;

;package
