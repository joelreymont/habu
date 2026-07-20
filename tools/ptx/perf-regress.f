\ perf-regress.f - CLI perf-regression gate over the profile-row registry.
\ Run: bin/hb tools/ptx/perf-regress.f [-- registry.tsv]
\ Compares the latest same-key row pairs and exits nonzero on any regression
\ beyond PERF:TOL-MILLI.

require lib/errors.f
require lib/string.f
require lib/fmt.f
require tools/ptx/perf-compare.f

: PERF-REGRESS-PATH$ ( -- ptr u8 n )
   SCRIPT-ARGC 0 > if 0 SCRIPT-ARGV$ exit then
   s" tools/ptx/perf-rows.tsv" ;

: PERF-REGRESS-MAIN ( -- )
   [: PERF-REGRESS-PATH$ PERF:LOAD ;] catch {: code:n :}
   code 0= if
      PERF:SCAN {: n:n :}
      s" perf-regress: rows=" type PERF:ROW# FMT:.U
      s"  regressions=" type n FMT:.U cr
      n 0 > if s" perf-regress: regression beyond PERF:TOL-MILLI" 1 die then
   else
      s" perf-regress: malformed registry row at line " type PERF:LINE@ FMT:.U
      s" : " type PERF:LAST-LINE$ type cr
      code throw
   then ;

PERF-REGRESS-MAIN
