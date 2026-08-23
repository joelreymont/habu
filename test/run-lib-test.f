\ run-lib-test.f - focused coverage for the resident gate schedule.
\ schedule-lint: allow-unscheduled - manual-standalone by design, see the run line
\ below.
\
\ Manual-standalone gate (not a TEST:SUITE member): run directly from the repo
\ root with
\   printf '' | bin/hb --load lib/prelude.f lib/string.f lib/fmt.f lib/test.f \
\          test/run-lib.f test/run-lib-test.f
\
require lib/prelude.f
require lib/string.f
require lib/fmt.f
require lib/test.f
require test/run-lib.f

package TEST
private

: EARLY-COUNT ( idx -- n ) {: phase:idx :}
   0
   0 begin dup EARLY-HOST-PHASES < while
      dup >IDX EARLY-HOST-ORDER@ IDX>N phase IDX>N = if swap 1+ swap then
      1+
   repeat drop ;

: EARLY-GROUPS ( -- )
   s" early schedule runs tool-repair once" T-LABEL
   21 >IDX EARLY-COUNT 1 T=
   s" early schedule runs tool-doc once" T-LABEL
   22 >IDX EARLY-COUNT 1 T=
   s" early schedule runs every tool-lint split once" T-LABEL
   36 >IDX EARLY-COUNT 1 T=
   37 >IDX EARLY-COUNT 1 T=
   38 >IDX EARLY-COUNT 1 T=
   39 >IDX EARLY-COUNT 1 T= ;

: RUN-LIB-TEST ( -- )
   EARLY-GROUPS
   T-REPORT
   s" run-lib-test: ok" type cr ;

RUN-LIB-TEST

;package
