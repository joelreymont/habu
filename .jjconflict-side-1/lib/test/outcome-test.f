\ outcome-test.f - focused coverage for the outcome assert helpers.
\ Run: bin/hb --load lib/test/outcome-test.f

require lib/test.f
require lib/test/outcome.f

\ every variant through its matching assert; the wrong arms are forced-fail
\ T= calls, so their behavior is the assert framework's own and the negative
\ side is covered by the checker (a non-outcome value cannot reach these).
: TOC-RUN ( -- )
   7 OUTCOME:EXITED 7 T-OUTCOME-EXITED=
   0 OUTCOME:EXITED 0 T-OUTCOME-EXITED=
   137 OUTCOME:EXITED 137 T-OUTCOME-EXITED=       \ exit 137 stays an exit, not a signal
   SIGKILL OUTCOME:SIGNALED SIGKILL T-OUTCOME-SIGNALED=
   15 OUTCOME:SIGNALED 15 T-OUTCOME-SIGNALED=
   OUTCOME:TIMEOUT T-OUTCOME-TIMEOUT ;

T-RESET
TOC-RUN
T-REPORT
