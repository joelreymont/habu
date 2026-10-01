\ gate-common-deadline.f - one rc check of test/gate-common-lib.f on an entry
\ whose own deadline expired. test/gate-common-test.f spawns it as a gate row
\ is spawned:
\   bin/hb --load test/gate-common-deadline.f -- ok|rc|nonzero
\ The check must print GE-FAIL's capture and leave by an uncaught
\ E-PROC-TIMEOUT; a check that returns exits 1.

require lib/errors.f
require lib/string.f
require test/gate-common.f

package GE-DEADLINE

\ The entry sleeps far longer than its deadline, so only the deadline ends it.
1 constant DEADLINE-MS

\ The status a SIGKILLed child reads: the reaper kills a timed-out entry, so a
\ deadline flattened into that kill would pass a check that wants it.
128 9 + constant KILLED-RC

: SLEEPER ( -- )
   GE-HB-RESET
   s" 5" GE-ARG+
   s" /bin/sleep" DEADLINE-MS GE-RUN-ENV ;

: CHECK ( ptr u8 n -- ) {: mode:ptr modeu:n :}
   mode modeu s" ok" STR= if
      s" deadline under GE-EXPECT-OK" GE-EXPECT-OK exit
   then
   mode modeu s" rc" STR= if
      KILLED-RC s" deadline under GE-EXPECT-RC" GE-EXPECT-RC exit
   then
   mode modeu s" nonzero" STR= if
      s" deadline under GE-EXPECT-NONZERO" GE-EXPECT-NONZERO exit
   then
   s" usage: -- ok|rc|nonzero" 64 die ;

public

: MAIN ( -- )
   SCRIPT-ARGC 1 <> if s" usage: -- ok|rc|nonzero" 64 die then
   SLEEPER
   0 SCRIPT-ARGV$ CHECK
   s" an rc check returned on a timed-out entry" 1 die ;

;package

GE-DEADLINE:MAIN
