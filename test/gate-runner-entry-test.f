\ gate-runner-entry-test.f - standalone-load regression for the gate-runner CLI.
\
\ GR-USAGE (test/gate-runner-lib.f) documents the invocation
\   bin/hb --load test/gate-runner-support.f test/gate-runner-entry.f -- PHASE
\ Loading that require closure in ONE process registers ~9.3k dictionary
\ entries; before the DICT-CAP raise (8192 -> 16384) the closure exhausted the
\ engine dictionary and the process died rc 77 with a bare ':' byte
\ (habu-gate-runner-entry-81c84af0). The native gate never spawns this entry -
\ resident phases fork per-phase subsets - so nothing else exercises the whole
\ closure in one process; this regression does, explicitly.
\
\ It spawns the exact documented invocation with an UNKNOWN phase token: reaching
\ GR-USAGE (die rc 64 with the usage banner on stderr) proves the entire closure
\ loaded and GR-MAIN ran. A dictionary-cap regression would instead die rc 77
\ before GR-MAIN, so the exit code alone is the red/green discriminator. The
\ child engine is HABU_UNDER_TEST when the gate sets it (the freshly built
\ candidate), else bin/hb.
\
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/memory.f lib/fs.f
\   lib/fs-mutate.f lib/process.f lib/process-argv.f lib/process-env.f
\   test/gate-runner-entry-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f

$1000 constant GRE-CAP
60000 constant GRE-TIMEOUT-MS
64 constant GRE-USAGE-RC             \ GR-USAGE-RC: the usage-die exit status

create GRE-OUT GRE-CAP allot
create GRE-ERR GRE-CAP allot
create GRE-EMPTY 1 allot             \ zero-length stdin

variable GRE-OUT-U
variable GRE-ERR-U
variable GRE-KIND
variable GRE-RC

\ Resolve the child engine: gate default env HABU_UNDER_TEST -> the freshly
\ built candidate; standalone runs fall back to bin/hb.
: GRE-HB$ ( -- ptr u8 n )
   s" HABU_UNDER_TEST" >LEN PROC-ENV-DEFAULT$? if LEN>N exit then
   2drop
   s" HABU_UNDER_TEST" GETENV dup 0= if
      2drop s" bin/hb" exit
   then ;

: GRE-STORE! ( len len n n -- ) {: outu:len erru:len kind:n code:n :}
   kind GRE-KIND !  code GRE-RC !
   erru LEN>N GRE-ERR-U !  outu LEN>N GRE-OUT-U ! ;

\ Spawn `<hb> --load test/gate-runner-support.f test/gate-runner-entry.f -- <phase>`
\ with empty stdin and capture the exit outcome.
: GRE-RUN ( ptr u8 n -- ) {: phase:ptr phaseu:n :}
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   s" test/gate-runner-support.f" >LEN PROC-ARGV+
   s" test/gate-runner-entry.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   phase phaseu >LEN PROC-ARGV+
   GRE-HB$ >LEN  GRE-EMPTY 0 >LEN  GRE-OUT GRE-CAP >LEN
   GRE-ERR GRE-CAP >LEN  GRE-TIMEOUT-MS >MS  RUN-ARGV-STDIN-CAPTURE-OUTCOME
   GRE-STORE! ;

: GRE-ERR$ ( -- ptr u8 n )
   GRE-ERR GRE-ERR-U @ ;

\ Reaching GR-USAGE proves the whole closure loaded: exit-kind, the usage exit
\ code, and the banner naming both source files (so a truncated banner or a
\ different die site does not pass).
: GRE-ASSERT-USAGE ( -- )
   GRE-KIND @ PROC-OUTCOME-EXIT T=
   GRE-RC @ GRE-USAGE-RC T=
   GRE-ERR$ s" usage: bin/hb --load test/gate-runner-support.f test/gate-runner-entry.f" CONTAINS? TTRUE ;

: GRE-TEST-UNKNOWN-PHASE ( -- )
   s" the documented --load closure loads and reaches GR-USAGE" T-LABEL
   s" gre-unknown-phase" GRE-RUN
   GRE-ASSERT-USAGE ;

: GRE-MAIN ( -- )
   T-RESET
   GRE-TEST-UNKNOWN-PHASE
   T-REPORT
   s" gate-runner-entry-test: ok" type cr ;

GRE-MAIN
