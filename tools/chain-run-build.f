\ chain-run-build.f - CLI entry for the checked generation-chain driver.
\
\ Exit status: 0 at a fixpoint; 64 for a refused command line; PROC-TIMEOUT-RC
\ (124, lib/process.f) when a deadline expired, in this process or in a native
\ build that exited with it; CHAIN-RUN:FAIL-RC for a non-fixpoint and for every
\ other throw, which it names first as `chain-run: uncaught throw code N`.

require lib/process.f
require tools/chain-run.f

package CHAIN-RUN-CLI

\ A throw code does not cross a process boundary, so the caught code leaves as
\ the exit status (lib/process.f PROC-EXIT-RC).
: MAIN ( -- )
   [: CHAIN-RUN:MAIN ;] catch {: rc:n :}
   rc 0= if exit then
   s" chain-run: uncaught throw code " type rc . cr
   s" " rc CHAIN-RUN:FAIL-RC PROC-EXIT-RC die ;

MAIN
;package
