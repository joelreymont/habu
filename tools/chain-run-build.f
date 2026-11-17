\ chain-run-build.f - CLI entry for the checked generation-chain driver.
\
\ Exit status: EXIT-STATUS:SUCCESS at a fixpoint; EXIT-STATUS:USAGE for a
\ refused command line; EXIT-STATUS:TIMEOUT when a build's deadline expired;
\ EXIT-STATUS:FAILURE for a non-fixpoint; EXIT-STATUS:UNCAUGHT for any other
\ throw, which it names first as `chain-run: uncaught throw code N`.

require src/lib/process.f
require tools/chain-run.f
require src/core/exit-status.f

package CHAIN-RUN-CLI

: MAIN ( -- )
   [: CHAIN-RUN:MAIN ;] catch {: rc:n :}
   rc 0= if exit then
   s" chain-run: uncaught throw code " type rc . cr
   s" " rc EXIT-STATUS:UNCAUGHT PROCESS:EXIT-RC die ;

MAIN
;package
