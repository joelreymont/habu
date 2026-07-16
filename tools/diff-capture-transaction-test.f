\ diff-capture-transaction-test.f - cleanup/publication outcome contracts.

require lib/errors.f
require lib/test.f
require lib/fs.f
require lib/fs-mutate.f
require tools/diff-capture-core.f

package DIFF-TXN
private

: TEST-PRIMARY-FAULT ( -- )
   E-FS-STAT throw ;

: TEST-CLEAN-FAULT ( -- )
   E-FS-DIR throw ;

: TEST-PUBLISH-FAULT ( -- )
   E-FS-IO throw ;

public

: TEST-PRIMARY-ON ( -- )
   [: TEST-PRIMARY-FAULT ;] is CAPTURE-JJ ;

: TEST-CLEAN-ON ( -- )
   [: TEST-CLEAN-FAULT ;] is CAPTURE-CLEAN ;

: TEST-PUBLISH-ON ( -- )
   [: TEST-PUBLISH-FAULT ;] is CAPTURE-PUBLISH ;

: TEST-SEAMS-OFF ( -- )
   RESET-CAPTURE-JJ
   RESET-CAPTURE-CLEAN
   RESET-CAPTURE-PUBLISH ;

: TEST-ROOT-EXISTS? ( -- bool )
   ROOT$ EXISTS? ;

: TEST-FORCE-CLEAN ( -- )
   CAPTURE-CLEAN-DEFAULT
   false ROOT-READY ! ;

: TEST-OUTCOME# ( -- n )
   LAST-CAPTURE-AT @ MATCH DIFF-CAPTURE:capture-outcome
      ok              OF 0 ENDOF
      primary-failed  OF 1 ENDOF
      cleanup-failed  OF 2 ENDOF
      combined-failed OF 3 ENDOF
   ;MATCH ;

: TEST-RESULT! ( n n -- )
   SET-CAPTURE-RESULT ;

: TEST-PUBLISH-RUN ( -- )
   RESET-REPORT
   PUBLISH ;

;package

package DIFF-TXN-TEST
private

: CAPTURE ( -- )
   s" " s" unused.hbdiff" s" from" s" to" DIFF-TXN:CAPTURE ;

: RESTORE ( n -- ) {: code:n :}
   DIFF-TXN:TEST-SEAMS-OFF
   code 0<> if code throw then ;

: PRIMARY ( -- )
   DIFF-TXN:TEST-PRIMARY-ON
   [: CAPTURE ;] catch RESTORE ;

: COMBINED ( -- )
   DIFF-TXN:TEST-PRIMARY-ON
   DIFF-TXN:TEST-CLEAN-ON
   [: CAPTURE ;] catch RESTORE ;

: PUBLISH ( -- )
   DIFF-TXN:TEST-PUBLISH-ON
   [: DIFF-TXN:TEST-PUBLISH-RUN ;] catch RESTORE ;

: CHECK ( n n n -- )
   {: primary:n cleanup:n outcome:n :}
   DIFF-TXN:LAST-PRIMARY primary T=
   DIFF-TXN:LAST-CLEANUP cleanup T=
   DIFF-TXN:TEST-OUTCOME# outcome T= ;

: MAIN ( -- )
   T-RESET
   [: PRIMARY ;] E-FS-STAT TTHROWSQ
   E-FS-STAT 0 1 CHECK
   DIFF-TXN:TEST-ROOT-EXISTS? TFALSE
   DIFF-CMD:COMMAND? TFALSE
   [: COMBINED ;] E-FS-STAT TTHROWSQ
   E-FS-STAT E-FS-DIR 3 CHECK
   DIFF-TXN:TEST-ROOT-EXISTS? TTRUE
   DIFF-TXN:TEST-FORCE-CLEAN
   DIFF-TXN:TEST-ROOT-EXISTS? TFALSE
   0 E-FS-DIR DIFF-TXN:TEST-RESULT!
   0 E-FS-DIR 2 CHECK
   [: PUBLISH ;] E-FS-IO TTHROWSQ
   E-FS-IO 0 1 CHECK
   T-REPORT
   s" diff-capture-transaction-test: ok" type cr ;

MAIN

;package
