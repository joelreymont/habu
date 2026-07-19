\ maki/test-harness.f - shared per-file timed RUN-FILE harness for the maki suite.
\ Factored out of maki/test.f when the monolithic suite was split into parallel
\ gate slices (dot habu-split-monolithic-maki-fccca4ea): maki/test.f (the full
\ inventory / standalone run-all) and every maki/test-<slice>.f loader require this
\ one harness so the RUN-FILE machinery lives in exactly one place. A consumer
\ requires this file, then drives TEST:RESET / TEST:GROUP ... / TEST:RUN itself.

require lib/test.f
require lib/float.f
require lib/fmt.f

package MAKI-TEST

1000000 constant NS-PER-MS

variable RUN-A
variable RUN-U
variable START-NS

: ELAPSED-MS ( -- n )
   mono-ns START-NS @ - NS-PER-MS / ;

: CURRENT$ ( -- ptr u8 n )
   RUN-A @ RUN-U @ ;

: NUM-TYPE ( n -- )
   SB-RESET SB-INT SB$ type ;

: INCLUDE-CURRENT ( -- )
   CURRENT$ included ;

: RUN-PASS ( -- )
   s" PASS: " type CURRENT$ type
   s"  (" type ELAPSED-MS NUM-TYPE s" ms)" type cr ;

: RUN-FAIL ( n -- ) {: rc:n :}
   s" FAIL: " type CURRENT$ type
   s"  rc=" type rc NUM-TYPE cr
   rc throw ;

: RUN-FILE ( ptr u8 n -- ) {: path:ptr pathu:n :}
   path RUN-A !
   pathu RUN-U !
   s" TEST: " type CURRENT$ type cr
   mono-ns START-NS !
   [: INCLUDE-CURRENT ;] catch {: rc:n :}
   rc 0= if RUN-PASS exit then
   rc RUN-FAIL ;

: INSTALL ( -- )
   [: RUN-FILE ;] TEST:RUNNER! ;

INSTALL

;package
