\ Shared native writer runner for the two fixpoint rows that probe its retained
\ runtime and stage reader. The sandbox row does not load or settle this image.
require tools/build-fixpoint-test-lib.f
require test/fixture-writer.f

package BUILD-FIXPOINT

\ The writer has already compiled the target runtime and driver I/O. It emits
\ an empty private image first, then runs the supplied source on stdin.
: BFT-WRITER-RUN ( ptr u8 n ptr u8 n -- n n n )
   {: output:ptr outputu:n src:ptr srcu:n :}
   FIXTURE-WRITER:PATH$ {: writer:ptr writeru:n :}
   PROC-ARGV-ENV-RESET
   s" HB_TMP" >LEN BFT-ROOT >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   s" --" BFT-ARG+
   output outputu BFT-ARG+
   writer writeru >LEN src srcu >LEN
   BFT-OUT BFT-CAPTURE-CAP >LEN BFT-ERR BFT-CAPTURE-CAP >LEN
   BFT-TIMEOUT-MS >MS RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME
      PROC-OUTCOME>DEADLINE-RC RC>N
   {: outu:len erru:len rc:n :}
   outu LEN>N erru LEN>N rc BFT-ERR BFT-FIXPOINT-RC ;

;package
