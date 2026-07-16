\ diff-capture-command-test.f - command capture failure contracts.

require lib/errors.f
require lib/test.f
require lib/fs.f
require lib/fs-mutate.f
require tools/diff-capture-command.f

package DIFF-CMD
private

variable TEST-OPEN-I
variable TEST-OPEN-FAIL-I

: TEST-OPEN-FAULT ( ptr u8 n -- fd )
   TEST-OPEN-I @ 1+ TEST-OPEN-I !
   TEST-OPEN-I @ TEST-OPEN-FAIL-I @ = if E-FS-OPEN throw then
   COMMAND-OPEN-DEFAULT ;

: TEST-SPAWN-FAULT ( -- )
   E-PROC-SPAWN throw ;

public

: TEST-OPEN-ON ( n -- )
   TEST-OPEN-FAIL-I !
   0 TEST-OPEN-I !
   [: TEST-OPEN-FAULT ;] is COMMAND-OPEN ;

: TEST-SPAWN-ON ( -- )
   [: TEST-SPAWN-FAULT ;] is COMMAND-SPAWN ;

: TEST-FAIL-SEAMS-OFF ( -- )
   RESET-COMMAND-OPEN
   RESET-COMMAND-SPAWN
   RESET-REPORT-LOADS ;

: TEST-RUN ( ptr u8 n -- ) {: root:ptr rootu:n :}
   root rootu s" " CONFIGURE
   ARGS-RESET
   s" probe" ARG
   DIFF--CAPTURE-COMMAND--PHASE:SNAPSHOT OUT-PATH OUT-PATH-U @ RUN-JJ ;

: TEST-FDS-CLOSED? ( -- bool )
   OUT-FD @ -1 = ERR-FD @ -1 = and ;

: TEST-OUTCOME# ( -- n )
   LAST-OUTCOME MATCH DIFF-CAPTURE:command-outcome
      succeeded OF 0 ENDOF
      exited    OF 1 ENDOF
      fault     OF 2 ENDOF
   ;MATCH ;

;package

package DIFF-CMD-TEST
private

create ROOT FS-PATH-CAP allot
variable ROOT-U

: PREPARE ( -- )
   s" habu-diff-command-test" TMPDIR-MKDIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY
   u ROOT-U ! ;

: ROOT$ ( -- ptr u8 n )
   ROOT ROOT-U @ ;

: RUN ( -- )
   ROOT$ DIFF-CMD:TEST-RUN ;

: RESTORE ( n -- ) {: code:n :}
   DIFF-CMD:TEST-FAIL-SEAMS-OFF
   code 0<> if code throw then ;

: OPEN-FIRST ( -- )
   1 DIFF-CMD:TEST-OPEN-ON
   [: RUN ;] catch RESTORE ;

: OPEN-SECOND ( -- )
   2 DIFF-CMD:TEST-OPEN-ON
   [: RUN ;] catch RESTORE ;

: SPAWN-FAULT ( -- )
   DIFF-CMD:TEST-SPAWN-ON
   [: RUN ;] catch RESTORE ;

: CHECK-FAULT ( n -- ) {: code:n :}
   DIFF-CMD:LAST-CODE code T=
   DIFF-CMD:LAST-RC 0 T=
   DIFF-CMD:TEST-OUTCOME# 2 T=
   DIFF-CMD:TEST-FDS-CLOSED? TTRUE ;

: MAIN ( -- )
   T-RESET
   PREPARE
   [: OPEN-FIRST ;] E-FS-OPEN TTHROWSQ
   E-FS-OPEN CHECK-FAULT
   [: OPEN-SECOND ;] E-FS-OPEN TTHROWSQ
   E-FS-OPEN CHECK-FAULT
   [: SPAWN-FAULT ;] E-PROC-SPAWN TTHROWSQ
   E-PROC-SPAWN CHECK-FAULT
   DIFF-CMD:ARG-COUNT 1 T=
   ROOT$ REMOVE-TREE
   T-REPORT
   s" diff-capture-command-test: ok" type cr ;

MAIN

;package
