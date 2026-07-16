\ diff-capture-command-test-support.f - integration command seams.

require lib/errors.f
require tools/diff-capture-core.f

package DIFF-CMD
private

create TEST-WARN-BYTES $FF c, $80 c, $22 c, $5C c, $0A c,
5 constant TEST-WARN-U
variable TEST-WARN-N
variable TEST-BARRIER-N

: SPAWN-WARN ( -- )
   SPAWN
   ERR-FD @ TEST-WARN-BYTES TEST-WARN-U write
   TEST-WARN-U <> if E-FS-IO throw then
   TEST-WARN-N @ 1+ TEST-WARN-N ! ;

: BARRIER ( -- )
   TEST-BARRIER-N @ 1+ TEST-BARRIER-N !
   ARGS-RESET
   REPO-ARG
   s" new" ARG s" -m" ARG s" concurrent operation" ARG
   DIFF--CAPTURE-COMMAND--PHASE:SNAPSHOT OUT-PATH OUT-PATH-U @ RUN-JJ ;

public

: TEST-SPAWN-WARN-ON ( -- )
   0 TEST-WARN-N !
   [: SPAWN-WARN ;] is COMMAND-SPAWN ;

: TEST-BARRIER-ON ( -- )
   0 TEST-BARRIER-N !
   [: BARRIER ;] is SNAPSHOT-BARRIER ;

: TEST-SEAMS-OFF ( -- )
   RESET-COMMAND-OPEN
   RESET-COMMAND-SPAWN
   RESET-SNAPSHOT-BARRIER
   RESET-REPORT-LOADS ;

: TEST-WARN-COUNT ( -- n )
   TEST-WARN-N @ ;

: TEST-BARRIER-COUNT ( -- n )
   TEST-BARRIER-N @ ;

;package
