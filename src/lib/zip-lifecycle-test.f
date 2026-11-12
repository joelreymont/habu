\ zip-lifecycle-test.f - an archive's snapshot goes with CLOSE, a host fault
\ during CLOSE keeps the host's code and still retires the archive, and a halt
\ during disposal takes effect after the archive's node is freed.
require src/lib/string.f
require src/lib/zip.f
require src/lib/zip-test-fixture.f
require src/lib/test.f
require src/lib/task.f
require src/lib/adt/result.f          \ the join's answer is MATCHed here
require src/lib/test/wait.f

package ZIP-TEST
public
EXPORT ORIGINAL$
;package

package ZIP
using FFI

create BASE PATH-CAP allot
variable BASE-LEN
create PATH-BUF PATH-CAP allot
create SNAP-PATH PATH-CAP allot
variable SNAP-PATH-U
PTR-VARIABLE CLOSE-NODE
variable CLOSE-STARTED
variable CLOSE-FINISHED
variable LOCK-HELD
TASK:MIN-STACK TASK:TASK ZIP-LOCK-HOLDER
TASK:MIN-STACK TASK:TASK ZIP-CLOSER
TASK:SEMAPHORE LOCK-RELEASE


: INPUT$ ( -- ptr u8 n )
   BASE BASE-LEN @ s" input.zip" PATH-BUF FS:JOIN-PATH PATH-BUF swap ;


: LIFECYCLE-SETUP ( -- )
   s" habu-zip-lifecycle" FS:MKTEMP-DIR {: directory bytes:n :}
   directory BASE bytes BYTE-COPY bytes BASE-LEN !
   BASE bytes FS:CLEANUP-TREE+
   INPUT$ ZIP-TEST:ORIGINAL$ FS:WRITE-ALL ;


\ A halt requested while disposal waits takes effect after the node is
\ unregistered and freed, or its C object is discarded twice.
: HOLD-ZIP-LOCK ( -- )
   STATE-GET
   1 LOCK-HELD atomic!
   LOCK-RELEASE TASK:WAIT-SEM
   STATE-RELEASE
   1 TASK:RETURN ;

: CLOSE-UNDER-HALT ( -- )
   1 CLOSE-STARTED atomic!
   CLOSE-NODE @ DISCARD-NODE
   1 CLOSE-FINISHED atomic!
   TASK:PAUSE
   2 CLOSE-FINISHED atomic! ;

: HOLDER-JOIN-OK ( result<n,n> -- )
   MATCH result ok OF 1 T= ENDOF err OF 0 T= ENDOF ;MATCH ;

: CLOSER-JOIN-HALTED ( result<n,n> -- )
   MATCH result ok OF 0 T= ENDOF err OF E-TASK-NO-RESULT T= ENDOF ;MATCH ;

: HALT-DURING-CLOSE ( -- )
   INPUT$ OPEN ARCHIVE-NODE CLOSE-NODE !
   0 CLOSE-STARTED atomic! 0 CLOSE-FINISHED atomic! 0 LOCK-HELD atomic!
   0 LOCK-RELEASE TASK:SEMAPHORE-INIT
   ['] HOLD-ZIP-LOCK ZIP-LOCK-HOLDER TASK:ACTIVATE
   LOCK-HELD 1 ZIP-LOCK-HOLDER TEST-WAIT:FOR-CELL
   ['] CLOSE-UNDER-HALT ZIP-CLOSER TASK:ACTIVATE
   begin CLOSE-STARTED atomic@ 0= while TASK:PAUSE repeat
   ZIP-CLOSER TASK:HALT
   LOCK-RELEASE TASK:SIGNAL-SEM
   ZIP-LOCK-HOLDER TASK:JOIN HOLDER-JOIN-OK
   ZIP-CLOSER TASK:JOIN CLOSER-JOIN-HALTED
   CLOSE-FINISHED @ 1 T=
   ARCHIVES @ NULL? TTRUE
   LOCK-RELEASE TASK:SEMAPHORE-DESTROY ;


: KEEP-SNAP-PATH ( archive -- ) ARCHIVE-NODE {: node:ptr :}
   node SOURCE-PTR @ SNAP-PATH node SOURCE-LEN @ BYTE-COPY
   node SOURCE-LEN @ SNAP-PATH-U ! ;

: SNAPSHOT-REMOVED ( -- )
   INPUT$ OPEN {: archive:archive :}
   archive KEEP-SNAP-PATH
   s" the open archive's snapshot exists" T-LABEL
   SNAP-PATH SNAP-PATH-U @ FS:EXISTS? TTRUE
   archive CLOSE
   s" CLOSE removed the archive's snapshot" T-LABEL
   SNAP-PATH SNAP-PATH-U @ FS:EXISTS? TFALSE ;

: CLOSE-KEEP ( archive -- archive ) dup CLOSE ;

: SNAPSHOT-GONE ( -- )
   INPUT$ OPEN {: archive:archive :}
   archive KEEP-SNAP-PATH
   SNAP-PATH SNAP-PATH-U @ FS:REMOVE-FILE
   s" CLOSE with its snapshot removed is E-FS-IO" T-LABEL
   archive [: CLOSE-KEEP ;] catch E-FS-IO T= drop
   s" CLOSE retired the archive whose snapshot was removed" T-LABEL
   archive [: CLOSE-KEEP ;] catch E-HANDLE T= drop ;

: SOURCE-FD-GONE ( -- )
   INPUT$ OPEN {: archive:archive :}
   s" the source input's descriptor closes under it" T-LABEL
   archive OPEN-SOURCE INPUT-NODE INPUT-FD @ C-CLOSE-FD 0 T=
   s" CLOSE with an open source input's descriptor closed is E-FS-IO" T-LABEL
   archive [: CLOSE-KEEP ;] catch E-FS-IO T= drop
   s" CLOSE retired the archive whose input's descriptor was closed" T-LABEL
   archive [: CLOSE-KEEP ;] catch E-HANDLE T= drop ;

: LIFECYCLE-RUN ( -- )
   T-RESET FS:CLEANUP-RESET LIFECYCLE-SETUP SNAPSHOT-REMOVED
   SNAPSHOT-GONE SOURCE-FD-GONE HALT-DURING-CLOSE FS:CLEANUP-RUN T-REPORT ;


LIFECYCLE-RUN
;using
;package
