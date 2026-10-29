\ zip-lifecycle-test.f - live ZIP archives retired by image preparation.
require src/lib/string.f
require src/lib/zip.f
require src/lib/zip-test-fixture.f
require src/lib/test.f
require src/lib/task.f
require src/lib/test/fork-hold.f

package ZIP-TEST
public
EXPORT ORIGINAL$
;package

package ZIP
using FFI
using IMAGE-LIFECYCLE

create BASE PATH-CAP allot
variable BASE-LEN
create PATH-BUF PATH-CAP allot
create SNAP-PATH PATH-CAP allot
variable SNAP-PATH-U
TYPED-VARIABLE OWNER archive
variable OWNER-LIVE
variable OWNER-CLOSES
variable OWNER-RESULT
PTR-VARIABLE CLOSE-NODE
variable CLOSE-STARTED
variable CLOSE-FINISHED
TASK:MIN-STACK TASK:TASK ZIP-LOCK-HOLDER
TASK:MIN-STACK TASK:TASK ZIP-CLOSER
TASK:SEMAPHORE LOCK-HELD
TASK:SEMAPHORE LOCK-RELEASE


: INPUT$ ( -- ptr u8 n )
   BASE BASE-LEN @ s" input.zip" PATH-BUF FS:JOIN-PATH PATH-BUF swap ;


: LIFECYCLE-SETUP ( -- )
   s" habu-zip-lifecycle" FS:MKTEMP-DIR {: directory bytes:n :}
   directory BASE bytes BYTE-COPY bytes BASE-LEN !
   BASE bytes FS:CLEANUP-TREE+
   INPUT$ ZIP-TEST:ORIGINAL$ FS:WRITE-ALL ;


\ The library may retire this identity before its application owner runs.
: OWNER-CLOSE ( -- )
   OWNER-LIVE @ 0= if exit then
   OWNER @ [: dup CLOSE ;] catch {: code:n :} drop
   code 0<> code E-HANDLE <> and if code throw then
   code OWNER-RESULT ! 1 OWNER-CLOSES +!
   0 >ARCHIVE OWNER ! 0 OWNER-LIVE ! ;


: OWNER-OPEN ( -- archive )
   [: OWNER-CLOSE ;] IMAGE-LIFECYCLE:REGISTER
   INPUT$ OPEN dup OWNER ! 1 OWNER-LIVE ! ;


: READ-STORED ( archive -- entry )
   dup 0 ENTRY {: archive:archive member:entry :}
   archive member NAME$ s" stored.txt" T$=
   archive member READ s" stored value" T$= member ;


: STALE-COUNT ( archive -- archive ) dup COUNT drop ;
: STALE-READ ( archive entry -- archive entry ) 2dup READ 2drop ;


: ASSERT-RETIRED ( -- )
   ARCHIVES @ NULL? TTRUE ;


: LIFECYCLE-USE ( -- )
   [: s" /no/such/habu-zip-lifecycle.zip" OPEN drop ;]
      E-OPEN TTHROWSQ ;


: LIFECYCLE-ASSERT-CLEAR ( -- )
   ASSERT-RETIRED REGISTERED @ 0 T= ;


: LIFECYCLE-ARCHIVES ( -- )
   OWNER-OPEN dup READ-STORED {: archive:archive member:entry :}
   INPUT$ EDIT {: other:archive :}
   other 0 ENTRY other swap s" replacement" REPLACE
   PREPARE LIFECYCLE-ASSERT-CLEAR
   OWNER-LIVE @ 0 T= OWNER-RESULT @ E-HANDLE T=
   archive [: STALE-COUNT ;] catch E-HANDLE T= drop
   archive member [: STALE-READ ;] catch E-HANDLE T= 2drop
   other [: STALE-COUNT ;] catch E-HANDLE T= drop
   OWNER-CLOSES @ OWNER-CLOSE OWNER-CLOSES @ T=
   INPUT$ OPEN {: fresh:archive :}
   fresh ARCHIVE>N archive ARCHIVE>N T<>
   fresh READ-STORED drop
   PREPARE LIFECYCLE-ASSERT-CLEAR
   fresh [: STALE-COUNT ;] catch E-HANDLE T= drop
   PREPARE LIFECYCLE-ASSERT-CLEAR ;


\ The child must reset a lock held by a task that fork did not copy.
: ZIP-CHILD-READ ( -- )
   INPUT$ OPEN {: archive:archive :}
   archive 0 ENTRY {: member:entry :}
   archive member READ s" stored value" STR= 0= if E-READ throw then
   archive CLOSE ;

: FORK-UNDER-ZIP-LOCK ( -- )
   [: STATE-GET [: FORK-HOLD:HOLDING ;] [: STATE-RELEASE ;] finally ;]
      FORK-HOLD:HOLD
   s" a child forked under a held ZIP lock read an archive"
   [: ZIP-CHILD-READ ;] 1 FORK-HOLD:FORK-CHECK
   STATE-LOCK atomic@ {: held:n :}
   FORK-HOLD:RELEASE
   s" the ZIP lock was still the holder's" T-LABEL
   held 1 T= ;


\ A halt requested while disposal waits must take effect after the node is
\ unregistered and freed. Otherwise image retirement discards its C object twice.
: HOLD-ZIP-LOCK ( -- )
   STATE-GET
   LOCK-HELD TASK:SIGNAL
   LOCK-RELEASE TASK:WAIT
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
   0 CLOSE-STARTED atomic! 0 CLOSE-FINISHED atomic!
   0 LOCK-HELD TASK:SEMAPHORE-INIT
   0 LOCK-RELEASE TASK:SEMAPHORE-INIT
   ['] HOLD-ZIP-LOCK ZIP-LOCK-HOLDER TASK:ACTIVATE
   LOCK-HELD TASK:WAIT
   ['] CLOSE-UNDER-HALT ZIP-CLOSER TASK:ACTIVATE
   begin CLOSE-STARTED atomic@ 0= while TASK:PAUSE repeat
   ZIP-CLOSER TASK:HALT
   LOCK-RELEASE TASK:SIGNAL
   ZIP-LOCK-HOLDER TASK:JOIN HOLDER-JOIN-OK
   ZIP-CLOSER TASK:JOIN CLOSER-JOIN-HALTED
   CLOSE-FINISHED @ 1 T=
   ARCHIVES @ NULL? TTRUE
   LOCK-HELD TASK:SEMAPHORE-DESTROY
   LOCK-RELEASE TASK:SEMAPHORE-DESTROY ;


\ An archive's snapshot exists while it is open and goes with CLOSE.
: SNAPSHOT-REMOVED ( -- )
   INPUT$ OPEN {: archive:archive :}
   archive ARCHIVE-NODE {: node:ptr :}
   node SOURCE-PTR @ SNAP-PATH node SOURCE-LEN @ BYTE-COPY
   node SOURCE-LEN @ SNAP-PATH-U !
   s" the open archive's snapshot exists" T-LABEL
   SNAP-PATH SNAP-PATH-U @ FS:EXISTS? TTRUE
   archive CLOSE
   s" CLOSE removed the archive's snapshot" T-LABEL
   SNAP-PATH SNAP-PATH-U @ FS:EXISTS? TFALSE ;

\ Even a failed open registers the retirement; every PREPARE leaves nothing.
: LIFECYCLE-RUN ( -- )
   T-RESET FS:CLEANUP-RESET
   LIFECYCLE-SETUP
   PREPARE LIFECYCLE-ASSERT-CLEAR
   LIFECYCLE-USE REGISTERED @ 1 T=
   PREPARE LIFECYCLE-ASSERT-CLEAR
   LIFECYCLE-ARCHIVES
   LIFECYCLE-USE PREPARE LIFECYCLE-ASSERT-CLEAR
   SNAPSHOT-REMOVED
   FORK-UNDER-ZIP-LOCK
   HALT-DURING-CLOSE
   FS:CLEANUP-RUN T-REPORT ;


LIFECYCLE-RUN
;using
;using
;package
