\ A rejected strict FILE export cannot reuse sections from an earlier unit.
\ Repeatable artifact: first.unit printed after the assertions.
\ bin/hb --load test/native-file-object-strict-e2e.f

require src/lib/test.f
require src/lib/fs.f
require src/compiler/workspace.f
require tools/native-unit-object.f

package NATIVE-FILE-STRICT-TEST
private

create ROOT FS-PATH-CAP allot variable ROOT-U
create GOOD FS-PATH-CAP allot variable GOOD-U
create STALE FS-PATH-CAP allot variable STALE-U
create KEY 64 allot
variable SAVED-CHECK
CAST: CHECK-XT ( n -- [ ptr u8 n -- n ] )

: GOOD$ ( -- ptr u8 n ) GOOD GOOD-U @ ;
: STALE$ ( -- ptr u8 n ) STALE STALE-U @ ;
: PACK-STALE ( -- )
   CWORK:FILE-MARK
   KEY 64 s" " s" FILE" NUNIT-OBJECT:PACK-NAMED 2drop ;
: PACK-STALE-INPUTS ( -- )
   CWORK:FILE-MARK
   KEY 64 s" " s" FILE" s" "
      NUNIT-OBJECT:PACK-NAMED-INPUTS 2drop ;
: EXPORT-STALE ( -- )
   CWORK:FILE-MARK
   STALE$ KEY 64 s" " s" FILE" NUNIT-OBJECT:EXPORT-NAMED ;
: PACK-UNTRACKED ( -- )
   CWORK:FILE-MARK
   CELL CWORK:TAKE drop
   KEY 64 s" " s" FILE" NUNIT-OBJECT:PACK-NAMED 2drop ;

public

: OTHER-CHECK ( ptr u8 n -- n ) SAVED-CHECK @ CHECK-XT execute ;
: ORIGINAL-CHECK ( -- n ) SAVED-CHECK @ ;
: START ( -- )
   T-RESET
   s" strict FILE export rejects a changed check owner after an earlier pack" T-LABEL
   s" native-file-object-strict" HB-TMP-MKDIR {: root:ptr size:n :}
   root ROOT size BYTE-COPY size ROOT-U !
   ROOT ROOT-U @ s" first.unit" GOOD JOIN-PATH GOOD-U !
   ROOT ROOT-U @ s" stale.unit" STALE JOIN-PATH STALE-U !
   NFILE-REF:BIND-SEED
   s" strict-export" NFILE-REF:FILE-INSTANCE!
   NUNIT-CAPTURE:BIND-PRODUCER
   NUNIT-CAPTURE:START-PREFIX
   NUNIT-ORIGIN:MARK
   [: NUNIT-CAPTURE:START-UNIT ;] NUNIT-CAPTURE:WITH
   KEY 64 s" " s" UNIT" s" "
      NUNIT-OBJECT:PACK-NAMED-INPUTS {: bytes:ptr count:n :}
   count 0 > TTRUE
   GOOD$ bytes count WRITE-ALL
   check@ SAVED-CHECK ! ;

: REFUSE ( -- )
   NUNIT-ORIGIN:ELIGIBLE? TFALSE
   ['] PACK-STALE E-NUNIT-PROFILE TTHROWSQ
   ['] PACK-STALE-INPUTS E-NUNIT-PROFILE TTHROWSQ
   ['] EXPORT-STALE E-NUNIT-PROFILE TTHROWSQ
   STALE$ EXISTS? TFALSE
   s" a caught CWORK export failure closes the FILE range" T-LABEL
   ['] PACK-UNTRACKED E-LAYOUT-BUFFER TTHROWSQ
   ['] PACK-STALE E-NUNIT-PROFILE TTHROWSQ
   NUNIT-OBJECT:CLOSE
   T-REPORT
   s" strict object export artifact: " type GOOD$ type cr ;
;package

NATIVE-FILE-STRICT-TEST:START
' NATIVE-FILE-STRICT-TEST:OTHER-CHECK set-check
NATIVE-FILE-STRICT-TEST:REFUSE
NATIVE-FILE-STRICT-TEST:ORIGINAL-CHECK set-check
