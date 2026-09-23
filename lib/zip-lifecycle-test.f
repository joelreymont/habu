\ zip-lifecycle-test.f - process-local libzip reset and retry coverage.
require lib/zip.f
require lib/zip-test-fixture.f
require lib/test.f

package ZIP-TEST
public
EXPORT ORIGINAL$
;package

package ZIP
using FFI
using IMAGE-LIFECYCLE

create LIFECYCLE-PATH $80 allot
variable SAVED-DLCLOSE
create BASE FS-PATH-CAP allot
variable BASE-LEN
create PATH-BUF FS-PATH-CAP allot
TYPED-VARIABLE OWNER archive
variable OWNER-LIVE
variable OWNER-CLOSES
variable OWNER-RESULT


: INPUT$ ( -- ptr u8 n )
   BASE BASE-LEN @ s" input.zip" PATH-BUF JOIN-PATH PATH-BUF swap ;


: LIFECYCLE-SETUP ( -- )
   s" habu-zip-lifecycle" HB-TMP-MKDIR {: directory bytes:n :}
   directory BASE bytes BYTE-COPY bytes BASE-LEN !
   BASE bytes CLEANUP-TREE+
   INPUT$ ZIP-TEST:ORIGINAL$ WRITE-ALL ;


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
   ARCHIVES @ NULL? TTRUE
   STAT-BYTES 0 ?do STAT-DATA i + c@ 0 T= loop ;


: LIFECYCLE-PATH$ ( -- ptr u8 )
   s" /no/such/habu-zip-lifecycle.zip" LIFECYCLE-PATH CSTR
   LIFECYCLE-PATH ;


: LIFECYCLE-USE ( -- )
   LIFECYCLE-PATH$ 0 C-OPEN >CELL 0 T= ;


: LIFECYCLE-ASSERT-CLEAR ( -- )
   ASSERT-RETIRED
   LIBRARY @ 0 T= INITIALIZED @ 0 T= REGISTERED @ 0 T=
   DLCLOSE-FN @ 0 T= OPEN-FN @ 0 T= COUNT-FN @ 0 T=
   STAT-FN @ 0 T= FOPEN-FN @ 0 T= FREAD-FN @ 0 T=
   FCLOSE-FN @ 0 T= DISCARD-FN @ 0 T=
   MKSTEMP-FN @ 0 T= CLOSE-FD-FN @ 0 T= ;


\ Harmless native fixture: ignore x0 and return one so LIBRARY-CLOSE fails.
TRUSTED: LIFECYCLE-FAIL-CLOSE ( -- n )
   cp@ {: fn:n :}
   $D2800020 fn patch32
   $D65F03C0 fn 4 + patch32
   fn ;


: LIFECYCLE-PARTIAL ( -- )
   REGISTER-CLEANUP
   s" dlclose" GLOBAL-SYMBOL-FIND DLCLOSE-FN !
   LIBRARY-OPEN
   s" zip_open" SYMBOL-FIND OPEN-FN !
   INITIALIZED @ 0 T= REGISTERED @ 1 T=
   LIBRARY @ 0 T<> OPEN-FN @ 0 T<> COUNT-FN @ 0 T=
   PREPARE
   LIFECYCLE-ASSERT-CLEAR ;


: LIFECYCLE-RETRY ( -- )
   OWNER-OPEN dup READ-STORED drop {: archive:archive :}
   DLCLOSE-FN @ SAVED-DLCLOSE !
   LIFECYCLE-FAIL-CLOSE DLCLOSE-FN !
   [: PREPARE ;] catch E-LIBRARY T=
   ASSERT-RETIRED OWNER-LIVE @ 1 T=
   archive [: STALE-COUNT ;] catch E-HANDLE T= drop
   INITIALIZED @ 0 T= REGISTERED @ 1 T=
   LIBRARY @ 0 T<> OPEN-FN @ 0 T<>
   SAVED-DLCLOSE @ DLCLOSE-FN !
   PREPARE LIFECYCLE-ASSERT-CLEAR
   OWNER-LIVE @ 0 T= OWNER-RESULT @ E-HANDLE T= ;


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


: LIFECYCLE-RUN ( -- )
   T-RESET CLEANUP-RESET
   LIFECYCLE-SETUP
   PREPARE LIFECYCLE-ASSERT-CLEAR
   LIFECYCLE-USE
   INITIALIZED @ 1 T= REGISTERED @ 1 T=
   LIBRARY @ 0 T<> OPEN-FN @ 0 T<>
   PREPARE LIFECYCLE-ASSERT-CLEAR
   LIFECYCLE-USE
   INITIALIZED @ 1 T= REGISTERED @ 1 T=
   PREPARE LIFECYCLE-ASSERT-CLEAR
   LIFECYCLE-PARTIAL
   LIFECYCLE-ARCHIVES
   LIFECYCLE-RETRY
   LIFECYCLE-USE PREPARE LIFECYCLE-ASSERT-CLEAR
   CLEANUP-RUN T-REPORT ;


LIFECYCLE-RUN
;using
;using
;package
