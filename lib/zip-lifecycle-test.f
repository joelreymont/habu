\ zip-lifecycle-test.f - live libzip archives retired by image preparation.
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


\ Even a failed open registers the retirement; every PREPARE leaves nothing.
: LIFECYCLE-RUN ( -- )
   T-RESET CLEANUP-RESET
   LIFECYCLE-SETUP
   PREPARE LIFECYCLE-ASSERT-CLEAR
   LIFECYCLE-USE REGISTERED @ 1 T=
   PREPARE LIFECYCLE-ASSERT-CLEAR
   LIFECYCLE-ARCHIVES
   LIFECYCLE-USE PREPARE LIFECYCLE-ASSERT-CLEAR
   CLEANUP-RUN T-REPORT ;


LIFECYCLE-RUN
;using
;using
;package
