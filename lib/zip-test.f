\ zip-test.f - public lifecycle and independent archive-preservation checks.
require lib/zip.f
require lib/zip-test-fixture.f
require lib/zip-framing-fixture.f
require lib/zip-count-fixture.f
require lib/test.f
require lib/fs.f
require lib/fs-mutate.f

package ZIP-TEST
using ZIP
using MEM

$10000 constant RAW-CAP
create RAW RAW-CAP allot
create BASE FS-PATH-CAP allot
variable BASE-LEN
create PATH-BUF FS-PATH-CAP allot
create PAYLOAD $20 allot

: BASE$ ( -- ptr u8 n ) BASE BASE-LEN @ ;
: PATH$ ( ptr u8 n -- ptr u8 n )
   BASE$ 2swap PATH-BUF JOIN-PATH PATH-BUF swap ;
: INPUT$ ( -- ptr u8 n ) s" input.zip" PATH$ ;
: OTHER$ ( -- ptr u8 n ) s" other.zip" PATH$ ;
: MOVED$ ( -- ptr u8 n ) s" moved" PATH$ ;
: OLD-DIR$ ( -- ptr u8 n ) s" gone" PATH$ ;
: FAIL-PATH$ ( -- ptr u8 n ) s" gone/edit.zip" PATH$ ;

: SETUP ( -- )
   s" habu-zip" HB-TMP-MKDIR {: dir:ptr len:n :}
   dir BASE len BYTE-COPY len BASE-LEN !
   BASE$ CLEANUP-TREE+ INPUT$ ORIGINAL$ WRITE-ALL
   OTHER$ ORIGINAL$ WRITE-ALL ;

: MEMBER= ( ZIP:archive n ptr u8 n -- ) {: archive:ZIP:archive idx:n want:ptr len:n :}
   archive idx ENTRY {: member:ZIP:entry :}
   archive member ZIP:READ want len T$= ;

: READ-MEMBERS ( -- )
   INPUT$ ZIP:OPEN {: archive:ZIP:archive :}
   archive ZIP:COUNT 5 T=
   archive 0 s" stored value" MEMBER=
   archive 2 s" " MEMBER=
   archive 3 s" first" MEMBER=
   archive 4 s" second" MEMBER=
   archive 1 ENTRY {: member:ZIP:entry :}
   archive member NAME$ s" deflated.txt" T$=
   archive member ZIP:READ {: data:ptr len:n :}
   len 340 T= data 17 s" compressed value " T$=
   archive ZIP:CLOSE ;

: DUP-NAMES ( -- )
   INPUT$ ZIP:OPEN {: archive:ZIP:archive :}
   archive 3 ENTRY archive 4 ENTRY {: first:ZIP:entry second:ZIP:entry :}
   archive first NAME$ archive second NAME$ T$=
   archive first ZIP:READ s" first" T$=
   archive second ZIP:READ s" second" T$=
   archive ZIP:CLOSE ;

: NEGATIVE-INDEX ( ZIP:archive -- ZIP:archive ) dup -1 ENTRY drop ;
: LARGE-INDEX ( ZIP:archive -- ZIP:archive ) dup 5 ENTRY drop ;
: STALE-COUNT ( ZIP:archive -- ZIP:archive ) dup ZIP:COUNT drop ;
: CLOSE-AGAIN ( ZIP:archive -- ZIP:archive ) dup ZIP:CLOSE ;
: WRONG-READ ( ZIP:archive ZIP:entry -- ZIP:archive ZIP:entry )
   2dup ZIP:READ 2drop ;
: WRONG-NAME ( ZIP:archive ZIP:entry -- ZIP:archive ZIP:entry )
   2dup NAME$ 2drop ;
: TRY-REPLACE ( ZIP:archive ZIP:entry -- ZIP:archive ZIP:entry )
   2dup s" replacement" REPLACE ;
: TRY-COMMIT ( ZIP:archive -- ZIP:archive ) dup COMMIT ;

: REJECT-INDICES ( -- )
   INPUT$ ZIP:OPEN
   [: NEGATIVE-INDEX ;] catch E-ENTRY T=
   [: LARGE-INDEX ;] catch E-ENTRY T=
   ZIP:CLOSE ;

: REJECT-HANDLES ( -- )
   INPUT$ ZIP:OPEN {: archive:ZIP:archive :}
   archive 0 ENTRY {: member:ZIP:entry :}
   OTHER$ ZIP:OPEN {: other:ZIP:archive :}
   other member [: WRONG-READ ;] catch E-ENTRY T= 2drop
   other member [: WRONG-NAME ;] catch E-ENTRY T= 2drop
   archive member [: TRY-REPLACE ;] catch E-READONLY T= 2drop
   archive ZIP:CLOSE
   archive [: STALE-COUNT ;] catch E-HANDLE T= drop
   archive [: CLOSE-AGAIN ;] catch E-HANDLE T= drop
   archive member [: WRONG-READ ;] catch E-HANDLE T= 2drop
   other member [: WRONG-READ ;] catch E-ENTRY T= 2drop
   other ZIP:CLOSE ;

: MALFORMED ( -- ) s" invalid.zip" PATH$ ZIP:OPEN drop ;
: EMPTY-PATH ( -- ) s" " ZIP:OPEN drop ;
: NUL-PATH ( -- ) S\" a\x00b" ZIP:OPEN drop ;
: BAD-PATHS ( -- )
   s" invalid.zip" PATH$ s" this is not a ZIP" WRITE-ALL
   [: MALFORMED ;] E-OPEN TTHROWSQ
   [: EMPTY-PATH ;] E-PATH TTHROWSQ
   [: NUL-PATH ;] E-PATH TTHROWSQ ;

: CORRUPT-FIXTURE ( -- )
   ORIGINAL$ {: src:ptr len:n :}
   src RAW len BYTE-COPY $58 RAW STORED-DATA-OFF + c!
   s" corrupt.zip" PATH$ RAW len WRITE-ALL ;

: READ-CORRUPT ( ZIP:archive -- ZIP:archive )
   dup 0 ENTRY over swap ZIP:READ 2drop ;

: CRC-FAILURE ( -- )
   CORRUPT-FIXTURE s" corrupt.zip" PATH$ ZIP:OPEN
   [: READ-CORRUPT ;] catch E-READ T= ZIP:CLOSE ;

: FILL-PAYLOAD ( -- ) s" replacement" PAYLOAD swap BYTE-COPY ;

: REPLACE-MEMBERS ( -- )
   INPUT$ EDIT {: archive:ZIP:archive :}
   archive 0 ENTRY {: member:ZIP:entry :}
   archive member ZIP:READ {: old:ptr old-len:n :}
   FILL-PAYLOAD archive member PAYLOAD 11 REPLACE
   $58 PAYLOAD c!
   archive member ZIP:READ s" replacement" T$=
   old old-len s" stored value" T$=
   archive 4 ENTRY {: duplicate:ZIP:entry :}
   archive duplicate s" " REPLACE
   archive duplicate ZIP:READ nip 0 T=
   archive COMMIT
   archive [: STALE-COUNT ;] catch E-HANDLE T= drop ;

: READ-REPLACED ( -- )
   INPUT$ ZIP:OPEN {: archive:ZIP:archive :}
   archive 0 s" replacement" MEMBER=
   archive 3 s" first" MEMBER=
   archive 4 s" " MEMBER=
   archive ZIP:CLOSE ;

: RAW-HAS ( ptr u8 n -- ) {: expected:ptr len:n :}
   INPUT$ RAW RAW-CAP READ-ALL {: size:n :}
   RAW size expected len CONTAINS? TTRUE ;

: PRESERVED ( -- )
   KEEP-LOCAL$ RAW-HAS
   KEEP-CENTRAL-HEAD$ RAW-HAS
   KEEP-CENTRAL-TAIL$ RAW-HAS
   s" archive comment" RAW-HAS ;

: DISCARD-CHANGES ( -- )
   INPUT$ EDIT {: archive:ZIP:archive :}
   archive 0 ENTRY {: member:ZIP:entry :}
   archive member s" discarded" REPLACE archive ZIP:CLOSE
   READ-REPLACED ;

: MOVE-DIRECTORY ( -- )
   OLD-DIR$ FS-PATH-CAP MEM-ALLOC-BYTES {: old:ptr old-len:n buf:ptr cap:n :}
   old buf old-len BYTE-COPY
   buf old-len MOVED$ RENAME-FILE
   buf cap BYTES-ALLOC-LEN RELEASE-BYTES ;

: COMMIT-FAILURE ( -- )
   OLD-DIR$ MAKE-DIR FAIL-PATH$ ORIGINAL$ WRITE-ALL
   FAIL-PATH$ EDIT {: archive:ZIP:archive :}
   archive 0 ENTRY {: member:ZIP:entry :}
   archive member s" changed" REPLACE MOVE-DIRECTORY
   archive [: TRY-COMMIT ;] catch E-COMMIT T= drop
   archive ZIP:COUNT 5 T=
   archive member ZIP:READ s" changed" T$=
   archive ZIP:CLOSE ;

: SAVE-INPUT ( -- )
   INPUT$ {: path:ptr len:n :}
   path RAW len BYTE-COPY RAW len s" saved.zip" PATH$ RENAME-FILE
   INPUT$ MAKE-DIR ;

: RESTORE-INPUT ( -- )
   INPUT$ REMOVE-DIR s" saved.zip" PATH$ {: path:ptr len:n :}
   path RAW len BYTE-COPY RAW len INPUT$ RENAME-FILE ;

: NO-TEMP ( ptr u8 n -- )
   BASENAME s" .habu-zip-" CONTAINS? TFALSE ;

: INSTALL-FAILURE ( -- )
   INPUT$ ORIGINAL$ WRITE-ALL INPUT$ EDIT {: archive:ZIP:archive :}
   archive 0 ENTRY archive swap s" pending" REPLACE SAVE-INPUT
   archive [: TRY-COMMIT ;] catch E-COMMIT T= drop
   archive 0 s" pending" MEMBER= archive 3 s" first" MEMBER=
   BASE$ [: NO-TEMP ;] WALK-FILES
   RESTORE-INPUT archive COMMIT
   INPUT$ ZIP:OPEN {: result:ZIP:archive :}
   result 0 s" pending" MEMBER= result ZIP:CLOSE ;

: READONLY-COMMIT ( -- )
   INPUT$ ORIGINAL$ WRITE-ALL INPUT$ ZIP:OPEN COMMIT
   INPUT$ RAW RAW-CAP READ-ALL RAW swap ORIGINAL$ T$= ;

: STALE-SOURCE ( ZIP:archive -- ZIP:archive ) dup SOURCE$ 2drop ;

: ORIGINAL-SOURCE ( -- )
   INPUT$ ORIGINAL$ WRITE-ALL INPUT$ EDIT {: archive:ZIP:archive :}
   archive SOURCE$ {: data:ptr len:n :}
   data len ORIGINAL$ T$=
   archive 0 ENTRY archive swap s" pending" REPLACE
   archive SOURCE$ ORIGINAL$ T$= data len ORIGINAL$ T$=
   archive ZIP:CLOSE
   archive [: STALE-SOURCE ;] catch E-HANDLE T= drop ;

: MANY-HANDLES ( -- )
   INPUT$ ZIP:OPEN {: archive:ZIP:archive :}
   257 0 ?do archive 0 ENTRY archive swap NAME$ 2drop loop
   archive ZIP:CLOSE ;

: REPLACE-FIRST ( -- )
   INPUT$ EDIT {: archive:ZIP:archive :}
   archive 1 ENTRY archive swap NAME$ s" raw.txt" T$=
   archive 0 ENTRY archive swap s" replacement text" REPLACE archive COMMIT ;

: FRAMING-CASE ( ptr u8 n ptr u8 n -- ) {: before:ptr before-len:n after:ptr after-len:n :}
   INPUT$ before before-len WRITE-ALL REPLACE-FIRST
   INPUT$ RAW RAW-CAP READ-ALL {: len:n :}
   RAW len after after-len T$=
   INPUT$ ZIP:OPEN {: archive:ZIP:archive :}
   archive 0 s" replacement text" MEMBER= archive 1 s" keep" MEMBER=
   archive ZIP:CLOSE ;

: RAW-FRAMING ( -- )
   FRAMED-BEFORE$ FRAMED-AFTER$ FRAMING-CASE
   WIDE-BEFORE$ WIDE-AFTER$ FRAMING-CASE
   FAKE-END-BEFORE$ FAKE-END-AFTER$ FRAMING-CASE
   PREFIX-BEFORE$ PREFIX-AFTER$ FRAMING-CASE ;

$10000 constant WIDE-COUNT

: COUNT-OFFSET! ( n ptr u8 -- ) {: value:n data:ptr :}
   4 0 ?do value i 8 * rshift data i + c! loop ;

: COUNT-FIXTURE ( ptr u8 -- n ) {: data:ptr :}
   COUNT-LOCAL$ {: local:ptr local-len:n :}
   COUNT-CENTRAL$ {: central:ptr central-len:n :}
   WIDE-COUNT 0 ?do local data i local-len * + local-len BYTE-COPY loop
   data WIDE-COUNT local-len * + {: directory:ptr :}
   WIDE-COUNT 0 ?do
      directory i central-len * + {: dest:ptr :}
      central dest central-len BYTE-COPY i local-len * dest $2A + COUNT-OFFSET!
   loop
   WIDE-COUNT local-len central-len + * {: end-off:n :}
   COUNT-END-BEFORE$ {: tail:ptr len:n :}
   tail data end-off + len BYTE-COPY end-off len + ;

: COUNT-CONTENT ( -- )
   INPUT$ ZIP:OPEN {: archive:ZIP:archive :}
   archive ZIP:COUNT WIDE-COUNT T=
   archive 0 s" replacement text" MEMBER=
   archive WIDE-COUNT 1- s" " MEMBER= archive ZIP:CLOSE ;

: ZIP64-COUNT ( -- )
   COUNT-LOCAL$ nip COUNT-CENTRAL$ nip + WIDE-COUNT * $100 +
   MEM-ALLOC-BYTES {: data:ptr cap:n :}
   INPUT$ data data COUNT-FIXTURE WRITE-ALL
   INPUT$ EDIT {: archive:ZIP:archive :}
   archive ZIP:COUNT WIDE-COUNT T=
   archive 0 ENTRY archive swap s" replacement text" REPLACE archive COMMIT
   INPUT$ data cap READ-ALL {: len:n :}
   COUNT-END-AFTER$ {: expected:ptr size:n :}
   data len size - + size expected size T$=
   data cap BYTES-ALLOC-LEN RELEASE-BYTES COUNT-CONTENT ;

: TYPE-REJECTIONS ( -- )
   s" WRONG-HANDLE ( ZIP:entry -- n ) ZIP:COUNT" CHECK! 0 T=
   s" WRONG-ENTRY ( ZIP:archive ZIP:archive -- ptr u8 n ) ZIP:READ" CHECK! 0 T=
   s" FORGED-HANDLE ( n -- ZIP:archive )" CHECK! 0 T= ;

: RUN ( -- )
   T-RESET CLEANUP-RESET SETUP
   READ-MEMBERS DUP-NAMES REJECT-INDICES REJECT-HANDLES BAD-PATHS
   REPLACE-MEMBERS READ-REPLACED PRESERVED DISCARD-CHANGES
   COMMIT-FAILURE INSTALL-FAILURE READONLY-COMMIT CRC-FAILURE MANY-HANDLES ORIGINAL-SOURCE RAW-FRAMING ZIP64-COUNT TYPE-REJECTIONS
   CLEANUP-RUN T-REPORT ;

RUN
;using
;using
;package

require lib/zip-offset-test.f
