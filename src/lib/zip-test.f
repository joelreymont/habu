\ zip-test.f - public lifecycle and independent archive-preservation checks.
require src/lib/string.f
require src/lib/zip.f
require src/lib/zip-test-fixture.f
require src/lib/zip-framing-fixture.f
require src/lib/zip-count-fixture.f
require src/lib/zip-boundary-fixture.f
require src/lib/test.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/task.f

package ZIP-TEST
using ZIP
using MEM

$10000 constant RAW-CAP
create RAW RAW-CAP allot
create BASE FS-PATH-CAP allot
variable BASE-LEN
create PATH-BUF FS-PATH-CAP allot
create PAYLOAD $20 allot
create STREAM-BUF $200 allot
create TASK-PATH-A FS-PATH-CAP allot
create TASK-PATH-B FS-PATH-CAP allot
variable TASK-PATH-A-LEN
variable TASK-PATH-B-LEN
TASK:MIN-STACK TASK:TASK ZIP-READER-A
TASK:MIN-STACK TASK:TASK ZIP-READER-B
TASK:SEMAPHORE ZIP-START
1000 constant ZIP-TASK-ROUNDS

: BASE$ ( -- ptr u8 n ) BASE BASE-LEN @ ;
: PATH$ ( ptr u8 n -- ptr u8 n )
   BASE$ 2swap PATH-BUF JOIN-PATH PATH-BUF swap ;
: INPUT$ ( -- ptr u8 n ) s" input.zip" PATH$ ;
: OTHER$ ( -- ptr u8 n ) s" other.zip" PATH$ ;
: MOVED$ ( -- ptr u8 n ) s" moved" PATH$ ;
: OLD-DIR$ ( -- ptr u8 n ) s" gone" PATH$ ;
: FAIL-PATH$ ( -- ptr u8 n ) s" gone/edit.zip" PATH$ ;

: SETUP ( -- )
   s" habu-zip" TMPDIR-MKDIR {: dir:ptr len:n :}
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
: READONLY-FILL ( n SPAN:span<u8> -- ) 2drop ;
: TRY-REPLACE-WITH ( ZIP:archive ZIP:entry -- ZIP:archive ZIP:entry )
   2dup 11 0 [: READONLY-FILL ;] ZIP:REPLACE-WITH ;
: TRY-COMMIT ( ZIP:archive -- ZIP:archive ) dup COMMIT ;

: REJECT-INDICES ( -- )
   INPUT$ ZIP:OPEN {: archive:ZIP:archive :}
   archive [: NEGATIVE-INDEX ;] catch E-ENTRY T= drop
   archive [: LARGE-INDEX ;] catch E-ENTRY T= drop
   archive ZIP:CLOSE ;

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
   s" invalid.zip" PATH$ s" temporarily unavailable" WRITE-ALL
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
   CORRUPT-FIXTURE s" corrupt.zip" PATH$ ZIP:OPEN {: archive:ZIP:archive :}
   archive [: READ-CORRUPT ;] catch E-READ T= drop
   archive ZIP:CLOSE ;

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

: REMOVED-FIRST-KEEPS-ORDER ( -- )
   INPUT$ ORIGINAL$ WRITE-ALL
   INPUT$ ZIP:EDIT {: archive:ZIP:archive :}
   archive archive 0 ENTRY ZIP:REMOVE archive COMMIT
   INPUT$ ZIP:OPEN {: result:ZIP:archive :}
   result ZIP:COUNT 4 T=
   result 0 ENTRY result swap NAME$ s" deflated.txt" T$=
   result 1 ENTRY result swap NAME$ s" empty" T$=
   result 2 s" first" MEMBER=
   result 3 s" second" MEMBER=
   result ZIP:CLOSE
   INPUT$ RAW RAW-CAP READ-ALL {: size:n :}
   RAW size s" stored value" CONTAINS? TFALSE ;

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
   INPUT$ RAW RAW-CAP READ-ALL RAW swap ORIGINAL$ T$=
   INPUT$ ZIP:OPEN-FILE {: archive:ZIP:archive :}
   archive 0 ENTRY {: member:ZIP:entry :}
   archive member [: TRY-REPLACE ;] catch E-READONLY T= 2drop
   archive member [: TRY-REPLACE-WITH ;] catch E-READONLY T= 2drop
   archive 0 s" stored value" MEMBER=
   archive COMMIT
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
   archive 0 ENTRY {: first:ZIP:entry :}
   archive first NAME$ {: name:ptr name-len:n :}
   768 0 ?do archive 1 ENTRY archive swap NAME$ 2drop loop
   name name-len s" stored.txt" T$=
   archive first ZIP:READ s" stored value" T$=
   archive ZIP:CLOSE
   archive first [: WRONG-READ ;] catch E-HANDLE T= 2drop ;

: REPLACE-FIRST ( -- )
   INPUT$ EDIT {: archive:ZIP:archive :}
   archive 1 ENTRY archive swap NAME$ s" raw.txt" T$=
   archive 0 ENTRY archive swap s" replacement text" REPLACE archive COMMIT ;

: FRAMING-CASE ( ptr u8 n ptr u8 n -- )
   {: before:ptr before-len:n after:ptr after-len:n :}
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
   data cap BYTES-ALLOC-LEN RELEASE-BYTES COUNT-CONTENT
   INPUT$ ZIP:OPEN-FILE {: file:ZIP:archive :}
   file ZIP:COUNT WIDE-COUNT T=
   file WIDE-COUNT 1- s" " MEMBER=
   file ZIP:CLOSE
   INPUT$ ZIP:EDIT {: edit:ZIP:archive :}
   edit edit WIDE-COUNT 1- ENTRY ZIP:REMOVE edit ZIP:COMMIT
   INPUT$ ZIP:OPEN-FILE {: kept:ZIP:archive :}
   kept ZIP:COUNT WIDE-COUNT 1- T=
   kept 0 s" replacement text" MEMBER=
   kept WIDE-COUNT 2 - s" " MEMBER=
   kept ZIP:CLOSE ;

: TYPE-REJECTIONS ( -- )
   s" WRONG-HANDLE ( ZIP:entry -- n ) ZIP:COUNT" CHECK! 0 T=
   s" WRONG-ENTRY ( ZIP:archive ZIP:archive -- ptr u8 n ) ZIP:READ" CHECK! 0 T=
   s" WRONG-INPUT ( ZIP:archive SPAN:span<u8> -- n ) ZIP:READ-IN" CHECK! 0 T=
   s" WRONG-SOURCE ( ZIP:entry -- ZIP:input ) ZIP:OPEN-SOURCE" CHECK! 0 T=
   s" FORGED-HANDLE ( n -- ZIP:archive )" CHECK! 0 T= ;

: FILE-MEMBERS ( -- )
   INPUT$ ORIGINAL$ WRITE-ALL
   INPUT$ ZIP:OPEN-FILE {: archive:ZIP:archive :}
   archive ZIP:COUNT 5 T=
   archive 0 s" stored value" MEMBER=
   archive 1 ENTRY archive swap ZIP:READ nip 340 T=
   archive 2 s" " MEMBER=
   archive ZIP:CLOSE ;

: FILE-STREAM-ONE ( ZIP:archive n ptr u8 n -- )
   {: archive:ZIP:archive idx:n want:ptr size:n :}
   archive archive idx ENTRY ZIP:OPEN-ENTRY {: input:ZIP:input :}
   0 begin dup size < while
      dup {: off:n :}
      size off - 7 min {: cap:n :}
      input STREAM-BUF off + cap SPAN:MAKE ZIP:READ-IN
      dup 0= if E-READ throw then +
   repeat drop
   input STREAM-BUF 1 SPAN:MAKE ZIP:READ-IN 0 T=
   input STREAM-BUF 1 SPAN:MAKE ZIP:READ-IN 0 T=
   input ZIP:CLOSE-ENTRY
   STREAM-BUF size want size T$= ;

: FILE-STREAMS ( -- )
   INPUT$ ORIGINAL$ WRITE-ALL
   INPUT$ ZIP:OPEN-FILE {: archive:ZIP:archive :}
   archive 0 s" stored value" FILE-STREAM-ONE
   archive 1 archive archive 1 ENTRY ZIP:READ FILE-STREAM-ONE
   archive 2 s" " FILE-STREAM-ONE
   archive 3 s" first" FILE-STREAM-ONE
   archive 4 s" second" FILE-STREAM-ONE
   archive ZIP:CLOSE ;

: FILE-SOURCE-STREAM ( ZIP:archive -- ) {: archive:ZIP:archive :}
   archive ZIP:OPEN-SOURCE {: input:ZIP:input :}
   ORIGINAL$ {: expected:ptr size:n :}
   0 begin dup size < while
      dup {: off:n :}
      input RAW off + size off - 7 min SPAN:MAKE ZIP:READ-IN
      dup 0= if E-READ throw then +
   repeat drop
   input RAW 1 SPAN:MAKE ZIP:READ-IN 0 T=
   input ZIP:CLOSE-ENTRY
   RAW size expected size T$= ;

: FILE-SNAPSHOT ( -- )
   INPUT$ ORIGINAL$ WRITE-ALL
   INPUT$ ZIP:OPEN-FILE {: archive:ZIP:archive :}
   INPUT$ s" changed at original path" WRITE-ALL
   archive 0 s" stored value" MEMBER=
   archive FILE-SOURCE-STREAM
   archive ZIP:OPEN-SOURCE {: early:ZIP:input :}
   early RAW 1 SPAN:MAKE ZIP:READ-IN 1 T=
   early ZIP:CLOSE-ENTRY
   archive ZIP:CLOSE ;

: FILL-BYTE ( n SPAN:span<u8> -- ) {: byte:n dst :}
   dst SPAN:$ {: data:ptr len:n :}
   len 0 ?do byte data i + c! loop ;

: FAIL-FILL ( n SPAN:span<u8> -- )
   2drop E-WRITE throw ;

: TRY-FILL ( ZIP:archive ZIP:entry -- ZIP:archive ZIP:entry )
   2dup 11 0 [: FAIL-FILL ;] ZIP:REPLACE-WITH ;

: FILE-REPLACE-WITH ( -- )
   INPUT$ ORIGINAL$ WRITE-ALL
   INPUT$ ZIP:EDIT-FILE {: archive:ZIP:archive :}
   archive 0 ENTRY {: member:ZIP:entry :}
   archive member [: TRY-FILL ;] catch E-WRITE T= 2drop
   archive 0 s" stored value" MEMBER=
   archive member 31 65 [: FILL-BYTE ;] ZIP:REPLACE-WITH
   archive 2 ENTRY archive swap ZIP:READ nip 0 T=
   archive COMMIT
   INPUT$ ZIP:OPEN-FILE {: result:ZIP:archive :}
   result 0 ENTRY result swap ZIP:READ {: data:ptr len:n :}
   len 31 T=
   len 0 ?do data i + c@ 65 T= loop
   result ZIP:CLOSE ;

: LARGE-FILE-COPY ( -- )
   INPUT$ ORIGINAL$ WRITE-ALL
   INPUT$ ZIP:EDIT-FILE {: first:ZIP:archive :}
   first first 0 ENTRY 888832 65 [: FILL-BYTE ;] ZIP:REPLACE-WITH
   first COMMIT
   INPUT$ ZIP:EDIT-FILE {: second:ZIP:archive :}
   second 1 ENTRY second swap s" changed" REPLACE second COMMIT
   INPUT$ ZIP:OPEN-FILE {: result:ZIP:archive :}
   result result 0 ENTRY ZIP:OPEN-ENTRY {: input:ZIP:input :}
   0 begin input STREAM-BUF $200 SPAN:MAKE ZIP:READ-IN dup 0<> while
      +
   repeat drop
   888832 T=
   input ZIP:CLOSE-ENTRY
   result 1 s" changed" MEMBER=
   result ZIP:CLOSE ;

: FILE-INSTALL-FAILURE ( -- )
   INPUT$ ORIGINAL$ WRITE-ALL
   INPUT$ ZIP:EDIT-FILE {: archive:ZIP:archive :}
   archive 0 ENTRY archive swap s" pending" REPLACE SAVE-INPUT
   archive [: TRY-COMMIT ;] catch E-COMMIT T= drop
   archive 0 s" pending" MEMBER=
   archive ZIP:COUNT 5 T=
   BASE$ [: NO-TEMP ;] WALK-FILES
   RESTORE-INPUT archive COMMIT
   INPUT$ ZIP:OPEN-FILE {: result:ZIP:archive :}
   result 0 s" pending" MEMBER= result ZIP:CLOSE ;

: FILE-CRC-CLOSE ( -- )
   CORRUPT-FIXTURE
   s" corrupt.zip" PATH$ ZIP:OPEN-FILE {: archive:ZIP:archive :}
   archive archive 0 ENTRY ZIP:OPEN-ENTRY {: input:ZIP:input :}
   input [: dup ZIP:CLOSE-ENTRY ;] catch E-READ T= drop
   archive ZIP:CLOSE ;

: DRAIN-BAD ( ZIP:input -- ZIP:input ) {: input:ZIP:input :}
   begin input STREAM-BUF 7 SPAN:MAKE ZIP:READ-IN 0= until
   input ;

: STALE-INPUT ( ZIP:input SPAN:span<u8> -- ZIP:input SPAN:span<u8> )
   2dup ZIP:READ-IN drop ;

: FILE-CRC-READ ( -- )
   CORRUPT-FIXTURE
   s" corrupt.zip" PATH$ ZIP:OPEN-FILE {: archive:ZIP:archive :}
   archive archive 0 ENTRY ZIP:OPEN-ENTRY {: input:ZIP:input :}
   input [: DRAIN-BAD ;] catch E-READ T= drop
   input STREAM-BUF 1 SPAN:MAKE [: STALE-INPUT ;] catch
      >r 2drop r> E-HANDLE T=
   archive ZIP:CLOSE ;

: SIZE-CASE ( n -- ) {: declared:n :}
   ORIGINAL$ {: data:ptr len:n :}
   data RAW len BYTE-COPY
   declared RAW $128 + c!
   s" size.zip" PATH$ RAW len WRITE-ALL
   s" size.zip" PATH$ ZIP:OPEN-FILE {: archive:ZIP:archive :}
   archive archive 0 ENTRY ZIP:OPEN-ENTRY {: input:ZIP:input :}
   input [: dup ZIP:CLOSE-ENTRY ;] catch E-READ T= drop
   archive ZIP:CLOSE ;

: FILE-SIZE-ERRORS ( -- ) 11 SIZE-CASE 13 SIZE-CASE ;

: FILE-INPUT-RETIRE ( -- )
   INPUT$ ORIGINAL$ WRITE-ALL
   INPUT$ ZIP:OPEN-FILE {: archive:ZIP:archive :}
   archive archive 0 ENTRY ZIP:OPEN-ENTRY {: input:ZIP:input :}
   archive ZIP:CLOSE
   input RAW 1 SPAN:MAKE [: STALE-INPUT ;] catch
   >r 2drop r> E-HANDLE T= ;

: FILE-REPLACE ( -- )
   INPUT$ ZIP:EDIT-FILE {: archive:ZIP:archive :}
   archive 0 ENTRY archive swap s" replacement" REPLACE
   archive 4 ENTRY archive swap s" " REPLACE
   archive COMMIT
   READ-REPLACED PRESERVED ;

: BOUNDARY-PATH$ ( -- ptr u8 n ) s" boundary.zip" PATH$ ;
: BOUNDARY-BASE ( -- n )
   BOUNDARY-STORE$ {: data:ptr len:n :}
   data RAW len BYTE-COPY len ;
: BOUNDARY-EFS ( -- n )
   BOUNDARY-EFS$ {: data:ptr len:n :}
   data RAW len BYTE-COPY len ;
: BOUNDARY-BYTE ( n n -- ) {: off:n value:n :} value RAW off + c! ;
: BOUNDARY-SAVE ( n -- ) {: len:n :} BOUNDARY-PATH$ RAW len WRITE-ALL ;
: BOUNDARY-OPEN ( -- ) BOUNDARY-PATH$ ZIP:OPEN drop ;
: BOUNDARY-GOOD ( -- )
   BOUNDARY-PATH$ ZIP:OPEN {: archive:ZIP:archive :}
   archive ZIP:COUNT 1 T=
   archive 0 s" 0123456789abcdefghijklmnopqrstuv" MEMBER=
   archive archive 0 ENTRY ZIP:OPEN-ENTRY {: input:ZIP:input :}
   input STREAM-BUF 1 SPAN:MAKE ZIP:READ-IN 1 T=
   input ZIP:CLOSE-ENTRY archive ZIP:CLOSE ;
: BOUNDARY-BAD-CLOSE ( -- )
   BOUNDARY-PATH$ ZIP:OPEN {: archive:ZIP:archive :}
   archive archive 0 ENTRY ZIP:OPEN-ENTRY {: input:ZIP:input :}
   input [: dup ZIP:CLOSE-ENTRY ;] catch E-READ T= drop
   archive ZIP:CLOSE ;
: BOUNDARY-BAD-READ ( -- )
   BOUNDARY-PATH$ ZIP:OPEN {: archive:ZIP:archive :}
   archive [: READ-CORRUPT ;] catch E-READ T= drop
   archive ZIP:CLOSE ;
: BOUNDARY-OPEN-ERRORS ( -- )
   BOUNDARY-BASE 134 3 BOUNDARY-BYTE BOUNDARY-SAVE
   [: BOUNDARY-OPEN ;] E-OPEN TTHROWSQ
   BOUNDARY-EFS 31 $FF BOUNDARY-BYTE 121 $FF BOUNDARY-BYTE BOUNDARY-SAVE
   [: BOUNDARY-OPEN ;] E-OPEN TTHROWSQ
   BOUNDARY-EFS 133 $FF BOUNDARY-BYTE BOUNDARY-SAVE
   [: BOUNDARY-OPEN ;] E-OPEN TTHROWSQ
   BOUNDARY-BASE 150 0 BOUNDARY-BYTE 152 0 BOUNDARY-BYTE BOUNDARY-SAVE
   [: BOUNDARY-OPEN ;] E-OPEN TTHROWSQ
   BOUNDARY-BASE 150 2 BOUNDARY-BYTE 152 2 BOUNDARY-BYTE BOUNDARY-SAVE
   [: BOUNDARY-OPEN ;] E-OPEN TTHROWSQ
   BOUNDARY-BASE 8 $FF BOUNDARY-BYTE 9 $FF BOUNDARY-BYTE
      87 $FF BOUNDARY-BYTE 88 $FF BOUNDARY-BYTE BOUNDARY-SAVE
   BOUNDARY-BAD-READ
   BOUNDARY-BASE 6 1 BOUNDARY-BYTE 85 1 BOUNDARY-BYTE BOUNDARY-SAVE
   BOUNDARY-BAD-READ
   BOUNDARY-BASE 8 99 BOUNDARY-BYTE 87 99 BOUNDARY-BYTE BOUNDARY-SAVE
   [: BOUNDARY-OPEN ;] E-OPEN TTHROWSQ ;
: BOUNDARY-READ-ERRORS ( -- )
   BOUNDARY-BASE 93 $52 BOUNDARY-BYTE BOUNDARY-SAVE
   BOUNDARY-BAD-READ BOUNDARY-BAD-CLOSE
   BOUNDARY-BASE 97 $1F BOUNDARY-BYTE BOUNDARY-SAVE
   BOUNDARY-BAD-READ BOUNDARY-BAD-CLOSE
   BOUNDARY-BASE 101 $1F BOUNDARY-BYTE BOUNDARY-SAVE
   BOUNDARY-BAD-READ BOUNDARY-BAD-CLOSE
   BOUNDARY-BASE drop
   BOUNDARY-STORE$ {: data:ptr len:n :}
   data 77 + RAW 76 + len 77 - BYTE-COPY
   157 76 BOUNDARY-BYTE 175 BOUNDARY-SAVE
   BOUNDARY-BAD-READ BOUNDARY-BAD-CLOSE ;
: BOUNDARY-DISAGREEMENT ( -- )
   BOUNDARY-BASE 36 $58 BOUNDARY-BYTE BOUNDARY-SAVE BOUNDARY-GOOD
   BOUNDARY-BASE 129 $58 BOUNDARY-BYTE BOUNDARY-SAVE BOUNDARY-GOOD
   BOUNDARY-BASE 8 8 BOUNDARY-BYTE BOUNDARY-SAVE BOUNDARY-GOOD
   BOUNDARY-BASE 7 8 BOUNDARY-BYTE BOUNDARY-SAVE BOUNDARY-GOOD
   BOUNDARY-BASE 86 8 BOUNDARY-BYTE BOUNDARY-SAVE BOUNDARY-GOOD
   BOUNDARY-BASE 14 $52 BOUNDARY-BYTE BOUNDARY-SAVE BOUNDARY-GOOD
   BOUNDARY-BASE 18 $1F BOUNDARY-BYTE BOUNDARY-SAVE BOUNDARY-GOOD
   BOUNDARY-BASE 22 $1F BOUNDARY-BYTE BOUNDARY-SAVE BOUNDARY-GOOD
   BOUNDARY-BASE 41 3 BOUNDARY-BYTE BOUNDARY-SAVE BOUNDARY-GOOD ;
: BOUNDARY-CURSORS ( -- )
   BOUNDARY-BASE BOUNDARY-SAVE
   BOUNDARY-PATH$ ZIP:OPEN {: archive:ZIP:archive :}
   archive archive 0 ENTRY ZIP:OPEN-ENTRY {: first:ZIP:input :}
   archive archive 0 ENTRY ZIP:OPEN-ENTRY {: second:ZIP:input :}
   first STREAM-BUF 1 SPAN:MAKE ZIP:READ-IN 1 T=
   second STREAM-BUF 2 SPAN:MAKE ZIP:READ-IN 2 T=
   first STREAM-BUF 2 SPAN:MAKE ZIP:READ-IN 2 T=
   STREAM-BUF 2 s" 12" T$=
   second STREAM-BUF 1 SPAN:MAKE ZIP:READ-IN 1 T=
   STREAM-BUF 1 s" 2" T$=
   first ZIP:CLOSE-ENTRY second ZIP:CLOSE-ENTRY archive ZIP:CLOSE ;

: BOUNDARY-NUM! ( n n n -- ) {: value:n off:n width:n :}
   width 0 ?do off i + value i 8 * rshift BOUNDARY-BYTE loop ;

\ Replace the central extra with ZIP64 data; the local header stays intact.
: BOUNDARY-ZIP64 ( n bool -- n ) {: size:n wide:bool :}
   BOUNDARY-STORE$ {: data:ptr len:n :}
   data RAW 132 BYTE-COPY
   data 138 + RAW 136 size + + len 138 - BYTE-COPY
   1 132 2 BOUNDARY-NUM! size 134 2 BOUNDARY-NUM!
   size 4 + 107 2 BOUNDARY-NUM!
   size 0 ?do 136 i + 0 BOUNDARY-BYTE loop
   32 136 8 BOUNDARY-NUM!
   size 24 >= if 32 144 8 BOUNDARY-NUM! then
   wide if $FFFFFFFF 101 4 BOUNDARY-NUM! then
   63 size + 152 size + 4 BOUNDARY-NUM!
   len size 2 - + ;

: BOUNDARY-ZIP64-CASES ( -- )
   8 true BOUNDARY-ZIP64 BOUNDARY-SAVE BOUNDARY-GOOD
   9 true BOUNDARY-ZIP64 BOUNDARY-SAVE
   [: BOUNDARY-OPEN ;] E-OPEN TTHROWSQ
   24 true BOUNDARY-ZIP64 BOUNDARY-SAVE BOUNDARY-GOOD
   24 true BOUNDARY-ZIP64 144 33 BOUNDARY-BYTE BOUNDARY-SAVE
   [: BOUNDARY-OPEN ;] E-OPEN TTHROWSQ
   28 true BOUNDARY-ZIP64 BOUNDARY-SAVE BOUNDARY-GOOD
   28 true BOUNDARY-ZIP64 160 1 BOUNDARY-BYTE BOUNDARY-SAVE
   [: BOUNDARY-OPEN ;] E-OPEN TTHROWSQ
   9 false BOUNDARY-ZIP64 BOUNDARY-SAVE BOUNDARY-GOOD ;

: WRONG-SIZE ( ZIP:archive ZIP:entry -- ZIP:archive ZIP:entry )
   2dup ZIP:COMPRESSED-SIZE drop ;

\ Stored, deflated, pending and ZIP64 entries. The ZIP64 case marks only the
\ central compressed field (offset 97) U32-MAX; its extra field holds 32. The
\ last case's local header says 31 where the central directory says 32.
: COMPRESSED-SIZES ( -- )
   INPUT$ ORIGINAL$ WRITE-ALL
   INPUT$ ZIP:EDIT {: archive:ZIP:archive :}
   archive archive 0 ENTRY ZIP:COMPRESSED-SIZE 12 T=
   archive 1 ENTRY {: member:ZIP:entry :}
   archive member ZIP:COMPRESSED-SIZE 24 T=
   archive member s" replacement" REPLACE
   archive member ZIP:COMPRESSED-SIZE 11 T=
   OTHER$ ZIP:OPEN {: other:ZIP:archive :}
   other member [: WRONG-SIZE ;] catch E-ENTRY T= 2drop
   other ZIP:CLOSE archive ZIP:CLOSE
   archive member [: WRONG-SIZE ;] catch E-HANDLE T= 2drop
   8 false BOUNDARY-ZIP64 $FFFFFFFF 97 4 BOUNDARY-NUM! BOUNDARY-SAVE
   BOUNDARY-PATH$ ZIP:OPEN {: big:ZIP:archive :}
   big big 0 ENTRY ZIP:COMPRESSED-SIZE 32 T=
   big ZIP:CLOSE
   BOUNDARY-BASE 18 $1F BOUNDARY-BYTE BOUNDARY-SAVE
   BOUNDARY-PATH$ ZIP:OPEN {: skew:ZIP:archive :}
   skew skew 0 ENTRY ZIP:COMPRESSED-SIZE 32 T=
   skew ZIP:CLOSE ;

: WIDE-BOUNDARY-BASE ( -- n )
   WIDE-BEFORE$ {: data:ptr len:n :}
   data RAW len BYTE-COPY len ;
: WIDE-BOUNDARY-GOOD ( -- )
   BOUNDARY-PATH$ ZIP:OPEN {: archive:ZIP:archive :}
   archive ZIP:COUNT 3 T= archive ZIP:CLOSE ;
: WIDE-BOUNDARY-BAD ( -- )
   [: BOUNDARY-OPEN ;] E-OPEN TTHROWSQ ;
: WIDE-EOCD-CASES ( -- )
   WIDE-BOUNDARY-BASE BOUNDARY-SAVE WIDE-BOUNDARY-GOOD
   WIDE-BOUNDARY-BASE $FFFF $35E 2 BOUNDARY-NUM!
      $FFFF $360 2 BOUNDARY-NUM!
      $FFFF $362 2 BOUNDARY-NUM!
      $FFFF $364 2 BOUNDARY-NUM!
      $FFFFFFFF $366 4 BOUNDARY-NUM!
      $FFFFFFFF $36A 4 BOUNDARY-NUM!
      BOUNDARY-SAVE WIDE-BOUNDARY-GOOD
   WIDE-BOUNDARY-BASE 1 $34A 4 BOUNDARY-NUM! BOUNDARY-SAVE WIDE-BOUNDARY-BAD
   WIDE-BOUNDARY-BASE 2 $356 4 BOUNDARY-NUM! BOUNDARY-SAVE WIDE-BOUNDARY-BAD
   WIDE-BOUNDARY-BASE 1 $35E 2 BOUNDARY-NUM! BOUNDARY-SAVE WIDE-BOUNDARY-BAD
   WIDE-BOUNDARY-BASE 2 $362 2 BOUNDARY-NUM! BOUNDARY-SAVE WIDE-BOUNDARY-BAD
   WIDE-BOUNDARY-BASE 2 $364 2 BOUNDARY-NUM! BOUNDARY-SAVE WIDE-BOUNDARY-BAD
   WIDE-BOUNDARY-BASE 1 $366 4 BOUNDARY-NUM! BOUNDARY-SAVE WIDE-BOUNDARY-BAD ;

\ Different member sizes expose a concurrent zip_stat_index scratch collision.
: TASK-READ-ONE ( ptr u8 n n ptr u8 n ptr u8 n -- )
   {: path:ptr path-len:n idx:n name:ptr name-len:n data:ptr data-len:n :}
   path path-len ZIP:OPEN {: archive:ZIP:archive :}
   archive ZIP:COUNT 5 <> if E-READ throw then
   archive idx ENTRY {: member:ZIP:entry :}
   archive member NAME$ name name-len STR= 0= if E-READ throw then
   archive member ZIP:READ data data-len STR= 0= if E-READ throw then
   archive ZIP:CLOSE ;

: TASK-PATH-A$ ( -- ptr u8 n ) TASK-PATH-A TASK-PATH-A-LEN @ ;
: TASK-PATH-B$ ( -- ptr u8 n ) TASK-PATH-B TASK-PATH-B-LEN @ ;

: TASK-READ-A ( -- )
   ZIP-START TASK:WAIT
   ZIP-TASK-ROUNDS 0 ?do
      TASK-PATH-A$ 0 s" stored.txt" s" stored value" TASK-READ-ONE
      TASK:PAUSE
   loop
   1 TASK:RETURN ;

: TASK-READ-B ( -- )
   ZIP-START TASK:WAIT
   ZIP-TASK-ROUNDS 0 ?do
      TASK-PATH-B$ 4 s" dup" s" second" TASK-READ-ONE
      TASK:PAUSE
   loop
   1 TASK:RETURN ;

: TASK-JOIN-OK ( result<n,n> -- )
   MATCH result ok OF 1 T= ENDOF err OF 0 T= ENDOF ;MATCH ;

: TASK-READERS ( -- )
   INPUT$ ORIGINAL$ WRITE-ALL OTHER$ ORIGINAL$ WRITE-ALL
   INPUT$ {: path-a:ptr len-a:n :}
   path-a TASK-PATH-A len-a BYTE-COPY len-a TASK-PATH-A-LEN !
   OTHER$ {: path-b:ptr len-b:n :}
   path-b TASK-PATH-B len-b BYTE-COPY len-b TASK-PATH-B-LEN !
   0 ZIP-START TASK:SEMAPHORE-INIT
   ['] TASK-READ-A ZIP-READER-A TASK:ACTIVATE
   ['] TASK-READ-B ZIP-READER-B TASK:ACTIVATE
   ZIP-START TASK:SIGNAL ZIP-START TASK:SIGNAL
   ZIP-READER-A TASK:JOIN TASK-JOIN-OK
   ZIP-READER-B TASK:JOIN TASK-JOIN-OK
   ZIP-START TASK:SEMAPHORE-DESTROY ;

: RUN ( -- )
   T-RESET CLEANUP-RESET SETUP
   READ-MEMBERS DUP-NAMES REJECT-INDICES REJECT-HANDLES BAD-PATHS
   REPLACE-MEMBERS READ-REPLACED PRESERVED DISCARD-CHANGES
   REMOVED-FIRST-KEEPS-ORDER
   COMMIT-FAILURE INSTALL-FAILURE READONLY-COMMIT CRC-FAILURE MANY-HANDLES ORIGINAL-SOURCE RAW-FRAMING ZIP64-COUNT TYPE-REJECTIONS
   FILE-MEMBERS FILE-STREAMS FILE-SNAPSHOT FILE-REPLACE-WITH LARGE-FILE-COPY
   FILE-CRC-CLOSE FILE-CRC-READ FILE-SIZE-ERRORS
   FILE-INPUT-RETIRE FILE-INSTALL-FAILURE
   BOUNDARY-OPEN-ERRORS BOUNDARY-READ-ERRORS BOUNDARY-DISAGREEMENT BOUNDARY-CURSORS
   BOUNDARY-ZIP64-CASES COMPRESSED-SIZES WIDE-EOCD-CASES
   FILE-REPLACE TASK-READERS
   CLEANUP-RUN T-REPORT ;

RUN
;using
;using
;package

require src/lib/zip-offset-test.f
