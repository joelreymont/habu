\ arm64-place.f - LINKER:WRITE's placement of a build's code, checked on the
\ stripped programs one build writes after loading
\ test/compiler/arm64-place-fixture.f, each run in its own process. run prints
\ 11: PL-RUN's call, tail call and code address reach PL-LEAF, and its data
\ address reaches PL-V, which the entry set to 1. tick prints 8: PL-TICK reaches
\ PL-TOCK only through a code address. five prints 6: PL-FIVE's does> slot
\ reaches PL-MAKE's clause, and the link is not refused though PL-MAKE's own
\ body calls `create`. Each prints through the kernel primitive `.`, and run's
\ map holds the kernel's `.` and `execute`. Each program's map names the fixture
\ words its entry reaches and no other, and a second link of run writes the
\ same bytes. A link rooted at PL-MAKE itself
\ exits 74: test/compiler/arm64-place-unsafe-bad.f runs it in a child process.
\ Fixture words are PL- and defined once per process: the host checker keeps a
\ symbol for each name a build defines.

require src/lib/error.f
require src/lib/string.f
require src/lib/test.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process-command.f
require src/lib/engine-candidate.f
require src/habu/layout.f
require src/habu/xref.f
require src/compiler/target/model.f
require src/habu/linker.f
require src/habu/link-data.f
require tools/hb-build-core.f

package ARM64-PLACE

CAST: ENTRY-XT ( n -- [ -- ] )

create ROOT-BUF PATH-CAP allot variable ROOT-U
create PROG-BUF PATH-CAP allot
$20000 constant FILE-CAP
FILE-CAP BUFFER: FILE-A
FILE-CAP BUFFER: FILE-B

: ROOT$ ( -- ptr u8 n ) ROOT-BUF ROOT-U @ ;

: PROG$ ( ptr u8 n -- ptr u8 n )
   {: name:ptr name-n:n :}
   ROOT$ name name-n PROG-BUF FS:JOIN-PATH {: u:n :}
   PROG-BUF u ;


: FIXTURE ( -- )
   s" test/compiler/arm64-place-fixture.f" included
   s" : PL-GO-RUN ( -- ) 1 PL-V !  1 PL-RUN . ;" evaluate-closed
   s" : PL-GO-TICK ( -- ) 1 PL-TICK . ;" evaluate-closed
   s" : PL-GO-FIVE ( -- ) PL-FIVE . ;" evaluate-closed ;


: STORE-ENTRY ( ptr u8 n -- )
   XREF-FIND-INDEX XREF-REC XREF-START ENTRY-XT
   data-base APP-ENTRY:XT-CELL + xt! ;


: WRITE-PROG ( ptr u8 n ptr u8 n -- )
   {: word:ptr word-n:n file:ptr file-n:n :}
   word word-n STORE-ENTRY
   file file-n PROG$ LINKER:WRITE ;


\ run is linked again after the other programs.
: BODY ( -- )
   FIXTURE
   s" PL-GO-RUN" s" run" WRITE-PROG
   s" PL-GO-TICK" s" tick" WRITE-PROG
   s" PL-GO-FIVE" s" five" WRITE-PROG
   s" PL-GO-RUN" s" run-again" WRITE-PROG ;


: RUN-PROG ( CMD:command ptr u8 n ptr u8 n -- CMD:command )
   {: file:ptr file-n:n out:ptr out-n:n :}
   file file-n PROG$ PROCESS:WAIT-FOREVER CMD:RUN-OUTCOME PROCESS:ENDING>RC RC>N {: rc:n :}
   CMD:OUT$ out out-n STR=  rc 0=  and 0= if
      s" arm64-place: " type file file-n type s"  exited " type rc .
      s" stdout: " type CMD:OUT$ type cr  s" stderr: " type CMD:ERR$ type cr
   then
   file file-n T-LABEL  CMD:OUT$ out out-n T$=
   file file-n T-LABEL  rc 0 T= ;

: RUN-RUN ( CMD:command -- CMD:command ) s" run" s\" 11\n" RUN-PROG ;
: RUN-TICK ( CMD:command -- CMD:command ) s" tick" s\" 8\n" RUN-PROG ;
: RUN-FIVE ( CMD:command -- CMD:command ) s" five" s\" 6\n" RUN-PROG ;


\ Whether the map has a row for the word: a row's name stands between spaces.
: ROW? ( ptr u8 n ptr u8 n -- bool )
   {: map:ptr map-n:n name:ptr name-n:n :}
   map map-n PROG$ FILE-A FILE-CAP FS:READ-ALL {: u:n :}
   FILE-A u name name-n CONTAINS? ;


: RUN-MAP ( -- )
   s" run: the map holds PL-GO-RUN, PL-RUN and PL-LEAF" T-LABEL
   s" run.map" s"  PL-GO-RUN " ROW? TTRUE
   s" run.map" s"  PL-RUN " ROW? TTRUE
   s" run.map" s"  PL-LEAF " ROW? TTRUE
   s" run: the map holds the kernel primitives . and execute" T-LABEL
   s" run.map" s"  . word baked " ROW? TTRUE
   s" run.map" s"  execute word baked " ROW? TTRUE
   s" run: no PL-V, which a data address names, PL-MAKE or PL-FIVE" T-LABEL
   s" run.map" s"  PL-V " ROW? TFALSE
   s" run.map" s"  PL-MAKE " ROW? TFALSE
   s" run.map" s"  PL-FIVE " ROW? TFALSE ;


: TICK-MAP ( -- )
   s" tick: the map holds PL-TICK and PL-TOCK" T-LABEL
   s" tick.map" s"  PL-TICK " ROW? TTRUE
   s" tick.map" s"  PL-TOCK " ROW? TTRUE
   s" tick: no PL-RUN, PL-LEAF, PL-MAKE or PL-INIT" T-LABEL
   s" tick.map" s"  PL-RUN " ROW? TFALSE
   s" tick.map" s"  PL-LEAF " ROW? TFALSE
   s" tick.map" s"  PL-MAKE " ROW? TFALSE
   s" tick.map" s"  PL-INIT " ROW? TFALSE ;


\ PL-MAKE's emission holds its clause, and its body calls PL-INIT.
: FIVE-MAP ( -- )
   s" five: the map holds PL-FIVE, PL-MAKE and PL-INIT" T-LABEL
   s" five.map" s"  PL-FIVE " ROW? TTRUE
   s" five.map" s"  PL-MAKE " ROW? TTRUE
   s" five.map" s"  PL-INIT " ROW? TTRUE
   s" five: no PL-RUN, PL-LEAF, PL-V, PL-TICK or PL-TOCK" T-LABEL
   s" five.map" s"  PL-RUN " ROW? TFALSE
   s" five.map" s"  PL-LEAF " ROW? TFALSE
   s" five.map" s"  PL-V " ROW? TFALSE
   s" five.map" s"  PL-TICK " ROW? TFALSE
   s" five.map" s"  PL-TOCK " ROW? TFALSE ;


: SAME-BYTES ( -- )
   s" run" PROG$ FILE-A FILE-CAP FS:READ-ALL {: a:n :}
   s" run-again" PROG$ FILE-B FILE-CAP FS:READ-ALL {: b:n :}
   s" a second link of run writes the same bytes" T-LABEL
   FILE-A a FILE-B b STR= TTRUE ;


: UNSAFE-RUN ( CMD:command -- CMD:command )
   s" --load" CMD:ARG+  s" test/compiler/arm64-place-unsafe-bad.f" CMD:ARG+
   s" --" CMD:ARG+  HB-BUILD-CORE:KERNEL$ CMD:ARG+  s" make" PROG$ CMD:ARG+
   ENGINE-CANDIDATE:PATH$ PROCESS:WAIT-FOREVER CMD:RUN-OUTCOME PROCESS:ENDING>RC RC>N {: rc:n :}
   s" refusal: the child arms before the link" T-LABEL
   CMD:OUT$ s\" ARM64-PLACE-UNSAFE-ARMED\n" T$=
   s" refusal: the child names the caller and the word" T-LABEL
   CMD:ERR$ s" link: PL-MAKE calls create, which a stripped program cannot run" CONTAINS? TTRUE
   s" refusal: the child's reason" T-LABEL
   CMD:ERR$ s" link: a stripped program reaches a word it cannot run" CONTAINS? TTRUE
   s" refusal: the child exits 74 and writes no program" T-LABEL
   rc 74 T=  s" make" PROG$ FS:EXISTS? TFALSE ;


public

: RUN ( -- )
   T-RESET
   s" habu-place" FS:MKTEMP-DIR {: a:ptr u:n :}
   a ROOT-BUF u BYTE-COPY  u ROOT-U !
   false RTARGET:HOST-TARGET HB-BUILD-CORE:KERNEL-BUILD
   s" app" RTARGET:HOST-TARGET [: BODY ;] LINKER:WITH
   [: RUN-RUN ;] CMD:WITH-COMMAND
   [: RUN-TICK ;] CMD:WITH-COMMAND
   [: RUN-FIVE ;] CMD:WITH-COMMAND
   RUN-MAP  TICK-MAP  FIVE-MAP
   SAME-BYTES
   \ The refusal ends its process, so it runs in a child.
   [: UNSAFE-RUN ;] CMD:WITH-COMMAND
   ROOT$ FS:REMOVE-TREE
   T-REPORT ;

;package

ARM64-PLACE:RUN
