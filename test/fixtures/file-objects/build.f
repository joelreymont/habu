\ build.f - test/file-objects.f's child: a build of count.f, shape.f, main.f and
\ reopen.f written as the program `prog` under the out root, with again.f loaded
\ twice in the same build, then host.f loaded after the build closes; prints
\ `compiled ` and LINKER:COMPILED. The body lays one cell between shape.f and
\ main.f, which no file's data holds. With the script argument `second` the
\ build loads pad.f first. With `stop`, four builds each fail: in STOP-FILE and
\ STOP-BODY every later load and write is LINKER:E-FAILED, STOP-INSIDE's file
\ moves DP back and its own load is LINKER:E-FAILED, and STOP-ALIGN's file marks
\ a cell its object cannot place, E-COMP-STATE.
\ The build's files load from its body: in a build's file `require` is
\ E-UNDEFINED (rc 70), and creating through a `does>` definer ends the process
\ at rec-wide-publish (rc 70).

require src/lib/error.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/build-out.f
require src/habu/layout.f
require src/habu/xref.f
require src/habu/linker.f
require src/habu/link-data.f
require src/os/script-argv.f

package FILE-OBJECTS-BUILD

CAST: ENTRY-XT ( n -- [ -- ] )

PATH-CAP BUFFER: PROG-BUF

: PROG$ ( -- ptr u8 n )
   BUILD-OUT:BASE$ s" prog" PROG-BUF FS:JOIN-PATH {: u:n :}
   PROG-BUF u ;

70 constant REFUSED   \ the checker's refusal (src/core/check-hook.f CHECK-RC)

\ The entry store app-image-core.f START! makes, of the word named a u.
: STORE-ENTRY ( ptr u8 n -- )
   XREF-FIND-INDEX XREF-REC XREF-START ENTRY-XT
   data-base APP-ENTRY:XT-CELL + xt! ;

: PAD ( -- ) s" test/fixtures/file-objects/pad.f" included ;
: RESIDUE ( -- ) s" test/fixtures/file-objects/residue.f" included ;
: LOAD-AGAIN ( -- ) s" test/fixtures/file-objects/again.f" included ;

: ROLLBACK ( -- ) s" test/fixtures/file-objects/rollback.f" included ;
: UNALIGNED ( -- ) s" test/fixtures/file-objects/unaligned.f" included ;

\ The one argument, second or stop, or empty; any other ends the build, rc 1.
: MODE$ ( -- ptr u8 n )
   SCRIPT-ARGC 0 = if s" " exit then
   SCRIPT-ARGC 1 = if
      0 SCRIPT-ARGV$ s" second" STR= if s" second" exit then
      0 SCRIPT-ARGV$ s" stop" STR= if s" stop" exit then
   then
   s" build: unknown argument" 1 die ;

: SECOND? ( -- bool ) MODE$ s" second" STR= ;
: STOP? ( -- bool ) MODE$ s" stop" STR= ;

\ The case's code is the one wanted, or the build ends rc 1 with the message.
: EXPECT ( n n ptr u8 n -- )
   {: rc:n want:n msg:ptr msgu:n :}
   rc want <> if msg msgu 1 die then ;

: STOP-FILE ( -- )
   s" test/fixtures/file-objects/count.f" included
   ['] RESIDUE catch E-EVAL-RESIDUE s" build: STOP-FILE: residue.f not refused" EXPECT
   ['] RESIDUE catch LINKER:E-FAILED s" build: STOP-FILE: a load after a refused file ran" EXPECT
   [: PROG$ LINKER:WRITE ;] catch LINKER:E-FAILED
   s" build: STOP-FILE: a write after a refused file ran" EXPECT ;

: STOP-BODY ( -- )
   [: s" : FOB-BAD ( -- n ) dup ;" evaluate-closed ;] catch
   REFUSED s" build: STOP-BODY: the definition was not refused" EXPECT
   [: PROG$ LINKER:WRITE ;] catch LINKER:E-FAILED s" build: STOP-BODY: a write after a rollback ran" EXPECT
   ['] RESIDUE catch LINKER:E-FAILED s" build: STOP-BODY: a load after a rollback ran" EXPECT ;

: STOP-INSIDE ( -- )
   ['] ROLLBACK catch LINKER:E-FAILED
   s" build: STOP-INSIDE: a file that moved DP back was not refused" EXPECT ;

: STOP-ALIGN ( -- )
   ['] UNALIGNED catch E-COMP-STATE
   s" build: STOP-ALIGN: an unaligned address cell was not refused" EXPECT ;

: BODY ( -- )
   SECOND? {: second:bool :}
   second if PAD then
   s" test/fixtures/file-objects/count.f" included
   s" test/fixtures/file-objects/shape.f" included
   8 allot
   s" test/fixtures/file-objects/main.f" included
   s" test/fixtures/file-objects/reopen.f" included
   LOAD-AGAIN LOAD-AGAIN
   s" FILE-OBJECTS-MAIN:RUN" STORE-ENTRY
   PROG$ LINKER:WRITE ;

: STOPS ( -- )
   s" app" [: STOP-FILE ;] LINKER:WITH
   s" app" [: STOP-BODY ;] LINKER:WITH
   s" app" [: STOP-INSIDE ;] LINKER:WITH
   s" app" [: STOP-ALIGN ;] LINKER:WITH
   s" stopped" type cr ;

public

: RUN ( -- )
   STOP? if STOPS exit then
   s" app" [: BODY ;] LINKER:WITH
   s" test/fixtures/file-objects/host.f" included
   s" compiled " type LINKER:COMPILED . ;

;package

FILE-OBJECTS-BUILD:RUN
