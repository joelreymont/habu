\ build.f - test/file-objects.f's child: a build of count.f, shape.f and main.f
\ written as the program `prog` under the out root, with residue.f refused
\ (skipped when the script argument is `skip-residue`) and again.f loaded twice
\ in the same build, then host.f loaded after the build closes; prints
\ `compiled ` and LINKER:COMPILED.
\ The build's files load from its body: in a build's file `require` is
\ E-UNDEFINED (rc 70), and creating through a `does>` definer ends the process
\ at rec-wide-publish (rc 70).

require src/lib/errors.f
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

\ The entry store app-image-core.f START! makes, of the word named a u.
: STORE-ENTRY ( ptr u8 n -- )
   XREF-FIND-INDEX XREF-REC XREF-START ENTRY-XT
   data-base APP-ENTRY:XT-CELL + xt! ;

: RESIDUE ( -- ) s" test/fixtures/file-objects/residue.f" included ;
: LOAD-AGAIN ( -- ) s" test/fixtures/file-objects/again.f" included ;

\ True for the one argument skip-residue; any other ends the build, rc 1.
: SKIP-RESIDUE? ( -- bool )
   SCRIPT-ARGC 0 = if false exit then
   SCRIPT-ARGC 1 = if
      0 SCRIPT-ARGV$ s" skip-residue" STR= if true exit then
   then
   s" build: unknown argument" 1 die ;

: REFUSE-RESIDUE ( -- )
   ['] RESIDUE catch {: rc:n :}
   rc E-EVAL-RESIDUE <> if s" build: residue.f not refused" 1 die then ;

: BODY ( -- )
   s" test/fixtures/file-objects/count.f" included
   s" test/fixtures/file-objects/shape.f" included
   s" test/fixtures/file-objects/main.f" included
   SKIP-RESIDUE? 0= if REFUSE-RESIDUE then
   LOAD-AGAIN LOAD-AGAIN
   s" FILE-OBJECTS-MAIN:RUN" STORE-ENTRY
   PROG$ LINKER:WRITE ;

public

: RUN ( -- )
   s" app" [: BODY ;] LINKER:WITH
   s" test/fixtures/file-objects/host.f" included
   s" compiled " type LINKER:COMPILED . ;

;package

FILE-OBJECTS-BUILD:RUN
