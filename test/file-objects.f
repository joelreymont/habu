\ file-objects.f - a build of the files in test/fixtures/file-objects/,
\ written as a program and run: it prints `Hi` and `ok` and exits 0.
\ LINKER:COMPILED counts the three files the build compiled and not host.f,
\ which the host loads after the build closes. The program runs in its own
\ process: the boundary under test is the written file.
\ The build's files load from its body: in a build's file `require` is
\ E-UNDEFINED (rc 70), and creating through a `does>` definer ends the process
\ at rec-wide-publish (rc 70).

require src/lib/errors.f
require src/lib/string.f
require src/lib/test.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process-command.f
require src/habu/layout.f
require src/habu/xref.f
require src/habu/linker.f
require src/habu/link-data.f

package FILE-OBJECTS-TEST

CAST: ENTRY-XT ( n -- [ -- ] )

create ROOT-BUF PATH-CAP allot variable ROOT-U
create PROG-BUF PATH-CAP allot


: PROG$ ( -- ptr u8 n )
   ROOT-BUF ROOT-U @ s" prog" PROG-BUF FS:JOIN-PATH {: u:n :}
   PROG-BUF u ;


\ The entry store app-image-core.f START! makes, of the word named a u.
: STORE-ENTRY ( ptr u8 n -- )
   XREF-FIND-INDEX XREF-REC XREF-START ENTRY-XT
   data-base APP-ENTRY:XT-CELL + xt! ;


: BODY ( -- )
   s" test/fixtures/file-objects/count.f" included
   s" test/fixtures/file-objects/shape.f" included
   s" test/fixtures/file-objects/main.f" included
   s" FILE-OBJECTS-MAIN:RUN" STORE-ENTRY
   PROG$ LINKER:WRITE ;


\ The program prints Hi and ok and exits 0; a failed check shows what it
\ printed on both streams.
: RUN-PROG ( CMD:command -- CMD:command )
   PROG$ 60000 >MS CMD:RUN-OUTCOME PROC-OUTCOME>RC RC>N {: rc:n :}
   CMD:OUT$ s\" Hi\nok\n" STR=  rc 0 =  and 0= if
      s" file-objects: exited " type rc .
      s" stdout: " type CMD:OUT$ type cr  s" stderr: " type CMD:ERR$ type cr
   then
   s" the program prints Hi and ok" T-LABEL  CMD:OUT$ s\" Hi\nok\n" T$=
   s" the program exits 0" T-LABEL  rc 0 T= ;


public

: RUN ( -- )
   T-RESET
   s" habu-file-objects" FS:MKTEMP-DIR {: a:ptr u:n :}
   a ROOT-BUF u BYTE-COPY  u ROOT-U !
   s" app" [: BODY ;] LINKER:WITH
   s" test/fixtures/file-objects/host.f" included
   s" the build compiled its three files, the host none" T-LABEL
   LINKER:COMPILED 3 T=
   [: RUN-PROG ;] CMD:WITH-COMMAND
   ROOT-BUF ROOT-U @ FS:REMOVE-TREE
   T-REPORT ;

;package

FILE-OBJECTS-TEST:RUN
