\ stderr-test.f - STDERR:CAPTURE holds one capture at a time, and
\ STDERR:CAPTURING reads a capture whole past a throw.
\ Run: bin/hb --load src/lib/test/stderr-test.f

require src/lib/error.f
require src/lib/arena.f
require src/lib/string.f
require src/lib/test.f
require src/lib/fd.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/test/stderr.f

package STDERR-TEST

64 constant FILE-CAP

create PATH-BUF PATH-CAP allot      \ the capture file's path
variable PATH-U
create FILE-BUF FILE-CAP allot      \ the capture file's bytes, read back
variable CODE-A
variable CODE-B

: PATH$ ( -- ^u8 n )  PATH-BUF PATH-U @ ;


: PATH-MAKE ( -- )
   s" stderr-test" FS:MKTEMP-DIR {: dir:^ dirlen:n :}
   dir dirlen FS:CLEANUP-TREE+
   dir dirlen s" err.txt" PATH-BUF FS:JOIN-PATH PATH-U ! ;


: FILE$ ( -- ^u8 n )  FILE-BUF  PATH$ FILE-BUF FILE-CAP FS:READ-ALL ;


: ERR-OUT ( ^u8 n -- )
   {: at:^ u:n :}
   FD:STDERR at u FD:WRITE-FULL ;


: CAPTURED ( [ -- ] -- )
   PATH$ STDERR:CAPTURE
   [: STDERR:;CAPTURE ;] finally ;


: NESTED ( -- )
   S\" before\n" ERR-OUT
   [: PATH$ STDERR:CAPTURE ;] catch CODE-A !
   S\" after\n" ERR-OUT ;


: REOPEN ( -- )  [: ;] CAPTURED ;


\ The refused capture neither truncates the open one's file nor moves fd 2.
: NESTED-CASE ( -- )
   s" a capture inside a capture is refused" T-LABEL
   [: NESTED ;] CAPTURED
   FILE$ S\" before\nafter\n" T$=
   CODE-A @ STDERR:E-STATE T=
   [: STDERR:;CAPTURE ;] catch CODE-B !
   CODE-B @ STDERR:E-STATE T=
   [: REOPEN ;] catch 0 T= ;


\ 40 bytes: two pass FILE-CAP.
: LINE$ ( -- ^u8 n )
   S\" a regression logs this line twice .....\n" ;


: LOUD ( -- )  LINE$ ERR-OUT  LINE$ ERR-OUT  77 throw ;


: HALVES ( ^u8 n -- )
   {: text:^ u:n :}
   LINE$ nip {: half:n :}
   u half 2 * T=
   text half LINE$ T$=
   text half + half LINE$ T$= ;


: WHOLE-CASE ( -- )
   s" a capture is read whole past a throw" T-LABEL
   ARENA:NEW {: a:^ :}
   [: LOUD ;] PATH$ a STDERR:CAPTURING HALVES
   77 T=
   PATH$ FS:FILE? TFALSE
   a ARENA:RELEASE ;


: STDERR-RUN ( -- )
   T-RESET
   PATH-MAKE
   NESTED-CASE
   WHOLE-CASE
   T-REPORT ;

STDERR-RUN

;package
