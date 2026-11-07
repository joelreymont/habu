\ file-objects.f - five builds of the files in test/fixtures/file-objects/ by
\ build.f, each in a fresh hb: a fresh process, since an object must be a
\ function of the files, not of the process. Builds A and B, each with HABU_OUT
\ its own temp directory, compile count.f, shape.f, main.f, reopen.f, mark.f and
\ again.f twice, and load host.f after they close. A writes seven objects; B
\ loads pad.f first, so its later files' records, emissions and data sit
\ elsewhere, and writes eight. Each of A's objects is byte-identical to B's of
\ the same name: an address in an object shows as different bytes. Build C, in
\ build.f's `stop` mode, runs nine failed builds, which write one object,
\ count.f's, with A's bytes: a refused file writes none. Its three edit builds
\ write a.f's and edit-b.f's objects and compile 2, 2 and 0 files. A's program
\ prints Hi and ok and exits 0, run in its own process: the boundary under test
\ is the written file.
\ Then two warm builds in A's out root link its objects. A2 compiles none,
\ prints A's state and writes A's program byte for byte, A's objects unchanged;
\ B2, in `second` mode, compiles pad.f alone, prints B's state, writes B's
\ program and leaves A's root holding B's objects. Each warm build compares its
\ program's bytes, its printed state and the objects' bytes with a cold build's.

require src/lib/error.f
require src/lib/string.f
require src/lib/test.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/fs-list.f
require src/lib/process-command.f
require src/lib/engine-candidate.f
require src/core/sha256.f

package FILE-OBJECTS-TEST

7 constant FILES                         \ build A's objects
8 constant FILES-B                       \ build B's: A's and pad.f's
68 constant NAME-BYTES                   \ <hex key>.hfo
\ The names, newline-separated.
: LISTING-BYTES ( n -- n ) NAME-BYTES 1 + * 1 - ;
40 constant INPUT-BYTES                  \ an object's INPUT: a size, then a SHA-256
4096 constant CAP                        \ a listing's or an object's bytes
16384 constant STATE-CAP                 \ a build's output before its last line
65536 constant PROG-CAP

PATH-CAP BUFFER: OUT-A   variable OUT-A-U
PATH-CAP BUFFER: OUT-B   variable OUT-B-U
PATH-CAP BUFFER: OUT-C   variable OUT-C-U
PATH-CAP BUFFER: OBJ-DIR
PATH-CAP BUFFER: JOINED
PATH-CAP BUFFER: JOINED-2
CAP BUFFER: NAMES-A      variable NAMES-A-U
CAP BUFFER: NAMES-B      variable NAMES-B-U
CAP BUFFER: NAMES-C      variable NAMES-C-U
CAP BUFFER: BYTES-A
CAP BUFFER: BYTES-B
CAP BUFFER: SOURCE-BYTES
SHA256-CTX-BYTES BUFFER: CTX
INPUT-BYTES BUFFER: EXPECT
STATE-CAP BUFFER: STATE-A  variable STATE-A-U
STATE-CAP BUFFER: STATE-B  variable STATE-B-U
STATE-CAP BUFFER: STATE-W  variable STATE-W-U  \ a warm build's, or C's
PROG-CAP BUFFER: PROG-1
PROG-CAP BUFFER: PROG-2
8 BUFFER: WHICH          variable WHICH-U  \ the build the labels name
128 BUFFER: LABEL-BUF


: A$ ( -- ptr u8 n ) OUT-A OUT-A-U @ ;
: B$ ( -- ptr u8 n ) OUT-B OUT-B-U @ ;
: C$ ( -- ptr u8 n ) OUT-C OUT-C-U @ ;

: IN$ ( ptr u8 n ptr u8 n -- ptr u8 n ) {: out:ptr outu:n name:ptr nameu:n :}
   out outu name nameu JOINED FS:JOIN-PATH {: u:n :}
   JOINED u ;

: OBJ$ ( ptr u8 n ptr u8 n -- ptr u8 n ) {: out:ptr outu:n name:ptr nameu:n :}
   out outu s" obj" OBJ-DIR FS:JOIN-PATH {: du:n :}
   OBJ-DIR du name nameu IN$ ;

: WHICH! ( ptr u8 n -- ) {: a:ptr u:n :}
   a WHICH u BYTE-COPY  u WHICH-U ! ;

\ The build's name, ": " and the text, as the next label.
: LABEL ( ptr u8 n -- )
   {: a:ptr u:n :}
   WHICH WHICH-U @ {: w:ptr wu:n :}
   w LABEL-BUF wu BYTE-COPY
   s" : " {: c:ptr cu:n :}
   c LABEL-BUF wu + cu BYTE-COPY
   a LABEL-BUF wu + cu + u BYTE-COPY
   LABEL-BUF wu cu + u + T-LABEL ;


\ A failed run shows its status and what it printed on both streams.
: SHOW ( CMD:command n -- CMD:command ) {: rc:n :}
   s" file-objects: exited " type rc .
   s" stdout: " type CMD:OUT$ type cr  s" stderr: " type CMD:ERR$ type cr ;

\ The length of the text before its last line.
: BEFORE-LAST ( ptr u8 n -- n ) {: a:ptr u:n :}
   u 2 < if 0 exit then
   0 u 2 - ?do
      a i + c@ $0A = if i 1 + unloop exit then
   -1 +loop
   0 ;

\ build.f with the mode, if any, as its argument; dst gets the output before the
\ given last line. Answers its length, -1 if it did not exit 0 or overflowed dst.
: BUILD ( CMD:command ptr u8 n ptr u8 n ptr u8 n ptr u8 -- CMD:command n )
   {: out:ptr outu:n mode:ptr modeu:n last:ptr lastu:n dst:ptr :}
   CMD:RESET
   s" --load" CMD:ARG+  s" test/fixtures/file-objects/build.f" CMD:ARG+
   modeu 0<> if s" --" CMD:ARG+  mode modeu CMD:ARG+ then
   s" HABU_OUT" out outu CMD:ENV+
   ENGINE-CANDIDATE:PATH$ 120000 >MS CMD:RUN-OUTCOME
   PROCESS:ENDING>RC RC>N {: rc:n :}
   CMD:OUT$ last lastu ENDS-WITH? {: done:bool :}
   done  rc 0 =  and 0= if rc SHOW then
   s" the build's last line is the one expected" LABEL  done TTRUE
   s" the build exits 0" LABEL  rc 0 T=
   rc 0<> if -1 exit then
   CMD:OUT$ {: o:ptr ou:n :}
   o ou BEFORE-LAST {: u:n :}
   s" the build's output fits its buffer" LABEL  u STATE-CAP <= TTRUE
   u STATE-CAP > if -1 exit then
   o dst u BYTE-COPY  u ;

: RUN-PROG ( CMD:command ptr u8 n -- CMD:command ) {: out:ptr outu:n :}
   CMD:RESET
   out outu s" prog" IN$ 60000 >MS CMD:RUN-OUTCOME PROCESS:ENDING>RC RC>N {: rc:n :}
   CMD:OUT$ s\" Hi\nok\n" STR=  rc 0 =  and 0= if rc SHOW then
   s" the program prints Hi and ok" LABEL  CMD:OUT$ s\" Hi\nok\n" T$=
   s" the program exits 0" LABEL  rc 0 T= ;


: LIST ( ptr u8 n ptr u8 -- n ) {: out:ptr outu:n dst:ptr :}
   out outu s" obj" OBJ-DIR FS:JOIN-PATH {: du:n :}
   OBJ-DIR du dst CAP FS-LIST:NAMES ;

: LIST-A ( -- ) A$ NAMES-A LIST NAMES-A-U ! ;
: LIST-B ( -- ) B$ NAMES-B LIST NAMES-B-U ! ;
: LIST-C ( -- ) C$ NAMES-C LIST NAMES-C-U ! ;

\ A build that made no obj directory lists no names.
: LIST-OR-NONE ( [ -- ] -- ) catch {: rc:n :}
   rc 0 =  rc E-FS-LIST-OPEN =  or 0= if rc throw then ;

: RELIST-A ( -- ) 0 NAMES-A-U !  ['] LIST-A LIST-OR-NONE ;

: NAME ( n -- ptr u8 n )
   NAME-BYTES 1 + * {: off:n :}  NAMES-A off + NAME-BYTES ;

: C-NAME ( n -- ptr u8 n )
   NAME-BYTES 1 + * {: off:n :}  NAMES-C off + NAME-BYTES ;

\ A's object at the index has the bytes of B's of the same name.
: SAME-OBJECT ( n -- )
   NAME {: name:ptr nameu:n :}
   s" B wrote each of A's objects" LABEL
   NAMES-B NAMES-B-U @ name nameu CONTAINS? TTRUE
   NAMES-B NAMES-B-U @ name nameu CONTAINS? 0= if exit then
   A$ name nameu OBJ$ BYTES-A CAP FS:READ-ALL {: au:n :}
   B$ name nameu OBJ$ BYTES-B CAP FS:READ-ALL {: bu:n :}
   s" each of A's objects has B's bytes" LABEL  BYTES-A au BYTES-B bu T$= ;

\ Each of the objects A's listing names, when it names that many.
: SAME-OBJECTS ( n -- ) {: count:n :}
   NAMES-A-U @ count LISTING-BYTES <> if exit then
   count 0 ?do i SAME-OBJECT loop ;

\ Whether the first out root's file holds the bytes of the second's.
: SAME-FILE? ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- bool )
   {: a:ptr au:n an:ptr anu:n b:ptr bu:n bn:ptr bnu:n :}
   a au an anu IN$ PROG-1 PROG-CAP FS:READ-ALL {: u1:n :}
   b bu bn bnu IN$ PROG-2 PROG-CAP FS:READ-ALL {: u2:n :}
   PROG-1 u1 PROG-2 u2 STR= ;

\ EXPECT: the INPUT section of the file: its size, low byte first, and SHA-256.
: EXPECT! ( ptr u8 n -- ) {: path:ptr pathu:n :}
   path pathu SOURCE-BYTES CAP FS:READ-ALL {: u:n :}
   8 0 ?do u i 8 * rshift $FF and EXPECT i + c! loop
   CTX SOURCE-BYTES u EXPECT 8 + SHA256-IN ;

\ The index of the one name of C's three that A's listing holds, or -1.
: STOP-NAME ( -- n )
   3 0 ?do NAMES-A NAMES-A-U @ i C-NAME CONTAINS? if i unloop exit then loop
   -1 ;

\ Build C's three objects: the edit builds' two, and the one whose name A also
\ wrote, which holds count.f's INPUT and A's bytes.
: CHECK-STOP ( -- )
   s" C" WHICH!
   s" the build wrote three objects" LABEL  NAMES-C-U @ 3 LISTING-BYTES T=
   NAMES-C-U @ 3 LISTING-BYTES <> if exit then
   STOP-NAME {: k:n :}
   s" A wrote one of C's objects" LABEL  k 0 >= TTRUE
   k 0 < if exit then
   k C-NAME {: name:ptr nameu:n :}
   C$ name nameu OBJ$ BYTES-B CAP FS:READ-ALL {: cu:n :}
   s" test/fixtures/file-objects/count.f" EXPECT!
   s" the object holds count.f's size and SHA-256" LABEL
   BYTES-B cu EXPECT INPUT-BYTES CONTAINS? TTRUE
   A$ name nameu OBJ$ BYTES-A CAP FS:READ-ALL {: au:n :}
   s" the object has A's bytes" LABEL  BYTES-A au BYTES-B cu T$= ;

\ C's edit builds: the second compiles a.f, whose INPUT changed, and edit-b.f,
\ whose FILES row names a.f's object; the third links both.
: CHECK-EDIT ( -- )
   s" C" WHICH!
   s" the edit builds compile 2, 2 and 0 files" LABEL
   STATE-W STATE-W-U @ s\" edit: compiled 2 2 0\n" CONTAINS? TTRUE ;

: COLD ( CMD:command -- CMD:command )
   s" A" WHICH!  A$ s" " s\" compiled 7\n" STATE-A BUILD 0 max STATE-A-U !
   A$ RUN-PROG
   s" B" WHICH!  B$ s" second" s\" compiled 8\n" STATE-B BUILD 0 max STATE-B-U !
   s" C" WHICH!  C$ s" stop" s\" stopped\n" STATE-W BUILD 0 max STATE-W-U ! ;

: CHECK-COLD ( -- )
   RELIST-A
   0 NAMES-B-U !  ['] LIST-B LIST-OR-NONE
   0 NAMES-C-U !  ['] LIST-C LIST-OR-NONE
   s" A" WHICH!
   s" the build wrote seven objects" LABEL
   NAMES-A NAMES-A-U @ s" .hfo" COUNT-SUB FILES T=
   s" the names are seven keys" LABEL  NAMES-A-U @ FILES LISTING-BYTES T=
   s" B" WHICH!
   s" the build wrote eight objects" LABEL
   NAMES-B NAMES-B-U @ s" .hfo" COUNT-SUB FILES-B T=
   s" the names are eight keys" LABEL  NAMES-B-U @ FILES-B LISTING-BYTES T=
   s" A" WHICH!  FILES SAME-OBJECTS
   CHECK-STOP CHECK-EDIT ;

\ Moves A's program to prog-cold, so A2 writes its own.
: COLD-ASIDE ( -- )
   A$ s" prog-cold" JOINED-2 FS:JOIN-PATH {: u:n :}
   A$ s" prog" IN$ JOINED-2 u FS:RENAME-FILE ;

\ A2: A's files again in A's out root, every object linked.
: WARM-A ( CMD:command -- CMD:command )
   s" A2" WHICH!
   COLD-ASIDE
   A$ s" " s\" compiled 0\n" STATE-W BUILD {: u:n :}
   u 0 < if exit then
   s" the build prints A's state" LABEL  STATE-W u STATE-A STATE-A-U @ T$=
   s" the program has A's bytes" LABEL  A$ s" prog" A$ s" prog-cold" SAME-FILE? TTRUE
   RELIST-A
   s" A's root still holds seven objects" LABEL  NAMES-A-U @ FILES LISTING-BYTES T=
   FILES SAME-OBJECTS ;

\ B2: B's files in A's out root, pad.f compiled and every other object linked.
: WARM-B ( CMD:command -- CMD:command )
   s" B2" WHICH!
   A$ s" second" s\" compiled 1\n" STATE-W BUILD {: u:n :}
   u 0 < if exit then
   s" the build prints B's state" LABEL  STATE-W u STATE-B STATE-B-U @ T$=
   s" the program has B's bytes" LABEL  A$ s" prog" B$ s" prog" SAME-FILE? TTRUE
   RELIST-A
   s" A's root lists B's objects" LABEL  NAMES-A NAMES-A-U @ NAMES-B NAMES-B-U @ T$=
   FILES-B SAME-OBJECTS ;

: CHECKS ( -- )
   [: COLD ;] CMD:WITH-COMMAND
   CHECK-COLD
   [: WARM-A WARM-B ;] CMD:WITH-COMMAND ;


: REMOVE-A ( -- ) A$ FS:REMOVE-TREE ;
: REMOVE-B ( -- ) B$ FS:REMOVE-TREE ;
: REMOVE-C ( -- ) C$ FS:REMOVE-TREE ;

: WITH-C ( -- )
   s" habu-file-objects-c" FS:MKTEMP-DIR {: c:ptr u:n :}
   c OUT-C u BYTE-COPY  u OUT-C-U !
   ['] CHECKS ['] REMOVE-C finally ;

: WITH-B ( -- )
   s" habu-file-objects-b" FS:MKTEMP-DIR {: b:ptr u:n :}
   b OUT-B u BYTE-COPY  u OUT-B-U !
   ['] WITH-C ['] REMOVE-B finally ;

public

: RUN ( -- )
   T-RESET
   s" habu-file-objects-a" FS:MKTEMP-DIR {: a:ptr u:n :}
   a OUT-A u BYTE-COPY  u OUT-A-U !
   ['] WITH-B ['] REMOVE-A finally
   T-REPORT ;

;package

FILE-OBJECTS-TEST:RUN
