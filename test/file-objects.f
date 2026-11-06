\ file-objects.f - two builds of the files in test/fixtures/file-objects/, each
\ by build.f in a fresh hb with HABU_OUT its own temp directory: a fresh
\ process, since objects must be a function of the files, not of the process.
\ Each build compiles count.f, shape.f, main.f and again.f twice and loads
\ host.f after it closes, and writes five objects; build A refuses residue.f
\ and build B skips it, so a refused load that changed another file's object
\ shows as different names or bytes. The first build's program prints
\ `Hi` and `ok` and exits 0, run in its own process: the boundary under test is
\ the written file.

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

5 constant FILES
4 constant FIXTURES                      \ the files with objects
64 constant HEX-BYTES
HEX-BYTES 4 + constant NAME-BYTES        \ <hex key>.hfo
\ The names, newline-separated.
FILES NAME-BYTES 1 + * 1 - constant LISTING-BYTES
56 constant EMPTY-AT                     \ FILES REQUESTS NAMES PIECES: 0 0 each
64 constant EMPTY-BYTES
120 constant INPUT-AT                    \ the header's end
40 constant INPUT-BYTES                  \ a size, then a SHA-256
INPUT-AT INPUT-BYTES + constant OBJECT-BYTES
4096 constant CAP

PATH-CAP BUFFER: OUT-A   variable OUT-A-U
PATH-CAP BUFFER: OUT-B   variable OUT-B-U
PATH-CAP BUFFER: OBJ-DIR
PATH-CAP BUFFER: JOINED
CAP BUFFER: NAMES-A      variable NAMES-A-U
CAP BUFFER: NAMES-B      variable NAMES-B-U
CAP BUFFER: BYTES-A
CAP BUFFER: BYTES-B
CAP BUFFER: SOURCE-BYTES
SHA256-CTX-BYTES BUFFER: CTX
HEX-BYTES BUFFER: HEX
INPUT-BYTES BUFFER: EXPECT
EMPTY-BYTES BUFFER: ZEROS
\ A byte per fixture file: the objects whose INPUT matched it.
variable MATCHED


: A$ ( -- ptr u8 n ) OUT-A OUT-A-U @ ;
: B$ ( -- ptr u8 n ) OUT-B OUT-B-U @ ;

: OBJ$ ( ptr u8 n ptr u8 n -- ptr u8 n ) {: out:ptr outu:n name:ptr nameu:n :}
   out outu s" obj" OBJ-DIR FS:JOIN-PATH {: du:n :}
   OBJ-DIR du name nameu JOINED FS:JOIN-PATH {: u:n :}
   JOINED u ;


\ A failed run shows its status and what it printed on both streams.
: SHOW ( CMD:command n -- CMD:command ) {: rc:n :}
   s" file-objects: exited " type rc .
   s" stdout: " type CMD:OUT$ type cr  s" stderr: " type CMD:ERR$ type cr ;

: COMPILED? ( CMD:command -- CMD:command bool )
   CMD:OUT$ s\" compiled 5\n" ENDS-WITH? ;

: BUILD ( CMD:command ptr u8 n bool -- CMD:command )
   {: out:ptr outu:n skip:bool :}
   CMD:RESET
   s" --load" CMD:ARG+  s" test/fixtures/file-objects/build.f" CMD:ARG+
   skip if s" --" CMD:ARG+  s" skip-residue" CMD:ARG+ then
   s" HABU_OUT" out outu CMD:ENV+
   ENGINE-CANDIDATE:PATH$ 120000 >MS CMD:RUN-OUTCOME
   PROC-OUTCOME>RC RC>N {: rc:n :}
   COMPILED?  rc 0 =  and 0= if rc SHOW then
   s" the build compiled its five loads, residue.f and host.f none" T-LABEL
   COMPILED? TTRUE
   s" the build exits 0" T-LABEL  rc 0 T= ;

: RUN-PROG ( CMD:command ptr u8 n -- CMD:command ) {: out:ptr outu:n :}
   CMD:RESET
   out outu s" prog" JOINED FS:JOIN-PATH {: u:n :}
   JOINED u 60000 >MS CMD:RUN-OUTCOME PROC-OUTCOME>RC RC>N {: rc:n :}
   CMD:OUT$ s\" Hi\nok\n" STR=  rc 0 =  and 0= if rc SHOW then
   s" the program prints Hi and ok" T-LABEL  CMD:OUT$ s\" Hi\nok\n" T$=
   s" the program exits 0" T-LABEL  rc 0 T= ;


: LIST ( ptr u8 n ptr u8 -- n ) {: out:ptr outu:n dst:ptr :}
   out outu s" obj" OBJ-DIR FS:JOIN-PATH {: du:n :}
   OBJ-DIR du dst CAP FS-LIST:NAMES ;

: LIST-A ( -- ) A$ NAMES-A LIST NAMES-A-U ! ;
: LIST-B ( -- ) B$ NAMES-B LIST NAMES-B-U ! ;

\ A build that made no obj directory lists no names.
: LIST-OR-NONE ( [ -- ] -- ) catch {: rc:n :}
   rc 0 =  rc E-FS-LIST-OPEN =  or 0= if rc throw then ;

: NAME ( n -- ptr u8 n )
   NAME-BYTES 1 + * {: off:n :}  NAMES-A off + NAME-BYTES ;

: U64@ ( ptr u8 -- n ) {: a:ptr :}
   0  8 0 ?do 8 lshift  a 7 i - + c@ or loop ;

: FIXTURE$ ( n -- ptr u8 n ) {: j:n :}
   j 0 = if s" test/fixtures/file-objects/count.f" exit then
   j 1 = if s" test/fixtures/file-objects/shape.f" exit then
   j 2 = if s" test/fixtures/file-objects/main.f" exit then
   s" test/fixtures/file-objects/again.f" ;

\ EXPECT: the INPUT section of the file: its size, low byte first, and SHA-256.
: EXPECT! ( ptr u8 n -- ) {: path:ptr pathu:n :}
   path pathu SOURCE-BYTES CAP FS:READ-ALL {: u:n :}
   8 0 ?do u i 8 * rshift $FF and EXPECT i + c! loop
   CTX SOURCE-BYTES u EXPECT 8 + SHA256-IN ;

: MATCH-INPUT ( ptr u8 -- ) {: input:ptr :}
   FIXTURES 0 ?do
      i FIXTURE$ EXPECT!
      input INPUT-BYTES EXPECT INPUT-BYTES STR=
         if 1 i 8 * lshift MATCHED +! then
   loop ;

: CHECK-OBJECT ( n -- ) NAME {: name:ptr nameu:n :}
   A$ name nameu OBJ$ BYTES-A CAP FS:READ-ALL {: au:n :}
   B$ name nameu OBJ$ BYTES-B CAP FS:READ-ALL {: bu:n :}
   s" both builds wrote the object's bytes" T-LABEL  BYTES-A au BYTES-B bu T$=
   s" the object is a header and INPUT" T-LABEL  au OBJECT-BYTES T=
   s" the object starts with its magic" T-LABEL  BYTES-A 8 s" HABUFOBJ" T$=
   BYTES-A 8 + HEX SHA256>HEX
   s" the object holds the key its name spells" T-LABEL
   HEX HEX-BYTES name HEX-BYTES T$=
   s" INPUT starts where the header ends" T-LABEL  BYTES-A 40 + U64@ INPUT-AT T=
   s" INPUT is a size and a SHA-256" T-LABEL  BYTES-A 48 + U64@ INPUT-BYTES T=
   s" the four other sections are empty" T-LABEL
   BYTES-A EMPTY-AT + EMPTY-BYTES ZEROS EMPTY-BYTES T$=
   BYTES-A INPUT-AT + MATCH-INPUT ;

: CHECKS ( -- )
   [: A$ false BUILD  A$ RUN-PROG  B$ true BUILD ;] CMD:WITH-COMMAND
   0 NAMES-A-U !  ['] LIST-A LIST-OR-NONE
   0 NAMES-B-U !  ['] LIST-B LIST-OR-NONE
   s" each build wrote five objects" T-LABEL
   NAMES-A NAMES-A-U @ s" .hfo" COUNT-SUB FILES T=
   s" the names are five keys" T-LABEL  NAMES-A-U @ LISTING-BYTES T=
   s" both builds wrote the same names" T-LABEL
   NAMES-A NAMES-A-U @ NAMES-B NAMES-B-U @ T$=
   NAMES-A-U @ LISTING-BYTES =
   NAMES-A NAMES-A-U @ NAMES-B NAMES-B-U @ STR=  and if
      0 MATCHED !
      FILES 0 ?do i CHECK-OBJECT loop
      s" the INPUT sections are the files' sizes and SHA-256s" T-LABEL
      MATCHED @ $02010101 T=
   then ;


: REMOVE-A ( -- ) A$ FS:REMOVE-TREE ;
: REMOVE-B ( -- ) B$ FS:REMOVE-TREE ;

: WITH-B ( -- )
   s" habu-file-objects-b" FS:MKTEMP-DIR {: b:ptr u:n :}
   b OUT-B u BYTE-COPY  u OUT-B-U !
   ['] CHECKS ['] REMOVE-B finally ;

public

: RUN ( -- )
   T-RESET
   s" habu-file-objects-a" FS:MKTEMP-DIR {: a:ptr u:n :}
   a OUT-A u BYTE-COPY  u OUT-A-U !
   ['] WITH-B ['] REMOVE-A finally
   T-REPORT ;

;package

FILE-OBJECTS-TEST:RUN
