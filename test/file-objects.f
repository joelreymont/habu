\ file-objects.f - builds of the files in test/fixtures/file-objects/ by
\ build.f, each in a fresh hb: a fresh process, since an object must be a
\ function of the files, not of the process. Cold builds A, B and D, each with
\ HABU_OUT its own temp directory, compile count.f, shape.f, main.f, reopen.f,
\ mark.f and again.f twice, and load host.f after they close. A writes seven
\ objects. D, in `shifted` mode, declares a package and lays three cells first,
\ so its records, emissions, wordlist ids and data sit elsewhere, and writes
\ seven with A's bytes: an absolute index or address in an object shows as
\ different bytes. B loads pad.f first and writes eight, under A's names and
\ pad.f's. Build C, in `stop` mode, runs its refused builds,
\ its stale builds and three edit sequences in one out root: the stale build
\ writes a load list of 8 bytes, the magic alone, the build of count.f and
\ shape.f after it compiles 2, and so does one over a list whose path holds a 0
\ byte; each sequence compiles 2, 2 and 0, and the tail sequence's fourth
\ build, of d.f alone, compiles 1: d.f's object's prefix sums c.f. C writes
\ eight objects, and the two whose names A also wrote, count.f's and shape.f's,
\ have A's bytes: a refused file writes none. A's program prints Hi and ok and
\ exits 0, run in its own process: the boundary under test is the written file.
\ Then five warm builds in A's out root. A2 compiles none, prints A's state and
\ writes A's program byte for byte, A's objects unchanged. A3, after one byte of
\ the string green in an object changes, A4, after count.f's object is cut to
\ half its length, and A5, on macOS, after it grows to 2^48 bytes, more than a
\ build can hold, each compile all seven with A2's outcome. B2, in `second`
\ mode, loads pad.f, which A's builds never loaded, first, so it compiles all
\ eight, prints B's state, writes B's program and leaves A's root holding B's
\ objects. Each warm build compares its program's bytes, its printed state and
\ the objects' bytes with a cold build's.

require src/lib/error.f
require src/lib/string.f
require src/lib/test.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/fd.f
require src/lib/fs-list.f
require src/lib/process-command.f
require src/lib/engine-candidate.f
require src/core/sha256.f

package FILE-OBJECTS-TEST

7 constant FILES                         \ build A's objects, and D's
8 constant FILES-B                       \ build B's: A's and pad.f's
8 constant FILES-C                       \ build C's: count.f's, shape.f's, two per sequence
68 constant NAME-BYTES                   \ <hex key>.hfo
\ The names, newline-separated.
: LISTING-BYTES ( n -- n ) NAME-BYTES 1 + * 1 - ;
: NAME-COUNT ( n -- n ) 1 + NAME-BYTES 1 + / ;
40 constant INPUT-BYTES                  \ an object's INPUT's first 40 bytes: a size, then a SHA-256
8192 constant CAP                        \ a listing's or an object's bytes
16384 constant STATE-CAP                 \ a build's output before its last line
65536 constant PROG-CAP

PATH-CAP BUFFER: OUT-A   variable OUT-A-U
PATH-CAP BUFFER: OUT-B   variable OUT-B-U
PATH-CAP BUFFER: OUT-C   variable OUT-C-U
PATH-CAP BUFFER: OUT-D   variable OUT-D-U
PATH-CAP BUFFER: OBJ-DIR
PATH-CAP BUFFER: JOINED
PATH-CAP BUFFER: JOINED-2
CAP BUFFER: NAMES-A      variable NAMES-A-U
CAP BUFFER: NAMES-B      variable NAMES-B-U
CAP BUFFER: NAMES-C      variable NAMES-C-U
CAP BUFFER: NAMES-D      variable NAMES-D-U
CAP BUFFER: BYTES-A
CAP BUFFER: BYTES-B
CAP BUFFER: SOURCE-BYTES
SHA256-CTX-BYTES BUFFER: CTX
INPUT-BYTES BUFFER: EXPECT
STATE-CAP BUFFER: STATE-A  variable STATE-A-U
STATE-CAP BUFFER: STATE-B  variable STATE-B-U
STATE-CAP BUFFER: STATE-W  variable STATE-W-U  \ a warm build's, C's or D's
PROG-CAP BUFFER: PROG-1
PROG-CAP BUFFER: PROG-2
8 BUFFER: WHICH          variable WHICH-U  \ the build the labels name
96 BUFFER: TEXT-BUF
128 BUFFER: LABEL-BUF
variable SHARED-N                        \ C's objects whose names A also wrote
variable INPUT-N                         \ those holding count.f's INPUT


: A$ ( -- ptr u8 n ) OUT-A OUT-A-U @ ;
: B$ ( -- ptr u8 n ) OUT-B OUT-B-U @ ;
: C$ ( -- ptr u8 n ) OUT-C OUT-C-U @ ;
: D$ ( -- ptr u8 n ) OUT-D OUT-D-U @ ;

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

\ The text, another build's name and more text, as the next label.
: LABEL-NAMING ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: a:ptr au:n b:ptr bu:n c:ptr cu:n :}
   a TEXT-BUF au BYTE-COPY
   b TEXT-BUF au + bu BYTE-COPY
   c TEXT-BUF au + bu + cu BYTE-COPY
   TEXT-BUF au bu + cu + LABEL ;


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
: LIST-D ( -- ) D$ NAMES-D LIST NAMES-D-U ! ;

\ A build that made no obj directory lists no names.
: LIST-OR-NONE ( [ -- ] -- ) catch {: rc:n :}
   rc 0 =  rc E-FS-LIST-OPEN =  or 0= if rc throw then ;

: RELIST-A ( -- ) 0 NAMES-A-U !  ['] LIST-A LIST-OR-NONE ;

: NAME ( n -- ptr u8 n )
   NAME-BYTES 1 + * {: off:n :}  NAMES-A off + NAME-BYTES ;

: C-NAME ( n -- ptr u8 n )
   NAME-BYTES 1 + * {: off:n :}  NAMES-C off + NAME-BYTES ;

: A-OBJ$ ( n -- ptr u8 n ) NAME {: name:ptr nameu:n :}  A$ name nameu OBJ$ ;

\ A's object at the index read into BYTES-A; answers its length.
: A-OBJECT ( n -- n ) A-OBJ$ BYTES-A CAP FS:READ-ALL ;

\ The index of A's object holding the bytes, or -1.
: HOLDING ( ptr u8 n -- n )
   {: a:ptr u:n :}
   NAMES-A-U @ NAME-COUNT 0 ?do
      BYTES-A i A-OBJECT  a u CONTAINS? if i unloop exit then
   loop
   -1 ;

\ Whether the other build's listing names A's object at the index, as a test.
: LISTED? ( ptr u8 n ptr u8 n n -- bool )
   {: names:ptr namesu:n peer:ptr peeru:n i:n :}
   names namesu i NAME CONTAINS? {: has:bool :}
   s" " peer peeru s"  wrote each of A's names" LABEL-NAMING  has TTRUE
   has ;

\ A's object at the index has the bytes of the other build's of the same name.
: SAME-OBJECT ( ptr u8 n ptr u8 n ptr u8 n n -- )
   {: out:ptr outu:n names:ptr namesu:n peer:ptr peeru:n i:n :}
   names namesu peer peeru i LISTED? 0= if exit then
   i NAME {: name:ptr nameu:n :}
   A$ name nameu OBJ$ BYTES-A CAP FS:READ-ALL {: au:n :}
   out outu name nameu OBJ$ BYTES-B CAP FS:READ-ALL {: bu:n :}
   s" each of A's objects has " peer peeru s" 's bytes" LABEL-NAMING
   BYTES-A au BYTES-B bu T$= ;

\ Each of the objects A's listing names, when it names that many, against the
\ other build's out root, listing and name.
: SAME-OBJECTS ( ptr u8 n ptr u8 n ptr u8 n n -- )
   {: out:ptr outu:n names:ptr namesu:n peer:ptr peeru:n count:n :}
   NAMES-A-U @ count LISTING-BYTES <> if exit then
   count 0 ?do out outu names namesu peer peeru i SAME-OBJECT loop ;

: SAME-AS-D ( -- ) D$ NAMES-D NAMES-D-U @ s" D" FILES SAME-OBJECTS ;
: SAME-AS-B ( -- ) B$ NAMES-B NAMES-B-U @ s" B" FILES-B SAME-OBJECTS ;

\ B lists each of A's names; their bytes are not compared.
: B-NAMES ( -- )
   NAMES-A-U @ FILES LISTING-BYTES <> if exit then
   FILES 0 ?do NAMES-B NAMES-B-U @ s" B" i LISTED? drop loop ;

\ Whether the first out root's file holds the bytes of the second's.
: SAME-FILE? ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- bool )
   {: a:ptr au:n an:ptr anu:n b:ptr bu:n bn:ptr bnu:n :}
   a au an anu IN$ PROG-1 PROG-CAP FS:READ-ALL {: u1:n :}
   b bu bn bnu IN$ PROG-2 PROG-CAP FS:READ-ALL {: u2:n :}
   PROG-1 u1 PROG-2 u2 STR= ;

\ EXPECT: the first 40 bytes of the file's INPUT: its size, low byte first, and SHA-256.
: EXPECT! ( ptr u8 n -- ) {: path:ptr pathu:n :}
   path pathu SOURCE-BYTES CAP FS:READ-ALL {: u:n :}
   8 0 ?do u i 8 * rshift $FF and EXPECT i + c! loop
   CTX SOURCE-BYTES u EXPECT 8 + SHA256-IN ;

\ C's object at the index, when A also wrote its name, has A's bytes; counted in
\ SHARED-N, and in INPUT-N when it holds EXPECT.
: SHARED-OBJECT ( n -- )
   C-NAME {: name:ptr nameu:n :}
   NAMES-A NAMES-A-U @ name nameu CONTAINS? 0= if exit then
   1 SHARED-N +!
   C$ name nameu OBJ$ BYTES-B CAP FS:READ-ALL {: cu:n :}
   BYTES-B cu EXPECT INPUT-BYTES CONTAINS? if 1 INPUT-N +! then
   A$ name nameu OBJ$ BYTES-A CAP FS:READ-ALL {: au:n :}
   s" each object A also wrote has A's bytes" LABEL  BYTES-A au BYTES-B cu T$= ;

\ Build C's objects: the edit sequences' two each, count.f's and shape.f's,
\ whose names A also wrote; count.f's holds its INPUT.
: CHECK-STOP ( -- )
   s" C" WHICH!
   s" the build wrote eight objects" LABEL  NAMES-C-U @ FILES-C LISTING-BYTES T=
   NAMES-C-U @ NAME-COUNT {: n:n :}
   NAMES-C-U @ n LISTING-BYTES <> if exit then
   s" test/fixtures/file-objects/count.f" EXPECT!
   0 SHARED-N !  0 INPUT-N !
   n 0 ?do i SHARED-OBJECT loop
   s" A wrote two of C's objects" LABEL  SHARED-N @ 2 T=
   s" one of them holds count.f's size and SHA-256" LABEL  INPUT-N @ 1 T= ;

\ A stale list holds the magic alone. A corrupt list, an edit to either file of a sequence, or
\ a first file loaded without the files its object's prefix sums makes the build compile every file.
: CHECK-EDIT ( -- )
   s" C" WHICH!
   s" the stale build writes an empty list" LABEL
   STATE-W STATE-W-U @ s\" stale list: 8\n" CONTAINS? TTRUE
   s" the stale build compiles 2 files" LABEL
   STATE-W STATE-W-U @ s\" stale: compiled 2\n" CONTAINS? TTRUE
   s" a corrupt list compiles every file" LABEL
   STATE-W STATE-W-U @ s\" corrupt: compiled 2\n" CONTAINS? TTRUE
   s" the edit builds compile 2, 2 and 0 files" LABEL
   STATE-W STATE-W-U @ s\" edit: compiled 2 2 0\n" CONTAINS? TTRUE
   s" the tail builds compile 2, 2, 0 and 1 files" LABEL
   STATE-W STATE-W-U @ s\" tail: compiled 2 2 0 1\n" CONTAINS? TTRUE
   s" the share builds compile 2, 2 and 0 files" LABEL
   STATE-W STATE-W-U @ s\" share: compiled 2 2 0\n" CONTAINS? TTRUE ;

: COLD ( CMD:command -- CMD:command )
   s" A" WHICH!  A$ s" " s\" compiled 7\n" STATE-A BUILD 0 max STATE-A-U !
   A$ RUN-PROG
   s" B" WHICH!  B$ s" second" s\" compiled 8\n" STATE-B BUILD 0 max STATE-B-U !
   s" D" WHICH!  D$ s" shifted" s\" compiled 7\n" STATE-W BUILD drop
   s" C" WHICH!  C$ s" stop" s\" stopped\n" STATE-W BUILD 0 max STATE-W-U ! ;

: CHECK-COLD ( -- )
   RELIST-A
   0 NAMES-B-U !  ['] LIST-B LIST-OR-NONE
   0 NAMES-C-U !  ['] LIST-C LIST-OR-NONE
   0 NAMES-D-U !  ['] LIST-D LIST-OR-NONE
   s" A" WHICH!
   s" the build wrote seven objects" LABEL
   NAMES-A NAMES-A-U @ s" .hfo" COUNT-SUB FILES T=
   s" the names are seven keys" LABEL  NAMES-A-U @ FILES LISTING-BYTES T=
   s" B" WHICH!
   s" the build wrote eight objects" LABEL
   NAMES-B NAMES-B-U @ s" .hfo" COUNT-SUB FILES-B T=
   s" the names are eight keys" LABEL  NAMES-B-U @ FILES-B LISTING-BYTES T=
   s" A" WHICH!  B-NAMES  SAME-AS-D
   CHECK-STOP CHECK-EDIT ;

\ Moves A's program to prog-cold, so A2 writes its own.
: COLD-ASIDE ( -- )
   A$ s" prog-cold" JOINED-2 FS:JOIN-PATH {: u:n :}
   A$ s" prog" IN$ JOINED-2 u FS:RENAME-FILE ;

\ A build of A's files in A's out root, whose last line is the given one:
\ it prints A's state, writes A's program and leaves A's objects with D's bytes.
: WARM ( CMD:command ptr u8 n -- CMD:command )
   {: last:ptr lastu:n :}
   A$ s" " last lastu STATE-W BUILD {: u:n :}
   u 0 < if exit then
   s" the build prints A's state" LABEL  STATE-W u STATE-A STATE-A-U @ T$=
   s" the program has A's bytes" LABEL  A$ s" prog" A$ s" prog-cold" SAME-FILE? TTRUE
   RELIST-A
   s" A's root still holds seven objects" LABEL  NAMES-A-U @ FILES LISTING-BYTES T=
   SAME-AS-D ;

\ A2: every object linked.
: WARM-A ( CMD:command -- CMD:command )
   s" A2" WHICH!  COLD-ASIDE  s\" compiled 0\n" WARM ;

\ A3's edit: the first e of the string green in A's object that holds it
\ becomes f, past the INPUT, which the object's source still matches.
: FLIP ( -- )
   s" green" HOLDING {: k:n :}
   s" one of A's objects holds green" LABEL  k 0 >= TTRUE
   k 0 < if exit then
   k A-OBJECT {: u:n :}
   BYTES-A u s" green" FIND-SUB MATCH option
      none OF ENDOF
      some OF IDX>N {: at:n :}  [char] f BYTES-A at + 2 + c! ENDOF
   ;MATCH
   k A-OBJ$ BYTES-A u FS:WRITE-ALL ;

\ The listing index of count.f's object, found by its INPUT, or -1 after a failed check.
: COUNT-OBJ ( -- n )
   s" test/fixtures/file-objects/count.f" EXPECT!
   EXPECT INPUT-BYTES HOLDING {: k:n :}
   s" one of A's objects holds count.f's size and SHA-256" LABEL  k 0 >= TTRUE
   k ;

\ A4's edit: count.f's object cut to half its length.
: CUT ( -- )
   COUNT-OBJ {: k:n :}
   k 0 < if exit then
   k A-OBJECT {: u:n :}
   k A-OBJ$ BYTES-A u 2 / FS:WRITE-ALL ;

: WARM-FLIP ( CMD:command -- CMD:command )
   s" A3" WHICH!  FLIP  s\" compiled 7\n" WARM ;

\ A5's edit: count.f's object grown in place to 2^48 bytes, past what mmap maps on an arm64
\ host; the bytes added are a hole, so the disk holds none.
: GROW ( -- )
   COUNT-OBJ {: k:n :}
   k 0 < if exit then
   k A-OBJ$ FS:OPEN-APPEND-FD >FD {: f:fd :}
   f 1 48 lshift FD:RESIZE
   f FD>N close ;

: WARM-CUT ( CMD:command -- CMD:command )
   s" A4" WHICH!  CUT  s\" compiled 7\n" WARM ;

\ macOS only: Linux ext4 refuses a file past 16 TiB, so FD:RESIZE throws E-FS-IO there.
: WARM-GROW ( CMD:command -- CMD:command )
   HB-TARGET-MACOS? 0= if exit then
   s" A5" WHICH!  GROW  s\" compiled 7\n" WARM ;

: WARM-B ( CMD:command -- CMD:command )
   s" B2" WHICH!
   A$ s" second" s\" compiled 8\n" STATE-W BUILD {: u:n :}
   u 0 < if exit then
   s" the build prints B's state" LABEL  STATE-W u STATE-B STATE-B-U @ T$=
   s" the program has B's bytes" LABEL  A$ s" prog" B$ s" prog" SAME-FILE? TTRUE
   RELIST-A
   s" A's root lists B's objects" LABEL  NAMES-A NAMES-A-U @ NAMES-B NAMES-B-U @ T$=
   SAME-AS-B ;

: CHECKS ( -- )
   [: COLD ;] CMD:WITH-COMMAND
   CHECK-COLD
   [: WARM-A WARM-FLIP WARM-CUT WARM-GROW WARM-B ;] CMD:WITH-COMMAND ;


: REMOVE-A ( -- ) A$ FS:REMOVE-TREE ;
: REMOVE-B ( -- ) B$ FS:REMOVE-TREE ;
: REMOVE-C ( -- ) C$ FS:REMOVE-TREE ;
: REMOVE-D ( -- ) D$ FS:REMOVE-TREE ;

: WITH-D ( -- )
   s" habu-file-objects-d" FS:MKTEMP-DIR {: d:ptr u:n :}
   d OUT-D u BYTE-COPY  u OUT-D-U !
   ['] CHECKS ['] REMOVE-D finally ;

: WITH-C ( -- )
   s" habu-file-objects-c" FS:MKTEMP-DIR {: c:ptr u:n :}
   c OUT-C u BYTE-COPY  u OUT-C-U !
   ['] WITH-D ['] REMOVE-C finally ;

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
