\ file-objects.f - builds of the files in test/fixtures/file-objects/ by
\ build.f, each in a fresh hb: a fresh process, since an object must be a
\ function of the files, not of the process. Cold builds A, B and D, each with
\ HABU_OUT its own temp directory, compile count.f, shape.f, main.f, reopen.f,
\ mark.f and again.f twice, and load host.f after they close. A writes seven
\ objects, each found by the end of its path under <out>/obj. D, in `shifted`
\ mode, declares a package and lays three cells first, so its records,
\ emissions, wordlist ids and data sit elsewhere, and writes seven whose bytes
\ from PIECES on are A's: an absolute index or address in an object shows as
\ different bytes. B loads pad.f first and writes eight, A's seven and pad.f's.
\ Build C, in `stop` mode, runs its refused builds, its mixed builds,
\ its edit sequences, the twins, the order sequence and the crafted sequence
\ in one out root: a
\ build of count.f alone links it, one of count.f and shape.f then compiles
\ shape.f alone, after count.f links, and the next compiles none; the edit,
\ share and laid sequences, whose first file is edited, compile 2, 2 and 0: the
\ second file reads the first's variable, spells the package the first comes to
\ declare, or lays the string the first comes to lay. The tail sequence, whose
\ second file is edited, compiles 2, 1 and 0; its fourth build, of d.f alone,
\ compiles 1: d.f's object names c.f, which defines V, a name d.f spells. The
\ private sequence, whose first file's private word changes, compiles 2, 1 and
\ 0: the second spells that word's name for its own package's word. The parts
\ sequence, whose second file's private constant changes, compiles 3, 2 and 0:
\ the third reopens their package and reads the constant, which leaves no
\ reference. The scope sequence, whose first file leaves its package open for
\ the second, which is edited, compiles 3, 3 and 2: the first two files never
\ link and compile each time; the third, which names the second's word,
\ compiles when that word's file is edited and links after. i.f, built inside
\ a package one file leaves open and another closes, then alone, inside again
\ and alone, compiles 3, 1, 3 and 1: a load inside a package never links, and
\ its object replaces the one the load alone wrote. Two files named a.f in two
\ directories, each built alone in turn twice, compile 1, 1, 0 and 0: each
\ keeps an object of its own. Four files, two storing into one cell and the fourth reading it
\ at top level, built in order, twice with the storers swapped and in order
\ again, compile 4, 3, 0 and 3: of two stores into one cell the later holds, so
\ the reader compiles again when the storers swap. The crafted sequence's
\ builds of e.f over seven objects the link refuses or links wrong compile 1,
\ and a plain build after them links. C writes twenty-eight objects, and the
\ two of A's files, count.f's and shape.f's, have A's bytes from PIECES on: a
\ refused file writes none. A's
\ printed state gives FILE-OBJECTS-COUNT:HI's record the facts text `( -- ) `
\ and FILE-OBJECTS-BODY-SUM's, which the build's body defines, `( n n -- n ) `,
\ and main.f's literal with the bytes of HI's text lies at that text. Objects
\ carry texts by value: D's and C's shared objects equal A's from PIECES on;
\ their heads hold the pieces' times, which differ between out roots. A's
\ program prints Hi and ok and exits 0, run in its own process: the boundary
\ under test is the written file. A's map lists RUN as main.f's. Then
\ four warm builds in A's out root. A2 compiles none, prints A's state and
\ writes A's program and map byte for byte, A's objects unchanged and its
\ records' texts A's. A4, after count.f's object is cut to half its length,
\ and A5, on macOS, after it grows to 2^48 bytes, more than a build can hold,
\ each compile five files with A2's outcome: count.f, whose piece gets a new
\ time, and shape.f, main.f, reopen.f and mark.f, whose objects name that
\ piece by its time; again.f's two loads link. B2, in `second` mode, loads
\ pad.f, which A's builds never loaded, first, so it compiles four: pad.f,
\ main.f and mark.f, which spell its SCRATCH and FIRST, and reopen.f, which
\ runs words, and every run word reaches pad.f's stores into header data. It
\ links the other four, prints B's state, writes B's program and leaves A's
\ root holding B's objects. Each warm build compares its program's bytes, its
\ printed state and the objects' bytes from PIECES on with a cold build's.
\ Build E, in `facts` mode in its own out root, loads count.f and
\ facts.f, compiles callers of their words, runs one and writes the program,
\ then compiles two definitions the checker refuses for misusing them; it
\ compiles 2. E2, in E's root after E's program moves aside, compiles none and
\ prints E's lines, E's refusals on stderr and writes E's program byte for byte:
\ its body's code is checked against the linked words as against compiled ones.
\ Builds A to E link the tree's stripped kernel object, which this process
\ writes once (HB-BUILD-CORE:KERNEL-BUILD) and passes as build.f's first
\ argument, so their out roots hold only the files' objects.
\ Build L builds log-main.f, whose first file takes LOG's facility and leaves HELD held: the program
\ holds no mutex the build locked (macOS), and its MAIN takes HELD, logs its line and releases HELD.
\ M has no require, so app-end.f's object becomes M's, and its program prints the enum its MAIN
\ fetched through a pointer, a fetch tier 0 checks; L2 builds log-main.f again, compiling
\ app-end.f beside log-main.f's reused objects.
\ P builds path-main.f, whose top level caches the build's engine path and wakes the main task's
\ park; after M2, P2 links it beside a recompiled app-end.f, and its program prints its own path
\ and wakes its own park. A compile replaces a file's object (linker.f WRITE-OBJECT) and a link
\ leaves it, so log-first.f's object keeps its size and change time from L to L2, and path-main.f's
\ from P to P2.

require src/lib/error.f
require src/lib/string.f
require src/lib/test.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/fd.f
require src/lib/process-command.f
require src/lib/engine-candidate.f
require src/compiler/target/model.f
require tools/hb-build-core.f

package FILE-OBJECTS-TEST

7 constant FILES                         \ build A's objects, and D's
8 constant FILES-B                       \ build B's: A's and pad.f's
8 constant LOG-FIRST                     \ TAIL$'s numbers for log-first.f's and path-main.f's objects
9 constant PATH-MAIN
28 constant FILES-C                      \ build C's: count.f's, shape.f's, each sequence's, those that
                                         \ never link included, the twins', the order sequence's four, e.f's
144 constant HEADER-BYTES                \ an object's header, INPUT right after it (linker.f)
128 constant PIECES-FIELD                \ the header's PIECES offset (linker.f WRITE-OBJECT)
24 constant INPUT-BYTES                  \ an object's INPUT: its file's stamp, 0 0 -1 if it never links
8192 constant CAP                        \ an object's bytes
16384 constant STATE-CAP                 \ a build's output before its last line
262144 constant PROG-CAP                 \ a program's bytes, L's included

PATH-CAP BUFFER: OUT-A   variable OUT-A-U
PATH-CAP BUFFER: OUT-B   variable OUT-B-U
PATH-CAP BUFFER: OUT-C   variable OUT-C-U
PATH-CAP BUFFER: OUT-D   variable OUT-D-U
PATH-CAP BUFFER: OUT-E   variable OUT-E-U
PATH-CAP BUFFER: OUT-L   variable OUT-L-U
PATH-CAP BUFFER: OUT-F   variable OUT-F-U    \ the fixture builds' out root, in L's directory
PATH-CAP BUFFER: JOINED
PATH-CAP BUFFER: JOINED-2
PATH-CAP BUFFER: ROOT    variable ROOT-U     \ the obj directory a walk starts at
PATH-CAP BUFFER: FOUND   variable FOUND-U    \ the walk's last path that ends with its tail
PATH-CAP BUFFER: KEPT    variable KEPT-U     \ the object KEEP found, 0 long if none
CAP BUFFER: BYTES-A
CAP BUFFER: BYTES-B
INPUT-BYTES BUFFER: EXPECT
STATE-CAP BUFFER: STATE-A  variable STATE-A-U
STATE-CAP BUFFER: STATE-B  variable STATE-B-U
STATE-CAP BUFFER: STATE-W  variable STATE-W-U  \ a warm build's, C's or D's
STATE-CAP BUFFER: STATE-E  variable STATE-E-U
STATE-CAP BUFFER: ERR-E    variable ERR-E-U    \ E's stderr
PROG-CAP BUFFER: PROG-1
PROG-CAP BUFFER: PROG-2
8 BUFFER: WHICH          variable WHICH-U  \ the build the labels name
96 BUFFER: TEXT-BUF
128 BUFFER: LABEL-BUF
variable OBJ-N                          \ the walk's objects
variable TAIL-I                          \ the tail the walk looks for (TAIL$)
variable TAIL-N                          \ and the paths that end with it
variable KEPT-SIZE                       \ KEPT$'s size and its change time's seconds and
variable KEPT-S                          \ nanoseconds, as KEEP read them
variable KEPT-NS


: A$ ( -- ptr u8 n ) OUT-A OUT-A-U @ ;
: B$ ( -- ptr u8 n ) OUT-B OUT-B-U @ ;
: C$ ( -- ptr u8 n ) OUT-C OUT-C-U @ ;
: D$ ( -- ptr u8 n ) OUT-D OUT-D-U @ ;
: E$ ( -- ptr u8 n ) OUT-E OUT-E-U @ ;
: L$ ( -- ptr u8 n ) OUT-L OUT-L-U @ ;
: F$ ( -- ptr u8 n ) OUT-F OUT-F-U @ ;

: IN$ ( ptr u8 n ptr u8 n -- ptr u8 n ) {: out:ptr outu:n name:ptr nameu:n :}
   out outu name nameu JOINED FS:JOIN-PATH {: u:n :}
   JOINED u ;

: WHICH! ( ptr u8 n -- ) {: a:ptr u:n :}
   a WHICH u BYTE-COPY  u WHICH-U ! ;

\ The build's name, ": " and the text, as the next label.
: LABEL-AS ( ptr u8 n ptr u8 n -- )
   {: w:ptr wu:n a:ptr u:n :}
   w LABEL-BUF wu BYTE-COPY
   s" : " {: c:ptr cu:n :}
   c LABEL-BUF wu + cu BYTE-COPY
   a LABEL-BUF wu + cu + u BYTE-COPY
   LABEL-BUF wu cu + u + T-LABEL ;

: LABEL ( ptr u8 n -- ) {: a:ptr u:n :}  WHICH WHICH-U @ a u LABEL-AS ;

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

\ build.f with the kernel object and the mode, if any, as its arguments; dst
\ gets the output before the given last line. Answers its length, -1 if it did
\ not exit 0 or overflowed dst.
: BUILD ( CMD:command ptr u8 n ptr u8 n ptr u8 n ptr u8 -- CMD:command n )
   {: out:ptr outu:n mode:ptr modeu:n last:ptr lastu:n dst:ptr :}
   CMD:RESET
   s" --load" CMD:ARG+  s" test/fixtures/file-objects/build.f" CMD:ARG+
   s" --" CMD:ARG+  HB-BUILD-CORE:KERNEL$ CMD:ARG+
   modeu 0<> if mode modeu CMD:ARG+ then
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


\ The tail of object i's path: A's from 0 (count.f's, shape.f's, main.f's, reopen.f's, mark.f's,
\ again.f's first load's and its second's), pad.f's at 7, then log-first.f's and path-main.f's.
: TAIL$ ( n -- ptr u8 n )
   {: i:n :}
   i 0 = if s" /test/fixtures/file-objects/count.f.hfo" exit then
   i 1 = if s" /test/fixtures/file-objects/shape.f.hfo" exit then
   i 2 = if s" /test/fixtures/file-objects/main.f.hfo" exit then
   i 3 = if s" /test/fixtures/file-objects/reopen.f.hfo" exit then
   i 4 = if s" /test/fixtures/file-objects/mark.f.hfo" exit then
   i 5 = if s" /test/fixtures/file-objects/again.f.hfo" exit then
   i 6 = if s" /test/fixtures/file-objects/again.f.2.hfo" exit then
   i 7 = if s" /test/fixtures/file-objects/pad.f.hfo" exit then
   i LOG-FIRST = if s" /test/fixtures/file-objects/log-first.f.hfo" exit then
   s" /test/fixtures/file-objects/path-main.f.hfo" ;

: ROOT! ( ptr u8 n -- ) {: out:ptr outu:n :}
   out outu s" obj" ROOT FS:JOIN-PATH ROOT-U ! ;

: OBJ-SEEN ( ptr u8 n -- ) s" .hfo" ENDS-WITH? if 1 OBJ-N +! then ;

: TAIL-SEEN ( ptr u8 n -- )
   {: p:ptr pu:n :}
   p pu TAIL-I @ TAIL$ ENDS-WITH? 0= if exit then
   1 TAIL-N +!  p FOUND pu BYTE-COPY  pu FOUND-U ! ;

: COUNT-WALK ( -- ) ROOT ROOT-U @ ['] OBJ-SEEN FS:WALK-FILES ;
: TAIL-WALK ( -- ) ROOT ROOT-U @ ['] TAIL-SEEN FS:WALK-FILES ;

\ A walk of a build that made no obj directory visits nothing: the missing root is E-FS-STAT.
: WALKED ( [ -- ] -- ) catch {: rc:n :}
   rc 0 =  rc E-FS-STAT =  or 0= if rc throw then ;

\ The objects under the out root's obj directory.
: OBJECTS ( ptr u8 n -- n )
   ROOT!  0 OBJ-N !  ['] COUNT-WALK WALKED  OBJ-N @ ;

\ Whether exactly one path under the out root's obj directory ends with tail i, as a test under
\ the build's name; FOUND$ is that path.
: FOUND? ( ptr u8 n ptr u8 n n -- bool )
   {: out:ptr outu:n name:ptr nameu:n i:n :}
   out outu ROOT!  i TAIL-I !  0 TAIL-N !  0 FOUND-U !
   ['] TAIL-WALK WALKED
   TAIL-N @ 1 = {: one:bool :}
   name nameu s" the build wrote that object" LABEL-AS  one TTRUE
   one ;

: FOUND$ ( -- ptr u8 n ) FOUND FOUND-U @ ;
: KEPT$ ( -- ptr u8 n ) KEPT KEPT-U @ ;

\ Whether BYTES-A's first au bytes and BYTES-B's first bu have one length, one PIECES offset and
\ the same bytes from it on.
: FROM-PIECES? ( n n -- bool )
   {: au:n bu:n :}
   au bu <> if false exit then
   au HEADER-BYTES < if false exit then
   BYTES-A PIECES-FIELD + LE:U64@ {: at:n :}
   BYTES-B PIECES-FIELD + LE:U64@ at <> if false exit then
   at au > if false exit then
   BYTES-A at +  au at -  BYTES-B at +  au at -  STR= ;

\ A's object of tail i equals the peer's from PIECES on. The head before PIECES is not compared:
\ it holds the pieces' times, which differ between out roots.
: SAME-OBJECT ( ptr u8 n ptr u8 n n -- )
   {: out:ptr outu:n peer:ptr peeru:n i:n :}
   A$ s" A" i FOUND? 0= if exit then
   FOUND$ BYTES-A CAP FS:READ-ALL {: au:n :}
   out outu peer peeru i FOUND? 0= if exit then
   FOUND$ BYTES-B CAP FS:READ-ALL {: bu:n :}
   s" each of A's objects has " peer peeru s" 's bytes from PIECES on" LABEL-NAMING
   au bu FROM-PIECES? TTRUE ;

: SAME-AS-D ( -- ) FILES 0 ?do D$ s" D" i SAME-OBJECT loop ;
: SAME-AS-B ( -- ) FILES-B 0 ?do B$ s" B" i SAME-OBJECT loop ;

\ Whether the first out root's file holds the bytes of the second's.
: SAME-FILE? ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- bool )
   {: a:ptr au:n an:ptr anu:n b:ptr bu:n bn:ptr bnu:n :}
   a au an anu IN$ PROG-1 PROG-CAP FS:READ-ALL {: u1:n :}
   b bu bn bnu IN$ PROG-2 PROG-CAP FS:READ-ALL {: u2:n :}
   PROG-1 u1 PROG-2 u2 STR= ;

\ EXPECT: the file's INPUT, its change time seconds, nanoseconds and size, each low byte first.
: EXPECT! ( ptr u8 n -- )
   FS:FILE-META {: size:n ms:n mns:n cs:n cns:n :}
   cs EXPECT LE:U64!  cns EXPECT 8 + LE:U64!  size EXPECT 16 + LE:U64! ;

\ Build C's objects: each sequence's, the twins', e.f's, and count.f's and shape.f's, which A also
\ writes; count.f's holds its INPUT.
: CHECK-STOP ( -- )
   s" C" WHICH!
   s" the build wrote twenty-eight objects" LABEL  C$ OBJECTS FILES-C T=
   C$ s" C" 0 SAME-OBJECT  C$ s" C" 1 SAME-OBJECT
   C$ s" C" 0 FOUND? 0= if exit then
   FOUND$ BYTES-B CAP FS:READ-ALL {: u:n :}
   s" test/fixtures/file-objects/count.f" EXPECT!
   u HEADER-BYTES - 0 max INPUT-BYTES min {: v:n :}
   s" count.f's object holds its stamp" LABEL
   BYTES-B HEADER-BYTES + v EXPECT INPUT-BYTES T$= ;

\ A file compiles when it, a file its object references or an earlier file that defines a public
\ name it spells, or a private name of a package it opens, is edited, when an earlier file lays a
\ string its object lays, when it is loaded without the files its object names, when files that
\ stored into a cell it read load in another order, or when the link refuses its object or would
\ link it wrong; any other file links, a compile after it included.
: CHECK-EDIT ( -- )
   s" C" WHICH!
   s" the mixed builds compile 0, 1 and 0 files" LABEL
   STATE-W STATE-W-U @ s\" mixed: compiled 0 1 0\n" CONTAINS? TTRUE
   s" the edit builds compile 2, 2 and 0 files" LABEL
   STATE-W STATE-W-U @ s\" edit: compiled 2 2 0\n" CONTAINS? TTRUE
   s" the tail builds compile 2, 1, 0 and 1 files" LABEL
   STATE-W STATE-W-U @ s\" tail: compiled 2 1 0 1\n" CONTAINS? TTRUE
   s" the share builds compile 2, 2 and 0 files" LABEL
   STATE-W STATE-W-U @ s\" share: compiled 2 2 0\n" CONTAINS? TTRUE
   s" the laid builds compile 2, 2 and 0 files" LABEL
   STATE-W STATE-W-U @ s\" laid: compiled 2 2 0\n" CONTAINS? TTRUE
   s" the private builds compile 2, 1 and 0 files" LABEL
   STATE-W STATE-W-U @ s\" private: compiled 2 1 0\n" CONTAINS? TTRUE
   s" the parts builds compile 3, 2 and 0 files" LABEL
   STATE-W STATE-W-U @ s\" parts: compiled 3 2 0\n" CONTAINS? TTRUE
   s" the scope builds compile 3, 3 and 2 files" LABEL
   STATE-W STATE-W-U @ s\" scope: compiled 3 3 2\n" CONTAINS? TTRUE
   s" the inside builds compile 3, 1, 3 and 1 files" LABEL
   STATE-W STATE-W-U @ s\" inside: compiled 3 1 3 1\n" CONTAINS? TTRUE
   s" the twin builds compile 1, 1, 0 and 0 files" LABEL
   STATE-W STATE-W-U @ s\" twins: compiled 1 1 0 0\n" CONTAINS? TTRUE
   s" the order builds compile 4, 3, 0 and 3 files" LABEL
   STATE-W STATE-W-U @ s\" order: compiled 4 3 0 3\n" CONTAINS? TTRUE
   s" the crafted builds compile 1, 1, 1, 1, 1, 1, 1, 1 and 0 files" LABEL
   STATE-W STATE-W-U @ s\" crafted: compiled 1 1 1 1 1 1 1 1 0\n" CONTAINS? TTRUE ;

\ Whether A's printed state holds the line, a record's name line with its newlines, followed
\ by a fields line that ends in the text, the record's facts text.
: FACTS-OF? ( ptr u8 n ptr u8 n -- bool )
   {: name:ptr nameu:n text:ptr textu:n :}
   STATE-A STATE-A-U @ {: s:ptr su:n :}
   s su name nameu FIND-SUB MATCH option
      none OF -1 ENDOF
      some OF IDX>N nameu + ENDOF
   ;MATCH {: at:n :}
   at 0< if false exit then
   s at +  su at - {: rest:ptr restu:n :}
   rest restu s\" \n" FIND-SUB MATCH option
      none OF restu ENDOF
      some OF IDX>N ENDOF
   ;MATCH {: lineu:n :}
   rest lineu text textu ENDS-WITH? ;

\ A's map lists RUN as a word main.f loaded; a map lists only the code its
\ program holds.
: CHECK-MAP ( -- )
   s" A" WHICH!
   A$ s" prog.map" IN$ PROG-1 PROG-CAP FS:READ-ALL {: u:n :}
   s" the map lists RUN as main.f's" LABEL
   PROG-1 u s\"  RUN word loaded test/fixtures/file-objects/main.f\n" CONTAINS? TTRUE ;

\ A file's texts and the body's are placed, and a later file's literal finds an earlier file's text.
: CHECK-FACTS ( -- )
   s" A" WHICH!
   s" HI's record holds its facts text" LABEL
   s\" \nHI\n" s" ( -- ) " FACTS-OF? TTRUE
   s" the body's word's record holds its facts text" LABEL
   s\" \nFILE-OBJECTS-BODY-SUM\n" s" ( n n -- n ) " FACTS-OF? TTRUE
   s" main.f's literal lies at HI's text" LABEL
   STATE-A STATE-A-U @ s\" shared -1 \n" CONTAINS? TTRUE ;

\ E's build, a fresh process as every build is: E2's process then holds E's
\ objects alone, never the checker memory of the process that compiled them.
: COLD-E ( CMD:command -- CMD:command )
   s" E" WHICH!  E$ s" facts" s\" facts: compiled 2\n" STATE-E BUILD 0 max STATE-E-U !
   CMD:ERR$ {: e:ptr eu:n :}
   s" the build's stderr fits its buffer" LABEL  eu STATE-CAP <= TTRUE
   eu STATE-CAP min {: u:n :}
   e ERR-E u BYTE-COPY  u ERR-E-U ! ;

\ Whether E's output before its last line holds the line, newline-ended, as a test.
: E-LINE ( ptr u8 n -- )
   {: a:ptr u:n :}
   s" the build prints " a u 1 - s" " LABEL-NAMING
   STATE-E STATE-E-U @ a u CONTAINS? TTRUE ;

\ The callers compile and run; the misuse and the dead code are refused, by TAKE and FAIL.
: CHECK-E ( -- )
   s" E" WHICH!
   s\" after: 0\n" E-LINE  s\" wrap: 0\n" E-LINE  s\" Hi\n" E-LINE  s\" after ran\n" E-LINE
   s\" misuse: 70\n" E-LINE  s\" dead: 70\n" E-LINE
   s" the misuse refusal names TAKE" LABEL
   ERR-E ERR-E-U @ s" fob-misuse at 'FILE-OBJECTS-FACTS:TAKE'" CONTAINS? TTRUE
   s" the dead-code refusal names FAIL" LABEL
   ERR-E ERR-E-U @ s" at '2' after 'FILE-OBJECTS-FACTS:FAIL'" CONTAINS? TTRUE ;

: COLD ( CMD:command -- CMD:command )
   s" A" WHICH!  A$ s" " s\" compiled 7\n" STATE-A BUILD 0 max STATE-A-U !
   A$ RUN-PROG
   s" B" WHICH!  B$ s" second" s\" compiled 8\n" STATE-B BUILD 0 max STATE-B-U !
   s" D" WHICH!  D$ s" shifted" s\" compiled 7\n" STATE-W BUILD drop
   s" C" WHICH!  C$ s" stop" s\" stopped\n" STATE-W BUILD 0 max STATE-W-U !
   COLD-E ;

: CHECK-COLD ( -- )
   s" A" WHICH!
   s" the build wrote seven objects" LABEL  A$ OBJECTS FILES T=
   s" B" WHICH!
   s" the build wrote eight objects" LABEL  B$ OBJECTS FILES-B T=
   s" D" WHICH!
   s" the build wrote seven objects" LABEL  D$ OBJECTS FILES T=
   s" A" WHICH!  SAME-AS-D
   CHECK-MAP CHECK-FACTS CHECK-STOP CHECK-EDIT CHECK-E ;

\ Moves A's program and map to prog-cold and prog-cold.map, so A2 writes its own.
: COLD-ASIDE ( -- )
   A$ s" prog-cold" JOINED-2 FS:JOIN-PATH {: u:n :}
   A$ s" prog" IN$ JOINED-2 u FS:RENAME-FILE
   A$ s" prog-cold.map" JOINED-2 FS:JOIN-PATH {: v:n :}
   A$ s" prog.map" IN$ JOINED-2 v FS:RENAME-FILE ;

\ Moves E's program to prog-cold, so E2 writes its own.
: COLD-ASIDE-E ( -- )
   E$ s" prog-cold" JOINED-2 FS:JOIN-PATH {: u:n :}
   E$ s" prog" IN$ JOINED-2 u FS:RENAME-FILE ;

\ E2, a build in E's out root after E's. A failed E2 fails BUILD's checks; its
\ output and stderr are still compared, and it wrote no program to compare.
: WARM-E ( CMD:command -- CMD:command )
   STATE-E-U @ 0= if exit then
   s" E2" WHICH!  COLD-ASIDE-E
   E$ s" facts" s\" facts: compiled 0\n" STATE-W BUILD {: u:n :}
   CMD:OUT$ {: o:ptr ou:n :}
   u 0< if ou else u then {: shown:n :}
   s" the build prints E's lines" LABEL  o shown STATE-E STATE-E-U @ T$=
   s" the build's stderr is E's" LABEL  CMD:ERR$ ERR-E ERR-E-U @ T$=
   u 0< if exit then
   s" the program has E's bytes" LABEL  E$ s" prog" E$ s" prog-cold" SAME-FILE? TTRUE ;

\ A build of A's files in A's out root, whose last line is the given one:
\ it prints A's state, writes A's program and map and leaves A's seven objects
\ with D's bytes from PIECES on.
: WARM ( CMD:command ptr u8 n -- CMD:command )
   {: last:ptr lastu:n :}
   A$ s" " last lastu STATE-W BUILD {: u:n :}
   u 0 < if exit then
   s" the build prints A's state" LABEL  STATE-W u STATE-A STATE-A-U @ T$=
   s" the program has A's bytes" LABEL  A$ s" prog" A$ s" prog-cold" SAME-FILE? TTRUE
   s" the map has A's bytes" LABEL  A$ s" prog.map" A$ s" prog-cold.map" SAME-FILE? TTRUE
   s" A's root still holds seven objects" LABEL  A$ OBJECTS FILES T=
   SAME-AS-D ;

\ A2: every object linked.
: WARM-A ( CMD:command -- CMD:command )
   s" A2" WHICH!  COLD-ASIDE  s\" compiled 0\n" WARM ;

\ A4's edit: count.f's object cut to half its length.
: CUT ( -- )
   A$ s" A" 0 FOUND? 0= if exit then
   FOUND$ BYTES-A CAP FS:READ-ALL {: u:n :}
   FOUND$ BYTES-A u 2 / FS:WRITE-ALL ;

\ A5's edit: count.f's object grown in place to 2^48 bytes, past what mmap maps on an arm64
\ host; the bytes added are a hole, so the disk holds none.
: GROW ( -- )
   A$ s" A" 0 FOUND? 0= if exit then
   FOUND$ FS:OPEN-APPEND-FD >FD {: f:fd :}
   f 1 48 lshift FD:RESIZE
   f FD>N close ;

: WARM-CUT ( CMD:command -- CMD:command )
   s" A4" WHICH!  CUT  s\" compiled 5\n" WARM ;

\ macOS only: Linux ext4 refuses a file past 16 TiB, so FD:RESIZE throws E-FS-IO there.
: WARM-GROW ( CMD:command -- CMD:command )
   HB-TARGET-MACOS? 0= if exit then
   s" A5" WHICH!  GROW  s\" compiled 5\n" WARM ;

: WARM-B ( CMD:command -- CMD:command )
   s" B2" WHICH!
   A$ s" second" s\" compiled 4\n" STATE-W BUILD {: u:n :}
   u 0 < if exit then
   s" the build prints B's state" LABEL  STATE-W u STATE-B STATE-B-U @ T$=
   s" the program has B's bytes" LABEL  A$ s" prog" B$ s" prog" SAME-FILE? TTRUE
   s" A's root holds eight objects" LABEL  A$ OBJECTS FILES-B T=
   SAME-AS-B ;

\ Builds a fixture to L's prog by tools/hb-build.f, since log.f requires and creates through does>
\ (build.f's files may not), in the fixture out root every fixture build shares; true on rc 0.
: FIXTURE-BUILD ( CMD:command ptr u8 n -- CMD:command bool )
   {: src:ptr src-u:n :}
   CMD:RESET
   s" --load" CMD:ARG+  s" tools/hb-build.f" CMD:ARG+
   s" --" CMD:ARG+  src src-u CMD:ARG+
   s" -o" CMD:ARG+  L$ s" prog" IN$ CMD:ARG+
   s" HABU_OUT" F$ CMD:ENV+
   ENGINE-CANDIDATE:PATH$ 120000 >MS CMD:RUN-OUTCOME
   PROCESS:ENDING>RC RC>N {: rc:n :}
   rc 0<> if rc SHOW then
   s" the build exits 0" LABEL  rc 0 T=
   rc 0 = ;

\ Darwin's first lock of a mutex sets its signature, $4D55545A (the bytes ZTUM), and unlocking leaves it.
: LOG-UNLOCKED ( -- )
   HB-TARGET-MACOS? 0= if exit then
   L$ s" prog" IN$ PROG-1 PROG-CAP FS:READ-ALL {: u:n :}
   s" the program holds no mutex the build locked" LABEL  PROG-1 u s" ZTUM" CONTAINS? TFALSE ;

\ L's program, run in its own process: the boundary under test is the written file.
: LOG-RUN ( CMD:command -- CMD:command )
   CMD:RESET
   L$ s" prog" IN$ 60000 >MS CMD:RUN-OUTCOME PROCESS:ENDING>RC RC>N {: rc:n :}
   CMD:ERR$ s\"  warn main: ran\n" ENDS-WITH? {: ran:bool :}
   ran  rc 0 =  and 0= if rc SHOW then
   s" the program logs MAIN's line" LABEL  ran TTRUE
   s" the program exits 0" LABEL  rc 0 T= ;

\ KEPT$ gets the path of the build's object of tail i; KEEP holds its size and change time.
: KEEP ( n -- ) {: i:n :}
   0 KEPT-U !
   F$ WHICH WHICH-U @ i FOUND? 0= if exit then
   FOUND$ {: p:ptr pu:n :}
   p KEPT pu BYTE-COPY  pu KEPT-U !
   KEPT$ FS:FILE-META {: size:n ms:n mns:n cs:n cns:n :}
   size KEPT-SIZE !  cs KEPT-S !  cns KEPT-NS ! ;

\ The label holds when the object KEEP found keeps its size and change time.
: LINKED ( ptr u8 n -- ) {: a:ptr u:n :}
   a u LABEL
   KEPT-U @ 0= if false TTRUE exit then
   KEPT$ FS:FILE? 0= if false TTRUE exit then
   KEPT$ FS:FILE-META {: size:n ms:n mns:n cs:n cns:n :}
   size KEPT-SIZE @ =  cs KEPT-S @ =  and  cns KEPT-NS @ =  and TTRUE ;

\ path-main.f's program, run in its own process: the path it reports is the written
\ file's, and the park it wakes is its own.
: PATH-RUN ( CMD:command -- CMD:command )
   CMD:RESET
   L$ s" prog" IN$ 60000 >MS CMD:RUN-OUTCOME PROCESS:ENDING>RC RC>N {: rc:n :}
   CMD:OUT$ s\" /prog\n" ENDS-WITH? {: own:bool :}
   own  rc 0 =  and 0= if rc SHOW then
   s" the program reports its own path" LABEL  own TTRUE
   s" the program exits 0" LABEL  rc 0 T= ;

\ M's program, run in its own process: only a built program lacks the engine's
\ own compiler files, so the build and the written file are the boundary under test.
: BARE-RUN ( CMD:command -- CMD:command )
   CMD:RESET
   L$ s" prog" IN$ 60000 >MS CMD:RUN-OUTCOME PROCESS:ENDING>RC RC>N {: rc:n :}
   CMD:OUT$ s\" valued\n" STR= {: same:bool :}
   same  rc 0 =  and 0= if rc SHOW then
   s" the program prints the enum its MAIN fetched" LABEL  same TTRUE
   s" the program exits 0" LABEL  rc 0 T= ;

: LOG-CASE ( CMD:command -- CMD:command )
   s" L" WHICH!  s" test/fixtures/file-objects/log-main.f" FIXTURE-BUILD 0= if exit then
   LOG-UNLOCKED  LOG-RUN  LOG-FIRST KEEP
   s" M" WHICH!  s" test/fixtures/file-objects/bare-main.f" FIXTURE-BUILD 0= if exit then
   BARE-RUN
   s" L2" WHICH!  s" test/fixtures/file-objects/log-main.f" FIXTURE-BUILD 0= if exit then
   s" the build links log-first.f" LINKED  LOG-RUN ;

: PATH-CASE ( CMD:command -- CMD:command )
   s" P" WHICH!  s" test/fixtures/file-objects/path-main.f" FIXTURE-BUILD 0= if exit then
   PATH-MAIN KEEP
   s" M2" WHICH!  s" test/fixtures/file-objects/bare-main.f" FIXTURE-BUILD 0= if exit then
   s" P2" WHICH!  s" test/fixtures/file-objects/path-main.f" FIXTURE-BUILD 0= if exit then
   s" the build links path-main.f" LINKED  PATH-RUN ;

: CHECKS ( -- )
   [: COLD ;] CMD:WITH-COMMAND
   CHECK-COLD
   [: WARM-A WARM-CUT WARM-GROW WARM-B WARM-E LOG-CASE PATH-CASE ;] CMD:WITH-COMMAND ;


: REMOVE-A ( -- ) A$ FS:REMOVE-TREE ;
: REMOVE-B ( -- ) B$ FS:REMOVE-TREE ;
: REMOVE-C ( -- ) C$ FS:REMOVE-TREE ;
: REMOVE-D ( -- ) D$ FS:REMOVE-TREE ;
: REMOVE-E ( -- ) E$ FS:REMOVE-TREE ;
: REMOVE-L ( -- ) L$ FS:REMOVE-TREE ;

: WITH-L ( -- )
   s" habu-file-objects-l" FS:MKTEMP-DIR {: l:ptr u:n :}
   l OUT-L u BYTE-COPY  u OUT-L-U !
   L$ s" out" OUT-F FS:JOIN-PATH OUT-F-U !
   ['] CHECKS ['] REMOVE-L finally ;

: WITH-E ( -- )
   s" habu-file-objects-e" FS:MKTEMP-DIR {: e:ptr u:n :}
   e OUT-E u BYTE-COPY  u OUT-E-U !
   ['] WITH-L ['] REMOVE-E finally ;

: WITH-D ( -- )
   s" habu-file-objects-d" FS:MKTEMP-DIR {: d:ptr u:n :}
   d OUT-D u BYTE-COPY  u OUT-D-U !
   ['] WITH-E ['] REMOVE-D finally ;

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
   false RTARGET:HOST-TARGET HB-BUILD-CORE:KERNEL-BUILD
   s" habu-file-objects-a" FS:MKTEMP-DIR {: a:ptr u:n :}
   a OUT-A u BYTE-COPY  u OUT-A-U !
   ['] WITH-B ['] REMOVE-A finally
   T-REPORT ;

;package

FILE-OBJECTS-TEST:RUN
