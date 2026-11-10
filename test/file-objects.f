\ file-objects.f - builds of the files in test/fixtures/file-objects/, each in a fresh hb: an object must be
\ a function of the files, not of the process.

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
8 constant LOG-FIRST                     \ TAIL$'s numbers for log-first.f's, path-main.f's,
9 constant PATH-MAIN                     \ stale-spell.f's, sealed-spell.f's, stored-run.f's,
10 constant STALE-SPELL                  \ retired-spell.f's, order-x2.f's, hidden-spell.f's and
11 constant SEALED-SPELL                 \ seal-second.f's objects, mid-four.f's last part's,
12 constant STORED-RUN                   \ unclean.f's last part's, flip.f's second part's and
13 constant RETIRED-SPELL                \ redo.f's
14 constant ORDER-X2
15 constant HIDDEN-SPELL
16 constant SEAL-SECOND
17 constant MID-FOUR-LAST
18 constant UNCLEAN-LAST
19 constant FLIP-SECOND
20 constant REDO
21 constant UNCLEAN-FIRST                \ unclean.f's first part's, which an unclean cut ends,
22 constant UNCLEAN-SECOND               \ and its second's, which follows that cut
23 constant TAILS                        \ TAIL$'s tails
46 constant FILES-C                     \ build C's: count.f's, shape.f's, each sequence's, those that
                                         \ never link included, the twins', the order sequence's four,
                                         \ the header, same and held sequences' three each, e.f's, the
                                         \ words sequence's five, the ran sequence's four
208 constant HEADER-BYTES                \ an object's header, INPUT right after it (linker.f)
192 constant PIECES-FIELD               \ the header's PIECES offset (linker.f WRITE-OBJECT)
72 constant FILES-LEN-FIELD              \ the header's FILES length offset (linker.f WRITE-OBJECT)
32 constant FILES-ROW                    \ a FILES row's bytes (linker.f FILES-ROW-BYTES)
24 constant INPUT-BYTES                  \ an object's INPUT: its file's stamp
16384 constant CAP                       \ an object's bytes
16384 constant STATE-CAP                 \ a build's output before its last line
262144 constant PROG-CAP                 \ a program's bytes, L's included

PATH-CAP BUFFER: OUT-A   variable OUT-A-U
PATH-CAP BUFFER: OUT-B   variable OUT-B-U
PATH-CAP BUFFER: OUT-C   variable OUT-C-U
PATH-CAP BUFFER: OUT-D   variable OUT-D-U
PATH-CAP BUFFER: OUT-E   variable OUT-E-U
PATH-CAP BUFFER: OUT-L   variable OUT-L-U
PATH-CAP BUFFER: OUT-F   variable OUT-F-U    \ the fixture builds' out root, in L's directory
PATH-CAP BUFFER: NEST-DIR  variable NEST-DIR-U  \ U's or I's sources: L's directory's nested or same-name
PATH-CAP BUFFER: NEST-PATH                      \ one of them
PATH-CAP 2 * BUFFER: NEST-SRC  variable NEST-SRC-U  \ the text NEST-WRITE writes
PATH-CAP BUFFER: JOINED
PATH-CAP BUFFER: JOINED-2
PATH-CAP BUFFER: ROOT    variable ROOT-U     \ the obj directory a walk starts at
PATH-CAP BUFFER: FOUND   variable FOUND-U    \ the walk's last path that ends with its tail, or REWRITTEN's
CAP BUFFER: BYTES-A
CAP BUFFER: BYTES-B
DYNAMIC-BUFFER FIXTURE-BYTES u8
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
T-LABEL-CAP BUFFER: HELD-LABEL           \ a label FIXTURE-OBJECT keeps across FOUND?
128 BUFFER: LABEL-BUF
PATH-CAP 128 + BUFFER: REFUSAL  variable REFUSAL-U  \ the loader refusal REFUSED-BUILD expects
variable OBJ-N                          \ the walk's objects
variable TAIL-I                          \ the tail the walk looks for (TAIL$)
variable TAIL-N                          \ and the paths that end with it
variable MARK-S                          \ the newest object change time MARK saw, seconds
variable MARK-NS                         \ and nanoseconds
variable NEWER-N                         \ the objects REWRITTEN saw changed after it
TAILS TYPED-BUFFER KEPT-SIZE n          \ by tail, the object's size and its change time's
TAILS TYPED-BUFFER KEPT-S n             \ seconds and nanoseconds, as KEEP read them; size -1
TAILS TYPED-BUFFER KEPT-NS n            \ if KEEP found none or never ran


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

\ A's program, run in its own process: the boundary under test is the written file.
: RUN-PROG ( CMD:command ptr u8 n -- CMD:command ) {: out:ptr outu:n :}
   CMD:RESET
   out outu s" prog" IN$ 60000 >MS CMD:RUN-OUTCOME PROCESS:ENDING>RC RC>N {: rc:n :}
   CMD:OUT$ s\" Hi\nok\n" STR=  rc 0 =  and 0= if rc SHOW then
   s" the program prints Hi and ok" LABEL  CMD:OUT$ s\" Hi\nok\n" T$=
   s" the program exits 0" LABEL  rc 0 T= ;


\ The tail of object i's path: 0 to 6 A's files in load order, 7 pad.f's, then the named tails'.
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
   i PATH-MAIN = if s" /test/fixtures/file-objects/path-main.f.hfo" exit then
   i STALE-SPELL = if s" /test/fixtures/file-objects/stale-spell.f.hfo" exit then
   i SEALED-SPELL = if s" /test/fixtures/file-objects/sealed-spell.f.hfo" exit then
   i STORED-RUN = if s" /test/fixtures/file-objects/stored-run.f.hfo" exit then
   i RETIRED-SPELL = if s" /test/fixtures/file-objects/retired-spell.f.hfo" exit then
   i ORDER-X2 = if s" /test/fixtures/file-objects/order-x2.f.hfo" exit then
   i HIDDEN-SPELL = if s" /test/fixtures/file-objects/hidden-spell.f.hfo" exit then
   i MID-FOUR-LAST = if s" /test/fixtures/file-objects/mid-four.f+3.hfo" exit then
   i UNCLEAN-LAST = if s" /test/fixtures/file-objects/unclean.f+2.hfo" exit then
   i FLIP-SECOND = if s" /test/fixtures/file-objects/flip.f+1.hfo" exit then
   i REDO = if s" /test/fixtures/file-objects/redo.f.hfo" exit then
   i UNCLEAN-FIRST = if s" /test/fixtures/file-objects/unclean.f.hfo" exit then
   i UNCLEAN-SECOND = if s" /test/fixtures/file-objects/unclean.f+1.hfo" exit then
   s" /test/fixtures/file-objects/seal-second.f.hfo" ;

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

\ D's shifted build moves records, emissions, wordlist ids and data, so an absolute index or address in an
\ object would show as bytes different from A's; objects carry texts by value.
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
\ writes; a refused file writes none, and count.f's holds its INPUT.
: CHECK-STOP ( -- )
   s" C" WHICH!
   s" the build wrote forty-six objects" LABEL  C$ OBJECTS FILES-C T=
   C$ s" C" 0 SAME-OBJECT  C$ s" C" 1 SAME-OBJECT
   C$ s" C" 0 FOUND? 0= if exit then
   FOUND$ BYTES-B CAP FS:READ-ALL {: u:n :}
   s" test/fixtures/file-objects/count.f" EXPECT!
   u HEADER-BYTES - 0 max INPUT-BYTES min {: v:n :}
   s" count.f's object holds its stamp" LABEL
   BYTES-B HEADER-BYTES + v EXPECT INPUT-BYTES T$= ;

\ A file compiles when it is edited, when what its object names in another file changes, when an
\ earlier file that defines a public name it spells, or a private name of a package it opens, is
\ edited, when an earlier file lays a string its object lays, when it is loaded without the
\ files its object names, when files that
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
   s" the header builds compile 2, 1 and 0 files" LABEL
   STATE-W STATE-W-U @ s\" header: compiled 2 1 0\n" CONTAINS? TTRUE
   s" the same builds compile 3 and 0 files" LABEL
   STATE-W STATE-W-U @ s\" same: compiled 3 0\n" CONTAINS? TTRUE
   s" the held builds compile 2, 2 and 0 files" LABEL
   STATE-W STATE-W-U @ s\" held: compiled 2 2 0\n" CONTAINS? TTRUE
   s" the crafted builds compile 1, 1, 1, 1, 1, 1, 1, 1, 1 and 0 files" LABEL
   STATE-W STATE-W-U @ s\" crafted: compiled 1 1 1 1 1 1 1 1 1 0\n" CONTAINS? TTRUE
   s" the words builds compile 5, 4, 0, 2, 1, 1 and 0 files" LABEL
   STATE-W STATE-W-U @ s\" words: compiled 5 4 0 2 1 1 0\n" CONTAINS? TTRUE
   s" the ran builds compile 4, 1, 3, 0, 3 and 0 files" LABEL
   STATE-W STATE-W-U @ s\" ran: compiled 4 1 3 0 3 0\n" CONTAINS? TTRUE ;

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

\ C's program, the same sequence's, has K as its entry: its map lists K. A failed C wrote none.
: CHECK-SAME ( -- )
   s" C" WHICH!
   STATE-W-U @ 0= if exit then
   C$ s" prog.map" IN$ PROG-1 PROG-CAP FS:READ-ALL {: u:n :}
   s" the map lists K as a word the build loaded" LABEL
   PROG-1 u s"  K word loaded " CONTAINS? TTRUE ;

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
   CHECK-MAP CHECK-FACTS CHECK-STOP CHECK-EDIT CHECK-SAME CHECK-E ;

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

\ Guards a cut object: count.f compiles, and its piece's new time compiles shape.f, main.f, reopen.f and
\ mark.f, whose objects name that piece by its time; again.f's two loads link.
: WARM-CUT ( CMD:command -- CMD:command )
   s" A4" WHICH!  CUT  s\" compiled 5\n" WARM ;

\ macOS only: Linux ext4 refuses a file past 16 TiB, so FD:RESIZE throws E-FS-IO there.
: WARM-GROW ( CMD:command -- CMD:command )
   HB-TARGET-MACOS? 0= if exit then
   s" A5" WHICH!  GROW  s\" compiled 5\n" WARM ;

\ Guards a new first file: pad.f compiles, and so do main.f and mark.f, which spell its SCRATCH and FIRST,
\ and reopen.f, which runs count.f's LATER after main.f stores into count.f's data; the other four link.
: WARM-B ( CMD:command -- CMD:command )
   s" B2" WHICH!
   A$ s" second" s\" compiled 4\n" STATE-W BUILD {: u:n :}
   u 0 < if exit then
   s" the build prints B's state" LABEL  STATE-W u STATE-B STATE-B-U @ T$=
   s" the program has B's bytes" LABEL  A$ s" prog" B$ s" prog" SAME-FILE? TTRUE
   s" A's root holds eight objects" LABEL  A$ OBJECTS FILES-B T=
   SAME-AS-B ;

: TOOL-ARGS ( CMD:command -- CMD:command )
   CMD:RESET  s" --load" CMD:ARG+  s" tools/hb-build.f" CMD:ARG+  s" --" CMD:ARG+ ;

\ After TOOL-ARGS and its options, builds the fixture to L's prog in the shared fixture out root.
\ Its TMPDIR is L's directory: a build SEALED-FAIL refuses runs no exit hook (outer.f
\ FAIL-CLOSED), so its scratch goes with L.
: TOOL-RUN ( CMD:command ptr u8 n -- CMD:command n )
   CMD:ARG+
   s" -o" CMD:ARG+  L$ s" prog" IN$ CMD:ARG+
   s" HABU_OUT" F$ CMD:ENV+  s" TMPDIR" L$ CMD:ENV+
   ENGINE-CANDIDATE:PATH$ 120000 >MS CMD:RUN-OUTCOME
   PROCESS:ENDING>RC RC>N ;

\ Builds a fixture by tools/hb-build.f (TOOL-RUN), since log.f requires and creates through does>
\ (build.f's files may not); --repl when the flag is true.
: BUILD-RC ( CMD:command ptr u8 n bool -- CMD:command n )
   {: src:ptr src-u:n repl:bool :}
   TOOL-ARGS
   repl if s" --repl" CMD:ARG+ then
   src src-u TOOL-RUN ;

: TOOL-BUILD ( CMD:command ptr u8 n bool -- CMD:command bool )
   BUILD-RC {: rc:n :}
   rc 0<> if rc SHOW then
   s" the build exits 0" LABEL  rc 0 T=
   rc 0 = ;

: FIXTURE-BUILD ( CMD:command ptr u8 n -- CMD:command bool ) false TOOL-BUILD ;
: REPL-BUILD ( CMD:command ptr u8 n -- CMD:command bool ) true TOOL-BUILD ;

\ Builds a fixture the build refuses and answers its exit code.
: FAILING-BUILD ( CMD:command ptr u8 n -- CMD:command n ) false BUILD-RC ;

\ The linker refuses task-left-main.f at its end.
: TASK-LEFT-BUILD ( CMD:command -- CMD:command )
   s" test/fixtures/file-objects/task-left-main.f" FAILING-BUILD {: rc:n :}
   CMD:ERR$ s" linker: " CONTAINS? {: named:bool :}
   CMD:ERR$ s" task-left-main.f: a task is still running at the file's end" CONTAINS?
   named and {: said:bool :}
   rc 0 =  said 0=  or if rc SHOW then
   s" the build exits nonzero" LABEL  rc 0<> TTRUE
   s" the build says a task is still running at the file's end" LABEL  said TTRUE ;

\ The loader's refusal TEXT of word-load-inc.f, by its canonical path, as the
\ next build of a fixture whose word reaches that file should say it.
: REFUSAL! ( ptr u8 n -- ) {: a:ptr u:n :}
   a REFUSAL u BYTE-COPY
   s" test/fixtures/file-objects/word-load-inc.f" SOURCE-ROOT:CANONICAL drop {: p:ptr pu:n :}
   p REFUSAL u + pu BYTE-COPY
   u pu + REFUSAL-U ! ;

\ The loader refuses word-load-inc.f, which the fixture's word reaches at its
\ top level.
: REFUSED-BUILD ( CMD:command ptr u8 n -- CMD:command ) {: src:ptr srcu:n :}
   src srcu FAILING-BUILD {: rc:n :}
   CMD:ERR$ REFUSAL REFUSAL-U @ CONTAINS? {: said:bool :}
   rc 0 =  said 0=  or if rc SHOW then
   s" the build exits nonzero" LABEL  rc 0<> TTRUE
   s" the build refuses the word's file by its path" LABEL  said TTRUE ;

\ A refusal ends the build's process, so each entry a word reaches the file
\ through is its own build: included (word-load-main.f) and provided
\ (word-provided-main.f, before it stores the file's row), each refused as a
\ word's act in a build, and SOURCE-ROOT:READ-OS (word-read-main.f), refused
\ because hb's top frame is held. SOURCE-INPUT:READ reaches the same READ-OS
\ through the default reader, so it needs no build of its own.
: WORD-LOAD-BUILD ( CMD:command -- CMD:command )
   s" include: a word loaded or provided a file during a build; do it at the file's top level: "
   REFUSAL!
   s" X" WHICH!  s" test/fixtures/file-objects/word-load-main.f" REFUSED-BUILD
   s" X4" WHICH!  s" test/fixtures/file-objects/word-provided-main.f" REFUSED-BUILD
   s" include: READ-OS reads only the file a load is opening; a word read: "
   REFUSAL!
   s" X2" WHICH!  s" test/fixtures/file-objects/word-read-main.f" REFUSED-BUILD ;

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

\ KEEP holds the size and change time of the build's object of tail i.
: KEEP ( n -- ) {: i:n :}
   -1 i KEPT-SIZE !
   F$ WHICH WHICH-U @ i FOUND? 0= if exit then
   FOUND$ FS:FILE-META {: size:n ms:n mns:n cs:n cns:n :}
   size i KEPT-SIZE !  cs i KEPT-S !  cns i KEPT-NS ! ;

\ Whether KEEP and FOUND? each found the build's object of tail i; a miss fails the label, else the
\ label names the caller's next assertion.
: KEPT-FOUND? ( ptr u8 n n -- bool ) {: a:ptr u:n i:n :}
   i KEPT-SIZE @ 0 < if a u LABEL false TTRUE false exit then
   F$ WHICH WHICH-U @ i FOUND? {: one:bool :}
   a u LABEL
   one 0= if false TTRUE then  one ;

\ Whether FOUND$ keeps the size and change time KEEP held for tail i: a compile replaces a file's
\ object (linker.f WRITE-OBJECT) and a link leaves it.
: KEPT? ( n -- bool ) {: i:n :}
   FOUND$ FS:FILE-META {: size:n ms:n mns:n cs:n cns:n :}
   size i KEPT-SIZE @ =  cs i KEPT-S @ =  and  cns i KEPT-NS @ =  and ;

\ Whether a change time, seconds and nanoseconds, is later than the one MARK holds.
: LATER? ( n n -- bool ) {: sec:n nsec:n :}
   sec MARK-S @ >  sec MARK-S @ =  nsec MARK-NS @ >  and  or ;

: MARK-SEEN ( ptr u8 n -- )
   {: p:ptr pu:n :}
   p pu s" .hfo" ENDS-WITH? 0= if exit then
   p pu FS:FILE-META {: size:n ms:n mns:n cs:n cns:n :}
   cs cns LATER? 0= if exit then
   cs MARK-S !  cns MARK-NS ! ;

: NEWER-SEEN ( ptr u8 n -- )
   {: p:ptr pu:n :}
   p pu s" .hfo" ENDS-WITH? 0= if exit then
   p pu FS:FILE-META {: size:n ms:n mns:n cs:n cns:n :}
   cs cns LATER? 0= if exit then
   1 NEWER-N +!  p FOUND pu BYTE-COPY  pu FOUND-U ! ;

: MARK-WALK ( -- ) ROOT ROOT-U @ ['] MARK-SEEN FS:WALK-FILES ;
: NEWER-WALK ( -- ) ROOT ROOT-U @ ['] NEWER-SEEN FS:WALK-FILES ;

\ MARK holds the newest change time among the fixture builds' objects.
: MARK ( -- ) F$ ROOT!  0 MARK-S !  0 MARK-NS !  ['] MARK-WALK WALKED ;

\ The fixture builds' objects changed after MARK's time; FOUND$ is the last of them.
: REWRITTEN ( -- n ) F$ ROOT!  0 NEWER-N !  0 FOUND-U !  ['] NEWER-WALK WALKED  NEWER-N @ ;

\ The label holds when the build's object of tail i is the one KEEP held.
: LINKED ( ptr u8 n n -- ) {: a:ptr u:n i:n :}
   a u i KEPT-FOUND? 0= if exit then  i KEPT? TTRUE ;

\ The label holds when the build's object of tail i replaces the one KEEP held.
: COMPILED ( ptr u8 n n -- ) {: a:ptr u:n i:n :}
   a u i KEPT-FOUND? 0= if exit then  i KEPT? TFALSE ;

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

\ H's program, run in its own process: the trap ends it, so its exit code and stderr are what is checked.
: PART-RUN ( CMD:command -- CMD:command )
   CMD:RESET
   L$ s" prog" IN$ 60000 >MS CMD:RUN-OUTCOME PROCESS:ENDING>RC RC>N {: rc:n :}
   CMD:ERR$ s\" hb: bad layout tag\n" STR= {: trapped:bool :}
   trapped  rc 85 =  and 0= if rc SHOW then
   s" the program dies with the bad-tag error" LABEL  trapped TTRUE
   s" the program exits 85" LABEL  rc 85 T= ;

\ scratch-main.f's program, run in its own process: the boundary under test is the written file.
: SCRATCH-RUN ( CMD:command -- CMD:command )
   CMD:RESET
   L$ s" prog" IN$ 60000 >MS CMD:RUN-OUTCOME PROCESS:ENDING>RC RC>N {: rc:n :}
   rc 0<> if rc SHOW then
   s" a file's scratch store ends with the file" LABEL  CMD:OUT$ s\" 0\n0\n0\n0\n0\n0\n" T$= ;

\ kept-main.f's program, run in its own process: the boundary under test is the written file.
: KEPT-RUN ( CMD:command -- CMD:command )
   CMD:RESET
   L$ s" prog" IN$ 60000 >MS CMD:RUN-OUTCOME PROCESS:ENDING>RC RC>N {: rc:n :}
   rc 0<> if rc SHOW then
   s" a kept handle is refused and word docs read at build and run time count 2 and 1" LABEL
   CMD:OUT$ s\" zip refused\naio refused\ndocs 2 1\n" T$= ;

\ kept-genio.f's program, run in its own process with empty stdin, which ends its REPL.
: GENIO-RUN ( CMD:command -- CMD:command )
   CMD:RESET
   L$ s" prog" IN$ 60000 >MS CMD:RUN-OUTCOME PROCESS:ENDING>RC RC>N {: rc:n :}
   rc 0<> if rc SHOW then
   s" a device a file kept is refused in the program" LABEL  CMD:OUT$ s\" genio refused\n" T$= ;

\ L's program, run in its own process: the boundary under test is the written file. The label
\ holds when its stdout is the wanted text.
: PRINTS-RUN ( CMD:command ptr u8 n ptr u8 n -- CMD:command )
   {: want:ptr wantu:n label:ptr labelu:n :}
   CMD:RESET
   L$ s" prog" IN$ 60000 >MS CMD:RUN-OUTCOME PROCESS:ENDING>RC RC>N {: rc:n :}
   rc 0<> if rc SHOW then
   label labelu LABEL  CMD:OUT$ want wantu T$= ;

\ Guards a lock across a build: L's program holds no mutex the build locked and takes HELD, which log-first.f left
\ held; and linker.f OWNED-RECORDS: L2, after M rewrites app-end.f's object, rewrites only that object.
: LOG-CASE ( CMD:command -- CMD:command )
   s" L" WHICH!  s" test/fixtures/file-objects/log-main.f" FIXTURE-BUILD 0= if exit then
   LOG-UNLOCKED  LOG-RUN  LOG-FIRST KEEP
   s" M" WHICH!  s" test/fixtures/file-objects/bare-main.f" FIXTURE-BUILD 0= if exit then
   BARE-RUN  MARK
   s" L2" WHICH!  s" test/fixtures/file-objects/log-main.f" FIXTURE-BUILD 0= if exit then
   s" the build links log-first.f" LOG-FIRST LINKED
   REWRITTEN {: n:n :}
   s" the build rewrites only app-end.f's object" LABEL  n 1 T=
   s" the object it rewrites is app-end.f's" LABEL  FOUND$ s" /app-end.f.hfo" ENDS-WITH? TTRUE
   LOG-RUN ;

\ Guards a linked file's top-level cache: P2 links path-main.f after M2, and its program reports its own path;
\ H's program dies on a bad tag though part-main.f's NFETCH-CHECK has a BAD-TAG that returns.
: PATH-CASE ( CMD:command -- CMD:command )
   s" P" WHICH!  s" test/fixtures/file-objects/path-main.f" FIXTURE-BUILD 0= if exit then
   PATH-MAIN KEEP
   s" M2" WHICH!  s" test/fixtures/file-objects/bare-main.f" FIXTURE-BUILD 0= if exit then
   s" P2" WHICH!  s" test/fixtures/file-objects/path-main.f" FIXTURE-BUILD 0= if exit then
   s" the build links path-main.f" PATH-MAIN LINKED  PATH-RUN
   s" H" WHICH!  s" test/fixtures/file-objects/part-main.f" FIXTURE-BUILD 0= if exit then
   PART-RUN ;

\ FIXTURE-BYTES gets the fixture out root's object of tail i; answers its length, -1 if there is not
\ one or it is shorter than a header. FOUND? asserts under its own label, then the caller's is back.
: FIXTURE-OBJECT ( n -- n )
   {: i:n :}
   T-LABEL$ {: la:ptr lu:n :}
   la HELD-LABEL lu BYTE-COPY
   F$ WHICH WHICH-U @ i FOUND? {: one:bool :}
   HELD-LABEL lu T-LABEL
   one 0= if -1 exit then
   FOUND$ FS:FILE-SIZE {: size:n :}
   size HEADER-BYTES < if -1 exit then
   size FIXTURE-BYTES-RESERVE
   FOUND$ 0 FIXTURE-BYTES size FS:READ-ALL {: u:n :}
   u HEADER-BYTES < if -1 exit then
   u ;

\ The FILES length of the fixture out root's object of tail i, -1 if there is not one.
: OBJECT-FILES ( n -- n )
   FIXTURE-OBJECT 0 < if -1 exit then
   0 FIXTURE-BYTES FILES-LEN-FIELD + LE:U64@ ;

\ The offset of the text in the fixture out root's object of tail i, -1 if it holds none or there
\ is not one.
: OBJECT-TEXT-AT ( n ptr u8 n -- n )
   {: i:n a:ptr u:n :}
   i FIXTURE-OBJECT {: ou:n :}
   ou 0 < if -1 exit then
   0 FIXTURE-BYTES ou a u FIND-SUB MATCH option
      none OF -1 ENDOF
      some OF IDX>N ENDOF
   ;MATCH ;

\ Whether the fixture out root's object of tail i holds the text, as it holds each path it names.
: OBJECT-HOLDS? ( n ptr u8 n -- bool ) OBJECT-TEXT-AT 0 >= ;


\ Whether order-x2.f's object holds the first path before the second: it lays the paths of the
\ storers it read in the order they linked.
: X2-BEFORE? ( ptr u8 n ptr u8 n -- bool )
   {: a:ptr au:n b:ptr bu:n :}
   ORDER-X2 a au OBJECT-TEXT-AT {: x:n :}
   ORDER-X2 b bu OBJECT-TEXT-AT {: y:n :}
   x 0 >=  y 0 >=  and  x y <  and ;


\ Three builds are the boundary: S gives stale-def.f an object, S2 gives stale-spell.f one naming no
\ file, and only S3 checks both in one walk, before either links; a reused object would name no file.
: STALE-CASE ( CMD:command -- CMD:command )
   s" S" WHICH!  s" test/fixtures/file-objects/stale-root.f" FIXTURE-BUILD 0= if exit then
   s" S2" WHICH!  s" test/fixtures/file-objects/stale-alone.f" FIXTURE-BUILD 0= if exit then
   s" stale-spell.f's object names no file" LABEL  STALE-SPELL OBJECT-FILES 0 T=
   s" S3" WHICH!  s" test/fixtures/file-objects/stale-root.f" FIXTURE-BUILD 0= if exit then
   s" stale-spell.f compiles: its object names stale-def.f" LABEL
   STALE-SPELL OBJECT-FILES FILES-ROW T= ;

\ Guards a private name of a checked-but-unlinked piece: K3 checks hidden-def.f's W unlinked.
: HIDDEN-CASE ( CMD:command -- CMD:command )
   s" K" WHICH!  s" test/fixtures/file-objects/hidden-root.f" FIXTURE-BUILD 0= if exit then
   s" K2" WHICH!  s" test/fixtures/file-objects/hidden-alone.f" FIXTURE-BUILD 0= if exit then
   s" hidden-spell.f's object names hidden-pkg.f alone" LABEL
   HIDDEN-SPELL OBJECT-FILES FILES-ROW T=
   s" K3" WHICH!  s" test/fixtures/file-objects/hidden-root.f" FIXTURE-BUILD 0= if exit then
   s" hidden-spell.f compiles: its object names hidden-def.f too" LABEL
   HIDDEN-SPELL OBJECT-FILES FILES-ROW 2 * T= ;

\ Guards a name a checked-but-unlinked piece changes: G3 checks sealed-seal.f's seal unlinked.
: SEALED-CASE ( CMD:command -- CMD:command )
   s" G" WHICH!  s" test/fixtures/file-objects/sealed-root.f" FIXTURE-BUILD 0= if exit then
   s" G2" WHICH!  s" test/fixtures/file-objects/sealed-alone.f" FIXTURE-BUILD 0= if exit then
   s" sealed-spell.f's object names sealed-pkg.f alone" LABEL
   SEALED-SPELL OBJECT-FILES FILES-ROW T=
   s" G3" WHICH!  s" test/fixtures/file-objects/sealed-root.f" FIXTURE-BUILD 0= if exit then
   s" sealed-spell.f compiles: its object names sealed-seal.f too" LABEL
   SEALED-SPELL OBJECT-FILES FILES-ROW 2 * T= ;

\ Guards a store a checked-but-unlinked piece makes: T3 checks stored-store.f's store unlinked.
\ stored-run.f's object also names the kernel's files its run reaches, so a path is pinned, not a count.
: STORED-CASE ( CMD:command -- CMD:command )
   s" T" WHICH!  s" test/fixtures/file-objects/stored-root.f" FIXTURE-BUILD 0= if exit then
   s" T2" WHICH!  s" test/fixtures/file-objects/stored-alone.f" FIXTURE-BUILD 0= if exit then
   s" stored-run.f's object names no stored-store.f" LABEL
   STORED-RUN s" /test/fixtures/file-objects/stored-store.f" OBJECT-HOLDS? TFALSE
   s" T3" WHICH!  s" test/fixtures/file-objects/stored-root.f" FIXTURE-BUILD 0= if exit then
   s" stored-run.f compiles: its object names stored-store.f" LABEL
   STORED-RUN s" /test/fixtures/file-objects/stored-store.f" OBJECT-HOLDS? TTRUE ;

\ Guards a name a checked-but-unlinked piece retires: V3 checks retired-undefine.f's undefine unlinked.
: RETIRED-CASE ( CMD:command -- CMD:command )
   s" V" WHICH!  s" test/fixtures/file-objects/retired-root.f" FIXTURE-BUILD 0= if exit then
   s" V2" WHICH!  s" test/fixtures/file-objects/retired-alone.f" FIXTURE-BUILD 0= if exit then
   s" retired-spell.f's object names no file" LABEL  RETIRED-SPELL OBJECT-FILES 0 T=
   s" V3" WHICH!  s" test/fixtures/file-objects/retired-root.f" FIXTURE-BUILD 0= if exit then
   s" retired-spell.f compiles: its object names retired-undefine.f" LABEL
   RETIRED-SPELL OBJECT-FILES FILES-ROW T= ;

\ Guards a file's check against built and pending pieces' rows in ascending order: O3 checks order-x2.f.
: ORDER-CASE ( CMD:command -- CMD:command )
   s" O" WHICH!  s" test/fixtures/file-objects/order-root-a.f" FIXTURE-BUILD 0= if exit then
   s" O2" WHICH!  s" test/fixtures/file-objects/order-root-b.f" FIXTURE-BUILD 0= if exit then
   s" order-x2.f's object names order-q2.f before order-q1.f" LABEL
   s" /test/fixtures/file-objects/order-q2.f" s" /test/fixtures/file-objects/order-q1.f"
   X2-BEFORE? TTRUE
   s" O3" WHICH!  s" test/fixtures/file-objects/order-root-a.f" FIXTURE-BUILD 0= if exit then
   s" order-x2.f compiles: its object names order-q1.f before order-q2.f" LABEL
   s" /test/fixtures/file-objects/order-q1.f" s" /test/fixtures/file-objects/order-q2.f"
   X2-BEFORE? TTRUE
   ORDER-X2 KEEP
   s" O4" WHICH!  s" test/fixtures/file-objects/order-root-a.f" FIXTURE-BUILD 0= if exit then
   s" the build links order-x2.f" ORDER-X2 LINKED ;

\ Guards linking parts after a compiled part, each from its own start, and a part retiring its own records; compiling a
\ part whose start moves unedited (linked: Q twice) or number shifts after an unclean top-scope cut (linked: no request).
: PART-LINK-CASE ( CMD:command -- CMD:command )
   s" Y" WHICH!  s" test/fixtures/file-objects/parts-root-1.f" FIXTURE-BUILD 0= if exit then
   MID-FOUR-LAST KEEP  UNCLEAN-LAST KEEP  UNCLEAN-FIRST KEEP  UNCLEAN-SECOND KEEP  REDO KEEP
   s" Y2" WHICH!  s" test/fixtures/file-objects/parts-root-2.f" FIXTURE-BUILD 0= if exit then
   s" the build links mid-four.f's last part" MID-FOUR-LAST LINKED
   s" the build links unclean.f's last part after an unclean request and a clean one"
   UNCLEAN-LAST LINKED
   s" unclean.f's part an unclean request ends never links" UNCLEAN-FIRST COMPILED
   s" unclean.f's part after an unclean request never links" UNCLEAN-SECOND COMPILED
   s" the build links redo.f's part" REDO LINKED
   s\" 10\n2\n30\n40\n5\n1\n2\n3\n7\n9\n2\n6\n11\n"
   s" the program prints mid-four.f's words with mid-w2.f's W, redo.f's second Y, S and T" PRINTS-RUN
   FLIP-SECOND KEEP
   s" Y3" WHICH!  s" test/fixtures/file-objects/parts-root-2.f" FIXTURE-BUILD 0= if exit then
   s" the build links the part that starts at [then]" FLIP-SECOND LINKED
   s\" 10\n2\n30\n40\n5\n1\n2\n3\n7\n9\n2\n6\n11\n"
   s" the program prints flip-a.f's F, P, Q, R, A, C, S and T" PRINTS-RUN ;

\ Guards a file's top level in a build: a scratch store ends with the file, a kept handle or device is refused in the program,
\ word docs keep no address of a freed text, a task left running or a run word that loads, provides or reads a file fails it.
: PIECE-CASE ( CMD:command -- CMD:command )
   s" R" WHICH!  s" test/fixtures/file-objects/scratch-main.f" FIXTURE-BUILD 0= if exit then
   SCRATCH-RUN
   s" Q" WHICH!  s" test/fixtures/file-objects/kept-main.f" FIXTURE-BUILD 0= if exit then  KEPT-RUN
   s" N" WHICH!  s" test/fixtures/file-objects/kept-genio.f" REPL-BUILD 0= if exit then  GENIO-RUN
   s" W" WHICH!  TASK-LEFT-BUILD
   s" W2" WHICH!  TASK-LEFT-BUILD
   WORD-LOAD-BUILD ;

\ seal-main.f's build, in a fresh hb since SEALED-FAIL ends the process it refuses in: it exits 84
\ with E-SEALED-PACKAGE naming the definition and seal-main.f's line.
: SEAL-REFUSED ( CMD:command -- CMD:command )
   s" test/fixtures/file-objects/seal-main.f" false BUILD-RC {: rc:n :}
   CMD:ERR$ s" E-SEALED-PACKAGE: FILE-OBJECTS-SEAL:D at " CONTAINS? {: named:bool :}
   CMD:ERR$ s" test/fixtures/file-objects/seal-main.f:6" CONTAINS? {: placed:bool :}
   named placed and  rc ENGINE-ERROR:SEAL-PACKAGE =  and 0= if rc SHOW then
   s" the build exits 84" LABEL  rc ENGINE-ERROR:SEAL-PACKAGE T=
   s" the build refuses FILE-OBJECTS-SEAL:D" LABEL  named TTRUE
   s" the refusal names seal-main.f's line 6" LABEL  placed TTRUE ;

\ Guards a seal a link carries: F2 links seal-second.f's object and is refused as F is.
: SEAL-CASE ( CMD:command -- CMD:command )
   s" F" WHICH!  SEAL-REFUSED  SEAL-SECOND KEEP
   s" F2" WHICH!  SEAL-REFUSED
   s" the build links seal-second.f" SEAL-SECOND LINKED ;


\ The path of U's source of that name.
: NEST-PATH$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   NEST-DIR NEST-DIR-U @ a u NEST-PATH FS:JOIN-PATH {: v:n :}
   NEST-PATH v ;

: SRC+ ( ptr u8 n -- ) {: a:ptr u:n :}
   a NEST-SRC NEST-SRC-U @ + u BYTE-COPY  u NEST-SRC-U +! ;

\ A line that requires U's source of that name by its absolute path.
: REQUIRE+ ( ptr u8 n -- ) s" require " SRC+  NEST-PATH$ SRC+  s\" \n" SRC+ ;

\ Writes the text to U's source of that name and starts the next text.
: NEST-WRITE ( ptr u8 n -- ) NEST-PATH$ NEST-SRC NEST-SRC-U @ FS:WRITE-ALL  0 NEST-SRC-U ! ;

\ x.f: FILE-OBJECTS-NESTED:K, whose value is the text, then a require of y.f, then a second package.
: WRITE-X ( ptr u8 n -- ) {: k:ptr ku:n :}
   s" package FILE-OBJECTS-NESTED public " SRC+  k ku SRC+  s\"  constant K ;package\n" SRC+
   s" y.f" REQUIRE+
   s\" package FILE-OBJECTS-NESTED-X public : X ( -- ) ; ;package\n" SRC+
   s" x.f" NEST-WRITE ;

: WRITE-NESTED ( -- )
   L$ s" nested" NEST-DIR FS:JOIN-PATH NEST-DIR-U !  NEST-DIR NEST-DIR-U @ FS:MAKE-DIR
   s\" package FILE-OBJECTS-NESTED-Y public : Y ( -- ) ; ;package\n" SRC+  s" y.f" NEST-WRITE
   s" 1" WRITE-X
   s" x.f" REQUIRE+  s\" : MAIN ( -- ) FILE-OBJECTS-NESTED:K . ;\n" SRC+  s" r.f" NEST-WRITE
   s" x.f" REQUIRE+  s\" : MAIN ( -- ) ;\n" SRC+  s" m.f" NEST-WRITE ;

\ tools/hb-build.f's build of a NEST-DIR source to the program of that name, in a fresh hb with L's
\ directory as its out root: the boundary under test is the object another build wrote. True when it exits 0.
: NEST-BUILD ( CMD:command ptr u8 n ptr u8 n -- CMD:command bool )
   {: src:ptr srcu:n prog:ptr progu:n :}
   CMD:RESET
   s" --load" CMD:ARG+  s" tools/hb-build.f" CMD:ARG+
   s" --" CMD:ARG+  src srcu NEST-PATH$ CMD:ARG+
   s" -o" CMD:ARG+  L$ prog progu IN$ CMD:ARG+
   s" HABU_OUT" L$ CMD:ENV+
   ENGINE-CANDIDATE:PATH$ 120000 >MS CMD:RUN-OUTCOME
   PROCESS:ENDING>RC RC>N {: rc:n :}
   rc 0<> if rc SHOW then
   s" the build exits 0" LABEL  rc 0 T=
   rc 0 = ;

\ L's program of that name, run in its own process: the boundary under test is the written file.
\ The label holds when its stdout is the wanted text.
: NEST-RUN ( CMD:command ptr u8 n ptr u8 n ptr u8 n -- CMD:command )
   {: prog:ptr progu:n want:ptr wantu:n label:ptr labelu:n :}
   CMD:RESET
   L$ prog progu IN$ 60000 >MS CMD:RUN-OUTCOME PROCESS:ENDING>RC RC>N {: rc:n :}
   CMD:OUT$ want wantu STR=  rc 0 =  and 0= if rc SHOW then
   label labelu LABEL  CMD:OUT$ want wantu T$= ;

\ r.f, m.f after x.f's K changes, which rewrites x.f's object and leaves r.f's, then r.f again: U3's
\ program prints the new K, since r.f's last part lists K, whose time x.f's compile in U2 changed.
: NEST-CASE ( CMD:command -- CMD:command )
   WRITE-NESTED
   s" U" WHICH!  s" r.f" s" prog-n" NEST-BUILD 0= if exit then
   s" prog-n" s\" 1\n" s" the program prints K" NEST-RUN
   s" 2" WRITE-X
   s" U2" WHICH!  s" m.f" s" prog-m" NEST-BUILD 0= if exit then
   s" U3" WHICH!  s" r.f" s" prog-n" NEST-BUILD 0= if exit then
   s" prog-n" s\" 2\n" s" the program prints K" NEST-RUN ;

\ The same-name sequence's g.f: DEF makes a word of the name it reads, or reads it and makes nothing;
\ FLAG is true when DEF makes one.
: WRITE-G ( bool -- ) {: make:bool :}
   s" package FILE-OBJECTS-SAME-G public : DEF ( -- ) " SRC+
   make if s" create 0 , " else s" parse-name drop drop " then SRC+
   s" ; " SRC+  make if s" true" else s" false" then SRC+
   s\"  constant FLAG ;package\n" SRC+  s" g.f" NEST-WRITE ;

\ f.f's second part makes private Y and X when g.f's DEF makes them and a private definer MK when g.f's
\ FLAG is true; its third part has two Y, X and MK, whose X answer 10 and 20 and MK add 0 and 100.
: WRITE-SAME ( -- )
   L$ s" same-name" NEST-DIR FS:JOIN-PATH NEST-DIR-U !  NEST-DIR NEST-DIR-U @ FS:MAKE-DIR
   true WRITE-G
   s\" : SAME-H1 ( -- n ) 1 ;\n" SRC+  s" h1.f" NEST-WRITE
   s\" : SAME-H2 ( -- n ) 2 ;\n" SRC+  s" h2.f" NEST-WRITE
   s\" : SAME-H3 ( -- n ) 3 ;\n" SRC+  s" h3.f" NEST-WRITE
   s" g.f" REQUIRE+
   s\" package FILE-OBJECTS-SAME FILE-OBJECTS-SAME-G:DEF Y FILE-OBJECTS-SAME-G:DEF X\n" SRC+
   s\" FILE-OBJECTS-SAME-G:FLAG [if]\n" SRC+
   s\" : MK ( n -- ) create , does> ( -- n ) @ 1000 + ;\n" SRC+
   s\" [then]\n;package\n" SRC+
   s" h1.f" REQUIRE+
   s\" : Y ( -- n ) 1 ; : X ( -- n ) Y 10 * ; : W1 ( -- n ) X ; undefine X undefine Y\n" SRC+
   s\" : Y ( -- n ) 2 ; : X ( -- n ) Y 10 * ; : W ( -- n ) X ;\n" SRC+
   s\" : MK ( n -- ) create , does> ( -- n ) @ ;\n: MK1 ( n -- ) MK ;\nundefine MK\n" SRC+
   s\" : MK ( n -- ) create , does> ( -- n ) @ 100 + ;\n: MK2 ( n -- ) MK ;\n" SRC+
   s" h2.f" REQUIRE+  s\" : SHOW3 ( -- ) X . Y . ;\n" SRC+
   s" h3.f" REQUIRE+  s\" : SHOW4 ( -- ) W . W1 . ;\n" SRC+  s" f.f" NEST-WRITE
   s" f.f" REQUIRE+  s\" : SHOW ( -- ) X . Y . ;\n" SRC+  s" u.f" NEST-WRITE
   s" f.f" REQUIRE+  s\" W constant K 5 MK2 FIVE : SHOWV ( -- ) W1 . K . FIVE . ;\n" SRC+
   s" v.f" NEST-WRITE
   s" f.f" REQUIRE+  s" u.f" REQUIRE+  s" v.f" REQUIRE+
   s\" : MAIN ( -- ) SHOW SHOWV SHOW3 SHOW4 cr ;\n" SRC+  s" main.f" NEST-WRITE ;

\ I's program prints what a cold build prints under either g.f.
: SAME-RUN ( CMD:command -- CMD:command )
   s" prog-s" s\" 20\n2\n10\n20\n105\n20\n2\n20\n10\n\n" s" the program prints each X and Y it reached"
   NEST-RUN ;

\ Guards a user's binding when f.f's second part gains or drops same-name records: u.f spells X and Y,
\ v.f and f.f's last part reach them only by W1 and W, and v.f's FIVE the second MK only by MK2.
: SAME-CASE ( CMD:command -- CMD:command )
   WRITE-SAME
   s" I" WHICH!  s" main.f" s" prog-s" NEST-BUILD 0= if exit then  SAME-RUN
   false WRITE-G
   s" I2" WHICH!  s" main.f" s" prog-s" NEST-BUILD 0= if exit then  SAME-RUN
   true WRITE-G
   s" I3" WHICH!  s" main.f" s" prog-s" NEST-BUILD 0= if exit then  SAME-RUN ;

\ Guards a cross build, whose words have no emission (linker.f WORD-RECORD+): bare-main.f for Wasm in a
\ fresh hb, since the process fixes the build's target. The module never runs.
: CROSS-CASE ( CMD:command -- CMD:command )
   s" WASM" WHICH!
   TOOL-ARGS  s" --target" CMD:ARG+  s" wasm" CMD:ARG+
   s" test/fixtures/file-objects/bare-main.f" TOOL-RUN {: rc:n :}
   rc 0<> if rc SHOW then
   s" the build exits 0" LABEL  rc 0 T= ;

: CHECKS ( -- )
   [: COLD ;] CMD:WITH-COMMAND
   CHECK-COLD
   [: WARM-A WARM-CUT WARM-GROW WARM-B WARM-E LOG-CASE PATH-CASE STALE-CASE HIDDEN-CASE SEALED-CASE
      STORED-CASE RETIRED-CASE ORDER-CASE PART-LINK-CASE PIECE-CASE SEAL-CASE
      NEST-CASE SAME-CASE CROSS-CASE ;]
   CMD:WITH-COMMAND ;


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

\ Every tail starts unkept, so a label on a tail no KEEP ran fails.
: UNKEPT ( -- ) TAILS 0 ?do -1 i KEPT-SIZE ! loop ;

public

\ Builds A to E link the stripped kernel object this process writes once and BUILD passes, so their out
\ roots hold only the files' objects.
: RUN ( -- )
   T-RESET  UNKEPT
   false RTARGET:HOST-TARGET HB-BUILD-CORE:KERNEL-BUILD
   s" habu-file-objects-a" FS:MKTEMP-DIR {: a:ptr u:n :}
   a OUT-A u BYTE-COPY  u OUT-A-U !
   ['] WITH-B ['] REMOVE-A finally
   T-REPORT ;

;package

FILE-OBJECTS-TEST:RUN
