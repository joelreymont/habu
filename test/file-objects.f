\ file-objects.f - builds of the files in test/fixtures/file-objects/, each in a fresh hb: an object must be
\ a function of the files, not of the process.

require src/lib/error.f
require src/lib/string.f
require src/lib/fmt.f
require src/lib/test.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/fs-identity.f
require src/lib/fd.f
require src/lib/codesign.f
require src/lib/json.f
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
23 constant ROOT-SET-LAST                \ root-set.f's last part's, after its request from the root its first part sets
24 constant SPELLED-STRIDE               \ the spelled sequence's stride.f's
25 constant SPELLED-EFFECT               \ and effect.f's
26 constant PACKAGED-BARE                \ the packaged sequence's bare.f's
27 constant PACKAGED-BOXED               \ and boxed.f's
28 constant CAST-A                       \ the cast sequence's a.f's
29 constant EVALUATED-LAST               \ the evaluated sequence's main.f's last part's
30 constant OPENED-LAST                  \ and the opened sequence's
31 constant CTOR-SUM                     \ ctor-sum.f's
32 constant EXPORT-MAIN                  \ export-main.f's
33 constant TAILS                        \ TAIL$'s tails
65 constant FILES-C                     \ build C's: count.f's, shape.f's, each sequence's, those that
                                         \ never link included, the twins', the order sequence's four,
                                         \ the header, same and held sequences' three each, e.f's, the
                                         \ words sequence's five, the ran and stores sequences' four each,
                                         \ the gain sequence's three, the own and own-cold builds' two
                                         \ each, the through and through-cold builds' four each
208 constant HEADER-BYTES                \ an object's header, INPUT right after it (linker.f)
192 constant PIECES-FIELD               \ the header's PIECES offset (linker.f WRITE-OBJECT)
72 constant FILES-LEN-FIELD              \ the header's FILES length offset (linker.f WRITE-OBJECT)
32 constant FILES-ROW                    \ a FILES row's bytes (linker.f FILES-ROW-BYTES)
24 constant STAMP-BYTES                  \ the stamp an object's INPUT starts with
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
STAMP-BYTES BUFFER: EXPECT
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
TYPED-VARIABLE REPORT-CURSOR JSON:cursor
variable REPORT-TOTAL                   \ N's report's size.total,
variable REPORT-CODE                    \ its size.code,
variable REPORT-SUM                     \ the sum of its six size classes
variable REPORT-FILES                   \ and files_reused plus files_compiled
variable CLASS-N                        \ the class CLASS+ read


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
   ENGINE-CANDIDATE:PATH$ PROCESS:WAIT-FOREVER CMD:RUN-OUTCOME
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
   out outu s" prog" IN$ PROCESS:WAIT-FOREVER CMD:RUN-OUTCOME PROCESS:ENDING>RC RC>N {: rc:n :}
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
   i ROOT-SET-LAST = if s" /test/fixtures/file-objects/root-set.f+2.hfo" exit then
   i SPELLED-STRIDE = if s" /spelled/stride.f.hfo" exit then
   i SPELLED-EFFECT = if s" /spelled/effect.f.hfo" exit then
   i PACKAGED-BARE = if s" /packaged/bare.f.hfo" exit then
   i PACKAGED-BOXED = if s" /packaged/boxed.f.hfo" exit then
   i CAST-A = if s" /cast/a.f.hfo" exit then
   i EVALUATED-LAST = if s" /evaluated/main.f+3.hfo" exit then
   i OPENED-LAST = if s" /opened/main.f+3.hfo" exit then
   i CTOR-SUM = if s" /test/fixtures/file-objects/ctor-sum.f.hfo" exit then
   i EXPORT-MAIN = if s" /test/fixtures/file-objects/export-main.f.hfo" exit then
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

\ EXPECT: the regular file's stamp, its change time seconds, its type over its change time nanoseconds
\ and its size, each low byte first.
: EXPECT! ( ptr u8 n -- )
   FS:FILE-META {: size:n ms:n mns:n cs:n cns:n :}
   cs EXPECT LE:U64!  cns FS:S-IFREG 32 lshift or EXPECT 8 + LE:U64!  size EXPECT 16 + LE:U64! ;

\ Build C's objects: each sequence's, the twins', e.f's, and count.f's and shape.f's, which A also
\ writes; a refused file writes none, and count.f's holds its INPUT.
: CHECK-STOP ( -- )
   s" C" WHICH!
   s" the build wrote sixty-five objects" LABEL  C$ OBJECTS FILES-C T=
   C$ s" C" 0 SAME-OBJECT  C$ s" C" 1 SAME-OBJECT
   C$ s" C" 0 FOUND? 0= if exit then
   FOUND$ BYTES-B CAP FS:READ-ALL {: u:n :}
   s" test/fixtures/file-objects/count.f" EXPECT!
   u HEADER-BYTES - 0 max STAMP-BYTES min {: v:n :}
   s" count.f's object holds its stamp" LABEL
   BYTES-B HEADER-BYTES + v EXPECT STAMP-BYTES T$= ;

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
   s" the crafted builds compile 1, 1, 1, 1, 1, 1, 1 and 0 files" LABEL
   STATE-W STATE-W-U @ s\" crafted: compiled 1 1 1 1 1 1 1 0\n" CONTAINS? TTRUE
   s" the words builds compile 5, 4, 0, 2, 1, 1 and 0 files" LABEL
   STATE-W STATE-W-U @ s\" words: compiled 5 4 0 2 1 1 0\n" CONTAINS? TTRUE
   s" the ran builds compile 4, 1, 3, 0, 3 and 0 files" LABEL
   STATE-W STATE-W-U @ s\" ran: compiled 4 1 3 0 3 0\n" CONTAINS? TTRUE
   s" the stores builds compile 4, 1, 2, 2, 2, 0 and 2 files" LABEL
   STATE-W STATE-W-U @ s\" stores: compiled 4 1 2 2 2 0 2\n" CONTAINS? TTRUE
   s" the gain builds compile 3, 2, 2 and 0 files" LABEL
   STATE-W STATE-W-U @ s\" gain: compiled 3 2 2 0\n" CONTAINS? TTRUE
   s" the own builds compile 2, 0, 2 and 0 files, the edited quotation's Q as a cold build's" LABEL
   STATE-W STATE-W-U @ s\" own: compiled 2 0 2 0 q 42 42 43 43 cold 2 q 43\n" CONTAINS? TTRUE
   s" the through builds compile 4, 0, 3 and 0 files, the edited quotation's Q as a cold build's" LABEL
   STATE-W STATE-W-U @ s\" through: compiled 4 0 3 0 q 42 42 43 43 cold 4 q 43\n" CONTAINS? TTRUE ;

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

\ A file's texts are placed, and a later file's literal finds an earlier file's text.
: CHECK-FACTS ( -- )
   s" A" WHICH!
   s" HI's record holds its facts text" LABEL
   s\" \nHI\n" s" ( -- ) " FACTS-OF? TTRUE
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
\ Its TMPDIR is L's directory: a build SEALED-FAIL refuses runs no exit hook (interpreter.f
\ FAIL-CLOSED), so its scratch goes with L.
: TOOL-RUN ( CMD:command ptr u8 n -- CMD:command n )
   CMD:ARG+
   s" -o" CMD:ARG+  L$ s" prog" IN$ CMD:ARG+
   s" HABU_OUT" F$ CMD:ENV+  s" TMPDIR" L$ CMD:ENV+
   ENGINE-CANDIDATE:PATH$ PROCESS:WAIT-FOREVER CMD:RUN-OUTCOME
   PROCESS:ENDING>RC RC>N ;

\ Builds a fixture by tools/hb-build.f (TOOL-RUN), since log.f requires and creates through does>
\ (build.f's files may not); --repl and --report-json when the flag is true.
: BUILD-RC ( CMD:command ptr u8 n bool -- CMD:command n )
   {: src:ptr src-u:n repl:bool :}
   TOOL-ARGS
   repl if
      s" --repl" CMD:ARG+
      s" --report-json" CMD:ARG+
   then
   src src-u TOOL-RUN ;

: BUILT? ( CMD:command n -- CMD:command bool ) {: rc:n :}
   rc 0<> if rc SHOW then
   s" the build exits 0" LABEL  rc 0 T=
   rc 0 = ;

: TOOL-BUILD ( CMD:command ptr u8 n bool -- CMD:command bool ) BUILD-RC BUILT? ;

: FIXTURE-BUILD ( CMD:command ptr u8 n -- CMD:command bool ) false TOOL-BUILD ;
: REPL-BUILD ( CMD:command ptr u8 n -- CMD:command bool ) true TOOL-BUILD ;

: SIZE-BUILD ( CMD:command ptr u8 n -- CMD:command bool )
   {: src:ptr src-u:n :}
   TOOL-ARGS
   s" --size-report" CMD:ARG+
   src src-u TOOL-RUN BUILT? ;

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
   L$ s" prog" IN$ PROCESS:WAIT-FOREVER CMD:RUN-OUTCOME PROCESS:ENDING>RC RC>N {: rc:n :}
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

\ MARK holds the newest change time among the out root's objects.
: MARK ( ptr u8 n -- ) ROOT!  0 MARK-S !  0 MARK-NS !  ['] MARK-WALK WALKED ;

\ The out root's objects changed after MARK's time; FOUND$ is the last of them.
: REWRITTEN ( ptr u8 n -- n ) ROOT!  0 NEWER-N !  0 FOUND-U !  ['] NEWER-WALK WALKED  NEWER-N @ ;

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
   L$ s" prog" IN$ PROCESS:WAIT-FOREVER CMD:RUN-OUTCOME PROCESS:ENDING>RC RC>N {: rc:n :}
   CMD:OUT$ s\" /prog\n" ENDS-WITH? {: own:bool :}
   own  rc 0 =  and 0= if rc SHOW then
   s" the program reports its own path" LABEL  own TTRUE
   s" the program exits 0" LABEL  rc 0 T= ;

\ M's program, run in its own process: only a built program lacks the engine's
\ own compiler files, so the build and the written file are the boundary under test.
: BARE-RUN ( CMD:command -- CMD:command )
   CMD:RESET
   L$ s" prog" IN$ PROCESS:WAIT-FOREVER CMD:RUN-OUTCOME PROCESS:ENDING>RC RC>N {: rc:n :}
   CMD:OUT$ s\" valued\n" STR= {: same:bool :}
   same  rc 0 =  and 0= if rc SHOW then
   s" the program prints the enum its MAIN fetched" LABEL  same TTRUE
   s" the program exits 0" LABEL  rc 0 T= ;

\ scratch-main.f's program, run in its own process: the boundary under test is the written file.
: SCRATCH-RUN ( CMD:command -- CMD:command )
   CMD:RESET
   L$ s" prog" IN$ PROCESS:WAIT-FOREVER CMD:RUN-OUTCOME PROCESS:ENDING>RC RC>N {: rc:n :}
   rc 0<> if rc SHOW then
   s" a file's scratch store ends with the file" LABEL  CMD:OUT$ s\" 0\n0\n0\n0\n0\n0\n" T$= ;

\ kept-main.f's program, run in its own process: the boundary under test is the written file.
: KEPT-RUN ( CMD:command -- CMD:command )
   CMD:RESET
   L$ s" prog" IN$ PROCESS:WAIT-FOREVER CMD:RUN-OUTCOME PROCESS:ENDING>RC RC>N {: rc:n :}
   rc 0<> if rc SHOW then
   s" a kept handle is refused and word docs read at build and run time count 2 and 1" LABEL
   CMD:OUT$ s\" zip refused\ndocs 2 1\n" T$= ;

\ Stores the member's integer in the cell; false when the object has no such member.
: MEMBER! ( JSON:reader ptr u8 n ptr n -- JSON:reader bool )
   {: key:ptr keyu:n dst:ptr :}
   key keyu JSON:READ-FIND-KEY 0= if false exit then
   JSON:READ-TOKEN JSON:T-INT <> if false exit then
   JSON:READ-INT dst ! true ;

: CLASS+ ( JSON:reader ptr u8 n -- JSON:reader bool )
   CLASS-N MEMBER! dup if CLASS-N @ REPORT-SUM +! then ;

\ The members are read in the order the report writes them: READ-FIND-KEY reads forward.
: COUNTS ( JSON:reader -- JSON:reader bool )
   s" files_reused" REPORT-FILES MEMBER! 0= if false exit then
   REPORT-FILES @ {: reused:n :}
   s" files_compiled" REPORT-FILES MEMBER! 0= if false exit then
   REPORT-FILES @ reused + REPORT-FILES ! true ;

: CLASSES ( JSON:reader -- JSON:reader bool )
   s" code" REPORT-CODE MEMBER! 0= if false exit then
   REPORT-CODE @ REPORT-SUM !
   s" names" CLASS+ 0= if false exit then
   s" data_written" CLASS+ 0= if false exit then
   s" data_zero_filled" CLASS+ 0= if false exit then
   s" padding" CLASS+ 0= if false exit then
   s" other" CLASS+ ;

: SIZES ( JSON:reader -- JSON:reader bool )
   s" size" JSON:READ-FIND-KEY 0= if false exit then
   JSON:READ-TOKEN JSON:T-OBJ <> if false exit then
   s" total" REPORT-TOTAL MEMBER! 0= if false exit then
   CLASSES ;

\ After `other`: the size object's close, the report's and the document's end.
: CLOSES? ( JSON:reader -- JSON:reader bool )
   JSON:READ-NEXT JSON:T-OBJ-END <> if false exit then
   JSON:READ-NEXT JSON:T-OBJ-END <> if false exit then
   JSON:READ-NEXT JSON:T-END = ;

\ Fills the REPORT- cells from the report; true when it reads whole.
: REPORT-READ ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   REPORT-CURSOR a u JSON:READ-INIT
   JSON:READ-NEXT JSON:T-OBJ = if COUNTS else false then
   if SIZES else false then
   if CLOSES? else false then
   swap JSON:READ-CLOSE ;

\ N's report, stdout's last line: it parses, its total is the program's length, its six classes
\ sum to the total, it charges code and it counts the build's files.
: REPORT-RUN ( CMD:command -- CMD:command )
   CMD:OUT$ {: o:ptr ou:n :}
   o ou BEFORE-LAST {: at:n :}
   o at + ou at - REPORT-READ {: read:bool :}
   read 0= if 0 SHOW then
   s" the build's report parses" LABEL  read TTRUE
   read 0= if exit then
   s" the report's total is the program's length" LABEL
   REPORT-TOTAL @  L$ s" prog" IN$ FS:FILE-SIZE T=
   s" the report's classes sum to its total" LABEL  REPORT-SUM @ REPORT-TOTAL @ T=
   s" the report charges code" LABEL  REPORT-CODE @ 0 > TTRUE
   s" the report counts the build's files" LABEL  REPORT-FILES @ 0 > TTRUE ;

\ kept-genio.f's program, run in its own process with empty stdin, which ends its REPL.
: GENIO-RUN ( CMD:command -- CMD:command )
   CMD:RESET
   L$ s" prog" IN$ PROCESS:WAIT-FOREVER CMD:RUN-OUTCOME PROCESS:ENDING>RC RC>N {: rc:n :}
   rc 0<> if rc SHOW then
   s" a device a file kept is refused in the program" LABEL  CMD:OUT$ s\" genio refused\n" T$= ;

\ L's program, run in its own process: the boundary under test is the written file. The label
\ holds when its stdout is the wanted text.
: PRINTS-RUN ( CMD:command ptr u8 n ptr u8 n -- CMD:command )
   {: want:ptr wantu:n label:ptr labelu:n :}
   CMD:RESET
   L$ s" prog" IN$ PROCESS:WAIT-FOREVER CMD:RUN-OUTCOME PROCESS:ENDING>RC RC>N {: rc:n :}
   rc 0<> if rc SHOW then
   label labelu LABEL  CMD:OUT$ want wantu T$= ;

: SIZE-RUN ( CMD:command -- CMD:command )
   s" the size report groups the code by package" LABEL
   CMD:OUT$ s\" \nby package\n" CONTAINS? TTRUE
   s" the size report groups the code by source" LABEL
   CMD:OUT$ s\" \nby source\n" CONTAINS? TTRUE ;

\ Guards a lock across a build: L's program holds no mutex the build locked and takes HELD, which log-first.f left
\ held; and linker.f OWNED-RECORDS: L2, after M rewrites app-end.f's object, rewrites only that object.
\ M's size report groups its code by package and by source.
: LOG-CASE ( CMD:command -- CMD:command )
   s" L" WHICH!  s" test/fixtures/file-objects/log-main.f" FIXTURE-BUILD 0= if exit then
   LOG-UNLOCKED  LOG-RUN  LOG-FIRST KEEP
   s" M" WHICH!  s" test/fixtures/file-objects/bare-main.f" SIZE-BUILD 0= if exit then
   SIZE-RUN  BARE-RUN  F$ MARK
   s" L2" WHICH!  s" test/fixtures/file-objects/log-main.f" FIXTURE-BUILD 0= if exit then
   s" the build links log-first.f" LOG-FIRST LINKED
   F$ REWRITTEN {: n:n :}
   s" the build rewrites only app-end.f's object" LABEL  n 1 T=
   s" the object it rewrites is app-end.f's" LABEL  FOUND$ s" /app-end.f.hfo" ENDS-WITH? TTRUE
   LOG-RUN ;

\ Guards a linked file's top-level cache: P2 links path-main.f after M2, and its program reports its own path.
: PATH-CASE ( CMD:command -- CMD:command )
   s" P" WHICH!  s" test/fixtures/file-objects/path-main.f" FIXTURE-BUILD 0= if exit then
   PATH-MAIN KEEP
   s" M2" WHICH!  s" test/fixtures/file-objects/bare-main.f" FIXTURE-BUILD 0= if exit then
   s" P2" WHICH!  s" test/fixtures/file-objects/path-main.f" FIXTURE-BUILD 0= if exit then
   s" the build links path-main.f" PATH-MAIN LINKED  PATH-RUN ;

false [if] \ off until dot habu-add-the-checks-a49d054d
\ H's program, run in its own process: the trap ends it, so its exit code and stderr are what is checked.
: PART-RUN ( CMD:command -- CMD:command )
   CMD:RESET
   L$ s" prog" IN$ PROCESS:WAIT-FOREVER CMD:RUN-OUTCOME PROCESS:ENDING>RC RC>N {: rc:n :}
   CMD:ERR$ s\" E-BAD-TAG habu: layout tag 7 outside 0..1\n" STR= {: trapped:bool :}
   trapped  rc 85 =  and 0= if rc SHOW then
   s" the program dies with the bad-tag error" LABEL  trapped TTRUE
   s" the program exits 85" LABEL  rc 85 T= ;

\ H's linked program dies on a fetch of an out-of-domain tag.
: PART-CASE ( CMD:command -- CMD:command )
   s" H" WHICH!  s" test/fixtures/file-objects/part-main.f" FIXTURE-BUILD 0= if exit then
   PART-RUN ;
[then]

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

\ Guards parts linking after a compiled part, each from its own start, retiring their own records and requesting from the
\ root a linked part set (LINK-ROOT); compiling a part that moves unedited (linked: Q twice) or shifts after an unclean cut.
: PART-LINK-CASE ( CMD:command -- CMD:command )
   s" Y" WHICH!  s" test/fixtures/file-objects/parts-root-1.f" FIXTURE-BUILD 0= if exit then
   MID-FOUR-LAST KEEP  UNCLEAN-LAST KEEP  UNCLEAN-FIRST KEEP  UNCLEAN-SECOND KEEP  REDO KEEP
   ROOT-SET-LAST KEEP
   s" Y2" WHICH!  s" test/fixtures/file-objects/parts-root-2.f" FIXTURE-BUILD 0= if exit then
   s" the build links mid-four.f's last part" MID-FOUR-LAST LINKED
   s" the build links unclean.f's last part after an unclean request and a clean one"
   UNCLEAN-LAST LINKED
   s" unclean.f's part an unclean request ends never links" UNCLEAN-FIRST COMPILED
   s" unclean.f's part after an unclean request never links" UNCLEAN-SECOND COMPILED
   s" the build links redo.f's part" REDO LINKED
   s" the build links root-set.f's last part, after its first part sets the root" ROOT-SET-LAST LINKED
   s\" 10\n2\n30\n40\n5\n1\n2\n3\n7\n9\n2\n6\n11\n13\n"
   s" the program prints MAIN's numbers: mid-four.f's with mid-w2.f's W, redo.f's X with its second Y" PRINTS-RUN
   FLIP-SECOND KEEP
   s" Y3" WHICH!  s" test/fixtures/file-objects/parts-root-2.f" FIXTURE-BUILD 0= if exit then
   s" the build links the part that starts at [then]" FLIP-SECOND LINKED
   s\" 10\n2\n30\n40\n5\n1\n2\n3\n7\n9\n2\n6\n11\n13\n"
   s" the program prints MAIN's numbers with flip.f's part at [then] linked" PRINTS-RUN ;

\ Guards a file's top level in a build: a scratch store ends with the file, a kept handle or device is refused in the program,
\ word docs keep no address of a freed text, a task left running or a run word that loads, provides or reads a file fails it.
\ N, the one engine build, reports its sizes.
: PIECE-CASE ( CMD:command -- CMD:command )
   s" R" WHICH!  s" test/fixtures/file-objects/scratch-main.f" FIXTURE-BUILD 0= if exit then
   SCRATCH-RUN
   s" Q" WHICH!  s" test/fixtures/file-objects/kept-main.f" FIXTURE-BUILD 0= if exit then  KEPT-RUN
   s" N" WHICH!  s" test/fixtures/file-objects/kept-genio.f" REPL-BUILD 0= if exit then
   REPORT-RUN  GENIO-RUN
   s" W" WHICH!  TASK-LEFT-BUILD
   s" W2" WHICH!  TASK-LEFT-BUILD
   WORD-LOAD-BUILD ;

\ seal-main.f's build, in a fresh hb since SEALED-FAIL ends the process it refuses in: it exits 84
\ with E-SEALED-PACKAGE naming the definition and seal-main.f's line.
: SEAL-REFUSED ( CMD:command -- CMD:command )
   s" test/fixtures/file-objects/seal-main.f" false BUILD-RC {: rc:n :}
   CMD:ERR$ s" E-SEALED-PACKAGE habu: " CONTAINS? {: classed:bool :}
   CMD:ERR$ s" 'FILE-OBJECTS-SEAL:D' is sealed" CONTAINS? classed and {: named:bool :}
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

\ squat-main.f's build, in a fresh hb as SEAL-REFUSED's, as a --repl image when the flag is true:
\ it exits 84 with E-SEALED-PACKAGE naming CHECKER-RESOLVE and squat-main.f's line 4.
: SQUAT-REFUSED ( CMD:command bool -- CMD:command ) {: repl:bool :}
   s" test/fixtures/file-objects/squat-main.f" repl BUILD-RC {: rc:n :}
   CMD:ERR$ s" E-SEALED-PACKAGE habu: " CONTAINS? {: classed:bool :}
   CMD:ERR$ s" 'CHECKER-RESOLVE' is sealed" CONTAINS? classed and {: named:bool :}
   CMD:ERR$ s" test/fixtures/file-objects/squat-main.f:4" CONTAINS? {: placed:bool :}
   named placed and  rc ENGINE-ERROR:SEAL-PACKAGE =  and 0= if rc SHOW then
   s" the build exits 84" LABEL  rc ENGINE-ERROR:SEAL-PACKAGE T=
   s" the build refuses CHECKER-RESOLVE" LABEL  named TTRUE
   s" the refusal names squat-main.f's line 4" LABEL  placed TTRUE ;

\ Guards the engine's seal in a program build (SQ, stripped) and a --repl build (SQ2).
: SQUAT-CASE ( CMD:command -- CMD:command )
   s" SQ" WHICH!  false SQUAT-REFUSED
   s" SQ2" WHICH!  true SQUAT-REFUSED ;

\ rollback.f's build, which catches a refused definition at its top level: it exits 74
\ and the linker says the file gave back data space.
: CAUGHT-CASE ( CMD:command -- CMD:command )
   s" CF" WHICH!
   s" test/fixtures/file-objects/rollback.f" FAILING-BUILD {: rc:n :}
   CMD:ERR$ s" test/fixtures/file-objects/rollback.f: it gave back data space"
   CONTAINS? {: said:bool :}
   rc 74 =  said and 0= if rc SHOW then
   s" the build exits 74" LABEL  rc 74 T=
   s" the build says rollback.f gave back data space" LABEL  said TTRUE ;

\ ctor-main.f's build, in a fresh hb since the refusal ends the build: it fails with
\ E-CTOR-PROTECTED's code. Answers its exit code.
: CTOR-REFUSED ( CMD:command -- CMD:command n )
   s" test/fixtures/file-objects/ctor-main.f" false BUILD-RC {: rc:n :}
   SB-RESET  s" threw " SB-APPEND  E-CTOR-PROTECTED FMT:SB-INT
   CMD:ERR$ SB$ CONTAINS? {: named:bool :}
   named  rc 0<>  and 0= if rc SHOW then
   s" the build fails" LABEL  rc 0<> TTRUE
   s" the build refuses with E-CTOR-PROTECTED" LABEL  named TTRUE
   rc ;

\ Guards a constructor package a link carries: CT2 links ctor-sum.f's object and is refused as
\ CT, which compiles it, is.
: CTOR-CASE ( CMD:command -- CMD:command )
   s" CT" WHICH!  CTOR-REFUSED {: cold:n :}  CTOR-SUM KEEP
   s" CT2" WHICH!  CTOR-REFUSED {: warm:n :}
   s" the build exits as CT does" LABEL  warm cold T=
   s" the build links ctor-sum.f" CTOR-SUM LINKED ;

\ export-main.f's image, run in its own process, since the boundary under test is the written
\ file: its input calls each export in a definition, then at top level, defines a word with
\ the exported does> definer and ends at the undefined export, as a load of the file does.
: EXPORT-RUN ( CMD:command -- CMD:command )
   CMD:RESET
   SB-RESET
   S\" : FILE-OBJECTS-EXP-SHOW ( -- ) FILE-OBJECTS-EXP:HIDDEN . FILE-OBJECTS-EXP:PUB . " SB-APPEND
   S\" FILE-OBJECTS-EXP:THREE . FILE-OBJECTS-EXP:KEPT . ;\n" SB-APPEND
   S\" FILE-OBJECTS-EXP-SHOW\nFILE-OBJECTS-EXP:HIDDEN . FILE-OBJECTS-EXP:PUB . " SB-APPEND
   S\" FILE-OBJECTS-EXP:THREE . FILE-OBJECTS-EXP:KEPT .\n" SB-APPEND
   S\" 11 FILE-OBJECTS-EXP:CELLED FILE-OBJECTS-EXP-ELEVEN  FILE-OBJECTS-EXP-ELEVEN .\n" SB-APPEND
   S\" FILE-OBJECTS-EXP:GONE .\n" SB-APPEND
   SB$ CMD:IN!
   L$ s" prog" IN$ 60000 >MS CMD:RUN-OUTCOME PROCESS:ENDING>RC RC>N {: rc:n :}
   rc 70 <> if rc SHOW then
   s" the image's definition and top level call the exports" LABEL
   CMD:OUT$ s\" 42\n5\n3\n9\n42\n5\n3\n9\n11\n" T$=
   s" the image refuses the undefined export" LABEL
   CMD:ERR$ s\" E-UNDEFINED habu: undefined word 'FILE-OBJECTS-EXP:GONE'\n" T$=
   s" the image exits 70" LABEL  rc 70 T= ;

\ Guards an image's exports: EX compiles export-main.f into a --repl image, EX2 links its object
\ into another, and each resolves an exported private word, another package's public word, a
\ constant, a does> definer and an export whose source the file undefines, and refuses an
\ export the file undefines.
: EXPORT-CASE ( CMD:command -- CMD:command )
   s" EX" WHICH!  s" test/fixtures/file-objects/export-main.f" REPL-BUILD 0= if exit then
   EXPORT-RUN  EXPORT-MAIN KEEP
   s" EX2" WHICH!  s" test/fixtures/file-objects/export-main.f" REPL-BUILD 0= if exit then
   s" the build links export-main.f" EXPORT-MAIN LINKED  EXPORT-RUN ;


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
\ directory as its out root: the boundary under test is the object another build wrote. Answers its
\ exit code.
: NEST-RC ( CMD:command ptr u8 n ptr u8 n -- CMD:command n )
   {: src:ptr srcu:n prog:ptr progu:n :}
   CMD:RESET
   s" --load" CMD:ARG+  s" tools/hb-build.f" CMD:ARG+
   s" --" CMD:ARG+  src srcu NEST-PATH$ CMD:ARG+
   s" -o" CMD:ARG+  L$ prog progu IN$ CMD:ARG+
   s" HABU_OUT" L$ CMD:ENV+
   ENGINE-CANDIDATE:PATH$ PROCESS:WAIT-FOREVER CMD:RUN-OUTCOME
   PROCESS:ENDING>RC RC>N ;

\ NEST-RC's build, true when it exits 0.
: NEST-BUILD ( CMD:command ptr u8 n ptr u8 n -- CMD:command bool )
   NEST-RC {: rc:n :}
   rc 0<> if rc SHOW then
   s" the build exits 0" LABEL  rc 0 T=
   rc 0 = ;

\ L's program of that name, run in its own process: the boundary under test is the written file.
\ The label holds when its stdout is the wanted text.
: NEST-RUN ( CMD:command ptr u8 n ptr u8 n ptr u8 n -- CMD:command )
   {: prog:ptr progu:n want:ptr wantu:n label:ptr labelu:n :}
   CMD:RESET
   L$ prog progu IN$ PROCESS:WAIT-FOREVER CMD:RUN-OUTCOME PROCESS:ENDING>RC RC>N {: rc:n :}
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

\ prog-n's inode and change time, seconds and nanoseconds.
: PROG-N-STAMP ( -- n n n )
   L$ s" prog-n" IN$ FS:PATH-IDENTITY nip
   L$ s" prog-n" IN$ FS:FILE-META {: size:n ms:n mns:n cs:n cns:n :}
   cs cns ;

: STAMP= ( n n n n n n -- bool ) {: i1:n s1:n n1:n i2:n s2:n n2:n :}
   i1 i2 = s1 s2 = and n1 n2 = and ;

\ Guards the install of an unchanged program: U4, with nothing changed since U3, leaves prog-n as it
\ was; U5, after x.f's K changes, replaces it with a team-signed one.
: KEPT-CASE ( CMD:command -- CMD:command )
   L$ s" prog-n" IN$ FS:FILE? 0= if exit then
   PROG-N-STAMP {: ino:n cs:n cns:n :}
   s" U4" WHICH!  s" r.f" s" prog-n" NEST-BUILD 0= if exit then
   s" the build leaves the program's inode and change time" LABEL
   ino cs cns PROG-N-STAMP STAMP= TTRUE
   s" the program's signature is valid" LABEL  L$ s" prog-n" IN$ CODESIGN:VERIFY-RC 0 T=
   s" 3" WRITE-X
   s" U5" WHICH!  s" r.f" s" prog-n" NEST-BUILD 0= if exit then
   s" the build replaces the program" LABEL  ino cs cns PROG-N-STAMP STAMP= TFALSE
   s" the program is signed for the team" LABEL
   L$ s" prog-n" IN$ CODESIGN:TEAM-SIGNED? TTRUE
   s" prog-n" s\" 3\n" s" the program prints K" NEST-RUN ;

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

\ The retire sequence's s.f: a package with no store, and an undefine of h.f's W when the flag is true.
: WRITE-RETIRE-S ( bool -- ) {: retire:bool :}
   s\" package FILE-OBJECTS-RETIRE-S public variable Z ;package\n" SRC+
   retire if s\" undefine FILE-OBJECTS-RETIRE-W\n" SRC+ then
   s" s.f" NEST-WRITE ;

\ r.f's K is 1 when h.f's W is in the current wordlist as r.f loads, else 0; r.f names neither W nor s.f.
: WRITE-RETIRE ( -- )
   L$ s" retire" NEST-DIR FS:JOIN-PATH NEST-DIR-U !  NEST-DIR NEST-DIR-U @ FS:MAKE-DIR
   s\" : FILE-OBJECTS-RETIRE-W ( -- n ) 1 ;\npackage FILE-OBJECTS-RETIRE public\n" SRC+
   s\" : FOUND ( -- n ) s\" FILE-OBJECTS-RETIRE-W\" get-current search-wl 0= if 0 else 1 then ;\n" SRC+
   s\" ;package\n" SRC+  s" h.f" NEST-WRITE
   true WRITE-RETIRE-S
   s\" FILE-OBJECTS-RETIRE:FOUND constant FILE-OBJECTS-RETIRE-K\n" SRC+  s" r.f" NEST-WRITE
   s" h.f" REQUIRE+  s" s.f" REQUIRE+  s" r.f" REQUIRE+
   s\" : MAIN ( -- ) FILE-OBJECTS-RETIRE-K . ;\n" SRC+  s" main.f" NEST-WRITE ;

\ Guards a piece that stops changing an earlier record and has no stores: Z2 must compile r.f, whose K
\ read W's record while s.f retired it, and print a cold build's 1; Z3, with nothing changed, links every file.
: RETIRE-CASE ( CMD:command -- CMD:command )
   WRITE-RETIRE
   s" Z" WHICH!  s" main.f" s" prog-r" NEST-BUILD 0= if exit then
   s" prog-r" s\" 0\n" s" the program prints K" NEST-RUN
   false WRITE-RETIRE-S
   s" Z2" WHICH!  s" main.f" s" prog-r" NEST-BUILD 0= if exit then
   s" prog-r" s\" 1\n" s" the program prints K" NEST-RUN  L$ MARK
   s" Z3" WHICH!  s" main.f" s" prog-r" NEST-BUILD 0= if exit then
   L$ REWRITTEN {: n:n :}
   s" build three rewrites no object" LABEL  n 0 T=
   s" prog-r" s\" 1\n" s" the program prints K" NEST-RUN ;

\ The clause sequence: h.f's definer FILE-OBJECTS-CL has a does> clause, w.f defines a word of the
\ clause's name, one.f requires h.f, and two.f w.f then h.f.
: WRITE-CLAUSE ( -- )
   L$ s" clause" NEST-DIR FS:JOIN-PATH NEST-DIR-U !  NEST-DIR NEST-DIR-U @ FS:MAKE-DIR
   s\" : FILE-OBJECTS-CL ( -- ) create does> ( -- ) drop ;\n" SRC+  s" h.f" NEST-WRITE
   s\" : FILE-OBJECTS-CL;does ( -- ) ;\n" SRC+  s" w.f" NEST-WRITE
   s" h.f" REQUIRE+  s\" : MAIN ( -- ) ;\n" SRC+  s" one.f" NEST-WRITE
   s" w.f" REQUIRE+  s" h.f" REQUIRE+  s\" : MAIN ( -- ) ;\n" SRC+  s" two.f" NEST-WRITE ;

\ two.f's build, refused at h.f's line 1 since w.f's word holds the name of FILE-OBJECTS-CL's
\ clause. Answers its exit code.
: CLAUSE-REFUSED ( CMD:command -- CMD:command n )
   s" two.f" s" prog-c" NEST-RC {: rc:n :}
   CMD:ERR$ s" h.f:1: duplicate definition: FILE-OBJECTS-CL;does" CONTAINS? {: named:bool :}
   named  rc 0<>  and 0= if rc SHOW then
   s" the build refuses the clause's name" LABEL  named TTRUE
   rc ;

\ Guards a does> clause a link places: CL2 compiles h.f with no word of its clause's name live. h.f's
\ object records that name, so in CL3 w.f's word makes it not hold: h.f compiles and is refused as in CL.
: CLAUSE-CASE ( CMD:command -- CMD:command )
   WRITE-CLAUSE
   s" CL" WHICH!  CLAUSE-REFUSED {: cold:n :}
   s" CL2" WHICH!  s" one.f" s" prog-c" NEST-BUILD 0= if exit then
   s" CL3" WHICH!  CLAUSE-REFUSED {: warm:n :}
   s" the build exits as CL does" LABEL  warm cold T= ;

\ The read sequence's d.txt, the text f.f reads as it loads.
: WRITE-DATA ( ptr u8 n -- ) SRC+  s" d.txt" NEST-WRITE ;

\ f.f's K is d.txt's first byte, which it reads by its absolute path through FS:READ-ALL.
: WRITE-READ ( -- )
   L$ s" read" NEST-DIR FS:JOIN-PATH NEST-DIR-U !  NEST-DIR NEST-DIR-U @ FS:MAKE-DIR
   s" 1" WRITE-DATA
   s\" require src/lib/fs.f\npackage FILE-OBJECTS-READ\n1 BUFFER: B\ns\" " SRC+
   s" d.txt" NEST-PATH$ SRC+
   s\" \" B 1 FS:READ-ALL drop\npublic B c@ constant K ;package\n" SRC+  s" f.f" NEST-WRITE
   s" f.f" REQUIRE+  s\" : MAIN ( -- ) FILE-OBJECTS-READ:K . ;\n" SRC+  s" main.f" NEST-WRITE ;

\ Guards a file a load reads: RD2, after d.txt's byte changes and its size does not, prints a cold
\ build's 50; RD3, with nothing changed, links every file.
: READ-CASE ( CMD:command -- CMD:command )
   WRITE-READ
   s" RD" WHICH!  s" main.f" s" prog-d" NEST-BUILD 0= if exit then
   s" prog-d" s\" 49\n" s" the program prints d.txt's byte" NEST-RUN
   s" 2" WRITE-DATA
   s" RD2" WHICH!  s" main.f" s" prog-d" NEST-BUILD 0= if exit then
   s" prog-d" s\" 50\n" s" the program prints d.txt's byte" NEST-RUN  L$ MARK
   s" RD3" WHICH!  s" main.f" s" prog-d" NEST-BUILD 0= if exit then
   L$ REWRITTEN {: n:n :}
   s" build three rewrites no object" LABEL  n 0 T=
   s" prog-d" s\" 50\n" s" the program prints d.txt's byte" NEST-RUN ;

\ The listing sequence's f.f: K counts the entries of e/, which it lists by its absolute path through FS-LIST:EACH.
: WRITE-LIST ( -- )
   L$ s" list" NEST-DIR FS:JOIN-PATH NEST-DIR-U !  NEST-DIR NEST-DIR-U @ FS:MAKE-DIR
   s" e" NEST-PATH$ FS:MAKE-DIR
   s" 1" SRC+  s" e/a.txt" NEST-WRITE
   s\" require src/lib/fs-list.f\npackage FILE-OBJECTS-LIST\nvariable N\n: COUNTED ( -- ) s\" " SRC+
   s" e" NEST-PATH$ SRC+
   s\" \" [: 2drop 1 N +! ;] FS-LIST:EACH ;\nCOUNTED public N @ constant K ;package\n" SRC+
   s" f.f" NEST-WRITE
   s" f.f" REQUIRE+  s\" : MAIN ( -- ) FILE-OBJECTS-LIST:K . ;\n" SRC+  s" main.f" NEST-WRITE ;

\ Guards a directory a load lists: LS2, after a file is added to e/, prints a cold build's 2; LS3, with
\ nothing changed, links every file.
: LIST-CASE ( CMD:command -- CMD:command )
   WRITE-LIST
   s" LS" WHICH!  s" main.f" s" prog-l" NEST-BUILD 0= if exit then
   s" prog-l" s\" 1\n" s" the program prints e/'s entry count" NEST-RUN
   s" 2" SRC+  s" e/b.txt" NEST-WRITE
   s" LS2" WHICH!  s" main.f" s" prog-l" NEST-BUILD 0= if exit then
   s" prog-l" s\" 2\n" s" the program prints e/'s entry count" NEST-RUN  L$ MARK
   s" LS3" WHICH!  s" main.f" s" prog-l" NEST-BUILD 0= if exit then
   L$ REWRITTEN {: n:n :}
   s" build three rewrites no object" LABEL  n 0 T=
   s" prog-l" s\" 2\n" s" the program prints e/'s entry count" NEST-RUN ;

\ The link sequence's f.f: K is whether l is a symlink, which it asks by its absolute path through
\ FS:SYMLINK?; l names a missing file.
: WRITE-LINK ( -- )
   L$ s" link" NEST-DIR FS:JOIN-PATH NEST-DIR-U !  NEST-DIR NEST-DIR-U @ FS:MAKE-DIR
   s" missing" s" l" NEST-PATH$ FS:MAKE-SYMLINK
   s\" require src/lib/fs.f\npackage FILE-OBJECTS-LINK\npublic s\" " SRC+
   s" l" NEST-PATH$ SRC+
   s\" \" FS:SYMLINK? bool>n constant K ;package\n" SRC+  s" f.f" NEST-WRITE
   s" f.f" REQUIRE+  s\" : MAIN ( -- ) FILE-OBJECTS-LINK:K . ;\n" SRC+  s" main.f" NEST-WRITE ;

\ Guards a dangling symlink a load asks about: SL2, with nothing changed, links every file; SL3, after
\ l is removed, prints a cold build's 0; SL4, with nothing changed, links every file.
: LINK-CASE ( CMD:command -- CMD:command )
   WRITE-LINK
   s" SL" WHICH!  s" main.f" s" prog-y" NEST-BUILD 0= if exit then
   s" prog-y" s\" -1\n" s" the program prints whether l is a symlink" NEST-RUN  L$ MARK
   s" SL2" WHICH!  s" main.f" s" prog-y" NEST-BUILD 0= if exit then
   L$ REWRITTEN {: kept:n :}
   s" build two rewrites no object" LABEL  kept 0 T=
   s" l" NEST-PATH$ FS:REMOVE-FILE
   s" SL3" WHICH!  s" main.f" s" prog-y" NEST-BUILD 0= if exit then
   s" prog-y" s\" 0\n" s" the program prints whether l is a symlink" NEST-RUN  L$ MARK
   s" SL4" WHICH!  s" main.f" s" prog-y" NEST-BUILD 0= if exit then
   L$ REWRITTEN {: n:n :}
   s" build four rewrites no object" LABEL  n 0 T=
   s" prog-y" s\" 0\n" s" the program prints whether l is a symlink" NEST-RUN ;

\ The task sequence's f.f: K is d.txt's first byte, which a task f.f runs and joins as it loads reads
\ by its absolute path through FS:READ-ALL.
: WRITE-TASK ( -- )
   L$ s" task" NEST-DIR FS:JOIN-PATH NEST-DIR-U !  NEST-DIR NEST-DIR-U @ FS:MAKE-DIR
   s" 1" WRITE-DATA
   s\" require src/lib/fs.f\nrequire src/lib/task.f\npackage FILE-OBJECTS-TASK\n1 BUFFER: B\n" SRC+
   s\" TASK:MIN-STACK TASK:TASK READER\n: READ-D ( -- ) s\" " SRC+
   s" d.txt" NEST-PATH$ SRC+
   s\" \" B 1 FS:READ-ALL drop 0 TASK:RETURN ;\n" SRC+
   s\" : RUN-READER ( -- ) ['] READ-D READER TASK:ACTIVATE\n" SRC+
   s\"    READER TASK:JOIN MATCH result ok OF drop ENDOF err OF throw ENDOF ;MATCH ;\n" SRC+
   s\" RUN-READER public B c@ constant K ;package\n" SRC+  s" f.f" NEST-WRITE
   s" f.f" REQUIRE+  s\" : MAIN ( -- ) FILE-OBJECTS-TASK:K . ;\n" SRC+  s" main.f" NEST-WRITE ;

\ Guards a file a task reads, which no recording holds: TK2, with nothing changed, still compiles f.f.
: TASK-CASE ( CMD:command -- CMD:command )
   WRITE-TASK
   s" TK" WHICH!  s" main.f" s" prog-t" NEST-BUILD 0= if exit then
   s" prog-t" s\" 49\n" s" the program prints d.txt's byte" NEST-RUN  L$ MARK
   s" TK2" WHICH!  s" main.f" s" prog-t" NEST-BUILD 0= if exit then
   L$ REWRITTEN {: n:n :}
   s" build two rewrites an object" LABEL  n 0 > TTRUE
   s" prog-t" s\" 49\n" s" the program prints d.txt's byte" NEST-RUN ;

\ The samefile sequence's f.f: K is whether a.txt and l, a symlink to it, name one file, which it
\ asks by their absolute paths through FS:SAMEFILE.
: WRITE-SAMEFILE ( -- )
   L$ s" samefile" NEST-DIR FS:JOIN-PATH NEST-DIR-U !  NEST-DIR NEST-DIR-U @ FS:MAKE-DIR
   s" 1" SRC+  s" a.txt" NEST-WRITE
   s" 22" SRC+  s" b.txt" NEST-WRITE
   s" a.txt" s" l" NEST-PATH$ FS:MAKE-SYMLINK
   s\" require src/lib/fs-identity.f\npackage FILE-OBJECTS-SAMEFILE\npublic s\" " SRC+
   s" a.txt" NEST-PATH$ SRC+  s\" \" s\" " SRC+  s" l" NEST-PATH$ SRC+
   s\" \" FS:SAMEFILE bool>n constant K ;package\n" SRC+  s" f.f" NEST-WRITE
   s" f.f" REQUIRE+  s\" : MAIN ( -- ) FILE-OBJECTS-SAMEFILE:K . ;\n" SRC+  s" main.f" NEST-WRITE ;

\ Guards a symlink a load compares through: SF2, after l is re-pointed to b.txt, prints a cold build's
\ 0; SF3, with nothing changed, links every file.
: SAMEFILE-CASE ( CMD:command -- CMD:command )
   WRITE-SAMEFILE
   s" SF" WHICH!  s" main.f" s" prog-sf" NEST-BUILD 0= if exit then
   s" prog-sf" s\" -1\n" s" the program prints whether a.txt and l are one file" NEST-RUN
   s" l" NEST-PATH$ FS:REMOVE-FILE  s" b.txt" s" l" NEST-PATH$ FS:MAKE-SYMLINK
   s" SF2" WHICH!  s" main.f" s" prog-sf" NEST-BUILD 0= if exit then
   s" prog-sf" s\" 0\n" s" the program prints whether a.txt and l are one file" NEST-RUN  L$ MARK
   s" SF3" WHICH!  s" main.f" s" prog-sf" NEST-BUILD 0= if exit then
   L$ REWRITTEN {: n:n :}
   s" build three rewrites no object" LABEL  n 0 T=
   s" prog-sf" s\" 0\n" s" the program prints whether a.txt and l are one file" NEST-RUN ;

\ The anchored sequence's f.f: K is the byte count of d.txt, which it reads through FS:READ-FILE-AT
\ from its directory, opened by its real path (FS:OPEN-DIR follows no symlink) as it loads.
: WRITE-ANCHORED ( -- )
   L$ s" anchored" NEST-DIR FS:JOIN-PATH NEST-DIR-U !  NEST-DIR NEST-DIR-U @ FS:MAKE-DIR
   s" 1" WRITE-DATA
   s\" require src/lib/fs-anchored.f\npackage FILE-OBJECTS-ANCHORED\n1 SPAN-BUFFER: B\n" SRC+
   s\" : READ-D ( -- n ) s\" " SRC+
   NEST-DIR NEST-DIR-U @ FS:>PATHZ NEST-PATH PATH-CAP realpath {: real:n :}
   NEST-PATH real SRC+
   s\" \" FS:OPEN-DIR {: d:fd :} d s\" d.txt\" B FS:READ-FILE-AT d FS:CLOSE-DIR ;\n" SRC+
   s\" public READ-D constant K ;package\n" SRC+  s" f.f" NEST-WRITE
   s" f.f" REQUIRE+  s\" : MAIN ( -- ) FILE-OBJECTS-ANCHORED:K . ;\n" SRC+  s" main.f" NEST-WRITE ;

\ Guards a file a load reads through an anchor, which no recording holds: AN2, with nothing changed,
\ still compiles f.f.
: ANCHORED-CASE ( CMD:command -- CMD:command )
   WRITE-ANCHORED
   s" AN" WHICH!  s" main.f" s" prog-an" NEST-BUILD 0= if exit then
   s" prog-an" s\" 1\n" s" the program prints d.txt's byte count" NEST-RUN  L$ MARK
   s" AN2" WHICH!  s" main.f" s" prog-an" NEST-BUILD 0= if exit then
   L$ REWRITTEN {: n:n :}
   s" build two rewrites an object" LABEL  n 0 > TTRUE
   s" prog-an" s\" 1\n" s" the program prints d.txt's byte count" NEST-RUN ;

\ The spawn sequence's f.f: K is d.txt's first byte, which /bin/cat, run as f.f loads, reads by its
\ absolute path and prints.
: WRITE-SPAWN ( -- )
   L$ s" spawn" NEST-DIR FS:JOIN-PATH NEST-DIR-U !  NEST-DIR NEST-DIR-U @ FS:MAKE-DIR
   s" 1" WRITE-DATA
   s\" require src/lib/process-command.f\npackage FILE-OBJECTS-SPAWN\nvariable N\n" SRC+
   s\" : CAT ( CMD:command -- CMD:command ) s\" " SRC+  s" d.txt" NEST-PATH$ SRC+
   s\" \" CMD:ARG+ s\" /bin/cat\" PROCESS:WAIT-FOREVER 1 CMD:RUN CMD:OUT$ drop c@ N ! ;\n" SRC+
   s\" : RUN-CAT ( -- ) [: CAT ;] CMD:WITH-COMMAND ;\nRUN-CAT public N @ constant K ;package\n" SRC+
   s" f.f" NEST-WRITE
   s" f.f" REQUIRE+  s\" : MAIN ( -- ) FILE-OBJECTS-SPAWN:K . ;\n" SRC+  s" main.f" NEST-WRITE ;

\ Guards a file a process a load starts reads, which no recording holds: SP2, after d.txt's byte
\ changes, prints a cold build's 50.
: SPAWN-CASE ( CMD:command -- CMD:command )
   WRITE-SPAWN
   s" SP" WHICH!  s" main.f" s" prog-sp" NEST-BUILD 0= if exit then
   s" prog-sp" s\" 49\n" s" the program prints d.txt's byte" NEST-RUN
   s" 2" WRITE-DATA
   s" SP2" WHICH!  s" main.f" s" prog-sp" NEST-BUILD 0= if exit then
   s" prog-sp" s\" 50\n" s" the program prints d.txt's byte" NEST-RUN ;

\ The zip sequence's f.f: K is the entry count of z.zip, an empty archive, which it opens by its
\ absolute path through ZIP:OPEN and closes.
: WRITE-ZIP ( -- )
   L$ s" zip" NEST-DIR FS:JOIN-PATH NEST-DIR-U !  NEST-DIR NEST-DIR-U @ FS:MAKE-DIR
   s\" PK\x05\x06\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00" SRC+
   s" z.zip" NEST-WRITE
   s\" require src/lib/zip.f\npackage FILE-OBJECTS-ZIP\n: COUNTED ( -- n ) s\" " SRC+
   s" z.zip" NEST-PATH$ SRC+
   s\" \" ZIP:OPEN {: z:ZIP:archive :} z ZIP:COUNT z ZIP:CLOSE ;\n" SRC+
   s\" public COUNTED constant K ;package\n" SRC+  s" f.f" NEST-WRITE
   s" f.f" REQUIRE+  s\" : MAIN ( -- ) FILE-OBJECTS-ZIP:K . ;\n" SRC+  s" main.f" NEST-WRITE ;

\ Guards the temp root a ZIP:OPEN snapshots into, which no compile reads: ZP2, with nothing changed,
\ links every file.
: ZIP-CASE ( CMD:command -- CMD:command )
   WRITE-ZIP
   s" ZP" WHICH!  s" main.f" s" prog-z" NEST-BUILD 0= if exit then
   s" prog-z" s\" 0\n" s" the program prints z.zip's entry count" NEST-RUN  L$ MARK
   s" ZP2" WHICH!  s" main.f" s" prog-z" NEST-BUILD 0= if exit then
   L$ REWRITTEN {: n:n :}
   s" build two rewrites no object" LABEL  n 0 T=
   s" prog-z" s\" 0\n" s" the program prints z.zip's entry count" NEST-RUN ;

\ The jsonl sequence's f.f: K counts the lines of d.jsonl, which it reads by its absolute path
\ through JSONL:OPEN.
: WRITE-JSONL ( -- )
   L$ s" jsonl" NEST-DIR FS:JOIN-PATH NEST-DIR-U !  NEST-DIR NEST-DIR-U @ FS:MAKE-DIR
   s\" 1\n" SRC+  s" d.jsonl" NEST-WRITE
   s\" require src/lib/byte-buffer.f\nrequire src/lib/jsonl.f\npackage FILE-OBJECTS-JSONL\n" SRC+
   s\" TYPED-VARIABLE C JSONL:cursor\nvariable N\n: COUNTED ( -- ) C s\" " SRC+
   s" d.jsonl" NEST-PATH$ SRC+
   s\" \" 64 BUF:N>BLEN JSONL:OPEN begin C JSONL:NEXT while 1 N +! repeat ;\n" SRC+
   s\" COUNTED public N @ constant K ;package\n" SRC+  s" f.f" NEST-WRITE
   s" f.f" REQUIRE+  s\" : MAIN ( -- ) FILE-OBJECTS-JSONL:K . ;\n" SRC+  s" main.f" NEST-WRITE ;

\ Guards a file a load reads through JSONL: JL2, after a line is added to d.jsonl, prints a cold
\ build's 2; JL3, with nothing changed, links every file.
: JSONL-CASE ( CMD:command -- CMD:command )
   WRITE-JSONL
   s" JL" WHICH!  s" main.f" s" prog-j" NEST-BUILD 0= if exit then
   s" prog-j" s\" 1\n" s" the program prints d.jsonl's line count" NEST-RUN
   s\" 1\n2\n" SRC+  s" d.jsonl" NEST-WRITE
   s" JL2" WHICH!  s" main.f" s" prog-j" NEST-BUILD 0= if exit then
   s" prog-j" s\" 2\n" s" the program prints d.jsonl's line count" NEST-RUN  L$ MARK
   s" JL3" WHICH!  s" main.f" s" prog-j" NEST-BUILD 0= if exit then
   L$ REWRITTEN {: n:n :}
   s" build three rewrites no object" LABEL  n 0 T=
   s" prog-j" s\" 2\n" s" the program prints d.jsonl's line count" NEST-RUN ;

\ The kind sequence's f.f: K is whether k is a directory, which it asks by its absolute path through FS:DIR?.
: WRITE-KIND ( -- )
   L$ s" kind" NEST-DIR FS:JOIN-PATH NEST-DIR-U !  NEST-DIR NEST-DIR-U @ FS:MAKE-DIR
   s\" require src/lib/fs.f\npackage FILE-OBJECTS-KIND\npublic s\" " SRC+
   s" k" NEST-PATH$ SRC+
   s\" \" FS:DIR? bool>n constant K ;package\n" SRC+  s" f.f" NEST-WRITE
   s" f.f" REQUIRE+  s\" : MAIN ( -- ) FILE-OBJECTS-KIND:K . ;\n" SRC+  s" main.f" NEST-WRITE ;

\ Guards a file's kind a load asks: KD2, after a missing k becomes a directory, and KD3, after that
\ directory becomes a regular file, print a cold build's answer.
: KIND-CASE ( CMD:command -- CMD:command )
   WRITE-KIND
   s" KD" WHICH!  s" main.f" s" prog-k" NEST-BUILD 0= if exit then
   s" prog-k" s\" 0\n" s" the program prints whether k is a directory" NEST-RUN
   s" k" NEST-PATH$ FS:MAKE-DIR
   s" KD2" WHICH!  s" main.f" s" prog-k" NEST-BUILD 0= if exit then
   s" prog-k" s\" -1\n" s" the program prints whether k is a directory" NEST-RUN
   s" k" NEST-PATH$ FS:REMOVE-DIR  s" 1" SRC+  s" k" NEST-WRITE
   s" KD3" WHICH!  s" main.f" s" prog-k" NEST-BUILD 0= if exit then
   s" prog-k" s\" 0\n" s" the program prints whether k is a directory" NEST-RUN ;

\ The arena sequence: kt.f keeps an arena in A, kb.f a block of one, ht.f a heap record in Q, and
\ da.f drops an arena; each x-main.f requires x.f, and o.f none of them.
: WRITE-ARENA ( -- )
   L$ s" arena" NEST-DIR FS:JOIN-PATH NEST-DIR-U !  NEST-DIR NEST-DIR-U @ FS:MAKE-DIR
   s\" require src/lib/arena.f\npackage FILE-OBJECTS-ARENA\npublic\n" SRC+
   s\" TYPED-VARIABLE A ptr ARENA:arena\nARENA:NEW A !\n;package\n" SRC+  s" kt.f" NEST-WRITE
   s" kt.f" REQUIRE+
   s\" : MAIN ( -- ) FILE-OBJECTS-ARENA:A @ NULL-PTR - 0 = bool>n . ;\n" SRC+
   s" kt-main.f" NEST-WRITE
   s\" require src/lib/arena.f\npackage FILE-OBJECTS-ARENA\npublic\n" SRC+
   s\" PTR-VARIABLE A\nARENA:NEW 8 ARENA:ALLOC drop A !\n;package\n" SRC+  s" kb.f" NEST-WRITE
   s" kb.f" REQUIRE+  s\" : KEPT ( -- ptr u8 ) FILE-OBJECTS-ARENA:A @ ;\n" SRC+
   s\" : MAIN ( -- ) KEPT NULL-PTR - 0 = bool>n . ;\n" SRC+  s" kb-main.f" NEST-WRITE
   s\" require src/lib/arena.f\npackage FILE-OBJECTS-DROPPED\nARENA:NEW drop\n;package\n" SRC+
   s" da.f" NEST-WRITE
   s" da.f" REQUIRE+  s\" : MAIN ( -- ) 0 . ;\n" SRC+  s" da-main.f" NEST-WRITE
   s\" package FILE-OBJECTS-HEAP\nHEAP H n\npublic\nTYPED-VARIABLE Q ptr n\nH-NEW Q !\n;package\n"
   SRC+  s" ht.f" NEST-WRITE
   s" ht.f" REQUIRE+
   s\" : MAIN ( -- ) FILE-OBJECTS-HEAP:Q @ NULL-PTR - 0 = bool>n . ;\n" SRC+
   s" ht-main.f" NEST-WRITE
   s\" : MAIN ( -- ) ;\n" SRC+  s" o.f" NEST-WRITE ;

\ The main's build, refused at link naming the fixture and the cell that holds the address. Answers
\ its exit code.
: ARENA-REFUSED ( CMD:command ptr u8 n ptr u8 n ptr u8 n -- CMD:command n )
   {: main:ptr mainu:n f:ptr fu:n cell:ptr cellu:n :}
   main mainu s" prog-ar" NEST-RC {: rc:n :}
   CMD:ERR$ f fu NEST-PATH$ CONTAINS? {: named:bool :}
   CMD:ERR$ cell cellu CONTAINS? {: celled:bool :}
   CMD:ERR$ s" which an object cannot name" CONTAINS? {: unnamed:bool :}
   named celled and unnamed and  rc 0<> and 0= if rc SHOW then
   s" the build fails" LABEL  rc 0<> TTRUE
   s" the refusal names the fixture" LABEL  named TTRUE
   s" the refusal names the cell" LABEL  celled TTRUE
   s" the refusal says no object can name the address" LABEL  unnamed TTRUE
   rc ;

: ARENA-OTHER ( CMD:command -- CMD:command ) s" o.f" s" prog-ao" NEST-BUILD drop ;

: ARENA-KT ( CMD:command -- CMD:command )
   s" AR" WHICH!  s" kt-main.f" s" kt.f" s"  in A +0 holds" ARENA-REFUSED {: cold:n :}
   s" AR2" WHICH!  ARENA-OTHER
   s" AR3" WHICH!  s" kt-main.f" s" kt.f" s"  in A +0 holds" ARENA-REFUSED {: warm:n :}
   s" the build exits as AR does" LABEL  warm cold T= ;

: ARENA-KB ( CMD:command -- CMD:command )
   s" AR4" WHICH!  s" kb-main.f" s" prog-ar" NEST-BUILD drop
   s" prog-ar" s\" -1\n" s" the program reads A null" NEST-RUN
   s" AR5" WHICH!  ARENA-OTHER
   s" AR6" WHICH!  s" kb-main.f" s" prog-ar" NEST-BUILD drop
   s" prog-ar" s\" -1\n" s" the program reads A null" NEST-RUN ;

: ARENA-DA ( CMD:command -- CMD:command )
   s" AR7" WHICH!  s" da-main.f" s" prog-ar" NEST-BUILD drop
   s" prog-ar" s\" 0\n" s" the program prints 0" NEST-RUN
   s" AR8" WHICH!  ARENA-OTHER
   s" AR9" WHICH!  s" da-main.f" s" prog-ar" NEST-BUILD drop
   s" prog-ar" s\" 0\n" s" the program prints 0" NEST-RUN ;

: ARENA-HT ( CMD:command -- CMD:command )
   s" AR10" WHICH!  s" ht-main.f" s" ht.f" s"  in Q +0 holds" ARENA-REFUSED {: cold:n :}
   s" AR11" WHICH!  ARENA-OTHER
   s" AR12" WHICH!  s" ht-main.f" s" ht.f" s"  in Q +0 holds" ARENA-REFUSED {: warm:n :}
   s" the build exits as AR10 does" LABEL  warm cold T= ;

\ Guards that a build gives a program no address of memory a top level mapped.
: ARENA-CASE ( CMD:command -- CMD:command )
   WRITE-ARENA  ARENA-KT ARENA-KB ARENA-DA ARENA-HT ;

\ The route sequence's f.f: its top level makes D, a device whose emit and write add their byte
\ counts to N, the output, types to it and leaves it there, and stores 7 into the user row U.
\ main.f's MAIN prints U, N, how making D the output ends, and N. genio-core.f needs --repl.
: WRITE-ROUTE ( -- )
   L$ s" route" NEST-DIR FS:JOIN-PATH NEST-DIR-U !  NEST-DIR NEST-DIR-U @ FS:MAKE-DIR
   s\" require src/lib/genio-core.f\nrequire src/lib/task.f\npackage FILE-OBJECTS-ROUTE\n" SRC+
   s\" public variable N  TASK:+USER U n  TYPED-VARIABLE D GENIO:device\nprivate\n" SRC+
   s\" : NOTHING ( -- ) ;\n: ADD-CHAR ( n -- ) drop 1 N +! ;\n: NO-KEY ( -- n ) 0 ;\n" SRC+
   s\" : NOT-READY ( -- bool ) false ;\n: NO-READ ( ptr u8 n -- n ) {: a:ptr u:n :} 0 ;\n" SRC+
   s\" : ADD-BYTES ( ptr u8 n -- ) {: a:ptr u:n :} u N +! ;\n: BUILD ( -- GENIO:device )\n" SRC+
   s\"    [: ADD-CHAR ;] [: NO-KEY ;] [: NOT-READY ;] [: NO-READ ;]\n" SRC+
   s\"    [: ADD-BYTES ;] [: NO-READ ;] [: NOTHING ;] [: NOTHING ;] 0 GENIO:DEVICE ;\n" SRC+
   s\" BUILD D !  D @ GENIO:OUTPUT!  s\" xyz\" type  7 U !\n;package\n" SRC+  s" f.f" NEST-WRITE
   s" f.f" REQUIRE+
   s\" : TO-D ( -- ) FILE-OBJECTS-ROUTE:D @ GENIO:OUTPUT!  s\" abc\" type ;\n" SRC+
   s\" : MAIN ( -- )\n   .\" seen\" cr  FILE-OBJECTS-ROUTE:U @ .  FILE-OBJECTS-ROUTE:N @ .\n" SRC+
   s\"    [: TO-D ;] catch {: code:n :}  GENIO:TERMINAL GENIO:OUTPUT!\n" SRC+
   s\"    code E-GENIO-STATE = if .\" refused\" cr else code . then\n" SRC+
   s\"    FILE-OBJECTS-ROUTE:N @ . ;\n" SRC+  s" main.f" NEST-WRITE ;

\ Guards a program's start: it starts on the terminal with fresh user rows whatever a file's top level left.
: ROUTE-CASE ( CMD:command -- CMD:command )
   WRITE-ROUTE
   s" RT" WHICH!  s" main.f" NEST-PATH$ REPL-BUILD 0= if exit then
   s" prog" s\" seen\n0\n3\nrefused\n3\n" s" D got the build's bytes; the program starts on the terminal, U 0"
      NEST-RUN
   s" RM" WHICH!  s" test/fixtures/file-objects/bare-main.f" FIXTURE-BUILD 0= if exit then
   s" RT2" WHICH!  s" main.f" NEST-PATH$ REPL-BUILD 0= if exit then
   s" prog" s\" seen\n0\n3\nrefused\n3\n" s" D got the build's bytes; the program starts on the terminal, U 0"
      NEST-RUN ;

\ The device sequence's f.f: K is the byte count a read of /dev/null answers, a file whose change time
\ does not track its bytes.
: WRITE-DEVICE ( -- )
   L$ s" device" NEST-DIR FS:JOIN-PATH NEST-DIR-U !  NEST-DIR NEST-DIR-U @ FS:MAKE-DIR
   s\" require src/lib/fs.f\npackage FILE-OBJECTS-DEVICE\n1 BUFFER: B\n" SRC+
   s\" public s\" /dev/null\" B 1 FS:READ-ALL constant K ;package\n" SRC+  s" f.f" NEST-WRITE
   s" f.f" REQUIRE+  s\" : MAIN ( -- ) FILE-OBJECTS-DEVICE:K . ;\n" SRC+  s" main.f" NEST-WRITE ;

\ Guards a read of a file no stamp covers: DV2, with nothing changed, still compiles f.f.
: DEVICE-CASE ( CMD:command -- CMD:command )
   WRITE-DEVICE
   s" DV" WHICH!  s" main.f" s" prog-v" NEST-BUILD 0= if exit then
   s" prog-v" s\" 0\n" s" the program prints /dev/null's byte count" NEST-RUN  L$ MARK
   s" DV2" WHICH!  s" main.f" s" prog-v" NEST-BUILD 0= if exit then
   L$ REWRITTEN {: n:n :}
   s" build two rewrites an object" LABEL  n 0 > TTRUE
   s" prog-v" s\" 0\n" s" the program prints /dev/null's byte count" NEST-RUN ;

\ The spelled sequence's turn.f: the record turn, with a third field when the
\ flag is true, and box<a>.
: WRITE-TURN ( bool -- )
   {: wide:bool :}
   s\" STRUCTURE turn DERIVE addr\n   FIELD x n\n   FIELD y n\n" SRC+
   wide if s\"    FIELD z n\n" SRC+ then
   s\" ;STRUCTURE\nSTRUCTURE box<a> DERIVE addr\n" SRC+
   s\"    FIELD item a\n   FIELD rest n\n;STRUCTURE\n" SRC+
   s" : MAKE-BOX ( -- box<turn> ) 1 2 " SRC+  wide if s" 9 " SRC+ then
   s\" TURN:MAKE 3 BOX:MAKE ;\n" SRC+  s" turn.f" NEST-WRITE ;

\ stride.f and effect.f spell turn only inside box<turn>: a typed buffer's
\ stride and an effect.
: WRITE-SPELLED ( -- )
   L$ s" spelled" NEST-DIR FS:JOIN-PATH NEST-DIR-U !
   NEST-DIR NEST-DIR-U @ FS:MAKE-DIR
   false WRITE-TURN
   s\" 4 TYPED-BUFFER BT box<turn>\n" SRC+
   s\" : B-STEP ( -- n ) 1 BT NULL-PTR -  0 BT NULL-PTR -  - ;\n" SRC+
   s" stride.f" NEST-WRITE
   s\" : W ( box<turn> -- n ) drop 7 ;\n" SRC+  s" effect.f" NEST-WRITE
   s" turn.f" REQUIRE+  s" stride.f" REQUIRE+  s" effect.f" REQUIRE+
   s\" : MAIN ( -- ) B-STEP . 11 MAKE-BOX W . . ;\n" SRC+
   s" main.f" NEST-WRITE ;

\ Guards a type named only inside `<...>`: after turn gains a field, J2 compiles
\ stride.f and effect.f, and its program prints a cold build's stride, W's 7 and
\ the 11 under the box W drops.
: SPELLED-CASE ( CMD:command -- CMD:command )
   WRITE-SPELLED
   s" J" WHICH!  s" main.f" NEST-PATH$ FIXTURE-BUILD 0= if exit then
   SPELLED-STRIDE KEEP  SPELLED-EFFECT KEEP
   true WRITE-TURN
   s" J2" WHICH!  s" main.f" NEST-PATH$ FIXTURE-BUILD 0= if exit then
   s" the build compiles stride.f" SPELLED-STRIDE COMPILED
   s" the build compiles effect.f" SPELLED-EFFECT COMPILED
   s\" 32\n7\n11\n" s" the program prints a cold build's numbers" PRINTS-RUN ;

\ The packaged sequence's t.f: package TURNS's record turn, with a third field
\ when the flag is true, and box<a>.
: WRITE-TURNS ( bool -- )
   {: wide:bool :}
   s\" package TURNS public\n" SRC+
   s\" STRUCTURE turn DERIVE addr\n   FIELD x n\n   FIELD y n\n" SRC+
   wide if s\"    FIELD z n\n" SRC+ then
   s\" ;STRUCTURE\nSTRUCTURE box<a> DERIVE addr\n" SRC+
   s\"    FIELD item a\n   FIELD rest n\n;STRUCTURE\n" SRC+
   s" : MAKE-TURN ( -- turn ) 1 2 " SRC+  wide if s" 9 " SRC+ then
   s\" TURNS-TURN:MAKE ;\n" SRC+
   s" : MAKE-BOX ( -- box<turn> ) 1 2 " SRC+  wide if s" 9 " SRC+ then
   s\" TURNS-TURN:MAKE 3 TURNS-BOX:MAKE ;\n;package\n" SRC+
   s" t.f" NEST-WRITE ;

\ bare.f spells turn bare under `using TURNS`, boxed.f only inside
\ TURNS:box<TURNS:turn>: each a typed buffer's stride and an effect.
: WRITE-PACKAGED ( -- )
   L$ s" packaged" NEST-DIR FS:JOIN-PATH NEST-DIR-U !
   NEST-DIR NEST-DIR-U @ FS:MAKE-DIR
   false WRITE-TURNS
   s\" using TURNS\n4 TYPED-BUFFER BT turn\n" SRC+
   s\" : B-STEP ( -- n ) 1 BT NULL-PTR -  0 BT NULL-PTR -  - ;\n" SRC+
   s\" : W ( turn -- n ) drop 7 ;\n;using\n" SRC+  s" bare.f" NEST-WRITE
   s\" 4 TYPED-BUFFER UT TURNS:box<TURNS:turn>\n" SRC+
   s\" : U-STEP ( -- n ) 1 UT NULL-PTR -  0 UT NULL-PTR -  - ;\n" SRC+
   s\" : V ( TURNS:box<TURNS:turn> -- n ) drop 8 ;\n" SRC+
   s" boxed.f" NEST-WRITE
   s" t.f" REQUIRE+  s" bare.f" REQUIRE+  s" boxed.f" REQUIRE+
   s" : MAIN ( -- ) B-STEP . U-STEP . 11 TURNS:MAKE-TURN W . . " SRC+
   s\" 12 TURNS:MAKE-BOX V . . ;\n" SRC+
   s" main.f" NEST-WRITE ;

\ Guards a packaged type its user names by its tail: after turn gains a field,
\ PK2 compiles bare.f and boxed.f, and its program prints a cold build's
\ strides, W's 7, V's 8 and the numbers under them.
: PACKAGED-CASE ( CMD:command -- CMD:command )
   WRITE-PACKAGED
   s" PK" WHICH!  s" main.f" NEST-PATH$ FIXTURE-BUILD 0= if exit then
   PACKAGED-BARE KEEP  PACKAGED-BOXED KEEP
   true WRITE-TURNS
   s" PK2" WHICH!  s" main.f" NEST-PATH$ FIXTURE-BUILD 0= if exit then
   s" the build compiles bare.f" PACKAGED-BARE COMPILED
   s" the build compiles boxed.f" PACKAGED-BOXED COMPILED
   s\" 24\n32\n7\n11\n8\n12\n"
   s" the program prints a cold build's numbers" PRINTS-RUN ;

\ The evaluated sequence's a.f: package P's private Z, and a private K when the
\ flag is true.
: WRITE-P ( bool -- )
   {: own:bool :}
   s\" package P\nprivate\n: Z ( -- n ) 0 ;\n" SRC+
   own if s\" : K ( -- n ) 2 ;\n" SRC+ then
   s\" ;package\n" SRC+  s" a.f" NEST-WRITE ;

\ main.f spells K only inside the text DEF-G evaluates as G's body.
: WRITE-EVALUATED ( -- )
   L$ s" evaluated" NEST-DIR FS:JOIN-PATH NEST-DIR-U !
   NEST-DIR NEST-DIR-U @ FS:MAKE-DIR
   s\" : K ( -- n ) 1 ;\n" SRC+  s" k.f" NEST-WRITE
   s\" : DEF-G ( -- ) s\" : G ( -- n ) K ;\" evaluate-closed ;\n" SRC+
   s" gen.f" NEST-WRITE
   false WRITE-P
   s" k.f" REQUIRE+  s" gen.f" REQUIRE+  s" a.f" REQUIRE+
   s\" package P\npublic\nDEF-G\n;package\n" SRC+
   s\" : MAIN ( -- ) P:G . ;\n" SRC+  s" main.f" NEST-WRITE ;

\ Guards a name only an evaluated text spells: after a.f gains P's private K,
\ EV2 compiles main.f's last part, whose G now binds it, and its program
\ prints a cold build's 2.
: EVALUATED-CASE ( CMD:command -- CMD:command )
   WRITE-EVALUATED
   s" EV" WHICH!  s" main.f" NEST-PATH$ FIXTURE-BUILD 0= if exit then
   EVALUATED-LAST KEEP
   true WRITE-P
   s" EV2" WHICH!  s" main.f" NEST-PATH$ FIXTURE-BUILD 0= if exit then
   s" the build compiles main.f's last part" EVALUATED-LAST COMPILED
   s\" 2\n" s" the program prints a cold build's number" PRINTS-RUN ;

\ main.f's last part spells K in package P, which only the text OPEN-P
\ evaluates opens.
: WRITE-OPENED ( -- )
   L$ s" opened" NEST-DIR FS:JOIN-PATH NEST-DIR-U !
   NEST-DIR NEST-DIR-U @ FS:MAKE-DIR
   s\" : K ( -- n ) 1 ;\n" SRC+  s" k.f" NEST-WRITE
   s\" : OPEN-P ( -- ) s\" package P\" evaluate-closed ;\n" SRC+
   s" gen.f" NEST-WRITE
   false WRITE-P
   s" k.f" REQUIRE+  s" gen.f" REQUIRE+  s" a.f" REQUIRE+
   s\" OPEN-P\npublic\n: G ( -- n ) K ;\n;package\n" SRC+
   s\" : MAIN ( -- ) P:G . ;\n" SRC+  s" main.f" NEST-WRITE ;

\ Guards a source name bound in a package only an evaluated text opens: after
\ a.f gains P's private K, OP2 compiles main.f's last part, whose G now binds
\ it, and its program prints a cold build's 2.
: OPENED-CASE ( CMD:command -- CMD:command )
   WRITE-OPENED
   s" OP" WHICH!  s" main.f" NEST-PATH$ FIXTURE-BUILD 0= if exit then
   OPENED-LAST KEEP
   true WRITE-P
   s" OP2" WHICH!  s" main.f" NEST-PATH$ FIXTURE-BUILD 0= if exit then
   s" the build compiles main.f's last part" OPENED-LAST COMPILED
   s\" 2\n" s" the program prints a cold build's number" PRINTS-RUN ;

\ A directory of L's of that name, holding the cast sequence's a.f: package
\ FILE-OBJECTS-CAST's private generic cast >ELEM and its public generic AT.
: WRITE-CAST ( ptr u8 n -- )
   {: a:ptr u:n :}
   L$ a u NEST-DIR FS:JOIN-PATH NEST-DIR-U !
   NEST-DIR NEST-DIR-U @ FS:MAKE-DIR
   s\" package FILE-OBJECTS-CAST\nCAST: >ELEM ( ptr u8 -- ptr t )\n" SRC+
   s\" public\n: AT ( ptr u8 -- ptr a ) >ELEM ;\n;package\n" SRC+
   s" a.f" NEST-WRITE ;

\ b.f calls AT at elem: a record, or a linear type when the flag is true.
: WRITE-CAST-B ( bool -- )
   {: lin:bool :}
   s" a.f" REQUIRE+
   lin if s\" DEFLINEAR elem\n" SRC+ else
      s\" STRUCTURE elem DERIVE addr FIELD x n FIELD y n ;STRUCTURE\n" SRC+
   then
   s\" 32 BUFFER: CAST-BUF\n" SRC+
   s\" : SLOT ( -- ptr elem ) CAST-BUF FILE-OBJECTS-CAST:AT ;\n" SRC+
   lin if s\" : MAIN ( -- ) ;\n" SRC+ else
      s\" : MAIN ( -- ) 5 SLOT ELEM:Y !  SLOT ELEM:Y @ . ;\n" SRC+
   then
   s" b.f" NEST-WRITE ;

\ A build of b.f the cast's restriction refuses; answers its exit code. The
\ build prints E-CAST-LINEAR's reason at the call, not the code.
: CAST-REFUSED ( CMD:command -- CMD:command n )
   s" b.f" NEST-PATH$ FAILING-BUILD {: rc:n :}
   CMD:ERR$ s" at 'FILE-OBJECTS-CAST:AT' a cast's stored type would hold a linear value"
   CONTAINS? {: said:bool :}
   rc 0 =  said 0=  or if rc SHOW then
   s" the build exits nonzero" LABEL  rc 0<> TTRUE
   s" the build refuses AT at a linear type" LABEL  said TTRUE
   rc ;

\ Guards the restriction a linked object's AT carries: CA2 links a.f and
\ refuses b.f's SLOT as CA3, a cold build of the same files, does.
: CAST-CASE ( CMD:command -- CMD:command )
   s" cast" WRITE-CAST  false WRITE-CAST-B
   s" CA" WHICH!  s" b.f" NEST-PATH$ FIXTURE-BUILD 0= if exit then
   s\" 5\n" s" the program prints the field it stored" PRINTS-RUN
   CAST-A KEEP
   true WRITE-CAST-B
   s" CA2" WHICH!  CAST-REFUSED {: warm:n :}
   s" the build links a.f" CAST-A LINKED
   s" cast-cold" WRITE-CAST  true WRITE-CAST-B
   s" CA3" WHICH!  CAST-REFUSED {: cold:n :}
   s" CA2 exits as the build does" LABEL  warm cold T= ;

\ A Wasm build of bare-main.f in a fresh hb, since the process fixes the build's target; its module never runs.
: WASM-BUILD ( CMD:command -- CMD:command )
   TOOL-ARGS  s" --target" CMD:ARG+  s" wasm" CMD:ARG+
   s" test/fixtures/file-objects/bare-main.f" TOOL-RUN {: rc:n :}
   rc 0<> if rc SHOW then
   s" the build exits 0" LABEL  rc 0 T= ;

\ Guards a cross build's objects, which hold each definition's target and host copies: a second Wasm build
\ links every file and writes the first's module.
: CROSS-CASE ( CMD:command -- CMD:command )
   s" WASM" WHICH!  WASM-BUILD
   L$ s" prog" IN$ PROG-1 PROG-CAP FS:READ-ALL {: u1:n :}
   F$ MARK
   s" WASM2" WHICH!  WASM-BUILD
   s" the build rewrites no object" LABEL  F$ REWRITTEN 0 T=
   L$ s" prog" IN$ PROG-2 PROG-CAP FS:READ-ALL {: u2:n :}
   s" the module is the first build's" LABEL  PROG-1 u1 PROG-2 u2 STR= TTRUE ;

\ A Wasm build exits 0 where a target word is only named and is refused where top level runs it, a linked file's
\ by its name; two builds, since a refused build ends its hb.
: TARGET-CASE ( CMD:command -- CMD:command )
   s" TN" WHICH!
   TOOL-ARGS  s" --target" CMD:ARG+  s" wasm" CMD:ARG+
   s" test/fixtures/file-objects/target-named.f" TOOL-RUN {: named:n :}
   named 0<> if named SHOW then
   s" the build exits 0" LABEL  named 0 T=
   s" TR" WHICH!
   TOOL-ARGS  s" --target" CMD:ARG+  s" wasm" CMD:ARG+
   s" test/fixtures/file-objects/target-run.f" TOOL-RUN {: rc:n :}
   CMD:ERR$ s" E-TARGET-WORD habu: " CONTAINS? {: classed:bool :}
   CMD:ERR$ s" test/fixtures/file-objects/target-run.f:8: 'FO-TAIL' is a target word"
      CONTAINS? {: placed:bool :}
   classed placed and  rc 0<> and 0= if rc SHOW then
   s" the build fails" LABEL  rc 0<> TTRUE
   s" the refusal is E-TARGET-WORD" LABEL  classed TTRUE
   s" the refusal names linked FO-TAIL on target-run.f's line 8" LABEL  placed TTRUE ;

: CHECKS ( -- )
   [: COLD ;] CMD:WITH-COMMAND
   CHECK-COLD
   [: WARM-A WARM-CUT WARM-GROW WARM-B WARM-E LOG-CASE PATH-CASE STALE-CASE HIDDEN-CASE SEALED-CASE
      STORED-CASE RETIRED-CASE ORDER-CASE PART-LINK-CASE PIECE-CASE SEAL-CASE SQUAT-CASE CAUGHT-CASE
      CTOR-CASE EXPORT-CASE
      NEST-CASE KEPT-CASE SAME-CASE RETIRE-CASE CLAUSE-CASE READ-CASE LIST-CASE LINK-CASE TASK-CASE
      SAMEFILE-CASE ANCHORED-CASE SPAWN-CASE ZIP-CASE JSONL-CASE KIND-CASE ARENA-CASE ROUTE-CASE
      DEVICE-CASE SPELLED-CASE PACKAGED-CASE EVALUATED-CASE OPENED-CASE CAST-CASE
      CROSS-CASE TARGET-CASE ;]
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
