\ file-objects.f - builds of the files in test/fixtures/file-objects/, each in a fresh hb: an object must be
\ a function of the files, not of the process.

require src/lib/error.f
require src/lib/string.f
require src/lib/string-option.f
require src/lib/fmt.f
require src/lib/test/assert.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/fs-identity.f
require src/lib/fs-list.f
require src/lib/fd.f
require src/lib/ffi-abi.f
require src/lib/codesign.f
require src/lib/json.f
require src/lib/process-command.f
require src/lib/pty.f
require src/lib/engine-candidate.f
require src/compiler/target/model.f
require tools/hb-build-core.f
require src/lib/process.f
require src/lib/le.f
require src/lib/json/read.f

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
33 constant PT-DEF                       \ pt-def.f's
34 constant PT-USE                       \ and pt-use.f's
35 constant PFIELD-PF                    \ the +FIELD sequence's pf.f's
36 constant PFIELD-MAIN                  \ and pf-main.f's
37 constant SPELLED-BLOB                 \ the spelled sequence's blob.f's
38 constant MUTUAL-TYPES                 \ the MUTUAL sequence's mt.f's
39 constant MUTUAL-MAIN                  \ and mt-main.f's
40 constant FOLDED-LAST                  \ the folded sequence's main.f's last part's
41 constant SCOPE-OPEN                   \ scope-open.f's, which leaves its package open,
42 constant SCOPE-USE                    \ scope-use.f's, which names its SEVEN,
43 constant INSIDE-I                     \ and inside-i.f's, which loads inside a package or alone
44 constant AIO-OBJ                      \ src/lib/aio.f's, which kept-aio.f requires
45 constant MOVED-CALL                   \ the moved sequence's four.f's +3 part's, which calls W,
46 constant MOVED-LAST                   \ and its last part's
47 constant TAILS                        \ TAIL$'s tails
52 constant FILES-C                     \ build C's objects
256 constant HEADER-BYTES                \ an object's header, INPUT right after it (linker.f)
240 constant PIECES-FIELD               \ the header's PIECES offset (linker.f WRITE-OBJECT)
104 constant FILES-LEN-FIELD             \ the header's FILES length offset (linker.f WRITE-OBJECT)
32 constant FILES-ROW                    \ a FILES row's bytes (linker.f FILES-ROW-BYTES)
24 constant STAMP-BYTES                  \ the stamp an object's INPUT starts with
16384 constant CAP                       \ an object's bytes
16384 constant STATE-CAP                 \ a build's output before its last line

PATH-CAP BUFFER: OUT-A   variable OUT-A-U
PATH-CAP BUFFER: OUT-B   variable OUT-B-U
PATH-CAP BUFFER: OUT-C   variable OUT-C-U
PATH-CAP BUFFER: OUT-D   variable OUT-D-U
PATH-CAP BUFFER: OUT-E   variable OUT-E-U
PATH-CAP BUFFER: OUT-L   variable OUT-L-U
variable TMP-N                           \ TOOL-RUN's runs; run n's TMPDIR is tmp-<n> in L's directory
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
PTY:SLAVE-PATH-CAP BUFFER: SLAVE
32 BUFFER: TIMES                        \ utimes' access and write times, each seconds then microseconds
STATE-CAP BUFFER: STATE-A  variable STATE-A-U
STATE-CAP BUFFER: STATE-B  variable STATE-B-U
STATE-CAP BUFFER: STATE-W  variable STATE-W-U  \ a warm build's, C's or D's
STATE-CAP BUFFER: STATE-E  variable STATE-E-U
STATE-CAP BUFFER: ERR-E    variable ERR-E-U    \ E's stderr
8 BUFFER: WHICH          variable WHICH-U  \ the build the labels name
96 BUFFER: TEXT-BUF
T-LABEL-CAP BUFFER: HELD-LABEL           \ a label FIXTURE-OBJECT keeps across FOUND?
128 BUFFER: LABEL-BUF
PATH-CAP 128 + BUFFER: REJECTION  variable REJECTION-U  \ the loader rejection REJECTED-BUILD expects
variable OBJ-N                          \ the walk's objects
variable TAIL-I                          \ the tail the walk looks for (TAIL$)
variable TAIL-N                          \ and the paths that end with it
variable MARK-S                          \ the newest object change time MARK saw, seconds
variable MARK-NS                         \ and nanoseconds
variable NEWER-N                         \ the objects REWRITTEN saw changed after it
variable LEFT-N                          \ the entries TOOL-RUN found in its TMPDIR
TAILS TYPED-BUFFER KEPT-SIZE n          \ by tail, the object's size and its change time's
TAILS TYPED-BUFFER KEPT-S n             \ seconds and nanoseconds, as KEEP read them; size -1
TAILS TYPED-BUFFER KEPT-NS n            \ if KEEP found none or never ran
TYPED-VARIABLE REPORT-CURSOR JSON:cursor
variable REPORT-TOTAL                   \ N's report's size.total,
variable REPORT-CODE                    \ its size.code,
variable REPORT-SUM                     \ the sum of its six size classes
variable REPORT-FILES                   \ and files_reused plus files_compiled
variable CLASS-N                        \ the class CLASS+ read

PROCESS-SYMBOLS
FUNCTION: UTIMES-CALL utimes ( ^u8 ^u8 -- i32 ) ;FUNCTION


: A$ ( -- str ) OUT-A OUT-A-U @ >str ;
: B$ ( -- str ) OUT-B OUT-B-U @ >str ;
: C$ ( -- str ) OUT-C OUT-C-U @ >str ;
: D$ ( -- str ) OUT-D OUT-D-U @ >str ;
: E$ ( -- str ) OUT-E OUT-E-U @ >str ;
: L$ ( -- str ) OUT-L OUT-L-U @ >str ;
: F$ ( -- str ) OUT-F OUT-F-U @ >str ;

: IN$ ( str str -- str ) {: out:str name:str :}
   out name JOINED FS:JOIN-PATH {: u:n :}
   JOINED u >str ;

: WHICH! ( str -- ) {: s:str :}
   s STR:DATA WHICH s STR:LEN BYTE-COPY  s STR:LEN WHICH-U ! ;

\ The build's name, ": " and the text, as the next label.
: LABEL-AS ( str str -- )
   {: w:str s:str :}
   w STR:LEN {: wu:n :}  s STR:LEN {: u:n :}
   w STR:DATA LABEL-BUF wu BYTE-COPY
   s" : " {: c:str :}  c STR:LEN {: cu:n :}
   c STR:DATA LABEL-BUF wu + cu BYTE-COPY
   s STR:DATA LABEL-BUF wu + cu + u BYTE-COPY
   LABEL-BUF wu cu + u + >str T-LABEL ;

: LABEL ( str -- ) {: s:str :}  WHICH WHICH-U @ >str s LABEL-AS ;

\ The text, another build's name and more text, as the next label.
: LABEL-NAMING ( str str str -- )
   {: a:str b:str c:str :}
   a STR:LEN {: au:n :}  b STR:LEN {: bu:n :}  c STR:LEN {: cu:n :}
   a STR:DATA TEXT-BUF au BYTE-COPY
   b STR:DATA TEXT-BUF au + bu BYTE-COPY
   c STR:DATA TEXT-BUF au + bu + cu BYTE-COPY
   TEXT-BUF au bu + cu + >str LABEL ;


\ A failed run shows its status and what it printed on both streams.
: SHOW ( CMD:command n -- CMD:command ) {: rc:n :}
   s" file-objects: exited " type rc .
   s" stdout: " type CMD:OUT$ type cr  s" stderr: " type CMD:ERR$ type cr ;

\ The length of the text before its last line.
: BEFORE-LAST ( str -- n ) {: s:str :}
   s STR:LEN 2 < if 0 exit then
   0 s STR:LEN 2 - ?do
      s STR:DATA i + c@ $0A = if i 1 + unloop exit then
   -1 +loop
   0 ;

\ build.f with the kernel object and the mode, if any, as its arguments, in the
\ out root; answers its exit code.
: BUILD-RUN ( CMD:command str str -- CMD:command n )
   {: out:str mode:str :}
   CMD:RESET
   s" --load" CMD:ARG+  s" test/fixtures/file-objects/build.f" CMD:ARG+
   s" --" CMD:ARG+  HB-BUILD-CORE:KERNEL$ CMD:ARG+
   mode STR:LEN 0<> if mode CMD:ARG+ then
   s" HABU_OUT" out CMD:ENV+
   ENGINE-CANDIDATE:PATH$ PROCESS:WAIT-FOREVER CMD:RUN-OUTCOME
   PROCESS:ENDING>RC RC>N ;

\ BUILD-RUN; dst gets the output before the given last line. Answers its
\ length, -1 if it did not exit 0 or overflowed dst.
: BUILD ( CMD:command str str str ^u8 -- CMD:command n )
   {: out:str mode:str last:str dst:^ :}
   out mode BUILD-RUN {: rc:n :}
   CMD:OUT$ last STR:ENDS-WITH? {: done:bool :}
   done  rc 0 =  and 0= if rc SHOW then
   s" the build's last line is the one expected" LABEL  done TTRUE
   s" the build exits 0" LABEL  rc 0 T=
   rc 0<> if -1 exit then
   CMD:OUT$ {: o:str :}
   o BEFORE-LAST {: u:n :}
   s" the build's output fits its buffer" LABEL  u STATE-CAP <= TTRUE
   u STATE-CAP > if -1 exit then
   o STR:DATA dst u BYTE-COPY  u ;

\ A's program, run in its own process: the boundary under test is the written file.
: RUN-PROG ( CMD:command str -- CMD:command ) {: out:str :}
   CMD:RESET
   out s" prog" IN$ PROCESS:WAIT-FOREVER CMD:RUN-OUTCOME PROCESS:ENDING>RC RC>N {: rc:n :}
   CMD:OUT$ s\" Hi\nok\n" STR:=  rc 0 =  and 0= if rc SHOW then
   s" the program prints Hi and ok" LABEL  CMD:OUT$ s\" Hi\nok\n" T$=
   s" the program exits 0" LABEL  rc 0 T= ;


\ The tail of object i's path: 0 to 6 A's files in load order, 7 pad.f's, then the named tails'.
: TAIL$ ( n -- str )
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
   i SPELLED-BLOB = if s" /spelled/blob.f.hfo" exit then
   i PACKAGED-BARE = if s" /packaged/bare.f.hfo" exit then
   i PACKAGED-BOXED = if s" /packaged/boxed.f.hfo" exit then
   i CAST-A = if s" /cast/a.f.hfo" exit then
   i EVALUATED-LAST = if s" /evaluated/main.f+3.hfo" exit then
   i OPENED-LAST = if s" /opened/main.f+3.hfo" exit then
   i CTOR-SUM = if s" /test/fixtures/file-objects/ctor-sum.f.hfo" exit then
   i EXPORT-MAIN = if s" /test/fixtures/file-objects/export-main.f.hfo" exit then
   i PT-DEF = if s" /test/fixtures/file-objects/pt-def.f.hfo" exit then
   i PT-USE = if s" /test/fixtures/file-objects/pt-use.f.hfo" exit then
   i PFIELD-PF = if s" /pfield/pf.f.hfo" exit then
   i PFIELD-MAIN = if s" /pfield/pf-main.f.hfo" exit then
   i MUTUAL-TYPES = if s" /mutual/mt.f.hfo" exit then
   i MUTUAL-MAIN = if s" /mutual/mt-main.f.hfo" exit then
   i FOLDED-LAST = if s" /folded/main.f+2.hfo" exit then
   i SCOPE-OPEN = if s" /test/fixtures/file-objects/scope-open.f.hfo" exit then
   i SCOPE-USE = if s" /test/fixtures/file-objects/scope-use.f.hfo" exit then
   i INSIDE-I = if s" /test/fixtures/file-objects/inside-i.f.hfo" exit then
   i AIO-OBJ = if s" /src/lib/aio.f.hfo" exit then
   i MOVED-CALL = if s" /moved/four.f+3.hfo" exit then
   i MOVED-LAST = if s" /moved/four.f+5.hfo" exit then
   s" /test/fixtures/file-objects/seal-second.f.hfo" ;

: ROOT! ( str -- ) {: out:str :}
   out s" obj" ROOT FS:JOIN-PATH ROOT-U ! ;

: OBJ-SEEN ( str -- ) s" .hfo" STR:ENDS-WITH? if 1 OBJ-N +! then ;

: TAIL-SEEN ( str -- )
   {: p:str :}
   p TAIL-I @ TAIL$ STR:ENDS-WITH? 0= if exit then
   1 TAIL-N +!  p STR:DATA FOUND p STR:LEN BYTE-COPY  p STR:LEN FOUND-U ! ;

: COUNT-WALK ( -- ) ROOT ROOT-U @ >str ['] OBJ-SEEN FS:WALK-FILES ;
: TAIL-WALK ( -- ) ROOT ROOT-U @ >str ['] TAIL-SEEN FS:WALK-FILES ;

\ A walk of a build that made no obj directory visits nothing: the missing root is E-FS-STAT.
: WALKED ( [ -- ] -- ) catch {: rc:n :}
   rc 0 =  rc E-FS-STAT =  or 0= if rc throw then ;

\ The objects under the out root's obj directory.
: OBJECTS ( str -- n )
   ROOT!  0 OBJ-N !  ['] COUNT-WALK WALKED  OBJ-N @ ;

\ Whether exactly one path under the out root's obj directory ends with tail i, as a test under
\ the build's name; FOUND$ is that path.
: FOUND? ( str str n -- bool )
   {: out:str name:str i:n :}
   out ROOT!  i TAIL-I !  0 TAIL-N !  0 FOUND-U !
   ['] TAIL-WALK WALKED
   TAIL-N @ 1 = {: one:bool :}
   name s" the build wrote that object" LABEL-AS  one TTRUE
   one ;

: FOUND$ ( -- str ) FOUND FOUND-U @ >str ;

\ Whether BYTES-A's first au bytes and BYTES-B's first bu have one length, one PIECES offset and
\ the same bytes from it on.
: FROM-PIECES? ( n n -- bool )
   {: au:n bu:n :}
   au bu <> if false exit then
   au HEADER-BYTES < if false exit then
   BYTES-A PIECES-FIELD + LE:U64@ {: at:n :}
   BYTES-B PIECES-FIELD + LE:U64@ at <> if false exit then
   at au > if false exit then
   BYTES-A at +  au at - >str  BYTES-B at +  au at - >str  STR:= ;

\ A's object of tail i equals the peer's from PIECES on. The head before PIECES is not compared:
\ it holds the pieces' times, which differ between out roots.
: SAME-OBJECT ( str str n -- )
   {: out:str peer:str i:n :}
   A$ s" A" i FOUND? 0= if exit then
   FOUND$ BYTES-A CAP FS:READ-ALL {: au:n :}
   out peer i FOUND? 0= if exit then
   FOUND$ BYTES-B CAP FS:READ-ALL {: bu:n :}
   s" each of A's objects has " peer s" 's bytes from PIECES on" LABEL-NAMING
   au bu FROM-PIECES? TTRUE ;

\ D's shifted build moves records, emissions, wordlist ids and data, so an absolute index or address in an
\ object would show as bytes different from A's; objects carry texts by value.
: SAME-AS-D ( -- ) FILES 0 ?do D$ s" D" i SAME-OBJECT loop ;
: SAME-AS-B ( -- ) FILES-B 0 ?do B$ s" B" i SAME-OBJECT loop ;

\ The file at the path, whole, in room its size takes from the arena.
: WHOLE-FILE ( ^ARENA:arena str -- str )
   {: a:^ s:str :}
   a s FS:FILE-SIZE ARENA:ALLOC {: buf:^ cap:n :}
   buf  s buf cap FS:READ-ALL >str ;

\ Whether the first out root's file holds the bytes of the second's.
: SAME-FILE? ( str str str str -- bool )
   {: a:str an:str b:str bn:str :}
   [: dup a an IN$ WHOLE-FILE
      swap b bn IN$ WHOLE-FILE STR:= ;] ARENA:WITH ;

\ EXPECT: the regular file's stamp, its change time seconds, its type over its change time nanoseconds
\ and its size, each low byte first.
: EXPECT! ( str -- )
   FS:FILE-META {: size:n ms:n mns:n cs:n cns:n :}
   cs EXPECT LE:U64!  cns FS:S-IFREG 32 lshift or EXPECT 8 + LE:U64!  size EXPECT 16 + LE:U64! ;

\ Build C's objects: each sequence's, the twins', and count.f's and shape.f's, which A also
\ writes; a rejected file writes none, and count.f's holds its INPUT.
: CHECK-STOP ( -- )
   s" C" WHICH!
   s" the build wrote fifty-two objects" LABEL  C$ OBJECTS FILES-C T=
   C$ s" C" 0 SAME-OBJECT  C$ s" C" 1 SAME-OBJECT
   C$ s" C" 0 FOUND? 0= if exit then
   FOUND$ BYTES-B CAP FS:READ-ALL {: u:n :}
   s" test/fixtures/file-objects/count.f" EXPECT!
   u HEADER-BYTES - 0 max STAMP-BYTES min {: v:n :}
   s" count.f's object holds its stamp" LABEL
   BYTES-B HEADER-BYTES + v >str EXPECT STAMP-BYTES >str T$= ;

\ A file compiles when it is edited, when what its object names in another file changes, when an
\ earlier file that defines a public name it spells, or a private name of a package it opens, is
\ edited, when an earlier file lays a string its object lays, when it is loaded without the
\ files its object names, when files that
\ stored into a cell it read load in another order, or when its records do not fit the dictionary;
\ any other file links, a compile after it included.
: CHECK-EDIT ( -- )
   s" C" WHICH!
   s" the mixed builds compile 0, 1 and 0 files" LABEL
   STATE-W STATE-W-U @ >str s\" mixed: compiled 0 1 0\n" STR:CONTAINS? TTRUE
   s" the edit builds compile 2, 2 and 0 files" LABEL
   STATE-W STATE-W-U @ >str s\" edit: compiled 2 2 0\n" STR:CONTAINS? TTRUE
   s" the tail builds compile 2, 1, 0 and 1 files" LABEL
   STATE-W STATE-W-U @ >str s\" tail: compiled 2 1 0 1\n" STR:CONTAINS? TTRUE
   s" the share builds compile 2, 2 and 0 files" LABEL
   STATE-W STATE-W-U @ >str s\" share: compiled 2 2 0\n" STR:CONTAINS? TTRUE
   s" the laid builds compile 2, 2 and 0 files" LABEL
   STATE-W STATE-W-U @ >str s\" laid: compiled 2 2 0\n" STR:CONTAINS? TTRUE
   s" the private builds compile 2, 1 and 0 files" LABEL
   STATE-W STATE-W-U @ >str s\" private: compiled 2 1 0\n" STR:CONTAINS? TTRUE
   s" the parts builds compile 3, 2 and 0 files" LABEL
   STATE-W STATE-W-U @ >str s\" parts: compiled 3 2 0\n" STR:CONTAINS? TTRUE
   s" the twin builds compile 1, 1, 0 and 0 files" LABEL
   STATE-W STATE-W-U @ >str s\" twins: compiled 1 1 0 0\n" STR:CONTAINS? TTRUE
   s" the order builds compile 4, 3, 0 and 3 files" LABEL
   STATE-W STATE-W-U @ >str s\" order: compiled 4 3 0 3\n" STR:CONTAINS? TTRUE
   s" the build after the body's caught text that left a record is rejected E-FAILED" LABEL
   STATE-W STATE-W-U @ >str s\" none: compiled -9690\n" STR:CONTAINS? TTRUE
   s" the words builds compile 5, 4, 0 and 2 files, then stop 70 at c.f's A2" LABEL
   STATE-W STATE-W-U @ >str s\" words: compiled 5 4 0 2 70\n" STR:CONTAINS? TTRUE
   s" the ran builds compile 4, 1, 3, 0, 3 and 0 files" LABEL
   STATE-W STATE-W-U @ >str s\" ran: compiled 4 1 3 0 3 0\n" STR:CONTAINS? TTRUE
   s" the stores builds compile 4, 1, 2, 2, 2, 0 and 2 files" LABEL
   STATE-W STATE-W-U @ >str s\" stores: compiled 4 1 2 2 2 0 2\n" STR:CONTAINS? TTRUE
   s" the gain builds compile 3, 2, 2 and 0 files" LABEL
   STATE-W STATE-W-U @ >str s\" gain: compiled 3 2 2 0\n" STR:CONTAINS? TTRUE
   s" the own builds compile 2, 0, 2 and 0 files, the edited quotation's Q as a cold build's" LABEL
   STATE-W STATE-W-U @ >str s\" own: compiled 2 0 2 0 q 42 42 43 43 cold 2 q 43\n" STR:CONTAINS? TTRUE
   s" the through builds compile 4, 0, 3 and 0 files, the edited quotation's Q as a cold build's" LABEL
   STATE-W STATE-W-U @ >str s\" through: compiled 4 0 3 0 q 42 42 43 43 cold 4 q 43\n" STR:CONTAINS? TTRUE
   s" the full builds compile 2 and 0 files, then are rejected twice with 90 and the record a cold build prints" LABEL
   STATE-W STATE-W-U @ >str s\" full: compiled 2 0 90 90 cold 90 same record -1\n" STR:CONTAINS? TTRUE ;

\ A's map lists RUN as a word main.f loaded; a map lists only the code its
\ program holds.
: CHECK-MAP ( -- )
   s" A" WHICH!
   [: A$ s" prog.map" IN$ WHOLE-FILE
      s" the map lists RUN as main.f's" LABEL
      s\"  RUN word loaded test/fixtures/file-objects/main.f\n" STR:CONTAINS? TTRUE ;]
   ARENA:WITH ;

\ E's build, a fresh process as every build is: E2's process then holds E's
\ objects alone, never the checker memory of the process that compiled them.
: COLD-E ( CMD:command -- CMD:command )
   s" E" WHICH!  E$ s" facts" s\" facts: compiled 2\n" STATE-E BUILD 0 max STATE-E-U !
   CMD:ERR$ {: e:str :}
   s" the build's stderr fits its buffer" LABEL  e STR:LEN STATE-CAP <= TTRUE
   e STR:LEN STATE-CAP min {: u:n :}
   e STR:DATA ERR-E u BYTE-COPY  u ERR-E-U ! ;

\ Whether E's output before its last line holds the line, newline-ended, as a test.
: E-LINE ( str -- )
   {: s:str :}
   s" the build prints " s STR:DATA s STR:LEN 1 - >str s" " LABEL-NAMING
   STATE-E STATE-E-U @ >str s STR:CONTAINS? TTRUE ;

\ The callers compile and run; the misuse and the dead code are rejected, by TAKE and FAIL.
: CHECK-E ( -- )
   s" E" WHICH!
   s\" after: 0\n" E-LINE  s\" wrap: 0\n" E-LINE  s\" Hi\n" E-LINE  s\" after ran\n" E-LINE
   s\" misuse: 70\n" E-LINE  s\" dead: 70\n" E-LINE
   s" the misuse rejection names TAKE" LABEL
   ERR-E ERR-E-U @ >str s" error[E-MISMATCH]" STR:CONTAINS? TTRUE
   ERR-E ERR-E-U @ >str s" `FILE-OBJECTS-FACTS:TAKE`" STR:CONTAINS? TTRUE
   s" the dead-code rejection names FAIL" LABEL
   ERR-E ERR-E-U @ >str s" error[E-DEAD-CODE]: `2` never runs: `FILE-OBJECTS-FACTS:FAIL`" STR:CONTAINS? TTRUE ;

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
   CHECK-MAP CHECK-STOP CHECK-EDIT CHECK-E ;

\ Moves A's program and map to prog-cold and prog-cold.map, so A2 writes its own.
: COLD-ASIDE ( -- )
   A$ s" prog-cold" JOINED-2 FS:JOIN-PATH {: u:n :}
   A$ s" prog" IN$ JOINED-2 u >str FS:RENAME-FILE
   A$ s" prog-cold.map" JOINED-2 FS:JOIN-PATH {: v:n :}
   A$ s" prog.map" IN$ JOINED-2 v >str FS:RENAME-FILE ;

\ Moves E's program to prog-cold, so E2 writes its own.
: COLD-ASIDE-E ( -- )
   E$ s" prog-cold" JOINED-2 FS:JOIN-PATH {: u:n :}
   E$ s" prog" IN$ JOINED-2 u >str FS:RENAME-FILE ;

\ E2, a build in E's out root after E's. A failed E2 fails BUILD's checks; its
\ output and stderr are still compared, and it wrote no program to compare.
: WARM-E ( CMD:command -- CMD:command )
   STATE-E-U @ 0= if exit then
   s" E2" WHICH!  COLD-ASIDE-E
   E$ s" facts" s\" facts: compiled 0\n" STATE-W BUILD {: u:n :}
   CMD:OUT$ {: o:str :}
   u 0< if o STR:LEN else u then {: shown:n :}
   s" the build prints E's lines" LABEL  o STR:DATA shown >str STATE-E STATE-E-U @ >str T$=
   s" the build's stderr is E's" LABEL  CMD:ERR$ ERR-E ERR-E-U @ >str T$=
   u 0< if exit then
   s" the program has E's bytes" LABEL  E$ s" prog" E$ s" prog-cold" SAME-FILE? TTRUE ;

\ A build of A's files in A's out root, whose last line is the given one:
\ it prints A's state, writes A's program and map and leaves A's seven objects
\ with D's bytes from PIECES on.
: WARM ( CMD:command str -- CMD:command )
   {: last:str :}
   A$ s" " last STATE-W BUILD {: u:n :}
   u 0 < if exit then
   s" the build prints A's state" LABEL  STATE-W u >str STATE-A STATE-A-U @ >str T$=
   s" the program has A's bytes" LABEL  A$ s" prog" A$ s" prog-cold" SAME-FILE? TTRUE
   s" the map has A's bytes" LABEL  A$ s" prog.map" A$ s" prog-cold.map" SAME-FILE? TTRUE
   s" A's root still holds seven objects" LABEL  A$ OBJECTS FILES T=
   SAME-AS-D ;

\ A2: every object linked.
: WARM-A ( CMD:command -- CMD:command )
   s" A2" WHICH!  COLD-ASIDE  s\" compiled 0\n" WARM ;

\ A4's edit: count.f's object cut to half its length; false when A wrote no such object.
: CUT ( -- bool )
   A$ s" A" 0 FOUND? 0= if false exit then
   FOUND$ BYTES-A CAP FS:READ-ALL {: u:n :}
   FOUND$ BYTES-A u 2 / >str FS:WRITE-ALL  true ;

\ A5's edit: count.f's object grown in place to 2^48 bytes, past what mmap maps on an arm64
\ host; the bytes added are a hole, so the disk holds none.
: GROW ( -- bool )
   A$ s" A" 0 FOUND? 0= if false exit then
   FOUND$ FS:OPEN-APPEND-FD >FD {: f:fd :}
   f 1 48 lshift FD:RESIZE
   f FD>N close  true ;

\ A6's edit: one byte of count.f's object flipped, the first of INPUT's stamp, so every length
\ and offset in it still holds.
: FLIP ( -- bool )
   A$ s" A" 0 FOUND? 0= if false exit then
   FOUND$ BYTES-A CAP FS:READ-ALL {: u:n :}
   BYTES-A HEADER-BYTES + dup c@ $FF xor swap c!
   FOUND$ BYTES-A u >str FS:WRITE-ALL  true ;

\ After FOUND$'s edit the build stops with DATAERR, its record naming the object, which stays; once deleted,
\ count.f compiles and its new time compiles shape.f, main.f, reopen.f and mark.f; again.f's loads link.
: DAMAGED ( CMD:command str bool -- CMD:command )
   {: why:str edited:bool :}
   edited 0= if exit then
   A$ s" " BUILD-RUN {: rc:n :}
   SB-RESET  s" ]: " SB-APPEND  FOUND$ SB-APPEND
   CMD:ERR$ SB$ STR:CONTAINS? {: named:bool :}
   CMD:ERR$ s" E-DAMAGED-OBJECT" STR:CONTAINS?  named and {: coded:bool :}
   CMD:ERR$ why STR:CONTAINS?  coded and {: said:bool :}
   rc EXIT-STATUS:DATAERR =  said and  0= if rc SHOW then
   s" the build stops on the damaged object" LABEL  rc EXIT-STATUS:DATAERR T=
   s" its record names the object and why" LABEL  said TTRUE
   s" the object stays" LABEL  FOUND$ FS:EXISTS? TTRUE
   FOUND$ FS:REMOVE-FILE
   s\" compiled 5\n" WARM ;

: WARM-CUT ( CMD:command -- CMD:command )
   s" A4" WHICH!  s"  is not the file hb wrote" CUT DAMAGED ;

\ macOS only: Linux ext4 rejects a file past 16 TiB, so FD:RESIZE throws E-FS-IO there.
: WARM-GROW ( CMD:command -- CMD:command )
   HB-TARGET-MACOS? 0= if exit then
   s" A5" WHICH!  s"  cannot be read" GROW DAMAGED ;

: WARM-FLIP ( CMD:command -- CMD:command )
   s" A6" WHICH!  s"  is not the file hb wrote" FLIP DAMAGED ;

\ Guards a new first file: pad.f compiles, and so do main.f and mark.f, which spell its SCRATCH and FIRST,
\ and reopen.f, which runs count.f's LATER after main.f stores into count.f's data; the other four link.
: WARM-B ( CMD:command -- CMD:command )
   s" B2" WHICH!
   A$ s" second" s\" compiled 4\n" STATE-W BUILD {: u:n :}
   u 0 < if exit then
   s" the build prints B's state" LABEL  STATE-W u >str STATE-B STATE-B-U @ >str T$=
   s" the program has B's bytes" LABEL  A$ s" prog" B$ s" prog" SAME-FILE? TTRUE
   s" A's root holds eight objects" LABEL  A$ OBJECTS FILES-B T=
   SAME-AS-B ;

: TOOL-ARGS ( CMD:command -- CMD:command )
   CMD:RESET  s" --load" CMD:ARG+  s" tools/hb-build.f" CMD:ARG+  s" --" CMD:ARG+ ;

\ The TMPDIR of run TMP-N, in JOINED.
: TMP$ ( -- str )
   SB-RESET  s" tmp-" SB-APPEND  TMP-N @ FMT:SB-U
   L$ SB$ IN$ ;

\ An entry TOOL-RUN found in its TMPDIR.
: LEFT+ ( str -- ) drop 1 LEFT-N +! ;

\ After TOOL-ARGS and its options, builds the fixture to L's prog in the shared fixture out root.
\ Its TMPDIR is a fresh directory per run, where the build leaves nothing.
: TOOL-RUN ( CMD:command str -- CMD:command n )
   CMD:ARG+
   s" -o" CMD:ARG+  L$ s" prog" IN$ CMD:ARG+
   1 TMP-N +!  TMP$ FS:MAKE-DIR
   s" HABU_OUT" F$ CMD:ENV+  s" TMPDIR" TMP$ CMD:ENV+
   ENGINE-CANDIDATE:PATH$ PROCESS:WAIT-FOREVER CMD:RUN-OUTCOME
   PROCESS:ENDING>RC RC>N
   0 LEFT-N !
   TMP$ [: LEFT+ ;] FS-LIST:EACH
   s" the build leaves nothing in TMPDIR" LABEL  LEFT-N @ 0 T= ;

\ Builds a fixture by tools/hb-build.f (TOOL-RUN), since log.f requires and creates through does>
\ (build.f's files may not); --repl and --report-json when the flag is true.
: BUILD-RC ( CMD:command str bool -- CMD:command n )
   {: src:str repl:bool :}
   TOOL-ARGS
   repl if
      s" --repl" CMD:ARG+
      s" --report-json" CMD:ARG+
   then
   src TOOL-RUN ;

: BUILT? ( CMD:command n -- CMD:command bool ) {: rc:n :}
   rc 0<> if rc SHOW then
   s" the build exits 0" LABEL  rc 0 T=
   rc 0 = ;

: TOOL-BUILD ( CMD:command str bool -- CMD:command bool ) BUILD-RC BUILT? ;

: FIXTURE-BUILD ( CMD:command str -- CMD:command bool ) false TOOL-BUILD ;
: REPL-BUILD ( CMD:command str -- CMD:command bool ) true TOOL-BUILD ;

: SIZE-BUILD ( CMD:command str -- CMD:command bool )
   {: src:str :}
   TOOL-ARGS
   s" --size-report" CMD:ARG+
   src TOOL-RUN BUILT? ;

\ Builds a fixture the build rejects and answers its exit code.
: FAILING-BUILD ( CMD:command str -- CMD:command n ) false BUILD-RC ;

\ The loader's rejection TEXT, then word-load-inc.f's canonical path, as the
\ next build of a fixture whose word reaches that file should say it.
: REJECTION! ( str -- ) {: s:str :}
   s STR:DATA REJECTION s STR:LEN BYTE-COPY
   s" test/fixtures/file-objects/word-load-inc.f" SOURCE-ROOT:CANONICAL drop {: p:str :}
   p STR:DATA REJECTION s STR:LEN + p STR:LEN BYTE-COPY
   s STR:LEN p STR:LEN + REJECTION-U ! ;

\ The loader rejects word-load-inc.f, which the fixture's word reaches at its
\ top level.
: REJECTED-BUILD ( CMD:command str -- CMD:command ) {: src:str :}
   src FAILING-BUILD {: rc:n :}
   CMD:ERR$ REJECTION REJECTION-U @ >str STR:CONTAINS? {: said:bool :}
   rc 0 =  said 0=  or if rc SHOW then
   s" the build exits nonzero" LABEL  rc 0<> TTRUE
   s" the build rejects the word's file by its path" LABEL  said TTRUE ;

\ Checks a failing build's stderr opens with rec and holds no second record and no line of its own, and
\ that the build exited want.
: ONE-RECORD ( CMD:command n n str -- CMD:command ) {: rc:n want:n rec:str :}
   CMD:ERR$ {: e:str :}
   e rec STR:STARTS-WITH? {: said:bool :}
   said if  e STR:DATA 1 + e STR:LEN 1 - >str s" error[" STR:CONTAINS?
   else false then {: more:bool :}
   e s" hb-build" STR:CONTAINS?  e s" linker:" STR:CONTAINS?  or {: told:bool :}
   rc want =  said and  more 0= and  told 0= and  0= if rc SHOW then
   s" the build prints its rejection first" LABEL  said TTRUE
   s" the build prints no second record" LABEL  more TFALSE
   s" the build prints no line of its own" LABEL  told TFALSE
   s" the build exits its status" LABEL  rc want T= ;

\ Builds the fixture at path p, which the linker rejects at its end with one record: its first line m,
\ the file note and the help h.
: FILE-REJECTED ( CMD:command str str str -- CMD:command )
   {: p:str m:str h:str :}
   p FAILING-BUILD {: rc:n :}
   SB-RESET  m SB-APPEND  s\" \n  = file: " SB-APPEND
   p SOURCE-ROOT:CANONICAL drop SB-APPEND
   s\" \n  = help: " SB-APPEND  h SB-APPEND  s\" \n" SB-APPEND
   rc EXIT-STATUS:REJECTED SB$ ONE-RECORD ;

\ task-left-main.f's task still runs at the file's end.
: TASK-LEFT-BUILD ( CMD:command -- CMD:command )
   s" test/fixtures/file-objects/task-left-main.f"
   s" error[E-LIVE-TASK]: a task is still running at the file's end, so the file's object would hold data the task can still change"
   s" end or join every task the file starts before the file ends" FILE-REJECTED ;

\ dp-back-main.f's top level gives data space back.
: DP-BACK-BUILD ( CMD:command -- CMD:command )
   s" test/fixtures/file-objects/dp-back-main.f"
   s" error[E-FAILED]: the file gave back data space"
   s" keep allot non-negative, and let no failure a text catches leave data behind" FILE-REJECTED ;

\ word-required-main.f's word catches the loader's rejection of its `required`:
\ the build exits 85 and prints that record alone.
: CAUGHT-REQUIRE-BUILD ( CMD:command -- CMD:command )
   s" error[E-SOURCE-ROOT]: `required` got `" REJECTION!
   s" X3" WHICH!
   s" test/fixtures/file-objects/word-required-main.f" FAILING-BUILD
   EXIT-STATUS:UNCAUGHT REJECTION REJECTION-U @ >str ONE-RECORD ;

\ A rejection ends its build, so each way a rejection reaches stderr is a build of its own.
: SENT-BUILDS ( CMD:command -- CMD:command )
   \ A task's rejection counts in main's SENT.
   s" X5" WHICH!  s" test/fixtures/file-objects/sent-task-main.f" FAILING-BUILD
   EXIT-STATUS:UNCAUGHT s\" error[E-SENT-TASK]: a task sent it\n" ONE-RECORD
   \ ERROR:UNCAUGHT's saved record counts when SEND-SAVED writes it.
   s" X6" WHICH!  s" test/fixtures/file-objects/sent-uncaught-main.f" FAILING-BUILD
   EXIT-STATUS:UNCAUGHT s\" error[E-THROW]: nothing caught a throw of code 12345\n" ONE-RECORD
   \ A captured record counts when RELEASE writes it.
   s" X7" WHICH!  s" test/fixtures/file-objects/sent-release-main.f" FAILING-BUILD
   EXIT-STATUS:UNCAUGHT s\" error[E-SENT-RELEASE]: released\n" ONE-RECORD
   \ A throw that carries its record prints it after a caught rejection was sent.
   s" error[E-SOURCE-ROOT]: `required` got `" REJECTION!
   s" X8" WHICH!  s" test/fixtures/file-objects/sent-throw-main.f" FAILING-BUILD {: rc:n :}
   CMD:ERR$ REJECTION REJECTION-U @ >str STR:STARTS-WITH? {: said:bool :}
   CMD:ERR$ s\" \nerror[E-THROW]: nothing caught a throw of code 12345\n" STR:CONTAINS? {: threw:bool :}
   said threw and 0= if rc SHOW then
   s" the build prints the caught rejection first" LABEL  said TTRUE
   s" the build prints the throw's record" LABEL  threw TTRUE
   \ A rejection the program's own copy of REJECTION kept prints through hb's.
   s" X9" WHICH!  s" test/fixtures/file-objects/carried-main.f" FAILING-BUILD
   EXIT-STATUS:UNCAUGHT s\" error[E-PATH-RANGE]: a path is empty\n" ONE-RECORD ;

\ A rejection ends its build, so each way a word reaches word-load-inc.f is a build of its own: included,
\ provided, a caught required and SOURCE-ROOT:READ-OS, which SOURCE-INPUT:READ's default reader also reaches.
: WORD-LOAD-BUILD ( CMD:command -- CMD:command )
   s" error[E-SOURCE-ROOT]: `included` got `" REJECTION!
   s" X" WHICH!  s" test/fixtures/file-objects/word-load-main.f" REJECTED-BUILD
   s" error[E-SOURCE-ROOT]: `provided` got `" REJECTION!
   s" X4" WHICH!  s" test/fixtures/file-objects/word-provided-main.f" REJECTED-BUILD
   CAUGHT-REQUIRE-BUILD
   s" error[E-SOURCE-ROOT]: `SOURCE-ROOT:READ-OS` got `" REJECTION!
   s" X2" WHICH!  s" test/fixtures/file-objects/word-read-main.f" REJECTED-BUILD ;

\ Darwin's first lock of a mutex sets its signature, $4D55545A (the bytes ZTUM), and unlocking leaves it.
: LOG-UNLOCKED ( -- )
   HB-TARGET-MACOS? 0= if exit then
   [: L$ s" prog" IN$ WHOLE-FILE
      s" the program holds no mutex the build locked" LABEL  s" ZTUM" STR:CONTAINS? TFALSE ;]
   ARENA:WITH ;

\ L's program, run in its own process: the boundary under test is the written file.
: LOG-RUN ( CMD:command -- CMD:command )
   CMD:RESET
   L$ s" prog" IN$ PROCESS:WAIT-FOREVER CMD:RUN-OUTCOME PROCESS:ENDING>RC RC>N {: rc:n :}
   CMD:ERR$ s\"  warn main: ran\n" STR:ENDS-WITH? {: ran:bool :}
   ran  rc 0 =  and 0= if rc SHOW then
   s" the program logs MAIN's line" LABEL  ran TTRUE
   s" the program exits 0" LABEL  rc 0 T= ;

\ KEEP holds the size and change time of the build's object of tail i.
: KEEP ( n -- ) {: i:n :}
   -1 i KEPT-SIZE !
   F$ WHICH WHICH-U @ >str i FOUND? 0= if exit then
   FOUND$ FS:FILE-META {: size:n ms:n mns:n cs:n cns:n :}
   size i KEPT-SIZE !  cs i KEPT-S !  cns i KEPT-NS ! ;

\ Whether KEEP and FOUND? each found the build's object of tail i; a miss fails the label, else the
\ label names the caller's next assertion.
: KEPT-FOUND? ( str n -- bool ) {: s:str i:n :}
   i KEPT-SIZE @ 0 < if s LABEL false TTRUE false exit then
   F$ WHICH WHICH-U @ >str i FOUND? {: one:bool :}
   s LABEL
   one 0= if false TTRUE then  one ;

\ Whether FOUND$ keeps the size and change time KEEP held for tail i: a compile replaces a file's
\ object (linker.f WRITE-OBJECT) and a link leaves it.
: KEPT? ( n -- bool ) {: i:n :}
   FOUND$ FS:FILE-META {: size:n ms:n mns:n cs:n cns:n :}
   size i KEPT-SIZE @ =  cs i KEPT-S @ =  and  cns i KEPT-NS @ =  and ;

\ Whether a change time, seconds and nanoseconds, is later than the one MARK holds.
: LATER? ( n n -- bool ) {: sec:n nsec:n :}
   sec MARK-S @ >  sec MARK-S @ =  nsec MARK-NS @ >  and  or ;

: MARK-SEEN ( str -- )
   {: p:str :}
   p s" .hfo" STR:ENDS-WITH? 0= if exit then
   p FS:FILE-META {: size:n ms:n mns:n cs:n cns:n :}
   cs cns LATER? 0= if exit then
   cs MARK-S !  cns MARK-NS ! ;

: NEWER-SEEN ( str -- )
   {: p:str :}
   p s" .hfo" STR:ENDS-WITH? 0= if exit then
   p FS:FILE-META {: size:n ms:n mns:n cs:n cns:n :}
   cs cns LATER? 0= if exit then
   1 NEWER-N +!  p STR:DATA FOUND p STR:LEN BYTE-COPY  p STR:LEN FOUND-U ! ;

: MARK-WALK ( -- ) ROOT ROOT-U @ >str ['] MARK-SEEN FS:WALK-FILES ;
: NEWER-WALK ( -- ) ROOT ROOT-U @ >str ['] NEWER-SEEN FS:WALK-FILES ;

\ MARK holds the newest change time among the out root's objects.
: MARK ( str -- ) ROOT!  0 MARK-S !  0 MARK-NS !  ['] MARK-WALK WALKED ;

\ The out root's objects changed after MARK's time; FOUND$ is the last of them.
: REWRITTEN ( str -- n ) ROOT!  0 NEWER-N !  0 FOUND-U !  ['] NEWER-WALK WALKED  NEWER-N @ ;

\ The label holds when the build's object of tail i is the one KEEP held.
: LINKED ( str n -- ) {: s:str i:n :}
   s i KEPT-FOUND? 0= if exit then  i KEPT? TTRUE ;

\ The label holds when the build's object of tail i replaces the one KEEP held.
: COMPILED ( str n -- ) {: s:str i:n :}
   s i KEPT-FOUND? 0= if exit then  i KEPT? TFALSE ;

\ path-main.f's program, run in its own process: the path it reports is the written
\ file's, and the park it wakes is its own.
: PATH-RUN ( CMD:command -- CMD:command )
   CMD:RESET
   L$ s" prog" IN$ PROCESS:WAIT-FOREVER CMD:RUN-OUTCOME PROCESS:ENDING>RC RC>N {: rc:n :}
   CMD:OUT$ s\" /prog\n" STR:ENDS-WITH? {: own:bool :}
   own  rc 0 =  and 0= if rc SHOW then
   s" the program reports its own path" LABEL  own TTRUE
   s" the program exits 0" LABEL  rc 0 T= ;

\ M's program, run in its own process: only a built program lacks the engine's
\ own compiler files, so the build and the written file are the boundary under test.
: BARE-RUN ( CMD:command -- CMD:command )
   CMD:RESET
   L$ s" prog" IN$ PROCESS:WAIT-FOREVER CMD:RUN-OUTCOME PROCESS:ENDING>RC RC>N {: rc:n :}
   CMD:OUT$ s\" valued\n" STR:= {: same:bool :}
   same  rc 0 =  and 0= if rc SHOW then
   s" the program prints the enum its MAIN fetched" LABEL  same TTRUE
   s" the program exits 0" LABEL  rc 0 T= ;

\ scratch-main.f's program, run in its own process: the boundary under test is the written file.
: SCRATCH-RUN ( CMD:command -- CMD:command )
   CMD:RESET
   L$ s" prog" IN$ PROCESS:WAIT-FOREVER CMD:RUN-OUTCOME PROCESS:ENDING>RC RC>N {: rc:n :}
   rc 0<> if rc SHOW then
   s" a file's scratch store ends with the file" LABEL  CMD:OUT$ s\" 0\n0\n0\n0\n0\n0\n0\n0\n0\n0\n" T$= ;

\ kept-main.f's program, run in its own process: the boundary under test is the written file.
: KEPT-RUN ( CMD:command -- CMD:command )
   CMD:RESET
   L$ s" prog" IN$ PROCESS:WAIT-FOREVER CMD:RUN-OUTCOME PROCESS:ENDING>RC RC>N {: rc:n :}
   rc 0<> if rc SHOW then
   s" a kept handle is rejected and word docs read at build and run time count 2 and 1" LABEL
   CMD:OUT$ s\" zip rejected\ndocs 2 1\n" T$= ;

\ Stores the member's integer in the cell; false when the object has no such member.
: MEMBER! ( JSON:reader str ^n -- JSON:reader bool )
   {: key:str dst:^ :}
   key JSON:READ-FIND-KEY 0= if false exit then
   JSON:READ-TOKEN JSON:T-INT <> if false exit then
   JSON:READ-INT dst ! true ;

: CLASS+ ( JSON:reader str -- JSON:reader bool )
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
: REPORT-READ ( str -- bool )
   {: s:str :}
   REPORT-CURSOR s JSON:READ-INIT
   JSON:READ-NEXT JSON:T-OBJ = if COUNTS else false then
   if SIZES else false then
   if CLOSES? else false then
   swap JSON:READ-CLOSE ;

\ N's report, stdout's last line: it parses, its total is the program's length, its six classes
\ sum to the total, it charges code and it counts the build's files.
: REPORT-RUN ( CMD:command -- CMD:command )
   CMD:OUT$ {: o:str :}
   o BEFORE-LAST {: at:n :}
   o STR:DATA at + o STR:LEN at - >str REPORT-READ {: read:bool :}
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
   s" a device a file kept is rejected in the program" LABEL  CMD:OUT$ s\" genio rejected\n" T$= ;

\ L's program, run in its own process: the boundary under test is the written file. The label
\ holds when its stdout is the wanted text.
: PRINTS-RUN ( CMD:command str str -- CMD:command )
   {: want:str label:str :}
   CMD:RESET
   L$ s" prog" IN$ PROCESS:WAIT-FOREVER CMD:RUN-OUTCOME PROCESS:ENDING>RC RC>N {: rc:n :}
   rc 0<> if rc SHOW then
   label LABEL  CMD:OUT$ want T$= ;

: SIZE-RUN ( CMD:command -- CMD:command )
   s" the size report groups the code by package" LABEL
   CMD:OUT$ s\" \nby package\n" STR:CONTAINS? TTRUE
   s" the size report groups the code by source" LABEL
   CMD:OUT$ s\" \nby source\n" STR:CONTAINS? TTRUE ;

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
   s" the object it rewrites is app-end.f's" LABEL  FOUND$ s" /app-end.f.hfo" STR:ENDS-WITH? TTRUE
   LOG-RUN ;

\ Guards a kept parses-through: row: T2 compiles pt-use.f's BLK use against pt-def.f's kept object
\ into T's bytes; without the row pt-use.f's object differs.
: PT-CASE ( CMD:command -- CMD:command )
   s" T" WHICH!  s" test/fixtures/file-objects/pt-main.f" FIXTURE-BUILD 0= if exit then
   F$ s" T" PT-DEF FOUND? 0= if exit then
   FOUND$ BYTES-A CAP FS:READ-ALL {: u:n :}
   s" pt-def.f's object holds BLK's parses-through row" LABEL
   BYTES-A u >str s" parses-through 0 ( ;BLK )" STR:CONTAINS? TTRUE
   s" pt-def.f's object holds no parses row" LABEL  BYTES-A u >str s" parses " STR:CONTAINS? TFALSE
   PT-DEF KEEP
   F$ s" T" PT-USE FOUND? 0= if exit then
   FOUND$ BYTES-B CAP FS:READ-ALL {: bu:n :}
   FOUND$ FS:REMOVE-FILE  F$ MARK
   s" T2" WHICH!  s" test/fixtures/file-objects/pt-main.f" FIXTURE-BUILD 0= if exit then
   s" the build links pt-def.f" PT-DEF LINKED
   F$ REWRITTEN {: n:n :}
   s" the build rewrites only pt-use.f's object" LABEL  n 1 T=
   s" the object it rewrites is pt-use.f's" LABEL  FOUND$ PT-USE TAIL$ STR:ENDS-WITH? TTRUE
   FOUND$ BYTES-A CAP FS:READ-ALL {: au:n :}
   s" it has T's bytes from PIECES on" LABEL  au bu FROM-PIECES? TTRUE
   s\" PT\n" s" the program prints PT" PRINTS-RUN ;

\ R's program, run in its own process: its stderr is MAIN's record alone.
: RECORD-RUN ( CMD:command -- CMD:command )
   CMD:RESET
   L$ s" prog" IN$ PROCESS:WAIT-FOREVER CMD:RUN-OUTCOME PROCESS:ENDING>RC RC>N {: rc:n :}
   CMD:ERR$ s\" error[E-MAIN]: ran\n\n" STR:= {: ran:bool :}
   ran  rc 0 =  and 0= if rc SHOW then
   s" the program prints MAIN's record" LABEL  ran TTRUE
   s" the program exits 0" LABEL  rc 0 T= ;

\ Guards the text a record is built in: record-first.f's top level prints a record, and the
\ build saves no address of the build process's mapping; its dropped capture leaves the build linking.
: RECORD-CASE ( CMD:command -- CMD:command )
   s" R" WHICH!  s" test/fixtures/file-objects/record-main.f" FIXTURE-BUILD 0= if exit then
   RECORD-RUN ;

\ L's program, run in its own process: a computed 0 thrown out of MAIN prints
\ E-THROW-ZERO's record and exits 85.
: ZERO-RUN ( CMD:command -- CMD:command )
   CMD:RESET
   L$ s" prog" IN$ PROCESS:WAIT-FOREVER CMD:RUN-OUTCOME PROCESS:ENDING>RC RC>N {: rc:n :}
   CMD:ERR$ s" error[E-THROW-ZERO]" STR:STARTS-WITH? {: opened:bool :}
   CMD:ERR$ s" throw a computed code only when it is nonzero: `rc 0<> if rc throw then`" STR:CONTAINS?
   opened and {: said:bool :}
   said  rc EXIT-STATUS:UNCAUGHT =  and 0= if rc SHOW then
   s" the program prints E-THROW-ZERO's record" LABEL  said TTRUE
   s" the program exits 85" LABEL  rc EXIT-STATUS:UNCAUGHT T= ;

\ Z is a stripped program; ZR, built with --repl, is hb running MAIN before its REPL.
: ZERO-CASE ( CMD:command -- CMD:command )
   s" Z" WHICH!  s" test/fixtures/file-objects/zero-main.f" FIXTURE-BUILD 0= if exit then
   ZERO-RUN
   s" ZR" WHICH!  s" test/fixtures/file-objects/zero-main.f" REPL-BUILD 0= if exit then
   ZERO-RUN ;

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
   CMD:ERR$ s\" E-BAD-TAG habu: layout tag 7 outside 0..1\n" STR:= {: trapped:bool :}
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
   T-LABEL$ {: la:str :}  la STR:LEN {: lu:n :}
   la STR:DATA HELD-LABEL lu BYTE-COPY
   F$ WHICH WHICH-U @ >str i FOUND? {: one:bool :}
   HELD-LABEL lu >str T-LABEL
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
: OBJECT-TEXT-AT ( n str -- n )
   {: i:n s:str :}
   i FIXTURE-OBJECT {: ou:n :}
   ou 0 < if -1 exit then
   0 FIXTURE-BYTES ou >str s STR:FIND-SUB MATCH option
      none OF -1 ENDOF
      some OF IDX>N ENDOF
   ;MATCH ;

\ Whether the fixture out root's object of tail i holds the text, as it holds each path it names.
: OBJECT-HOLDS? ( n str -- bool ) OBJECT-TEXT-AT 0 >= ;


\ Whether order-x2.f's object holds the first path before the second: it lays the paths of the
\ storers it read in the order they linked.
: X2-BEFORE? ( str str -- bool )
   {: a:str b:str :}
   ORDER-X2 a OBJECT-TEXT-AT {: x:n :}
   ORDER-X2 b OBJECT-TEXT-AT {: y:n :}
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
   ROOT-SET-LAST KEEP  SCOPE-OPEN KEEP  SCOPE-USE KEEP  INSIDE-I KEEP
   s" Y2" WHICH!  s" test/fixtures/file-objects/parts-root-2.f" FIXTURE-BUILD 0= if exit then
   s" the build links mid-four.f's last part" MID-FOUR-LAST LINKED
   s" the build links unclean.f's last part after an unclean request and a clean one"
   UNCLEAN-LAST LINKED
   s" unclean.f's part an unclean request ends never links" UNCLEAN-FIRST COMPILED
   s" unclean.f's part after an unclean request never links" UNCLEAN-SECOND COMPILED
   s" the build links redo.f's part" REDO LINKED
   s" the build links root-set.f's last part, after its first part sets the root" ROOT-SET-LAST LINKED
   s" scope-open.f, which leaves its package open, never links" SCOPE-OPEN COMPILED
   s" the build links scope-use.f, which names scope-open.f's SEVEN" SCOPE-USE LINKED
   s" inside-i.f's object from a load inside a package never links" INSIDE-I COMPILED
   s\" 10\n20\n30\n40\n5\n1\n2\n3\n7\n9\n2\n6\n11\n13\n"
   s" the program prints MAIN's numbers: mid-four.f's, redo.f's X with its second Y" PRINTS-RUN
   FLIP-SECOND KEEP  INSIDE-I KEEP
   s" Y3" WHICH!  s" test/fixtures/file-objects/parts-root-2.f" FIXTURE-BUILD 0= if exit then
   s" the build links the part that starts at [then]" FLIP-SECOND LINKED
   s" the build links inside-i.f's object from a load alone" INSIDE-I LINKED
   s\" 10\n20\n30\n40\n5\n1\n2\n3\n7\n9\n2\n6\n11\n13\n"
   s" the program prints MAIN's numbers with flip.f's part at [then] linked" PRINTS-RUN
   INSIDE-I KEEP
   s" Y4" WHICH!  s" test/fixtures/file-objects/parts-root-1.f" FIXTURE-BUILD 0= if exit then
   s" a load inside a package never links inside-i.f's object from a load alone" INSIDE-I COMPILED ;

\ kept-aio.f's build: it exits 70 with LINKER:E-ADDRESS naming OLD at kept-aio.f's line 8.
: KEPT-AIO-REJECTED ( CMD:command -- CMD:command )
   s" test/fixtures/file-objects/kept-aio.f" false BUILD-RC {: rc:n :}
   CMD:ERR$ s" error[LINKER:E-ADDRESS]" STR:CONTAINS? {: classed:bool :}
   CMD:ERR$ s" in OLD +0" STR:CONTAINS? classed and {: named:bool :}
   CMD:ERR$ s" test/fixtures/file-objects/kept-aio.f:8" STR:CONTAINS? {: placed:bool :}
   named placed and  rc 70 =  and 0= if rc SHOW then
   s" the build rejects OLD" LABEL  named TTRUE
   s" the rejection names kept-aio.f's line 8" LABEL  placed TTRUE
   s" the build exits 70" LABEL  rc 70 T= ;

\ Guards a file's top level in a build, a build per guard; N, the one engine build, reports its sizes.
: PIECE-CASE ( CMD:command -- CMD:command )
   s" R" WHICH!  s" test/fixtures/file-objects/scratch-main.f" FIXTURE-BUILD 0= if exit then
   SCRATCH-RUN
   s" Q" WHICH!  s" test/fixtures/file-objects/kept-main.f" FIXTURE-BUILD 0= if exit then  KEPT-RUN
   s" QA" WHICH!  KEPT-AIO-REJECTED  AIO-OBJ KEEP
   \ QA2's warm build links aio.f's object and is rejected the same.
   s" QA2" WHICH!  KEPT-AIO-REJECTED  s" the warm build links aio.f" AIO-OBJ LINKED
   s" N" WHICH!  s" test/fixtures/file-objects/kept-genio.f" REPL-BUILD 0= if exit then
   REPORT-RUN  GENIO-RUN
   s" NG" WHICH!  s" test/fixtures/file-objects/kept-genio.f" FIXTURE-BUILD 0= if exit then  GENIO-RUN
   s" W" WHICH!  TASK-LEFT-BUILD
   s" W2" WHICH!  TASK-LEFT-BUILD
   s" DB" WHICH!  DP-BACK-BUILD
   WORD-LOAD-BUILD  SENT-BUILDS ;

\ seal-main.f's build, in a fresh hb since SEALED-FAIL ends the process it rejects in: it exits 70
\ with E-SEALED-PACKAGE naming the definition and seal-main.f's line.
: SEAL-REJECTED ( CMD:command -- CMD:command )
   s" test/fixtures/file-objects/seal-main.f" false BUILD-RC {: rc:n :}
   CMD:ERR$ s" error[E-SEALED-PACKAGE]" STR:CONTAINS? {: classed:bool :}
   CMD:ERR$ s" `FILE-OBJECTS-SEAL:D` would change a sealed package" STR:CONTAINS? classed and {: named:bool :}
   CMD:ERR$ s" test/fixtures/file-objects/seal-main.f:6" STR:CONTAINS? {: placed:bool :}
   CMD:ERR$ s" E-FAILED" STR:CONTAINS? {: failed:bool :}
   named placed and  rc 70 =  and  failed 0= and 0= if rc SHOW then
   s" the build exits 70" LABEL  rc 70 T=
   s" the build rejects FILE-OBJECTS-SEAL:D" LABEL  named TTRUE
   s" the rejection names seal-main.f's line 6" LABEL  placed TTRUE
   s" the uncaught failure's rollbacks print no E-FAILED" LABEL  failed TFALSE ;

\ Guards a seal a link carries: F2 links seal-second.f's object and is rejected as F is.
: SEAL-CASE ( CMD:command -- CMD:command )
   s" F" WHICH!  SEAL-REJECTED  SEAL-SECOND KEEP
   s" F2" WHICH!  SEAL-REJECTED
   s" the build links seal-second.f" SEAL-SECOND LINKED ;

\ squat-main.f's build, in a fresh hb as SEAL-REJECTED's, as a --repl image when the flag is true:
\ it exits 70 with E-SEALED-PACKAGE naming CHECKER-RESOLVE and squat-main.f's line 4.
: SQUAT-REJECTED ( CMD:command bool -- CMD:command ) {: repl:bool :}
   s" test/fixtures/file-objects/squat-main.f" repl BUILD-RC {: rc:n :}
   CMD:ERR$ s" error[E-SEALED-PACKAGE]" STR:CONTAINS? {: classed:bool :}
   CMD:ERR$ s" `CHECKER-RESOLVE` would change a sealed package" STR:CONTAINS? classed and {: named:bool :}
   CMD:ERR$ s" test/fixtures/file-objects/squat-main.f:4" STR:CONTAINS? {: placed:bool :}
   named placed and  rc 70 =  and 0= if rc SHOW then
   s" the build exits 70" LABEL  rc 70 T=
   s" the build rejects CHECKER-RESOLVE" LABEL  named TTRUE
   s" the rejection names squat-main.f's line 4" LABEL  placed TTRUE ;

\ Guards the engine's seal in a program build (SQ, stripped) and a --repl build (SQ2).
: SQUAT-CASE ( CMD:command -- CMD:command )
   s" SQ" WHICH!  false SQUAT-REJECTED
   s" SQ2" WHICH!  true SQUAT-REJECTED ;

\ Guards a --repl build (SR) of spawn-main.f: its spawn.f opens PROCESS,
\ a package that build's engine files do not make.
: SPAWN-REPL-CASE ( CMD:command -- CMD:command )
   s" SR" WHICH!  s" test/fixtures/file-objects/spawn-main.f" REPL-BUILD drop ;

\ Whether the build's output holds an E-FAILED rejection, its place p with
\ the line that ran the failed text, and the caught code's note c.
: FAILED-AT? ( CMD:command str str -- CMD:command bool )
   {: p:str c:str :}
   CMD:ERR$ s" error[E-FAILED]: a text failed and its failure was caught" STR:CONTAINS? {: coded:bool :}
   CMD:ERR$ p STR:CONTAINS? {: placed:bool :}
   CMD:ERR$ c STR:CONTAINS? {: caught:bool :}
   coded placed and  caught and ;

\ Whether the build's output holds a record or line of the failed throw beside
\ its E-FAILED rejection, which that throw carries.
: THREW? ( CMD:command -- CMD:command bool )
   CMD:ERR$ s" this threw" STR:CONTAINS? {: placed:bool :}
   CMD:ERR$ s" threw -" STR:CONTAINS?  placed or ;

\ rollback.f's build, which catches texts that leave words and data: it exits 85,
\ rejected E-FAILED at line 9, which ran the newest of them, and its code 70.
: CAUGHT-CASE ( CMD:command -- CMD:command )
   s" CF" WHICH!
   s" test/fixtures/file-objects/rollback.f" FAILING-BUILD {: rc:n :}
   s\" rollback.f:9:1\n  |\n9 | FOB-TRY\n  | ^^^^^^^ this ran the text\n"
   s" = code: 70" FAILED-AT? {: placed:bool :}
   THREW? {: threw:bool :}
   rc EXIT-STATUS:UNCAUGHT =  placed and  threw 0= and 0= if rc SHOW then
   s" the build exits 85" LABEL  rc EXIT-STATUS:UNCAUGHT T=
   s" the build is rejected E-FAILED at rollback.f's line 9 with code 70" LABEL  placed TTRUE
   s" the build prints no record of the throw beside it" LABEL  threw TFALSE ;

\ rollback-passed.f's build: its caught E-PATH-RANGE never displaces FOB-TRY's E-FAILED at line 4.
: CAUGHT-PASSED-CASE ( CMD:command -- CMD:command )
   s" CP" WHICH!
   s" test/fixtures/file-objects/rollback-passed.f" FAILING-BUILD {: rc:n :}
   s\" rollback-passed.f:4:1\n  |\n4 | FOB-TRY\n  | ^^^^^^^ this ran the text\n"
   s" = code: 70" FAILED-AT? {: placed:bool :}
   CMD:ERR$ s" E-PATH-RANGE" STR:CONTAINS? {: passed:bool :}
   rc EXIT-STATUS:UNCAUGHT =  placed and  passed 0= and 0= if rc SHOW then
   s" the build exits 85" LABEL  rc EXIT-STATUS:UNCAUGHT T=
   s" the build is rejected E-FAILED at rollback-passed.f's line 4 with code 70" LABEL  placed TTRUE
   s" the build prints no E-PATH-RANGE" LABEL  passed TFALSE ;

\ rollback-load.f's build, whose text catches rollback-fail.f's failed load: it exits 85,
\ rejected E-FAILED at the line that ran the text, which requested the load, with a note for
\ rollback-dep.f, which rollback-fail.f required.
: CAUGHT-LOAD-CASE ( CMD:command -- CMD:command )
   s" LF" WHICH!
   s" test/fixtures/file-objects/rollback-load.f" FAILING-BUILD {: rc:n :}
   s\" rollback-load.f:6:1\n  |\n6 | FOB-LOAD\n  | ^^^^^^^^ this ran the text\n"
   s" = code: 12345" FAILED-AT? {: placed:bool :}
   \ The note prints the path absolute.
   CMD:ERR$ s\" /test/fixtures/file-objects/rollback-dep.f\n" STR:CONTAINS? {: noted:bool :}
   THREW? {: threw:bool :}
   rc EXIT-STATUS:UNCAUGHT =  placed and  noted and  threw 0= and 0= if rc SHOW then
   s" the build exits 85" LABEL  rc EXIT-STATUS:UNCAUGHT T=
   s" the build is rejected E-FAILED at rollback-load.f's line 6 with code 12345" LABEL  placed TTRUE
   s" the rejection names rollback-dep.f as required" LABEL  noted TTRUE
   s" the build prints no record of the throw beside it" LABEL  threw TFALSE ;

\ rollback-none.f's build, which catches a failing evaluate of a text that allots
\ nothing at its top level: it links, and its program prints what MAIN prints.
: CAUGHT-NONE-CASE ( CMD:command -- CMD:command )
   s" CN" WHICH!  s" test/fixtures/file-objects/rollback-none.f" FIXTURE-BUILD 0= if exit then
   s\" 1\n\n" s" the program prints MAIN's 1" PRINTS-RUN ;

\ DF's rejection, copied into the arena before DF2's build replaces it, then DF2's.
: DEFINE-AGAIN ( CMD:command ^ARENA:arena -- CMD:command )
   {: m:^ :}
   CMD:ERR$ {: e:str :}  e STR:LEN {: eu:n :}
   m eu ARENA:ALLOC {: buf:^ cap:n :}
   e STR:DATA buf eu BYTE-COPY
   s" DF2" WHICH!
   s" test/fixtures/file-objects/rollback-define.f" FAILING-BUILD {: rc2:n :}
   s" the warm rebuild exits 85" LABEL  rc2 EXIT-STATUS:UNCAUGHT T=
   s" the warm rebuild prints DF's text" LABEL  CMD:ERR$ buf eu >str STR:= TTRUE ;

\ rollback-define.f's build (DF) and its warm rebuild (DF2), whose top level catches
\ texts that define a word: DF is rejected E-FAILED at line 8, the newest such text
\ before the require on line 9, with its code 12345, and DF2 with DF's text.
: DEFINE-CASE ( CMD:command -- CMD:command )
   s" DF" WHICH!
   s" test/fixtures/file-objects/rollback-define.f" FAILING-BUILD {: rc:n :}
   s\" rollback-define.f:8:1\n  |\n8 | FOB-PROBE drop\n  | ^^^^^^^^^ this ran the text\n"
   s" = code: 12345" FAILED-AT? {: placed:bool :}
   THREW? {: threw:bool :}
   rc EXIT-STATUS:UNCAUGHT =  placed and  threw 0= and 0= if rc SHOW then
   s" the build exits 85" LABEL  rc EXIT-STATUS:UNCAUGHT T=
   s" the build is rejected E-FAILED at rollback-define.f's line 8 with code 12345" LABEL  placed TTRUE
   s" the build prints no record of the throw beside it" LABEL  threw TFALSE
   [: DEFINE-AGAIN ;] ARENA:WITH ;

\ rollback-again.f's build, whose top level catches the rejection of a require after a caught failed text
\ that left a word, then requires again: the build prints that text's record alone, at line 4.
: CAUGHT-AGAIN-CASE ( CMD:command -- CMD:command )
   s" AG" WHICH!
   s" test/fixtures/file-objects/rollback-again.f" {: p:str :}
   p FAILING-BUILD
   SB-RESET  s" error[E-FAILED]: a text failed and its failure was caught, so the build writes nothing" SB-APPEND
   s\" \n --> " SB-APPEND
   p SOURCE-ROOT:CANONICAL drop SB-APPEND  s\" :4:1\n" SB-APPEND
   s\"   |\n4 | FOB-PROBE drop\n  | ^^^^^^^^^ this ran the text\n  = code: 12345\n" SB-APPEND
   s"   = help: let the failure end the build, or keep a text that can fail" SB-APPEND
   s\"  from defining, allotting or undefining\n" SB-APPEND
   EXIT-STATUS:UNCAUGHT SB$ ONE-RECORD ;

\ ctor-main.f's build, in a fresh hb since the rejection ends the build: it fails with
\ E-CTOR-PROTECTED at ctor-main.f's line 6. Answers its exit code.
: CTOR-REJECTED ( CMD:command -- CMD:command n )
   s" test/fixtures/file-objects/ctor-main.f" false BUILD-RC {: rc:n :}
   CMD:ERR$ s" error[E-CTOR-PROTECTED]" STR:CONTAINS? {: coded:bool :}
   CMD:ERR$ s" test/fixtures/file-objects/ctor-main.f:6:" STR:CONTAINS? coded and {: named:bool :}
   named  rc 0<>  and 0= if rc SHOW then
   s" the build fails" LABEL  rc 0<> TTRUE
   s" the build rejects with E-CTOR-PROTECTED" LABEL  named TTRUE
   rc ;

\ Guards a constructor package a link carries: CT2 links ctor-sum.f's object and is rejected as
\ CT, which compiles it, is.
: CTOR-CASE ( CMD:command -- CMD:command )
   s" CT" WHICH!  CTOR-REJECTED {: cold:n :}  CTOR-SUM KEEP
   s" CT2" WHICH!  CTOR-REJECTED {: warm:n :}
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
   s" the image rejects the undefined export" LABEL
   CMD:ERR$ s\" error[E-UNDEFINED]: cannot find word `FILE-OBJECTS-EXP:GONE`\n --> <input>:5:1\n"
   STR:CONTAINS? TTRUE
   s" the image exits 70" LABEL  rc 70 T= ;

\ Guards an image's exports: EX compiles export-main.f into a --repl image, EX2 links its object
\ into another, and each resolves an exported private word, another package's public word, a
\ constant, a does> definer and an export whose source the file undefines, and rejects an
\ export the file undefines.
: EXPORT-CASE ( CMD:command -- CMD:command )
   s" EX" WHICH!  s" test/fixtures/file-objects/export-main.f" REPL-BUILD 0= if exit then
   EXPORT-RUN  EXPORT-MAIN KEEP
   s" EX2" WHICH!  s" test/fixtures/file-objects/export-main.f" REPL-BUILD 0= if exit then
   s" the build links export-main.f" EXPORT-MAIN LINKED  EXPORT-RUN ;


\ The path of U's source of that name.
: NEST-PATH$ ( str -- str ) {: s:str :}
   NEST-DIR NEST-DIR-U @ >str s NEST-PATH FS:JOIN-PATH {: v:n :}
   NEST-PATH v >str ;

: SRC+ ( str -- ) {: s:str :}
   s STR:DATA NEST-SRC NEST-SRC-U @ + s STR:LEN BYTE-COPY  s STR:LEN NEST-SRC-U +! ;

\ A line that requires U's source of that name by its absolute path.
: REQUIRE+ ( str -- ) s" require " SRC+  NEST-PATH$ SRC+  s\" \n" SRC+ ;

\ Writes the text to U's source of that name and starts the next text.
: NEST-WRITE ( str -- ) NEST-PATH$ NEST-SRC NEST-SRC-U @ >str FS:WRITE-ALL  0 NEST-SRC-U ! ;

\ x.f: FILE-OBJECTS-NESTED:K, whose value is the text, then a require of y.f, then a second package.
: WRITE-X ( str -- ) {: k:str :}
   s" package FILE-OBJECTS-NESTED public " SRC+  k SRC+  s\"  constant K ;package\n" SRC+
   s" y.f" REQUIRE+
   s\" package FILE-OBJECTS-NESTED-X public : X ( -- ) ; ;package\n" SRC+
   s" x.f" NEST-WRITE ;

: WRITE-NESTED ( -- )
   L$ s" nested" NEST-DIR FS:JOIN-PATH NEST-DIR-U !  NEST-DIR NEST-DIR-U @ >str FS:MAKE-DIR
   s\" package FILE-OBJECTS-NESTED-Y public : Y ( -- ) ; ;package\n" SRC+  s" y.f" NEST-WRITE
   s" 1" WRITE-X
   s" x.f" REQUIRE+  s\" : MAIN ( -- ) FILE-OBJECTS-NESTED:K . ;\n" SRC+  s" r.f" NEST-WRITE
   s" x.f" REQUIRE+  s\" : MAIN ( -- ) ;\n" SRC+  s" m.f" NEST-WRITE ;

\ tools/hb-build.f's build of a NEST-DIR source to L's program of that name, in a fresh hb with the out root
\ given, an image that keeps hb's REPL when the flag is true: the boundary under test is the object another
\ build wrote. Answers its exit code.
: OUT-RC ( CMD:command str str str bool -- CMD:command n )
   {: src:str prog:str out:str repl:bool :}
   CMD:RESET
   \ CMD:ENV+ copies the root before NEST-PATH$ and IN$ reuse the buffers it may lie in.
   s" HABU_OUT" out CMD:ENV+
   s" --load" CMD:ARG+  s" tools/hb-build.f" CMD:ARG+  s" --" CMD:ARG+
   repl if s" --repl" CMD:ARG+ then
   src NEST-PATH$ CMD:ARG+
   s" -o" CMD:ARG+  L$ prog IN$ CMD:ARG+
   ENGINE-CANDIDATE:PATH$ PROCESS:WAIT-FOREVER CMD:RUN-OUTCOME
   PROCESS:ENDING>RC RC>N ;

\ OUT-RC's build with L's directory as its out root.
: NEST-RC ( CMD:command str str -- CMD:command n ) L$ false OUT-RC ;

\ NEST-RC's build, true when it exits 0.
: NEST-BUILD ( CMD:command str str -- CMD:command bool )
   NEST-RC {: rc:n :}
   rc 0<> if rc SHOW then
   s" the build exits 0" LABEL  rc 0 T=
   rc 0 = ;

\ L's program of that name, run in its own process: the boundary under test is the written file.
\ The label holds when its stdout is the wanted text.
: NEST-RUN ( CMD:command str str str -- CMD:command )
   {: prog:str want:str label:str :}
   CMD:RESET
   L$ prog IN$ PROCESS:WAIT-FOREVER CMD:RUN-OUTCOME PROCESS:ENDING>RC RC>N {: rc:n :}
   CMD:OUT$ want STR:=  rc 0 =  and 0= if rc SHOW then
   label LABEL  CMD:OUT$ want T$= ;

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

\ The moved sequence's a.f and b.f: one defines FILE-OBJECTS-MOVED-W:W, answering the text, and
\ the other FILE-OBJECTS-MOVED-V:V; W's is b.f when the flag is true, else a.f.
: WRITE-MOVED-W ( str bool -- ) {: w:str moved:bool :}
   s" package FILE-OBJECTS-MOVED-W public : W ( -- n ) " SRC+  w SRC+  s\"  ; ;package\n" SRC+
   moved if s" b.f" else s" a.f" then NEST-WRITE
   s\" package FILE-OBJECTS-MOVED-V public : V ( -- n ) 0 ; ;package\n" SRC+
   moved if s" a.f" else s" b.f" then NEST-WRITE ;

\ four.f: requests of a.f, b.f, then A to D, each part but the last cut by a clean request, as
\ mid-four.f; six parts, only B's, +3, calls W. main.f's MAIN prints A to D.
: WRITE-MOVED ( -- )
   L$ s" moved" NEST-DIR FS:JOIN-PATH NEST-DIR-U !  NEST-DIR NEST-DIR-U @ >str FS:MAKE-DIR
   s\" package FILE-OBJECTS-MOVED-E1 public : E1 ( -- ) ; ;package\n" SRC+  s" e1.f" NEST-WRITE
   s\" package FILE-OBJECTS-MOVED-E2 public : E2 ( -- ) ; ;package\n" SRC+  s" e2.f" NEST-WRITE
   s\" package FILE-OBJECTS-MOVED-E3 public : E3 ( -- ) ; ;package\n" SRC+  s" e3.f" NEST-WRITE
   s" 1" false WRITE-MOVED-W
   s" a.f" REQUIRE+  s" b.f" REQUIRE+
   s\" package FILE-OBJECTS-MOVED-FOUR public : A ( -- n ) 10 ; ;package\n" SRC+  s" e1.f" REQUIRE+
   s\" package FILE-OBJECTS-MOVED-FOUR public : B ( -- n ) FILE-OBJECTS-MOVED-W:W ; ;package\n" SRC+
   s" e2.f" REQUIRE+
   s\" package FILE-OBJECTS-MOVED-FOUR public : C ( -- n ) 30 ; ;package\n" SRC+  s" e3.f" REQUIRE+
   s\" package FILE-OBJECTS-MOVED-FOUR public : D ( -- n ) 40 ; ;package\n" SRC+  s" four.f" NEST-WRITE
   s" four.f" REQUIRE+
   s\" : MAIN ( -- ) FILE-OBJECTS-MOVED-FOUR:A . FILE-OBJECTS-MOVED-FOUR:B .\n" SRC+
   s\"    FILE-OBJECTS-MOVED-FOUR:C . FILE-OBJECTS-MOVED-FOUR:D . ;\n" SRC+  s" main.f" NEST-WRITE ;

\ Guards a part calling a word that moves between the files its file requires: after W moves from
\ a.f to b.f, MV2 compiles four.f's part that calls W and links its last.
: MOVED-CASE ( CMD:command -- CMD:command )
   WRITE-MOVED
   s" MV" WHICH!  s" main.f" NEST-PATH$ FIXTURE-BUILD 0= if exit then
   MOVED-CALL KEEP  MOVED-LAST KEEP
   s" 2" true WRITE-MOVED-W
   s" MV2" WHICH!  s" main.f" NEST-PATH$ FIXTURE-BUILD 0= if exit then
   s" the build compiles four.f's part that calls W" MOVED-CALL COMPILED
   s" the build links four.f's last part" MOVED-LAST LINKED
   s\" 10\n2\n30\n40\n" s" the program prints four.f's numbers with b.f's W" PRINTS-RUN ;

\ The copy sequence's l.f, whose copy word W answers the text.
: WRITE-COPY-W ( str -- ) {: w:str :}
   s" package FILE-OBJECTS-COPY public : W ( -- n ) " SRC+  w SRC+  s\"  ; ;package\n" SRC+
   s" l.f" NEST-WRITE ;

\ x.f's X mentions W, so X's steps hold W's; main.f's MAIN prints X.
: WRITE-COPY ( -- )
   L$ s" copy" NEST-DIR FS:JOIN-PATH NEST-DIR-U !  NEST-DIR NEST-DIR-U @ >str FS:MAKE-DIR
   s" 3 5 +" WRITE-COPY-W
   s" l.f" REQUIRE+
   s\" package FILE-OBJECTS-COPY-X public : X ( -- n ) FILE-OBJECTS-COPY:W 1+ ; ;package\n" SRC+
   s" x.f" NEST-WRITE
   s" x.f" REQUIRE+  s\" : MAIN ( -- ) FILE-OBJECTS-COPY-X:X . ;\n" SRC+  s" main.f" NEST-WRITE ;

\ Guards the copy words' steps an image keeps after their code: after W's steps change and its code
\ does not, CP2's image, which links hb's files, has the bytes of CP3's, a cold build's, and prints 9.
: COPY-CASE ( CMD:command -- CMD:command )
   WRITE-COPY
   s" CP" WHICH!  s" main.f" s" prog-cp" F$ true OUT-RC BUILT? 0= if exit then
   s" 8" WRITE-COPY-W
   s" CP2" WHICH!  s" main.f" s" prog-cp" F$ true OUT-RC BUILT? 0= if exit then
   s" prog-cp" s\" 9\n" s" the image prints X" NEST-RUN
   s" cold" NEST-PATH$ FS:MAKE-DIR
   s" CP3" WHICH!  s" main.f" s" prog-cp-cold" s" cold" NEST-PATH$ true OUT-RC BUILT? 0= if exit then
   s" the image has a cold build's bytes" LABEL  L$ s" prog-cp" L$ s" prog-cp-cold" SAME-FILE? TTRUE ;

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
   make if s" create 0 , " else s" parse-name drop " then SRC+
   s" ; " SRC+  make if s" true" else s" false" then SRC+
   s\"  constant FLAG ;package\n" SRC+  s" g.f" NEST-WRITE ;

\ f.f's second part makes private Y and X when g.f's DEF makes them and a private definer MK when g.f's
\ FLAG is true; its third part has two Y, X and MK, whose X answer 10 and 20 and MK add 0 and 100.
: WRITE-SAME ( -- )
   L$ s" same-name" NEST-DIR FS:JOIN-PATH NEST-DIR-U !  NEST-DIR NEST-DIR-U @ >str FS:MAKE-DIR
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
   L$ s" retire" NEST-DIR FS:JOIN-PATH NEST-DIR-U !  NEST-DIR NEST-DIR-U @ >str FS:MAKE-DIR
   s\" : FILE-OBJECTS-RETIRE-W ( -- n ) 1 ;\npackage FILE-OBJECTS-RETIRE public\n" SRC+
   s\" : FOUND ( -- n ) s\" FILE-OBJECTS-RETIRE-W\" get-current search-wl 0= if 0 else 1 then ;\n" SRC+
   s\" ;package\n" SRC+  s" h.f" NEST-WRITE
   true WRITE-RETIRE-S
   s" h.f" REQUIRE+
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

\ The primitive sequence's main.f, which spells u.f's dup; the flag adds a comment line.
: WRITE-PRIMITIVE-MAIN ( bool -- ) {: edited:bool :}
   s" u.f" REQUIRE+
   edited if s\" \\ edited\n" SRC+ then
   s\" : MAIN ( -- ) 5 dup . ;\n" SRC+  s" main.f" NEST-WRITE ;

\ u.f retires hb's dup and defines its own, which main.f calls.
: WRITE-PRIMITIVE ( -- )
   L$ s" primitive" NEST-DIR FS:JOIN-PATH NEST-DIR-U !  NEST-DIR NEST-DIR-U @ >str FS:MAKE-DIR
   s\" undefine dup\n: dup ( n -- n ) 100 + ;\n" SRC+  s" u.f" NEST-WRITE
   false WRITE-PRIMITIVE-MAIN ;

\ Guards a retirement of hb's own record: P compiles u.f, whose object names hb's dup; P2, after
\ main.f's edit, links u.f's object, retiring hb's dup again, and compiles main.f's two parts alone.
: PRIMITIVE-CASE ( CMD:command -- CMD:command )
   WRITE-PRIMITIVE
   s" P" WHICH!  s" main.f" s" prog-p" NEST-BUILD 0= if exit then
   s" prog-p" s\" 105\n" s" the program prints u.f's dup of 5" NEST-RUN  L$ MARK
   true WRITE-PRIMITIVE-MAIN
   s" P2" WHICH!  s" main.f" s" prog-p" NEST-BUILD 0= if exit then
   L$ REWRITTEN {: n:n :}
   s" build two rewrites main.f's two objects alone" LABEL  n 2 T=
   s" prog-p" s\" 105\n" s" the program prints u.f's dup of 5" NEST-RUN ;

\ The hb-retire sequence's s.f: empty, or an undefine of hb's own dup when the flag is true.
: WRITE-HB-S ( bool -- ) {: retire:bool :}
   retire if s\" undefine dup\n" SRC+ then
   s" s.f" NEST-WRITE ;

\ main.f: a require of s.f, then the text.
: WRITE-HB-MAIN ( str -- ) {: m:str :}
   s" s.f" REQUIRE+  m SRC+  s" main.f" NEST-WRITE ;

: WRITE-HB-RETIRE ( -- )
   L$ s" hb-retire" NEST-DIR FS:JOIN-PATH NEST-DIR-U !  NEST-DIR NEST-DIR-U @ >str FS:MAKE-DIR
   false WRITE-HB-S
   s\" : MAIN ( -- ) 1 dup + . cr ;\n" WRITE-HB-MAIN ;

\ main.f's build with the out root given, rejected at its dup, which s.f retired. Answers its exit code.
: HB-UNDEFINED ( CMD:command str -- CMD:command n )
   {: out:str :}
   s" main.f" s" prog-h" out false OUT-RC {: rc:n :}
   CMD:ERR$ s" error[E-UNDEFINED]: cannot find word `dup`" STR:CONTAINS? {: coded:bool :}
   CMD:ERR$ s\" /main.f:2:17\n" STR:CONTAINS? coded and {: named:bool :}
   named  rc 0<>  and 0= if rc SHOW then
   s" the build rejects main.f's dup" LABEL  named TTRUE
   rc ;

\ Guards a file's retirement of one of hb's own words: H2 and H3, warm and cold, reject main.f's dup;
\ H4 links s.f and prints 1; H5, with nothing changed, links every file; H6 links s.f's retirement and rejects dup.
: HB-RETIRE-CASE ( CMD:command -- CMD:command )
   WRITE-HB-RETIRE
   s" H" WHICH!  s" main.f" s" prog-h" NEST-BUILD 0= if exit then
   s" prog-h" s\" 2\n\n" s" the program prints 2" NEST-RUN
   true WRITE-HB-S
   s" H2" WHICH!  L$ HB-UNDEFINED {: warm:n :}
   s" cold" NEST-PATH$ FS:MAKE-DIR
   s" H3" WHICH!  s" cold" NEST-PATH$ HB-UNDEFINED {: cold:n :}
   s" the build exits as H2 does" LABEL  cold warm T=
   s\" : MAIN ( -- ) 1 . cr ;\n" WRITE-HB-MAIN
   s" H4" WHICH!  s" main.f" s" prog-h" NEST-BUILD 0= if exit then
   s" prog-h" s\" 1\n\n" s" the program prints 1" NEST-RUN  L$ MARK
   s" H5" WHICH!  s" main.f" s" prog-h" NEST-BUILD 0= if exit then
   L$ REWRITTEN {: n:n :}
   s" build five rewrites no object" LABEL  n 0 T=
   s" prog-h" s\" 1\n\n" s" the program prints 1" NEST-RUN  L$ MARK
   s\" : MAIN ( -- ) 1 dup + . cr ;\n" WRITE-HB-MAIN
   s" H6" WHICH!  L$ HB-UNDEFINED drop
   L$ REWRITTEN {: n6:n :}
   s" build six rewrites only main.f's first object" LABEL  n6 1 T=
   s" the object it rewrites is main.f's" LABEL  FOUND$ s" /main.f.hfo" STR:ENDS-WITH? TTRUE ;

\ The probe sequence's s.f: a package with no store, then the text.
: WRITE-PROBE-S ( str -- ) {: t:str :}
   s\" package FILE-OBJECTS-PROBE-S public variable Z ;package\n" SRC+  t SRC+
   s" s.f" NEST-WRITE ;

\ The probe sequence in directory d: r.f's K is 1 when h.f's lookup q, the code after the name w's
\ string, finds w as r.f loads, else 0; no source spells w but s.f's text t.
: WRITE-PROBE ( str str str str -- ) {: d:str w:str q:str t:str :}
   L$ d NEST-DIR FS:JOIN-PATH NEST-DIR-U !  NEST-DIR NEST-DIR-U @ >str FS:MAKE-DIR
   s\" package FILE-OBJECTS-PROBE public\n: FOUND ( -- n ) s\" " SRC+  w SRC+  s\" \" " SRC+
   q SRC+  s\"  ;\n;package\n" SRC+
   s" h.f" NEST-WRITE
   t WRITE-PROBE-S
   s" h.f" REQUIRE+
   s\" FILE-OBJECTS-PROBE:FOUND constant FILE-OBJECTS-PROBE-K\n" SRC+  s" r.f" NEST-WRITE
   s" h.f" REQUIRE+  s" s.f" REQUIRE+  s" r.f" REQUIRE+
   s\" : MAIN ( -- ) FILE-OBJECTS-PROBE-K . cr ;\n" SRC+  s" main.f" NEST-WRITE ;

: WL-PROBE$ ( -- str ) s" get-current CHECKER-RESOLVE:WL-PROBE NULL-PTR = if 0 else 1 then" ;

\ Guards a lookup of a name no source spells: P2, after s.f stops retiring the engine's >U8, and
\ Q2, after s.f drops the word h.f probes, must compile r.f again and print what a cold build prints.
: PROBE-CASE ( CMD:command -- CMD:command )
   s" probe" s" >U8" WL-PROBE$ s\" undefine >U8\n" WRITE-PROBE
   s" P" WHICH!  s" main.f" s" prog-p" NEST-BUILD 0= if exit then
   s" prog-p" s\" 0\n\n" s" the program prints K" NEST-RUN
   s" " WRITE-PROBE-S
   s" P2" WHICH!  s" main.f" s" prog-p" NEST-BUILD 0= if exit then
   s" prog-p" s\" 1\n\n" s" the program prints K" NEST-RUN
   s" probe-zz" s" FILE-OBJECTS-PROBE-ZZ" WL-PROBE$ s\" : FILE-OBJECTS-PROBE-ZZ ( -- ) ;\n" WRITE-PROBE
   s" Q" WHICH!  s" main.f" s" prog-q" NEST-BUILD 0= if exit then
   s" prog-q" s\" 1\n\n" s" the program prints K" NEST-RUN
   s" " WRITE-PROBE-S
   s" Q2" WHICH!  s" main.f" s" prog-q" NEST-BUILD 0= if exit then
   s" prog-q" s\" 0\n\n" s" the program prints K" NEST-RUN ;

\ The probe sequence of directory d through lookup q: cold it prints 0, and warm build w, after s.f
\ stops retiring >U8, prints 1 as a cold build does.
: LOOKUP-RUN ( CMD:command str str str -- CMD:command ) {: d:str q:str w:str :}
   d s" >U8" q s\" undefine >U8\n" WRITE-PROBE
   d WHICH!  s" main.f" s" prog-l" NEST-BUILD 0= if exit then
   s" prog-l" s\" 0\n\n" s" the program prints K" NEST-RUN
   s" " WRITE-PROBE-S
   w WHICH!  s" main.f" s" prog-l" NEST-BUILD 0= if exit then
   s" prog-l" s\" 1\n\n" s" the program prints K" NEST-RUN ;

: SEARCH-WL-CASE ( CMD:command -- CMD:command )
   s" probe-swl" s" get-current search-wl 0= if 0 else 1 then" s" probe-swl2" LOOKUP-RUN ;

: SCOPE-FIND-CASE ( CMD:command -- CMD:command )
   s" probe-find" s" scope-find drop drop drop NULL-PTR = if 0 else 1 then" s" probe-find2"
   LOOKUP-RUN ;

\ The public-lookup sequence's s.f: package FILE-OBJECTS-QS with a private HELPER of value n.
: WRITE-PUBLIC-S ( str -- ) {: n:str :}
   s" package FILE-OBJECTS-QS : HELPER ( -- n ) " SRC+  n SRC+
   s\"  ; public : PUB ( -- n ) 3 ; ;package\n" SRC+  s" s.f" NEST-WRITE ;

\ r.f has a private HELPER of its own and calls only FILE-OBJECTS-QS:PUB.
: WRITE-PUBLIC ( -- )
   L$ s" public" NEST-DIR FS:JOIN-PATH NEST-DIR-U !  NEST-DIR NEST-DIR-U @ >str FS:MAKE-DIR
   s" 1" WRITE-PUBLIC-S
   s" s.f" REQUIRE+  s\" package FILE-OBJECTS-QR : HELPER ( -- n ) 5 ;\n" SRC+
   s\" public : K ( -- n ) HELPER FILE-OBJECTS-QS:PUB + ; ;package\n" SRC+  s" r.f" NEST-WRITE
   s" r.f" REQUIRE+  s\" : MAIN ( -- ) FILE-OBJECTS-QR:K . cr ;\n" SRC+  s" main.f" NEST-WRITE ;

\ Guards a lookup in a package's public wordlist, which reaches none of its private names: after QS's
\ private HELPER changes, PL2 compiles only s.f, and r.f, which spells a HELPER of its own, links.
: PUBLIC-LOOKUP-CASE ( CMD:command -- CMD:command )
   WRITE-PUBLIC
   s" PL" WHICH!  s" main.f" s" prog-pl" NEST-BUILD 0= if exit then  L$ MARK
   s" 2" WRITE-PUBLIC-S
   s" PL2" WHICH!  s" main.f" s" prog-pl" NEST-BUILD 0= if exit then
   L$ REWRITTEN {: n:n :}
   s" build two rewrites only s.f's object" LABEL  n 1 T=
   s" the object it rewrites is s.f's" LABEL  FOUND$ s" /s.f.hfo" STR:ENDS-WITH? TTRUE
   s" prog-pl" s\" 8\n\n" s" the program prints K" NEST-RUN ;

\ The clause sequence: h.f's definer FILE-OBJECTS-CL has a does> clause, w.f defines a word of the
\ clause's name, one.f requires h.f, and two.f w.f then h.f.
: WRITE-CLAUSE ( -- )
   L$ s" clause" NEST-DIR FS:JOIN-PATH NEST-DIR-U !  NEST-DIR NEST-DIR-U @ >str FS:MAKE-DIR
   s\" : FILE-OBJECTS-CL ( -- ) create does> ( -- ) drop ;\n" SRC+  s" h.f" NEST-WRITE
   s\" : FILE-OBJECTS-CL;does ( -- ) ;\n" SRC+  s" w.f" NEST-WRITE
   s" h.f" REQUIRE+  s\" : MAIN ( -- ) ;\n" SRC+  s" one.f" NEST-WRITE
   s" w.f" REQUIRE+  s" h.f" REQUIRE+  s\" : MAIN ( -- ) ;\n" SRC+  s" two.f" NEST-WRITE ;

\ two.f's build, rejected at h.f's FILE-OBJECTS-CL since w.f's word holds the name of its
\ clause. Answers its exit code.
: CLAUSE-REJECTED ( CMD:command -- CMD:command n )
   s" two.f" s" prog-c" NEST-RC {: rc:n :}
   CMD:ERR$ s" error[E-DUPLICATE-DEFINITION]: `FILE-OBJECTS-CL;does`" STR:CONTAINS? {: coded:bool :}
   CMD:ERR$ S\" /h.f:1:3\n" STR:CONTAINS? coded and {: named:bool :}
   named  rc 0<>  and 0= if rc SHOW then
   s" the build rejects the clause's name" LABEL  named TTRUE
   rc ;

\ Guards a does> clause a link places: CL2 compiles h.f with no word of its clause's name live. h.f's
\ object records that name, so in CL3 w.f's word makes it not hold: h.f compiles and is rejected as in CL.
: CLAUSE-CASE ( CMD:command -- CMD:command )
   WRITE-CLAUSE
   s" CL" WHICH!  CLAUSE-REJECTED {: cold:n :}
   s" CL2" WHICH!  s" one.f" s" prog-c" NEST-BUILD 0= if exit then
   s" CL3" WHICH!  CLAUSE-REJECTED {: warm:n :}
   s" the build exits as CL does" LABEL  warm cold T= ;

\ The derived sequence: h.f's definer FILE-OBJECTS-DV defines its argument's name plus "-W", spelled
\ nowhere, w.f defines a word of that name, one.f requires h.f, and two.f w.f then h.f.
: WRITE-DERIVED ( -- )
   L$ s" derived" NEST-DIR FS:JOIN-PATH NEST-DIR-U !  NEST-DIR NEST-DIR-U @ >str FS:MAKE-DIR
   s\" : FILE-OBJECTS-DV ( str -- )\n   SB-RESET SB-APPEND s\" -W\" SB-APPEND\n" SRC+
   s\"    SB$ EFFECT:EMPTY define-word  postpone ; ;\ns\" FILE-OBJECTS-DX\" FILE-OBJECTS-DV\n" SRC+
   s" h.f" NEST-WRITE
   s\" : FILE-OBJECTS-DX-W ( -- ) ;\n" SRC+  s" w.f" NEST-WRITE
   s" h.f" REQUIRE+  s\" : MAIN ( -- ) ;\n" SRC+  s" one.f" NEST-WRITE
   s" w.f" REQUIRE+  s" h.f" REQUIRE+  s\" : MAIN ( -- ) ;\n" SRC+  s" two.f" NEST-WRITE ;

\ two.f's build, rejected at h.f's use of FILE-OBJECTS-DV since w.f's word holds the name it
\ derives. Answers its exit code.
: DERIVED-REJECTED ( CMD:command -- CMD:command n )
   s" two.f" s" prog-dr" NEST-RC {: rc:n :}
   CMD:ERR$ s" error[E-DUPLICATE-DEFINITION]: `FILE-OBJECTS-DX-W`" STR:CONTAINS? {: coded:bool :}
   CMD:ERR$ S\" /h.f:4:21\n" STR:CONTAINS? coded and {: named:bool :}
   named  rc 0<>  and 0= if rc SHOW then
   s" the build rejects the derived name" LABEL  named TTRUE
   rc ;

\ Guards a record a definer names by deriving: DR2 compiles h.f with no word of that name live.
\ h.f's object records the name, so in DR3 w.f's word makes it not hold: h.f is rejected as in DR.
: DERIVED-CASE ( CMD:command -- CMD:command )
   WRITE-DERIVED
   s" DR" WHICH!  DERIVED-REJECTED {: cold:n :}
   s" DR2" WHICH!  s" one.f" s" prog-dr" NEST-BUILD 0= if exit then
   s" DR3" WHICH!  DERIVED-REJECTED {: warm:n :}
   s" the build exits as DR does" LABEL  warm cold T= ;

\ The read sequence's d.txt, the text f.f reads as it loads.
: WRITE-DATA ( str -- ) SRC+  s" d.txt" NEST-WRITE ;

\ f.f's K is d.txt's first byte, which it reads by its absolute path through FS:READ-ALL.
: WRITE-READ ( -- )
   L$ s" read" NEST-DIR FS:JOIN-PATH NEST-DIR-U !  NEST-DIR NEST-DIR-U @ >str FS:MAKE-DIR
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
   L$ s" list" NEST-DIR FS:JOIN-PATH NEST-DIR-U !  NEST-DIR NEST-DIR-U @ >str FS:MAKE-DIR
   s" e" NEST-PATH$ FS:MAKE-DIR
   s" 1" SRC+  s" e/a.txt" NEST-WRITE
   s\" require src/lib/fs-list.f\npackage FILE-OBJECTS-LIST\nvariable N\n: COUNTED ( -- ) s\" " SRC+
   s" e" NEST-PATH$ SRC+
   s\" \" [: str> 2drop 1 N +! ;] FS-LIST:EACH ;\nCOUNTED public N @ constant K ;package\n" SRC+
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
   L$ s" link" NEST-DIR FS:JOIN-PATH NEST-DIR-U !  NEST-DIR NEST-DIR-U @ >str FS:MAKE-DIR
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
   L$ s" task" NEST-DIR FS:JOIN-PATH NEST-DIR-U !  NEST-DIR NEST-DIR-U @ >str FS:MAKE-DIR
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
   L$ s" samefile" NEST-DIR FS:JOIN-PATH NEST-DIR-U !  NEST-DIR NEST-DIR-U @ >str FS:MAKE-DIR
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
   L$ s" anchored" NEST-DIR FS:JOIN-PATH NEST-DIR-U !  NEST-DIR NEST-DIR-U @ >str FS:MAKE-DIR
   s" 1" WRITE-DATA
   s\" require src/lib/fs-anchored.f\npackage FILE-OBJECTS-ANCHORED\n1 SPAN-BUFFER: B\n" SRC+
   s\" : READ-D ( -- n ) s\" " SRC+
   NEST-DIR NEST-DIR-U @ >str FS:>PATHZ NEST-PATH PATH-CAP realpath {: real:n :}
   NEST-PATH real >str SRC+
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
   L$ s" spawn" NEST-DIR FS:JOIN-PATH NEST-DIR-U !  NEST-DIR NEST-DIR-U @ >str FS:MAKE-DIR
   s" 1" WRITE-DATA
   s\" require src/lib/process-command.f\nrequire src/lib/process.f\npackage FILE-OBJECTS-SPAWN\nvariable N\n" SRC+
   s\" : CAT ( CMD:command -- CMD:command ) s\" " SRC+  s" d.txt" NEST-PATH$ SRC+
   s\" \" CMD:ARG+ s\" /bin/cat\" PROCESS:WAIT-FOREVER 1 CMD:RUN CMD:OUT$ str> drop c@ N ! ;\n" SRC+
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
   L$ s" zip" NEST-DIR FS:JOIN-PATH NEST-DIR-U !  NEST-DIR NEST-DIR-U @ >str FS:MAKE-DIR
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

\ The kind sequence's f.f: K is whether k is a directory, which it asks by its absolute path through FS:DIR?.
: WRITE-KIND ( -- )
   L$ s" kind" NEST-DIR FS:JOIN-PATH NEST-DIR-U !  NEST-DIR NEST-DIR-U @ >str FS:MAKE-DIR
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
   L$ s" arena" NEST-DIR FS:JOIN-PATH NEST-DIR-U !  NEST-DIR NEST-DIR-U @ >str FS:MAKE-DIR
   s\" require src/lib/arena.f\npackage FILE-OBJECTS-ARENA\npublic\n" SRC+
   s\" TYPED-VARIABLE A ^ARENA:arena\nARENA:NEW A !\n;package\n" SRC+  s" kt.f" NEST-WRITE
   s" kt.f" REQUIRE+
   s\" : MAIN ( -- ) FILE-OBJECTS-ARENA:A @ NULL-PTR = bool>n . ;\n" SRC+
   s" kt-main.f" NEST-WRITE
   s\" require src/lib/arena.f\npackage FILE-OBJECTS-ARENA\npublic\n" SRC+
   s\" PTR-VARIABLE A\nARENA:NEW 8 ARENA:ALLOC drop A !\n;package\n" SRC+  s" kb.f" NEST-WRITE
   s" kb.f" REQUIRE+  s\" : KEPT ( -- ^u8 ) FILE-OBJECTS-ARENA:A @ ;\n" SRC+
   s\" : MAIN ( -- ) KEPT NULL-PTR = bool>n . ;\n" SRC+  s" kb-main.f" NEST-WRITE
   s\" require src/lib/arena.f\npackage FILE-OBJECTS-DROPPED\nARENA:NEW drop\n;package\n" SRC+
   s" da.f" NEST-WRITE
   s" da.f" REQUIRE+  s\" : MAIN ( -- ) 0 . ;\n" SRC+  s" da-main.f" NEST-WRITE
   s\" package FILE-OBJECTS-HEAP\nHEAP H n\npublic\nTYPED-VARIABLE Q ^n\nH-NEW Q !\n;package\n"
   SRC+  s" ht.f" NEST-WRITE
   s" ht.f" REQUIRE+
   s\" : MAIN ( -- ) FILE-OBJECTS-HEAP:Q @ NULL-PTR = bool>n . ;\n" SRC+
   s" ht-main.f" NEST-WRITE
   s\" : MAIN ( -- ) ;\n" SRC+  s" o.f" NEST-WRITE ;

\ The main's build, rejected at link by one record placed at the fixture's file:line:col over its source line,
\ with the cell that holds the address, what it found and the fix. Answers its exit code.
: ARENA-REJECTED ( CMD:command str str str str -- CMD:command n )
   {: main:str f:str line:str cell:str :}
   main s" prog-ar" NEST-RC {: rc:n :}
   CMD:ERR$ f NEST-PATH$ STR:CONTAINS? {: named:bool :}
   CMD:ERR$ line STR:CONTAINS? {: shown:bool :}
   CMD:ERR$ cell STR:CONTAINS? {: celled:bool :}
   CMD:ERR$ s" outside hb's data and the build's space" STR:CONTAINS? {: found:bool :}
   CMD:ERR$ s" or clear the cell before the file ends" STR:CONTAINS? {: fixed:bool :}
   CMD:ERR$ s" error[" STR:COUNT-SUB 1 = {: one:bool :}
   named shown and celled and found and fixed and one and  rc 0<> and 0= if rc SHOW then
   s" the build fails" LABEL  rc 0<> TTRUE
   s" the rejection is placed at the declaration" LABEL  named TTRUE
   s" the rejection shows the declaration's line" LABEL  shown TTRUE
   s" the rejection names the cell" LABEL  celled TTRUE
   s" the rejection says what it found" LABEL  found TTRUE
   s" the rejection gives the fix" LABEL  fixed TTRUE
   s" the build prints one record" LABEL  one TTRUE
   rc ;

: ARENA-OTHER ( CMD:command -- CMD:command ) s" o.f" s" prog-ao" NEST-BUILD drop ;

: ARENA-KT ( CMD:command -- CMD:command )
   s" AR" WHICH!  s" kt-main.f" s" kt.f:4:16" s" | TYPED-VARIABLE A ^ARENA:arena"
   S\" in A +0\n" ARENA-REJECTED {: cold:n :}
   s" AR2" WHICH!  ARENA-OTHER
   s" AR3" WHICH!  s" kt-main.f" s" kt.f:4:16" s" | TYPED-VARIABLE A ^ARENA:arena"
   S\" in A +0\n" ARENA-REJECTED {: warm:n :}
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
   s" AR10" WHICH!  s" ht-main.f" s" ht.f:4:16" s" | TYPED-VARIABLE Q ^n"
   S\" in Q +0\n" ARENA-REJECTED {: cold:n :}
   s" AR11" WHICH!  ARENA-OTHER
   s" AR12" WHICH!  s" ht-main.f" s" ht.f:4:16" s" | TYPED-VARIABLE Q ^n"
   S\" in Q +0\n" ARENA-REJECTED {: warm:n :}
   s" the build exits as AR10 does" LABEL  warm cold T= ;

\ Guards that a build gives a program no address of memory a top level mapped.
: ARENA-CASE ( CMD:command -- CMD:command )
   WRITE-ARENA  ARENA-KT ARENA-KB ARENA-DA ARENA-HT ;

\ The +FIELD sequence: pf.f's package publishes PF.X and PF.Y; pf-main.f's
\ MAIN stores x and its successor through them and prints both.
: WRITE-PF-MAIN ( str -- ) {: x:str :}
   s" pf.f" REQUIRE+  s\" create PF-BUF FILE-OBJECTS-PFIELD:PF-SIZE allot\n" SRC+
   s" : MAIN ( -- ) " SRC+  x SRC+
   s\"  dup PF-BUF FILE-OBJECTS-PFIELD:PF.X ! 1 + PF-BUF FILE-OBJECTS-PFIELD:PF.Y !\n" SRC+
   s\"    PF-BUF FILE-OBJECTS-PFIELD:PF.X @ . PF-BUF FILE-OBJECTS-PFIELD:PF.Y @ . ;\n" SRC+
   s" pf-main.f" NEST-WRITE ;

: WRITE-PFIELD ( -- )
   L$ s" pfield" NEST-DIR FS:JOIN-PATH NEST-DIR-U !  NEST-DIR NEST-DIR-U @ >str FS:MAKE-DIR
   s\" package FILE-OBJECTS-PFIELD\npublic\nBEGIN-STRUCTURE PF-SIZE\n" SRC+
   s\" CELL +FIELD PF.X\nCELL +FIELD PF.Y\nEND-STRUCTURE\n;package\n" SRC+  s" pf.f" NEST-WRITE
   s" 3" WRITE-PF-MAIN ;

\ Guards a compiled file naming a linked file's +FIELD children: FD2 links pf.f and compiles pf-main.f.
: PFIELD-CASE ( CMD:command -- CMD:command )
   WRITE-PFIELD
   s" FD" WHICH!  s" pf-main.f" NEST-PATH$ FIXTURE-BUILD 0= if exit then
   s\" 3\n4\n" s" the program prints both fields" PRINTS-RUN
   PFIELD-PF KEEP  PFIELD-MAIN KEEP
   s" 5" WRITE-PF-MAIN
   s" FD2" WHICH!  s" pf-main.f" NEST-PATH$ FIXTURE-BUILD 0= if exit then
   s" the build links pf.f" PFIELD-PF LINKED
   s" the build compiles pf-main.f" PFIELD-MAIN COMPILED
   s\" 5\n6\n" s" the program prints both new fields" PRINTS-RUN ;

\ The MUTUAL sequence: mt.f's package declares mtnode and mtlink, which point at
\ each other, mtlink holding the closed mtnode by value; mt-main.f's mtuser holds
\ an mtlink by value, its MAIN prints 7, and the text given follows.
: WRITE-MT-MAIN ( str -- ) {: c:str :}
   s" mt.f" REQUIRE+  s\" using FILE-OBJECTS-MUTUAL\n" SRC+
   s\" STRUCTURE mtuser DERIVE addr\n   FIELD l mtlink\n;STRUCTURE\n" SRC+
   s\" : MAIN ( -- ) 7 . ;\n" SRC+  c SRC+  s" mt-main.f" NEST-WRITE ;

: WRITE-MUTUAL ( -- )
   L$ s" mutual" NEST-DIR FS:JOIN-PATH NEST-DIR-U !  NEST-DIR NEST-DIR-U @ >str FS:MAKE-DIR
   s\" package FILE-OBJECTS-MUTUAL\npublic\nMUTUAL mtnode mtlink\n" SRC+
   s\" STRUCTURE mtnode DERIVE addr\n   FIELD id n\n   FIELD first ^mtlink\n;STRUCTURE\n" SRC+
   s\" STRUCTURE mtlink DERIVE addr\n   FIELD owner ^mtnode\n   FIELD head mtnode\n" SRC+
   s\"    FIELD next ^mtlink\n;STRUCTURE\n;MUTUAL\n;package\n" SRC+  s" mt.f" NEST-WRITE
   s" " WRITE-MT-MAIN ;

\ MT3: tools/hb-build.f's build of mt-main.f to L's prog-mt, in a fresh hb whose out root,
\ mutual/out, is empty: the boundary under test is a build that reads no object.
: MUTUAL-COLD ( CMD:command -- CMD:command bool )
   s" out" NEST-PATH$ FS:MAKE-DIR
   CMD:RESET
   s" --load" CMD:ARG+  s" tools/hb-build.f" CMD:ARG+
   s" --" CMD:ARG+  s" mt-main.f" NEST-PATH$ CMD:ARG+
   s" -o" CMD:ARG+  L$ s" prog-mt" IN$ CMD:ARG+
   s" HABU_OUT" s" out" NEST-PATH$ CMD:ENV+
   ENGINE-CANDIDATE:PATH$ PROCESS:WAIT-FOREVER CMD:RUN-OUTCOME
   PROCESS:ENDING>RC RC>N BUILT? ;

\ Guards a warm build's read of type entries that name each other: MT2 links
\ mt.f, whose mtlink entry holds mtnode by value, compiles mt-main.f after a
\ comment is added, and writes the program and map that MT3, a cold build of
\ the same files, writes.
: MUTUAL-CASE ( CMD:command -- CMD:command )
   WRITE-MUTUAL
   s" MT" WHICH!  s" mt-main.f" NEST-PATH$ FIXTURE-BUILD 0= if exit then
   s\" 7\n" s" the program prints 7" PRINTS-RUN
   MUTUAL-TYPES KEEP  MUTUAL-MAIN KEEP
   s\" \\ edited\n" WRITE-MT-MAIN
   s" MT2" WHICH!  s" mt-main.f" NEST-PATH$ FIXTURE-BUILD 0= if exit then
   s" the build links mt.f" MUTUAL-TYPES LINKED
   s" the build compiles mt-main.f" MUTUAL-MAIN COMPILED
   s" MT3" WHICH!  MUTUAL-COLD 0= if exit then
   s" the program has MT3's bytes" LABEL  L$ s" prog" L$ s" prog-mt" SAME-FILE? TTRUE
   s" the map has MT3's bytes" LABEL  L$ s" prog.map" L$ s" prog-mt.map" SAME-FILE? TTRUE ;

\ The divisor sequence's dz-callee.f: package FILE-OBJECTS-DIVISOR's public
\ HALVE, which divides by its top input when the flag is true, else drops it.
: WRITE-HALVE ( bool -- )
   {: div:bool :}
   s\" package FILE-OBJECTS-DIVISOR\npublic\n: HALVE ( n n -- n ) " SRC+
   div if s" /" SRC+ else s" drop" SRC+ then
   s\"  ;\n;package\n" SRC+  s" dz-callee.f" NEST-WRITE ;

\ dz-main.f's MAIN prints 7 D H2, D the text given.
: WRITE-DZ-MAIN ( str -- ) {: d:str :}
   s" dz-mid.f" REQUIRE+
   s" : MAIN ( -- ) 7 " SRC+  d SRC+  s\"  H2 . ;\n" SRC+
   s" dz-main.f" NEST-WRITE ;

\ dz-mid.f's H2 calls HALVE, and dz-main.f's MAIN prints 7 0 H2.
: WRITE-DIVISOR ( -- )
   L$ s" divisor" NEST-DIR FS:JOIN-PATH NEST-DIR-U !
   NEST-DIR NEST-DIR-U @ >str FS:MAKE-DIR
   true WRITE-HALVE
   s" dz-callee.f" REQUIRE+
   s\" : H2 ( n n -- n ) FILE-OBJECTS-DIVISOR:HALVE ;\n" SRC+
   s" dz-mid.f" NEST-WRITE
   s" 0" WRITE-DZ-MAIN ;

\ dz-main.f's build, rejected where MAIN compiles: a part the build linked
\ would not be checked again. Answers its exit code.
: DIVISOR-REJECTED ( CMD:command -- CMD:command n )
   s" dz-main.f" s" prog-dz" NEST-RC {: rc:n :}
   CMD:ERR$ s" error[E-DIV-ZERO]: zero divisor: a literal 0 divisor has no quotient"
      STR:CONTAINS? {: said:bool :}
   CMD:ERR$ s" 2 | : MAIN ( -- ) 7 0 H2 . ;"
      STR:CONTAINS? {: compiled:bool :}
   CMD:ERR$ s" dz-main.f" NEST-PATH$ STR:CONTAINS? {: named:bool :}
   said compiled and named and  rc 0<> and 0= if rc SHOW then
   s" the build fails" LABEL  rc 0<> TTRUE
   s" the rejection gives the zero divisor's reason" LABEL  said TTRUE
   s" the build compiles dz-main.f's MAIN" LABEL  compiled TTRUE
   s" the rejection names dz-main.f" LABEL  named TTRUE
   rc ;

\ Guards that a definition's facts text carries the inputs its body divides
\ by: H2's source never changes, so in DZ3, where HALVE divides again, only
\ H2's facts text has MAIN, linked in DZ2 against a HALVE that drops, compiled
\ again; DZ5 edits only MAIN, whose call H2's facts text rejects.
: DIVISOR-CASE ( CMD:command -- CMD:command )
   WRITE-DIVISOR
   s" DZ" WHICH!  DIVISOR-REJECTED {: cold:n :}
   false WRITE-HALVE
   s" DZ2" WHICH!  s" dz-main.f" s" prog-dz" NEST-BUILD 0= if exit then
   true WRITE-HALVE
   s" DZ3" WHICH!  DIVISOR-REJECTED {: warm:n :}
   s" the build exits as DZ does" LABEL  warm cold T=
   s" 1" WRITE-DZ-MAIN
   s" DZ4" WHICH!  s" dz-main.f" s" prog-dz" NEST-BUILD 0= if exit then
   s" 0" WRITE-DZ-MAIN
   s" DZ5" WHICH!  DIVISOR-REJECTED {: edit:n :}
   s" the build exits as DZ does" LABEL  edit cold T= ;

\ The route sequence's f.f: its top level makes D, a device whose emit and write add their byte
\ counts to N, the output, types to it and leaves it there, and stores 7 into the user row U.
\ main.f's MAIN prints U, N, how making D the output ends, and N.
: WRITE-ROUTE ( -- )
   L$ s" route" NEST-DIR FS:JOIN-PATH NEST-DIR-U !  NEST-DIR NEST-DIR-U @ >str FS:MAKE-DIR
   s\" require src/lib/genio-core.f\nrequire src/lib/task.f\npackage FILE-OBJECTS-ROUTE\n" SRC+
   s\" public variable N  TASK:+USER U n  TYPED-VARIABLE D GENIO:device\nprivate\n" SRC+
   s\" : NOTHING ( -- ) ;\n: ADD-CHAR ( n -- ) drop 1 N +! ;\n: NO-KEY ( -- n ) 0 ;\n" SRC+
   s\" : NOT-READY ( -- bool ) false ;\n: NO-READ ( ^u8 n -- n ) {: a:^ u:n :} 0 ;\n" SRC+
   s\" : NO-ACCEPT ( ^u8 n -- n ) {: a:^ u:n :} -1 ;\n" SRC+
   s\" : ADD-BYTES ( str -- ) str> nip N +! ;\n: BUILD ( -- GENIO:device )\n" SRC+
   s\"    [: ADD-CHAR ;] [: NO-KEY ;] [: NOT-READY ;] [: NO-READ ;]\n" SRC+
   s\"    [: ADD-BYTES ;] [: NO-ACCEPT ;] [: NOTHING ;] [: NOTHING ;] 0 GENIO:DEVICE ;\n" SRC+
   s\" BUILD D !  D @ GENIO:OUTPUT!  s\" xyz\" type  7 U !\n;package\n" SRC+  s" f.f" NEST-WRITE
   s" f.f" REQUIRE+  s\" require src/lib/genio-core.f\n" SRC+
   s\" : TO-D ( -- ) FILE-OBJECTS-ROUTE:D @ GENIO:OUTPUT!  s\" abc\" type ;\n" SRC+
   s\" : MAIN ( -- )\n   .\" seen\" cr  FILE-OBJECTS-ROUTE:U @ .  FILE-OBJECTS-ROUTE:N @ .\n" SRC+
   s\"    [: TO-D ;] catch {: code:n :}  GENIO:TERMINAL GENIO:OUTPUT!\n" SRC+
   s\"    code E-GENIO-STATE = if .\" rejected\" cr else code . then\n" SRC+
   s\"    FILE-OBJECTS-ROUTE:N @ . ;\n" SRC+  s" main.f" NEST-WRITE ;

\ Guards a program's start: it starts on the terminal with fresh user rows whatever a file's top level left.
: ROUTE-CASE ( CMD:command -- CMD:command )
   WRITE-ROUTE
   s" RT" WHICH!  s" main.f" NEST-PATH$ REPL-BUILD 0= if exit then
   s" prog" s\" seen\n0\n3\nrejected\n3\n" s" D got the build's bytes; the program starts on the terminal, U 0"
      NEST-RUN
   s" RM" WHICH!  s" test/fixtures/file-objects/bare-main.f" FIXTURE-BUILD 0= if exit then
   s" RT2" WHICH!  s" main.f" NEST-PATH$ REPL-BUILD 0= if exit then
   s" prog" s\" seen\n0\n3\nrejected\n3\n" s" D got the build's bytes; the program starts on the terminal, U 0"
      NEST-RUN ;

\ The device sequence's f.f: K is the byte count a read of /dev/null answers, a file whose change time
\ does not track its bytes.
: WRITE-DEVICE ( -- )
   L$ s" device" NEST-DIR FS:JOIN-PATH NEST-DIR-U !  NEST-DIR NEST-DIR-U @ >str FS:MAKE-DIR
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

\ main.f's WEB: an HTTP server on port 0, started and stopped around n requests of its route, whose answers'
\ bodies it prints. Their connections are open at once, each with a worker, which registers cleanups on itself.
: WRITE-WEB ( -- )
   s\" TYPED-VARIABLE CFG HTTP:config\n" SRC+
   s\" : UP ( -- ) AIO:START  s\" GET\" s\" /hi\" [: nip s\" hello\" HTTP:BODY! ;] HTTP:ROUTE\n" SRC+
   s\"    $7F000001 CFG HTTP-CONFIG:ADDRESS !  0 CFG HTTP-CONFIG:PORT !\n" SRC+
   s\"    8 CFG HTTP-CONFIG:WORKERS !  500 >MS CFG HTTP-CONFIG:IDLE !  CFG HTTP:START ;\n" SRC+
   s\" : DOWN ( -- ) HTTP:STOP  AIO:STOP ;\n" SRC+
   s\" : DIAL ( -- TCP4:connection ) $7F000001 TCP4:ADDRESS HTTP:PORT TCP4:PORT TCP4:CONNECT\n" SRC+
   s\"    MATCH TCP4:connect-result connected OF ENDOF failed OF drop 1 throw ENDOF ;MATCH ;\n" SRC+
   s\" : DONE ( TCP4:status -- ) MATCH TCP4:status ok OF ENDOF failed OF drop 1 throw ENDOF ;MATCH ;\n" SRC+
   s\" : GET ( TCP4:connection -- )\n" SRC+
   s\"    s\\\" GET /hi HTTP/1.1\\r\\nHost: h\\r\\nConnection: close\\r\\n\\r\\n\" TCP4:WRITE DONE ;\n" SRC+
   s\" 256 SCRATCH-BUFFER: ANSWER  CAST: BLEN>N ( NUM:byte-len -- n )\n" SRC+
   s\" : HEAR ( TCP4:connection -- ) ANSWER 256 TCP4:TRANSFER-BYTES TCP4:READ-EXACT\n" SRC+
   s\"    MATCH TCP4:read-result data OF drop 2 throw ENDOF failed OF drop 1 throw ENDOF\n" SRC+
   s\"       closed OF BLEN>N ANSWER + 5 - 5 >str type cr ENDOF ;MATCH ;\n" SRC+
   s\" : GETS ( n -- ) {: n:n :} n 0= if exit then  DIAL {: c:TCP4:connection :}\n" SRC+
   s\"    n 1 - RECURSE  c GET  c HEAR  c TCP4:CLOSE DONE ;\n" SRC+
   s\" : WEB ( n -- ) UP GETS DOWN ;\n" SRC+ ;

\ The calls sequence's main.f: its load calls each library word once and leaves a pseudo-terminal and as many
\ files as its TMPDIR's last digit says open; its program, which keeps what it reaches, calls them again.
: WRITE-CALLS ( -- )
   L$ s" calls" NEST-DIR FS:JOIN-PATH NEST-DIR-U !  NEST-DIR NEST-DIR-U @ >str FS:MAKE-DIR
   s" abc" WRITE-DATA
   s\" require src/lib/fs.f\nrequire src/lib/fs-mutate.f\nrequire src/lib/process-tree.f\n" SRC+
   s\" require src/lib/net/http.f\nrequire src/lib/net/http-response.f\nrequire src/lib/net/http-router.f\n" SRC+
   s\" require src/lib/net/http-static.f\nrequire src/lib/net/tcp4.f\nrequire src/lib/pty.f\n" SRC+
   s\" require src/lib/heap.f\nrequire src/lib/vector.f\n" SRC+
   s\" require src/lib/secret.f\nrequire src/lib/secret/dir.f\n" SRC+
   s\" require src/arch/wasm/link.f\nrequire src/lib/pty-harness.f\n" SRC+
   s\" require src/lib/line-editor.f\nrequire src/lib/task.f\nrequire src/lib/aio.f\n" SRC+
   s\" : ASK ( -- ) s\" " SRC+  s" d.txt" NEST-PATH$ SRC+
   s\" \" FS:FILE-SIZE drop  s\" x\" TMP-PATH drop  getpid >PID PROC-TREE:CPU-NS drop ;\n" SRC+
   s\" : SERVE ( -- ) s\" x\" TMP-PATH s\" abc\" FS:WRITE-ALL\n" SRC+
   s\"    s\" TMPDIR\" GETENV HTTP:STATIC-ROOT  s\" x\" TMP-PATH FS:REMOVE-FILE ;\n" SRC+
   s\" PTY:SLAVE-PATH-CAP SCRATCH-BUFFER: SLAVE\n" SRC+
   s\" : TYPED ( PTY:master n -- ) {: m f:n :}\n   f 0 dup2 drop  f close\n" SRC+
   s\"    m s\" TMPDIR\" GETENV PTY:WRITE  m s\\\" \\n\" PTY:WRITE\n" SRC+
   s\"    LINE-EDITOR:READ-LINE drop  m PTY:CLOSE ;\n" SRC+
   s\" : TYPE-LINE ( -- ) SLAVE PTY:SLAVE-PATH-CAP PTY:OPEN drop\n" SRC+
   s\"    SLAVE PTY:PTY-OPEN-FLAGS 0 open TYPED ;\n" SRC+
   s\" : KEY ( -- ) HEAP:ALLOCATOR 0 [type] u8 VEC:NEW {: v:^ :}\n" SRC+
   s\"    s\" calls\" s\" x\" v SECRET:READ drop  v VEC:DELETE ;\n" SRC+
   s\" : BIND ( -- ) s\" TMPDIR\" GETENV s\" calls\" SECRET:DIR-BIND KEY ;\n" SRC+
   s\" : FN ( -- n ) s\" TMPDIR\" GETENV 0 0 1 WLINK-ORIGIN:KERNEL WLINK:FUNCTION+ ;\n" SRC+
   s\" : CHILD ( -- ) s\" /usr/bin/true\" PTY-HARNESS:SPAWN-ON-PTY PTY-HARNESS:REAP drop ;\n" SRC+
   s\" : HOLD ( -- ) s\" TMPDIR\" GETENV {: t:str :} t STR:DATA t STR:LEN 1- + c@ [char] 0 -\n" SRC+
   s\"    0 ?do s\" /dev/null\" FS:>PATHZ 0 0 open drop loop ;\n" SRC+
   s\" TASK:MIN-STACK TASK:TASK WAITER\n: REST ( -- ) 1 >MS AIO:TIMEOUT AIO:AWAIT drop  0 TASK:RETURN ;\n" SRC+
   s\" : WAIT ( -- ) AIO:START  ['] REST WAITER TASK:ACTIVATE  WAITER TASK:JOIN\n" SRC+
   s\"    MATCH result ok OF drop ENDOF err OF throw ENDOF ;MATCH  AIO:STOP ;\n" SRC+
   WRITE-WEB
   s\" ASK  8 WEB  SERVE TYPE-LINE BIND  FN WLINK:LINK drop  CHILD HOLD WAIT\n" SRC+
   s\" : MAIN ( -- ) ASK  HTTP:STATIC-COUNT . HTTP:STATIC-CLOSE\n   s\" " SRC+
   NEST-DIR NEST-DIR-U @ >str SRC+
   s\" \" HTTP:STATIC-ROOT HTTP:STATIC-COUNT .  LINE-EDITOR:READ-LINE drop\n" SRC+
   s\"    [: KEY ;] catch SECRET:E-CONFIG = bool>n .  FN dup . WLINK:LINK drop  CHILD WAIT\n" SRC+
   s\"    1 WEB ;\n" SRC+  s" main.f" NEST-WRITE ;

\ d.txt was last read n seconds and last written 1 second past the epoch.
: ACCESSED ( n -- )
   TIMES LE:U64!  0 TIMES 8 + LE:U64!
   1 TIMES 16 + LE:U64!  0 TIMES 24 + LE:U64!
   s" d.txt" NEST-PATH$ FS:>PATHZ TIMES UTIMES-CALL 0 T= ;

\ CB2, after d.txt's access time changes, under another TMPDIR and with another pty open, compiles main.f again.
: CALLS-AGAIN ( CMD:command ^ARENA:arena -- CMD:command )
   {: m:^ :}
   m L$ s" prog" IN$ WHOLE-FILE {: p1:str :}
   2 ACCESSED  F$ MARK
   s" CB2" WHICH!  s" main.f" NEST-PATH$ FIXTURE-BUILD 0= if exit then
   s" the build rewrites an object" LABEL  F$ REWRITTEN 0 > TTRUE
   m L$ s" prog" IN$ WHOLE-FILE {: p2:str :}
   s" the program is the first build's" LABEL  p1 p2 STR:= TTRUE
   s" prog" s\" 0\n2\n-1\n0\nhello\n"
   s" the program serves nothing, then d.txt and main.f, reads no secret, links 0, runs a child, waits and serves /hi"
   NEST-RUN ;

\ Guards a program against what the library words a load ran left in their cells.
: CALLS-CASE ( CMD:command -- CMD:command )
   WRITE-CALLS
   1 ACCESSED
   s" CB" WHICH!  s" main.f" NEST-PATH$ FIXTURE-BUILD 0= if exit then
   SLAVE PTY:SLAVE-PATH-CAP PTY:OPEN drop {: held:PTY:master :}
   [: CALLS-AGAIN ;] ARENA:WITH
   held PTY:CLOSE ;

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

\ stride.f and effect.f spell turn only inside box<turn>: a stride, rest's offset
\ and an effect. blob.f, unchanged by turn, holds a BUFFER record effect.f reads.
: WRITE-SPELLED ( -- )
   L$ s" spelled" NEST-DIR FS:JOIN-PATH NEST-DIR-U !
   NEST-DIR NEST-DIR-U @ >str FS:MAKE-DIR
   s\" STRUCTURE blob DERIVE addr\n   FIELD k n\n   BUFFER b 13\n;STRUCTURE\n" SRC+
   s\" 1 TYPED-BUFFER BB blob\n" SRC+  s" blob.f" NEST-WRITE
   false WRITE-TURN
   s" turn.f" REQUIRE+
   s\" 4 TYPED-BUFFER BT box<turn>\n" SRC+
   s\" : B-STEP ( -- n ) 1 BT NULL-PTR -  0 BT NULL-PTR -  - ;\n" SRC+
   s\" : R-AT ( -- n ) 0 BT BOX:REST NULL-PTR -  0 BT NULL-PTR -  - ;\n" SRC+
   s" stride.f" NEST-WRITE
   s" blob.f" REQUIRE+
   s\" : W ( box<turn> -- n ) drop 7 ;\n" SRC+
   s\" : B-LEN ( -- n ) 0 BB BLOB:B SPAN:LEN ;\n" SRC+
   s\" : B-AT ( -- n ) 0 BB BLOB:B SPAN:$ str> drop NULL-PTR -  0 BB NULL-PTR -  - ;\n" SRC+
   s" effect.f" NEST-WRITE
   s" blob.f" REQUIRE+  s" turn.f" REQUIRE+  s" stride.f" REQUIRE+  s" effect.f" REQUIRE+
   s\" : MAIN ( -- ) B-STEP . 11 MAKE-BOX W . .  B-LEN . B-AT . R-AT . ;\n" SRC+
   s" main.f" NEST-WRITE ;

\ After turn gains a field J2 compiles stride.f and effect.f, which spell it only
\ in <...>, links blob.f, and prints a cold build's numbers (rest at 24, J's 16).
: SPELLED-CASE ( CMD:command -- CMD:command )
   WRITE-SPELLED
   s" J" WHICH!  s" main.f" NEST-PATH$ FIXTURE-BUILD 0= if exit then
   SPELLED-STRIDE KEEP  SPELLED-EFFECT KEEP  SPELLED-BLOB KEEP
   true WRITE-TURN
   s" J2" WHICH!  s" main.f" NEST-PATH$ FIXTURE-BUILD 0= if exit then
   s" the build compiles stride.f" SPELLED-STRIDE COMPILED
   s" the build compiles effect.f" SPELLED-EFFECT COMPILED
   s" the build links blob.f" SPELLED-BLOB LINKED
   s\" 32\n7\n11\n13\n8\n24\n" s" the program prints a cold build's numbers" PRINTS-RUN ;

\ The folded sequence's thing.f: the record thing, with a second field when the
\ flag is true, held<a> and a held<thing> buffer.
: WRITE-THING ( bool -- )
   {: wide:bool :}
   s\" STRUCTURE thing DERIVE addr\n   FIELD x n\n" SRC+
   wide if s\"    FIELD y n\n" SRC+ then
   s\" ;STRUCTURE\nSTRUCTURE held<a> DERIVE addr\n" SRC+
   s\"    FIELD value a\n   FIELD set bool\n;STRUCTURE\n" SRC+
   s\" 1 TYPED-BUFFER HB held<thing>\n: HV ( -- ^held<thing> ) 0 HB ;\n" SRC+
   s" thing.f" NEST-WRITE ;

\ main.f calls set on the held<thing> lib.f's GETH answers, never spelling thing:
\ the offset its call adds is the one the fold reads off thing's width.
: WRITE-FOLDED ( -- )
   L$ s" folded" NEST-DIR FS:JOIN-PATH NEST-DIR-U !
   NEST-DIR NEST-DIR-U @ >str FS:MAKE-DIR
   false WRITE-THING
   s" thing.f" REQUIRE+
   s\" : GETH ( -- ^held<thing> ) HV ;\n" SRC+  s" lib.f" NEST-WRITE
   s" thing.f" REQUIRE+  s" lib.f" REQUIRE+
   s\" : MAIN ( -- ) GETH HELD:SET NULL-PTR -  GETH NULL-PTR -  - . ;\n" SRC+
   s" main.f" NEST-WRITE ;

\ After thing gains a field HF2 compiles main.f's last part, which never spells
\ thing, and prints a cold build's offset of set: 16, where HF's is 8.
: FOLDED-CASE ( CMD:command -- CMD:command )
   WRITE-FOLDED
   s" HF" WHICH!  s" main.f" NEST-PATH$ FIXTURE-BUILD 0= if exit then
   FOLDED-LAST KEEP
   true WRITE-THING
   s" HF2" WHICH!  s" main.f" NEST-PATH$ FIXTURE-BUILD 0= if exit then
   s" the build compiles main.f's last part" FOLDED-LAST COMPILED
   s\" 16\n" s" the program prints a cold build's offset" PRINTS-RUN ;

\ top.f's top level calls set on a held<thing> it never spells: once thing gains
\ a field, HT2's warm build rejects it as a cold build does.
: FOLDED-TOP-CASE ( CMD:command -- CMD:command )
   L$ s" folded-top" NEST-DIR FS:JOIN-PATH NEST-DIR-U !  NEST-DIR NEST-DIR-U @ >str FS:MAKE-DIR
   false WRITE-THING
   s" thing.f" REQUIRE+  s\" TYPED-VARIABLE PV ^held<thing>\n" SRC+  s" p.f" NEST-WRITE
   s" thing.f" REQUIRE+  s" p.f" REQUIRE+
   s\" PV @ HELD:SET NULL-PTR -  PV @ NULL-PTR -  - constant OFF\n" SRC+
   s\" : MAIN ( -- ) OFF . ;\n" SRC+  s" top.f" NEST-WRITE
   s" HT" WHICH!  s" top.f" s" prog-top" NEST-BUILD 0= if exit then
   s" prog-top" s\" 8\n" s" the program prints set's offset" NEST-RUN
   true WRITE-THING
   s" HT2" WHICH!  s" top.f" s" prog-top" NEST-RC {: rc:n :}
   CMD:ERR$ s" error[E-LAYOUT-VALUE]" STR:CONTAINS? {: said:bool :}
   CMD:ERR$ s" top.f" NEST-PATH$ STR:CONTAINS? {: named:bool :}
   said named and  rc 70 =  and 0= if rc SHOW then
   s" the build exits 70" LABEL  rc 70 T=
   s" the rejection gives the moved member's reason" LABEL  said TTRUE
   s" the rejection names top.f" LABEL  named TTRUE ;

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
   NEST-DIR NEST-DIR-U @ >str FS:MAKE-DIR
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
   NEST-DIR NEST-DIR-U @ >str FS:MAKE-DIR
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
   NEST-DIR NEST-DIR-U @ >str FS:MAKE-DIR
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

\ The built sequence's main.f: it runs lib.f's MAKE-EAT and prints 42 when the
\ flag is true, else 41.
: WRITE-EAT ( bool -- )
   {: warm:bool :}
   s" lib.f" REQUIRE+  s\" s\" BOX-EAT\" PL:MAKE-EAT\n" SRC+
   s" : MAIN ( -- ) " SRC+  warm if s" 42" SRC+ else s" 41" SRC+ then
   s\"  . cr ;\n" SRC+  s" main.f" NEST-WRITE ;

\ lib.f's definer MAKE-EAT names PL's plbox by its spelling. It is written
\ once: a rewritten file is compiled again, which reads its entry.
: WRITE-BUILT ( -- )
   L$ s" built" NEST-DIR FS:JOIN-PATH NEST-DIR-U !
   NEST-DIR NEST-DIR-U @ >str FS:MAKE-DIR
   s\" package PL\npublic\nSTRUCTURE plbox FIELD v n ;STRUCTURE\n" SRC+
   s\" : MAKE-EAT ( str -- )\n" SRC+
   s\"    s\" PL:plbox\" EFFECT:NAMED EFFECT:PTR {: p :}\n" SRC+
   s\"    EFFECT:EMPTY p EFFECT:IN define-word\n" SRC+
   s\"    postpone drop\n   postpone ; ;\n;package\n" SRC+  s" lib.f" NEST-WRITE
   false WRITE-EAT ;

\ Guards a family a definer finds by spelling in a linked object's entry, which
\ only a warm build leaves unread: BI2 builds main.f's edit and prints 42.
: BUILT-CASE ( CMD:command -- CMD:command )
   WRITE-BUILT
   s" BI" WHICH!  s" main.f" NEST-PATH$ FIXTURE-BUILD 0= if exit then
   true WRITE-EAT
   s" BI2" WHICH!  s" main.f" NEST-PATH$ FIXTURE-BUILD 0= if exit then
   s\" 42\n\n" s" the program prints the edited number" PRINTS-RUN ;

\ A directory of L's of that name, holding the cast sequence's a.f: package
\ FILE-OBJECTS-CAST's private generic cast >ELEM and its public generic AT.
: WRITE-CAST ( str -- )
   {: s:str :}
   L$ s NEST-DIR FS:JOIN-PATH NEST-DIR-U !
   NEST-DIR NEST-DIR-U @ >str FS:MAKE-DIR
   s\" package FILE-OBJECTS-CAST\nCAST: >ELEM ( ^u8 -- ^t )\n" SRC+
   s\" public\n: AT ( ^u8 -- ^a ) >ELEM ;\n;package\n" SRC+
   s" a.f" NEST-WRITE ;

\ b.f calls AT at elem: a record, or a linear type when the flag is true.
: WRITE-CAST-B ( bool -- )
   {: lin:bool :}
   s" a.f" REQUIRE+
   lin if s\" DEFLINEAR elem\n" SRC+ else
      s\" STRUCTURE elem DERIVE addr FIELD x n FIELD y n ;STRUCTURE\n" SRC+
   then
   s\" 32 BUFFER: CAST-BUF\n" SRC+
   s\" : SLOT ( -- ^elem ) CAST-BUF FILE-OBJECTS-CAST:AT ;\n" SRC+
   lin if s\" : MAIN ( -- ) ;\n" SRC+ else
      s\" : MAIN ( -- ) 5 SLOT ELEM:Y !  SLOT ELEM:Y @ . ;\n" SRC+
   then
   s" b.f" NEST-WRITE ;

\ A build of b.f the cast's restriction rejects; answers its exit code. The
\ build rejects with E-CAST-LINEAR at the call of AT, b.f's line 4.
: CAST-REJECTED ( CMD:command -- CMD:command n )
   s" b.f" NEST-PATH$ FAILING-BUILD {: rc:n :}
   CMD:ERR$ s" error[E-CAST-LINEAR]: a cast's stored type would hold a linear value"
   STR:CONTAINS? {: coded:bool :}
   CMD:ERR$ s" /b.f:4:30" STR:CONTAINS? coded and {: said:bool :}
   rc 0 =  said 0=  or if rc SHOW then
   s" the build exits nonzero" LABEL  rc 0<> TTRUE
   s" the build rejects AT at a linear type" LABEL  said TTRUE
   rc ;

\ Guards the restriction a linked object's AT carries: CA2 links a.f and
\ rejects b.f's SLOT as CA3, a cold build of the same files, does.
: CAST-CASE ( CMD:command -- CMD:command )
   s" cast" WRITE-CAST  false WRITE-CAST-B
   s" CA" WHICH!  s" b.f" NEST-PATH$ FIXTURE-BUILD 0= if exit then
   s\" 5\n" s" the program prints the field it stored" PRINTS-RUN
   CAST-A KEEP
   true WRITE-CAST-B
   s" CA2" WHICH!  CAST-REJECTED {: warm:n :}
   s" the build links a.f" CAST-A LINKED
   s" cast-cold" WRITE-CAST  true WRITE-CAST-B
   s" CA3" WHICH!  CAST-REJECTED {: cold:n :}
   s" CA2 exits as the build does" LABEL  warm cold T= ;

\ A Wasm build of bare-main.f in a fresh hb, since the process fixes the build's target; its module never runs.
: WASM-BUILD ( CMD:command -- CMD:command )
   TOOL-ARGS  s" --target" CMD:ARG+  s" wasm" CMD:ARG+
   s" test/fixtures/file-objects/bare-main.f" TOOL-RUN {: rc:n :}
   rc 0<> if rc SHOW then
   s" the build exits 0" LABEL  rc 0 T= ;

\ WASM's module, read into the arena before WASM2's build replaces it, then WASM2's.
: CROSS-AGAIN ( CMD:command ^ARENA:arena -- CMD:command )
   {: m:^ :}
   m L$ s" prog" IN$ WHOLE-FILE {: p1:str :}
   F$ MARK
   s" WASM2" WHICH!  WASM-BUILD
   s" the build rewrites no object" LABEL  F$ REWRITTEN 0 T=
   m L$ s" prog" IN$ WHOLE-FILE {: p2:str :}
   s" the module is the first build's" LABEL  p1 p2 STR:= TTRUE ;

\ Guards a cross build's objects, which hold each definition's target and host copies: a second Wasm build
\ links every file and writes the first's module.
: CROSS-CASE ( CMD:command -- CMD:command )
   s" WASM" WHICH!  WASM-BUILD
   [: CROSS-AGAIN ;] ARENA:WITH ;

\ A Wasm build exits 0 where a target word is only named and is rejected where top level runs it, a linked file's
\ by its name; two builds, since a rejected build ends its hb.
: TARGET-CASE ( CMD:command -- CMD:command )
   s" TN" WHICH!
   TOOL-ARGS  s" --target" CMD:ARG+  s" wasm" CMD:ARG+
   s" test/fixtures/file-objects/target-named.f" TOOL-RUN {: named:n :}
   named 0<> if named SHOW then
   s" the build exits 0" LABEL  named 0 T=
   s" TR" WHICH!
   TOOL-ARGS  s" --target" CMD:ARG+  s" wasm" CMD:ARG+
   s" test/fixtures/file-objects/target-run.f" TOOL-RUN {: rc:n :}
   CMD:ERR$ s" error[E-TARGET-WORD]" STR:CONTAINS? {: classed:bool :}
   CMD:ERR$ s" target word `FO-TAIL`" STR:CONTAINS? {: tail:bool :}
   CMD:ERR$ s" test/fixtures/file-objects/target-run.f:8:" STR:CONTAINS? tail and {: placed:bool :}
   classed placed and  rc 0<> and 0= if rc SHOW then
   s" the build fails" LABEL  rc 0<> TTRUE
   s" the rejection is E-TARGET-WORD" LABEL  classed TTRUE
   s" the rejection names linked FO-TAIL on target-run.f's line 8" LABEL  placed TTRUE ;

: CHECKS ( -- )
   [: COLD ;] CMD:WITH-COMMAND
   CHECK-COLD
   [: WARM-A WARM-CUT WARM-GROW WARM-FLIP WARM-B WARM-E LOG-CASE PT-CASE RECORD-CASE ZERO-CASE PATH-CASE STALE-CASE HIDDEN-CASE
      SEALED-CASE
      STORED-CASE RETIRED-CASE ORDER-CASE PART-LINK-CASE PIECE-CASE SEAL-CASE SQUAT-CASE SPAWN-REPL-CASE CAUGHT-CASE
      CAUGHT-PASSED-CASE CAUGHT-LOAD-CASE CAUGHT-NONE-CASE DEFINE-CASE CAUGHT-AGAIN-CASE CTOR-CASE EXPORT-CASE
      MOVED-CASE COPY-CASE NEST-CASE KEPT-CASE SAME-CASE RETIRE-CASE PRIMITIVE-CASE HB-RETIRE-CASE PROBE-CASE SEARCH-WL-CASE SCOPE-FIND-CASE
      PUBLIC-LOOKUP-CASE CLAUSE-CASE DERIVED-CASE READ-CASE LIST-CASE LINK-CASE TASK-CASE
      SAMEFILE-CASE ANCHORED-CASE SPAWN-CASE ZIP-CASE KIND-CASE ARENA-CASE PFIELD-CASE
      MUTUAL-CASE
      DIVISOR-CASE ROUTE-CASE DEVICE-CASE CALLS-CASE SPELLED-CASE FOLDED-CASE FOLDED-TOP-CASE PACKAGED-CASE
      EVALUATED-CASE OPENED-CASE BUILT-CASE CAST-CASE CROSS-CASE TARGET-CASE ;]
   CMD:WITH-COMMAND ;


: REMOVE-A ( -- ) A$ FS:REMOVE-TREE ;
: REMOVE-B ( -- ) B$ FS:REMOVE-TREE ;
: REMOVE-C ( -- ) C$ FS:REMOVE-TREE ;
: REMOVE-D ( -- ) D$ FS:REMOVE-TREE ;
: REMOVE-E ( -- ) E$ FS:REMOVE-TREE ;
: REMOVE-L ( -- ) L$ FS:REMOVE-TREE ;

: WITH-L ( -- )
   s" habu-file-objects-l" FS:MKTEMP-DIR {: l:str :}
   l STR:DATA OUT-L l STR:LEN BYTE-COPY  l STR:LEN OUT-L-U !
   L$ s" out" OUT-F FS:JOIN-PATH OUT-F-U !
   ['] CHECKS ['] REMOVE-L finally ;

: WITH-E ( -- )
   s" habu-file-objects-e" FS:MKTEMP-DIR {: e:str :}
   e STR:DATA OUT-E e STR:LEN BYTE-COPY  e STR:LEN OUT-E-U !
   ['] WITH-L ['] REMOVE-E finally ;

: WITH-D ( -- )
   s" habu-file-objects-d" FS:MKTEMP-DIR {: d:str :}
   d STR:DATA OUT-D d STR:LEN BYTE-COPY  d STR:LEN OUT-D-U !
   ['] WITH-E ['] REMOVE-D finally ;

: WITH-C ( -- )
   s" habu-file-objects-c" FS:MKTEMP-DIR {: c:str :}
   c STR:DATA OUT-C c STR:LEN BYTE-COPY  c STR:LEN OUT-C-U !
   ['] WITH-D ['] REMOVE-C finally ;

: WITH-B ( -- )
   s" habu-file-objects-b" FS:MKTEMP-DIR {: b:str :}
   b STR:DATA OUT-B b STR:LEN BYTE-COPY  b STR:LEN OUT-B-U !
   ['] WITH-C ['] REMOVE-B finally ;

\ Every tail starts unkept, so a label on a tail no KEEP ran fails.
: UNKEPT ( -- ) TAILS 0 ?do -1 i KEPT-SIZE ! loop ;

public

\ Builds A to E link the stripped kernel object this process writes once and BUILD passes, so their out
\ roots hold only the files' objects.
: RUN ( -- )
   T-RESET  UNKEPT
   false RTARGET:HOST-TARGET HB-BUILD-CORE:KERNEL-BUILD
   s" habu-file-objects-a" FS:MKTEMP-DIR {: a:str :}
   a STR:DATA OUT-A a STR:LEN BYTE-COPY  a STR:LEN OUT-A-U !
   ['] WITH-B ['] REMOVE-A finally
   T-REPORT ;

;package

FILE-OBJECTS-TEST:RUN
