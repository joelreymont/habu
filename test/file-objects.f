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
\ second file reads the variable the first comes to grow, spells the package
\ the first comes to declare, or lays the string the first comes to lay. The
\ tail sequence, whose second file is edited, compiles 2, 1 and 0; its fourth
\ build, of d.f alone,
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
\ the reader compiles again when the storers swap. The header sequence, whose
\ second and third builds load a file storing into header data between a file
\ and the one that runs its word, compiles 2, 1 and 0: the store is no
\ dependency of the run (linker.f "dependencies by run"). The same sequence,
\ whose two files store one word as the program's entry, compiles 3, then 0
\ when the first storer leaves the build: the second's object holds its store,
\ though it left the cell as the first did, so the program it writes has K as
\ its entry and its map lists K. The held sequence, the header sequence with a
\ runner that also holds a header-data address, compiles 2, 2 and 0: the store
\ is then a dependency of the run. The crafted sequence's
\ builds of e.f over eight objects the link refuses or links wrong compile 1,
\ and a plain build after them links. The words sequence compiles 5, 4, 0, 2,
\ 1, 1 and 0: a file depends on each earlier word its source spells, not on
\ the word's whole piece, and compiles when its source spells a word its
\ object does not list. The ran sequence compiles 4, 1, 3, 0, 3 and 0: a file
\ that runs a word depends on each word the run word reaches, not on the
\ word's whole piece. C writes forty-six objects, and the
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
\ runs count.f's LATER: main.f stores into count.f's data. It
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
\ H builds part-main.f, whose package's part NFETCH-CHECK has a BAD-TAG that returns; its
\ program dies with the bad-tag error all the same.
\ S, S2 and S3 build stale-root.f, stale-alone.f and stale-root.f: S2 leaves stale-spell.f's object
\ naming no file, and S3 checks it while stale-def.f, which defines V, a name stale-spell.f spells,
\ is checked and not yet linked, so S3 compiles it. K, K2 and K3 build hidden-root.f, hidden-alone.f
\ and hidden-root.f, and K3 exits 0: hidden-spell.f spells W, which hidden-def.f defines privately.
\ K2 leaves hidden-spell.f's object naming hidden-pkg.f alone, and K3 compiles it to name
\ hidden-def.f too. G, G2 and G3 build sealed-root.f, sealed-alone.f and sealed-root.f, and G3 exits 0: sealed-spell.f
\ spells FILE-OBJECTS-SEALED, which sealed-seal.f seals. G2 leaves sealed-spell.f's object naming
\ sealed-pkg.f alone, and G3 compiles it to name sealed-seal.f too. T, T2 and T3 build stored-root.f,
\ stored-alone.f and stored-root.f, and T3 exits 0: stored-run.f runs stored-pkg.f's SGET, which
\ reads the variable stored-store.f stores into. T2 leaves stored-run.f's object naming no
\ stored-store.f, and T3 compiles it to name that file. V, V2 and V3 build retired-root.f,
\ retired-alone.f and retired-root.f, and V3 exits 0: retired-spell.f spells RW, which
\ retired-undefine.f retires. V2 leaves retired-spell.f's object naming no file, and V3 compiles it
\ to name retired-undefine.f. O to O4 build order-root-a.f, order-root-b.f, order-root-a.f and
\ order-root-a.f, and each exits 0: O2 compiles order-x2.f after order-q2.f and order-q1.f, so its
\ object names order-q2.f first; O3 checks it while order-ha.f, order-hb.f and order-hc.f are built
\ and order-q1.f and order-q2.f are checked and not yet linked, and compiles it to name order-q1.f
\ first; O4 links it.
\ F builds seal-main.f, whose definition into the package seal-second.f seals is refused: exit 84,
\ E-SEALED-PACKAGE at its line. F2, the same build again, links seal-second.f's object, which keeps
\ its size and change time, and is refused the same way: the link carries the seal.
\ Y and Y2 build mid-root-1.f and mid-root-2.f, whose first files define W differently: Y2 compiles
\ mid-four.f's second part, which calls W, and links its last part after it, so its program prints
\ 10, 2, 30 and 40.
\ Z, Z2 and Z3 build flip-root-plain.f, flip-root-x.f and flip-root-x.f: Z2 loads flip-a.f first, so
\ flip.f's require of it cuts no part and flip.f's second part starts after its flip-b.f request,
\ which a true [if] makes, and compiles; Z3 links it. unclean.f's part after an unclean request and
\ a clean one links in Z2, and the program prints 5, 1, 2, 3, 7 and 9.
\ J and J2 build redo-main.f, whose one part undefines Y and X and defines them again: J2 links it
\ and exits 0, and its program prints 2.
\ R builds scratch-main.f, whose second file stores 7 into the first's scratch cell, SCRATCH-VARIABLE and
\ SCRATCH-BUFFER: at top level; its program prints 0 for each, read at the third file's top level and
\ again in the program: the stores end with that file. W and W2 build task-left-main.f, whose top
\ level leaves a task running, into one out root, and each fails naming that file. X, X4 and X2
\ build word-load-main.f, word-provided-main.f and word-read-main.f, whose top levels run a word
\ that loads, provides, or reads through SOURCE-ROOT:READ-OS, a file, and each fails naming that
\ file with its entry's refusal.
\ Q builds kept-main.f, whose files' top levels leave an archive open and await an aio ticket, each
\ keeping its handle; its program opens another of each and the kept handle is refused. N builds
\ kept-genio.f with --repl, since genio-core.f calls the REPL's KEY1, and its program refuses a device
\ handle kept the same way.
\ The nested build (NEST-CASE), by tools/hb-build.f in L's out root after L's build, which leaves the
\ kernel's objects there, builds r.f, which requires x.f, which requires y.f between two pieces; then
\ m.f, which also requires x.f, after x.f's first piece changes K; then r.f again, whose program
\ prints the new K: r.f's object names every piece built inside x.f's load.

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
12 constant STORED-RUN                   \ unclean.f's last part's and flip.f's second part's
13 constant RETIRED-SPELL
14 constant ORDER-X2
15 constant HIDDEN-SPELL
16 constant SEAL-SECOND
17 constant MID-FOUR-LAST
18 constant UNCLEAN-LAST
19 constant FLIP-SECOND
46 constant FILES-C                      \ build C's: count.f's, shape.f's, each sequence's, those that
                                         \ never link included, the twins', the order sequence's four,
                                         \ the header, same and held sequences' three each, e.f's, the
                                         \ words sequence's five, the ran sequence's four
208 constant HEADER-BYTES                \ an object's header, INPUT right after it (linker.f)
192 constant PIECES-FIELD               \ the header's PIECES offset (linker.f WRITE-OBJECT)
72 constant FILES-LEN-FIELD              \ the header's FILES length offset (linker.f WRITE-OBJECT)
32 constant FILES-ROW                    \ a FILES row's bytes (linker.f FILES-ROW-BYTES)
24 constant INPUT-BYTES                  \ an object's INPUT: its file's stamp, 0 0 -1 if it never links
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
PATH-CAP BUFFER: NEST-DIR  variable NEST-DIR-U  \ N's sources, <L's out>/nested
PATH-CAP BUFFER: NEST-PATH                      \ one of them
PATH-CAP 2 * BUFFER: NEST-SRC  variable NEST-SRC-U  \ the text NEST-WRITE writes
PATH-CAP BUFFER: JOINED
PATH-CAP BUFFER: JOINED-2
PATH-CAP BUFFER: ROOT    variable ROOT-U     \ the obj directory a walk starts at
PATH-CAP BUFFER: FOUND   variable FOUND-U    \ the walk's last path that ends with its tail
PATH-CAP BUFFER: KEPT    variable KEPT-U     \ the object KEEP found, 0 long if none
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


\ The tail of object i's path, from 0: A's count.f, shape.f, main.f, reopen.f, mark.f and again.f's
\ two loads; 7 pad.f's, which B adds; 8 log-first.f's; 9 path-main.f's; 10 stale-spell.f's, which S
\ writes; 11 sealed-spell.f's, G's; 12 stored-run.f's, T's; 13 retired-spell.f's, V's; 14
\ order-x2.f's, O's; 15 hidden-spell.f's, K's; 16 seal-second.f's, F's; 17 mid-four.f's part 3's,
\ Y's; 18 unclean.f's part 2's and 19 flip.f's part 1's, Z's.
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

\ Builds a fixture to L's prog in the shared fixture out root by tools/hb-build.f, since log.f
\ requires and creates through does> (build.f's files may not); --repl when the flag is true. Its
\ TMPDIR is L's directory: a build SEALED-FAIL refuses runs no exit hook (outer.f FAIL-CLOSED), so
\ its scratch goes with L.
: BUILD-RC ( CMD:command ptr u8 n bool -- CMD:command n )
   {: src:ptr src-u:n repl:bool :}
   CMD:RESET
   s" --load" CMD:ARG+  s" tools/hb-build.f" CMD:ARG+
   s" --" CMD:ARG+
   repl if s" --repl" CMD:ARG+ then
   src src-u CMD:ARG+
   s" -o" CMD:ARG+  L$ s" prog" IN$ CMD:ARG+
   s" HABU_OUT" F$ CMD:ENV+  s" TMPDIR" L$ CMD:ENV+
   ENGINE-CANDIDATE:PATH$ 120000 >MS CMD:RUN-OUTCOME
   PROCESS:ENDING>RC RC>N ;

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
   s" the build links path-main.f" LINKED  PATH-RUN
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
   s" the build links order-x2.f" LINKED ;

\ Guards linking the parts after a compiled part, each from its own start: Y2 compiles
\ mid-four.f's second part, which calls W.
: MID-CASE ( CMD:command -- CMD:command )
   s" Y" WHICH!  s" test/fixtures/file-objects/mid-root-1.f" FIXTURE-BUILD 0= if exit then
   MID-FOUR-LAST KEEP
   s" Y2" WHICH!  s" test/fixtures/file-objects/mid-root-2.f" FIXTURE-BUILD 0= if exit then
   s" the build links mid-four.f's last part" LINKED
   s\" 10\n2\n30\n40\n" s" the program prints mid-four.f's words with mid-w2.f's W" PRINTS-RUN ;

\ Guards a part's start moving between builds with no edit: Z2's flip.f part 1 starts after its
\ flip-b.f request, not Z's flip-a.f one, and compiles; linked from Z's object it defines Q twice.
: START-CASE ( CMD:command -- CMD:command )
   s" Z" WHICH!  s" test/fixtures/file-objects/flip-root-plain.f" FIXTURE-BUILD 0= if exit then
   UNCLEAN-LAST KEEP
   s" Z2" WHICH!  s" test/fixtures/file-objects/flip-root-x.f" FIXTURE-BUILD 0= if exit then
   s" the build links unclean.f's last part after an unclean request and a clean one" LINKED
   FLIP-SECOND KEEP
   s" Z3" WHICH!  s" test/fixtures/file-objects/flip-root-x.f" FIXTURE-BUILD 0= if exit then
   s" the build links the part that starts at [then]" LINKED
   s\" 5\n1\n2\n3\n7\n9\n" s" the program prints flip-a.f's F, P, Q, R, A and C" PRINTS-RUN ;

\ Guards a link of a part that retires its own records: J2 links redo-main.f's part.
: REDO-CASE ( CMD:command -- CMD:command )
   s" J" WHICH!  s" test/fixtures/file-objects/redo-main.f" FIXTURE-BUILD 0= if exit then
   s" J2" WHICH!  s" test/fixtures/file-objects/redo-main.f" FIXTURE-BUILD 0= if exit then
   s\" 2\n" s" the program calls the second Y" PRINTS-RUN ;

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

: SEAL-CASE ( CMD:command -- CMD:command )
   s" F" WHICH!  SEAL-REFUSED  SEAL-SECOND KEEP
   s" F2" WHICH!  SEAL-REFUSED
   s" the build links seal-second.f" LINKED ;


\ The path of N's source of that name.
: NEST-PATH$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   NEST-DIR NEST-DIR-U @ a u NEST-PATH FS:JOIN-PATH {: v:n :}
   NEST-PATH v ;

: SRC+ ( ptr u8 n -- ) {: a:ptr u:n :}
   a NEST-SRC NEST-SRC-U @ + u BYTE-COPY  u NEST-SRC-U +! ;

\ A line that requires N's source of that name by its absolute path.
: REQUIRE+ ( ptr u8 n -- ) s" require " SRC+  NEST-PATH$ SRC+  s\" \n" SRC+ ;

\ Writes the text to N's source of that name and starts the next text.
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

\ tools/hb-build.f's build of N's source to N's program of that name, in a fresh hb and L's out
\ root: the boundary under test is the object another build wrote. True when it exits 0.
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

\ N's program, run in its own process: the boundary under test is the written file.
: NEST-RUN ( CMD:command ptr u8 n -- CMD:command ) {: want:ptr wantu:n :}
   CMD:RESET
   L$ s" prog-n" IN$ 60000 >MS CMD:RUN-OUTCOME PROCESS:ENDING>RC RC>N {: rc:n :}
   CMD:OUT$ want wantu STR=  rc 0 =  and 0= if rc SHOW then
   s" the program prints K" LABEL  CMD:OUT$ want wantu T$= ;

\ r.f, m.f after x.f's K changes, which rewrites x.f's object and leaves r.f's, then r.f again.
: NEST-CASE ( CMD:command -- CMD:command )
   WRITE-NESTED
   s" N" WHICH!  s" r.f" s" prog-n" NEST-BUILD 0= if exit then
   s\" 1\n" NEST-RUN
   s" 2" WRITE-X
   s" N2" WHICH!  s" m.f" s" prog-m" NEST-BUILD 0= if exit then
   s" N3" WHICH!  s" r.f" s" prog-n" NEST-BUILD 0= if exit then
   s\" 2\n" NEST-RUN ;

: CHECKS ( -- )
   [: COLD ;] CMD:WITH-COMMAND
   CHECK-COLD
   [: WARM-A WARM-CUT WARM-GROW WARM-B WARM-E LOG-CASE PATH-CASE STALE-CASE HIDDEN-CASE SEALED-CASE
      STORED-CASE RETIRED-CASE ORDER-CASE MID-CASE START-CASE REDO-CASE PIECE-CASE SEAL-CASE
      NEST-CASE ;]
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
