\ source-root-exe-test.f - the engine finds its source root from its executable
\ (src/core/include.f SOURCE-ROOT:ENGINE$), with no environment variable: an
\ editor starts a language server in a working directory it cannot choose. The
\ root is the working directory when it is a Habu tree (it holds the engine's
\ first boot row), else the tree two levels above the executable when that is
\ one, else the working directory.

require src/lib/test/assert.f
require src/lib/string.f
require src/lib/fmt.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/process-cwd.f
require src/lib/engine-candidate.f
require test/pcap-result.f
require src/lib/process.f

package SOURCE-ROOT-EXE-TEST
private

PATH-CAP 1+ constant CAP
$10000 constant IO-CAP
PATH-CAP 1+ 4 * constant PROG-CAP
\ A prompt line holds 255 bytes, so `cd <name>` names at most this much.
200 constant LONG-NAME
\ Search without read permission (0111).
$49 constant SEARCH-ONLY
\ No permission at all (0000).
0 constant NO-ACCESS
\ A directory's usual mode (0755).
493 constant MODE-DIR

create GATE CAP allot
create TOP CAP allot
create FOREIGN CAP allot
create HB CAP allot
create B-ROOT CAP allot
create PA CAP allot
create PB CAP allot
create PE CAP allot
create COPY-DST CAP allot
create LONG LONG-NAME allot
create PROG PROG-CAP allot
create OUT IO-CAP allot
create ERR IO-CAP allot
variable GATE-U
variable TOP-U
variable FOREIGN-U
variable HB-U
variable B-ROOT-U
variable PROG-U
variable OUT-U
variable ERR-U
variable RC

: GATE$ ( -- str ) GATE GATE-U @ >str ;
: TOP$ ( -- str ) TOP TOP-U @ >str ;
: FOREIGN$ ( -- str ) FOREIGN FOREIGN-U @ >str ;
: HB$ ( -- str ) HB HB-U @ >str ;
: B-ROOT$ ( -- str ) B-ROOT B-ROOT-U @ >str ;
: OUT$ ( -- str ) OUT OUT-U @ >str ;
: ERR$ ( -- str ) ERR ERR-U @ >str ;

: COPY! ( str ^u8 ^n -- )
   {: s:str dst:^ lenp:^ :}
   s STR:LEN CAP > if E-FS-CAPACITY throw then
   s STR:DATA dst s STR:LEN BYTE-COPY s STR:LEN lenp ! ;

\ <tmp>/<rel> in one of three buffers, so a call can hold an engine and two
\ paths at once.
: A$ ( str -- str )
   {: rel:str :}
   TOP$ rel PA FS:JOIN-PATH PA swap >str ;

: B$ ( str -- str )
   {: rel:str :}
   TOP$ rel PB FS:JOIN-PATH PB swap >str ;

: HB-AT$ ( str -- str )
   {: rel:str :}
   TOP$ rel PE FS:JOIN-PATH PE swap >str ;

\ <gate tree>/<rel>
: GATE-B$ ( str -- str )
   {: rel:str :}
   GATE$ rel PB FS:JOIN-PATH PB swap >str ;

: LINE ( str -- ) SB-APPEND 10 SB-APPEND-C ;

: PROG-RESET ( -- ) 0 PROG-U ! ;

: PLINE ( str -- )
   {: s:str :}
   PROG-U @ s STR:LEN + 1+ PROG-CAP > if E-STR-CAPACITY throw then
   s STR:DATA PROG PROG-U @ + s STR:LEN BYTE-COPY
   10 PROG PROG-U @ s STR:LEN + + c!
   PROG-U @ s STR:LEN + 1+ PROG-U ! ;

: PROG$ ( -- str ) PROG PROG-U @ >str ;

: YN-LINE ( -- )
   S\" : YN ( bool -- ) if s\q yes\q else s\q no\q then type cr ;" PLINE ;

\ `<word> <tmp>/<rel>` as a program line.
: PATH-LINE ( str str -- )
   {: word:str rel:str :}
   SB-RESET word SB-APPEND s"  " SB-APPEND rel A$ SB-APPEND SB$ PLINE ;

: TEXT ( str str -- )
   {: name:str text:str :}
   name A$ text FS:WRITE-ALL ;

\ <tmp>/<link> names <gate tree>/<sub>.
: GATE-LINK ( str str -- )
   {: sub:str link:str :}
   sub GATE-B$ link A$ FS:MAKE-SYMLINK ;

\ <tmp>/<rel> is a copy of the engine under test: a symlink would resolve to
\ the candidate's own location.
: PLACE-ENGINE ( str -- )
   {: rel:str :}
   HB$ rel A$ FS:COPY-FILE-STREAM
   rel A$ FS:CHMOD-X ;

\ <tmp>/b/<path> is a real copy of the gate tree's file: a symlink back would
\ resolve to the gate tree.
: B-COPY ( str -- )
   {: s:str :}
   s FS:FILE? 0= if exit then
   B-ROOT$ s SOURCE-ROOT:CWD$ SOURCE-ROOT:RELATIVE COPY-DST FS:JOIN-PATH {: size:n :}
   COPY-DST size >str SOURCE-ROOT:DIRNAME FS:MAKE-DIRS
   s COPY-DST size >str FS:COPY-FILE-STREAM ;

: B-TREE ( -- )
   s" b" A$ B-ROOT B-ROOT-U COPY!
   s" src" [: B-COPY ;] FS:WALK-FILES
   s" test/fixtures/source-root-exe" [: B-COPY ;] FS:WALK-FILES ;

\ A package whose one word answers n, standing in for a file the engine bakes.
: MARKER ( str n -- )
   {: name:str value:n :}
   SB-RESET
   s" package " SB-APPEND name LINE
   s" public" LINE
   s" : MARK ( -- n ) " SB-APPEND value FMT:SB-U s"  ;" LINE
   s" ;package" LINE ;

: WD-APP ( -- )
   SB-RESET
   s" require test/fixtures/source-root-exe/outer.f" LINE
   s" package SOURCE-ROOT-EXE-WD" LINE
   S\" : YN ( bool -- ) if s\q yes\q else s\q no\q then type cr ;" LINE
   s" SOURCE-ROOT-EXE-OUTER:INNER-MARK 5 = SOURCE-ROOT-EXE-OUTER:MARK 6 = and YN" LINE
   s" ;package" LINE ;

\ A package that names itself as it loads, standing in for a library file.
: DECOY ( str -- )
   {: name:str :}
   SB-RESET
   s" package " SB-APPEND name LINE
   S\" : HELLO ( -- ) s\q " SB-APPEND name SB-APPEND S\" \q type cr ;" LINE
   s" HELLO" LINE
   s" ;package" LINE ;

: CD-FILE ( -- )
   SB-RESET
   s" package SOURCE-ROOT-EXE-CD" LINE
   S\" : HELLO ( -- ) s\q from cd\q type cr ;" LINE
   s" HELLO" LINE
   s" ;package" LINE ;

create DEEP CAP allot
variable DEEP-U

\ The most /<name> levels below <tmp>/deep whose path fits PATH-CAP.
: DEEP-LEVELS ( -- n ) PATH-CAP TOP-U @ - 5 - LONG-NAME 1+ / ;

\ deep and DEEP-LEVELS nested <name>s below it, built in DEEP: the path can
\ outgrow the string builder.
: DEEP-DIRS ( -- )
   s" deep" {: s:str :}
   s STR:DATA DEEP s STR:LEN BYTE-COPY s STR:LEN DEEP-U !
   DEEP-LEVELS 0 ?do
      [char] / DEEP DEEP-U @ + c!
      LONG DEEP DEEP-U @ + 1+ LONG-NAME BYTE-COPY
      LONG-NAME 1+ DEEP-U +!
   loop
   DEEP DEEP-U @ >str A$ FS:MAKE-DIRS ;

\ Each case runs a fresh child of a copy of the engine under test
\ (src/lib/engine-candidate.f), placed in the layout it needs.
: PREP ( -- )
   FS:CLEANUP-RESET
   s" habu-source-root-exe" FS:MKTEMP-DIR SOURCE-ROOT:CANONICAL TTRUE TOP TOP-U COPY!
   TOP$ FS:CLEANUP-TREE+
   SOURCE-ROOT:CWD$ GATE GATE-U COPY!
   ENGINE-CANDIDATE:PATH$ SOURCE-ROOT:CANONICAL TTRUE HB HB-U COPY!
   s" foreign" A$ FS:MAKE-DIR
   s" foreign" A$ FOREIGN FOREIGN-U COPY!
   \ The executable's tree, its files the gate tree's, and a link to its engine.
   s" tree/bin" A$ FS:MAKE-DIRS
   s" tree/bin/hb" PLACE-ENGINE
   s" src" s" tree/src" GATE-LINK
   s" tree/test/fixtures" A$ FS:MAKE-DIRS
   s" test/fixtures/source-root-exe" s" tree/test/fixtures/source-root-exe" GATE-LINK
   s" bin" A$ FS:MAKE-DIR
   s" tree/bin/hb" B$ s" bin/hb" A$ FS:MAKE-SYMLINK
   \ A lone engine, and a tree that has no lib.
   s" x" A$ FS:MAKE-DIR
   s" x/hb" PLACE-ENGINE
   s" p/bin" A$ FS:MAKE-DIRS
   s" p/bin/hb" PLACE-ENGINE
   s" p/src" A$ FS:MAKE-DIR
   s" src/core" s" p/src/core" GATE-LINK
   \ An engine whose directory is not a tree, beside an engine file's name.
   s" q/bin" A$ FS:MAKE-DIRS
   s" q/bin/hb" PLACE-ENGINE
   s" q/src/lib" A$ FS:MAKE-DIRS
   s" SOURCE-ROOT-EXE-NOT-TREE" 4 MARKER s" q/src/lib/error.f" SB$ TEXT
   \ A real copy of the tree, with an engine of its own.
   B-TREE
   s" b/bin" A$ FS:MAKE-DIRS
   s" b/bin/hb" PLACE-ENGINE
   s" app/src/lib" A$ FS:MAKE-DIRS
   s" test/source-root-exe-app.f" s" app/app.f" A$ FS:COPY-FILE-STREAM
   s" SOURCE-ROOT-EXE-APP-ERRORS" 1 MARKER s" app/src/lib/error.f" SB$ TEXT
   s" wd-app/test/fixtures/source-root-exe" A$ FS:MAKE-DIRS
   WD-APP s" wd-app/wd.f" SB$ TEXT
   s" SOURCE-ROOT-EXE-ENTRY-DECOY" DECOY
   s" wd-app/test/fixtures/source-root-exe/inner.f" SB$ TEXT
   s" shadow/test/fixtures/source-root-exe" A$ FS:MAKE-DIRS
   s" SOURCE-ROOT-EXE-WD-DECOY" DECOY
   s" shadow/test/fixtures/source-root-exe/outer.f" SB$ TEXT
   \ A program behind a link of another name requires the file beside it, then
   \ itself.
   s" real" A$ FS:MAKE-DIR
   s" real/x.f" S\" require y.f\nrequire x.f\ns\q x\q type cr\n" TEXT
   s" real/y.f" S\" s\q y\q type cr\n" TEXT
   s" links" A$ FS:MAKE-DIR
   s" real/x.f" B$ s" links/tool" A$ FS:MAKE-SYMLINK
   s" other/nested" A$ FS:MAKE-DIRS
   s" other/src/lib" A$ FS:MAKE-DIRS
   s" SOURCE-ROOT-EXE-OTHER" 3 MARKER s" other/src/lib/error.f" SB$ TEXT
   s" other/nested" B$ s" tree/branch" A$ FS:MAKE-SYMLINK
   s" wd" A$ FS:MAKE-DIR
   s" wd/src" A$ FS:MAKE-DIRS
   s" src/lib" s" wd/src/lib" GATE-LINK
   s" cd/sub" A$ FS:MAKE-DIRS
   CD-FILE s" cd/x.f" SB$ TEXT
   s" cd-load.f" S\" SOURCE-ROOT:CD cd\nrequire x.f\nSOURCE-ROOT:CD\n" TEXT
   s" first.f" S\" \\ A file loaded before CD.\n" TEXT
   s" push-load.f" S\" SOURCE-ROOT:PUSHPATH\n" TEXT
   s" pop-load.f" S\" SOURCE-ROOT:POPPATH\n" TEXT
   s" search-only" A$ FS:MAKE-DIR
   s" locked/inner" A$ FS:MAKE-DIRS
   LONG-NAME 0 ?do 97 LONG i + c! loop
   DEEP-DIRS ;

: RESULT! ( result<pcap:captured,pcap:failed> -- ) PCAP-RESULT:>N RC ! ERR-U ! OUT-U ! ;

\ The child's environment is TMPDIR alone, when this run has one: the
\ scratch a child makes lies under it, and no variable can lead the child
\ to a root.
: CHILD-ENV ( -- )
   PROC-ENV-RESET
   s" TMPDIR" GETENV {: tmp:str :}
   tmp STR:LEN 0= if exit then
   s" TMPDIR" tmp PROC-ENV-SET ;

: RUN-ARGS ( str str -- )
   {: exe:str cwd:str :}
   CHILD-ENV
   exe cwd
   OUT IO-CAP >LEN ERR IO-CAP >LEN PROCESS:WAIT-FOREVER
   PROC-CWD:RUN-ARGV-ENV-CWD-CAPTURE RESULT! ;

: RUN-EXE ( str str str -- )
   {: exe:str cwd:str in:str :}
   CHILD-ENV
   exe cwd in
   OUT IO-CAP >LEN ERR IO-CAP >LEN PROCESS:WAIT-FOREVER
   PROC-CWD:RUN-ARGV-ENV-CWD-STDIN-CAPTURE RESULT! ;

\ <tmp>/tree/bin/hb from <tmp>/foreign, the program on stdin.
: RUN-TREE ( -- )
   PROC-ARGV-RESET
   s" tree/bin/hb" HB-AT$ FOREIGN$ PROG$ RUN-EXE ;

: SHOW ( -- )
   s" child rc " type RC @ FMT:.INT cr
   s" child stdout:" type cr OUT$ type
   s" child stderr:" type cr ERR$ type ;

\ Exactly this stdout and exit status, and nothing on stderr.
: EXPECT ( str n -- )
   {: want:str rc:n :}
   RC @ rc = OUT$ want STR:= and ERR-U @ 0= and 0= if SHOW then
   RC @ rc T=
   OUT$ want T$=
   ERR-U @ 0 T= ;

: STDOUT ( str -- )
   {: want:str :}
   OUT$ want STR:= 0= if SHOW then
   OUT$ want T$= ;

\ The loader's rejection of the missing request name, which it looked for at
\ <dir>/<name> among others, a rejection.
: CANNOT-OPEN ( str str -- )
   {: dir:str name:str :}
   SB-RESET s" error[E-MISSING-SOURCE]: cannot open `" SB-APPEND
   name SB-APPEND 96 SB-APPEND-C
   RC @ 70 = ERR$ SB$ STR:CONTAINS? and 0= if SHOW then
   RC @ 70 T=
   ERR$ SB$ STR:CONTAINS? TTRUE
   SB-RESET dir SB-APPEND 47 SB-APPEND-C name SB-APPEND
   ERR$ SB$ STR:CONTAINS? dup 0= if SHOW then TTRUE ;

\ A rejection record: exit 70, nothing on stdout, this code on
\ stderr.
: REJECTED ( str -- )
   {: code:str :}
   RC @ 70 = OUT-U @ 0= and ERR$ code STR:CONTAINS? and 0= if SHOW then
   RC @ 70 T=
   OUT-U @ 0 T=
   ERR$ code STR:CONTAINS? TTRUE ;

\ The record says this too.
: SAYS ( str -- )
   {: want:str :}
   ERR$ want STR:CONTAINS? dup 0= if SHOW then TTRUE ;

\ The root, whether a frozen row answers, then a library only a root holds.
: ROOT-PROGRAM ( -- )
   PROG-RESET
   s" package SOURCE-ROOT-EXE-TREE" PLINE
   YN-LINE
   s" : RUN ( -- )" PLINE
   s"    SOURCE-ROOT:ENGINE$ type cr" PLINE
   S\"    s\q src/lib/string.f\q REQUIRE-REG:ENGINE-PROVIDES? YN ;" PLINE
   s" RUN" PLINE
   s" ;package" PLINE
   s" require test/fixtures/source-root-exe/inner.f" PLINE ;

: FROM-FOREIGN ( str -- )
   {: exe:str :}
   PROC-ARGV-RESET
   ROOT-PROGRAM
   exe FOREIGN$ PROG$ RUN-EXE
   SB-RESET s" tree" A$ LINE s" yes" LINE
   SB$ 0 EXPECT ;

: EXE-TREE ( -- )
   s" an engine started outside any tree takes its own tree as the root" T-LABEL
   s" tree/bin/hb" HB-AT$ FROM-FOREIGN
   s" so does one started through a symlink to it" T-LABEL
   s" bin/hb" HB-AT$ FROM-FOREIGN
   s" so does one started by a relative path" T-LABEL
   s" ../tree/bin/hb" FROM-FOREIGN ;

\ The executable's tree would make every file of this copy a second copy of an
\ engine file: a duplicate definition, exit 70.
: CWD-TREE ( -- )
   s" a working directory that is a Habu tree is the root" T-LABEL
   PROG-RESET
   s" SOURCE-ROOT:ENGINE$ type cr" PLINE
   s" require src/lib/string.f" PLINE
   s" require test/fixtures/source-root-exe/inner.f" PLINE
   PROC-ARGV-RESET
   s" tree/bin/hb" HB-AT$ s" b" A$ PROG$ RUN-EXE
   SB-RESET s" b" A$ LINE
   SB$ 0 EXPECT ;

: LONE ( -- )
   s" a lone engine in a tree's working directory takes that tree" T-LABEL
   PROC-ARGV-RESET
   s" x/hb" HB-AT$ GATE$ S\" SOURCE-ROOT:ENGINE$ type cr\n" RUN-EXE
   SB-RESET GATE$ LINE SB$ 0 EXPECT
   s" elsewhere it takes the working directory, and finds no library there" T-LABEL
   PROC-ARGV-RESET
   s" x/hb" HB-AT$ FOREIGN$ S\" SOURCE-ROOT:ENGINE$ type cr\nrequire test/fixtures/source-root-exe/inner.f\n"
   RUN-EXE
   SB-RESET FOREIGN$ LINE SB$ STDOUT
   FOREIGN$ s" test/fixtures/source-root-exe/inner.f" CANNOT-OPEN ;

\ The tree's lib is missing, so its frozen rows answer by name, also under a
\ discovery base, and a library is rejected naming the owner's path.
: PARTIAL ( -- )
   s" a tree without the library is the root, and nothing falls back" T-LABEL
   PROG-RESET
   s" package SOURCE-ROOT-EXE-PARTIAL" PLINE
   YN-LINE
   s" : RUN ( -- )" PLINE
   s"    SOURCE-ROOT:ENGINE$ type cr" PLINE
   S\"    s\q src/lib/string.f\q REQUIRE-REG:ENGINE-PROVIDES? YN" PLINE
   s"    REQUIRE-REG:SNAPSHOT" PLINE
   S\"    s\q src/lib/string.f\q REQUIRE-REG:ENGINE-PROVIDES? YN" PLINE
   s"    REQUIRE-REG:RESTORE ;" PLINE
   s" RUN" PLINE
   s" ;package" PLINE
   s" require test/fixtures/source-root-exe/inner.f" PLINE
   PROC-ARGV-RESET
   s" p/bin/hb" HB-AT$ FOREIGN$ PROG$ RUN-EXE
   SB-RESET s" p" A$ LINE s" yes" LINE s" yes" LINE SB$ STDOUT
   FOREIGN$ s" test/fixtures/source-root-exe/inner.f" CANNOT-OPEN ;

\ After the program's load lines: the root, and whether q's file loaded.
: NOT-TREE-RUN ( -- )
   s" package SOURCE-ROOT-EXE-Q" PLINE
   YN-LINE
   s" : RUN ( -- )" PLINE
   s"    SOURCE-ROOT:ENGINE$ type cr" PLINE
   s"    SOURCE-ROOT-EXE-NOT-TREE:MARK 4 = YN ;" PLINE
   s" RUN" PLINE
   s" ;package" PLINE
   PROC-ARGV-RESET
   s" q/bin/hb" HB-AT$ FOREIGN$ PROG$ RUN-EXE
   SB-RESET FOREIGN$ LINE s" yes" LINE SB$ 0 EXPECT ;

\ q holds no first row, so the working directory is the root and q's
\ src/lib/error.f is an application file, by its absolute path and under CD.
: NOT-TREE ( -- )
   s" a file beside an engine whose directory is not a tree is the application's"
   T-LABEL
   PROG-RESET
   SB-RESET S\" s\q " SB-APPEND s" q/src/lib/error.f" A$ SB-APPEND
   S\" \q required" SB-APPEND SB$ PLINE
   NOT-TREE-RUN
   s" so is one a relative require reaches under CD" T-LABEL
   PROG-RESET
   s" SOURCE-ROOT:CD" s" q" PATH-LINE
   s" require src/lib/error.f" PLINE
   NOT-TREE-RUN ;

: APP ( -- )
   s" an app outside the tree resolves its requires through the root" T-LABEL
   PROC-ARGV-RESET
   s" --load" PROC-ARGV+
   s" app/app.f" A$ PROC-ARGV+
   s" --" PROC-ARGV+
   FOREIGN$ PROC-ARGV+
   s" tree" A$ PROC-ARGV+
   s" tree/bin/hb" HB-AT$ FOREIGN$ RUN-ARGS
   S\" test: ok\n" 0 EXPECT ;

\ A relative dependency searches its owner, the root, then the working
\ directory; a relative --load entry is relative to the working directory only;
\ only the root's own copy matches a frozen engine row.
\ The entry lacks the fixture's outer.f, which the working directory holds, and
\ holds its inner.f, which the root's outer.f requires: each decoy would print
\ its package name, and leave SOURCE-ROOT-EXE-OUTER or -INNER undefined.
: WD ( -- )
   s" a library comes from the root, not the working or the entry directory" T-LABEL
   PROC-ARGV-RESET
   s" --load" PROC-ARGV+
   s" wd-app/wd.f" A$ PROC-ARGV+
   s" tree/bin/hb" HB-AT$ s" shadow" A$ RUN-ARGS
   S\" yes\n" 0 EXPECT
   s" so does one for a program file, whose own directory is its first root" T-LABEL
   PROC-ARGV-RESET
   s" wd-app/wd.f" A$ PROC-ARGV+
   s" tree/bin/hb" HB-AT$ s" shadow" A$ NULL$ RUN-EXE
   S\" yes\n" 0 EXPECT
   s" a program behind a renamed symlink loads once, from its own directory" T-LABEL
   PROC-ARGV-RESET
   s" links/tool" A$ PROC-ARGV+
   s" tree/bin/hb" HB-AT$ s" shadow" A$ NULL$ RUN-EXE
   S\" y\nx\n" 0 EXPECT ;

: ENTRY ( -- )
   s" a relative --load entry is relative to the working directory only" T-LABEL
   PROC-ARGV-RESET
   s" --load" PROC-ARGV+
   s" test/fixtures/source-root-exe/inner.f" PROC-ARGV+
   s" tree/bin/hb" HB-AT$ FOREIGN$ RUN-ARGS
   NULL$ STDOUT
   FOREIGN$ s" test/fixtures/source-root-exe/inner.f" CANNOT-OPEN ;

\ The root's src is a symlink into the gate tree; branch is a symlink
\ elsewhere, so branch/../src/lib/error.f only reads like the root's file.
: ALIAS ( -- )
   s" the root's files keep the engine's identity through its symlinks" T-LABEL
   PROG-RESET
   s" require src/lib/error.f" PLINE
   s" require src/core/util.f" PLINE
   s" require branch/../src/lib/error.f" PLINE
   s" package SOURCE-ROOT-EXE-ALIAS" PLINE
   YN-LINE
   s" : RUN ( -- )" PLINE
   S\"    s\q src/lib/error.f\q REQUIRE-REG:ENGINE-PROVIDES? YN" PLINE
   S\"    s\q src/core/util.f\q REQUIRE-REG:ENGINE-PROVIDES? YN" PLINE
   S\"    s\q branch/../src/lib/error.f\q REQUIRE-REG:ENGINE-PROVIDES? YN" PLINE
   s"    SOURCE-ROOT-EXE-OTHER:MARK 3 = YN ;" PLINE
   s" RUN" PLINE
   s" ;package" PLINE
   RUN-TREE
   S\" yes\nyes\nno\nyes\n" 0 EXPECT ;

\ The root's own files reached through another root are still the engine's: a
\ second load of one would exit 70 on its first definition.
: FIX-1 ( -- )
   s" the root's file required through its parent directory is the engine's" T-LABEL
   PROC-ARGV-RESET
   s" b/bin/hb" HB-AT$ TOP$ S\" SOURCE-ROOT:ENGINE$ type cr\nrequire b/src/lib/error.f\n"
   RUN-EXE
   SB-RESET s" b" A$ LINE SB$ 0 EXPECT
   s" the root's files reached through a symlinked working directory are the engine's"
   T-LABEL
   PROC-ARGV-RESET
   s" tree/bin/hb" HB-AT$ s" wd" A$ S\" require src/lib/string.f\nrequire src/lib/fs.f\n"
   RUN-EXE
   NULL$ 0 EXPECT ;

\ The working directory's src/lib/error.f is provided by its absolute path, so a
\ require of src/lib/error.f stops there, at the application's fact.
: FIX-2 ( -- )
   s" an application fact steers REQUIRE-REG:ENGINE-PROVIDES? and leaves the resolved root" T-LABEL
   PROG-RESET
   s" package SOURCE-ROOT-EXE-FACT" PLINE
   YN-LINE
   s" : RUN ( -- )" PLINE
   S\"    SOURCE-ROOT:CWD$ s\q src/lib/error.f\q SOURCE-ROOT:JOIN provided" PLINE
   S\"    s\q test/fixtures/source-root-exe/inner.f\q SOURCE-ROOT:RESOLVE drop drop" PLINE
   S\"    s\q src/lib/error.f\q REQUIRE-REG:ENGINE-PROVIDES? YN" PLINE
   s"    SOURCE-ROOT:RESOLVED-ROOT$ type cr ;" PLINE
   s" RUN" PLINE
   s" ;package" PLINE
   RUN-TREE
   SB-RESET s" no" LINE s" tree" A$ LINE SB$ 0 EXPECT ;

: BOOT-OUTSIDE ( -- )
   s" a boot file outside the root is rejected by name" T-LABEL
   PROG-RESET
   s" package SOURCE-ROOT-EXE-BOOT" PLINE
   s" : RUN ( -- )" PLINE
   s"    REQUIRE-REG:BOOT-OPEN" PLINE
   S\"    SOURCE-ROOT:CWD$ s\q x.f\q SOURCE-ROOT:JOIN required ;" PLINE
   s" RUN" PLINE
   s" ;package" PLINE
   s" foreign/x.f" NULL$ TEXT
   RUN-TREE
   s" error[E-SOURCE-ROOT]" REJECTED
   SB-RESET 96 SB-APPEND-C s" foreign/x.f" A$ SB-APPEND
   s" ` is a boot file outside the engine root" SB-APPEND SB$ SAYS ;

: CD-PROG ( str -- ) {: rel:str :}
   PROG-RESET s" source-root:cd" rel PATH-LINE ;

\ CD's rejection naming <tmp>/<rel>, and why.
: CD-WANT ( str str -- )
   {: rel:str want:str :}
   s" error[E-SOURCE-ROOT]" REJECTED
   SB-RESET s" `source-root:cd` got `" SB-APPEND rel A$ SB-APPEND 96 SB-APPEND-C SB$ SAYS
   want SAYS ;

: CD-REJECTED ( str str str -- )
   {: label:str rel:str want:str :}
   label T-LABEL
   rel CD-PROG
   RUN-TREE
   rel want CD-WANT ;

\ The canon fails behind a parent with no search permission, as it does for a
\ missing directory; the parent's mode comes back even when the run throws, so
\ the cleanup can remove the tree.
: CD-LOCKED ( -- )
   s" CD to a directory behind an unsearchable parent is rejected by name" T-LABEL
   s" locked" A$ NO-ACCESS FS:CHMOD-MODE
   s" locked/inner" CD-PROG
   [: RUN-TREE ;] [: s" locked" A$ MODE-DIR FS:CHMOD-MODE ;] finally
   s" locked/inner" s" does not resolve to a directory" CD-WANT ;

: CD-SEARCH-ONLY ( -- )
   s" a directory searchable without read permission is a current path" T-LABEL
   s" search-only" A$ SEARCH-ONLY FS:CHMOD-MODE
   PROG-RESET s" SOURCE-ROOT:CD" s" search-only" PATH-LINE s" SOURCE-ROOT:CD" PLINE
   RUN-TREE
   s" search-only" A$ MODE-DIR FS:CHMOD-MODE
   SB-RESET s" search-only" A$ LINE SB$ 0 EXPECT ;

\ Each step is one prompt line, so the path grows past the cap only by joining.
\ The first DEEP-LEVELS steps reach existing paths that fit; the next is over.
: CD-TOO-LONG ( -- )
   s" a current path longer than a loader path is rejected by name" T-LABEL
   PROG-RESET s" SOURCE-ROOT:CD" s" deep" PATH-LINE
   DEEP-LEVELS 1+ 0 do SB-RESET s" SOURCE-ROOT:CD " SB-APPEND LONG LONG-NAME >str SB-APPEND SB$ PLINE loop
   RUN-TREE
   s" error[E-PATH-RANGE]" REJECTED
   s" `SOURCE-ROOT:CD` got a path" SAYS
   s" bytes a path may have" SAYS ;

\ A load's CD moves the root for the rest of that file, as a relative require
\ there shows; the load's end restores the root it began with.
: PATH-IN-LOAD ( -- )
   s" CD in a loaded file lasts to the end of that file" T-LABEL
   PROG-RESET
   s" include" s" cd-load.f" PATH-LINE
   s" SOURCE-ROOT:CD" PLINE
   RUN-TREE
   SB-RESET s" from cd" LINE s" cd" A$ LINE FOREIGN$ LINE SB$ 0 EXPECT ;

\ The first CD parses `cd` from the line that calls G; the second prints the
\ scope's path and the third, after the scope, the caller's.
: PATH-IN-ROOT ( -- )
   s" CD in a root scope lasts to the end of the scope" T-LABEL
   PROG-RESET
   SB-RESET S\" : G ( -- ) s\q " SB-APPEND TOP$ SB-APPEND
   S\" \q [: SOURCE-ROOT:CD SOURCE-ROOT:CD ;] SOURCE-ROOT:WITH SOURCE-ROOT:CD ;" SB-APPEND
   SB$ PLINE
   s" G cd" PLINE
   RUN-TREE
   SB-RESET s" cd" A$ LINE FOREIGN$ LINE SB$ 0 EXPECT ;

\ A path saved in a scope is dropped at its end, and a scope restores no path
\ saved outside it.
: PATH-SAVED-IN-SCOPE ( -- )
   s" a load's PUSHPATH leaves nothing for a later POPPATH" T-LABEL
   PROG-RESET
   s" include" s" push-load.f" PATH-LINE
   s" SOURCE-ROOT:POPPATH" PLINE
   RUN-TREE
   s" error[E-SOURCE-ROOT]" REJECTED
   s" `SOURCE-ROOT:POPPATH` runs with no directory saved" SAYS
   s" a load's POPPATH restores no path saved before the load" T-LABEL
   PROG-RESET
   s" SOURCE-ROOT:PUSHPATH" PLINE
   s" include" s" pop-load.f" PATH-LINE
   RUN-TREE
   s" error[E-SOURCE-ROOT]" REJECTED
   s" `SOURCE-ROOT:POPPATH` runs with no directory saved" SAYS ;

\ A null slot is the working directory, which the last POPPATH restores.
: PATH-STACK ( -- )
   s" PUSHPATH and POPPATH save and restore the current path" T-LABEL
   PROG-RESET
   s" SOURCE-ROOT:PUSHPATH" PLINE
   s" SOURCE-ROOT:CD" s" cd" PATH-LINE
   s" SOURCE-ROOT:PUSHPATH" PLINE
   s" SOURCE-ROOT:CD sub" PLINE
   s" SOURCE-ROOT:CD" PLINE
   s" SOURCE-ROOT:POPPATH" PLINE
   s" SOURCE-ROOT:CD" PLINE
   s" SOURCE-ROOT:POPPATH" PLINE
   s" SOURCE-ROOT:CD" PLINE
   RUN-TREE
   SB-RESET s" cd/sub" A$ LINE s" cd" A$ LINE FOREIGN$ LINE SB$ 0 EXPECT
   s" POPPATH on an empty stack is rejected, named as written" T-LABEL
   PROG-RESET s" source-root:poppath" PLINE
   RUN-TREE
   s" error[E-SOURCE-ROOT]" REJECTED
   s" `source-root:poppath` runs with no directory saved" SAYS ;

: PATH-WORDS ( -- )
   s" CD names the current path, which include searches first" T-LABEL
   PROG-RESET
   \ A load first: CD then replaces the path that load restored.
   s" include" s" first.f" PATH-LINE
   s" SOURCE-ROOT:CD" s" cd" PATH-LINE
   s" include x.f" PLINE
   s" SOURCE-ROOT:CD sub" PLINE
   s" SOURCE-ROOT:CD" PLINE
   RUN-TREE
   SB-RESET s" from cd" LINE s" cd/sub" A$ LINE SB$ 0 EXPECT
   s" CD to a missing directory is rejected by name" s" absent"
   s" does not resolve to a directory" CD-REJECTED
   CD-LOCKED
   s" CD to a file is rejected by name" s" cd/x.f"
   s" is not a directory this process can search" CD-REJECTED
   CD-SEARCH-ONLY CD-TOO-LONG PATH-IN-LOAD PATH-IN-ROOT PATH-SAVED-IN-SCOPE
   PATH-STACK ;

: MAIN ( -- )
   T-RESET
   PREP
   EXE-TREE CWD-TREE LONE PARTIAL NOT-TREE
   APP WD ENTRY ALIAS FIX-1 FIX-2 BOOT-OUTSIDE
   PATH-WORDS
   FS:CLEANUP-RUN
   T-REPORT
   s" source-root-exe-test: ok" type cr ;

MAIN

;package
