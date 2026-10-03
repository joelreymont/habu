\ source-root-exe-test.f - the engine source root, found from the executable.
\
\ An editor starts a language server in a working directory it cannot choose,
\ so the engine finds its own source root from where its executable lives
\ (src/core/include.f SOURCE-ROOT:ENGINE$), with no environment variable. Each
\ case runs a fresh child of a copy of the engine under test
\ (lib/engine-candidate.f), placed in the layout the case needs, with an empty
\ environment, and checks what the child prints:
\ - the root is the working directory when it is a Habu tree (it holds the
\   engine's first boot row), else the tree two levels above the executable
\   when that is one, else the working directory; the executable is found by
\   its absolute path, through a symlink and by a relative path;
\ - a relative dependency searches its owner, the working directory, then the
\   root; a relative --load entry is relative to the working directory only; a
\   library no root holds is refused by name, with no fallback;
\ - only the root's own copy matches a frozen engine row, wherever the search
\   reaches it: an app's own file, a working-directory file and a file beside
\   an executable whose directory is not a tree, each with an engine file's
\   name, stay theirs, the root's files reached through its parent or
\   through a symlinked working directory are the engine's, a `..` spelling
\   that only looks like the root is not, and a frozen row with no file under
\   the root still answers;
\ - an application fact steers ENGINE-PROVIDES? as it steers require, and the
\   question leaves the resolved root alone;
\ - a boot file outside the root is refused by name;
\ - SOURCE-ROOT:CD, PUSHPATH and POPPATH move the top-level current path,
\   qualified or under `using SOURCE-ROOT`, and no image is saved with a path
\   pushed;
\ - a captured image keeps neither its engine's tree nor its root, and finds
\   its own;
\ - a native build from a directory that is not a tree dies at its first row
\   and writes nothing.

require lib/test.f
require lib/string.f
require lib/fmt.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process-argv.f
require lib/process-cwd.f
require lib/engine-candidate.f

using SOURCE-ROOT
package SOURCE-ROOT-EXE-TEST
private

FS-PATH-CAP 1+ constant CAP
$10000 constant IO-CAP
$1000 constant PROG-CAP
\ Guards against a hung child, not budgets: the host runs loaded.
120000 constant TIMEOUT-MS
\ A save compiles src/habu/app-image.f from source, which a loaded host makes slow.
600000 constant SAVE-TIMEOUT-MS
1800000 constant BUILD-TIMEOUT-MS
\ A prompt line holds 255 bytes, so `cd <name>` names at most this much; four
\ nested names fit the 1024-byte path cap and a fifth does not.
200 constant LONG-NAME
\ One more than the path stack holds (src/core/include.f PATH-DEPTH).
9 constant OVER-DEPTH
\ Search without read permission (0111).
$49 constant SEARCH-ONLY

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

: GATE$ ( -- ptr u8 n ) GATE GATE-U @ ;
: TOP$ ( -- ptr u8 n ) TOP TOP-U @ ;
: FOREIGN$ ( -- ptr u8 n ) FOREIGN FOREIGN-U @ ;
: HB$ ( -- ptr u8 n ) HB HB-U @ ;
: B-ROOT$ ( -- ptr u8 n ) B-ROOT B-ROOT-U @ ;
: OUT$ ( -- ptr u8 n ) OUT OUT-U @ ;
: ERR$ ( -- ptr u8 n ) ERR ERR-U @ ;

: COPY! ( ptr u8 n ptr u8 ptr n -- )
   {: a:ptr u:n dst:ptr lenp:ptr :}
   u CAP > if E-FS-CAPACITY throw then
   a dst u BYTE-COPY u lenp ! ;

\ <tmp>/<rel> in one of three buffers, so a call can hold an engine and two
\ paths at once.
: A$ ( ptr u8 n -- ptr u8 n )
   {: rel:ptr relu:n :}
   TOP$ rel relu PA JOIN-PATH PA swap ;

: B$ ( ptr u8 n -- ptr u8 n )
   {: rel:ptr relu:n :}
   TOP$ rel relu PB JOIN-PATH PB swap ;

: HB-AT$ ( ptr u8 n -- ptr u8 n )
   {: rel:ptr relu:n :}
   TOP$ rel relu PE JOIN-PATH PE swap ;

\ <gate tree>/<rel>
: GATE-B$ ( ptr u8 n -- ptr u8 n )
   {: rel:ptr relu:n :}
   GATE$ rel relu PB JOIN-PATH PB swap ;

: LINE ( ptr u8 n -- ) SB-APPEND 10 SB-APPEND-C ;

\ ---- the program a child reads on stdin -----------------------------------------

: PROG-RESET ( -- ) 0 PROG-U ! ;

: PLINE ( ptr u8 n -- )
   {: a:ptr u:n :}
   PROG-U @ u + 1+ PROG-CAP > if E-STR-CAPACITY throw then
   a PROG PROG-U @ + u BYTE-COPY
   10 PROG PROG-U @ u + + c!
   PROG-U @ u + 1+ PROG-U ! ;

: PROG$ ( -- ptr u8 n ) PROG PROG-U @ ;

: YN-LINE ( -- )
   S\" : YN ( bool -- ) if s\q yes\q else s\q no\q then type cr ;" PLINE ;

\ `<word> <tmp>/<rel>` as a program line.
: PATH-LINE ( ptr u8 n ptr u8 n -- )
   {: word:ptr wordu:n rel:ptr relu:n :}
   SB-RESET word wordu SB-APPEND s"  " SB-APPEND rel relu A$ SB-APPEND SB$ PLINE ;

\ ---- the fixture tree ----------------------------------------------------------

: TEXT ( ptr u8 n ptr u8 n -- )
   {: name:ptr nameu:n text:ptr textu:n :}
   name nameu A$ text textu WRITE-ALL ;

\ <tmp>/<link> names <gate tree>/<sub>.
: GATE-LINK ( ptr u8 n ptr u8 n -- )
   {: sub:ptr subu:n link:ptr linku:n :}
   sub subu GATE-B$ link linku A$ MAKE-SYMLINK ;

\ <tmp>/<rel> is a copy of the engine under test: a symlink would resolve to
\ the candidate's own location.
: PLACE-ENGINE ( ptr u8 n -- )
   {: rel:ptr relu:n :}
   HB$ rel relu A$ COPY-FILE-STREAM
   rel relu A$ CHMOD-X ;

\ <tmp>/b/<path> is a real copy of the gate tree's file: a symlink back would
\ resolve to the gate tree.
: B-COPY ( ptr u8 n -- )
   {: a:ptr u:n :}
   a u FILE? 0= if exit then
   B-ROOT$ a u CWD$ RELATIVE COPY-DST JOIN-PATH {: size:n :}
   COPY-DST size DIRNAME MAKE-DIRS
   a u COPY-DST size COPY-FILE-STREAM ;

: B-TREE ( -- )
   s" b" A$ B-ROOT B-ROOT-U COPY!
   s" src" [: B-COPY ;] WALK-FILES
   s" lib" [: B-COPY ;] WALK-FILES ;

\ A package whose one word answers n, standing in for a file the engine bakes.
: MARKER ( ptr u8 n n -- )
   {: name:ptr nameu:n value:n :}
   SB-RESET
   s" package " SB-APPEND name nameu LINE
   s" public" LINE
   s" : MARK ( -- n ) " SB-APPEND value FMT:SB-U s"  ;" LINE
   s" ;package" LINE ;

: WD-APP ( -- )
   SB-RESET
   s" require lib/memory.f" LINE
   s" package SOURCE-ROOT-EXE-WD" LINE
   S\" : YN ( bool -- ) if s\q yes\q else s\q no\q then type cr ;" LINE
   s" : RUN ( -- )" LINE
   s"    SOURCE-ROOT-EXE-SHADOW:MARK 2 = YN" LINE
   S\"    s\q lib/memory.f\q ENGINE-PROVIDES? YN ;" LINE
   s" RUN" LINE
   s" ;package" LINE ;

: CD-FILE ( -- )
   SB-RESET
   s" package SOURCE-ROOT-EXE-CD" LINE
   S\" : HELLO ( -- ) s\q from cd\q type cr ;" LINE
   s" HELLO" LINE
   s" ;package" LINE ;

\ deep/<name>/<name>/<name>/<name>
: DEEP-DIRS ( -- )
   SB-RESET s" deep" SB-APPEND
   4 0 do s" /" SB-APPEND LONG LONG-NAME SB-APPEND loop
   SB$ A$ MAKE-DIRS ;

: PREP ( -- )
   CLEANUP-RESET
   s" habu-source-root-exe" HB-TMP-MKDIR CANONICAL TTRUE TOP TOP-U COPY!
   TOP$ CLEANUP-TREE+
   CWD$ GATE GATE-U COPY!
   ENGINE-CANDIDATE:PATH$ CANONICAL TTRUE HB HB-U COPY!
   s" foreign" A$ MAKE-DIR
   s" foreign" A$ FOREIGN FOREIGN-U COPY!
   \ The executable's tree, its files the gate tree's, and a link to its engine.
   s" tree/bin" A$ MAKE-DIRS
   s" tree/bin/hb" PLACE-ENGINE
   s" lib" s" tree/lib" GATE-LINK
   s" src" s" tree/src" GATE-LINK
   s" bin" A$ MAKE-DIR
   s" tree/bin/hb" B$ s" bin/hb" A$ MAKE-SYMLINK
   \ A lone engine, and a tree that has no lib.
   s" x" A$ MAKE-DIR
   s" x/hb" PLACE-ENGINE
   s" p/bin" A$ MAKE-DIRS
   s" p/bin/hb" PLACE-ENGINE
   s" src" s" p/src" GATE-LINK
   \ An engine whose directory is not a tree, beside an engine file's name.
   s" q/bin" A$ MAKE-DIRS
   s" q/bin/hb" PLACE-ENGINE
   s" q/lib" A$ MAKE-DIRS
   s" SOURCE-ROOT-EXE-NOT-TREE" 4 MARKER s" q/lib/errors.f" SB$ TEXT
   \ A real copy of the tree, with an engine of its own.
   B-TREE
   s" b/bin" A$ MAKE-DIRS
   s" b/bin/hb" PLACE-ENGINE
   s" app/lib" A$ MAKE-DIRS
   s" test/source-root-exe-app.f" s" app/app.f" A$ FS-MUT-COPY-CAP COPY-FILE
   s" SOURCE-ROOT-EXE-APP-ERRORS" 1 MARKER s" app/lib/errors.f" SB$ TEXT
   s" wd-app" A$ MAKE-DIR
   WD-APP s" wd-app/wd.f" SB$ TEXT
   s" shadow/lib" A$ MAKE-DIRS
   s" SOURCE-ROOT-EXE-SHADOW" 2 MARKER s" shadow/lib/memory.f" SB$ TEXT
   s" other/nested" A$ MAKE-DIRS
   s" other/lib" A$ MAKE-DIRS
   s" SOURCE-ROOT-EXE-OTHER" 3 MARKER s" other/lib/errors.f" SB$ TEXT
   s" other/nested" B$ s" tree/branch" A$ MAKE-SYMLINK
   s" wd" A$ MAKE-DIR
   s" lib" s" wd/lib" GATE-LINK
   s" cd/sub" A$ MAKE-DIRS
   CD-FILE s" cd/x.f" SB$ TEXT
   SB-RESET s" SOURCE-ROOT:CD " SB-APPEND TOP$ LINE s" cd-load.f" SB$ TEXT
   s" first.f" S\" \\ A file loaded before CD.\n" TEXT
   s" push-load.f" S\" SOURCE-ROOT:PUSHPATH\n" TEXT
   s" pop-load.f" S\" SOURCE-ROOT:POPPATH\n" TEXT
   s" search-only" A$ MAKE-DIR
   LONG-NAME 0 ?do 97 LONG i + c! loop
   DEEP-DIRS
   s" moved/bin" A$ MAKE-DIRS
   s" lib" s" moved/lib" GATE-LINK
   s" src" s" moved/src" GATE-LINK
   s" cwd" A$ MAKE-DIR
   s" tools" s" cwd/tools" GATE-LINK ;

\ ---- one child ------------------------------------------------------------------

: RESULT! ( result<pcap:captured,pcap:failed> -- )
   MATCH result
      ok OF PCAP-CAPTURED:UNMAKE 0 ENDOF
      err OF PCAP-FAILED:UNMAKE RC>N ENDOF
   ;MATCH
   {: outu:len erru:len rc:n :}
   outu LEN>N OUT-U ! erru LEN>N ERR-U ! rc RC ! ;

\ The engine at exe in cwd, with the argv the caller staged.
: RUN-ARGS ( ptr u8 n ptr u8 n n -- )
   {: exe:ptr exeu:n cwd:ptr cwdu:n ms:n :}
   exe exeu >LEN cwd cwdu >LEN
   OUT IO-CAP >LEN ERR IO-CAP >LEN ms >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-CAPTURE RESULT! ;

\ The same with a program on stdin.
: RUN-EXE ( ptr u8 n ptr u8 n ptr u8 n n -- )
   {: exe:ptr exeu:n cwd:ptr cwdu:n in:ptr inu:n ms:n :}
   exe exeu >LEN cwd cwdu >LEN in inu >LEN
   OUT IO-CAP >LEN ERR IO-CAP >LEN ms >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-STDIN-CAPTURE RESULT! ;

\ <tmp>/tree/bin/hb from <tmp>/foreign, the program on stdin.
: RUN-TREE ( -- )
   PROC-ARGV-RESET
   s" tree/bin/hb" HB-AT$ FOREIGN$ PROG$ TIMEOUT-MS RUN-EXE ;

: SHOW ( -- )
   s" child rc " type RC @ FMT:.INT cr
   s" child stdout:" type cr OUT$ type
   s" child stderr:" type cr ERR$ type ;

\ Exactly this stdout and exit status, and nothing on stderr.
: EXPECT ( ptr u8 n n -- )
   {: want:ptr wantu:n rc:n :}
   RC @ rc = OUT$ want wantu STR= and ERR-U @ 0= and 0= if SHOW then
   RC @ rc T=
   OUT$ want wantu T$=
   ERR-U @ 0 T= ;

: STDOUT ( ptr u8 n -- )
   {: want:ptr wantu:n :}
   OUT$ want wantu STR= 0= if SHOW then
   OUT$ want wantu T$= ;

\ The loader's open failure for <dir><name>, exit 74.
: CANNOT-OPEN ( ptr u8 n ptr u8 n -- )
   {: dir:ptr diru:n name:ptr nameu:n :}
   SB-RESET s" include: cannot open " SB-APPEND dir diru SB-APPEND name nameu SB-APPEND
   RC @ INCLUDE-IO-RC = ERR$ SB$ CONTAINS? and 0= if SHOW then
   RC @ INCLUDE-IO-RC T=
   ERR$ SB$ CONTAINS? TTRUE ;

\ A refusal by name: exit 74, nothing on stdout, exactly this on stderr.
: REFUSAL ( ptr u8 n -- )
   {: want:ptr wantu:n :}
   RC @ INCLUDE-IO-RC = OUT-U @ 0= and ERR$ want wantu STR= and 0= if SHOW then
   RC @ INCLUDE-IO-RC T=
   OUT-U @ 0 T=
   ERR$ want wantu T$= ;

\ ---- the root -------------------------------------------------------------------

\ The root, whether a frozen row answers, then a library only a root holds.
: ROOT-PROGRAM ( -- )
   PROG-RESET
   s" package SOURCE-ROOT-EXE-TREE" PLINE
   YN-LINE
   s" : RUN ( -- )" PLINE
   s"    SOURCE-ROOT:ENGINE$ type cr" PLINE
   S\"    s\q lib/string.f\q ENGINE-PROVIDES? YN ;" PLINE
   s" RUN" PLINE
   s" ;package" PLINE
   s" require lib/fs.f" PLINE ;

: FROM-FOREIGN ( ptr u8 n -- )
   {: exe:ptr exeu:n :}
   PROC-ARGV-RESET
   ROOT-PROGRAM
   exe exeu FOREIGN$ PROG$ TIMEOUT-MS RUN-EXE
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
\ engine file: a duplicate definition, exit 78.
: CWD-TREE ( -- )
   s" a working directory that is a Habu tree is the root" T-LABEL
   PROG-RESET
   s" SOURCE-ROOT:ENGINE$ type cr" PLINE
   s" require lib/string.f" PLINE
   s" require lib/fs.f" PLINE
   PROC-ARGV-RESET
   s" tree/bin/hb" HB-AT$ s" b" A$ PROG$ TIMEOUT-MS RUN-EXE
   SB-RESET s" b" A$ LINE
   SB$ 0 EXPECT ;

: LONE ( -- )
   s" a lone engine in a tree's working directory takes that tree" T-LABEL
   PROC-ARGV-RESET
   s" x/hb" HB-AT$ GATE$ S\" SOURCE-ROOT:ENGINE$ type cr\n" TIMEOUT-MS RUN-EXE
   SB-RESET GATE$ LINE SB$ 0 EXPECT
   s" elsewhere it takes the working directory, and finds no library there" T-LABEL
   PROC-ARGV-RESET
   s" x/hb" HB-AT$ FOREIGN$ S\" SOURCE-ROOT:ENGINE$ type cr\nrequire lib/fs.f\n"
   TIMEOUT-MS RUN-EXE
   SB-RESET FOREIGN$ LINE SB$ STDOUT
   FOREIGN$ s" /lib/fs.f" CANNOT-OPEN ;

\ The tree's lib is missing, so its frozen rows answer by name, also under a
\ discovery base, and a library is refused naming the owner's path.
: PARTIAL ( -- )
   s" a tree without the library is the root, and nothing falls back" T-LABEL
   PROG-RESET
   s" package SOURCE-ROOT-EXE-PARTIAL" PLINE
   YN-LINE
   s" : RUN ( -- )" PLINE
   s"    SOURCE-ROOT:ENGINE$ type cr" PLINE
   S\"    s\q lib/string.f\q ENGINE-PROVIDES? YN" PLINE
   s"    REQUIRE-SNAPSHOT" PLINE
   S\"    s\q lib/string.f\q ENGINE-PROVIDES? YN" PLINE
   s"    REQUIRE-RESTORE ;" PLINE
   s" RUN" PLINE
   s" ;package" PLINE
   s" require lib/fs.f" PLINE
   PROC-ARGV-RESET
   s" p/bin/hb" HB-AT$ FOREIGN$ PROG$ TIMEOUT-MS RUN-EXE
   SB-RESET s" p" A$ LINE s" yes" LINE s" yes" LINE SB$ STDOUT
   FOREIGN$ s" /lib/fs.f" CANNOT-OPEN ;

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
   s" q/bin/hb" HB-AT$ FOREIGN$ PROG$ TIMEOUT-MS RUN-EXE
   SB-RESET FOREIGN$ LINE s" yes" LINE SB$ 0 EXPECT ;

\ q holds no first row, so the working directory is the root and q's
\ lib/errors.f is an application file, by its absolute path and under CD.
: NOT-TREE ( -- )
   s" a file beside an engine whose directory is not a tree is the application's"
   T-LABEL
   PROG-RESET
   SB-RESET S\" s\q " SB-APPEND s" q/lib/errors.f" A$ SB-APPEND
   S\" \q required" SB-APPEND SB$ PLINE
   NOT-TREE-RUN
   s" so is one a relative require reaches under CD" T-LABEL
   PROG-RESET
   s" SOURCE-ROOT:CD" s" q" PATH-LINE
   s" require lib/errors.f" PLINE
   NOT-TREE-RUN ;

\ ---- resolution and identity -----------------------------------------------------

: APP ( -- )
   s" an app outside the tree resolves its requires through the root" T-LABEL
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   s" app/app.f" A$ >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   FOREIGN$ >LEN PROC-ARGV+
   s" tree" A$ >LEN PROC-ARGV+
   s" tree/bin/hb" HB-AT$ FOREIGN$ TIMEOUT-MS RUN-ARGS
   S\" test: ok\n" 0 EXPECT ;

: WD ( -- )
   s" a working-directory file with an engine file's name stays its own" T-LABEL
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   s" wd-app/wd.f" A$ >LEN PROC-ARGV+
   s" tree/bin/hb" HB-AT$ s" shadow" A$ TIMEOUT-MS RUN-ARGS
   S\" yes\nno\n" 0 EXPECT ;

: ENTRY ( -- )
   s" a relative --load entry is relative to the working directory only" T-LABEL
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   s" lib/fs.f" >LEN PROC-ARGV+
   s" tree/bin/hb" HB-AT$ FOREIGN$ TIMEOUT-MS RUN-ARGS
   NULL$ STDOUT
   FOREIGN$ s" /lib/fs.f" CANNOT-OPEN ;

\ The root's lib and src are symlinks into the gate tree; branch is a symlink
\ elsewhere, so branch/../lib/errors.f only reads like the root's file.
: ALIAS ( -- )
   s" the root's files keep the engine's identity through its symlinks" T-LABEL
   PROG-RESET
   s" require lib/errors.f" PLINE
   s" require src/core/util.f" PLINE
   s" require branch/../lib/errors.f" PLINE
   s" package SOURCE-ROOT-EXE-ALIAS" PLINE
   YN-LINE
   s" : RUN ( -- )" PLINE
   S\"    s\q lib/errors.f\q ENGINE-PROVIDES? YN" PLINE
   S\"    s\q src/core/util.f\q ENGINE-PROVIDES? YN" PLINE
   S\"    s\q branch/../lib/errors.f\q ENGINE-PROVIDES? YN" PLINE
   s"    SOURCE-ROOT-EXE-OTHER:MARK 3 = YN ;" PLINE
   s" RUN" PLINE
   s" ;package" PLINE
   RUN-TREE
   S\" yes\nyes\nno\nyes\n" 0 EXPECT ;

\ The root's own files reached through another root are still the engine's: a
\ second load of one would exit 78 on its first definition.
: FIX-1 ( -- )
   s" the root's file required through its parent directory is the engine's" T-LABEL
   PROC-ARGV-RESET
   s" b/bin/hb" HB-AT$ TOP$ S\" SOURCE-ROOT:ENGINE$ type cr\nrequire b/lib/errors.f\n"
   TIMEOUT-MS RUN-EXE
   SB-RESET s" b" A$ LINE SB$ 0 EXPECT
   s" the root's files reached through a symlinked working directory are the engine's"
   T-LABEL
   PROC-ARGV-RESET
   s" tree/bin/hb" HB-AT$ s" wd" A$ S\" require lib/string.f\nrequire lib/fs.f\n"
   TIMEOUT-MS RUN-EXE
   NULL$ 0 EXPECT ;

\ The working directory's lib/errors.f is provided by its absolute path, so a
\ require of lib/errors.f stops there, at the application's fact.
: FIX-2 ( -- )
   s" an application fact steers ENGINE-PROVIDES? and leaves the resolved root" T-LABEL
   PROG-RESET
   s" package SOURCE-ROOT-EXE-FACT" PLINE
   YN-LINE
   s" : RUN ( -- )" PLINE
   S\"    SOURCE-ROOT:CWD$ s\q lib/errors.f\q SOURCE-ROOT:JOIN provided" PLINE
   S\"    s\q lib/fs.f\q SOURCE-ROOT:RESOLVE drop 2drop" PLINE
   S\"    s\q lib/errors.f\q ENGINE-PROVIDES? YN" PLINE
   s"    SOURCE-ROOT:RESOLVED-ROOT$ type cr ;" PLINE
   s" RUN" PLINE
   s" ;package" PLINE
   RUN-TREE
   SB-RESET s" no" LINE s" tree" A$ LINE SB$ 0 EXPECT ;

: BOOT-OUTSIDE ( -- )
   s" a boot file outside the root is refused by name" T-LABEL
   PROG-RESET
   s" package SOURCE-ROOT-EXE-BOOT" PLINE
   s" : RUN ( -- )" PLINE
   s"    REQUIRE-BOOT-OPEN" PLINE
   S\"    SOURCE-ROOT:CWD$ s\q x.f\q SOURCE-ROOT:JOIN required ;" PLINE
   s" RUN" PLINE
   s" ;package" PLINE
   RUN-TREE
   SB-RESET s" source root: a boot file is outside the engine root: " SB-APPEND
   s" foreign/x.f" A$ LINE SB$ REFUSAL ;

\ ---- CD, PUSHPATH, POPPATH -------------------------------------------------------

: CD-REFUSED ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: label:ptr labelu:n rel:ptr relu:n want:ptr wantu:n :}
   label labelu T-LABEL
   PROG-RESET s" SOURCE-ROOT:CD" rel relu PATH-LINE
   RUN-TREE
   SB-RESET want wantu SB-APPEND rel relu A$ LINE SB$ REFUSAL ;

: CD-SEARCH-ONLY ( -- )
   s" a directory searchable without read permission is a current path" T-LABEL
   s" search-only" A$ SEARCH-ONLY CHMOD-MODE
   PROG-RESET s" SOURCE-ROOT:CD" s" search-only" PATH-LINE s" SOURCE-ROOT:CD" PLINE
   RUN-TREE
   s" search-only" A$ FS-MUT-MODE-DIR CHMOD-MODE
   SB-RESET s" search-only" A$ LINE SB$ 0 EXPECT ;

\ Each step is one prompt line, so the path grows past the cap only by joining.
: CD-TOO-LONG ( -- )
   s" a current path longer than a loader path is refused by name" T-LABEL
   PROG-RESET s" SOURCE-ROOT:CD" s" deep" PATH-LINE
   5 0 do SB-RESET s" SOURCE-ROOT:CD " SB-APPEND LONG LONG-NAME SB-APPEND SB$ PLINE loop
   RUN-TREE
   S\" CD: path is too long\n" REFUSAL ;

\ A path word in <tmp>/<file>, the --load entry.
: IN-LOAD ( ptr u8 n ptr u8 n -- )
   {: file:ptr fileu:n want:ptr wantu:n :}
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   file fileu A$ >LEN PROC-ARGV+
   s" tree/bin/hb" HB-AT$ FOREIGN$ TIMEOUT-MS RUN-ARGS
   want wantu REFUSAL ;

\ A load holds the path it replaced, so none of the words may move it.
: PATH-IN-LOAD ( -- )
   s" the path words are refused inside a loaded file" T-LABEL
   s" cd-load.f" S\" CD: only at top level\n" IN-LOAD
   s" push-load.f" S\" PUSHPATH: only at top level\n" IN-LOAD
   s" pop-load.f" S\" POPPATH: only at top level\n" IN-LOAD ;

\ A null slot is the working directory, which the last POPPATH restores. The
\ round trip spells the words bare under `using SOURCE-ROOT`.
: PATH-STACK ( -- )
   s" PUSHPATH and POPPATH save and restore the current path" T-LABEL
   PROG-RESET
   s" using SOURCE-ROOT" PLINE
   s" PUSHPATH" PLINE
   s" CD" s" cd" PATH-LINE
   s" PUSHPATH" PLINE
   s" CD sub" PLINE
   s" CD" PLINE
   s" POPPATH" PLINE
   s" CD" PLINE
   s" POPPATH" PLINE
   s" CD" PLINE
   s" ;using" PLINE
   RUN-TREE
   SB-RESET s" cd/sub" A$ LINE s" cd" A$ LINE FOREIGN$ LINE SB$ 0 EXPECT
   s" POPPATH on an empty stack is refused by name" T-LABEL
   PROG-RESET s" SOURCE-ROOT:POPPATH" PLINE
   RUN-TREE
   S\" POPPATH: directory stack is empty\n" REFUSAL
   s" PUSHPATH on a full stack is refused by name" T-LABEL
   PROG-RESET OVER-DEPTH 0 do s" SOURCE-ROOT:PUSHPATH" PLINE loop
   RUN-TREE
   S\" PUSHPATH: directory stack is full\n" REFUSAL ;

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
   s" CD to a missing directory is refused by name" s" absent"
   s" CD: does not exist: " CD-REFUSED
   s" CD to a file is refused by name" s" cd/x.f"
   s" CD: is not a searchable directory: " CD-REFUSED
   CD-SEARCH-ONLY CD-TOO-LONG PATH-IN-LOAD PATH-STACK ;

\ ---- a captured image ------------------------------------------------------------

\ A restored image starts at the working directory, so a save with a path
\ pushed is refused by name and writes nothing.
: SAVE-PUSHED ( -- )
   s" an image is not saved with a path pushed" T-LABEL
   PROC-ARGV-RESET
   s" --" >LEN PROC-ARGV+
   s" pushed" A$ >LEN PROC-ARGV+
   PROG-RESET
   s" require src/habu/app-image.f" PLINE
   s" SOURCE-ROOT:PUSHPATH" PLINE
   s" 0 SCRIPT-ARGV$ APP-IMAGE:SAVE" PLINE
   s" tree/bin/hb" HB-AT$ FOREIGN$ PROG$ SAVE-TIMEOUT-MS RUN-EXE
   S\" PUSHPATH: an image cannot be saved with a path pushed\n" REFUSAL
   s" the refused save writes no image" T-LABEL
   s" pushed" A$ EXISTS? TFALSE ;

\ The image is saved by an engine whose tree no loaded file's canonical path
\ contains (its lib and src are links), so the tree's bytes can only come from
\ the loader's own state. The restored image reads its whole DATA for them,
\ with the canonical path of a file the save loaded as the proof that the read
\ sees what was captured.
: SCAN-PROGRAM ( -- )
   PROG-RESET
   s" : SRE-LEN ( -- n ) here data-base - ;" PLINE
   s" : SRE-HAS ( n -- ) SCRIPT-ARGV$ {: a:ptr u:n :}" PLINE
   S\"    data-base BYTE-VIEW SRE-LEN a u CONTAINS? if s\q present\q else s\q absent\q then type cr ;" PLINE
   s" 0 SRE-HAS 1 SRE-HAS" PLINE ;

: IMAGE$ ( -- ptr u8 n ) s" moved/bin/app" HB-AT$ ;

: CAPTURE ( -- )
   s" an engine outside any tree saves an image after recording events" T-LABEL
   PROC-ARGV-RESET
   s" --" >LEN PROC-ARGV+
   IMAGE$ >LEN PROC-ARGV+
   PROG-RESET
   \ A saved image retains native code only (src/habu/snap-lib.f PERSIST), so
   \ the library compiles at the optimizing tier, as src/habu/app-image.f does.
   s" 1 set-tier" PLINE
   s" EVENT-ON" PLINE
   s" require lib/fs.f" PLINE
   s" require src/habu/app-image.f" PLINE
   s" 0 SCRIPT-ARGV$ APP-IMAGE:SAVE" PLINE
   s" tree/bin/hb" HB-AT$ FOREIGN$ PROG$ SAVE-TIMEOUT-MS RUN-EXE
   RC @ 0= 0= if SHOW then
   RC @ 0 T=
   IMAGE$ EXECUTABLE? {: saved:bool :}
   saved TTRUE
   saved 0= if exit then
   s" the restored image holds neither its engine's tree nor that root" T-LABEL
   PROC-ARGV-RESET
   s" --" >LEN PROC-ARGV+
   s" tree" A$ >LEN PROC-ARGV+
   s" src/habu/app-image.f" GATE-B$ >LEN PROC-ARGV+
   SCAN-PROGRAM
   IMAGE$ FOREIGN$ PROG$ TIMEOUT-MS RUN-EXE
   S\" absent\npresent\n" 0 EXPECT
   s" the restored image takes the tree it was placed in" T-LABEL
   PROC-ARGV-RESET
   IMAGE$ FOREIGN$ S\" SOURCE-ROOT:ENGINE$ type cr\n" TIMEOUT-MS RUN-EXE
   SB-RESET s" moved" A$ LINE SB$ 0 EXPECT ;

\ ---- a build ---------------------------------------------------------------------

\ The working directory holds only the tree's tools, so the host compiles from
\ its own tree and the build records its rows under the working directory.
: BUILD-OUTSIDE ( -- )
   s" a native build outside a tree dies at its first row" T-LABEL
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   s" tools/native-build.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   s" hb-foreign" A$ >LEN PROC-ARGV+
   s" tree/bin/hb" HB-AT$ s" cwd" A$ BUILD-TIMEOUT-MS RUN-ARGS
   s" cwd" A$ s" /src/core/" CANNOT-OPEN
   s" the failed build writes no engine" T-LABEL
   s" hb-foreign" A$ EXISTS? TFALSE ;

: MAIN ( -- )
   T-RESET
   PREP
   EXE-TREE CWD-TREE LONE PARTIAL NOT-TREE
   APP WD ENTRY ALIAS FIX-1 FIX-2 BOOT-OUTSIDE
   PATH-WORDS SAVE-PUSHED CAPTURE BUILD-OUTSIDE
   CLEANUP-RUN
   T-REPORT
   s" source-root-exe-test: ok" type cr ;

MAIN

;package
;using
