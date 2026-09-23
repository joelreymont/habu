\ The cleanup registry runs at process exit: one child process per exit path.
\ Every case registers a directory inside this fixture's own temp root and then
\ leaves the process the way it means to - normally, by `die`, by an uncaught
\ throw, by dying inside a chained foreign hook, or by returning from or dying
\ in a stripped image's MAIN - and the assertion is what the child left on
\ disk. The children
\ take the directory as argument 0 rather than making their own under HB_TMP, so
\ a case that fails leaves its evidence inside the root this fixture removes.
\ A child script resolves `require lib/fs-mutate.f` against the working
\ directory, which is the checkout (test/app-image.f spawns the same way).
require lib/test.f
require lib/errors.f
require lib/string.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/engine-candidate.f

package EXIT-HOOK-TEST

$10000 constant CAP
1024 constant TEXT-CAP
600000 constant TIMEOUT-MS
create OUT CAP allot
create ERR CAP allot
create TEXT-BUF TEXT-CAP allot
create ROOT-BUF FS-PATH-CAP allot
create SCRIPT-BUF FS-PATH-CAP allot
create TREE-BUF FS-PATH-CAP allot
create SUBJECT-BUF FS-PATH-CAP allot
create IMAGE-BUF FS-PATH-CAP allot
variable TEXT-U
variable ROOT-U
variable SCRIPT-U
variable TREE-U
variable SUBJECT-U
variable IMAGE-U

: ROOT$ ( -- ptr u8 n ) ROOT-BUF ROOT-U @ ;
: SCRIPT$ ( -- ptr u8 n ) SCRIPT-BUF SCRIPT-U @ ;
: TREE$ ( -- ptr u8 n ) TREE-BUF TREE-U @ ;
: SUBJECT$ ( -- ptr u8 n ) SUBJECT-BUF SUBJECT-U @ ;
: IMAGE$ ( -- ptr u8 n ) IMAGE-BUF IMAGE-U @ ;
: TEXT$ ( -- ptr u8 n ) TEXT-BUF TEXT-U @ ;

\ Child sources are built a line at a time: one 255-byte source line of this
\ file cannot hold a whole script, and the pieces read as the program they are.
: TEXT+ ( ptr u8 n -- ) {: src:ptr srcu :}
   TEXT-U @ srcu + TEXT-CAP > if E-FS-CAPACITY throw then
   src TEXT-BUF TEXT-U @ + srcu BYTE-COPY
   TEXT-U @ srcu + TEXT-U ! ;

: PREPARE ( -- )
   CLEANUP-RESET
   s" exit-hook" TMPDIR-MKDIR {: path:ptr pathu :}
   path ROOT-BUF pathu BYTE-COPY pathu ROOT-U !
   ROOT$ CLEANUP-TREE+
   SOURCE-ROOT:CURRENT$ s" exit-hook-subject.f" SUBJECT-BUF JOIN-PATH SUBJECT-U !
   ROOT$ s" application" IMAGE-BUF JOIN-PATH IMAGE-U ! ;

: TREE-PATH ( ptr u8 n -- ) {: name:ptr nameu :}
   ROOT$ name nameu TREE-BUF JOIN-PATH TREE-U ! ;

\ The script path, the directory the child is told to register, and the two
\ lines every child script starts with.
: CASE-PATHS ( ptr u8 n ptr u8 n -- ) {: script:ptr scriptu tree:ptr treeu :}
   ROOT$ script scriptu SCRIPT-BUF JOIN-PATH SCRIPT-U !
   tree treeu TREE-PATH
   0 TEXT-U !
   S\" require lib/fs-mutate.f\n" TEXT+
   S\" : CHILD-TREE ( -- ptr u8 n ) 0 SCRIPT-ARGV$ ;\n" TEXT+ ;

: SHOW-FAILURE ( n n n -- ) {: outu:n erru:n rc:n :}
   s" exit-hook child rc " type rc . cr
   OUT outu type ERR erru type ;

: EXPECT-RC ( n n n n -- ) {: outu:n erru:n rc:n want:n :}
   rc want = 0= if outu erru rc SHOW-FAILURE then
   rc want T= ;

: ERR-HAS? ( n ptr u8 n -- bool ) {: erru:n want:ptr wantu :}
   ERR erru want wantu CONTAINS? ;

\ The child runs on the engine under test with the case's directory as its one
\ application argument, and with this process's environment, so HB_TMP and the
\ working directory are the ones the suite itself runs with. Capture gives the
\ child no stdin, so the script's own exit is what the case measures.
: RUN-CHILD ( -- n n n )
   SCRIPT$ TEXT$ ATOMIC-WRITE-FILE
   PROC-ARGV-ENV-RESET
   s" --load" >LEN PROC-ARGV+
   SCRIPT$ >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   TREE$ >LEN PROC-ARGV+
   PROC-ENV-INHERIT-MISSING
   ENGINE-CANDIDATE:PATH$ >LEN
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-ENV-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N
   {: outu:len erru:len rc:n :}
   outu LEN>N erru LEN>N rc ;

\ A normal top-level exit (the engine's LRBYE) reaches the vector.
: CASE-NORMAL ( -- )
   s" normal.f" s" normal-tree" CASE-PATHS
   S\" : CHILD-MAIN ( -- ) CHILD-TREE MAKE-DIR CHILD-TREE CLEANUP-TREE+ ;\n" TEXT+
   S\" CHILD-MAIN\n" TEXT+
   RUN-CHILD {: outu:n erru:n rc:n :}
   s" a normal exit removes the registered tree" T-LABEL
   outu erru rc 0 EXPECT-RC
   erru 0 T=
   TREE$ EXISTS? TFALSE ;

\ `die` writes its message and exits with its own code; the hook runs between
\ the two and the code it is carrying is untouched.
: CASE-DIE ( -- )
   s" die.f" s" die-tree" CASE-PATHS
   S\" : CHILD-MAIN ( -- ) CHILD-TREE MAKE-DIR CHILD-TREE CLEANUP-TREE+ s\q child: dying\q 7 die ;\n" TEXT+
   S\" CHILD-MAIN\n" TEXT+
   RUN-CHILD {: outu:n erru:n rc:n :}
   s" die removes the registered tree and keeps its exit code" T-LABEL
   outu erru rc 7 EXPECT-RC
   erru s" child: dying" ERR-HAS? TTRUE
   TREE$ EXISTS? TFALSE ;

\ The reported defect: an uncaught top-level throw. 1 is kernel-representable,
\ so the engine exits with the thrown code itself.
: CASE-THROW ( -- )
   s" throw.f" s" throw-tree" CASE-PATHS
   S\" : CHILD-MAIN ( -- ) CHILD-TREE MAKE-DIR CHILD-TREE CLEANUP-TREE+ 1 throw ;\n" TEXT+
   S\" CHILD-MAIN\n" TEXT+
   RUN-CHILD {: outu:n erru:n rc:n :}
   s" an uncaught throw removes the registered tree" T-LABEL
   outu erru rc 1 EXPECT-RC
   TREE$ EXISTS? TFALSE ;

\ A removal that fails is reported on fd 2 and changes nothing else. CLEANUP-RUN
\ no-ops on a path that is already gone, so the failure has to come from a
\ removal the kernel refuses: a non-empty directory registered with CLEANUP-DIR+
\ is rmdir ENOTEMPTY, which fs.f names E-FS-IO (-2105).
: CASE-REPORT ( -- )
   s" report.f" s" report-tree" CASE-PATHS
   S\" create CHILD-BUF FS-PATH-CAP allot\n" TEXT+
   S\" : CHILD-INNER ( -- ) CHILD-TREE s\q inner\q CHILD-BUF JOIN-PATH {: u :} CHILD-BUF u MAKE-DIR ;\n" TEXT+
   S\" : CHILD-MAIN ( -- ) CHILD-TREE MAKE-DIR CHILD-INNER CHILD-TREE CLEANUP-DIR+ s\q child: dying\q 9 die ;\n" TEXT+
   S\" CHILD-MAIN\n" TEXT+
   RUN-CHILD {: outu:n erru:n rc:n :}
   s" a cleanup that throws is reported on fd 2 and keeps the exit code" T-LABEL
   outu erru rc 9 EXPECT-RC
   erru s" child: dying" ERR-HAS? TTRUE
   erru s" hb: cleanup at exit threw -2105" ERR-HAS? TTRUE
   TREE$ EXISTS? TTRUE ;

\ The chain. The child takes the process's single exit slot for a hook of its
\ own BEFORE it registers anything; registration saves that vector and installs
\ the registry's, and the registry calls it back once its own paths are gone.
\ So the tree goes although the child never asked the registry to run, and the
\ process exits 3, the chained hook's code, not 5: `die` writes its message and
\ calls the vector, and the hook's own `die` exits from inside that call before
\ the outer exit_group is reached. Both vectors are cleared before they are
\ called, so neither `die` re-enters a hook.
: CASE-CHAIN ( -- )
   s" chain.f" s" chain-tree" CASE-PATHS
   S\" : CHILD-HOOK ( -- ) s\q hook: dying\q 3 die ;\n" TEXT+
   S\" TRUSTED: CHILD-ARM ( -- ) ['] CHILD-HOOK data-base EXIT-HOOK-CELL + ! ;\n" TEXT+
   S\" : CHILD-MAIN ( -- ) CHILD-ARM CHILD-TREE MAKE-DIR CHILD-TREE CLEANUP-TREE+ s\q outer: dying\q 5 die ;\n" TEXT+
   S\" CHILD-MAIN\n" TEXT+
   RUN-CHILD {: outu:n erru:n rc:n :}
   s" registering chains a foreign hook instead of replacing it" T-LABEL
   outu erru rc 3 EXPECT-RC
   erru s" outer: dying" ERR-HAS? TTRUE
   erru s" hook: dying" ERR-HAS? TTRUE
   TREE$ EXISTS? TFALSE ;

: BUILD ( -- bool )
   PROC-ARGV-ENV-RESET
   s" --load" >LEN PROC-ARGV+
   s" tools/hb-build.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   SUBJECT$ >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   IMAGE$ >LEN PROC-ARGV+
   s" HABU_FIXPOINT_ENGINE" >LEN ENGINE-CANDIDATE:PATH$ >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   ENGINE-CANDIDATE:PATH$ >LEN
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-ENV-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N
   {: outu:len erru:len rc:n :}
   s" the exit-hook subject builds stripped" T-LABEL
   outu LEN>N erru LEN>N rc 0 EXPECT-RC
   IMAGE$ EXECUTABLE? TTRUE
   rc 0= IMAGE$ EXECUTABLE? and ;

\ The built image runs with the arguments already queued: the tree to register
\ first, then whatever tells the subject how to leave.
: RUN-IMAGE ( -- n n n )
   PROC-ENV-INHERIT-MISSING
   IMAGE$ >LEN
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-ENV-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N
   {: outu:len erru:len rc:n :}
   outu LEN>N erru LEN>N rc ;

\ A stripped image has no engine label and no REPL: its entry calls the vector
\ inline on the way to its own exit(0).
: CASE-STRIPPED ( -- )
   s" stripped-tree" TREE-PATH
   PROC-ARGV-ENV-RESET
   TREE$ >LEN PROC-ARGV+
   RUN-IMAGE {: outu:n erru:n rc:n :}
   s" a stripped application's own exit removes the registered tree" T-LABEL
   outu erru rc 0 EXPECT-RC
   erru 0 T=
   OUT outu S\" exit-hook-subject: ok\n" T$=
   TREE$ EXISTS? TFALSE ;

\ `die` inside a stripped image: BDIE reaches the leaf by a direct BL that the
\ linker can only relocate through the (LEXITHOOK) engine-helper record, and
\ that is the exit a daemon takes. The second argument makes the subject die.
: CASE-STRIPPED-DIE ( -- )
   s" stripped-die-tree" TREE-PATH
   PROC-ARGV-ENV-RESET
   TREE$ >LEN PROC-ARGV+
   s" die" >LEN PROC-ARGV+
   RUN-IMAGE {: outu:n erru:n rc:n :}
   s" a stripped application's die removes the registered tree" T-LABEL
   outu erru rc 7 EXPECT-RC
   erru s" subject: dying" ERR-HAS? TTRUE
   OUT outu S\" exit-hook-subject: ok\n" T$=
   TREE$ EXISTS? TFALSE ;

: BODY ( -- )
   PREPARE
   CASE-NORMAL
   CASE-DIE
   CASE-THROW
   CASE-REPORT
   CASE-CHAIN
   BUILD if CASE-STRIPPED CASE-STRIPPED-DIE then ;

: RUN ( -- )
   T-RESET
   [: BODY ;] [: CLEANUP-RUN ;] finally
   T-REPORT
   s" exit-hook-test: ok" type cr ;

RUN
;package
