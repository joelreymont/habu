\ Process exit and the build boundary, one child per case: the exit actions
\ (src/lib/at-exit.f) and the cleanup registry on each exit path, and what a
\ built program keeps from its build, a queue its build defined included. A
\ child registers inside this fixture's temp root, so a failing case leaves its
\ evidence where the fixture removes it; it resolves
\ `require src/lib/fs-mutate.f` against the checkout (cwd).
\ A child is judged by what it left on disk or its exit code, a built program
\ by what it printed or removed.
require src/lib/test.f
require src/lib/error.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/engine-candidate.f

package EXIT-HOOK-TEST

$10000 constant CAP
1024 constant TEXT-CAP
600000 constant TIMEOUT-MS
create OUT CAP allot
create ERR CAP allot
create TEXT-BUF TEXT-CAP allot
create ROOT-BUF PATH-CAP allot
create SCRIPT-BUF PATH-CAP allot
create TREE-BUF PATH-CAP allot
variable TEXT-U
variable ROOT-U
variable SCRIPT-U
variable TREE-U

: ROOT$ ( -- ptr u8 n ) ROOT-BUF ROOT-U @ ;
: SCRIPT$ ( -- ptr u8 n ) SCRIPT-BUF SCRIPT-U @ ;
: TREE$ ( -- ptr u8 n ) TREE-BUF TREE-U @ ;
: TEXT$ ( -- ptr u8 n ) TEXT-BUF TEXT-U @ ;

\ Child sources are built a line at a time: one 255-byte source line of this
\ file cannot hold a whole script, and the pieces read as the program they are.
: TEXT+ ( ptr u8 n -- ) {: src:ptr srcu :}
   TEXT-U @ srcu + TEXT-CAP > if E-FS-CAPACITY throw then
   src TEXT-BUF TEXT-U @ + srcu BYTE-COPY
   TEXT-U @ srcu + TEXT-U ! ;

: PREPARE ( -- )
   FS:CLEANUP-RESET
   s" exit-hook" FS:MKTEMP-DIR {: path:ptr pathu :}
   path ROOT-BUF pathu BYTE-COPY pathu ROOT-U !
   ROOT$ FS:CLEANUP-TREE+ ;

: TREE-PATH ( ptr u8 n -- ) {: name:ptr nameu :}
   ROOT$ name nameu TREE-BUF FS:JOIN-PATH TREE-U ! ;

\ The script path, the directory the child is told to register, and the two
\ lines every child script starts with.
: CASE-PATHS ( ptr u8 n ptr u8 n -- ) {: script:ptr scriptu tree:ptr treeu :}
   ROOT$ script scriptu SCRIPT-BUF FS:JOIN-PATH SCRIPT-U !
   tree treeu TREE-PATH
   0 TEXT-U !
   S\" require src/lib/fs-mutate.f\n" TEXT+
   S\" : CHILD-TREE ( -- ptr u8 n ) 0 SCRIPT-ARGV$ ;\n" TEXT+ ;

: SHOW-FAILURE ( n n n -- ) {: outu:n erru:n rc:n :}
   s" exit-hook child rc " type rc . cr
   OUT outu type ERR erru type ;

: EXPECT-RC ( n n n n -- ) {: outu:n erru:n rc:n want:n :}
   rc want = 0= if outu erru rc SHOW-FAILURE then
   rc want T= ;

: ERR-HAS? ( n ptr u8 n -- bool ) {: erru:n want:ptr wantu :}
   ERR erru want wantu CONTAINS? ;

\ The child runs the program at the path with the arguments put so far, and
\ with this process's environment, so TMPDIR and the working directory are the
\ ones the suite itself runs with. Capture gives the child no stdin, so the
\ program's own exit is what the case measures.
: RUN-PATH ( ptr u8 n -- n n n ) {: path:ptr pathu :}
   PROC-ENV-INHERIT-MISSING
   path pathu >LEN
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-ENV-CAPTURE-OUTCOME PROCESS:ENDING>RC RC>N
   {: outu:len erru:len rc:n :}
   outu LEN>N erru LEN>N rc ;

: RUN-ARGS ( -- n n n ) ENGINE-CANDIDATE:PATH$ RUN-PATH ;

\ The script, with the case's directory as its one application argument.
: RUN-CHILD ( -- n n n )
   SCRIPT$ TEXT$ FS:ATOMIC-WRITE-FILE
   PROC-ARGV-ENV-RESET
   s" --load" >LEN PROC-ARGV+
   SCRIPT$ >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   TREE$ >LEN PROC-ARGV+
   RUN-ARGS ;

\ hb-build with the script as its source and the case's path as its -o.
: RUN-BUILD ( -- n n n )
   SCRIPT$ TEXT$ FS:ATOMIC-WRITE-FILE
   PROC-ARGV-ENV-RESET
   s" --load" >LEN PROC-ARGV+
   s" tools/hb-build.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   SCRIPT$ >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   TREE$ >LEN PROC-ARGV+
   RUN-ARGS ;

\ The program RUN-BUILD wrote, with no arguments.
: RUN-PRODUCT ( -- n n n )
   PROC-ARGV-ENV-RESET
   TREE$ RUN-PATH ;

: OUT-HAS? ( n ptr u8 n -- bool ) {: outu:n want:ptr wantu :}
   OUT outu want wantu CONTAINS? ;

\ A normal top-level exit reaches the vector.
: CASE-NORMAL ( -- )
   s" normal.f" s" normal-tree" CASE-PATHS
   S\" : CHILD-MAIN ( -- ) CHILD-TREE FS:MAKE-DIR CHILD-TREE FS:CLEANUP-TREE+ ;\n" TEXT+
   S\" CHILD-MAIN\n" TEXT+
   RUN-CHILD {: outu:n erru:n rc:n :}
   s" a normal exit removes the registered tree" T-LABEL
   outu erru rc 0 EXPECT-RC
   erru 0 T=
   TREE$ FS:EXISTS? TFALSE ;

\ `die` writes its message and exits with its own code; the hook runs between
\ the two and the code it is carrying is untouched.
: CASE-DIE ( -- )
   s" die.f" s" die-tree" CASE-PATHS
   S\" : CHILD-MAIN ( -- ) CHILD-TREE FS:MAKE-DIR CHILD-TREE FS:CLEANUP-TREE+ s\q child: dying\q 7 die ;\n" TEXT+
   S\" CHILD-MAIN\n" TEXT+
   RUN-CHILD {: outu:n erru:n rc:n :}
   s" die removes the registered tree and keeps its exit code" T-LABEL
   outu erru rc 7 EXPECT-RC
   erru s" child: dying" ERR-HAS? TTRUE
   TREE$ FS:EXISTS? TFALSE ;

\ An uncaught top-level throw: 1 is kernel-representable, so the engine exits
\ with the thrown code itself.
: CASE-THROW ( -- )
   s" throw.f" s" throw-tree" CASE-PATHS
   S\" : CHILD-MAIN ( -- ) CHILD-TREE FS:MAKE-DIR CHILD-TREE FS:CLEANUP-TREE+ 1 throw ;\n" TEXT+
   S\" CHILD-MAIN\n" TEXT+
   RUN-CHILD {: outu:n erru:n rc:n :}
   s" an uncaught throw removes the registered tree" T-LABEL
   outu erru rc 1 EXPECT-RC
   TREE$ FS:EXISTS? TFALSE ;

\ A removal that fails is reported on fd 2 and changes nothing else. FS:CLEANUP-RUN
\ no-ops on a path that is already gone, so the failure has to come from a
\ removal the kernel refuses: a non-empty directory registered with FS:CLEANUP-DIR+
\ is rmdir ENOTEMPTY, which fs.f names E-FS-IO (-2105).
: CASE-REPORT ( -- )
   s" report.f" s" report-tree" CASE-PATHS
   S\" create CHILD-BUF PATH-CAP allot\n" TEXT+
   S\" : CHILD-INNER ( -- ) CHILD-TREE s\q inner\q CHILD-BUF FS:JOIN-PATH {: u :} CHILD-BUF u FS:MAKE-DIR ;\n" TEXT+
   S\" : CHILD-MAIN ( -- ) CHILD-TREE FS:MAKE-DIR CHILD-INNER CHILD-TREE FS:CLEANUP-DIR+ s\q child: dying\q 9 die ;\n" TEXT+
   S\" CHILD-MAIN\n" TEXT+
   RUN-CHILD {: outu:n erru:n rc:n :}
   s" a cleanup that throws is reported on fd 2 and keeps the exit code" T-LABEL
   outu erru rc 9 EXPECT-RC
   erru s" child: dying" ERR-HAS? TTRUE
   erru s" hb: cleanup at exit threw -2105" ERR-HAS? TTRUE
   TREE$ FS:EXISTS? TTRUE ;

\ Actions run newest first, each taken off before it runs. The child's action,
\ added after the registry's, dies inside `die`'s call of the vector, which runs
\ the older actions in turn, so the tree goes and the process exits 3, the
\ action's code, not 5: the action's `die` exits before the outer exit_group.
: CASE-CHAIN ( -- )
   s" chain.f" s" chain-tree" CASE-PATHS
   S\" require src/lib/at-exit.f\n" TEXT+
   S\" : CHILD-ACTION ( -- ) s\q action: dying\q 3 die ;\n" TEXT+
   S\" : CHILD-MAIN ( -- ) CHILD-TREE FS:MAKE-DIR CHILD-TREE FS:CLEANUP-TREE+\n" TEXT+
   S\"    [: CHILD-ACTION ;] AT-EXIT:ADD s\q outer: dying\q 5 die ;\n" TEXT+
   S\" CHILD-MAIN\n" TEXT+
   RUN-CHILD {: outu:n erru:n rc:n :}
   s" an action that dies leaves the older actions to run" T-LABEL
   outu erru rc 3 EXPECT-RC
   erru s" outer: dying" ERR-HAS? TTRUE
   erru s" action: dying" ERR-HAS? TTRUE
   TREE$ FS:EXISTS? TFALSE ;

\ An action that throws, added while `die` runs inside a `catch`. The engine
\ runs the vector with the handler chain empty, so the throw is uncaught and
\ ends the process with the action's code, as CASE-CHAIN's action ends it with
\ its own; it never resumes the program's catch, which would return from `die`
\ and print the caught code and `after` on stdout, rc 0.
: CASE-HOOK-THROW ( -- )
   s" hook-throw.f" s" hook-throw-tree" CASE-PATHS
   S\" require src/lib/at-exit.f\n" TEXT+
   S\" : CHILD-ACTION ( -- ) 9 throw ;\n" TEXT+
   S\" : CHILD-ADD ( -- ) [: CHILD-ACTION ;] AT-EXIT:ADD ;\n" TEXT+
   S\" CHILD-ADD\n" TEXT+
   S\" : CHILD-DIE ( n -- ) 0< if s\q neg\q 3 die then ;\n" TEXT+
   S\" : CHILD-MAIN ( -- n ) [: -1 CHILD-DIE ;] catch ;\n" TEXT+
   S\" CHILD-MAIN . cr\n" TEXT+
   S\" s\q after\q type cr\n" TEXT+
   RUN-CHILD {: outu:n erru:n rc:n :}
   s" an action that throws ends the process instead of returning from die" T-LABEL
   outu erru rc 9 EXPECT-RC
   erru s" neg" ERR-HAS? TTRUE
   outu 0 T= ;

\ FS:REPLACE-STAGED registers the sibling it reserves, so a `die` inside the
\ filler - the image writer's on a failed open or write - takes the sibling
\ with it, and the file it was replacing stays as it was: one file, its old
\ bytes.
16 constant STAGED-CAP
create STAGED-BUF STAGED-CAP allot
create STAGED-OUT-BUF PATH-CAP allot
variable STAGED-OUT-U
variable STAGED-FILES

: STAGED-OUT$ ( -- ptr u8 n ) STAGED-OUT-BUF STAGED-OUT-U @ ;

: STAGED-COUNT ( ptr u8 n -- )
   2drop STAGED-FILES @ 1+ STAGED-FILES ! ;

: STAGED-FILES# ( -- n )
   0 STAGED-FILES !
   TREE$ [: STAGED-COUNT ;] FS:WALK-FILES
   STAGED-FILES @ ;

: CASE-STAGED ( -- )
   s" staged.f" s" staged-tree" CASE-PATHS
   S\" create CHILD-BUF PATH-CAP allot\nvariable CHILD-U\n" TEXT+
   S\" : CHILD-OUT ( -- ptr u8 n ) CHILD-BUF CHILD-U @ ;\n" TEXT+
   S\" : CHILD-OUT! ( -- ) CHILD-TREE s\q out\q CHILD-BUF FS:JOIN-PATH CHILD-U ! ;\n" TEXT+
   S\" : CHILD-FILL ( ptr u8 n -- ) 2drop s\q child: dying\q 7 die ;\n" TEXT+
   S\" : CHILD-MAIN ( -- ) CHILD-TREE FS:MAKE-DIR CHILD-OUT! CHILD-OUT s\q previous\q FS:WRITE-ALL\n" TEXT+
   S\"    CHILD-OUT [: CHILD-FILL ;] FS:REPLACE-STAGED ;\n" TEXT+
   S\" CHILD-MAIN\n" TEXT+
   RUN-CHILD {: outu:n erru:n rc:n :}
   s" a die while a file is replaced keeps it and removes the sibling" T-LABEL
   outu erru rc 7 EXPECT-RC
   erru s" child: dying" ERR-HAS? TTRUE
   TREE$ s" out" STAGED-OUT-BUF FS:JOIN-PATH STAGED-OUT-U !
   STAGED-OUT$ STAGED-BUF STAGED-CAP FS:READ-ALL {: u:n :}
   STAGED-BUF u s" previous" T$=
   STAGED-FILES# 1 T= ;

\ hb-build loads its source in its own process, so a source whose top level
\ ends the process ends hb-build inside the build: the build fails 74 naming
\ the source, and -o keeps its bytes.
: CASE-BUILD ( -- )
   s" build.f" s" build-out" CASE-PATHS
   0 TEXT-U !
   S\" s\q \q 0 die\n" TEXT+
   TREE$ s" previous" FS:WRITE-ALL
   RUN-BUILD {: outu:n erru:n rc:n :}
   s" a build whose source exits fails and keeps -o" T-LABEL
   outu erru rc 74 EXPECT-RC
   erru SCRIPT$ ERR-HAS? TTRUE
   erru s" ended the process during the build" ERR-HAS? TTRUE
   TREE$ STAGED-BUF STAGED-CAP FS:READ-ALL {: u:n :}
   STAGED-BUF u s" previous" T$= ;

create RUN-FILE-BUF PATH-CAP allot
variable RUN-FILE-U

: RUN-FILE$ ( -- ptr u8 n ) RUN-FILE-BUF RUN-FILE-U @ ;

\ It builds and runs a product because the build boundary is under test: a
\ built program starts with none of the exit actions and cleanups its source's
\ top level registered, and registering at run time works from empty; it inits
\ a queue its build defined, whose record address the build must carry.
: CASE-BUILT ( -- )
   s" built.f" s" built-out" CASE-PATHS
   ROOT$ s" built-run" RUN-FILE-BUF FS:JOIN-PATH RUN-FILE-U !
   0 TEXT-U !
   S\" require src/lib/fs-mutate.f\nrequire src/lib/queue.f\n" TEXT+
   S\" $10 QUEUE:QUEUE JOBS\n" TEXT+
   S\" : BUILD-ACTION ( -- ) s\q build-time exit action ran\q type cr ;\n" TEXT+
   S\" : RUN-ACTION ( -- ) s\q run-time exit action ran\q type cr ;\n" TEXT+
   S\" : BUILT-FILE ( -- ptr u8 n ) s\q " TEXT+  TREE$ TEXT+  S\" .built\q ;\n" TEXT+
   S\" : RUN-FILE ( -- ptr u8 n ) s\q " TEXT+  RUN-FILE$ TEXT+  S\" \q ;\n" TEXT+
   S\" ' BUILD-ACTION AT-EXIT:ADD\n" TEXT+
   S\" BUILT-FILE s\q b\q FS:WRITE-ALL BUILT-FILE FS:CLEANUP+\n" TEXT+
   S\" : MAIN ( -- ) ['] RUN-ACTION AT-EXIT:ADD\n" TEXT+
   S\"    RUN-FILE s\q r\q FS:WRITE-ALL RUN-FILE FS:CLEANUP+ JOBS QUEUE:INIT\n" TEXT+
   S\"    s\q main done\q type cr ;\n" TEXT+
   RUN-BUILD {: outu:n erru:n rc:n :}
   s" a built program starts with none of the build's exit actions and cleanups" T-LABEL
   outu erru rc 0 EXPECT-RC
   rc 0<> if exit then
   RUN-PRODUCT {: pu:n pe:n prc:n :}
   pu pe prc 0 EXPECT-RC
   pu s" main done" OUT-HAS? TTRUE
   pu s" run-time exit action ran" OUT-HAS? TTRUE
   pu s" build-time exit action ran" OUT-HAS? TFALSE
   RUN-FILE$ FS:EXISTS? TFALSE ;

\ hb-build dies after a failed build has unmapped its source's own AT-EXIT; it
\ starts a build because only a build closes the source's space before the exit.
: CASE-BUILD-ACTION ( -- )
   s" build-action.f" s" build-action-out" CASE-PATHS
   0 TEXT-U !
   S\" require src/lib/at-exit.f\n" TEXT+
   S\" : ACT ( -- ) ;  ' ACT AT-EXIT:ADD\n" TEXT+
   S\" NOSUCHWORD\n" TEXT+
   RUN-BUILD {: outu:n erru:n rc:n :}
   s" a failed build whose source added an exit action fails 74" T-LABEL
   outu erru rc 74 EXPECT-RC
   erru SCRIPT$ ERR-HAS? TTRUE
   erru s" habu-crash" ERR-HAS? TFALSE ;

\ The task and the two cleanups both build sources start with.
: CLEANUPS-TEXT ( -- )
   0 TEXT-U !
   S\" require src/lib/task.f\nTASK:MIN-STACK TASK:TASK CHILD-TASK\nvariable CHILD-MARK\n" TEXT+
   S\" : CHILD-FIRST ( -- ) CHILD-MARK @ 10 * 1 + CHILD-MARK ! ;\n" TEXT+
   S\" : CHILD-SECOND ( -- ) CHILD-MARK @ 10 * 2 + CHILD-MARK ! ;\n" TEXT+ ;

\ A build is the boundary under test: cleanups a source's top level registers
\ on a defined task hold rows in the table (src/lib/task.f TASK-EXIT-QT), which
\ the product carries; it prints 21, each cleanup run once, newest first.
: CASE-BUILD-CLEANUPS ( -- )
   s" cleanups.f" s" cleanups-out" CASE-PATHS
   CLEANUPS-TEXT
   S\" ' CHILD-FIRST CHILD-TASK TASK:AT-EXIT\n' CHILD-SECOND CHILD-TASK TASK:AT-EXIT\n" TEXT+
   S\" : MAIN ( -- ) [: 0 TASK:RETURN ;] CHILD-TASK TASK:ACTIVATE CHILD-TASK TASK:JOIN\n" TEXT+
   S\"    MATCH result ok OF drop ENDOF err OF . ENDOF ;MATCH CHILD-MARK @ . ;\n" TEXT+
   RUN-BUILD {: outu:n erru:n rc:n :}
   s" a build carries its task's cleanups into the product" T-LABEL
   outu erru rc 0 EXPECT-RC
   rc 0<> if exit then
   RUN-PRODUCT {: poutu:n perru:n prc:n :}
   s" the product's task runs each carried cleanup once, newest first" T-LABEL
   OUT poutu S\" 21\n" T$=
   poutu perru prc 0 EXPECT-RC ;

\ The 128 TASK:NEW tasks push the cleanups' rows past the table into a mapped
\ chunk, which no product can carry: the linker refuses the source by the
\ address cell EXIT-CHUNKS (src/lib/task.f) and the build fails 74.
: CASE-BUILD-CHUNK ( -- )
   s" cleanups-chunk.f" s" cleanups-chunk-out" CASE-PATHS
   CLEANUPS-TEXT
   S\" : CHILD-HOLD ( n -- ) {: k:n :} k 0= if ['] CHILD-FIRST CHILD-TASK TASK:AT-EXIT\n" TEXT+
   S\"    ['] CHILD-SECOND CHILD-TASK TASK:AT-EXIT exit then\n" TEXT+
   S\"    TASK:MIN-STACK TASK:NEW {: t:ptr :} [: ;] t TASK:AT-EXIT k 1 - RECURSE t TASK:FREE ;\n" TEXT+
   S\" 128 CHILD-HOLD\n: MAIN ( -- ) ;\n" TEXT+
   RUN-BUILD {: outu:n erru:n rc:n :}
   s" a source that leaves a cleanup chunk mapped is refused" T-LABEL
   outu erru rc 74 EXPECT-RC
   erru SCRIPT$ ERR-HAS? TTRUE
   erru s" in EXIT-CHUNKS +0 holds" ERR-HAS? TTRUE
   erru s" which an object cannot name" ERR-HAS? TTRUE ;

: BODY ( -- )
   PREPARE
   CASE-NORMAL
   CASE-DIE
   CASE-THROW
   CASE-REPORT
   CASE-CHAIN
   CASE-HOOK-THROW
   CASE-STAGED
   CASE-BUILD
   CASE-BUILT
   CASE-BUILD-ACTION
   CASE-BUILD-CLEANUPS
   CASE-BUILD-CHUNK ;

: RUN ( -- )
   T-RESET
   [: BODY ;] [: FS:CLEANUP-RUN ;] finally
   T-REPORT
   s" exit-hook-test: ok" type cr ;

RUN
;package
