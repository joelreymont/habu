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
require src/lib/fmt.f

package EXIT-HOOK-TEST

$10000 constant CAP
4096 constant TEXT-CAP
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

: ROOT$ ( -- ^u8 n ) ROOT-BUF ROOT-U @ ;
: SCRIPT$ ( -- ^u8 n ) SCRIPT-BUF SCRIPT-U @ ;
: TREE$ ( -- ^u8 n ) TREE-BUF TREE-U @ ;
: TEXT$ ( -- ^u8 n ) TEXT-BUF TEXT-U @ ;

\ Child sources are built a line at a time: one 255-byte source line of this
\ file cannot hold a whole script, and the pieces read as the program they are.
: TEXT+ ( ^u8 n -- ) {: src:^ srcu :}
   TEXT-U @ srcu + TEXT-CAP > if E-FS-CAPACITY throw then
   src TEXT-BUF TEXT-U @ + srcu BYTE-COPY
   TEXT-U @ srcu + TEXT-U ! ;

: PREPARE ( -- )
   FS:CLEANUP-RESET
   s" exit-hook" FS:MKTEMP-DIR {: path:^ pathu :}
   path ROOT-BUF pathu BYTE-COPY pathu ROOT-U !
   ROOT$ FS:CLEANUP-TREE+ ;

: TREE-PATH ( ^u8 n -- ) {: name:^ nameu :}
   ROOT$ name nameu TREE-BUF FS:JOIN-PATH TREE-U ! ;

\ The script path, the directory the child is told to register, and the two
\ lines every child script starts with.
: CASE-PATHS ( ^u8 n ^u8 n -- ) {: script:^ scriptu tree:^ treeu :}
   ROOT$ script scriptu SCRIPT-BUF FS:JOIN-PATH SCRIPT-U !
   tree treeu TREE-PATH
   0 TEXT-U !
   S\" require src/lib/fs-mutate.f\n" TEXT+
   S\" : CHILD-TREE ( -- ^u8 n ) 0 SCRIPT-ARGV$ ;\n" TEXT+ ;

: SHOW-FAILURE ( n n n -- ) {: outu:n erru:n rc:n :}
   s" exit-hook child rc " type rc . cr
   OUT outu type ERR erru type ;

: EXPECT-RC ( n n n n -- ) {: outu:n erru:n rc:n want:n :}
   rc want = 0= if outu erru rc SHOW-FAILURE then
   rc want T= ;

: ERR-HAS? ( n ^u8 n -- bool ) {: erru:n want:^ wantu :}
   ERR erru want wantu CONTAINS? ;

\ The child runs the program at the path with the arguments put so far, and
\ with this process's environment, so TMPDIR and the working directory are the
\ ones the suite itself runs with. Capture gives the child no stdin, so the
\ program's own exit is what the case measures.
: RUN-PATH ( ^u8 n -- n n n ) {: path:^ pathu :}
   PROC-ENV-INHERIT-MISSING
   path pathu >LEN
   OUT CAP >LEN ERR CAP >LEN PROCESS:WAIT-FOREVER
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

\ hb-build with the flag, if any, the script as its source and the case's path
\ as its -o.
: RUN-BUILD-AS ( ^u8 n -- n n n ) {: flag:^ flagu :}
   SCRIPT$ TEXT$ FS:ATOMIC-WRITE-FILE
   PROC-ARGV-ENV-RESET
   s" --load" >LEN PROC-ARGV+
   s" tools/hb-build.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   flagu 0<> if flag flagu >LEN PROC-ARGV+ then
   SCRIPT$ >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   TREE$ >LEN PROC-ARGV+
   RUN-ARGS ;

: RUN-BUILD ( -- n n n ) NULL$ RUN-BUILD-AS ;

\ The program RUN-BUILD wrote, with no arguments.
: RUN-TARGET ( -- n n n )
   PROC-ARGV-ENV-RESET
   TREE$ RUN-PATH ;

: OUT-HAS? ( n ^u8 n -- bool ) {: outu:n want:^ wantu :}
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

\ Action k counts itself into RAN; action 1, added first, runs last and prints
\ the count.
: ACTION-DEF ( n -- ) {: k:n :}
   s" : A" TEXT+ k FMT:INT$ TEXT+
   k 1 = if S\"  ( -- ) RAN @ . cr ;\n" TEXT+ exit then
   S\"  ( -- ) 1 RAN +! ;\n" TEXT+ ;

: ACTION-ADD ( n -- ) {: k:n :}
   s" ' A" TEXT+ k FMT:INT$ TEXT+ S\"  AT-EXIT:ADD\n" TEXT+ ;

\ A program's 70 distinct exit actions all run at its exit.
: CASE-ACTIONS-MANY ( -- )
   s" actions-many.f" s" actions-many-tree" CASE-PATHS
   S\" require src/lib/at-exit.f\nvariable RAN\n" TEXT+
   71 1 ?do i ACTION-DEF i ACTION-ADD loop
   RUN-CHILD {: outu:n erru:n rc:n :}
   s" a program's 70 exit actions all run" T-LABEL
   outu erru rc 0 EXPECT-RC
   outu s" 69" OUT-HAS? TTRUE ;

\ Reset k counts itself into RAN in the forked child, which exits with the
\ count.
: RESET-DEF ( n -- ) {: k:n :}
   s" : R" TEXT+ k FMT:INT$ TEXT+ S\"  ( -- ) 1 RAN +! ;  ' R" TEXT+
   k FMT:INT$ TEXT+ S\"  FORK-CHILD:REGISTER\n" TEXT+ ;

\ A build is the boundary under test: the fork resets a source's top level
\ registers are rows in its data, which the built program's forked child runs.
: CASE-RESETS-MANY ( -- )
   s" resets-many.f" s" resets-many-out" CASE-PATHS
   0 TEXT-U !
   S\" require src/lib/process.f\nrequire src/lib/process-fork.f\nvariable RAN\n" TEXT+
   21 1 ?do i RESET-DEF loop
   S\" : MAIN ( -- ) PROC-FORK:CHECKED {: pid:pid :}\n" TEXT+
   S\"    pid PID>N 0= if s\q \q RAN @ die then\n" TEXT+
   S\"    pid PROCESS:WAIT PROCESS:STATUS>RC RC>N s\q reaped \q type . cr ;\n" TEXT+
   RUN-BUILD {: outu:n erru:n rc:n :}
   s" a build carries its source's 20 fork resets into the target" T-LABEL
   outu erru rc 0 EXPECT-RC
   rc 0<> if exit then
   RUN-TARGET {: poutu:n perru:n prc:n :}
   s" a child the target forks runs each carried reset" T-LABEL
   poutu s" reaped 20" OUT-HAS? TTRUE
   poutu perru prc 0 EXPECT-RC ;

\ FS:REPLACE-STAGED registers the sibling it reserves, so a `die` inside the
\ filler - the image writer's on a failed open or write - takes the sibling
\ with it, and the file it was replacing stays as it was: one file, its old
\ bytes.
16 constant STAGED-CAP
create STAGED-BUF STAGED-CAP allot
create STAGED-OUT-BUF PATH-CAP allot
variable STAGED-OUT-U
variable STAGED-FILES

: STAGED-OUT$ ( -- ^u8 n ) STAGED-OUT-BUF STAGED-OUT-U @ ;

: STAGED-COUNT ( ^u8 n -- )
   2drop STAGED-FILES @ 1+ STAGED-FILES ! ;

: STAGED-FILES# ( -- n )
   0 STAGED-FILES !
   TREE$ [: STAGED-COUNT ;] FS:WALK-FILES
   STAGED-FILES @ ;

: CASE-STAGED ( -- )
   s" staged.f" s" staged-tree" CASE-PATHS
   S\" create CHILD-BUF PATH-CAP allot\nvariable CHILD-U\n" TEXT+
   S\" : CHILD-OUT ( -- ^u8 n ) CHILD-BUF CHILD-U @ ;\n" TEXT+
   S\" : CHILD-OUT! ( -- ) CHILD-TREE s\q out\q CHILD-BUF FS:JOIN-PATH CHILD-U ! ;\n" TEXT+
   S\" : CHILD-FILL ( ^u8 n -- ) 2drop s\q child: dying\q 7 die ;\n" TEXT+
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

\ An exit action a task alone adds runs at the main thread's exit: the action
\ table is process-wide, and hb starts with the main thread's vector armed.
: CASE-TASK-ADD ( -- )
   s" task-add.f" s" task-add-tree" CASE-PATHS
   S\" require src/lib/task.f\n$10000 TASK:TASK CHILD-TASK\n" TEXT+
   S\" : CHILD-WORK ( -- ) CHILD-TREE FS:CLEANUP-TREE+ 0 TASK:RETURN ;\n" TEXT+
   S\" : CHILD-MAIN ( -- ) CHILD-TREE FS:MAKE-DIR [: CHILD-WORK ;] CHILD-TASK TASK:ACTIVATE\n" TEXT+
   S\"    CHILD-TASK TASK:JOIN MATCH result ok OF drop ENDOF err OF . ENDOF ;MATCH ;\n" TEXT+
   S\" CHILD-MAIN\n" TEXT+
   RUN-CHILD {: outu:n erru:n rc:n :}
   s" an exit action a task alone adds runs at the main thread's exit" T-LABEL
   outu erru rc 0 EXPECT-RC
   outu 0 T=
   TREE$ FS:EXISTS? TFALSE ;

\ A task that never added an action dies: its exit runs the process's vector,
\ so the action the main thread added removes the tree.
: CASE-TASK-DIE ( -- )
   s" task-die.f" s" task-die-tree" CASE-PATHS
   S\" require src/lib/task.f\n$10000 TASK:TASK CHILD-TASK\n" TEXT+
   S\" : CHILD-WORK ( -- ) s\q task: dying\q 3 die ;\n" TEXT+
   S\" : CHILD-MAIN ( -- ) CHILD-TREE FS:MAKE-DIR CHILD-TREE FS:CLEANUP-TREE+\n" TEXT+
   S\"    [: CHILD-WORK ;] CHILD-TASK TASK:ACTIVATE\n" TEXT+
   S\"    CHILD-TASK TASK:JOIN MATCH result ok OF drop ENDOF err OF drop ENDOF ;MATCH ;\n" TEXT+
   S\" CHILD-MAIN\n" TEXT+
   RUN-CHILD {: outu:n erru:n rc:n :}
   s" a task's die runs the action the main thread added" T-LABEL
   outu erru rc 3 EXPECT-RC
   erru s" task: dying" ERR-HAS? TTRUE
   TREE$ FS:EXISTS? TFALSE ;

\ The main thread dies and its action runs; a task dies while the action
\ waits. The task's exit waits for the main thread's, so the action ends, its
\ line printed and the tree removed, and the process exits 5, main's code.
: CASE-TASK-WAIT ( -- )
   s" task-wait.f" s" task-wait-tree" CASE-PATHS
   S\" require src/lib/task.f\nrequire src/lib/at-exit.f\n$10000 TASK:TASK CHILD-TASK\n" TEXT+
   S\" here data-base - negate 7 and allot\nvariable STARTED\n" TEXT+
   S\" : ACT ( -- ) 1 STARTED atomic! 200 >MS TASK:SLEEP\n" TEXT+
   S\"    s\q action: done\q type cr CHILD-TREE FS:REMOVE-TREE ;\n" TEXT+
   S\" : WORK ( -- ) begin STARTED atomic@ 0= while 1 >MS TASK:SLEEP repeat\n" TEXT+
   S\"    s\q task: dying\q 3 die ;\n" TEXT+
   S\" : CHILD-MAIN ( -- ) CHILD-TREE FS:MAKE-DIR [: ACT ;] AT-EXIT:ADD\n" TEXT+
   S\"    [: WORK ;] CHILD-TASK TASK:ACTIVATE s\q main: dying\q 5 die ;\n" TEXT+
   S\" CHILD-MAIN\n" TEXT+
   RUN-CHILD {: outu:n erru:n rc:n :}
   s" a task's exit waits for the main thread's exit actions" T-LABEL
   outu erru rc 5 EXPECT-RC
   outu s" action: done" OUT-HAS? TTRUE
   TREE$ FS:EXISTS? TFALSE ;

\ The main thread's exit runs an action that blocks on a pipe until a task has
\ forked a child that dies 9 and reaped it. The child starts with no exit owner,
\ so it exits rather than waiting on the action's owner for good.
: CASE-FORK-OWNER ( -- )
   s" fork-owner.f" s" fork-owner-tree" CASE-PATHS
   S\" require src/lib/task.f\nrequire src/lib/at-exit.f\nrequire src/lib/process.f\n" TEXT+
   S\" require src/lib/process-fork.f\n$10000 TASK:TASK FORKER\n" TEXT+
   S\" variable GO-R variable GO-W variable REL-R variable REL-W\ncreate BYTE 8 allot\n" TEXT+
   S\" : PIPE-RC ( n -- ) 0<> if s\q pipe\q 2 die then ;\n" TEXT+
   S\" : PIPES ( -- ) pipe PIPE-RC GO-W ! GO-R ! pipe PIPE-RC REL-W ! REL-R ! ;\n" TEXT+
   S\" : ACT ( -- ) GO-W @ BYTE 1 write drop REL-R @ BYTE 1 read drop\n" TEXT+
   S\"    s\q action: done\q type cr ;\n" TEXT+
   S\" : WORK ( -- ) GO-R @ BYTE 1 read drop PROC-FORK:CHECKED {: pid:pid :}\n" TEXT+
   S\"    pid PID>N 0= if s\q \q 9 die then\n" TEXT+
   S\"    pid PROCESS:WAIT PROCESS:STATUS>RC RC>N s\q reaped \q type . cr\n" TEXT+
   S\"    REL-W @ BYTE 1 write drop 0 TASK:RETURN ;\n" TEXT+
   S\" : CHILD-MAIN ( -- ) PIPES [: ACT ;] AT-EXIT:ADD\n" TEXT+
   S\"    [: WORK ;] FORKER TASK:ACTIVATE s\q main: dying\q 5 die ;\n" TEXT+
   S\" CHILD-MAIN\n" TEXT+
   RUN-CHILD {: outu:n erru:n rc:n :}
   s" a child forked while another thread runs the exit actions exits" T-LABEL
   outu erru rc 5 EXPECT-RC
   outu s" reaped 9" OUT-HAS? TTRUE
   outu s" action: done" OUT-HAS? TTRUE ;

\ hb-build loads its source in its own process, so a source whose top level
\ ends the process ends hb-build inside the build: the build fails 70 naming
\ the source, and -o keeps its bytes.
: CASE-BUILD ( -- )
   s" build.f" s" build-out" CASE-PATHS
   0 TEXT-U !
   S\" s\q \q 0 die\n" TEXT+
   TREE$ s" previous" FS:WRITE-ALL
   RUN-BUILD {: outu:n erru:n rc:n :}
   s" a build whose source exits fails and keeps -o" T-LABEL
   outu erru rc 70 EXPECT-RC
   erru SCRIPT$ ERR-HAS? TTRUE
   erru s" ended the process during the build" ERR-HAS? TTRUE
   TREE$ STAGED-BUF STAGED-CAP FS:READ-ALL {: u:n :}
   STAGED-BUF u s" previous" T$= ;

\ The source's top level dies; its newer exit action dies too, and the older one still
\ runs, before hb-build's line.
: CASE-BUILD-EXIT ( -- )
   s" build-exit.f" s" build-exit-out" CASE-PATHS
   0 TEXT-U !
   S\" require src/lib/at-exit.f\n" TEXT+
   S\" : OLDER ( -- ) 2 s\q older \q write drop ;\n" TEXT+
   S\" : BYE ( -- ) s\q bye\q type cr s\q bye: dying\q 5 die ;\n" TEXT+
   S\" : ARM ( -- ) [: OLDER ;] AT-EXIT:ADD [: BYE ;] AT-EXIT:ADD ;\n" TEXT+
   S\" ARM\ns\q x\q 3 die\n: MAIN ( -- ) ;\n" TEXT+
   RUN-BUILD {: outu:n erru:n rc:n :}
   s" a build's exit action runs before hb-build's when its source exits" T-LABEL
   outu erru rc 70 EXPECT-RC
   erru SCRIPT$ ERR-HAS? TTRUE
   erru s" ended the process during the build" ERR-HAS? TTRUE
   outu s" bye" OUT-HAS? TTRUE
   erru s" older hb-build: " ERR-HAS? TTRUE ;

\ The source's task dies during the build: its exit runs hb's vector, so the
\ source's action runs and hb-build fails 70 with its line.
: CASE-BUILD-TASK ( -- )
   s" build-task.f" s" build-task-out" CASE-PATHS
   0 TEXT-U !
   S\" require src/lib/task.f\nrequire src/lib/at-exit.f\n$10000 TASK:TASK CHILD-TASK\n" TEXT+
   S\" : BYE ( -- ) s\q bye\q type cr ;\n: WORK ( -- ) s\q task: dying\q 3 die ;\n" TEXT+
   S\" : GO ( -- ) [: BYE ;] AT-EXIT:ADD [: WORK ;] CHILD-TASK TASK:ACTIVATE\n" TEXT+
   S\"    CHILD-TASK TASK:JOIN MATCH result ok OF drop ENDOF err OF drop ENDOF ;MATCH ;\n" TEXT+
   S\" GO\n: MAIN ( -- ) ;\n" TEXT+
   RUN-BUILD {: outu:n erru:n rc:n :}
   s" a build whose task dies fails 70 after the source's action" T-LABEL
   outu erru rc 70 EXPECT-RC
   erru SCRIPT$ ERR-HAS? TTRUE
   erru s" ended the process during the build" ERR-HAS? TTRUE
   outu s" bye" OUT-HAS? TTRUE ;

create RUN-FILE-BUF PATH-CAP allot
variable RUN-FILE-U

: RUN-FILE$ ( -- ^u8 n ) RUN-FILE-BUF RUN-FILE-U @ ;

\ It builds and runs a program because the build boundary is under test: the
\ exit actions and cleanups its source's top level registers run when the
\ build's part ends, the built program starts with none of them, and
\ registering at run time works from empty; it inits a queue its build
\ defined, whose record address the build must carry; it reads SRCLOC:TOKEN$,
\ data of src/core/checker.f, which the program reaches without the checker.
: CASE-BUILT ( -- )
   s" built.f" s" built-out" CASE-PATHS
   ROOT$ s" built-run" RUN-FILE-BUF FS:JOIN-PATH RUN-FILE-U !
   0 TEXT-U !
   S\" require src/lib/fs-mutate.f\nrequire src/lib/queue.f\n" TEXT+
   S\" $10 QUEUE:QUEUE JOBS\n" TEXT+
   S\" : BUILD-ACTION ( -- ) s\q build-time exit action ran\q type cr ;\n" TEXT+
   S\" : RUN-ACTION ( -- ) s\q run-time exit action ran\q type cr ;\n" TEXT+
   S\" : BUILT-FILE ( -- ^u8 n ) s\q " TEXT+  TREE$ TEXT+  S\" .built\q ;\n" TEXT+
   S\" : RUN-FILE ( -- ^u8 n ) s\q " TEXT+  RUN-FILE$ TEXT+  S\" \q ;\n" TEXT+
   S\" ' BUILD-ACTION AT-EXIT:ADD\n" TEXT+
   S\" BUILT-FILE s\q b\q FS:WRITE-ALL BUILT-FILE FS:CLEANUP+\n" TEXT+
   S\" : MAIN ( -- ) SRCLOC:TOKEN$ 2drop ['] RUN-ACTION AT-EXIT:ADD\n" TEXT+
   S\"    RUN-FILE s\q r\q FS:WRITE-ALL RUN-FILE FS:CLEANUP+ JOBS QUEUE:INIT\n" TEXT+
   S\"    s\q main done\q type cr ;\n" TEXT+
   RUN-BUILD {: outu:n erru:n rc:n :}
   s" a program that reaches the checker's data builds" T-LABEL
   outu erru rc 0 EXPECT-RC
   rc 0<> if exit then
   s" a built program starts with none of the build's exit actions and cleanups" T-LABEL
   outu s" build-time exit action ran" OUT-HAS? TTRUE
   0 TEXT-U !  TREE$ TEXT+  s" .built" TEXT+
   TEXT$ FS:EXISTS? TFALSE
   RUN-TARGET {: pu:n pe:n prc:n :}
   pu pe prc 0 EXPECT-RC
   pu s" main done" OUT-HAS? TTRUE
   pu s" run-time exit action ran" OUT-HAS? TTRUE
   pu s" build-time exit action ran" OUT-HAS? TFALSE
   RUN-FILE$ FS:EXISTS? TFALSE ;

\ A MAIN that throws the record its TMPDIR gives, else its own code's.
: RECORD-TEXT ( -- )
   0 TEXT-U !
   S\" require src/lib/fs-mutate.f\n" TEXT+
   S\" -100001 s\q the program's own failure\q ERROR:CODE E-BUILT-CASE\n" TEXT+
   S\" : MAIN ( -- ) s\q built\q FS:MKTEMP-DIR 2drop E-BUILT-CASE throw ;\n" TEXT+ ;

\ The built program under a TMPDIR that is a directory ends with the record of
\ E-BUILT-CASE, once, and status 67.
: EXPECT-CASE-RECORD ( -- )
   PROC-ARGV-ENV-RESET
   s" TMPDIR" >LEN ROOT$ >LEN PROC-ENV+
   TREE$ RUN-PATH {: qu:n qe:n qrc:n :}
   qu qe qrc 67 EXPECT-RC
   0 TEXT-U !
   S\" error[E-BUILT-CASE]: the program's own failure\n  = code: -100001\n" TEXT+
   ERR qe TEXT$ COUNT-SUB 1 T=
   qe s" uncaught throw" ERR-HAS? TFALSE ;

\ A built program ends an uncaught throw with one record and status 67, no
\ kernel line: the record its throw carried, else its code's declaration.
: CASE-BUILT-RECORD ( -- )
   s" built-record.f" s" built-record-out" CASE-PATHS
   RECORD-TEXT
   RUN-BUILD {: outu:n erru:n rc:n :}
   s" a built program prints the record its uncaught throw carries" T-LABEL
   outu erru rc 0 EXPECT-RC
   rc 0<> if exit then
   ROOT$ s" missing" RUN-FILE-BUF FS:JOIN-PATH RUN-FILE-U !
   PROC-ARGV-ENV-RESET
   s" TMPDIR" >LEN RUN-FILE$ >LEN PROC-ENV+
   TREE$ RUN-PATH {: pu:n pe:n prc:n :}
   pu pe prc 67 EXPECT-RC
   0 TEXT-U !
   S\" error[E-FS-DIR]: TMPDIR is not a directory\n  = TMPDIR: " TEXT+
   RUN-FILE$ TEXT+
   S\" \n  = help: point TMPDIR at an existing directory, or unset it to use /tmp\n" TEXT+
   ERR pe TEXT$ COUNT-SUB 1 T=
   pe s" uncaught throw" ERR-HAS? TFALSE
   s" a built program prints the record of a code its throw carries none for" T-LABEL
   EXPECT-CASE-RECORD ;

\ A second build: a --repl program runs MAIN from hb's own command line, not from
\ the entry a plain build gives it.
: CASE-REPL-RECORD ( -- )
   s" repl-record.f" s" repl-record-out" CASE-PATHS
   RECORD-TEXT
   s" --repl" RUN-BUILD-AS {: outu:n erru:n rc:n :}
   s" a --repl program prints the record of its MAIN's uncaught throw" T-LABEL
   outu erru rc 0 EXPECT-RC
   rc 0<> if exit then
   EXPECT-CASE-RECORD ;

\ hb-build dies after a failed build has unmapped its source's own AT-EXIT; it
\ starts a build because only a build closes the source's space before the exit.
: CASE-BUILD-ACTION ( -- )
   s" build-action.f" s" build-action-out" CASE-PATHS
   0 TEXT-U !
   S\" require src/lib/at-exit.f\n" TEXT+
   S\" : ACT ( -- ) s\q act ran\q type cr ;  ' ACT AT-EXIT:ADD\n" TEXT+
   S\" NOSUCHWORD\n" TEXT+
   RUN-BUILD {: outu:n erru:n rc:n :}
   s" a failed build whose source added an exit action fails 70" T-LABEL
   outu erru rc 70 EXPECT-RC
   erru SCRIPT$ ERR-HAS? TTRUE
   erru s" habu-crash" ERR-HAS? TFALSE
   outu s" act ran" OUT-HAS? TTRUE ;

\ The task and the two cleanups both build sources start with.
: CLEANUPS-TEXT ( -- )
   0 TEXT-U !
   S\" require src/lib/task.f\nTASK:MIN-STACK TASK:TASK CHILD-TASK\nvariable CHILD-MARK\n" TEXT+
   S\" : CHILD-FIRST ( -- ) CHILD-MARK @ 10 * 1 + CHILD-MARK ! ;\n" TEXT+
   S\" : CHILD-SECOND ( -- ) CHILD-MARK @ 10 * 2 + CHILD-MARK ! ;\n" TEXT+ ;

\ A build is the boundary under test: cleanups a source's top level registers
\ on a defined task hold rows in the table (src/lib/task.f TASK-EXIT-QT), which
\ the target carries; it prints 21, each cleanup run once, newest first.
: CASE-BUILD-CLEANUPS ( -- )
   s" cleanups.f" s" cleanups-out" CASE-PATHS
   CLEANUPS-TEXT
   S\" ' CHILD-FIRST CHILD-TASK TASK:AT-EXIT\n' CHILD-SECOND CHILD-TASK TASK:AT-EXIT\n" TEXT+
   S\" : MAIN ( -- ) [: 0 TASK:RETURN ;] CHILD-TASK TASK:ACTIVATE CHILD-TASK TASK:JOIN\n" TEXT+
   S\"    MATCH result ok OF drop ENDOF err OF . ENDOF ;MATCH CHILD-MARK @ . ;\n" TEXT+
   RUN-BUILD {: outu:n erru:n rc:n :}
   s" a build carries its task's cleanups into the target" T-LABEL
   outu erru rc 0 EXPECT-RC
   rc 0<> if exit then
   RUN-TARGET {: poutu:n perru:n prc:n :}
   s" the target's task runs each carried cleanup once, newest first" T-LABEL
   OUT poutu S\" 21\n" T$=
   poutu perru prc 0 EXPECT-RC ;

\ The 128 TASK:NEW tasks push the cleanups' rows past the table into a mapped
\ chunk, which no target can carry: the linker refuses the source by the
\ address cell EXIT-CHUNKS and the build fails 67.
: CASE-BUILD-CHUNK ( -- )
   s" cleanups-chunk.f" s" cleanups-chunk-out" CASE-PATHS
   CLEANUPS-TEXT
   S\" : CHILD-HOLD ( n -- ) {: k:n :} k 0= if ['] CHILD-FIRST CHILD-TASK TASK:AT-EXIT\n" TEXT+
   S\"    ['] CHILD-SECOND CHILD-TASK TASK:AT-EXIT exit then\n" TEXT+
   S\"    TASK:MIN-STACK TASK:NEW {: t:^ :} [: ;] t TASK:AT-EXIT k 1 - RECURSE t TASK:FREE ;\n" TEXT+
   S\" 128 CHILD-HOLD\n: MAIN ( -- ) ;\n" TEXT+
   RUN-BUILD {: outu:n erru:n rc:n :}
   s" a source that leaves a cleanup chunk mapped is refused" T-LABEL
   outu erru rc 67 EXPECT-RC
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
   CASE-ACTIONS-MANY
   CASE-RESETS-MANY
   CASE-STAGED
   CASE-TASK-ADD
   CASE-TASK-DIE
   CASE-TASK-WAIT
   CASE-FORK-OWNER
   CASE-BUILD
   CASE-BUILD-EXIT
   CASE-BUILD-TASK
   CASE-BUILT
   CASE-BUILT-RECORD
   CASE-REPL-RECORD
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
