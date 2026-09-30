\ pg-cluster.f - lib/pg-test.f and lib/db/rows-test.f against a private
\ PostgreSQL cluster.
\
\     bin/hb --load test/db/pg-cluster.f [-- case.f ...]
\
\ initdb makes a trust-authentication cluster under HB_TMP and this process
\ starts postgres on it, listening on a Unix-domain socket only
\ (listen_addresses=''), so rows running beside each other have no TCP port to
\ collide on. The case files - the script arguments, or else lib/pg-test.f and
\ then lib/db/rows-test.f - each run in a child engine that takes the conninfo
\ as its one script argument and writes its report straight to this process's
\ stdout and stderr; the first that fails ends the run, and the row's last line
\ names it. This process stops the server however the children end - exit,
\ failed assertion, die, crash or a deadline - and the cleanup registry
\ (lib/fs-mutate.f CLEANUP-TREE+) removes the directories it made when this
\ process exits, including by die.
\
\ THE SERVER IS THIS PROCESS'S CHILD FOR ITS WHOLE LIFE. pg_ctl starts a
\ postmaster that forks, calls setsid and execs a shell, and then pg_ctl exits:
\ the postmaster is left with init for a parent, in a session of its own, where
\ a pool's tree walk (lib/process-tree.f) cannot reach it, and a killed row's
\ server ran on until its lock-file recheck noticed the removed data directory,
\ about a minute later. Spawned here, with no shell and no setsid, the
\ postmaster keeps this process as its parent until this process reaps it, and
\ every process it forks keeps the postmaster as its own (measured: each calls
\ setsid, and none leaves its parent). A pool that kills this row stops and
\ kills them all with it. The server is ready when the status line of
\ postmaster.pid says so - the line pg_ctl -w waits for - and it is stopped with
\ the SIGQUIT of pg_ctl -m immediate.
\
\ THE SERVER ENDS THROUGH ITS OWN EXIT, NEVER BY SIGKILL ALONE. PostgreSQL makes
\ a SysV shared-memory segment, keyed by the data directory's inode, and removes
\ it only on its own way out. A server that is SIGKILLed leaves the segment for
\ good - APFS hands that inode out no more, and kern.sysv.shmmni caps the
\ segments host-wide (32 here) - so a killed pg row took one each time, and a
\ host out of them starts no PostgreSQL at all. So this process catches
\ SIGTERM, SIGINT and SIGHUP and answers one wherever it waits: the server is
\ sent its SIGQUIT and given QUIT-MS, the running case engine's tree is ended,
\ the directories go, and the process dies of the signal. A gate pool sends a
\ row's root SIGTERM, when it catches it, before it kills the row's tree
\ (test/gate-pool.f GT-POOL-ASK-END). initdb's step is not watched: a signal
\ that lands in it is answered when it ends, before the server starts.
\
\ SIGKILL cannot be answered. After one the server runs on with init for a
\ parent and both directories stay. Recover with `kill -QUIT` to the pid on the
\ first line of <root>/data/postmaster.pid, then remove the two directories;
\ the postmaster's command line names both (-D and -k). A server that was
\ SIGKILLed as well has left its segment: `ipcs -m` lists it with no process
\ attached and `ipcrm -m <id>` removes it (docs/db.md).
\
\ THE SOCKET DIRECTORY IS NOT UNDER HB_TMP. sun_path holds 104 bytes on macOS
\ and 108 on Linux, and a pool slot's HB_TMP takes about 100 of them: a socket
\ there makes postgres refuse to start with `Unix-domain socket path ... is too
\ long (maximum 103 bytes)`. Under the gate pool the socket is in HB_SOCK_TMP,
\ the short directory the pool made for this row and removes with it
\ (test/gate-pool.f). Run on its own, this process makes one under TMPDIR, as
\ lib/fs-mutate-test.f FMT-ROOT! does for its socket.
\
\ initdb and postgres on PATH are a gate requirement on every host
\ (docs/bootstrap.md "Requirements"). A missing one ends the row naming it,
\ never a skip.
require lib/errors.f
require lib/string.f
require lib/fmt.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/process-tree.f
require lib/signal.f
require lib/engine-candidate.f

package PG-CLUSTER

$10000 constant CAPTURE-CAP     \ initdb's chatter, or the server's log
\ The step deadlines - initdb, start, each case file and stop - sum to 300 s,
\ inside the row's 360 s (test/gate-stdlib-lib.f SUITE-TIMEOUT-MS), so this
\ process and not the pool is the one that gives up and stops the server.
60000 constant TOOL-MS
60000 constant FILE-MS
\ A signalled stop. An immediate shutdown SIGKILLs the children still alive
\ 5 s into it (PostgreSQL's SIGKILL_CHILDREN_AFTER_SECS) and then exits, so
\ 6 s is the server's whole stop. With the case engine's walk (2 s at most,
\ lib/process-tree.f SETTLE-MS) it stays inside the pool's grace
\ (test/gate-pool.f GT-POOL-GRACE-MS, 10 s).
6000 constant QUIT-MS
100 constant READY-POLL-MS      \ pg_ctl -w's own interval
8 constant STATUS-LINE          \ postmaster.pid's server status (LOCK_FILE_LINE_PM_STATUS)
3 constant SIGQUIT              \ immediate shutdown; 3 on every host
10 constant NEWLINE
-1 constant NO-PID
-1 constant NO-FD

FS-PATH-CAP BUFFER: INITDB      variable INITDB-U
FS-PATH-CAP BUFFER: POSTGRES    variable POSTGRES-U
FS-PATH-CAP BUFFER: ROOT        variable ROOT-U
FS-PATH-CAP BUFFER: SOCKETS     variable SOCKETS-U
FS-PATH-CAP BUFFER: DATA        variable DATA-U
FS-PATH-CAP BUFFER: LOG         variable LOG-U
FS-PATH-CAP BUFFER: PIDFILE     variable PIDFILE-U
FS-PATH-CAP $40 + constant TEXT-CAP
TEXT-CAP BUFFER: CONNINFO       variable CONNINFO-U
CAPTURE-CAP BUFFER: OUT
CAPTURE-CAP BUFFER: ERR

variable SERVER                 \ the postmaster's pid until it is reaped
variable WATCH                  \ readable once the postmaster has exited
variable RUNNING                \ the running case engine's pid until it is reaped
variable CASE-WATCH             \ readable once that engine has exited
TYPED-VARIABLE UP bool          \ the server said it was ready
TYPED-VARIABLE STOPPED bool     \ the server's stop ended it with exit 0
NO-PID SERVER !
NO-FD WATCH !
NO-PID RUNNING !
NO-FD CASE-WATCH !
false UP !
false STOPPED !

: INITDB$ ( -- ptr u8 n )    INITDB INITDB-U @ ;
: POSTGRES$ ( -- ptr u8 n )  POSTGRES POSTGRES-U @ ;
: ROOT$ ( -- ptr u8 n )      ROOT ROOT-U @ ;
: SOCKETS$ ( -- ptr u8 n )   SOCKETS SOCKETS-U @ ;
: DATA$ ( -- ptr u8 n )      DATA DATA-U @ ;
: LOG$ ( -- ptr u8 n )       LOG LOG-U @ ;
: PIDFILE$ ( -- ptr u8 n )   PIDFILE PIDFILE-U @ ;
: CONNINFO$ ( -- ptr u8 n )  CONNINFO CONNINFO-U @ ;
: CONNINFO+ ( ptr u8 n -- )  CONNINFO TEXT-CAP CONNINFO-U BUF-APPEND ;
: ARG+ ( ptr u8 n -- )       >LEN PROC-ARGV+ ;


\ ---- the tools and the two directories ------------------------------------
: TOOL ( ptr u8 n ptr u8 -- n ) {: name:ptr size:n dst:ptr :}
   name size >LEN dst FIND-EXECUTABLE MATCH option
      none OF
         s" pg-cluster: required executable missing on PATH: " type
         name size type cr
         s" " E-PROC-PATH die
      ENDOF
      some OF LEN>N ENDOF
   ;MATCH ;

: SOCKETS! ( ptr u8 n -- ) {: a:ptr u:n :}
   u FS-PATH-CAP > if E-FS-PATH throw then
   a SOCKETS u BYTE-COPY  u SOCKETS-U ! ;

\ A made directory's name lives in the per-task band the next MAKE-TEMP-DIR
\ reuses, so each is copied out before the other is made. The pool's socket
\ directory is the pool's to remove.
: MAKE-DIRS ( -- )
   s" habu-pg" HB-TMP-MKDIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY  u ROOT-U !
   ROOT$ CLEANUP-TREE+
   s" HB_SOCK_TMP" GETENV dup 0 > if
      SOCKETS!
   else
      2drop s" habu-pg" TMPDIR-MKDIR SOCKETS!
      SOCKETS$ CLEANUP-TREE+
   then
   ROOT$ s" data" DATA JOIN-PATH DATA-U !
   ROOT$ s" postgres.log" LOG JOIN-PATH LOG-U !
   DATA$ s" postmaster.pid" PIDFILE JOIN-PATH PIDFILE-U ! ;

\ libpq splits a conninfo at spaces, so it quotes the directory.
: TEXTS! ( -- )
   CONNINFO-U BUF-RESET
   s" host='" CONNINFO+ SOCKETS$ CONNINFO+
   s" ' dbname=postgres user=habu connect_timeout=5" CONNINFO+ ;


\ ---- one step at a time ----------------------------------------------------
\ Every step, and the server, inherits this process's environment with
\ LC_ALL=C: the cluster's locale is C whatever the host's, and on macOS a
\ postmaster started without a valid locale in its environment stops with
\ `postmaster became multithreaded during startup` (measured with the plain
\ RUN-ARGV-CAPTURE-OUTCOME, which passes an empty environment).
: ENV! ( -- )
   PROC-ENV-INHERIT-MISSING
   s" LC_ALL" >LEN s" C" >LEN PROC-ENV-SET ;

: STEP ( ptr u8 n n -- len len outcome ) {: path:ptr pathu:n deadline:n :}
   ENV!
   path pathu >LEN OUT CAPTURE-CAP >LEN ERR CAPTURE-CAP >LEN deadline >MS
   RUN-ARGV-ENV-CAPTURE-OUTCOME ;

: SHOW ( len len -- ) {: outu:len erru:len :}
   OUT outu LEN>N type
   2 ERR erru LEN>N write drop ;

\ True for a step that exited 0; otherwise one line says how it ended.
: EXITED-0? ( outcome ptr u8 n -- bool ) {: what:ptr whatu:n :}
   s" pg-cluster: " type what whatu type
   MATCH outcome
      exited OF
         dup 0= if drop s"  ok" type cr true exit then
         s"  exited " type FMT:.INT
      ENDOF
      signaled OF s"  died of signal " type FMT:.INT ENDOF
      timeout OF s"  passed its deadline" type ENDOF
   ;MATCH
   cr false ;

: SHOW-LOG ( -- )
   LOG$ EXISTS? 0= if exit then
   LOG$ OUT CAPTURE-CAP READ-ALL {: n:n :}
   OUT n type ;

: INITDB-RUN ( -- )
   PROC-ARGV-ENV-RESET
   s" -D" ARG+ DATA$ ARG+
   s" -A" ARG+ s" trust" ARG+
   s" -U" ARG+ s" habu" ARG+
   s" -E" ARG+ s" UTF8" ARG+
   s" --no-sync" ARG+
   INITDB$ TOOL-MS STEP s" initdb" EXITED-0? if 2drop exit then
   SHOW
   s" pg-cluster: initdb failed" 1 die ;


\ ---- waiting on a process -----------------------------------------------------
\ A process's watch (proc-watch-open) is readable once it has exited. macOS
\ cannot watch a process that has already exited (test/proc-watch-smoke.f) and
\ answers -1, so a process that has no watch has ended.
: GONE-WITHIN? ( n n -- bool ) {: watch:n ms:n :}
   watch 0 < if true exit then
   watch >FD POLLIN PROC-PFD!
   1 ms ms >MS PROC-DEADLINE-AT PROC-POLL-RESTART 0 > ;

: CLOSE-WATCH ( ptr n -- ) {: p:ptr :}
   p @ 0 >= if p @ close then
   NO-FD p ! ;


\ ---- the server -------------------------------------------------------------
: OPEN-NULL ( -- n )
   s\" /dev/null\z" drop open-rd {: fd:n :}
   fd 0 < if E-FS-OPEN throw then
   fd ;

: OPEN-LOG ( -- n )
   LOG$ FS-PATHZ FS-O-WRONLY FS-O-CREAT or FS-O-TRUNC or FS-MODE-0644 open {: fd:n :}
   fd 0 < if E-FS-OPEN throw then
   fd ;

\ A server with no watch had ended before the watch was asked for; it is
\ reaped here and READY? finds no server. The SIGQUIT is for a host whose -1
\ means something else: a live server is stopped rather than waited on.
: ENDED-EARLY ( -- )
   SERVER @ >PID SIGQUIT PROC-KILL-RAW drop
   s" pg-cluster: postgres ended before it was ready" type cr
   SERVER @ >PID PROC-WAIT-OUTCOME s" postgres" EXITED-0? drop
   NO-PID SERVER ! ;

\ postgres gets /dev/null for stdin and its log for stdout and stderr, as
\ pg_ctl -l gave it. No throw comes after the spawn: from there every way out
\ passes STOP or ANSWER.
: START ( -- )
   PROC-ARGV-ENV-RESET
   s" -D" ARG+ DATA$ ARG+
   s" -k" ARG+ SOCKETS$ ARG+
   s" -c" ARG+ s" listen_addresses=" ARG+
   ENV!
   OPEN-NULL OPEN-LOG {: in:n log:n :}
   in >FD FD-CLOEXEC!
   log >FD FD-CLOEXEC!
   POSTGRES$ >LEN in >FD log >FD log >FD PROC-SPAWN-ARGV-ENV-IO PID>N SERVER !
   in close
   log close
   SERVER @ proc-watch-open WATCH !
   WATCH @ 0 < if ENDED-EARLY then ;

\ The offset of line n, counted from 1, in the first u bytes of OUT; -1 when
\ they hold fewer lines.
: LINE-AT ( n n -- n ) {: line:n u:n :}
   line 1 = if 0 exit then
   1 u 0 ?do
      OUT i + c@ NEWLINE = if
         1+ dup line = if drop i 1+ unloop exit then
      then
   loop
   drop -1 ;

\ The server's status line says `ready` once it accepts connections. A file
\ that will not open or is not yet that long is a server not that far yet.
: STATUS-READY? ( -- bool )
   PIDFILE$ FS-PATHZ open-rd {: fd:n :}
   fd 0 < if false exit then
   fd OUT CAPTURE-CAP read {: got:n :}
   fd close
   got 0 <= if false exit then
   STATUS-LINE got LINE-AT {: at:n :}
   at 0 < if false exit then
   OUT at + got at - s" ready" STARTS-WITH? ;

\ The server is sent its SIGQUIT and given ms to end. One that outlives that is
\ killed with every process under it (lib/process-tree.f), so none outlives
\ this process either way - but a server killed so leaves its segment.
: STOP ( n -- ) {: ms:n :}
   SERVER @ NO-PID = if exit then
   SERVER @ >PID SIGQUIT PROC-KILL-RAW drop
   WATCH @ ms GONE-WITHIN? if
      SERVER @ >PID PROC-WAIT-OUTCOME s" postgres stop" EXITED-0? STOPPED !
   else
      SERVER @ >PID PROC-TREE:KILL-TREE
      SERVER @ >PID PROC-WAIT-STATUS drop
      OUTCOME:TIMEOUT s" postgres stop" EXITED-0? STOPPED !
   then
   NO-PID SERVER !
   WATCH CLOSE-WATCH ;


\ ---- the case engine --------------------------------------------------------
\ Its stdout and stderr are this process's, so its report reaches the row's
\ log as it runs and a red row carries it; its stdin is /dev/null.
: CASE-SPAWN ( ptr u8 n -- ) {: file:ptr fileu:n :}
   PROC-ARGV-ENV-RESET
   s" --load" ARG+ file fileu ARG+
   s" --" ARG+ CONNINFO$ ARG+
   ENV!
   OPEN-NULL {: in:n :}
   in >FD FD-CLOEXEC!
   ENGINE-CANDIDATE:PATH$ >LEN in >FD NO-FD >FD NO-FD >FD PROC-SPAWN-ARGV-ENV-IO
   PID>N RUNNING !
   in close
   RUNNING @ proc-watch-open CASE-WATCH ! ;

: CASE-REAP ( -- outcome )
   RUNNING @ >PID PROC-WAIT-OUTCOME
   NO-PID RUNNING !
   CASE-WATCH CLOSE-WATCH ;

\ The engine leads a group of its own, as every spawned child does, so it is
\ ended with its tree: once this process has gone, what it spawned would be
\ beyond a pool's walk.
: CASE-KILL ( -- )
   RUNNING @ NO-PID = if exit then
   RUNNING @ >PID PROC-TREE:KILL-TREE
   RUNNING @ >PID PROC-WAIT-STATUS drop
   NO-PID RUNNING !
   CASE-WATCH CLOSE-WATCH ;


\ ---- a caught signal ----------------------------------------------------------
\ SIGTERM, SIGINT and SIGHUP, as the gate root catches them (test/gate-pool.f
\ GT-POOL-CATCH-SIGNALS). One this process was started with ignored stays
\ ignored. No Forth word runs in a handler (lib/signal.f): the signal is a
\ number on a pipe that every wait below polls beside its process.
: CATCH-ONE ( n -- ) {: sig:n :}
   sig SIGNAL:IGNORED? if exit then
   sig SIGNAL:CATCH ;

: CATCH-SIGNALS ( -- )
   SIGNAL:INIT
   SIGNAL:SIGTERM CATCH-ONE
   SIGNAL:SIGINT CATCH-ONE
   SIGNAL:SIGHUP CATCH-ONE ;

: SAY-THROW ( ptr u8 n n -- ) {: what:ptr whatu:n code:n :}
   code 0= if exit then
   s" pg-cluster: " type what whatu type s"  threw " type code FMT:.INT cr ;

\ THE ANSWER. The server is sent its SIGQUIT first and shuts down while the case
\ engine's tree is ended; then it is given QUIT-MS. The directories go, and
\ the process dies of the signal, so its caller reads the status an uncaught
\ one would have left. A step that throws is named and the answer goes on.
\ It never returns: it may run from inside SERVE, and the die ends that too.
: ANSWER ( n -- ) {: sig:n :}
   s" pg-cluster: signal " type sig FMT:.INT s" , stopping postgres" type cr
   SERVER @ NO-PID <> if SERVER @ >PID SIGQUIT PROC-KILL-RAW drop then
   s" case engine kill" [: CASE-KILL ;] catch SAY-THROW
   s" postgres stop" [: QUIT-MS STOP ;] catch SAY-THROW
   s" cleanup" [: CLEANUP-RUN ;] catch SAY-THROW
   SIGNAL:RELEASE
   getpid >PID sig PROC-KILL-RAW drop
   s" pg-cluster: signal" 128 sig + die ;

: SIGNAL-CHECK ( -- )
   SIGNAL:TAKE MATCH SIGNAL:signal-result
      signal OF ANSWER ENDOF
      timeout OF ENDOF
   ;MATCH ;

\ TRUE once the watch reports its process has ended, FALSE when ms pass
\ first. A caught signal is answered here.
: ENDED-WITHIN? ( n n -- bool ) {: watch:n ms:n :}
   watch 0 < if true exit then
   ms >MS PROC-DEADLINE-AT {: deadline:n :}
   begin
      watch >FD POLLIN 0 >IDX PROC-PFD-AT!
      SIGNAL:FD POLLIN 1 >IDX PROC-PFD-AT!
      2 deadline PROC-LEFT-MS MS>N deadline PROC-POLL-RESTART {: rc:n :}
      rc 0 < if E-PROC-OUTPUT throw then
      rc 0= if false exit then
      1 >IDX PROC-PFD-REVENTS 0<> if SIGNAL-CHECK then
      0 >IDX PROC-PFD-REVENTS 0<> if true exit then
   again ;


\ ---- the run ------------------------------------------------------------------
\ Ready inside TOOL-MS; a server that ends first has failed to start.
: READY? ( -- bool )
   SERVER @ NO-PID = if false exit then
   TOOL-MS >MS PROC-DEADLINE-AT {: deadline:n :}
   begin
      STATUS-READY? if s" pg-cluster: postgres ready" type cr true exit then
      deadline PROC-LEFT-MS MS>N {: left:n :}
      left 0= if s" pg-cluster: postgres not ready inside its deadline" type cr false exit then
      WATCH @ left READY-POLL-MS min ENDED-WITHIN? if
         s" pg-cluster: postgres ended before it was ready" type cr false exit
      then
   again ;

: RUN-FILE ( ptr u8 n -- bool ) {: file:ptr fileu:n :}
   file fileu CASE-SPAWN
   CASE-WATCH @ FILE-MS ENDED-WITHIN? if
      CASE-REAP
   else
      CASE-KILL OUTCOME:TIMEOUT
   then
   file fileu EXITED-0? ;

\ The case files are the script arguments, or lib/pg-test.f and then
\ lib/db/rows-test.f when there are none.
: CASE-N ( -- n )
   SCRIPT-ARGC 0= if 2 exit then
   SCRIPT-ARGC ;

: CASE$ ( n -- ptr u8 n ) {: i:n :}
   SCRIPT-ARGC 0 > if i SCRIPT-ARGV$ exit then
   i 0= if s" lib/pg-test.f" exit then
   s" lib/db/rows-test.f" ;

\ The files run against the one cluster in order, and the first that fails
\ ends the run: the answer is the last file run and whether it passed.
: CASES ( -- ptr u8 n bool )
   CASE-N 1- 0 ?do
      i CASE$ 2dup RUN-FILE 0= if false unloop exit then 2drop
   loop
   CASE-N 1- CASE$ 2dup RUN-FILE ;

: SERVE ( -- ptr u8 n bool )
   READY? 0= if s" postgres start" false exit then
   true UP !
   CASES ;

: END ( -- )
   CASE-KILL
   TOOL-MS STOP ;

\ The log is shown for a server that never came up, once STOP has ended it:
\ SHOW-LOG's READ-ALL throws E-FS-CAPACITY on a log over CAPTURE-CAP, which a
\ server that ran the cases may write.
: MAIN ( -- )
   CATCH-SIGNALS
   s" initdb" INITDB TOOL INITDB-U !
   s" postgres" POSTGRES TOOL POSTGRES-U !
   MAKE-DIRS TEXTS!
   INITDB-RUN
   SIGNAL-CHECK
   START
   [: SERVE ;] [: END ;] finally {: what:ptr whatu:n passed:bool :}
   SIGNAL-CHECK
   UP @ 0= if SHOW-LOG then
   passed STOPPED @ and if exit then
   passed 0= if s" pg-cluster: " type what whatu type s"  failed" type cr then
   s" " 1 die ;

MAIN

;package
