\ pg-cluster.f - src/lib/pg-test.f and src/lib/pg-rows-test.f against a private
\ PostgreSQL cluster.
\
\     bin/hb --load test/db/pg-cluster.f [-- case.f ...]
require src/lib/errors.f
require src/lib/string.f
require src/lib/fmt.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/process-tree.f
require src/lib/signal.f
require src/lib/engine-candidate.f
require src/lib/test/fifo.f

package PG-CLUSTER

$10000 constant CAPTURE-CAP     \ initdb's chatter, or the server's log
60000 constant TOOL-MS
60000 constant DEFAULT-CASE-MS
$7FFFFFFF constant CASE-MAX-MS  \ poll(2) receives a signed 32-bit millisecond count
\ A signalled stop: an immediate shutdown SIGKILLs the children still alive 5 s
\ in (PostgreSQL's SIGKILL_CHILDREN_AFTER_SECS) and exits, so 6 s is the
\ server's whole stop. With the case engine's walk (src/lib/process-tree.f
\ SETTLE-MS, 2 s at most) it stays inside the pool's grace (test/gate-pool.f
\ GT-POOL-GRACE-MS, 10 s). initdb gets the same.
6000 constant QUIT-MS
100 constant READY-POLL-MS      \ pg_ctl -w's own interval
8 constant STATUS-LINE          \ postmaster.pid's server status (LOCK_FILE_LINE_PM_STATUS)
3 constant SIGQUIT              \ immediate shutdown; 3 on every host
10 constant NEWLINE
-1 constant NO-PID
-1 constant NO-FD

PATH-CAP BUFFER: INITDB      variable INITDB-U
PATH-CAP BUFFER: POSTGRES    variable POSTGRES-U
PATH-CAP BUFFER: ROOT        variable ROOT-U
PATH-CAP BUFFER: SOCKETS     variable SOCKETS-U
PATH-CAP BUFFER: DATA        variable DATA-U
PATH-CAP BUFFER: LOG         variable LOG-U
PATH-CAP BUFFER: INITDB-LOG  variable INITDB-LOG-U
PATH-CAP BUFFER: PIDFILE     variable PIDFILE-U
PATH-CAP $40 + constant TEXT-CAP
TEXT-CAP BUFFER: CONNINFO       variable CONNINFO-U
CAPTURE-CAP BUFFER: OUT

variable INITDB-PID             \ initdb's pid until it is reaped
variable INITDB-WATCH           \ readable once initdb has exited
variable SERVER                 \ the postmaster's pid until it is reaped
variable WATCH                  \ readable once the postmaster has exited
variable RUNNING                \ the running case engine's pid until it is reaped
variable CASE-WATCH             \ readable once that engine has exited
variable CASE-MS                \ one deadline for each case file
variable HOLD                   \ PG_HOLD_FIFO's write end, NO-FD without one
TYPED-VARIABLE UP bool          \ the server said it was ready
TYPED-VARIABLE STOPPED bool     \ the server's stop ended it with exit 0
TYPED-VARIABLE FAILED bool
TYPED-VARIABLE LATE bool        \ the first step to fail passed its deadline
NO-PID INITDB-PID !
NO-FD INITDB-WATCH !
NO-PID SERVER !
NO-FD WATCH !
NO-PID RUNNING !
NO-FD CASE-WATCH !
NO-FD HOLD !
false UP !
false STOPPED !
false FAILED !
false LATE !
DEFAULT-CASE-MS CASE-MS !

: INITDB$ ( -- ptr u8 n )    INITDB INITDB-U @ ;
: POSTGRES$ ( -- ptr u8 n )  POSTGRES POSTGRES-U @ ;
: ROOT$ ( -- ptr u8 n )      ROOT ROOT-U @ ;
: SOCKETS$ ( -- ptr u8 n )   SOCKETS SOCKETS-U @ ;
: DATA$ ( -- ptr u8 n )      DATA DATA-U @ ;
: LOG$ ( -- ptr u8 n )       LOG LOG-U @ ;
: INITDB-LOG$ ( -- ptr u8 n ) INITDB-LOG INITDB-LOG-U @ ;
: PIDFILE$ ( -- ptr u8 n )   PIDFILE PIDFILE-U @ ;
: CONNINFO$ ( -- ptr u8 n )  CONNINFO CONNINFO-U @ ;
: CONNINFO+ ( ptr u8 n -- )  CONNINFO TEXT-CAP CONNINFO-U BUF-APPEND ;
: ARG+ ( ptr u8 n -- )       >LEN PROC-ARGV+ ;

: BAD-CASE-MS ( -- )
   s" pg-cluster: HB_PG_CASE_MS must be 1..2147483647 milliseconds" 64 die ;

: READ-CASE-MS ( -- )
   s" HB_PG_CASE_MS" GETENV {: value:ptr size:n :}
   value NULL-PTR = if exit then
   value size STR>NUMBER? MATCH option
      none OF BAD-CASE-MS ENDOF
      some OF ENDOF
   ;MATCH {: ms:n :}
   ms 1 < ms CASE-MAX-MS > or if BAD-CASE-MS then
   ms CASE-MS ! ;


\ initdb and postgres on PATH are a gate requirement on every host
\ (docs/bootstrap.md "Requirements"): a missing one fails the row, never skips
\ it.
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
   u PATH-CAP > if E-FS-PATH throw then
   a SOCKETS u BYTE-COPY  u SOCKETS-U ! ;

\ sun_path holds 104 bytes on macOS and 108 on Linux, and a pool slot's TMPDIR
\ takes about 100 of them, so under the gate pool the socket goes in
\ HB_SOCK_TMP, the short directory the pool makes and removes for this row
\ (test/gate-pool.f); alone it goes under TMPDIR, as src/lib/fs-mutate-test.f
\ FMT-ROOT! does. A made directory's name lives in the per-task band the next
\ FS:MKTEMP-DIR-IN reuses, so each is copied out before the other is made.
: MAKE-DIRS ( -- )
   s" habu-pg" FS:MKTEMP-DIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY  u ROOT-U !
   ROOT$ FS:CLEANUP-TREE+
   s" HB_SOCK_TMP" GETENV dup 0 > if
      SOCKETS!
   else
      2drop s" habu-pg" FS:MKTEMP-DIR SOCKETS!
      SOCKETS$ FS:CLEANUP-TREE+
   then
   ROOT$ s" data" DATA FS:JOIN-PATH DATA-U !
   ROOT$ s" postgres.log" LOG FS:JOIN-PATH LOG-U !
   ROOT$ s" initdb.log" INITDB-LOG FS:JOIN-PATH INITDB-LOG-U !
   DATA$ s" postmaster.pid" PIDFILE FS:JOIN-PATH PIDFILE-U ! ;

\ libpq splits a conninfo at spaces, so it quotes the directory.
: TEXTS! ( -- )
   CONNINFO-U BUF-RESET
   s" host='" CONNINFO+ SOCKETS$ CONNINFO+
   s" ' dbname=postgres user=habu connect_timeout=5" CONNINFO+ ;


\ Every step, and the server, gets LC_ALL=C: the cluster's locale is C whatever
\ the host's, and on macOS a postmaster started without a valid locale stops
\ with `postmaster became multithreaded during startup` (measured with
\ RUN-ARGV-CAPTURE-OUTCOME's empty environment).
: ENV! ( -- )
   PROC-ENV-INHERIT-MISSING
   s" LC_ALL" >LEN s" C" >LEN PROC-ENV-SET ;

\ The first step to fail decides how the row ends (FAIL-ROW); later failures
\ change nothing.
: STEP-FAILED ( bool -- ) {: late:bool :}
   FAILED @ if exit then
   true FAILED !
   late LATE ! ;

\ A case engine exits PROC-TIMEOUT-RC (124) when a wait of its own passed its
\ deadline (src/lib/pg-test.f RUN), so that exit is a late step, as is a case
\ ended at CASE-MS.
: EXITED-0? ( outcome ptr u8 n -- bool )
   {: what:ptr whatu:n :}
   MATCH outcome
      exited OF
         s" pg-cluster: " type what whatu type
         dup 0= if drop s"  ok" type cr true exit then
         s"  exited " type dup FMT:.INT
         PROC-TIMEOUT-RC = dup if s" , past its own deadline" type then
         STEP-FAILED
      ENDOF
      signaled OF
         s" pg-cluster: " type what whatu type
         s"  died of signal " type FMT:.INT false STEP-FAILED
      ENDOF
      timeout OF
         s" pg-cluster " type what whatu type s" : a deadline expired" type
         true STEP-FAILED
      ENDOF
   ;MATCH
   cr false ;

: SHOW-LOG ( ptr u8 n -- ) {: path:ptr pathu:n :}
   path pathu FS:EXISTS? 0= if exit then
   path pathu OUT CAPTURE-CAP FS:READ-ALL {: n:n :}
   OUT n type ;


\ macOS cannot watch a process that has already exited and answers -1, so a
\ process with no watch has ended.
: GONE-WITHIN? ( n n -- bool ) {: watch:n ms:n :}
   watch 0 < if true exit then
   watch >FD POLLIN PROC-PFD!
   1 ms ms >MS PROC-DEADLINE-AT PROC-POLL-RESTART 0 > ;

: CLOSE-WATCH ( ptr n -- ) {: p:ptr :}
   p @ 0 >= if p @ close then
   NO-FD p ! ;


: OPEN-NULL ( -- n )
   s\" /dev/null\z" drop open-rd {: fd:n :}
   fd 0 < if E-FS-OPEN throw then
   fd ;

: OPEN-LOG ( ptr u8 n -- n )
   FS:>PATHZ FS:O-WRONLY FS:O-CREAT or FS:O-TRUNC or FS:MODE-0644 open {: fd:n :}
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

\ postgres is spawned directly, not through pg_ctl, whose postmaster calls
\ setsid and is reparented to init, beyond a pool's tree walk
\ (src/lib/process-tree.f). Spawned here it stays this process's child and its
\ forks stay its own (measured), so a pool that kills this row kills them all.
\ listen_addresses is empty: rows running side by side share no TCP port. No
\ throw comes after the spawn: from there every way out passes STOP or ANSWER.
: START ( -- )
   PROC-ARGV-ENV-RESET
   s" -D" ARG+ DATA$ ARG+
   s" -k" ARG+ SOCKETS$ ARG+
   s" -c" ARG+ s" listen_addresses=" ARG+
   ENV!
   OPEN-NULL LOG$ OPEN-LOG {: in:n log:n :}
   in >FD FD-CLOEXEC!
   log >FD FD-CLOEXEC!
   POSTGRES$ >LEN in >FD log >FD log >FD HOLD @ >FD TEST-FIFO:SPAWN PID>N SERVER !
   in close
   log close
   SERVER @ proc-watch-open WATCH !
   WATCH @ 0 < if ENDED-EARLY then ;

\ The offset of line n, counted from 1, in OUT's first u bytes; -1 when they
\ hold fewer lines.
: LINE-AT ( n n -- n ) {: line:n u:n :}
   line 1 = if 0 exit then
   1 u 0 ?do
      OUT i + c@ NEWLINE = if
         1+ dup line = if drop i 1+ unloop exit then
      then
   loop
   drop -1 ;

\ A pidfile that will not open or is not yet that long is a server not ready
\ yet.
: STATUS-READY? ( -- bool )
   PIDFILE$ FS:>PATHZ open-rd {: fd:n :}
   fd 0 < if false exit then
   fd OUT CAPTURE-CAP read {: got:n :}
   fd close
   got 0 <= if false exit then
   STATUS-LINE got LINE-AT {: at:n :}
   at 0 < if false exit then
   OUT at + got at - s" ready" STARTS-WITH? ;

\ A server that outlives ms is killed with its tree (src/lib/process-tree.f) and
\ leaves its shared-memory segment.
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


\ The engine's stdout and stderr are this process's, so its report reaches the
\ row's log as it runs.
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


\ initdb is watched beside the signal pipe, as the server and the case engine
\ are: its backend holds the cluster's segment while a step runs.
: INITDB-SPAWN ( -- )
   PROC-ARGV-ENV-RESET
   s" -D" ARG+ DATA$ ARG+
   s" -A" ARG+ s" trust" ARG+
   s" -U" ARG+ s" habu" ARG+
   s" -E" ARG+ s" UTF8" ARG+
   s" --no-sync" ARG+
   ENV!
   OPEN-NULL INITDB-LOG$ OPEN-LOG {: in:n log:n :}
   in >FD FD-CLOEXEC!
   log >FD FD-CLOEXEC!
   INITDB$ >LEN in >FD log >FD log >FD HOLD @ >FD TEST-FIFO:SPAWN PID>N INITDB-PID !
   in close
   log close
   INITDB-PID @ proc-watch-open INITDB-WATCH ! ;

: INITDB-REAP ( -- outcome )
   INITDB-PID @ >PID PROC-WAIT-OUTCOME
   NO-PID INITDB-PID !
   INITDB-WATCH CLOSE-WATCH ;

\ initdb catches SIGTERM and exits once its step has ended, its backend and
\ segment gone. One that outlives QUIT-MS is killed with its tree, the backend
\ too, which leaves the segment.
: INITDB-STOP ( -- )
   INITDB-PID @ NO-PID = if exit then
   INITDB-PID @ >PID SIGNAL:SIGTERM PROC-KILL-RAW drop
   INITDB-WATCH @ QUIT-MS GONE-WITHIN? 0= if
      INITDB-PID @ >PID PROC-TREE:KILL-TREE
      OUTCOME:TIMEOUT s" initdb stop" EXITED-0? drop
   then
   INITDB-PID @ >PID PROC-WAIT-STATUS drop
   NO-PID INITDB-PID !
   INITDB-WATCH CLOSE-WATCH ;


\ SIGTERM, SIGINT and SIGHUP are caught as the gate root catches them
\ (src/lib/signal.f CATCH-STOPS); a gate pool sends a row's root SIGTERM before
\ it kills the row's tree (test/gate-pool.f GT-POOL-ASK-END). No Forth word runs
\ in a handler: the signal is a number on a pipe that every wait polls beside
\ its process.
\ SIGKILL cannot be answered: the server runs on under init and both directories
\ stay. Recover with `kill -QUIT` to the pid on line 1 of
\ <root>/data/postmaster.pid, then remove the directories the postmaster's -D
\ and -k name. A SIGKILLed server's segment shows in `ipcs -m -p` with NATTCH 0;
\ line 7 of postmaster.pid names it as `<key> <id>`, and `ipcrm -m <id>` removes
\ it. initdb leads its own process group (src/lib/process.f SPAWN), so it
\ outlives such a SIGKILL, logging to <root>/initdb.log: wait for it or SIGTERM
\ it (it then removes <root>/data), then remove the directories.

: SAY-THROW ( ptr u8 n n -- ) {: what:ptr whatu:n code:n :}
   code 0= if exit then
   s" pg-cluster: " type what whatu type s"  threw " type code FMT:.INT cr ;

\ The server and initdb's backend each make a SysV shared-memory segment, keyed
\ by the data directory's inode, that only their own exit removes. A SIGKILLed
\ one leaks it for good: APFS reuses no inode and kern.sysv.shmmni caps segments
\ host-wide (32 here), and a host out of them starts no PostgreSQL. So a caught
\ signal stops initdb, sends the server its SIGQUIT before the case engine's
\ tree is ended so both shut down at once, and dies of the signal. A step that
\ throws is named and the answer goes on. It never returns: it may run inside
\ SERVE.
: ANSWER ( n -- ) {: sig:n :}
   s" pg-cluster: signal " type sig FMT:.INT s" , stopping the cluster" type cr
   s" initdb stop" [: INITDB-STOP ;] catch SAY-THROW
   SERVER @ NO-PID <> if SERVER @ >PID SIGQUIT PROC-KILL-RAW drop then
   s" case engine kill" [: CASE-KILL ;] catch SAY-THROW
   s" postgres stop" [: QUIT-MS STOP ;] catch SAY-THROW
   s" cleanup" [: FS:CLEANUP-RUN ;] catch SAY-THROW
   s" pg-cluster: signal" sig SIGNAL:DIE-OF ;

: SIGNAL-CHECK ( -- )
   SIGNAL:TAKE MATCH SIGNAL:signal-result
      signal OF ANSWER ENDOF
      timeout OF ENDOF
   ;MATCH ;

\ A run whose first failed step passed its deadline ends with an uncaught
\ E-PROC-TIMEOUT: the engine reports that code on stderr last and exits
\ UNCAUGHT-RC. Any other failure exits 1. A caught signal is answered first.
: FAIL-ROW ( ptr u8 n -- ) {: msg:ptr msgu:n :}
   SIGNAL-CHECK
   LATE @ if E-PROC-TIMEOUT throw then
   msg msgu 1 die ;

\ A caught signal is answered here.
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


: INITDB-RUN ( -- )
   INITDB-SPAWN
   INITDB-WATCH @ TOOL-MS ENDED-WITHIN? if
      INITDB-REAP
   else
      INITDB-STOP OUTCOME:TIMEOUT
   then
   s" initdb" EXITED-0? if exit then
   INITDB-LOG$ SHOW-LOG
   s" pg-cluster: initdb failed" FAIL-ROW ;

: READY? ( -- bool )
   SERVER @ NO-PID = if false exit then
   TOOL-MS >MS PROC-DEADLINE-AT {: deadline:n :}
   begin
      STATUS-READY? if s" pg-cluster: postgres ready" type cr true exit then
      deadline PROC-LEFT-MS MS>N {: left:n :}
      left 0= if
         s" pg-cluster READY?: a deadline expired" type cr
         true STEP-FAILED false exit
      then
      WATCH @ left READY-POLL-MS min ENDED-WITHIN? if
         s" pg-cluster: postgres ended before it was ready" type cr
         false STEP-FAILED false exit
      then
   again ;

\ Each case file runs in a child engine given the conninfo; the first to fail
\ ends the run.
: RUN-FILE ( ptr u8 n -- bool ) {: file:ptr fileu:n :}
   file fileu CASE-SPAWN
   CASE-WATCH @ CASE-MS @ ENDED-WITHIN? if
      CASE-REAP
   else
      CASE-KILL OUTCOME:TIMEOUT
   then
   file fileu EXITED-0? ;

\ The case files are the script arguments, or src/lib/pg-test.f and then
\ src/lib/pg-rows-test.f when there are none.
: CASE-N ( -- n )
   SCRIPT-ARGC 0= if 2 exit then
   SCRIPT-ARGC ;

: CASE$ ( n -- ptr u8 n ) {: i:n :}
   SCRIPT-ARGC 0 > if i SCRIPT-ARGV$ exit then
   i 0= if s" src/lib/pg-test.f" exit then
   s" src/lib/pg-rows-test.f" ;

\ The first file to fail ends the run; the answer is the last file run and
\ whether it passed.
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

\ With PG_HOLD_FIFO set, test/db/pg-kill-test.f watches this row's tree: the
\ FIFO's write end (src/lib/test/fifo.f) goes into initdb and the postmaster,
\ whose forks inherit it; the case engine opens it by path.
: OPEN-HOLD ( -- )
   s" PG_HOLD_FIFO" GETENV nip 0= if exit then
   s" PG_HOLD_FIFO" TEST-FIFO:WRITER FD>N HOLD ! ;

\ The log is shown for a server that never came up, once STOP has ended it:
\ SHOW-LOG's FS:READ-ALL throws E-FS-CAPACITY on a log over CAPTURE-CAP, which a
\ server that ran the cases may write.
: MAIN ( -- )
   OPEN-HOLD
   READ-CASE-MS
   SIGNAL:CATCH-STOPS
   s" initdb" INITDB TOOL INITDB-U !
   s" postgres" POSTGRES TOOL POSTGRES-U !
   MAKE-DIRS TEXTS!
   INITDB-RUN
   SIGNAL-CHECK
   START
   [: SERVE ;] [: END ;] finally {: what:ptr whatu:n passed:bool :}
   SIGNAL-CHECK
   UP @ 0= if LOG$ SHOW-LOG then
   passed STOPPED @ and if exit then
   passed 0= if s" pg-cluster: " type what whatu type s"  failed" type cr then
   s" " FAIL-ROW ;

MAIN

;package
