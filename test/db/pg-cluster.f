\ pg-cluster.f - lib/pg-test.f against a private PostgreSQL cluster.
\
\     bin/hb --load test/db/pg-cluster.f
\
\ initdb makes a trust-authentication cluster under HB_TMP and pg_ctl starts it
\ listening on a Unix-domain socket only (listen_addresses=''), so rows running
\ beside each other have no TCP port to collide on. The cases run in a child
\ engine that takes the conninfo as its one script argument. This process stops
\ the cluster however that child ends - exit, failed assertion, die, crash or
\ its deadline - and the cleanup registry (lib/fs-mutate.f CLEANUP-TREE+)
\ removes both directories when this process exits, including by die.
\
\ A SIGNAL THAT ENDS THIS PROCESS STOPS NOTHING. The engine runs the exit hook
\ only on its own way out, so after a SIGTERM or SIGKILL the postmaster keeps
\ running and both directories stay (measured: SIGTERM mid-run, rc 143). Recover
\ with `pg_ctl -D <root>/data -m immediate stop`, then remove the two
\ directories; the postmaster's command line names both (-D and -k). Under the
\ gate pool, retiring the slot removes HB_TMP and the data directory with it,
\ and the postmaster's lock-file recheck then stops it within about a minute;
\ the socket directory under TMPDIR stays.
\
\ THE SOCKET DIRECTORY IS NOT UNDER HB_TMP. sun_path holds 104 bytes on macOS
\ and 108 on Linux, and a pool slot's HB_TMP takes about 100 of them: a socket
\ there makes postgres refuse to start with `Unix-domain socket path ... is too
\ long (maximum 103 bytes)`. It takes the short base TMPDIR-MKDIR gives, as
\ lib/fs-mutate-test.f FMT-ROOT! does for its socket.
\
\ initdb and pg_ctl on PATH, with postgres beside pg_ctl, are a gate requirement
\ on every host (docs/bootstrap.md "Requirements"). A missing one ends the row
\ naming it, never a skip.
require lib/errors.f
require lib/string.f
require lib/fmt.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/engine-candidate.f

package PG-CLUSTER

$10000 constant CAPTURE-CAP     \ initdb's chatter or the cases' whole report
\ The step deadlines sum to 300 s, inside the row's 360 s
\ (test/gate-stdlib-lib.f SUITE-TIMEOUT-MS), so this process and not the pool
\ is the one that gives up and stops the cluster. pg_ctl's own wait (-t) ends
\ before its capture deadline does.
60000 constant TOOL-MS
120000 constant CASES-MS
: PG-CTL-WAIT$ ( -- ptr u8 n ) s" 50" ;

FS-PATH-CAP BUFFER: INITDB      variable INITDB-U
FS-PATH-CAP BUFFER: PG-CTL      variable PG-CTL-U
FS-PATH-CAP BUFFER: ROOT        variable ROOT-U
FS-PATH-CAP BUFFER: SOCKETS     variable SOCKETS-U
FS-PATH-CAP BUFFER: DATA        variable DATA-U
FS-PATH-CAP BUFFER: LOG         variable LOG-U
FS-PATH-CAP BUFFER: PIDFILE     variable PIDFILE-U
FS-PATH-CAP $40 + constant TEXT-CAP
TEXT-CAP BUFFER: OPTIONS        variable OPTIONS-U
TEXT-CAP BUFFER: CONNINFO       variable CONNINFO-U
CAPTURE-CAP BUFFER: OUT
CAPTURE-CAP BUFFER: ERR

: INITDB$ ( -- ptr u8 n )    INITDB INITDB-U @ ;
: PG-CTL$ ( -- ptr u8 n )    PG-CTL PG-CTL-U @ ;
: ROOT$ ( -- ptr u8 n )      ROOT ROOT-U @ ;
: SOCKETS$ ( -- ptr u8 n )   SOCKETS SOCKETS-U @ ;
: DATA$ ( -- ptr u8 n )      DATA DATA-U @ ;
: LOG$ ( -- ptr u8 n )       LOG LOG-U @ ;
: PIDFILE$ ( -- ptr u8 n )   PIDFILE PIDFILE-U @ ;
: OPTIONS$ ( -- ptr u8 n )   OPTIONS OPTIONS-U @ ;
: CONNINFO$ ( -- ptr u8 n )  CONNINFO CONNINFO-U @ ;
: OPTIONS+ ( ptr u8 n -- )   OPTIONS TEXT-CAP OPTIONS-U BUF-APPEND ;
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

\ A made directory's name lives in the per-task band the next MAKE-TEMP-DIR
\ reuses, so each is copied out before the other is made.
: MAKE-DIRS ( -- )
   s" habu-pg" HB-TMP-MKDIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY  u ROOT-U !
   ROOT$ CLEANUP-TREE+
   s" habu-pg" TMPDIR-MKDIR {: b:ptr v:n :}
   b SOCKETS v BYTE-COPY  v SOCKETS-U !
   SOCKETS$ CLEANUP-TREE+
   ROOT$ s" data" DATA JOIN-PATH DATA-U !
   ROOT$ s" postgres.log" LOG JOIN-PATH LOG-U !
   DATA$ s" postmaster.pid" PIDFILE JOIN-PATH PIDFILE-U ! ;

\ pg_ctl hands -o to /bin/sh, and libpq splits a conninfo at spaces, so both
\ quote the directory.
: TEXTS! ( -- )
   OPTIONS-U BUF-RESET
   s" -k '" OPTIONS+ SOCKETS$ OPTIONS+ s" ' -c listen_addresses=''" OPTIONS+
   CONNINFO-U BUF-RESET
   s" host='" CONNINFO+ SOCKETS$ CONNINFO+
   s" ' dbname=postgres user=habu connect_timeout=5" CONNINFO+ ;


\ ---- one step at a time ----------------------------------------------------
\ Every step inherits this process's environment with LC_ALL=C: the cluster's
\ locale is C whatever the host's, and on macOS a postmaster started without a
\ valid locale in its environment stops with `postmaster became multithreaded
\ during startup` (measured with the plain RUN-ARGV-CAPTURE-OUTCOME, which
\ passes an empty environment).
: STEP ( ptr u8 n n -- len len outcome ) {: path:ptr pathu:n deadline:n :}
   PROC-ENV-INHERIT-MISSING
   s" LC_ALL" >LEN s" C" >LEN PROC-ENV-SET
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

\ postmaster.pid is the server's own mark that it may be running: a start that
\ never got that far leaves nothing to stop.
: STOP ( -- )
   PIDFILE$ EXISTS? 0= if exit then
   PROC-ARGV-ENV-RESET
   s" -D" ARG+ DATA$ ARG+
   s" -m" ARG+ s" immediate" ARG+
   s" -w" ARG+ s" -t" ARG+ PG-CTL-WAIT$ ARG+ s" -s" ARG+
   s" stop" ARG+
   PG-CTL$ TOOL-MS STEP s" pg_ctl stop" EXITED-0? if 2drop exit then
   SHOW
   s" pg-cluster: pg_ctl stop failed" 1 die ;

: START ( -- )
   PROC-ARGV-ENV-RESET
   s" -D" ARG+ DATA$ ARG+
   s" -l" ARG+ LOG$ ARG+
   s" -o" ARG+ OPTIONS$ ARG+
   s" -w" ARG+ s" -t" ARG+ PG-CTL-WAIT$ ARG+ s" -s" ARG+
   s" start" ARG+
   PG-CTL$ TOOL-MS STEP s" pg_ctl start" EXITED-0? if 2drop exit then
   \ SHOW prints the start's capture before STOP's reuses OUT, and STOP runs
   \ before SHOW-LOG, whose READ-ALL throws E-FS-CAPACITY on a log over
   \ CAPTURE-CAP. The log is outside the data directory and survives the stop.
   SHOW
   STOP
   SHOW-LOG
   s" pg-cluster: pg_ctl start failed" 1 die ;

\ The child's report is shown whatever its outcome, so a red row carries it.
: CASES ( -- bool )
   PROC-ARGV-ENV-RESET
   s" --load" ARG+ s" lib/pg-test.f" ARG+
   s" --" ARG+ CONNINFO$ ARG+
   ENGINE-CANDIDATE:PATH$ CASES-MS STEP {: o :}
   SHOW
   o s" lib/pg-test.f" EXITED-0? ;

: MAIN ( -- )
   s" initdb" INITDB TOOL INITDB-U !
   s" pg_ctl" PG-CTL TOOL PG-CTL-U !
   MAKE-DIRS TEXTS!
   INITDB-RUN
   START
   [: CASES ;] [: STOP ;] finally
   0= if s" pg-cluster: lib/pg-test.f failed" 1 die then ;

MAIN

;package
