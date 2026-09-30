\ pg-hold.f - the case file test/db/pg-kill-test.f has the pg harness run.
\
\ It connects with the conninfo that is its one script argument and reports two
\ lines on the descriptor PG_HOLD_FD names: the directory the server made its
\ socket in, and line 7 of the server's postmaster.pid, `<key> <shmid>`, which
\ names the SysV shared-memory segment the server holds. Then it waits in
\ pg_sleep, with a backend busy on its behalf, until the row is killed. If
\ nothing kills it, the harness's own deadline for a case file does. The
\ descriptor is a pipe write end the test left open across every spawn:
\ nothing here closes it.

require lib/errors.f
require lib/string.f
require lib/adt/option.f
require lib/fs.f
require lib/aio.f
require lib/pg.f

package PG-HOLD

64 constant USAGE-RC
7 constant SHM-LINE                 \ LOCK_FILE_LINE_SHMEM_KEY
$1000 constant PIDFILE-CAP
10 constant NEWLINE

create PIDFILE FS-PATH-CAP allot
create TEXT PIDFILE-CAP allot

: REPORT-FD ( -- n )
   s" PG_HOLD_FD" GETENV STR>NUMBER? MATCH option
      none OF s" pg-hold: PG_HOLD_FD names no descriptor" USAGE-RC die ENDOF
      some OF ENDOF
   ;MATCH ;

: REPORT ( ptr u8 n -- ) {: a:ptr u:n :}
   REPORT-FD {: fd:n :}
   fd a u write u <> if s" pg-hold: report refused" USAGE-RC die then
   fd s\" \n" write 1 <> if s" pg-hold: report refused" USAGE-RC die then ;

\ The offset of line n, counted from 1, in the first u bytes of TEXT; -1 when
\ they hold fewer lines.
: LINE-AT ( n n -- n ) {: line:n u:n :}
   line 1 = if 0 exit then
   1 u 0 ?do
      TEXT i + c@ NEWLINE = if
         1+ dup line = if drop i 1+ unloop exit then
      then
   loop
   drop -1 ;

\ Line n of the first u bytes of TEXT, without its newline.
: LINE$ ( n n -- ptr u8 n ) {: line:n u:n :}
   line u LINE-AT {: at:n :}
   at 0 < if s" pg-hold: postmaster.pid is short" 1 die then
   at begin dup u < if TEXT over + c@ NEWLINE <> else false then while 1+ repeat
   {: stop:n :}
   TEXT at + stop at - ;

using PG

: OPEN ( -- PG:connection )
   1 1 1 CONFIGURE
   0 SCRIPT-ARGV$ CONNECT MATCH PG:connect-result
      connected OF ENDOF
      refused OF type cr s" pg-hold: connect refused" 1 die ENDOF
   ;MATCH ;

\ The one value a one-row, one-column query answers, reported.
: REPORT-SHOW ( PG:connection ptr u8 n -- ) {: c:PG:connection q:ptr qu:n :}
   c q qu EXEC {: r :}
   r 0 >ROW 0 >COL TEXT$ REPORT
   r CLEAR ;

: REPORT-SEGMENT ( PG:connection -- ) {: c:PG:connection :}
   c s" show data_directory" EXEC {: r :}
   r 0 >ROW 0 >COL TEXT$ s" postmaster.pid" PIDFILE JOIN-PATH {: pu:n :}
   r CLEAR
   PIDFILE pu TEXT PIDFILE-CAP READ-ALL {: u:n :}
   SHM-LINE u LINE$ REPORT ;

: MAIN ( -- )
   SCRIPT-ARGC 1 <> if
      s" pg-hold: test/db/pg-cluster.f runs this with a conninfo" USAGE-RC die
   then
   AIO:START
   OPEN {: c :}
   c s" show unix_socket_directories" REPORT-SHOW
   c REPORT-SEGMENT
   c s" select pg_sleep(600)" EXEC CLEAR
   s" pg-hold: the sleep ended before the row was killed" 1 die ;

MAIN

;using
;package
