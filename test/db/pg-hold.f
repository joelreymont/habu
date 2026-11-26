\ pg-hold.f - the case file test/db/pg-kill-test.f has the pg harness run. It
\ connects with the conninfo of its one script argument and writes two lines on
\ the FIFO PG_HOLD_FIFO names: the server's socket directory, and line 7 of its
\ postmaster.pid, `<key> <shmid>`, naming its SysV shared-memory segment. Then
\ it waits in pg_sleep, a backend busy for it, until killed or the harness's
\ case deadline.

require src/core/exit-status.f
require src/lib/error.f
require src/lib/fs.f
require src/lib/aio.f
require src/lib/pg.f
require src/lib/test/fifo.f

package PG-HOLD

7 constant SHM-LINE                 \ LOCK_FILE_LINE_SHMEM_KEY
$1000 constant PIDFILE-CAP
10 constant NEWLINE

create PIDFILE PATH-CAP allot
create TEXT PIDFILE-CAP allot
variable HOLD                       \ the FIFO's write end

: REPORT ( str -- ) {: s:str :}
   HOLD @ {: fd:n :}
   fd s write s STR:LEN <> if s" pg-hold: report failed" EXIT-STATUS:IOERR die then
   fd s\" \n" write 1 <> if s" pg-hold: report failed" EXIT-STATUS:IOERR die then ;

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

: LINE$ ( n n -- str ) {: line:n u:n :}
   line u LINE-AT {: at:n :}
   at 0 < if s" pg-hold: postmaster.pid is short" EXIT-STATUS:FAILURE die then
   at begin dup u < if TEXT over + c@ NEWLINE <> else false then while 1+ repeat
   {: stop:n :}
   TEXT at + stop at - >str ;

: OPEN ( -- PG:connection )
   1 1 PG:CONFIGURE
   0 SCRIPT-ARGV$ s" " PG:CONNECT MATCH PG:connect-result
      connected OF ENDOF
      rejected OF type cr s" pg-hold: connect rejected" EXIT-STATUS:UNAVAILABLE die ENDOF
   ;MATCH ;

\ The one value a one-row, one-column query answers, reported.
: REPORT-SHOW ( PG:connection str -- ) {: c:PG:connection q:str :}
   c q [: 0 PG:>ROW 0 PG:>COL PG:TEXT$ REPORT ;] PG:WITH-RESULT ;

: REPORT-SEGMENT ( PG:connection -- ) {: c:PG:connection :}
   c s" show data_directory"
   [: 0 PG:>ROW 0 PG:>COL PG:TEXT$ s" postmaster.pid" PIDFILE FS:JOIN-PATH swap ;] PG:WITH-RESULT
   {: pu:n :}
   PIDFILE pu >str TEXT PIDFILE-CAP FS:READ-ALL {: u:n :}
   SHM-LINE u LINE$ REPORT ;

\ It opens the FIFO's write end first (src/lib/test/fifo.f), never closed.
: MAIN ( -- )
   s" PG_HOLD_FIFO" TEST-FIFO:WRITER FD>N HOLD !
   SCRIPT-ARGC 1 <> if
      s" pg-hold: test/db/pg-cluster.f runs this with a conninfo" EXIT-STATUS:USAGE die
   then
   AIO:START
   OPEN {: c :}
   c s" show unix_socket_directories" REPORT-SHOW
   c REPORT-SEGMENT
   c s" select pg_sleep(600)" [: ;] PG:WITH-RESULT
   s" pg-hold: the sleep ended before the row was killed" EXIT-STATUS:FAILURE die ;

MAIN

;package
