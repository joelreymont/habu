\ process.f - checked process helpers.
\
\ Load after lib/errors.f.

1024 constant PROC-PATHZ-CAP
1 constant POLLIN
2 constant F-SETFD
1 constant FD-CLOEXEC

create PROC-PATHZ-BUF PROC-PATHZ-CAP allot
create PROC-PFD 8 allot

: PROC-WAIT-RAW ( n -- n )
   wait-rc ;

: PROC-SPAWN-RAW ( ptr u8 n n n -- n )
   spawn-io ;

: PROC-ZCOPY ( ptr u8 n ptr u8 n -- ptr u8 ) {: a:ptr u dst:ptr cap :}
   u 1 + cap > if E-PROC-OUTPUT throw then
   0 begin dup u < while
      dup a + c@  over dst + c!
      1 +
   repeat drop
   0 dst u + c!
   dst ;

: PATHZ ( ptr u8 n -- ptr u8 )
   PROC-PATHZ-BUF PROC-PATHZ-CAP PROC-ZCOPY ;

: WAIT-RC ( n -- n )
   PROC-WAIT-RAW {: rc :}
   rc 0 < if E-PROC-WAIT throw then
   rc ;

: SPAWN-IO ( ptr u8 n n n n -- n ) {: a:ptr u infd outfd errfd :}
   a u PATHZ infd outfd errfd PROC-SPAWN-RAW {: pid :}
   pid 0 < if E-PROC-SPAWN throw then
   pid ;

: RUN-RC ( ptr u8 n -- n )
   -1 -1 -1 SPAWN-IO WAIT-RC ;

: FD-CLOEXEC! ( n -- ) {: fd :}
   fd F-SETFD FD-CLOEXEC fcntl 0 <> if E-PROC-OUTPUT throw then ;

: PIPE-PAIR ( -- n n )
   pipe {: r w rc :}
   rc 0 <> if E-PROC-OUTPUT throw then
   r w ;

: PROC-PFD! ( n n -- ) {: fd events :}
   events 32 lshift  fd $FFFFFFFF and  or  PROC-PFD ! ;

: POLL-IN ( n n -- n ) {: fd ms :}
   fd POLLIN PROC-PFD!
   PROC-PFD 1 ms poll ;

: POLL-IN-OR-TIMEOUT ( n n -- n )
   POLL-IN {: rc :}
   rc 0 < if E-PROC-OUTPUT throw then
   rc 0= if E-PROC-TIMEOUT throw then
   rc ;
