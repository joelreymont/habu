\ process.f - checked process helpers.
\
\ Load after lib/errors.f.

1024 constant PROC-PATHZ-CAP
1000000 constant PROC-NS-PER-MS
1 constant POLLIN
9 constant SIGKILL
2 constant F-SETFD
1 constant FD-CLOEXEC

create PROC-PATHZ-BUF PROC-PATHZ-CAP allot
create PROC-PFD 16 allot
create PROC-PROBE 1 allot

variable PROC-PID
variable PROC-RC
variable PROC-OUT-R
variable PROC-OUT-W
variable PROC-ERR-R
variable PROC-ERR-W
variable PROC-OUT-LEN
variable PROC-ERR-LEN
variable PROC-DEADLINE
variable PROC-RD

: PROC-WAIT-RAW ( n -- n )
   wait-rc ;

: PROC-SPAWN-RAW ( ptr u8 n n n -- n )
   spawn-io ;

: PROC-KILL-RAW ( n n -- n )
   kill ;

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

: PROC-PFD-SLOT ( n -- ptr a ) {: idx :}
   idx 8 * PROC-PFD + ;

: PROC-PFD-AT! ( n n n -- ) {: fd events idx :}
   events 32 lshift  fd $FFFFFFFF and  or  idx PROC-PFD-SLOT ! ;

: PROC-PFD! ( n n -- ) {: fd events :}
   fd events 0 PROC-PFD-AT! ;

: PROC-PFD-REVENTS ( n -- n )
   PROC-PFD-SLOT @ 48 rshift $FFFF and ;

: POLL-IN ( n n -- n ) {: fd ms :}
   fd POLLIN PROC-PFD!
   PROC-PFD 1 ms poll ;

: POLL-IN-OR-TIMEOUT ( n n -- n )
   POLL-IN {: rc :}
   rc 0 < if E-PROC-OUTPUT throw then
   rc 0= if E-PROC-TIMEOUT throw then
   rc ;

: PROC-CAPTURE-RESET ( -- )
   -1 PROC-PID !
   -1 PROC-RC !
   -1 PROC-OUT-R !
   -1 PROC-OUT-W !
   -1 PROC-ERR-R !
   -1 PROC-ERR-W !
   0 PROC-OUT-LEN !
   0 PROC-ERR-LEN ! ;

: PROC-CLOSE-CELL ( ptr a -- ) {: p:ptr :}
   p @ dup 0 >= if
      close
      -1 p !
   else
      drop
   then ;

: PROC-CLOSE-CAPTURE-FDS ( -- )
   PROC-OUT-R PROC-CLOSE-CELL
   PROC-OUT-W PROC-CLOSE-CELL
   PROC-ERR-R PROC-CLOSE-CELL
   PROC-ERR-W PROC-CLOSE-CELL ;

: PROC-REAP-CAPTURE ( -- )
   PROC-PID @ dup 0 >= if
      WAIT-RC PROC-RC !
      -1 PROC-PID !
   else
      drop
   then ;

: PROC-KILL-CAPTURE ( -- )
   PROC-PID @ dup 0 >= if
      SIGKILL PROC-KILL-RAW drop
      PROC-REAP-CAPTURE
   else
      drop
   then ;

: PROC-THROW-CAPTURE ( n -- ) {: code :}
   PROC-KILL-CAPTURE
   PROC-CLOSE-CAPTURE-FDS
   code throw ;

: PROC-OPEN-PIPE ( ptr a ptr a -- ) {: rp:ptr wp:ptr :}
   pipe {: r w rc :}
   rc 0 <> if E-PROC-OUTPUT PROC-THROW-CAPTURE then
   r rp !
   w wp ! ;

: PROC-CLOEXEC-CELL ( ptr a -- ) {: p:ptr :}
   p @ F-SETFD FD-CLOEXEC fcntl 0 <> if E-PROC-OUTPUT PROC-THROW-CAPTURE then ;

: PROC-SETUP-CAPTURE-FDS ( -- )
   PROC-OUT-R PROC-OUT-W PROC-OPEN-PIPE
   PROC-ERR-R PROC-ERR-W PROC-OPEN-PIPE
   PROC-OUT-R PROC-CLOEXEC-CELL
   PROC-OUT-W PROC-CLOEXEC-CELL
   PROC-ERR-R PROC-CLOEXEC-CELL
   PROC-ERR-W PROC-CLOEXEC-CELL ;

: PROC-CAPTURE-DEADLINE! ( n -- ) {: timeout :}
   timeout 0 < if E-PROC-TIMEOUT throw then
   mono-ns timeout PROC-NS-PER-MS * + PROC-DEADLINE ! ;

: PROC-REMAINING-MS ( -- n )
   PROC-DEADLINE @ mono-ns - dup 0 <= if
      drop 0
   else
      PROC-NS-PER-MS /
   then ;

: PROC-POLL-CAPTURE ( n -- n ) {: ms :}
   PROC-OUT-R @ POLLIN 0 PROC-PFD-AT!
   PROC-ERR-R @ POLLIN 1 PROC-PFD-AT!
   PROC-PFD 2 ms poll {: rc :}
   rc 0 < if E-PROC-OUTPUT PROC-THROW-CAPTURE then
   rc 0= if E-PROC-TIMEOUT PROC-THROW-CAPTURE then
   rc ;

: PROC-READ-STREAM ( ptr a ptr u8 n ptr a -- ) {: fdp:ptr buf:ptr cap lenp:ptr :}
   cap lenp @ - 0 <= if E-PROC-TRUNCATED PROC-THROW-CAPTURE then
   fdp @ buf lenp @ + cap lenp @ - read PROC-RD !
   PROC-RD @ 0 < if E-PROC-OUTPUT PROC-THROW-CAPTURE then
   PROC-RD @ cap lenp @ - > if E-PROC-OUTPUT PROC-THROW-CAPTURE then
   PROC-RD @ 0= if
      fdp PROC-CLOSE-CELL
   else
      lenp @ PROC-RD @ + lenp !
   then ;

: PROC-PROBE-FULL-STREAM ( ptr a -- ) {: fdp:ptr :}
   fdp @ PROC-PROBE 1 read PROC-RD !
   PROC-RD @ 0 < if E-PROC-OUTPUT PROC-THROW-CAPTURE then
   PROC-RD @ 1 > if E-PROC-OUTPUT PROC-THROW-CAPTURE then
   PROC-RD @ 0= if
      fdp PROC-CLOSE-CELL
   else
      E-PROC-TRUNCATED PROC-THROW-CAPTURE
   then ;

: PROC-READ-OR-PROBE-STREAM ( ptr a ptr u8 n ptr a -- ) {: fdp:ptr buf:ptr cap lenp:ptr :}
   cap lenp @ - 0 <= if
      fdp PROC-PROBE-FULL-STREAM
   else
      fdp buf cap lenp PROC-READ-STREAM
   then ;

: PROC-DRAIN-READY ( ptr u8 n ptr u8 n -- ) {: out:ptr outcap err:ptr errcap :}
   0 PROC-PFD-REVENTS 0 <> if
      PROC-OUT-R out outcap PROC-OUT-LEN PROC-READ-OR-PROBE-STREAM
   then
   1 PROC-PFD-REVENTS 0 <> if
      PROC-ERR-R err errcap PROC-ERR-LEN PROC-READ-OR-PROBE-STREAM
   then ;

: PROC-CAPTURE-DONE? ( -- bool )
   PROC-OUT-R @ 0 < PROC-ERR-R @ 0 < and ;

: PROC-RUN-CAPTURE-LOOP ( ptr u8 n ptr u8 n -- ) {: out:ptr outcap err:ptr errcap :}
   begin PROC-CAPTURE-DONE? 0= while
      PROC-REMAINING-MS PROC-POLL-CAPTURE drop
      out outcap err errcap PROC-DRAIN-READY
   repeat ;

: PROC-SPAWN-CAPTURE ( ptr u8 -- )
   -1 PROC-OUT-W @ PROC-ERR-W @ PROC-SPAWN-RAW {: pid :}
   pid 0 < if E-PROC-SPAWN PROC-THROW-CAPTURE then
   pid PROC-PID !
   PROC-OUT-W PROC-CLOSE-CELL
   PROC-ERR-W PROC-CLOSE-CELL ;

: RUN-CAPTURE ( ptr u8 n ptr u8 n ptr u8 n n -- n n n )
   {: path:ptr pathu out:ptr outcap err:ptr errcap timeout :}
   pathu 0 < if E-PROC-OUTPUT throw then
   outcap 0 < if E-PROC-OUTPUT throw then
   errcap 0 < if E-PROC-OUTPUT throw then
   PROC-CAPTURE-RESET
   timeout PROC-CAPTURE-DEADLINE!
   path pathu PATHZ {: pathz:ptr :}
   PROC-SETUP-CAPTURE-FDS
   pathz PROC-SPAWN-CAPTURE
   out outcap err errcap PROC-RUN-CAPTURE-LOOP
   PROC-CLOSE-CAPTURE-FDS
   PROC-REAP-CAPTURE
   PROC-OUT-LEN @ PROC-ERR-LEN @ PROC-RC @ ;
