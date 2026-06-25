\ process.f - checked process helpers.
\
\ Load after lib/errors.f.

1024 constant PROC-PATHZ-CAP
1000000 constant PROC-NS-PER-MS
1 constant POLLIN
9 constant SIGKILL
2 constant F-SETFD
73 constant F-SETNOSIGPIPE
1 constant FD-CLOEXEC
$7F constant PROC-WAIT-TERM-MASK
$FF constant PROC-WAIT-EXIT-MASK
0 constant PROC-OUTCOME-EXIT
1 constant PROC-OUTCOME-SIGNAL
2 constant PROC-OUTCOME-TIMEOUT

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
variable PROC-STATUS
variable PROC-OUTCOME-KIND
variable PROC-OUTCOME-CODE

: PROC-WAIT-RAW ( pid -- rc ) {: pid :}
   pid PID>N wait-rc >RC ;

: PROC-WAIT-STATUS-RAW ( pid -- n ) {: pid :}
   pid PID>N wait-status ;

: PROC-SPAWN-RAW ( ptr u8 fd fd fd -- pid ) {: pathz:ptr infd outfd errfd :}
   pathz infd FD>N outfd FD>N errfd FD>N spawn-io >PID ;

: PROC-KILL-RAW ( pid n -- rc ) {: pid sig :}
   pid PID>N sig kill >RC ;

: PROC-ZCOPY ( ptr u8 len ptr u8 len -- ptr u8 ) {: a:ptr u dst:ptr cap :}
   u LEN>N 1 + cap LEN>N > if E-PROC-OUTPUT throw then
   0 begin dup u LEN>N < while
      dup a + c@  over dst + c!
      1 +
   repeat drop
   0 dst u LEN>N + c!
   dst ;

: PATHZ ( ptr u8 len -- ptr u8 )
   PROC-PATHZ-BUF PROC-PATHZ-CAP >LEN PROC-ZCOPY ;

: WAIT-STATUS ( pid -- n )
   PROC-WAIT-STATUS-RAW {: status :}
   status 0 < if E-PROC-WAIT throw then
   status ;

: PROC-STATUS>OUTCOME ( n -- n n ) {: status :}
   status PROC-WAIT-TERM-MASK and {: term :}
   term 0= if
      PROC-OUTCOME-EXIT status 8 rshift PROC-WAIT-EXIT-MASK and
      exit
   then
   PROC-OUTCOME-SIGNAL term ;

: PROC-OUTCOME>RC ( n n -- rc ) {: kind code :}
   kind PROC-OUTCOME-EXIT = if code >RC exit then
   128 code + >RC ;

: PROC-STATUS>RC ( n -- rc )
   PROC-STATUS>OUTCOME PROC-OUTCOME>RC ;

: WAIT-OUTCOME ( pid -- n n )
   WAIT-STATUS PROC-STATUS>OUTCOME ;

: WAIT-RC ( pid -- rc )
   WAIT-STATUS PROC-STATUS>RC ;

: SPAWN-IO ( ptr u8 len fd fd fd -- pid ) {: a:ptr u infd outfd errfd :}
   a u PATHZ infd outfd errfd PROC-SPAWN-RAW {: pid :}
   pid PID>N 0 < if E-PROC-SPAWN throw then
   pid ;

: RUN-RC ( ptr u8 len -- rc )
   -1 >FD -1 >FD -1 >FD SPAWN-IO WAIT-RC ;

: RUN-IO-RC ( ptr u8 len fd fd fd -- rc )
   SPAWN-IO WAIT-RC ;

: FD-CLOEXEC! ( fd -- ) {: fd :}
   fd FD>N F-SETFD FD-CLOEXEC fcntl 0 <> if E-PROC-OUTPUT throw then ;

: FD-NOSIGPIPE! ( fd -- ) {: fd :}
   fd FD>N F-SETNOSIGPIPE 1 fcntl 0 <> if E-PROC-OUTPUT throw then ;

: PIPE-PAIR ( -- fd fd )
   pipe {: r w rc :}
   rc 0 <> if E-PROC-OUTPUT throw then
   r >FD w >FD ;

: PROC-PFD-SLOT ( idx -- ptr a ) {: idx :}
   idx IDX>N 8 * PROC-PFD + ;

: PROC-PFD-AT! ( fd n idx -- ) {: fd events idx :}
   events 32 lshift  fd FD>N $FFFFFFFF and  or  idx PROC-PFD-SLOT ! ;

: PROC-PFD! ( fd n -- ) {: fd events :}
   fd events 0 >IDX PROC-PFD-AT! ;

: PROC-PFD-REVENTS ( idx -- n )
   PROC-PFD-SLOT @ 48 rshift $FFFF and ;

: POLL-IN ( fd ms -- count ) {: fd ms :}
   fd POLLIN PROC-PFD!
   PROC-PFD 1 ms MS>N poll >COUNT ;

: POLL-IN-OR-TIMEOUT ( fd ms -- count )
   POLL-IN {: rc :}
   rc COUNT>N 0 < if E-PROC-OUTPUT throw then
   rc COUNT>N 0= if E-PROC-TIMEOUT throw then
   rc ;

: PROC-CAPTURE-RESET ( -- )
   -1 >PID PROC-PID !
   -1 >RC PROC-RC !
   -1 >FD PROC-OUT-R !
   -1 >FD PROC-OUT-W !
   -1 >FD PROC-ERR-R !
   -1 >FD PROC-ERR-W !
   0 >LEN PROC-OUT-LEN !
   0 >LEN PROC-ERR-LEN !
   0 PROC-STATUS !
   PROC-OUTCOME-EXIT PROC-OUTCOME-KIND !
   0 PROC-OUTCOME-CODE ! ;

: PROC-CLOSE-CELL ( ptr fd -- ) {: p:ptr :}
   p @ dup FD>N 0 >= if
      FD>N close
      -1 >FD p !
   else
      drop
   then ;

: PROC-CLOSE-CAPTURE-FDS ( -- )
   PROC-OUT-R PROC-CLOSE-CELL
   PROC-OUT-W PROC-CLOSE-CELL
   PROC-ERR-R PROC-CLOSE-CELL
   PROC-ERR-W PROC-CLOSE-CELL ;

: PROC-REAP-CAPTURE ( -- )
   PROC-PID @ dup PID>N 0 >= if
      WAIT-STATUS dup PROC-STATUS !
      dup PROC-STATUS>RC PROC-RC !
      PROC-STATUS>OUTCOME PROC-OUTCOME-CODE ! PROC-OUTCOME-KIND !
      -1 >PID PROC-PID !
   else
      drop
   then ;

: PROC-REAP-CAPTURE-TIMEOUT ( -- )
   PROC-PID @ dup PID>N 0 >= if
      dup SIGKILL PROC-KILL-RAW drop
      WAIT-STATUS PROC-STATUS !
      -1 >PID PROC-PID !
   else
      drop
   then
   PROC-OUTCOME-TIMEOUT PROC-OUTCOME-KIND !
   SIGKILL PROC-OUTCOME-CODE !
   128 SIGKILL + >RC PROC-RC ! ;

: PROC-KILL-CAPTURE ( -- )
   PROC-PID @ dup PID>N 0 >= if
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
   r >FD rp !
   w >FD wp ! ;

: PROC-CLOEXEC-CELL ( ptr a -- ) {: p:ptr :}
   p @ FD>N F-SETFD FD-CLOEXEC fcntl 0 <> if E-PROC-OUTPUT PROC-THROW-CAPTURE then ;

: PROC-SETUP-CAPTURE-FDS ( -- )
   PROC-OUT-R PROC-OUT-W PROC-OPEN-PIPE
   PROC-ERR-R PROC-ERR-W PROC-OPEN-PIPE
   PROC-OUT-R PROC-CLOEXEC-CELL
   PROC-OUT-W PROC-CLOEXEC-CELL
   PROC-ERR-R PROC-CLOEXEC-CELL
   PROC-ERR-W PROC-CLOEXEC-CELL ;

: PROC-CAPTURE-DEADLINE! ( ms -- ) {: timeout :}
   timeout MS>N 0 < if E-PROC-TIMEOUT throw then
   mono-ns timeout MS>N PROC-NS-PER-MS * + >NS PROC-DEADLINE ! ;

: PROC-REMAINING-MS ( -- ms )
   PROC-DEADLINE @ NS>N mono-ns - dup 0 <= if
      drop 0 >MS
   else
      PROC-NS-PER-MS / >MS
   then ;

: PROC-POLL-CAPTURE ( ms -- count ) {: ms :}
   PROC-OUT-R @ POLLIN 0 >IDX PROC-PFD-AT!
   PROC-ERR-R @ POLLIN 1 >IDX PROC-PFD-AT!
   PROC-PFD 2 ms MS>N poll {: rc :}
   rc 0 < if E-PROC-OUTPUT PROC-THROW-CAPTURE then
   rc 0= if E-PROC-TIMEOUT PROC-THROW-CAPTURE then
   rc >COUNT ;

: PROC-POLL-CAPTURE-OUTCOME ( ms -- count ) {: ms :}
   PROC-OUT-R @ POLLIN 0 >IDX PROC-PFD-AT!
   PROC-ERR-R @ POLLIN 1 >IDX PROC-PFD-AT!
   PROC-PFD 2 ms MS>N poll {: rc :}
   rc 0 < if E-PROC-OUTPUT PROC-THROW-CAPTURE then
   rc >COUNT ;

: PROC-READ-STREAM ( ptr fd ptr u8 len ptr len -- ) {: fdp:ptr buf:ptr cap lenp:ptr :}
   lenp @ LEN>N 0 < if E-PROC-TRUNCATED PROC-THROW-CAPTURE then
   lenp @ LEN>N cap LEN>N > if E-PROC-TRUNCATED PROC-THROW-CAPTURE then
   cap LEN>N lenp @ LEN>N - 0 <= if E-PROC-TRUNCATED PROC-THROW-CAPTURE then
   fdp @ FD>N buf lenp @ LEN>N + cap LEN>N lenp @ LEN>N - read PROC-RD !
   PROC-RD @ 0 < if E-PROC-OUTPUT PROC-THROW-CAPTURE then
   PROC-RD @ cap LEN>N lenp @ LEN>N - > if E-PROC-OUTPUT PROC-THROW-CAPTURE then
   PROC-RD @ 0= if
      fdp PROC-CLOSE-CELL
   else
      lenp @ LEN>N PROC-RD @ + >LEN lenp !
   then ;

: PROC-PROBE-FULL-STREAM ( ptr fd -- ) {: fdp:ptr :}
   fdp @ FD>N PROC-PROBE 1 read PROC-RD !
   PROC-RD @ 0 < if E-PROC-OUTPUT PROC-THROW-CAPTURE then
   PROC-RD @ 1 > if E-PROC-OUTPUT PROC-THROW-CAPTURE then
   PROC-RD @ 0= if
      fdp PROC-CLOSE-CELL
   else
      E-PROC-TRUNCATED PROC-THROW-CAPTURE
   then ;

: PROC-READ-OR-PROBE-STREAM ( ptr fd ptr u8 len ptr len -- ) {: fdp:ptr buf:ptr cap lenp:ptr :}
   cap LEN>N lenp @ LEN>N - 0 <= if
      fdp PROC-PROBE-FULL-STREAM
   else
      fdp buf cap lenp PROC-READ-STREAM
   then ;

: PROC-DRAIN-READY ( ptr u8 len ptr u8 len -- ) {: out:ptr outcap err:ptr errcap :}
   0 >IDX PROC-PFD-REVENTS 0 <> if
      PROC-OUT-R out outcap PROC-OUT-LEN PROC-READ-OR-PROBE-STREAM
   then
   1 >IDX PROC-PFD-REVENTS 0 <> if
      PROC-ERR-R err errcap PROC-ERR-LEN PROC-READ-OR-PROBE-STREAM
   then ;

: PROC-CAPTURE-DONE? ( -- bool )
   PROC-OUT-R @ 0 < PROC-ERR-R @ 0 < and ;

: PROC-RUN-CAPTURE-LOOP ( ptr u8 len ptr u8 len -- ) {: out:ptr outcap err:ptr errcap :}
   begin PROC-CAPTURE-DONE? 0= while
      PROC-REMAINING-MS PROC-POLL-CAPTURE drop
      out outcap err errcap PROC-DRAIN-READY
   repeat ;

: PROC-RUN-CAPTURE-OUTCOME-LOOP ( ptr u8 len ptr u8 len -- ) {: out:ptr outcap err:ptr errcap :}
   begin PROC-CAPTURE-DONE? 0= while
      PROC-REMAINING-MS PROC-POLL-CAPTURE-OUTCOME dup COUNT>N 0= if
         drop
         PROC-REAP-CAPTURE-TIMEOUT
         exit
      then
      drop
      out outcap err errcap PROC-DRAIN-READY
   repeat
   PROC-REAP-CAPTURE ;

: PROC-SPAWN-CAPTURE ( ptr u8 -- )
   -1 >FD PROC-OUT-W @ PROC-ERR-W @ PROC-SPAWN-RAW {: pid :}
   pid PID>N 0 < if E-PROC-SPAWN PROC-THROW-CAPTURE then
   pid PROC-PID !
   PROC-OUT-W PROC-CLOSE-CELL
   PROC-ERR-W PROC-CLOSE-CELL ;

: RUN-CAPTURE ( ptr u8 len ptr u8 len ptr u8 len ms -- len len rc )
   {: path:ptr pathu out:ptr outcap err:ptr errcap timeout :}
   pathu LEN>N 0 < if E-PROC-OUTPUT throw then
   outcap LEN>N 0 < if E-PROC-OUTPUT throw then
   errcap LEN>N 0 < if E-PROC-OUTPUT throw then
   PROC-CAPTURE-RESET
   timeout PROC-CAPTURE-DEADLINE!
   path pathu PATHZ {: pathz:ptr :}
   PROC-SETUP-CAPTURE-FDS
   pathz PROC-SPAWN-CAPTURE
   out outcap err errcap PROC-RUN-CAPTURE-LOOP
   PROC-CLOSE-CAPTURE-FDS
   PROC-REAP-CAPTURE
   PROC-OUT-LEN @ PROC-ERR-LEN @ PROC-RC @ ;

: RUN-CAPTURE-OUTCOME ( ptr u8 len ptr u8 len ptr u8 len ms -- len len n n )
   {: path:ptr pathu out:ptr outcap err:ptr errcap timeout :}
   pathu LEN>N 0 < if E-PROC-OUTPUT throw then
   outcap LEN>N 0 < if E-PROC-OUTPUT throw then
   errcap LEN>N 0 < if E-PROC-OUTPUT throw then
   PROC-CAPTURE-RESET
   timeout PROC-CAPTURE-DEADLINE!
   path pathu PATHZ {: pathz:ptr :}
   PROC-SETUP-CAPTURE-FDS
   pathz PROC-SPAWN-CAPTURE
   out outcap err errcap PROC-RUN-CAPTURE-OUTCOME-LOOP
   PROC-CLOSE-CAPTURE-FDS
   PROC-OUT-LEN @ PROC-ERR-LEN @ PROC-OUTCOME-KIND @ PROC-OUTCOME-CODE @ ;
