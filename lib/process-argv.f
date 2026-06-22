\ process-argv.f - checked argv process helpers.
\
\ Load after lib/errors.f and lib/process.f.

64 constant PROC-ARGV-MAX
32768 constant PROC-ARGV-BUF-CAP
4 constant POLLOUT
8 constant POLLERR
16 constant POLLHUP
32 constant POLLNVAL
3 constant F-GETFL
4 constant F-SETFL
4 constant O-NONBLOCK
4096 constant PROC-ARGV-STDIN-CHUNK-CAP

create PROC-ARGV-TABLE PROC-ARGV-MAX 1 + cells allot
create PROC-ARGV-BUF PROC-ARGV-BUF-CAP allot
create PROC-ARGV-PFD 24 allot

variable PROC-ARGV-N
variable PROC-ARGV-OFF
variable PROC-ARGV-IN-R
variable PROC-ARGV-IN-W
variable PROC-ARGV-IN-OFF

: PROC-SPAWN-ARGV-RAW ( ptr u8 ptr a fd fd fd -- pid )
   {: pathz:ptr argv:ptr infd outfd errfd :}
   pathz argv infd FD>N outfd FD>N errfd FD>N spawn-argv-io >PID ;

: PROC-ARGV-RESET ( -- )
   0 >COUNT PROC-ARGV-N !
   0 >OFF PROC-ARGV-OFF ! ;

: PROC-ARGV-CAPTURE-RESET ( -- )
   PROC-CAPTURE-RESET
   -1 >FD PROC-ARGV-IN-R !
   -1 >FD PROC-ARGV-IN-W !
   0 >OFF PROC-ARGV-IN-OFF ! ;

: PROC-ARGV-TRUE ( -- bool )
   0 0= ;

: PROC-ARGV-FALSE ( -- bool )
   0 0= 0= ;

: PROC-ARGV-SLOT ( idx -- ptr a ) {: idx :}
   idx IDX>N 0 < if E-PROC-OUTPUT throw then
   idx IDX>N PROC-ARGV-MAX > if E-PROC-OUTPUT throw then
   idx IDX>N cells PROC-ARGV-TABLE + ;

: PROC-ARGV-CHECK-EXTRA ( -- )
   PROC-ARGV-N @ COUNT>N PROC-ARGV-MAX 1- >= if E-PROC-OUTPUT throw then ;

: PROC-ARGV-ZCOPY ( ptr u8 len -- ptr u8 ) {: a:ptr u :}
   u LEN>N 0 < if E-PROC-OUTPUT throw then
   PROC-ARGV-OFF @ {: off :}
   off OFF>N u LEN>N 1 + + PROC-ARGV-BUF-CAP > if E-PROC-OUTPUT throw then
   a u PROC-ARGV-BUF off OFF>N + PROC-ARGV-BUF-CAP off OFF>N - >LEN PROC-ZCOPY {: z:ptr :}
   off OFF>N u LEN>N 1 + + >OFF PROC-ARGV-OFF !
   z ;

: PROC-ARGV+ ( ptr u8 len -- ) {: a:ptr u :}
   PROC-ARGV-CHECK-EXTRA
   a u PROC-ARGV-ZCOPY
   PROC-ARGV-N @ COUNT>N 1+ >IDX PROC-ARGV-SLOT !
   PROC-ARGV-N @ COUNT>N 1+ >COUNT PROC-ARGV-N ! ;

: PROC-ARGV-PREPARE ( ptr u8 len -- ptr u8 ptr a ) {: path:ptr pathu :}
   pathu LEN>N 0 <= if E-PROC-OUTPUT throw then
   path pathu PATHZ {: pathz:ptr :}
   pathz 0 >IDX PROC-ARGV-SLOT !
   0 PROC-ARGV-N @ COUNT>N 1+ >IDX PROC-ARGV-SLOT !
   pathz PROC-ARGV-TABLE ;

: SPAWN-ARGV-IO ( ptr u8 len fd fd fd -- pid ) {: a:ptr u infd outfd errfd :}
   a u PROC-ARGV-PREPARE infd outfd errfd PROC-SPAWN-ARGV-RAW {: pid :}
   PROC-ARGV-RESET
   pid PID>N 0 < if E-PROC-SPAWN throw then
   pid ;

: RUN-ARGV-IO-RC ( ptr u8 len fd fd fd -- rc )
   SPAWN-ARGV-IO WAIT-RC ;

: PROC-ARGV-CHECK-PATH ( ptr u8 len -- ) {: path:ptr pathu :}
   pathu LEN>N 0 <= if E-PROC-OUTPUT throw then
   pathu LEN>N 1 + PROC-PATHZ-CAP > if E-PROC-OUTPUT throw then ;

: PROC-ARGV-CLOSE-STDIN-FDS ( -- )
   PROC-ARGV-IN-R PROC-CLOSE-CELL
   PROC-ARGV-IN-W PROC-CLOSE-CELL ;

: PROC-ARGV-THROW-CAPTURE ( n -- ) {: code :}
   PROC-ARGV-CLOSE-STDIN-FDS
   code PROC-THROW-CAPTURE ;

: PROC-ARGV-CLOEXEC-CELL ( ptr a -- ) {: p:ptr :}
   p @ F-SETFD FD-CLOEXEC fcntl 0 <> if E-PROC-OUTPUT PROC-ARGV-THROW-CAPTURE then ;

: PROC-ARGV-NONBLOCK! ( fd -- ) {: fd :}
   fd FD>N F-GETFL 0 fcntl {: flags :}
   flags 0 < if E-PROC-OUTPUT PROC-ARGV-THROW-CAPTURE then
   fd FD>N F-SETFL flags O-NONBLOCK or fcntl 0 <> if E-PROC-OUTPUT PROC-ARGV-THROW-CAPTURE then ;

: PROC-ARGV-NOSIGPIPE! ( fd -- ) {: fd :}
   fd FD>N F-SETNOSIGPIPE 1 fcntl 0 <> if E-PROC-OUTPUT PROC-ARGV-THROW-CAPTURE then ;

: PROC-ARGV-SETUP-STDIN-FDS ( -- )
   PROC-ARGV-IN-R PROC-ARGV-IN-W PROC-OPEN-PIPE
   PROC-ARGV-IN-R PROC-ARGV-CLOEXEC-CELL
   PROC-ARGV-IN-W PROC-ARGV-CLOEXEC-CELL
   PROC-ARGV-IN-W @ PROC-ARGV-NOSIGPIPE!
   PROC-ARGV-IN-W @ PROC-ARGV-NONBLOCK! ;

: PROC-SPAWN-ARGV-CAPTURE ( ptr u8 ptr a -- ) {: pathz:ptr argv:ptr :}
   pathz argv -1 >FD PROC-OUT-W @ PROC-ERR-W @ PROC-SPAWN-ARGV-RAW {: pid :}
   PROC-ARGV-RESET
   pid PID>N 0 < if E-PROC-SPAWN PROC-THROW-CAPTURE then
   pid PROC-PID !
   PROC-OUT-W PROC-CLOSE-CELL
   PROC-ERR-W PROC-CLOSE-CELL ;

: PROC-SPAWN-ARGV-STDIN-CAPTURE ( ptr u8 ptr a -- ) {: pathz:ptr argv:ptr :}
   pathz argv PROC-ARGV-IN-R @ PROC-OUT-W @ PROC-ERR-W @ PROC-SPAWN-ARGV-RAW {: pid :}
   PROC-ARGV-RESET
   pid PID>N 0 < if E-PROC-SPAWN PROC-ARGV-THROW-CAPTURE then
   pid PROC-PID !
   PROC-ARGV-IN-R PROC-CLOSE-CELL
   PROC-OUT-W PROC-CLOSE-CELL
   PROC-ERR-W PROC-CLOSE-CELL ;

: PROC-ARGV-PFD-SLOT ( idx -- ptr a ) {: idx :}
   idx IDX>N 8 * PROC-ARGV-PFD + ;

: PROC-ARGV-PFD-AT! ( fd n idx -- ) {: fd events idx :}
   events 32 lshift  fd FD>N $FFFFFFFF and  or  idx PROC-ARGV-PFD-SLOT ! ;

: PROC-ARGV-PFD-REVENTS ( idx -- n )
   PROC-ARGV-PFD-SLOT @ 48 rshift $FFFF and ;

: PROC-ARGV-READ-STREAM ( ptr fd ptr u8 len ptr len -- ) {: fdp:ptr buf:ptr cap lenp:ptr :}
   cap LEN>N lenp @ LEN>N - 0 <= if E-PROC-TRUNCATED PROC-ARGV-THROW-CAPTURE then
   fdp @ FD>N buf lenp @ LEN>N + cap LEN>N lenp @ LEN>N - read PROC-RD !
   PROC-RD @ 0 < if E-PROC-OUTPUT PROC-ARGV-THROW-CAPTURE then
   PROC-RD @ cap LEN>N lenp @ LEN>N - > if E-PROC-OUTPUT PROC-ARGV-THROW-CAPTURE then
   PROC-RD @ 0= if
      fdp PROC-CLOSE-CELL
   else
      lenp @ LEN>N PROC-RD @ + >LEN lenp !
   then ;

: PROC-ARGV-PROBE-FULL-STREAM ( ptr fd -- ) {: fdp:ptr :}
   fdp @ FD>N PROC-PROBE 1 read PROC-RD !
   PROC-RD @ 0 < if E-PROC-OUTPUT PROC-ARGV-THROW-CAPTURE then
   PROC-RD @ 1 > if E-PROC-OUTPUT PROC-ARGV-THROW-CAPTURE then
   PROC-RD @ 0= if
      fdp PROC-CLOSE-CELL
   else
      E-PROC-TRUNCATED PROC-ARGV-THROW-CAPTURE
   then ;

: PROC-ARGV-READ-OR-PROBE-STREAM ( ptr fd ptr u8 len ptr len -- ) {: fdp:ptr buf:ptr cap lenp:ptr :}
   cap LEN>N lenp @ LEN>N - 0 <= if
      fdp PROC-ARGV-PROBE-FULL-STREAM
   else
      fdp buf cap lenp PROC-ARGV-READ-STREAM
   then ;

: PROC-ARGV-DRAIN-READY ( ptr u8 len ptr u8 len -- ) {: out:ptr outcap err:ptr errcap :}
   0 >IDX PROC-ARGV-PFD-REVENTS 0 <> if
      PROC-OUT-R out outcap PROC-OUT-LEN PROC-ARGV-READ-OR-PROBE-STREAM
   then
   1 >IDX PROC-ARGV-PFD-REVENTS 0 <> if
      PROC-ERR-R err errcap PROC-ERR-LEN PROC-ARGV-READ-OR-PROBE-STREAM
   then ;

: PROC-ARGV-STDIN-CHUNK ( len -- len ) {: u :}
   u LEN>N PROC-ARGV-STDIN-CHUNK-CAP > if
      PROC-ARGV-STDIN-CHUNK-CAP >LEN
   else
      u
   then ;

: PROC-ARGV-CLOSE-STDIN-DONE ( len -- ) {: inu :}
   PROC-ARGV-IN-OFF @ OFF>N inu LEN>N >= if PROC-ARGV-IN-W PROC-CLOSE-CELL then ;

: PROC-ARGV-BROKEN-STDIN? ( n -- bool ) {: events :}
   events POLLERR and 0= 0= if PROC-ARGV-TRUE exit then
   events POLLHUP and 0= 0= if PROC-ARGV-TRUE exit then
   events POLLNVAL and 0= 0= if PROC-ARGV-TRUE exit then
   PROC-ARGV-FALSE ;

: PROC-ARGV-WRITE-STDIN-ACTIVE ( ptr u8 len -- ) {: src:ptr inu :}
   inu LEN>N PROC-ARGV-IN-OFF @ OFF>N - >LEN PROC-ARGV-STDIN-CHUNK {: chunk :}
   PROC-ARGV-IN-W @ FD>N src PROC-ARGV-IN-OFF @ OFF>N + chunk LEN>N write {: wrote :}
   wrote 0 < if PROC-ARGV-IN-W PROC-CLOSE-CELL exit then
   wrote chunk LEN>N > if E-PROC-OUTPUT PROC-ARGV-THROW-CAPTURE then
   PROC-ARGV-IN-OFF @ OFF>N wrote + >OFF PROC-ARGV-IN-OFF !
   inu PROC-ARGV-CLOSE-STDIN-DONE ;

: PROC-ARGV-WRITE-STDIN ( ptr u8 len -- ) {: src:ptr inu :}
   PROC-ARGV-IN-W @ FD>N 0 < if exit then
   PROC-ARGV-IN-OFF @ OFF>N inu LEN>N >= if PROC-ARGV-IN-W PROC-CLOSE-CELL exit then
   src inu PROC-ARGV-WRITE-STDIN-ACTIVE ;

: PROC-ARGV-POLL-IO ( ms -- count ) {: ms :}
   PROC-OUT-R @ POLLIN 0 >IDX PROC-ARGV-PFD-AT!
   PROC-ERR-R @ POLLIN 1 >IDX PROC-ARGV-PFD-AT!
   PROC-ARGV-IN-W @ FD>N 0 >= if
      PROC-ARGV-IN-W @ POLLOUT 2 >IDX PROC-ARGV-PFD-AT!
   else
      -1 >FD 0 2 >IDX PROC-ARGV-PFD-AT!
   then
   PROC-ARGV-PFD 3 ms MS>N poll {: rc :}
   rc 0 < if E-PROC-OUTPUT PROC-ARGV-THROW-CAPTURE then
   rc 0= if E-PROC-TIMEOUT PROC-ARGV-THROW-CAPTURE then
   rc >COUNT ;

: PROC-ARGV-POLL-IO-OUTCOME ( ms -- count ) {: ms :}
   PROC-OUT-R @ POLLIN 0 >IDX PROC-ARGV-PFD-AT!
   PROC-ERR-R @ POLLIN 1 >IDX PROC-ARGV-PFD-AT!
   PROC-ARGV-IN-W @ FD>N 0 >= if
      PROC-ARGV-IN-W @ POLLOUT 2 >IDX PROC-ARGV-PFD-AT!
   else
      -1 >FD 0 2 >IDX PROC-ARGV-PFD-AT!
   then
   PROC-ARGV-PFD 3 ms MS>N poll {: rc :}
   rc 0 < if E-PROC-OUTPUT PROC-ARGV-THROW-CAPTURE then
   rc >COUNT ;

: PROC-ARGV-DRIVE-STDIN ( ptr u8 len -- ) {: in:ptr inu :}
   2 >IDX PROC-ARGV-PFD-REVENTS {: events :}
   events PROC-ARGV-BROKEN-STDIN? if PROC-ARGV-IN-W PROC-CLOSE-CELL exit then
   events POLLOUT and 0= 0= if
      in inu PROC-ARGV-WRITE-STDIN
   then ;

: PROC-ARGV-STDIN-CAPTURE-DONE? ( -- bool )
   PROC-CAPTURE-DONE? PROC-ARGV-IN-W @ 0 < and ;

: PROC-RUN-STDIN-CAPTURE-LOOP ( ptr u8 len ptr u8 len ptr u8 len -- )
   {: in:ptr inu out:ptr outcap err:ptr errcap :}
   inu LEN>N 0 <= if PROC-ARGV-IN-W PROC-CLOSE-CELL then
   begin PROC-ARGV-STDIN-CAPTURE-DONE? 0= while
      PROC-REMAINING-MS PROC-ARGV-POLL-IO drop
      in inu PROC-ARGV-DRIVE-STDIN
      out outcap err errcap PROC-ARGV-DRAIN-READY
   repeat ;

: PROC-RUN-STDIN-CAPTURE-OUTCOME-LOOP ( ptr u8 len ptr u8 len ptr u8 len -- )
   {: in:ptr inu out:ptr outcap err:ptr errcap :}
   inu LEN>N 0 <= if PROC-ARGV-IN-W PROC-CLOSE-CELL then
   begin PROC-ARGV-STDIN-CAPTURE-DONE? 0= while
      PROC-REMAINING-MS PROC-ARGV-POLL-IO-OUTCOME dup COUNT>N 0= if
         drop
         PROC-ARGV-CLOSE-STDIN-FDS
         PROC-REAP-CAPTURE-TIMEOUT
         exit
      then
      drop
      in inu PROC-ARGV-DRIVE-STDIN
      out outcap err errcap PROC-ARGV-DRAIN-READY
   repeat
   PROC-REAP-CAPTURE ;

: RUN-ARGV-CAPTURE ( ptr u8 len ptr u8 len ptr u8 len ms -- len len rc )
   {: path:ptr pathu out:ptr outcap err:ptr errcap timeout :}
   path pathu PROC-ARGV-CHECK-PATH
   outcap LEN>N 0 < if E-PROC-OUTPUT throw then
   errcap LEN>N 0 < if E-PROC-OUTPUT throw then
   PROC-CAPTURE-RESET
   timeout PROC-CAPTURE-DEADLINE!
   PROC-SETUP-CAPTURE-FDS
   path pathu PROC-ARGV-PREPARE PROC-SPAWN-ARGV-CAPTURE
   out outcap err errcap PROC-RUN-CAPTURE-LOOP
   PROC-CLOSE-CAPTURE-FDS
   PROC-REAP-CAPTURE
   PROC-OUT-LEN @ PROC-ERR-LEN @ PROC-RC @ ;

: RUN-ARGV-CAPTURE-OUTCOME ( ptr u8 len ptr u8 len ptr u8 len ms -- len len n n )
   {: path:ptr pathu out:ptr outcap err:ptr errcap timeout :}
   path pathu PROC-ARGV-CHECK-PATH
   outcap LEN>N 0 < if E-PROC-OUTPUT throw then
   errcap LEN>N 0 < if E-PROC-OUTPUT throw then
   PROC-CAPTURE-RESET
   timeout PROC-CAPTURE-DEADLINE!
   PROC-SETUP-CAPTURE-FDS
   path pathu PROC-ARGV-PREPARE PROC-SPAWN-ARGV-CAPTURE
   out outcap err errcap PROC-RUN-CAPTURE-OUTCOME-LOOP
   PROC-CLOSE-CAPTURE-FDS
   PROC-OUT-LEN @ PROC-ERR-LEN @ PROC-OUTCOME-KIND @ PROC-OUTCOME-CODE @ ;

: RUN-ARGV-STDIN-CAPTURE ( ptr u8 len ptr u8 len ptr u8 len ptr u8 len ms -- len len rc )
   {: path:ptr pathu in:ptr inu out:ptr outcap err:ptr errcap timeout :}
   path pathu PROC-ARGV-CHECK-PATH
   inu LEN>N 0 < if E-PROC-OUTPUT throw then
   outcap LEN>N 0 < if E-PROC-OUTPUT throw then
   errcap LEN>N 0 < if E-PROC-OUTPUT throw then
   PROC-ARGV-CAPTURE-RESET
   timeout PROC-CAPTURE-DEADLINE!
   PROC-SETUP-CAPTURE-FDS
   PROC-ARGV-SETUP-STDIN-FDS
   path pathu PROC-ARGV-PREPARE PROC-SPAWN-ARGV-STDIN-CAPTURE
   in inu out outcap err errcap PROC-RUN-STDIN-CAPTURE-LOOP
   PROC-ARGV-CLOSE-STDIN-FDS
   PROC-CLOSE-CAPTURE-FDS
   PROC-REAP-CAPTURE
   PROC-OUT-LEN @ PROC-ERR-LEN @ PROC-RC @ ;

: RUN-ARGV-STDIN-CAPTURE-OUTCOME ( ptr u8 len ptr u8 len ptr u8 len ptr u8 len ms -- len len n n )
   {: path:ptr pathu in:ptr inu out:ptr outcap err:ptr errcap timeout :}
   path pathu PROC-ARGV-CHECK-PATH
   inu LEN>N 0 < if E-PROC-OUTPUT throw then
   outcap LEN>N 0 < if E-PROC-OUTPUT throw then
   errcap LEN>N 0 < if E-PROC-OUTPUT throw then
   PROC-ARGV-CAPTURE-RESET
   timeout PROC-CAPTURE-DEADLINE!
   PROC-SETUP-CAPTURE-FDS
   PROC-ARGV-SETUP-STDIN-FDS
   path pathu PROC-ARGV-PREPARE PROC-SPAWN-ARGV-STDIN-CAPTURE
   in inu out outcap err errcap PROC-RUN-STDIN-CAPTURE-OUTCOME-LOOP
   PROC-ARGV-CLOSE-STDIN-FDS
   PROC-CLOSE-CAPTURE-FDS
   PROC-OUT-LEN @ PROC-ERR-LEN @ PROC-OUTCOME-KIND @ PROC-OUTCOME-CODE @ ;
