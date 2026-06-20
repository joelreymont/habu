\ process-argv.f - checked argv process helpers.
\
\ Load after lib/errors.f and lib/process.f.

64 constant PROC-ARGV-MAX
32768 constant PROC-ARGV-BUF-CAP
4 constant POLLOUT
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

: PROC-SPAWN-ARGV-RAW ( ptr u8 ptr a n n n -- n )
   spawn-argv-io ;

: PROC-ARGV-RESET ( -- )
   0 PROC-ARGV-N !
   0 PROC-ARGV-OFF ! ;

: PROC-ARGV-CAPTURE-RESET ( -- )
   PROC-CAPTURE-RESET
   -1 PROC-ARGV-IN-R !
   -1 PROC-ARGV-IN-W !
   0 PROC-ARGV-IN-OFF ! ;

: PROC-ARGV-SLOT ( n -- ptr a ) {: idx :}
   idx 0 < if E-PROC-OUTPUT throw then
   idx PROC-ARGV-MAX > if E-PROC-OUTPUT throw then
   idx cells PROC-ARGV-TABLE + ;

: PROC-ARGV-CHECK-EXTRA ( -- )
   PROC-ARGV-N @ PROC-ARGV-MAX 1- >= if E-PROC-OUTPUT throw then ;

: PROC-ARGV-ZCOPY ( ptr u8 n -- ptr u8 ) {: a:ptr u :}
   u 0 < if E-PROC-OUTPUT throw then
   PROC-ARGV-OFF @ {: off :}
   off u 1 + + PROC-ARGV-BUF-CAP > if E-PROC-OUTPUT throw then
   a u PROC-ARGV-BUF off + PROC-ARGV-BUF-CAP off - PROC-ZCOPY {: z:ptr :}
   off u 1 + + PROC-ARGV-OFF !
   z ;

: PROC-ARGV+ ( ptr u8 n -- ) {: a:ptr u :}
   PROC-ARGV-CHECK-EXTRA
   a u PROC-ARGV-ZCOPY
   PROC-ARGV-N @ 1+ PROC-ARGV-SLOT !
   PROC-ARGV-N @ 1+ PROC-ARGV-N ! ;

: PROC-ARGV-PREPARE ( ptr u8 n -- ptr u8 ptr a ) {: path:ptr pathu :}
   pathu 0 <= if E-PROC-OUTPUT throw then
   path pathu PATHZ {: pathz:ptr :}
   pathz 0 PROC-ARGV-SLOT !
   0 PROC-ARGV-N @ 1+ PROC-ARGV-SLOT !
   pathz PROC-ARGV-TABLE ;

: SPAWN-ARGV-IO ( ptr u8 n n n n -- n ) {: a:ptr u infd outfd errfd :}
   a u PROC-ARGV-PREPARE infd outfd errfd PROC-SPAWN-ARGV-RAW {: pid :}
   PROC-ARGV-RESET
   pid 0 < if E-PROC-SPAWN throw then
   pid ;

: RUN-ARGV-IO-RC ( ptr u8 n n n n -- n )
   SPAWN-ARGV-IO WAIT-RC ;

: PROC-ARGV-CHECK-PATH ( ptr u8 n -- ) {: path:ptr pathu :}
   pathu 0 <= if E-PROC-OUTPUT throw then
   pathu 1 + PROC-PATHZ-CAP > if E-PROC-OUTPUT throw then ;

: PROC-ARGV-CLOSE-STDIN-FDS ( -- )
   PROC-ARGV-IN-R PROC-CLOSE-CELL
   PROC-ARGV-IN-W PROC-CLOSE-CELL ;

: PROC-ARGV-THROW-CAPTURE ( n -- ) {: code :}
   PROC-ARGV-CLOSE-STDIN-FDS
   code PROC-THROW-CAPTURE ;

: PROC-ARGV-CLOEXEC-CELL ( ptr a -- ) {: p:ptr :}
   p @ F-SETFD FD-CLOEXEC fcntl 0 <> if E-PROC-OUTPUT PROC-ARGV-THROW-CAPTURE then ;

: PROC-ARGV-NONBLOCK! ( n -- ) {: fd :}
   fd F-GETFL 0 fcntl {: flags :}
   flags 0 < if E-PROC-OUTPUT PROC-ARGV-THROW-CAPTURE then
   fd F-SETFL flags O-NONBLOCK or fcntl 0 <> if E-PROC-OUTPUT PROC-ARGV-THROW-CAPTURE then ;

: PROC-ARGV-SETUP-STDIN-FDS ( -- )
   PROC-ARGV-IN-R PROC-ARGV-IN-W PROC-OPEN-PIPE
   PROC-ARGV-IN-R PROC-ARGV-CLOEXEC-CELL
   PROC-ARGV-IN-W PROC-ARGV-CLOEXEC-CELL
   PROC-ARGV-IN-W @ PROC-ARGV-NONBLOCK! ;

: PROC-SPAWN-ARGV-CAPTURE ( ptr u8 ptr a -- ) {: pathz:ptr argv:ptr :}
   pathz argv -1 PROC-OUT-W @ PROC-ERR-W @ PROC-SPAWN-ARGV-RAW {: pid :}
   PROC-ARGV-RESET
   pid 0 < if E-PROC-SPAWN PROC-THROW-CAPTURE then
   pid PROC-PID !
   PROC-OUT-W PROC-CLOSE-CELL
   PROC-ERR-W PROC-CLOSE-CELL ;

: PROC-SPAWN-ARGV-STDIN-CAPTURE ( ptr u8 ptr a -- ) {: pathz:ptr argv:ptr :}
   pathz argv PROC-ARGV-IN-R @ PROC-OUT-W @ PROC-ERR-W @ PROC-SPAWN-ARGV-RAW {: pid :}
   PROC-ARGV-RESET
   pid 0 < if E-PROC-SPAWN PROC-ARGV-THROW-CAPTURE then
   pid PROC-PID !
   PROC-ARGV-IN-R PROC-CLOSE-CELL
   PROC-OUT-W PROC-CLOSE-CELL
   PROC-ERR-W PROC-CLOSE-CELL ;

: PROC-ARGV-PFD-SLOT ( n -- ptr a ) {: idx :}
   idx 8 * PROC-ARGV-PFD + ;

: PROC-ARGV-PFD-AT! ( n n n -- ) {: fd events idx :}
   events 32 lshift  fd $FFFFFFFF and  or  idx PROC-ARGV-PFD-SLOT ! ;

: PROC-ARGV-PFD-REVENTS ( n -- n )
   PROC-ARGV-PFD-SLOT @ 48 rshift $FFFF and ;

: PROC-ARGV-READ-STREAM ( ptr a ptr u8 n ptr a -- ) {: fdp:ptr buf:ptr cap lenp:ptr :}
   cap lenp @ - 0 <= if E-PROC-TRUNCATED PROC-ARGV-THROW-CAPTURE then
   fdp @ buf lenp @ + cap lenp @ - read PROC-RD !
   PROC-RD @ 0 < if E-PROC-OUTPUT PROC-ARGV-THROW-CAPTURE then
   PROC-RD @ cap lenp @ - > if E-PROC-OUTPUT PROC-ARGV-THROW-CAPTURE then
   PROC-RD @ 0= if
      fdp PROC-CLOSE-CELL
   else
      lenp @ PROC-RD @ + lenp !
   then ;

: PROC-ARGV-PROBE-FULL-STREAM ( ptr a -- ) {: fdp:ptr :}
   fdp @ PROC-PROBE 1 read PROC-RD !
   PROC-RD @ 0 < if E-PROC-OUTPUT PROC-ARGV-THROW-CAPTURE then
   PROC-RD @ 1 > if E-PROC-OUTPUT PROC-ARGV-THROW-CAPTURE then
   PROC-RD @ 0= if
      fdp PROC-CLOSE-CELL
   else
      E-PROC-TRUNCATED PROC-ARGV-THROW-CAPTURE
   then ;

: PROC-ARGV-READ-OR-PROBE-STREAM ( ptr a ptr u8 n ptr a -- ) {: fdp:ptr buf:ptr cap lenp:ptr :}
   cap lenp @ - 0 <= if
      fdp PROC-ARGV-PROBE-FULL-STREAM
   else
      fdp buf cap lenp PROC-ARGV-READ-STREAM
   then ;

: PROC-ARGV-DRAIN-READY ( ptr u8 n ptr u8 n -- ) {: out:ptr outcap err:ptr errcap :}
   0 PROC-ARGV-PFD-REVENTS 0 <> if
      PROC-OUT-R out outcap PROC-OUT-LEN PROC-ARGV-READ-OR-PROBE-STREAM
   then
   1 PROC-ARGV-PFD-REVENTS 0 <> if
      PROC-ERR-R err errcap PROC-ERR-LEN PROC-ARGV-READ-OR-PROBE-STREAM
   then ;

: PROC-ARGV-STDIN-CHUNK ( n -- n ) {: u :}
   u PROC-ARGV-STDIN-CHUNK-CAP > if
      PROC-ARGV-STDIN-CHUNK-CAP
   else
      u
   then ;

: PROC-ARGV-CLOSE-STDIN-DONE ( n -- ) {: inu :}
   PROC-ARGV-IN-OFF @ inu >= if PROC-ARGV-IN-W PROC-CLOSE-CELL then ;

: PROC-ARGV-WRITE-STDIN-ACTIVE ( ptr u8 n -- ) {: src:ptr inu :}
   inu PROC-ARGV-IN-OFF @ - PROC-ARGV-STDIN-CHUNK {: chunk :}
   PROC-ARGV-IN-W @ src PROC-ARGV-IN-OFF @ + chunk write {: wrote :}
   wrote chunk <> if E-PROC-OUTPUT PROC-ARGV-THROW-CAPTURE then
   PROC-ARGV-IN-OFF @ wrote + PROC-ARGV-IN-OFF !
   inu PROC-ARGV-CLOSE-STDIN-DONE ;

: PROC-ARGV-WRITE-STDIN ( ptr u8 n -- ) {: src:ptr inu :}
   PROC-ARGV-IN-W @ 0 < if exit then
   PROC-ARGV-IN-OFF @ inu >= if PROC-ARGV-IN-W PROC-CLOSE-CELL exit then
   src inu PROC-ARGV-WRITE-STDIN-ACTIVE ;

: PROC-ARGV-POLL-IO ( n -- n ) {: ms :}
   PROC-OUT-R @ POLLIN 0 PROC-ARGV-PFD-AT!
   PROC-ERR-R @ POLLIN 1 PROC-ARGV-PFD-AT!
   PROC-ARGV-IN-W @ 0 >= if
      PROC-ARGV-IN-W @ POLLOUT 2 PROC-ARGV-PFD-AT!
   else
      -1 0 2 PROC-ARGV-PFD-AT!
   then
   PROC-ARGV-PFD 3 ms poll {: rc :}
   rc 0 < if E-PROC-OUTPUT PROC-ARGV-THROW-CAPTURE then
   rc 0= if E-PROC-TIMEOUT PROC-ARGV-THROW-CAPTURE then
   rc ;

: PROC-ARGV-DRIVE-STDIN ( ptr u8 n -- ) {: in:ptr inu :}
   2 PROC-ARGV-PFD-REVENTS 0 <> if
      in inu PROC-ARGV-WRITE-STDIN
   then ;

: PROC-ARGV-STDIN-CAPTURE-DONE? ( -- bool )
   PROC-CAPTURE-DONE? PROC-ARGV-IN-W @ 0 < and ;

: PROC-RUN-STDIN-CAPTURE-LOOP ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: in:ptr inu out:ptr outcap err:ptr errcap :}
   inu 0 <= if PROC-ARGV-IN-W PROC-CLOSE-CELL then
   begin PROC-ARGV-STDIN-CAPTURE-DONE? 0= while
      PROC-REMAINING-MS PROC-ARGV-POLL-IO drop
      in inu PROC-ARGV-DRIVE-STDIN
      out outcap err errcap PROC-ARGV-DRAIN-READY
   repeat ;

: RUN-ARGV-CAPTURE ( ptr u8 n ptr u8 n ptr u8 n n -- n n n )
   {: path:ptr pathu out:ptr outcap err:ptr errcap timeout :}
   path pathu PROC-ARGV-CHECK-PATH
   outcap 0 < if E-PROC-OUTPUT throw then
   errcap 0 < if E-PROC-OUTPUT throw then
   PROC-CAPTURE-RESET
   timeout PROC-CAPTURE-DEADLINE!
   PROC-SETUP-CAPTURE-FDS
   path pathu PROC-ARGV-PREPARE PROC-SPAWN-ARGV-CAPTURE
   out outcap err errcap PROC-RUN-CAPTURE-LOOP
   PROC-CLOSE-CAPTURE-FDS
   PROC-REAP-CAPTURE
   PROC-OUT-LEN @ PROC-ERR-LEN @ PROC-RC @ ;

: RUN-ARGV-CAPTURE-OUTCOME ( ptr u8 n ptr u8 n ptr u8 n n -- n n n n )
   {: path:ptr pathu out:ptr outcap err:ptr errcap timeout :}
   path pathu PROC-ARGV-CHECK-PATH
   outcap 0 < if E-PROC-OUTPUT throw then
   errcap 0 < if E-PROC-OUTPUT throw then
   PROC-CAPTURE-RESET
   timeout PROC-CAPTURE-DEADLINE!
   PROC-SETUP-CAPTURE-FDS
   path pathu PROC-ARGV-PREPARE PROC-SPAWN-ARGV-CAPTURE
   out outcap err errcap PROC-RUN-CAPTURE-OUTCOME-LOOP
   PROC-CLOSE-CAPTURE-FDS
   PROC-OUT-LEN @ PROC-ERR-LEN @ PROC-OUTCOME-KIND @ PROC-OUTCOME-CODE @ ;

: RUN-ARGV-STDIN-CAPTURE ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n n -- n n n )
   {: path:ptr pathu in:ptr inu out:ptr outcap err:ptr errcap timeout :}
   path pathu PROC-ARGV-CHECK-PATH
   inu 0 < if E-PROC-OUTPUT throw then
   outcap 0 < if E-PROC-OUTPUT throw then
   errcap 0 < if E-PROC-OUTPUT throw then
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
