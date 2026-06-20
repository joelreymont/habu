\ process-argv.f - checked argv process helpers.
\
\ Load after lib/errors.f and lib/process.f.

64 constant PROC-ARGV-MAX
32768 constant PROC-ARGV-BUF-CAP

create PROC-ARGV-TABLE PROC-ARGV-MAX 1 + cells allot
create PROC-ARGV-BUF PROC-ARGV-BUF-CAP allot

variable PROC-ARGV-N
variable PROC-ARGV-OFF

: PROC-SPAWN-ARGV-RAW ( ptr u8 ptr a n n n -- n )
   spawn-argv-io ;

: PROC-ARGV-RESET ( -- )
   0 PROC-ARGV-N !
   0 PROC-ARGV-OFF ! ;

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

: PROC-SPAWN-ARGV-CAPTURE ( ptr u8 ptr a -- ) {: pathz:ptr argv:ptr :}
   pathz argv -1 PROC-OUT-W @ PROC-ERR-W @ PROC-SPAWN-ARGV-RAW {: pid :}
   PROC-ARGV-RESET
   pid 0 < if E-PROC-SPAWN PROC-THROW-CAPTURE then
   pid PROC-PID !
   PROC-OUT-W PROC-CLOSE-CELL
   PROC-ERR-W PROC-CLOSE-CELL ;

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
