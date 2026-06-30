\ process-argv.f - checked argv process helpers.
\
\ Load after lib/errors.f, lib/memory.f, and lib/process.f.

s" lib/memory.f" required
s" lib/process.f" required

$100 constant PROC-ARGV-MAX
32768 constant PROC-ARGV-BUF-CAP

create PROC-ARGV-TABLE PROC-ARGV-MAX 1 + cells allot

variable PROC-ARGV-N
variable PROC-ARGV-OFF
variable PROC-ARGV-BUF-A

: PROC-ARGV-BUF-A-FIELD ( -- ptr ptr u8 )
   PROC-ARGV-BUF-A 0 ptr-field ;

: PROC-ARGV-BUF@ ( -- ptr u8 )
   PROC-ARGV-BUF-A-FIELD @ ;

: PROC-ARGV-BUF! ( ptr u8 -- )
   PROC-ARGV-BUF-A-FIELD ! ;

: PROC-ARGV-BUF ( -- ptr u8 )
   PROC-ARGV-BUF@ 0= if
      PROC-ARGV-BUF-CAP MEM-ALLOC-BYTES drop PROC-ARGV-BUF!
   then
   PROC-ARGV-BUF@ ;

: PROC-SPAWN-ARGV-RAW ( ptr u8 ptr a fd fd fd -- pid )
   {: pathz:ptr argv:ptr infd outfd errfd :}
   pathz argv infd FD>N outfd FD>N errfd FD>N spawn-argv-io >PID ;

: PROC-ARGV-RESET ( -- )
   0 >COUNT PROC-ARGV-N !
   0 >OFF PROC-ARGV-OFF ! ;

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
   path pathu PROC-PATHZ {: pathz:ptr :}
   pathz 0 >IDX PROC-ARGV-SLOT !
   0 PROC-ARGV-N @ COUNT>N 1+ >IDX PROC-ARGV-SLOT !
   pathz PROC-ARGV-TABLE ;

: PROC-SPAWN-ARGV-IO ( ptr u8 len fd fd fd -- pid ) {: a:ptr u infd outfd errfd :}
   a u PROC-ARGV-PREPARE infd outfd errfd PROC-SPAWN-ARGV-RAW {: pid :}
   PROC-ARGV-RESET
   pid PID>N 0 < if E-PROC-SPAWN throw then
   pid ;

: PROC-RUN-ARGV-IO-RC ( ptr u8 len fd fd fd -- rc )
   PROC-SPAWN-ARGV-IO PROC-WAIT-RC ;

: PROC-ARGV-CHECK-PATH ( ptr u8 len -- ) {: path:ptr pathu :}
   pathu LEN>N 0 <= if E-PROC-OUTPUT throw then
   pathu LEN>N 1 + PROC-PATHZ-CAP > if E-PROC-OUTPUT throw then ;

: PROC-SPAWN-ARGV-CAPTURE ( ptr u8 ptr a -- ) {: pathz:ptr argv:ptr :}
   pathz argv -1 >FD PROC-OUT-W @ PROC-ERR-W @ PROC-SPAWN-ARGV-RAW {: pid :}
   PROC-ARGV-RESET
   pid PID>N 0 < if E-PROC-SPAWN PROC-THROW-CAPTURE then
   pid PROC-PID !
   PROC-OUT-W PROC-CLOSE-CELL
   PROC-ERR-W PROC-CLOSE-CELL ;

: PROC-SPAWN-ARGV-STDIN-CAPTURE ( ptr u8 ptr a -- ) {: pathz:ptr argv:ptr :}
   pathz argv PROC-IN-R @ PROC-OUT-W @ PROC-ERR-W @ PROC-SPAWN-ARGV-RAW {: pid :}
   PROC-ARGV-RESET
   pid PID>N 0 < if E-PROC-SPAWN PROC-THROW-CAPTURE then
   pid PROC-PID !
   PROC-IN-R PROC-CLOSE-CELL
   PROC-OUT-W PROC-CLOSE-CELL
   PROC-ERR-W PROC-CLOSE-CELL ;

: RUN-ARGV-CAPTURE ( ptr u8 len ptr u8 len ptr u8 len ms -- len len rc )
   {: path:ptr pathu out:ptr outcap err:ptr errcap timeout :}
   path pathu PROC-ARGV-CHECK-PATH
   outcap errcap PROC-CAPTURE-CHECK-CAPS
   path pathu PROC-ARGV-PREPARE {: pathz:ptr argv:ptr :}
   timeout PROC-CAPTURE-BEGIN
   pathz argv PROC-SPAWN-ARGV-CAPTURE
   out outcap err errcap PROC-RUN-CAPTURE-LOOP
   PROC-CAPTURE-FINISH-RC ;

: RUN-ARGV-CAPTURE-OUTCOME ( ptr u8 len ptr u8 len ptr u8 len ms -- len len n n )
   {: path:ptr pathu out:ptr outcap err:ptr errcap timeout :}
   path pathu PROC-ARGV-CHECK-PATH
   outcap errcap PROC-CAPTURE-CHECK-CAPS
   path pathu PROC-ARGV-PREPARE {: pathz:ptr argv:ptr :}
   timeout PROC-CAPTURE-BEGIN
   pathz argv PROC-SPAWN-ARGV-CAPTURE
   out outcap err errcap PROC-RUN-CAPTURE-OUTCOME-LOOP
   PROC-CAPTURE-FINISH-OUTCOME ;

: RUN-ARGV-STDIN-CAPTURE ( ptr u8 len ptr u8 len ptr u8 len ptr u8 len ms -- len len rc )
   {: path:ptr pathu in:ptr inu out:ptr outcap err:ptr errcap timeout :}
   path pathu PROC-ARGV-CHECK-PATH
   inu PROC-CAPTURE-CHECK-STDIN
   outcap errcap PROC-CAPTURE-CHECK-CAPS
   path pathu PROC-ARGV-PREPARE {: pathz:ptr argv:ptr :}
   timeout PROC-STDIN-CAPTURE-BEGIN
   pathz argv PROC-SPAWN-ARGV-STDIN-CAPTURE
   in inu out outcap err errcap PROC-RUN-STDIN-CAPTURE-LOOP
   PROC-CAPTURE-FINISH-RC ;

: RUN-ARGV-STDIN-CAPTURE-OUTCOME ( ptr u8 len ptr u8 len ptr u8 len ptr u8 len ms -- len len n n )
   {: path:ptr pathu in:ptr inu out:ptr outcap err:ptr errcap timeout :}
   path pathu PROC-ARGV-CHECK-PATH
   inu PROC-CAPTURE-CHECK-STDIN
   outcap errcap PROC-CAPTURE-CHECK-CAPS
   path pathu PROC-ARGV-PREPARE {: pathz:ptr argv:ptr :}
   timeout PROC-STDIN-CAPTURE-BEGIN
   pathz argv PROC-SPAWN-ARGV-STDIN-CAPTURE
   in inu out outcap err errcap PROC-RUN-STDIN-CAPTURE-OUTCOME-LOOP
   PROC-CAPTURE-FINISH-OUTCOME ;
