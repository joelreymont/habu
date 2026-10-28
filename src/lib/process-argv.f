\ process-argv.f - checked argv process helpers.
\
\ STORAGE CLASS. PROCESS-WIDE: the argv table and its row bytes are one set
\ for the image, so a task preparing a spawn through them holds them until the
\ spawn resets them. Both grow with the rows added; a reset keeps their storage
\ for the next command line and an image save releases it. An owned CMD run
\ (src/lib/process-command.f) passes its own vectors and touches none of this.
\
\ ENVIRONMENT. These spawns pass the child no variables. To inherit, prepare an
\ environment (src/lib/process-env.f PROC-ARGV-ENV-RESET ...
\ PROC-ENV-INHERIT-MISSING) and spawn through its *-ARGV-ENV-* words.

s" src/lib/errors.f" required
s" src/lib/memory.f" required
s" src/lib/process.f" required
require src/lib/fd-lock.f
require src/lib/process-chunks.f
require src/lib/string.f
require src/lib/image-lifecycle.f

variable PROC-ARGV-N

\ The argv table holds the address of every zero-terminated argument, so it is
\ declared pointer storage: a pointer stored in or fetched from an undeclared
\ `create`d cell is refused (E-RAW-CELL-PTR).
package PROC-ARGV-ROWS
private

DYNAMIC-BUFFER TABLE ptr u8
DYNAMIC-BUFFER CHUNKS SPAN:span<u8>
PROC-CHUNKS:CONTROL-CELLS TYPED-BUFFER CONTROL n
TYPED-VARIABLE REGISTERED bool
false REGISTERED !

: RELEASE ( -- )
   0 CONTROL [: drop CHUNKS ;] PROC-CHUNKS:FREE
   CHUNKS-RELEASE
   TABLE-RELEASE
   0 >COUNT PROC-ARGV-N !
   false REGISTERED ! ;


\ Register before allocation: refusal cannot leave an untracked mapping.
\ A failed allocation leaves a harmless hook, cleared by the next capture.
: REGISTER ( -- )
   REGISTERED @ if exit then
   [: RELEASE ;] IMAGE-LIFECYCLE:REGISTER
   true REGISTERED ! ;

public

: SLOT ( idx -- ptr ptr u8 )
   {: idx :}
   idx IDX>N 0 < if E-PROC-OUTPUT throw then
   REGISTER
   idx IDX>N 1+ TABLE-RESERVE
   idx IDX>N TABLE ;


: BYTES ( n -- ptr u8 )
   REGISTER
   0 CONTROL [: drop CHUNKS ;] [: drop CHUNKS-RESERVE ;] PROC-CHUNKS:ROOM ;


: REWIND ( -- )
   0 CONTROL PROC-CHUNKS:REWIND ;

;package

: PROC-SPAWN-ARGV-RAW-HELD ( ptr u8 ptr a fd fd fd -- pid )
   {: pathz:ptr argv:ptr infd outfd errfd :}
   pathz argv infd FD>N outfd FD>N errfd FD>N spawn-argv-io >PID ;

: PROC-SPAWN-ARGV-RAW ( ptr u8 ptr a fd fd fd -- pid )
   [: PROC-SPAWN-ARGV-RAW-HELD ;] FD-LOCK:WITH ;

: PROC-ARGV-RESET ( -- )
   0 >COUNT PROC-ARGV-N !
   PROC-ARGV-ROWS:REWIND ;

: PROC-ARGV-SLOT ( idx -- ptr ptr u8 )
   PROC-ARGV-ROWS:SLOT ;

\ A length below zero or one whose NUL would not fit a cell is refused.
: PROC-ARGV+ ( ptr u8 len -- )
   {: a:ptr u :}
   u LEN>N 0 < if E-PROC-OUTPUT throw then
   u LEN>N MEM-MAX-N >= if E-PROC-OUTPUT throw then
   u LEN>N 1+ {: need:n :}
   a u need PROC-ARGV-ROWS:BYTES need >LEN PROC-ZCOPY
   PROC-ARGV-N @ COUNT>N 1+ >IDX PROC-ARGV-SLOT !
   PROC-ARGV-N @ COUNT>N 1+ >COUNT PROC-ARGV-N ! ;

\ Appends exe and the staged arguments as one command line, then `stdin:` and
\ the stdin text when there is any: the program a report names for a rerun.
\ Call it before the spawn, which resets the table.
: PROC-ARGV-PROGRAM+ ( ptr u8 n ptr u8 n ptr u8 n ptr len -- )
   {: exe:ptr exeu:n stdin:ptr stdinu:n dst:ptr cap:n lenp:ptr :}
   exe exeu dst cap lenp BUF-APPEND
   PROC-ARGV-N @ COUNT>N 0 ?do
      STR-SPACE dst cap lenp BUF-APPEND-C
      i 1+ >IDX PROC-ARGV-SLOT @ dup ZLEN dst cap lenp BUF-APPEND
   loop
   stdinu 0 > if
      S\" \nstdin:\n" dst cap lenp BUF-APPEND
      stdin stdinu dst cap lenp BUF-APPEND
   then ;

: PROC-ARGV-PREPARE ( ptr u8 len -- ptr u8 ptr ptr u8 ) {: path:ptr pathu :}
   pathu LEN>N 0 <= if E-PROC-OUTPUT throw then
   path pathu PROC-PATHZ {: pathz:ptr :}
   pathz 0 >IDX PROC-ARGV-SLOT !
   NULL$ drop PROC-ARGV-N @ COUNT>N 1+ >IDX PROC-ARGV-SLOT !
   pathz 0 >IDX PROC-ARGV-SLOT ;

: PROC-SPAWN-ARGV-IO ( ptr u8 len fd fd fd -- pid ) {: a:ptr u infd outfd errfd :}
   a u PROC-ARGV-PREPARE infd outfd errfd PROC-SPAWN-ARGV-RAW {: pid :}
   PROC-ARGV-RESET
   pid PID>N 0 < if E-PROC-SPAWN throw then
   pid ;

: PROC-RUN-ARGV-IO-RC ( ptr u8 len fd fd fd -- result<n,n> )   \ ok = clean exit (0), err = nonzero completion rc
   PROC-SPAWN-ARGV-IO PROC-WAIT-RC ;

: PROC-ARGV-CHECK-PATH ( ptr u8 len -- ) {: path:ptr pathu :}
   pathu LEN>N 0 <= if E-PROC-OUTPUT throw then
   pathu LEN>N PROC-PATHZ-CAP >= if E-PROC-OUTPUT throw then ;

: PROC-SPAWN-ARGV-CAPTURE ( ptr u8 ptr a -- ) {: pathz:ptr argv:ptr :}
   pathz argv PROC-CAPTURE-NULL-INPUT PROC-OUT-W @ >FD PROC-ERR-W @ >FD PROC-SPAWN-ARGV-RAW {: pid :}
   PROC-ARGV-RESET
   pid PID>N 0 < if E-PROC-SPAWN PROC-THROW-CAPTURE then
   pid PROC-CAPTURE-PID!
   PROC-IN-R PROC-CLOSE-CELL
   PROC-OUT-W PROC-CLOSE-CELL
   PROC-ERR-W PROC-CLOSE-CELL ;

: PROC-SPAWN-ARGV-STDIN-CAPTURE ( ptr u8 ptr a -- ) {: pathz:ptr argv:ptr :}
   pathz argv PROC-IN-R @ >FD PROC-OUT-W @ >FD PROC-ERR-W @ >FD PROC-SPAWN-ARGV-RAW {: pid :}
   PROC-ARGV-RESET
   pid PID>N 0 < if E-PROC-SPAWN PROC-THROW-CAPTURE then
   pid PROC-CAPTURE-PID!
   PROC-IN-R PROC-CLOSE-CELL
   PROC-OUT-W PROC-CLOSE-CELL
   PROC-ERR-W PROC-CLOSE-CELL ;

: RUN-ARGV-CAPTURE ( ptr u8 len ptr u8 len ptr u8 len ms -- result<pcap:captured,pcap:failed> )
   {: path:ptr pathu out:ptr outcap err:ptr errcap timeout :}
   path pathu PROC-ARGV-CHECK-PATH
   outcap errcap PROC-CAPTURE-CHECK-CAPS
   path pathu PROC-ARGV-PREPARE {: pathz:ptr argv:ptr :}
   timeout PROC-CAPTURE-BEGIN
   pathz argv PROC-SPAWN-ARGV-CAPTURE
   out outcap err errcap PROC-RUN-CAPTURE-LOOP
   PROC-CAPTURE-FINISH-RC ;

: RUN-ARGV-CAPTURE-OUTCOME ( ptr u8 len ptr u8 len ptr u8 len ms -- len len outcome )
   {: path:ptr pathu out:ptr outcap err:ptr errcap timeout :}
   path pathu PROC-ARGV-CHECK-PATH
   outcap errcap PROC-CAPTURE-CHECK-CAPS
   path pathu PROC-ARGV-PREPARE {: pathz:ptr argv:ptr :}
   timeout PROC-CAPTURE-BEGIN
   pathz argv PROC-SPAWN-ARGV-CAPTURE
   out outcap err errcap PROC-RUN-CAPTURE-OUTCOME-LOOP
   PROC-CAPTURE-FINISH-OUTCOME ;

: RUN-ARGV-STDIN-CAPTURE ( ptr u8 len ptr u8 len ptr u8 len ptr u8 len ms -- result<pcap:captured,pcap:failed> )
   {: path:ptr pathu in:ptr inu out:ptr outcap err:ptr errcap timeout :}
   path pathu PROC-ARGV-CHECK-PATH
   inu PROC-CAPTURE-CHECK-STDIN
   outcap errcap PROC-CAPTURE-CHECK-CAPS
   path pathu PROC-ARGV-PREPARE {: pathz:ptr argv:ptr :}
   timeout PROC-STDIN-CAPTURE-BEGIN
   pathz argv PROC-SPAWN-ARGV-STDIN-CAPTURE
   in inu out outcap err errcap PROC-RUN-STDIN-CAPTURE-LOOP
   PROC-CAPTURE-FINISH-RC ;

: RUN-ARGV-STDIN-CAPTURE-OUTCOME ( ptr u8 len ptr u8 len ptr u8 len ptr u8 len ms -- len len outcome )
   {: path:ptr pathu in:ptr inu out:ptr outcap err:ptr errcap timeout :}
   path pathu PROC-ARGV-CHECK-PATH
   inu PROC-CAPTURE-CHECK-STDIN
   outcap errcap PROC-CAPTURE-CHECK-CAPS
   path pathu PROC-ARGV-PREPARE {: pathz:ptr argv:ptr :}
   timeout PROC-STDIN-CAPTURE-BEGIN
   pathz argv PROC-SPAWN-ARGV-STDIN-CAPTURE
   in inu out outcap err errcap PROC-RUN-STDIN-CAPTURE-OUTCOME-LOOP
   PROC-CAPTURE-FINISH-OUTCOME ;
