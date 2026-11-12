\ process-argv.f - checked argv process helpers; how they spawn is src/lib/process.f
\ "SPAWN". These spawns pass the child no variables: to inherit, prepare an
\ environment (src/lib/process-env.f) and spawn through its *-ARGV-ENV-* words.
\ STORAGE CLASS. PROCESS-WIDE: one argv table and row bytes for the image, held by
\ the task preparing a spawn until the spawn resets them; a reset keeps the storage.
\ It is scratch, so a build leaves it out. An owned CMD run (process-command.f)
\ touches none of it.

s" src/lib/error.f" required
s" src/lib/memory.f" required
s" src/lib/process.f" required
require src/lib/process-chunks.f
require src/lib/string.f

SCRATCH-VARIABLE PROC-ARGV-N

\ The argv table holds the address of every zero-terminated argument, so it is
\ declared pointer storage: a pointer stored in or fetched from an undeclared
\ `create`d cell is refused (E-RAW-CELL-PTR).
package PROC-ARGV-ROWS
private

DYNAMIC-BUFFER TABLE ptr u8
DYNAMIC-BUFFER CHUNKS SPAN:span<u8>
create CONTROL PROC-CHUNKS:CONTROL-CELLS cells SCRATCH-ALLOT

public

: SLOT ( idx -- ptr ptr u8 )
   {: idx :}
   idx IDX>N 0 < if E-PROC-OUTPUT throw then
   idx IDX>N 1+ TABLE-RESERVE
   idx IDX>N TABLE ;


: BYTES ( n -- ptr u8 )
   CONTROL [: drop CHUNKS ;] [: drop CHUNKS-RESERVE ;] PROC-CHUNKS:ROOM ;


: REWIND ( -- )
   CONTROL PROC-CHUNKS:REWIND ;

;package

package PROC-RAW
public

: SPAWN-ARGV ( ptr u8 ptr a fd fd fd -- ptr u8 ptr a fd fd fd )
   {: pathz:ptr argv:ptr infd outfd errfd :}
   pathz dup ZLEN PROCESS:NEW PROCESS:HERMETIC
   argv ROWS PROCESS:ARGV-ROWS
   infd outfd errfd LAUNCH
   pathz argv infd outfd errfd ;

;package

: PROC-SPAWN-ARGV-RAW ( ptr u8 ptr a fd fd fd -- pid )
   [: PROC-RAW:SPAWN-ARGV ;] catch {: rc :}
   drop drop drop drop drop
   rc PROC-RAW:ANSWER ;

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
   a u need PROC-ARGV-ROWS:BYTES need >LEN PROCESS:ZCOPY
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
   path pathu PROCESS:PATHZ {: pathz:ptr :}
   pathz 0 >IDX PROC-ARGV-SLOT !
   NULL$ drop PROC-ARGV-N @ COUNT>N 1+ >IDX PROC-ARGV-SLOT !
   pathz 0 >IDX PROC-ARGV-SLOT ;

: PROC-SPAWN-ARGV-IO ( ptr u8 len fd fd fd -- pid ) {: a:ptr u infd outfd errfd :}
   a u PROC-ARGV-PREPARE infd outfd errfd PROC-SPAWN-ARGV-RAW {: pid :}
   PROC-ARGV-RESET
   pid PID>N 0 < if E-PROC-SPAWN throw then
   pid ;

: PROC-RUN-ARGV-IO-RC ( ptr u8 len fd fd fd -- result<n,n> )
   PROC-SPAWN-ARGV-IO PROCESS:WAIT PROCESS:STATUS>RC PROCESS:RC>RESULT ;

: PROC-ARGV-CHECK-PATH ( ptr u8 len -- ) {: path:ptr pathu :}
   pathu LEN>N 0 <= if E-PROC-OUTPUT throw then
   pathu LEN>N PROCESS:PATHZ-CAP >= if E-PROC-OUTPUT throw then ;

: PROC-SPAWN-ARGV-CAPTURE ( ptr u8 ptr a -- ) {: pathz:ptr argv:ptr :}
   [: [: pathz argv PROCESS:CAPTURE-NULL-INPUT
         PROCESS:OUT-W @ >FD PROCESS:ERR-W @ >FD PROC-SPAWN-ARGV-RAW ;]
      [: PROC-ARGV-RESET ;] finally ;]
   PROCESS:CAPTURE-ADOPT-SPAWN ;

: PROC-SPAWN-ARGV-STDIN-CAPTURE ( ptr u8 ptr a -- ) {: pathz:ptr argv:ptr :}
   [: [: pathz argv PROCESS:IN-R @ >FD
         PROCESS:OUT-W @ >FD PROCESS:ERR-W @ >FD PROC-SPAWN-ARGV-RAW ;]
      [: PROC-ARGV-RESET ;] finally ;]
   PROCESS:CAPTURE-ADOPT-SPAWN ;

: RUN-ARGV-CAPTURE ( ptr u8 len ptr u8 len ptr u8 len ms -- result<pcap:captured,pcap:failed> )
   {: path:ptr pathu out:ptr outcap err:ptr errcap timeout :}
   path pathu PROC-ARGV-CHECK-PATH
   outcap errcap PROCESS:CAPTURE-CHECK-CAPS
   path pathu PROC-ARGV-PREPARE {: pathz:ptr argv:ptr :}
   timeout PROCESS:CAPTURE-BEGIN
   pathz argv PROC-SPAWN-ARGV-CAPTURE
   out outcap err errcap PROCESS:RUN-CAPTURE-LOOP
   PROCESS:CAPTURE-FINISH-RC ;

: RUN-ARGV-CAPTURE-OUTCOME ( ptr u8 len ptr u8 len ptr u8 len ms -- len len PROCESS:ending )
   {: path:ptr pathu out:ptr outcap err:ptr errcap timeout :}
   path pathu PROC-ARGV-CHECK-PATH
   outcap errcap PROCESS:CAPTURE-CHECK-CAPS
   path pathu PROC-ARGV-PREPARE {: pathz:ptr argv:ptr :}
   timeout PROCESS:CAPTURE-BEGIN
   pathz argv PROC-SPAWN-ARGV-CAPTURE
   out outcap err errcap PROCESS:RUN-CAPTURE-ENDING-LOOP
   PROCESS:CAPTURE-FINISH-ENDING ;

: RUN-ARGV-STDIN-CAPTURE ( ptr u8 len ptr u8 len ptr u8 len ptr u8 len ms -- result<pcap:captured,pcap:failed> )
   {: path:ptr pathu in:ptr inu out:ptr outcap err:ptr errcap timeout :}
   path pathu PROC-ARGV-CHECK-PATH
   inu PROCESS:CAPTURE-CHECK-STDIN
   outcap errcap PROCESS:CAPTURE-CHECK-CAPS
   path pathu PROC-ARGV-PREPARE {: pathz:ptr argv:ptr :}
   timeout PROCESS:STDIN-CAPTURE-BEGIN
   pathz argv PROC-SPAWN-ARGV-STDIN-CAPTURE
   in inu out outcap err errcap PROCESS:RUN-STDIN-CAPTURE-LOOP
   PROCESS:CAPTURE-FINISH-RC ;

: RUN-ARGV-STDIN-CAPTURE-OUTCOME ( ptr u8 len ptr u8 len ptr u8 len ptr u8 len ms -- len len PROCESS:ending )
   {: path:ptr pathu in:ptr inu out:ptr outcap err:ptr errcap timeout :}
   path pathu PROC-ARGV-CHECK-PATH
   inu PROCESS:CAPTURE-CHECK-STDIN
   outcap errcap PROCESS:CAPTURE-CHECK-CAPS
   path pathu PROC-ARGV-PREPARE {: pathz:ptr argv:ptr :}
   timeout PROCESS:STDIN-CAPTURE-BEGIN
   pathz argv PROC-SPAWN-ARGV-STDIN-CAPTURE
   in inu out outcap err errcap PROCESS:RUN-STDIN-CAPTURE-ENDING-LOOP
   PROCESS:CAPTURE-FINISH-ENDING ;
