\ process-argv.f - checked argv process helpers.

s" lib/errors.f" required
s" lib/memory.f" required
s" lib/process.f" required
require lib/image-lifecycle.f

$100 constant PROC-ARGV-MAX
32768 constant PROC-ARGV-BUF-CAP

\ The argv table holds the address of every zero-terminated argument and
\ PROC-ARGV-BUF-A the address of the mapping they live in, so both are declared
\ pointer storage: a pointer stored in or fetched from an undeclared `create`d
\ cell is refused (E-RAW-CELL-PTR).
PROC-ARGV-MAX 1 + TYPED-BUFFER PROC-ARGV-TABLE ptr u8

variable PROC-ARGV-N
variable PROC-ARGV-OFF
TYPED-VARIABLE PROC-ARGV-BUF-A ptr u8

: PROC-ARGV-BUF@ ( -- ptr u8 )
   PROC-ARGV-BUF-A @ ;

: PROC-ARGV-BUF! ( ptr u8 -- )
   PROC-ARGV-BUF-A ! ;

package PROC-ARGV-LIFECYCLE
private

TYPED-VARIABLE REGISTERED bool
false REGISTERED !

: RELEASE ( -- )
   PROC-ARGV-BUF@ {: bytes:ptr :}
   NULL-PTR PROC-ARGV-BUF!
   0 >COUNT PROC-ARGV-N !
   0 >OFF PROC-ARGV-OFF !
   PROC-ARGV-MAX 1+ 0 ?do
      NULL-PTR i PROC-ARGV-TABLE !
   loop
   bytes 0= 0= if
      bytes PROC-ARGV-BUF-CAP MEM:BYTES-ALLOC-LEN MEM:RELEASE-BYTES
   then
   false REGISTERED ! ;

public

\ Register before allocation: refusal cannot leave an untracked mapping.
\ A failed allocation leaves a harmless hook, cleared by the next capture.
: REGISTER ( -- )
   REGISTERED @ if exit then
   [: RELEASE ;] IMAGE-LIFECYCLE:REGISTER
   true REGISTERED ! ;

;package

\ PROC-ARGV-BUF-CAP is a positive library constant: MEM:BYTES-ALLOC-LEN narrows the
\ raw size to the validated alloc role before MEM:ALLOC-BYTES, throwing E-MEM-SIZE
\ on any refusal (unreachable for the constant).
: PROC-ARGV-BUF ( -- ptr u8 )
   PROC-ARGV-BUF@ 0= if
      PROC-ARGV-LIFECYCLE:REGISTER
      PROC-ARGV-BUF-CAP MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop PROC-ARGV-BUF!
   then
   PROC-ARGV-BUF@ ;

: PROC-SPAWN-ARGV-RAW ( ptr u8 ptr a fd fd fd -- pid )
   {: pathz:ptr argv:ptr infd outfd errfd :}
   pathz argv infd FD>N outfd FD>N errfd FD>N spawn-argv-io >PID ;

: PROC-ARGV-RESET ( -- )
   0 >COUNT PROC-ARGV-N !
   0 >OFF PROC-ARGV-OFF ! ;

: PROC-ARGV-SLOT ( idx -- ptr ptr u8 ) {: idx :}
   idx IDX>N 0 < if E-PROC-OUTPUT throw then
   idx IDX>N PROC-ARGV-MAX > if E-PROC-OUTPUT throw then
   idx IDX>N PROC-ARGV-TABLE ;

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

: PROC-ARGV-PREPARE ( ptr u8 len -- ptr u8 ptr ptr u8 ) {: path:ptr pathu :}
   pathu LEN>N 0 <= if E-PROC-OUTPUT throw then
   \ Even an argument-free command stores a process address in the table.
   PROC-ARGV-LIFECYCLE:REGISTER
   path pathu PROC-PATHZ {: pathz:ptr :}
   pathz 0 >IDX PROC-ARGV-SLOT !
   NULL$ drop PROC-ARGV-N @ COUNT>N 1+ >IDX PROC-ARGV-SLOT !
   pathz 0 PROC-ARGV-TABLE ;

: PROC-SPAWN-ARGV-IO ( ptr u8 len fd fd fd -- pid ) {: a:ptr u infd outfd errfd :}
   a u PROC-ARGV-PREPARE infd outfd errfd PROC-SPAWN-ARGV-RAW {: pid :}
   PROC-ARGV-RESET
   pid PID>N 0 < if E-PROC-SPAWN throw then
   pid ;

: PROC-RUN-ARGV-IO-RC ( ptr u8 len fd fd fd -- result<n,n> )   \ ok = clean exit (0), err = nonzero completion rc
   PROC-SPAWN-ARGV-IO PROC-WAIT-RC ;

: PROC-ARGV-CHECK-PATH ( ptr u8 len -- ) {: path:ptr pathu :}
   pathu LEN>N 0 <= if E-PROC-OUTPUT throw then
   pathu LEN>N 1 + PROC-PATHZ-CAP > if E-PROC-OUTPUT throw then ;

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
