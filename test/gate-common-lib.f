\ Checked helpers for native tests, loaded through test/gate-common.f.

require src/lib/error.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-fork.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/content-key.f
require src/lib/test/runner.f
require src/lib/fd.f
require src/lib/fmt.f
require src/lib/engine-candidate.f
require src/core/exit-status.f

$40000 constant GE-SRC-CAP
10 constant GE-LF
32 constant GE-SP
34 constant GE-DQ

create GE-SRC-BUF GE-SRC-CAP allot
create GE-ARGV-BUF GE-SRC-CAP allot

variable GE-SRC-U
variable GE-RD
variable GE-ARGV-U
TYPED-VARIABLE GE-EVAL-SRC-A ^u8
variable GE-EVAL-SRC-U

: GE-EVAL-SRC! ( str -- ) {: s:str :}
   s STR:LEN 0 < if E-STR-BOUNDS throw then
   s STR:DATA GE-EVAL-SRC-A !
   s STR:LEN GE-EVAL-SRC-U ! ;

: GE-EVAL-SRC$ ( -- str )
   GE-EVAL-SRC-A @ GE-EVAL-SRC-U @ >str ;

: GE-STORE-OUTCOME ( len len PROCESS:ending -- )
   GT-STORE-RUN ;

: GE-ARGV-RESET ( -- )
   0 GE-ARGV-U ! ;

: GE-ARGV-C ( n -- ) {: c:n :}
   GE-ARGV-U @ 1 + GE-SRC-CAP > if E-STR-CAPACITY throw then
   c GE-ARGV-BUF GE-ARGV-U @ + c!
   GE-ARGV-U @ 1+ GE-ARGV-U ! ;

: GE-ARGV+ ( str -- ) {: s:str :}
   s STR:LEN 0 < if E-STR-BOUNDS throw then
   GE-ARGV-U @ s STR:LEN + 3 + GE-SRC-CAP > if E-STR-CAPACITY throw then
   GE-SP GE-ARGV-C
   GE-SP GE-ARGV-C
   s STR:DATA GE-ARGV-BUF GE-ARGV-U @ + s STR:LEN BYTE-COPY
   GE-ARGV-U @ s STR:LEN + GE-ARGV-U !
   GE-LF GE-ARGV-C ;

: GE-ARGV$ ( -- str )
   GE-ARGV-BUF GE-ARGV-U @ >str ;

: GE-SPAWN-FAIL. ( -- )
   s" spawn errno: " type PROCESS:ERRNO FMT:.INT cr ;

\ Spawns pathz with argv and envp, over this process's environment, as the
\ capture child, its stdin on in and its stdout and stderr on the capture pipes,
\ and records its pid. Stack-preserving under catch, so GE-LAUNCH-RC answers a
\ rejection's code.
: GE-LAUNCH ( ^u8 ^^u8 ^^u8 fd -- ^u8 ^^u8 ^^u8 fd )
   {: pathz:^ argv:^ envp:^ in:fd :}
   pathz dup ZLEN >str PROCESS:NEW
   argv PROCESS:ARGV-ROWS
   envp PROCESS:ENV-ROWS
   0 >FD in PROCESS:FD
   1 >FD PROCESS:OUT-W @ >FD PROCESS:FD
   2 >FD PROCESS:ERR-W @ >FD PROCESS:FD
   PROCESS:SPAWN PID>N PROCESS:PID !
   pathz argv envp in ;

\ GE-LAUNCH's catch code, with the staged argv and env reset either way.
: GE-LAUNCH-RC ( ^u8 ^^u8 ^^u8 fd -- n )
   [: GE-LAUNCH ;] catch {: code:n :}
   drop drop drop drop
   PROC-ARGV-ENV-RESET
   code ;

\ A rejected launch prints its errno and ends the capture with its code.
: GE-LAUNCHED ( n -- ) {: code:n :}
   code 0= if exit then
   GE-SPAWN-FAIL.
   code PROCESS:THROW-CAPTURE ;

: GE-SPAWN-STDIN-CAPTURE ( ^u8 ^^u8 ^^u8 -- )
   PROCESS:IN-R @ >FD GE-LAUNCH-RC GE-LAUNCHED
   PROCESS:IN-R PROCESS:CLOSE-CELL
   PROCESS:OUT-W PROCESS:CLOSE-CELL
   PROCESS:ERR-W PROCESS:CLOSE-CELL ;

: GE-RUN-ENV ( str ms -- ) {: path:str timeout:ms :}
   path PROC-ARGV-PREPARE {: pathz:^ argv:^ :}
   PROC-ENV-PREPARE {: envp:^ :}
   timeout PROCESS:STDIN-CAPTURE-BEGIN
   pathz argv envp GE-SPAWN-STDIN-CAPTURE
   s" " [: GT-OUT-ROOM ;] [: GT-ERR-ROOM ;]
   PROCESS:RUN-STDIN-GROWING-BOTH-CAPTURE-ENDING-LOOP
   PROCESS:CAPTURE-FINISH-ENDING GE-STORE-OUTCOME ;

: GE-RUN-STDIN ( str str ms -- ) {: path:str in:str timeout:ms :}
   path PROC-ARGV-PREPARE {: pathz:^ argv:^ :}
   PROC-ENV-PREPARE {: envp:^ :}
   timeout PROCESS:STDIN-CAPTURE-BEGIN
   pathz argv envp GE-SPAWN-STDIN-CAPTURE
   in [: GT-OUT-ROOM ;] [: GT-ERR-ROOM ;]
   PROCESS:RUN-STDIN-GROWING-BOTH-CAPTURE-ENDING-LOOP
   PROCESS:CAPTURE-FINISH-ENDING GE-STORE-OUTCOME ;

: GE-OUTCOME. ( PROCESS:ending -- )
   MATCH PROCESS:ending
     exited OF drop s" exit" type ENDOF
     signaled OF drop s" signal" type ENDOF
     timeout OF s" timeout" type ENDOF
   ;MATCH ;

: GE-OUTCOME-CODE. ( PROCESS:ending -- )
   MATCH PROCESS:ending
     exited OF FMT:.INT ENDOF
     signaled OF FMT:.INT ENDOF
     timeout OF s" -" type ENDOF
   ;MATCH ;

: GE-RC-NAME. ( n -- ) {: rc:n :}
   rc case
      E-PROC-SPAWN of s" E-PROC-SPAWN" type endof
      E-PROC-WAIT of s" E-PROC-WAIT" type endof
      E-PROC-TIMEOUT of s" E-PROC-TIMEOUT" type endof
      E-PROC-OUTPUT of s" E-PROC-OUTPUT" type endof
      E-PROC-STDOUT-CAPACITY of s" E-PROC-STDOUT-CAPACITY" type endof
      E-PROC-STDERR-CAPACITY of s" E-PROC-STDERR-CAPACITY" type endof
      E-PROC-ENV of s" E-PROC-ENV" type endof
      E-PROC-PATH of s" E-PROC-PATH" type endof
      E-FS-OPEN of s" E-FS-OPEN" type endof
      E-FS-CAPACITY of s" E-FS-CAPACITY" type endof
      E-STR-BOUNDS of s" E-STR-BOUNDS" type endof
      E-STR-CAPACITY of s" E-STR-CAPACITY" type endof
      60 of s" E-PROC-SPAWN" type endof
      59 of s" E-PROC-WAIT" type endof
      58 of s" E-PROC-TIMEOUT" type endof
      57 of s" E-PROC-OUTPUT" type endof
      56 of s" E-PROC-STDOUT-CAPACITY" type endof
      49 of s" E-PROC-STDERR-CAPACITY" type endof
      55 of s" E-PROC-ENV" type endof
      54 of s" E-PROC-PATH" type endof
      70 of s" E-CHECK" type endof
      202 of s" E-FS-OPEN" type endof
      198 of s" E-FS-CAPACITY" type endof
      104 of s" E-STR-BOUNDS" type endof
      103 of s" E-STR-CAPACITY" type endof
      s" unmapped" type
   endcase ;

: GE-PRINT-OUTCOME ( -- )
   s" outcome: " type GT-OUTCOME@ GE-OUTCOME.
   s"  code: " type GT-OUTCOME@ GE-OUTCOME-CODE.
   GT-TIMED-OUT @ if cr exit then          \ a deadline has no rc: GT-RC@ throws
   s"  rc: " type GT-RC@ FMT:.INT s"  (" type GT-RC@ GE-RC-NAME. s" )" type cr ;

: GE-PRINT-CAPTURE-STATS ( -- )
   s" stdout bytes: " type GT-OUT$ STR:LEN FMT:.INT cr
   s" stderr bytes: " type GT-ERR$ STR:LEN FMT:.INT cr ;

: GE-FAIL ( str -- ) {: label:str :}
   s" FAIL: " type label type cr
   GE-PRINT-OUTCOME
   GE-PRINT-CAPTURE-STATS
   GE-ARGV$ STR:LEN 0 > if
      s" argv:" type cr
      GE-ARGV$ type
   then
   s" stdout:" type cr
   GT-OUT$ type
   s" stderr:" type cr
   GT-ERR$ type
   \ A child whose deadline expired failed no check of its own: rethrow the
   \ deadline instead of failing this check. That is this capture's deadline, or
   \ one inside the child, which then exits 124.
   GT-TIMED-OUT @ if E-PROC-TIMEOUT throw then
   GT-RC@ 124 = if E-PROC-TIMEOUT throw then
   s" native test failed" EXIT-STATUS:FAILURE die ;

\ The entry's exit status for an rc check. A timed-out entry has none, and
\ GT-RC@ would throw before any report, so it fails here: GE-FAIL prints its
\ capture and then throws E-PROC-TIMEOUT.
: GE-RC@ ( str -- n ) {: label:str :}
   GT-TIMED-OUT @ if label GE-FAIL then
   GT-RC@ ;

: GE-EXPECT-OK ( str -- ) {: label:str :}
   label GE-RC@ 0 <> if label GE-FAIL then ;

: GE-EXPECT-RC ( n str -- ) {: want:n label:str :}
   label GE-RC@ want <> if label GE-FAIL then ;

: GE-EXPECT-NONZERO ( str -- ) {: label:str :}
   label GE-RC@ 0= if label GE-FAIL then ;

: GE-EXPECT-SILENT ( str -- ) {: label:str :}
   GT-OUT$ STR:LEN 0 <> if label GE-FAIL then
   GT-ERR$ STR:LEN 0 <> if label GE-FAIL then ;

: GE-EXPECT-OUT ( str str -- ) {: want:str label:str :}
   GT-OUT$ want STR:= 0= if label GE-FAIL then ;

: GE-EXPECT-OUT-HAS ( str str -- ) {: want:str label:str :}
   GT-OUT$ want STR:CONTAINS? 0= if label GE-FAIL then ;

: GE-EXPECT-OUT-LACKS ( str str -- ) {: want:str label:str :}
   GT-OUT$ want STR:CONTAINS? if label GE-FAIL then ;

\ The whole of stderr, byte for byte - the check a one-line diagnostic asks for.
: GE-EXPECT-ERR ( str str -- ) {: want:str label:str :}
   GT-ERR$ want STR:= 0= if label GE-FAIL then ;

: GE-EXPECT-ERR-HAS ( str str -- ) {: want:str label:str :}
   GT-ERR$ want STR:CONTAINS? 0= if label GE-FAIL then ;

: GE-EXPECT-ERR-LACKS ( str str -- ) {: want:str label:str :}
   GT-ERR$ want STR:CONTAINS? if label GE-FAIL then ;

: GE-SB-LF ( -- )
   GE-LF SB-APPEND-C ;

: GE-OUT-LINE ( str -- )
   SB-APPEND
   GE-SB-LF ;

: GE-SRC-RESET ( -- )
   0 GE-SRC-U ! ;

: GE-SRC-C ( n -- ) {: c:n :}
   c 0 < if E-STR-BOUNDS throw then
   c STR:BYTE-MAX > if E-STR-BOUNDS throw then
   GE-SRC-U @ 1 + GE-SRC-CAP > if E-STR-CAPACITY throw then
   c GE-SRC-BUF GE-SRC-U @ + c!
   GE-SRC-U @ 1+ GE-SRC-U ! ;

: GE-SRC-U+ ( n -- ) {: n:n :}
   n 0 < if E-STR-BOUNDS throw then
   n 10 >= if n 10 / recurse then
   n 10 mod STR:ZERO + GE-SRC-C ;

: GE-SRC-REPEAT-C ( n n -- ) {: u:n c:n :}
   u 0 < if E-STR-BOUNDS throw then
   u 0 ?do c GE-SRC-C loop ;

: GE-SRC+ ( str -- ) {: s:str :}
   s STR:LEN 0 < if E-STR-BOUNDS throw then
   GE-SRC-U @ s STR:LEN + GE-SRC-CAP > if E-STR-CAPACITY throw then
   s STR:DATA GE-SRC-BUF GE-SRC-U @ + s STR:LEN BYTE-COPY
   GE-SRC-U @ s STR:LEN + GE-SRC-U ! ;

: GE-SRC-SP ( -- )
   GE-SP GE-SRC-C ;

: GE-SRC-LF ( -- )
   GE-LF GE-SRC-C ;

: GE-SRC-LINE ( str -- )
   GE-SRC+
   GE-SRC-LF ;

: GE-SRC-S" ( str -- ) {: s:str :}
   s" s" GE-SRC+
   GE-DQ GE-SRC-C
   GE-SRC-SP
   s GE-SRC+
   GE-DQ GE-SRC-C ;

: GE-SRC-CHECK-LINE ( str -- )
   GE-SRC-S"
   s"  CHECK! ." GE-SRC-LINE ;

: GE-SRC-FILE+ ( str -- ) {: path:str :}
   path GE-SRC-BUF GE-SRC-U @ + GE-SRC-CAP GE-SRC-U @ -
   FS:READ-ALL GE-RD !
   GE-SRC-U @ GE-RD @ + GE-SRC-U ! ;

: GE-ARG+ ( str -- )
   dup GE-ARGV+
   PROC-ARGV+ ;

: GE-COPY! ( str ^u8 ^n -- ) {: s:str dst:^ up:^ :}
   s STR:LEN 0 < if E-FS-PATH throw then
   s STR:LEN PATH-CAP > if E-FS-PATH throw then
   s STR:DATA dst s STR:LEN BYTE-COPY
   s STR:LEN up ! ;

: GE-HB-RESET ( -- )
   PROC-ARGV-ENV-RESET
   GE-ARGV-RESET ;

: GE-HB$ ( -- str ) ENGINE-CANDIDATE:PATH$ ;

: GE-HB-RUN-STDIN ( str -- ) {: label:str :}
   label GT-PROGRESS-RUN
   GE-HB$ GE-SRC-BUF GE-SRC-U @ >str
   PROCESS:WAIT-FOREVER GE-RUN-STDIN
   label GE-EXPECT-OK
   label GT-PROGRESS-PASS ;

: GE-HB-RUN-STDIN-NZ ( str -- ) {: label:str :}
   label GT-PROGRESS-RUN
   GE-HB$ GE-SRC-BUF GE-SRC-U @ >str
   PROCESS:WAIT-FOREVER GE-RUN-STDIN
   label GE-EXPECT-NONZERO
   label GT-PROGRESS-PASS ;

: GE-EVAL-DUP2! ( n n -- ) {: src:n dst:n :}
   src dst dup2 dup 0 < if drop E-PROC-OUTPUT throw then drop ;

: GE-EVAL-REDIRECT! ( -- )
   PROCESS:OUT-W @ FD:STDOUT FD>N GE-EVAL-DUP2!
   PROCESS:ERR-W @ FD:STDERR FD>N GE-EVAL-DUP2!
   PROCESS:OUT-W PROCESS:CLOSE-CELL
   PROCESS:ERR-W PROCESS:CLOSE-CELL ;

: GE-EVAL-SOURCE ( -- ) GE-EVAL-SRC$ evaluate-closed ;

\ An in-process action's capture (src/lib/test/runner.f GT-CAPTURE-ACTION), stored
\ as an exit whose code is the action's throw code.
: GE-CAPTURE-ACTION ( [ -- ] -- )
   {: q :}
   q GT-ROOT [: GT-OUT-ROOM ;] [: GT-ERR-ROOM ;] GT-CAPTURE-ACTION {: outu:len erru:len rc:n :}
   outu erru rc PROCESS-ENDING:EXITED GE-STORE-OUTCOME ;

: GE-EVAL-FORK-EXIT ( n -- )
   s" " swap die ;

: GE-EVAL-FORK-CHILD ( -- )
   GE-EVAL-REDIRECT!
   [: GE-EVAL-SOURCE ;] catch {: rc:n :}
   rc GE-EVAL-FORK-EXIT ;

: GE-EVAL-FORK-SPAWN ( -- )
   0 >PID [: drop PROC-FORK:RAW ;] catch {: pid code:n :}
   code 0<> if code PROCESS:THROW-CAPTURE then
   pid PID>N 0 < if E-PROC-SPAWN PROCESS:THROW-CAPTURE then
   pid PID>N 0= if [: GE-EVAL-FORK-CHILD ;] ERROR:CHILD-TOP then
   pid PID>N PROCESS:PID !
   PROCESS:OUT-W PROCESS:CLOSE-CELL
   PROCESS:ERR-W PROCESS:CLOSE-CELL ;

: GE-EVAL-FORK-CAPTURE ( -- )
   GE-SRC-BUF GE-SRC-U @ >str GE-EVAL-SRC!
   PROCESS:WAIT-FOREVER PROCESS:CAPTURE-BEGIN
   GE-EVAL-FORK-SPAWN
   s" " [: GT-OUT-ROOM ;] [: GT-ERR-ROOM ;]
   PROCESS:RUN-STDIN-GROWING-BOTH-CAPTURE-ENDING-LOOP
   PROCESS:CAPTURE-FINISH-ENDING GE-STORE-OUTCOME ;

: GE-EVAL-FORK-BAD ( n str str -- )
   {: rc:n needle:str label:str :}
   label GT-PROGRESS-RUN
   GE-EVAL-FORK-CAPTURE
   rc label GE-EXPECT-RC
   needle label GE-EXPECT-ERR-HAS
   label GT-PROGRESS-PASS ;

: GE-EVAL-RUN-STDIN ( str -- ) {: label:str :}
   label GT-PROGRESS-RUN
   GE-EVAL-FORK-CAPTURE
   label GE-EXPECT-OK
   label GT-PROGRESS-PASS ;

: GE-BIN-HB? ( str -- bool )
   s" bin/hb" STR:= ;

: GE-REMOVE-BIN-OTHER ( str -- ) {: path:str :}
   path FS:FILE? if
      path GE-BIN-HB? 0= if path FS:REMOVE-FILE then
   then ;

: GE-CLEAN-BIN ( -- )
   s" bin" [: GE-REMOVE-BIN-OTHER ;] FS:WALK-FILES ;
