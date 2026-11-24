\ process-cwd.f - checked argv/env/cwd process helpers in `package PROC-CWD`.
\ STORAGE CLASS. PROCESS-WIDE: the cwd copy buffer is one per image, so one task
\ at a time prepares a cwd child through RUN-ARGV-ENV-CWD-*.
\ src/lib/process.f "SPAWN" states how these words spawn.
require src/lib/error.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f

package PROC-CWD

private

create PROC-CWDZ-BUF PROCESS:PATHZ-CAP allot

public

: SPAWN-ARGV-ENV-CWD-RAW ( ^u8 ^a ^a ^u8 fd fd fd -- pid )
   [: PROC-RAW:SPAWN-ARGV-ENV-CWD ;] catch {: rc :}
   drop drop drop drop drop drop drop
   rc PROC-RAW:ANSWER ;

: CWDZ ( str -- ^u8 ) {: s:str :}
   s STR:LEN 0 <= if E-PROC-OUTPUT throw then
   s PROC-CWDZ-BUF PROCESS:PATHZ-CAP >LEN PROCESS:ZCOPY ;

: ARGV-ENV-CWD-RESET ( -- )
   PROC-ARGV-ENV-RESET ;

: SPAWN-ARGV-ENV-CWD-IO ( str str fd fd fd -- pid )
   {: path:str cwd:str infd outfd errfd :}
   [: path PROC-ARGV-PREPARE PROC-ENV-PREPARE cwd CWDZ
      infd outfd errfd SPAWN-ARGV-ENV-CWD-RAW ;]
   [: ARGV-ENV-CWD-RESET ;] finally {: pid:pid :}
   pid PID>N 0 < if E-PROC-SPAWN throw then
   pid ;

private

: PROC-RUN-ARGV-ENV-CWD-IO-RC ( str str fd fd fd -- result<n,n> )
   SPAWN-ARGV-ENV-CWD-IO PROCESS:WAIT PROCESS:STATUS>RC PROCESS:RC>RESULT ;

: PROC-SPAWN-ARGV-ENV-CWD-CAPTURE ( ^u8 ^a ^a ^u8 -- )
   {: pathz:^ argv:^ envp:^ cwdz:^ :}
   [: pathz argv envp cwdz PROCESS:CAPTURE-NULL-INPUT
      PROCESS:OUT-W @ >FD PROCESS:ERR-W @ >FD SPAWN-ARGV-ENV-CWD-RAW ;]
   PROCESS:CAPTURE-ADOPT-SPAWN ;

: PROC-SPAWN-ARGV-ENV-CWD-STDIN-CAPTURE ( ^u8 ^a ^a ^u8 -- )
   {: pathz:^ argv:^ envp:^ cwdz:^ :}
   [: pathz argv envp cwdz PROCESS:IN-R @ >FD
      PROCESS:OUT-W @ >FD PROCESS:ERR-W @ >FD SPAWN-ARGV-ENV-CWD-RAW ;]
   PROCESS:CAPTURE-ADOPT-SPAWN ;

public

: RUN-ARGV-ENV-CWD-CAPTURE ( str str ^u8 len ^u8 len ms -- result<pcap:captured,pcap:failed> )
   {: path:str cwd:str out:^ outcap err:^ errcap timeout :}
   [: path PROC-ARGV-CHECK-PATH
      cwd STR:LEN 0 <= if E-PROC-OUTPUT throw then
      outcap errcap PROCESS:CAPTURE-CHECK-CAPS
      path PROC-ARGV-PREPARE PROC-ENV-PREPARE
      cwd CWDZ
      timeout PROCESS:CAPTURE-BEGIN
      PROC-SPAWN-ARGV-ENV-CWD-CAPTURE ;]
   [: ARGV-ENV-CWD-RESET ;] finally
   out outcap err errcap PROCESS:RUN-CAPTURE-LOOP
   PROCESS:CAPTURE-FINISH-RC ;

: RUN-ARGV-ENV-CWD-STDIN-CAPTURE ( str str str ^u8 len ^u8 len ms -- result<pcap:captured,pcap:failed> )
   {: path:str cwd:str in:str out:^ outcap err:^ errcap timeout :}
   [: path PROC-ARGV-CHECK-PATH
      cwd STR:LEN 0 <= if E-PROC-OUTPUT throw then
      in STR:LEN PROCESS:CAPTURE-CHECK-STDIN
      outcap errcap PROCESS:CAPTURE-CHECK-CAPS
      path PROC-ARGV-PREPARE PROC-ENV-PREPARE
      cwd CWDZ
      timeout PROCESS:STDIN-CAPTURE-BEGIN
      PROC-SPAWN-ARGV-ENV-CWD-STDIN-CAPTURE ;]
   [: ARGV-ENV-CWD-RESET ;] finally
   in out outcap err errcap PROCESS:RUN-STDIN-CAPTURE-LOOP
   PROCESS:CAPTURE-FINISH-RC ;

: RUN-ARGV-ENV-CWD-STDIN-CAPTURE-OUTCOME ( str str str ^u8 len ^u8 len ms -- len len PROCESS:ending )
   {: path:str cwd:str in:str out:^ outcap err:^ errcap timeout :}
   [: path PROC-ARGV-CHECK-PATH
      cwd STR:LEN 0 <= if E-PROC-OUTPUT throw then
      in STR:LEN PROCESS:CAPTURE-CHECK-STDIN
      outcap errcap PROCESS:CAPTURE-CHECK-CAPS
      path PROC-ARGV-PREPARE PROC-ENV-PREPARE
      cwd CWDZ
      timeout PROCESS:STDIN-CAPTURE-BEGIN
      PROC-SPAWN-ARGV-ENV-CWD-STDIN-CAPTURE ;]
   [: ARGV-ENV-CWD-RESET ;] finally
   in out outcap err errcap PROCESS:RUN-STDIN-CAPTURE-ENDING-LOOP
   PROCESS:CAPTURE-FINISH-ENDING ;

;package
