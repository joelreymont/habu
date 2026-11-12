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

package PROC-RAW
public

: SPAWN-ARGV-ENV-CWD ( ptr u8 ptr a ptr a ptr u8 fd fd fd -- ptr u8 ptr a ptr a ptr u8 fd fd fd )
   {: pathz:ptr argv:ptr envp:ptr cwdz:ptr infd outfd errfd :}
   pathz dup ZLEN PROCESS:NEW PROCESS:HERMETIC
   argv ROWS PROCESS:ARGV-ROWS
   envp ROWS PROCESS:ENV-ROWS
   cwdz dup ZLEN PROCESS:CWD
   infd outfd errfd LAUNCH
   pathz argv envp cwdz infd outfd errfd ;

;package

package PROC-CWD

private

create PROC-CWDZ-BUF PROCESS:PATHZ-CAP allot

public

: SPAWN-ARGV-ENV-CWD-RAW ( ptr u8 ptr a ptr a ptr u8 fd fd fd -- pid )
   [: PROC-RAW:SPAWN-ARGV-ENV-CWD ;] catch {: rc :}
   drop drop drop drop drop drop drop
   rc PROC-RAW:ANSWER ;

: CWDZ ( ptr u8 len -- ptr u8 ) {: a:ptr u:len :}
   u LEN>N 0 <= if E-PROC-OUTPUT throw then
   a u PROC-CWDZ-BUF PROCESS:PATHZ-CAP >LEN PROCESS:ZCOPY ;

: ARGV-ENV-CWD-RESET ( -- )
   PROC-ARGV-ENV-RESET ;

: SPAWN-ARGV-ENV-CWD-IO ( ptr u8 len ptr u8 len fd fd fd -- pid )
   {: path:ptr pathu cwd:ptr cwdu infd outfd errfd :}
   [: path pathu PROC-ARGV-PREPARE PROC-ENV-PREPARE cwd cwdu CWDZ
      infd outfd errfd SPAWN-ARGV-ENV-CWD-RAW ;]
   [: ARGV-ENV-CWD-RESET ;] finally {: pid:pid :}
   pid PID>N 0 < if E-PROC-SPAWN throw then
   pid ;

private

: PROC-RUN-ARGV-ENV-CWD-IO-RC ( ptr u8 len ptr u8 len fd fd fd -- result<n,n> )
   SPAWN-ARGV-ENV-CWD-IO PROCESS:WAIT PROCESS:STATUS>RC PROCESS:RC>RESULT ;

: PROC-SPAWN-ARGV-ENV-CWD-CAPTURE ( ptr u8 ptr a ptr a ptr u8 -- )
   {: pathz:ptr argv:ptr envp:ptr cwdz:ptr :}
   [: pathz argv envp cwdz PROCESS:CAPTURE-NULL-INPUT
      PROCESS:OUT-W @ >FD PROCESS:ERR-W @ >FD SPAWN-ARGV-ENV-CWD-RAW ;]
   PROCESS:CAPTURE-ADOPT-SPAWN ;

: PROC-SPAWN-ARGV-ENV-CWD-STDIN-CAPTURE ( ptr u8 ptr a ptr a ptr u8 -- )
   {: pathz:ptr argv:ptr envp:ptr cwdz:ptr :}
   [: pathz argv envp cwdz PROCESS:IN-R @ >FD
      PROCESS:OUT-W @ >FD PROCESS:ERR-W @ >FD SPAWN-ARGV-ENV-CWD-RAW ;]
   PROCESS:CAPTURE-ADOPT-SPAWN ;

public

: RUN-ARGV-ENV-CWD-CAPTURE ( ptr u8 len ptr u8 len ptr u8 len ptr u8 len ms -- result<pcap:captured,pcap:failed> )
   {: path:ptr pathu cwd:ptr cwdu out:ptr outcap err:ptr errcap timeout :}
   [: path pathu PROC-ARGV-CHECK-PATH
      cwdu LEN>N 0 <= if E-PROC-OUTPUT throw then
      outcap errcap PROCESS:CAPTURE-CHECK-CAPS
      path pathu PROC-ARGV-PREPARE PROC-ENV-PREPARE
      cwd cwdu CWDZ
      timeout PROCESS:CAPTURE-BEGIN
      PROC-SPAWN-ARGV-ENV-CWD-CAPTURE ;]
   [: ARGV-ENV-CWD-RESET ;] finally
   out outcap err errcap PROCESS:RUN-CAPTURE-LOOP
   PROCESS:CAPTURE-FINISH-RC ;

: RUN-ARGV-ENV-CWD-STDIN-CAPTURE ( ptr u8 len ptr u8 len ptr u8 len ptr u8 len ptr u8 len ms -- result<pcap:captured,pcap:failed> )
   {: path:ptr pathu cwd:ptr cwdu in:ptr inu out:ptr outcap err:ptr errcap timeout :}
   [: path pathu PROC-ARGV-CHECK-PATH
      cwdu LEN>N 0 <= if E-PROC-OUTPUT throw then
      inu PROCESS:CAPTURE-CHECK-STDIN
      outcap errcap PROCESS:CAPTURE-CHECK-CAPS
      path pathu PROC-ARGV-PREPARE PROC-ENV-PREPARE
      cwd cwdu CWDZ
      timeout PROCESS:STDIN-CAPTURE-BEGIN
      PROC-SPAWN-ARGV-ENV-CWD-STDIN-CAPTURE ;]
   [: ARGV-ENV-CWD-RESET ;] finally
   in inu out outcap err errcap PROCESS:RUN-STDIN-CAPTURE-LOOP
   PROCESS:CAPTURE-FINISH-RC ;

: RUN-ARGV-ENV-CWD-STDIN-CAPTURE-OUTCOME ( ptr u8 len ptr u8 len ptr u8 len ptr u8 len ptr u8 len ms -- len len PROCESS:ending )
   {: path:ptr pathu cwd:ptr cwdu in:ptr inu out:ptr outcap err:ptr errcap timeout :}
   [: path pathu PROC-ARGV-CHECK-PATH
      cwdu LEN>N 0 <= if E-PROC-OUTPUT throw then
      inu PROCESS:CAPTURE-CHECK-STDIN
      outcap errcap PROCESS:CAPTURE-CHECK-CAPS
      path pathu PROC-ARGV-PREPARE PROC-ENV-PREPARE
      cwd cwdu CWDZ
      timeout PROCESS:STDIN-CAPTURE-BEGIN
      PROC-SPAWN-ARGV-ENV-CWD-STDIN-CAPTURE ;]
   [: ARGV-ENV-CWD-RESET ;] finally
   in inu out outcap err errcap PROCESS:RUN-STDIN-CAPTURE-ENDING-LOOP
   PROCESS:CAPTURE-FINISH-ENDING ;

;package
