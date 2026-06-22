\ process-cwd.f - checked argv/env/cwd process helpers.
\
\ Load after lib/errors.f, lib/string.f, lib/fs.f, lib/process.f,
\ lib/process-argv.f, and lib/process-env.f.

create PROC-CWDZ-BUF PROC-PATHZ-CAP allot

: PROC-SPAWN-ARGV-ENV-CWD-RAW ( ptr u8 ptr a ptr a ptr u8 fd fd fd -- pid )
   {: pathz:ptr argv:ptr envp:ptr cwdz:ptr infd outfd errfd :}
   pathz argv envp cwdz infd FD>N outfd FD>N errfd FD>N spawn-argv-env-cwd-io >PID ;

: PROC-CWDZ ( ptr u8 len -- ptr u8 ) {: a:ptr u :}
   u LEN>N 0 <= if E-PROC-OUTPUT throw then
   a u PROC-CWDZ-BUF PROC-PATHZ-CAP >LEN PROC-ZCOPY ;

: PROC-ARGV-ENV-CWD-RESET ( -- )
   PROC-ARGV-ENV-RESET ;

: SPAWN-ARGV-ENV-CWD-IO ( ptr u8 len ptr u8 len fd fd fd -- pid )
   {: path:ptr pathu cwd:ptr cwdu infd outfd errfd :}
   path pathu PROC-ARGV-PREPARE PROC-ENV-PREPARE cwd cwdu PROC-CWDZ
   infd outfd errfd PROC-SPAWN-ARGV-ENV-CWD-RAW {: pid :}
   PROC-ARGV-ENV-CWD-RESET
   pid PID>N 0 < if E-PROC-SPAWN throw then
   pid ;

: RUN-ARGV-ENV-CWD-IO-RC ( ptr u8 len ptr u8 len fd fd fd -- rc )
   SPAWN-ARGV-ENV-CWD-IO WAIT-RC ;

: PROC-SPAWN-ARGV-ENV-CWD-CAPTURE ( ptr u8 ptr a ptr a ptr u8 -- )
   {: pathz:ptr argv:ptr envp:ptr cwdz:ptr :}
   pathz argv envp cwdz -1 >FD PROC-OUT-W @ PROC-ERR-W @
   PROC-SPAWN-ARGV-ENV-CWD-RAW {: pid :}
   PROC-ARGV-ENV-CWD-RESET
   pid PID>N 0 < if E-PROC-SPAWN PROC-THROW-CAPTURE then
   pid PROC-PID !
   PROC-OUT-W PROC-CLOSE-CELL
   PROC-ERR-W PROC-CLOSE-CELL ;

: PROC-SPAWN-ARGV-ENV-CWD-STDIN-CAPTURE ( ptr u8 ptr a ptr a ptr u8 -- )
   {: pathz:ptr argv:ptr envp:ptr cwdz:ptr :}
   pathz argv envp cwdz PROC-ARGV-IN-R @ PROC-OUT-W @ PROC-ERR-W @
   PROC-SPAWN-ARGV-ENV-CWD-RAW {: pid :}
   PROC-ARGV-ENV-CWD-RESET
   pid PID>N 0 < if E-PROC-SPAWN PROC-ARGV-THROW-CAPTURE then
   pid PROC-PID !
   PROC-ARGV-IN-R PROC-CLOSE-CELL
   PROC-OUT-W PROC-CLOSE-CELL
   PROC-ERR-W PROC-CLOSE-CELL ;

: RUN-ARGV-ENV-CWD-CAPTURE ( ptr u8 len ptr u8 len ptr u8 len ptr u8 len ms -- len len rc )
   {: path:ptr pathu cwd:ptr cwdu out:ptr outcap err:ptr errcap timeout :}
   path pathu PROC-ARGV-CHECK-PATH
   cwdu LEN>N 0 <= if E-PROC-OUTPUT throw then
   outcap LEN>N 0 < if E-PROC-OUTPUT throw then
   errcap LEN>N 0 < if E-PROC-OUTPUT throw then
   PROC-CAPTURE-RESET
   timeout PROC-CAPTURE-DEADLINE!
   PROC-SETUP-CAPTURE-FDS
   path pathu PROC-ARGV-PREPARE PROC-ENV-PREPARE cwd cwdu PROC-CWDZ
   PROC-SPAWN-ARGV-ENV-CWD-CAPTURE
   out outcap err errcap PROC-RUN-CAPTURE-LOOP
   PROC-CLOSE-CAPTURE-FDS
   PROC-REAP-CAPTURE
   PROC-OUT-LEN @ PROC-ERR-LEN @ PROC-RC @ ;

: RUN-ARGV-ENV-CWD-STDIN-CAPTURE ( ptr u8 len ptr u8 len ptr u8 len ptr u8 len ptr u8 len ms -- len len rc )
   {: path:ptr pathu cwd:ptr cwdu in:ptr inu out:ptr outcap err:ptr errcap timeout :}
   path pathu PROC-ARGV-CHECK-PATH
   cwdu LEN>N 0 <= if E-PROC-OUTPUT throw then
   inu LEN>N 0 < if E-PROC-OUTPUT throw then
   outcap LEN>N 0 < if E-PROC-OUTPUT throw then
   errcap LEN>N 0 < if E-PROC-OUTPUT throw then
   PROC-ARGV-CAPTURE-RESET
   timeout PROC-CAPTURE-DEADLINE!
   PROC-SETUP-CAPTURE-FDS
   PROC-ARGV-SETUP-STDIN-FDS
   path pathu PROC-ARGV-PREPARE PROC-ENV-PREPARE cwd cwdu PROC-CWDZ
   PROC-SPAWN-ARGV-ENV-CWD-STDIN-CAPTURE
   in inu out outcap err errcap PROC-RUN-STDIN-CAPTURE-LOOP
   PROC-ARGV-CLOSE-STDIN-FDS
   PROC-CLOSE-CAPTURE-FDS
   PROC-REAP-CAPTURE
   PROC-OUT-LEN @ PROC-ERR-LEN @ PROC-RC @ ;
