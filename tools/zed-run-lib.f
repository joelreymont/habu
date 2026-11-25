\ zed-run-lib.f - checked remote device-run harness over argv-spawned ssh/scp/rsync
\ (src/lib/process-command.f), never a shell. Remote commands are built from
\ controlled path tokens only; test data travels as files. Failures throw E-ZED-*;
\ an expired deadline throws E-PROC-TIMEOUT. With HABU_ZED unset or `0` a manual
\ caller may report the device unavailable; set, every failure is fail-closed.
\ ZED_HOST (default `zed`), ZED_SSH, ZED_SCP and ZED_RSYNC override.

require src/lib/error.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/memory.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/process-command.f

package ZED

$EA60 constant DEFAULT-TIMEOUT-MS   \ 60000 ms

$1000 constant CMD-CAP
$400 constant DEST-CAP
$400 constant SCRATCH-CAP

create CMD-BUF CMD-CAP allot
create DEST-BUF DEST-CAP allot
create SCRATCH-BUF SCRATCH-CAP allot

variable CMD-U
variable DEST-U
variable SCRATCH-U
variable TIMEOUT-MS
variable TRIM-N

DEFAULT-TIMEOUT-MS TIMEOUT-MS !

: YES ( -- bool )  0 0= ;
: NO ( -- bool )  0 0= 0= ;

\ ---- env-driven configuration ------------------------------------------------

: ENV-OR ( str str -- str ) {: ds:str name:str :}
   name GETENV dup STR:LEN 0= if drop ds then ;

: HOST$ ( -- str )
   s" zed" s" ZED_HOST" ENV-OR ;

: SSH$ ( -- str )
   s" /usr/bin/ssh" s" ZED_SSH" ENV-OR ;

: SCP$ ( -- str )
   s" /usr/bin/scp" s" ZED_SCP" ENV-OR ;

: RSYNC$ ( -- str )
   s" /usr/bin/rsync" s" ZED_RSYNC" ENV-OR ;

\ ---- remote-command string builder (space-separated tokens) ------------------

: CMD-RESET ( -- )
   0 CMD-U ! ;

: CMD-RAW ( str -- ) {: s:str :}
   CMD-U @ s STR:LEN + CMD-CAP > if E-ZED-ARG throw then
   s STR:DATA CMD-BUF CMD-U @ + s STR:LEN BYTE-COPY
   CMD-U @ s STR:LEN + CMD-U ! ;

: CMD-TOK ( str -- )
   CMD-U @ 0 > if s"  " CMD-RAW then
   CMD-RAW ;

: CMD$ ( -- str )
   CMD-BUF CMD-U @ >str ;

\ ---- rsync/scp destination: host:scratch/ ------------------------------------

: DEST+ ( str -- ) {: s:str :}
   DEST-U @ s STR:LEN + DEST-CAP > if E-ZED-ARG throw then
   s STR:DATA DEST-BUF DEST-U @ + s STR:LEN BYTE-COPY
   DEST-U @ s STR:LEN + DEST-U ! ;

: DEST-BUILD ( -- )
   0 DEST-U !
   HOST$ DEST+  s" :" DEST+  SCRATCH-BUF SCRATCH-U @ >str DEST+  s" /" DEST+ ;

: DEST$ ( -- str )
   DEST-BUF DEST-U @ >str ;

\ ---- ssh/scp/rsync argv runners (capture stdout/stderr/rc) -------------------

: BATCH-OPTS ( CMD:command -- CMD:command )
   s" -o" CMD:ARG+  s" BatchMode=yes" CMD:ARG+
   s" -o" CMD:ARG+  s" ConnectTimeout=10" CMD:ARG+ ;

: SSH-RUN ( CMD:command str -- CMD:command rc )
   {: s:str :}
   BATCH-OPTS
   HOST$ CMD:ARG+
   s CMD:ARG+
   SSH$ TIMEOUT-MS @ >MS CMD:RUN-RC MATCH result
     ok  OF >RC ENDOF
     err OF >RC ENDOF
   ;MATCH ;

: EXEC-BODY ( str CMD:command -- rc CMD:command )
   >r {: s:str :} r>
   s SSH-RUN swap ;

: SSH-EXEC ( str -- rc )   \ the rc alone; the captures end with the scope
   [: EXEC-BODY ;] CMD:WITH-COMMAND ;

: WITH-RUN-BODY ( R str [ R rc CMD:command -- S CMD:command ] CMD:command -- S CMD:command )
   >r {: s:str body :} r>
   s SSH-RUN swap body execute ;

\ Run the upload's argv and throw E-ZED-PUT for a nonzero completion.
: PUT-RUN ( CMD:command str -- CMD:command )
   TIMEOUT-MS @ >MS CMD:RUN-RC MATCH result
     ok  OF drop ENDOF
     err OF drop E-ZED-PUT throw ENDOF
   ;MATCH ;

: PUT-DIR-BODY ( str CMD:command -- CMD:command )
   >r {: s:str :} r>
   s" -rlt" CMD:ARG+
   s CMD:ARG+
   DEST$ CMD:ARG+
   RSYNC$ PUT-RUN ;

: PUT-FILE-BODY ( str CMD:command -- CMD:command )
   >r {: s:str :} r>
   s" -o" CMD:ARG+  s" BatchMode=yes" CMD:ARG+
   s CMD:ARG+
   DEST$ CMD:ARG+
   SCP$ PUT-RUN ;

\ ---- newline/whitespace str:trim (mktemp output) ---------------------------------

: WS-BYTE? ( n -- bool ) {: c:n :}   \ trailing whitespace: LF $0A, CR $0D, space $20
   c $0A = c $0D = or c $20 = or ;

: TRIM-TRAIL ( str -- str ) {: s:str :}
   s STR:LEN TRIM-N !
   begin
      TRIM-N @ 0 > if s STR:DATA TRIM-N @ 1- + c@ WS-BYTE? else NO then
   while
      TRIM-N @ 1- TRIM-N !
   repeat
   s STR:DATA TRIM-N @ >str ;

: SCRATCH! ( str -- ) {: s:str :}
   s STR:LEN SCRATCH-CAP > if E-ZED-ARG throw then
   s STR:DATA SCRATCH-BUF s STR:LEN BYTE-COPY
   s STR:LEN SCRATCH-U ! ;

public

\ ---- availability / skip policy ----------------------------------------------

: ENV-AVAILABLE? ( str -- bool ) {: s:str :}
   s STR:LEN 0= if NO exit then
   s s" 0" STR:= if NO exit then
   YES ;

: AVAILABLE? ( -- bool )
   s" HABU_ZED" GETENV ENV-AVAILABLE? ;

: SKIP ( str -- )
   s" SKIP (HABU_ZED unset/0): " type
   type cr ;

: NEED-DEVICE ( -- )   \ device mandatory: fail-closed instead of skippable
   AVAILABLE? 0= if E-ZED-DISABLED throw then ;

: TIMEOUT! ( n -- )
   >MS TIMEOUT-MS ! ;

\ ---- outcome classification --------------------------------------------------

\ Remote tools may legitimately exit 255 (ptxas does), so a general command rc
\ cannot distinguish transport failure from tool failure. PING owns the
\ transport class: running `true`, ANY nonzero rc is unreachability. RUN-OK
\ classifies every nonzero rc of an ordinary command as a remote failure. A run
\ whose deadline expired never reaches either: CMD:RUN-RC throws
\ E-PROC-TIMEOUT for it.
: UNREACH-OK ( rc -- )
   RC>N 0 <> if E-ZED-UNREACH throw then ;

: RUN-OK ( rc -- )
   RC>N 0 <> if E-ZED-RC throw then ;

\ ---- remote command execution ------------------------------------------------

: RUN ( str -- rc )
   SSH-EXEC ;

\ Run one remote command string and hand the body its rc and the command, whose
\ CMD:OUT$ and CMD:ERR$ hold the run's whole stdout and stderr until the body
\ returns or throws.
: WITH-RUN ( R str [ R rc CMD:command -- S CMD:command ] -- S )
   [: WITH-RUN-BODY ;] CMD:WITH-COMMAND ;

: RUN-IN ( str -- rc ) {: s:str :}   \ run with cwd = scratch
   CMD-RESET
   s" cd" CMD-TOK  SCRATCH-BUF SCRATCH-U @ >str CMD-TOK  s" &&" CMD-TOK  s CMD-TOK
   CMD$ SSH-EXEC ;

: PING ( -- )   \ reachability probe: throws E-ZED-UNREACH if the host is down
   s" true" RUN UNREACH-OK ;

: SCRATCH$ ( -- str )
   SCRATCH-BUF SCRATCH-U @ >str ;

private

: SCRATCH-READ ( rc CMD:command -- CMD:command )
   >r RUN-OK r>
   CMD:OUT$ TRIM-TRAIL SCRATCH! ;

public

: SCRATCH-MK ( -- )   \ create ~/habu-zed-scratch/run.XXXXXX, store its path
   s" mkdir -p ~/habu-zed-scratch && mktemp -d ~/habu-zed-scratch/run.XXXXXX"
   [: SCRATCH-READ ;] WITH-RUN ;

: SCRATCH-RM ( -- )   \ remove the scratch tree (fail-closed)
   SCRATCH-U @ 0= if exit then
   CMD-RESET
   s" rm -rf" CMD-TOK  SCRATCH-BUF SCRATCH-U @ >str CMD-TOK
   CMD$ SSH-EXEC RUN-OK
   0 SCRATCH-U ! ;

: PUT-DIR ( str -- )   \ rsync local dir into scratch/
   DEST-BUILD
   [: PUT-DIR-BODY ;] CMD:WITH-COMMAND ;

: PUT-FILE ( str -- )   \ scp local file into scratch/
   DEST-BUILD
   [: PUT-FILE-BODY ;] CMD:WITH-COMMAND ;

: HAVE-TOOL? ( str -- bool ) {: s:str :}   \ remote `command -v <tool>`
   CMD-RESET
   s" command -v" CMD-TOK  s CMD-TOK  s" >/dev/null 2>&1" CMD-TOK
   CMD$ SSH-EXEC RC>N 0= ;

: NEED-TOOL ( str -- )   \ fail-closed if a required remote tool is absent
   dup HAVE-TOOL? if drop exit then
   drop E-ZED-TOOLCHAIN throw ;

;package
