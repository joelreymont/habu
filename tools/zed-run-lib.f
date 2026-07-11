\ zed-run-lib.f - checked remote device-run harness over ssh/scp/rsync.
\
\ Runs commands and ships artifacts to a remote host (default alias `zed`) using
\ argv-spawned ssh/scp/rsync via lib/process-command.f - never a shell script and
\ never a shell-interpolated remote command built from untrusted input. Remote
\ command strings are assembled here from controlled path tokens only; test data
\ travels as files, not as command arguments. Failures map to named E-ZED-*
\ throw codes (lib/errors.f). Device availability is policy: when HABU_ZED is
\ unset or `0`, device suites SKIP explicitly with a printed reason; when set,
\ every failure is fail-closed. First consumer: the ptx device goldens.
\
\ Config via env (each has a default): HABU_ZED (availability), ZED_HOST (ssh
\ target, default `zed`), ZED_SSH/ZED_SCP/ZED_RSYNC (tool paths).

require lib/errors.f
require lib/string.f
require lib/fs.f
require lib/fs-mutate.f
require lib/memory.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/process-command.f

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

: ENV-OR ( ptr u8 n ptr u8 n -- ptr u8 n ) {: da:ptr du:n name:ptr nameu:n :}
   name nameu GETENV dup 0= if 2drop da du then ;

: HOST$ ( -- ptr u8 n )
   s" zed" s" ZED_HOST" ENV-OR ;

: SSH$ ( -- ptr u8 n )
   s" /usr/bin/ssh" s" ZED_SSH" ENV-OR ;

: SCP$ ( -- ptr u8 n )
   s" /usr/bin/scp" s" ZED_SCP" ENV-OR ;

: RSYNC$ ( -- ptr u8 n )
   s" /usr/bin/rsync" s" ZED_RSYNC" ENV-OR ;

\ ---- remote-command string builder (space-separated tokens) ------------------

: CMD-RESET ( -- )
   0 CMD-U ! ;

: CMD-RAW ( ptr u8 n -- ) {: a:ptr u:n :}
   CMD-U @ u + CMD-CAP > if E-ZED-ARG throw then
   a CMD-BUF CMD-U @ + u BYTE-COPY
   CMD-U @ u + CMD-U ! ;

: CMD-TOK ( ptr u8 n -- )
   CMD-U @ 0 > if s"  " CMD-RAW then
   CMD-RAW ;

: CMD$ ( -- ptr u8 n )
   CMD-BUF CMD-U @ ;

\ ---- rsync/scp destination: host:scratch/ ------------------------------------

: DEST+ ( ptr u8 n -- ) {: a:ptr u:n :}
   DEST-U @ u + DEST-CAP > if E-ZED-ARG throw then
   a DEST-BUF DEST-U @ + u BYTE-COPY
   DEST-U @ u + DEST-U ! ;

: DEST-BUILD ( -- )
   0 DEST-U !
   HOST$ DEST+  s" :" DEST+  SCRATCH-BUF SCRATCH-U @ DEST+  s" /" DEST+ ;

: DEST$ ( -- ptr u8 n )
   DEST-BUF DEST-U @ ;

\ ---- ssh/scp/rsync argv runners (capture stdout/stderr/rc) -------------------

: BATCH-OPTS ( -- )
   s" -o" >LEN PROC-CMD-ARG+  s" BatchMode=yes" >LEN PROC-CMD-ARG+
   s" -o" >LEN PROC-CMD-ARG+  s" ConnectTimeout=10" >LEN PROC-CMD-ARG+ ;

: SSH-EXEC ( ptr u8 n -- rc ) {: a:ptr u:n :}   \ run one remote command string
   PROC-CMD-RESET
   BATCH-OPTS
   HOST$ >LEN PROC-CMD-ARG+
   a u >LEN PROC-CMD-ARG+
   SSH$ >LEN TIMEOUT-MS @ >MS PROC-CMD-RUN-RC ;

: TIMED-OUT? ( -- bool )
   PROC-CMD-OUTCOME@ MATCH outcome
     exited OF drop 0 0= 0= ENDOF
     signaled OF drop 0 0= 0= ENDOF
     timeout OF 0 0= ENDOF
   ;MATCH ;

\ ---- newline/whitespace trim (mktemp output) ---------------------------------

: WS-BYTE? ( n -- bool ) {: c:n :}   \ trailing whitespace: LF $0A, CR $0D, space $20
   c $0A = c $0D = or c $20 = or ;

: TRIM-TRAIL ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   u TRIM-N !
   begin
      TRIM-N @ 0 > if a TRIM-N @ 1- + c@ WS-BYTE? else NO then
   while
      TRIM-N @ 1- TRIM-N !
   repeat
   a TRIM-N @ ;

: SCRATCH! ( ptr u8 n -- ) {: a:ptr u:n :}
   u SCRATCH-CAP > if E-ZED-ARG throw then
   a SCRATCH-BUF u BYTE-COPY
   u SCRATCH-U ! ;

public

\ ---- availability / skip policy ----------------------------------------------

: ENV-AVAILABLE? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 0= if NO exit then
   a u s" 0" STR= if NO exit then
   YES ;

: AVAILABLE? ( -- bool )
   s" HABU_ZED" GETENV ENV-AVAILABLE? ;

: SKIP ( ptr u8 n -- )
   s" SKIP (HABU_ZED unset/0): " type
   type cr ;

\ `REQUIRE` would be the natural name but `require` is a reserved loader token
\ (tools/reserved-name-lint.f), so the mandatory-device word follows NEED-TOOL.
: NEED-DEVICE ( -- )   \ device mandatory: fail-closed instead of skippable
   AVAILABLE? 0= if E-ZED-DISABLED throw then ;

: TIMEOUT! ( n -- )
   >MS TIMEOUT-MS ! ;

\ ---- outcome classification --------------------------------------------------

\ Remote tools may legitimately exit 255 (ptxas does), so a general command rc
\ cannot distinguish transport failure from tool failure. PING owns the
\ transport class: running `true`, ANY nonzero rc is unreachability. RUN-OK
\ classifies every nonzero rc of an ordinary command as a remote failure.
: UNREACH-OK ( rc -- )
   TIMED-OUT? if drop E-ZED-TIMEOUT throw then
   RC>N 0 <> if E-ZED-UNREACH throw then ;

: RUN-OK ( rc -- )
   TIMED-OUT? if drop E-ZED-TIMEOUT throw then
   RC>N 0 <> if E-ZED-RC throw then ;

: OUT$ ( -- ptr u8 n )
   PROC-CMD-OUT$ ;

: ERR$ ( -- ptr u8 n )
   PROC-CMD-ERR$ ;

: RC@ ( -- rc )
   PROC-CMD-RC@ ;

\ ---- remote command execution ------------------------------------------------

: RUN ( ptr u8 n -- rc )
   SSH-EXEC ;

: RUN-IN ( ptr u8 n -- rc ) {: a:ptr u:n :}   \ run with cwd = scratch
   CMD-RESET
   s" cd" CMD-TOK  SCRATCH-BUF SCRATCH-U @ CMD-TOK  s" &&" CMD-TOK  a u CMD-TOK
   CMD$ SSH-EXEC ;

: PING ( -- )   \ reachability probe: throws E-ZED-UNREACH if the host is down
   s" true" RUN UNREACH-OK ;

: SCRATCH$ ( -- ptr u8 n )
   SCRATCH-BUF SCRATCH-U @ ;

: SCRATCH-MK ( -- )   \ create ~/habu-zed-scratch/run.XXXXXX, store its path
   s" mkdir -p ~/habu-zed-scratch && mktemp -d ~/habu-zed-scratch/run.XXXXXX"
   RUN RUN-OK
   OUT$ TRIM-TRAIL SCRATCH! ;

: SCRATCH-RM ( -- )   \ remove the scratch tree (fail-closed)
   SCRATCH-U @ 0= if exit then
   CMD-RESET
   s" rm -rf" CMD-TOK  SCRATCH-BUF SCRATCH-U @ CMD-TOK
   CMD$ SSH-EXEC RUN-OK
   0 SCRATCH-U ! ;

: PUT-DIR ( ptr u8 n -- ) {: a:ptr u:n :}   \ rsync local dir into scratch/
   DEST-BUILD
   PROC-CMD-RESET
   s" -rlt" >LEN PROC-CMD-ARG+
   a u >LEN PROC-CMD-ARG+
   DEST$ >LEN PROC-CMD-ARG+
   RSYNC$ >LEN TIMEOUT-MS @ >MS PROC-CMD-RUN-RC
   RC>N 0= if exit then E-ZED-PUT throw ;

: PUT-FILE ( ptr u8 n -- ) {: a:ptr u:n :}   \ scp local file into scratch/
   DEST-BUILD
   PROC-CMD-RESET
   s" -o" >LEN PROC-CMD-ARG+  s" BatchMode=yes" >LEN PROC-CMD-ARG+
   a u >LEN PROC-CMD-ARG+
   DEST$ >LEN PROC-CMD-ARG+
   SCP$ >LEN TIMEOUT-MS @ >MS PROC-CMD-RUN-RC
   RC>N 0= if exit then E-ZED-PUT throw ;

: HAVE-TOOL? ( ptr u8 n -- bool ) {: a:ptr u:n :}   \ remote `command -v <tool>`
   CMD-RESET
   s" command -v" CMD-TOK  a u CMD-TOK  s" >/dev/null 2>&1" CMD-TOK
   CMD$ SSH-EXEC RC>N 0= ;

: NEED-TOOL ( ptr u8 n -- )   \ fail-closed if a required remote tool is absent
   2dup HAVE-TOOL? if 2drop exit then
   2drop E-ZED-TOOLCHAIN throw ;

end-package
