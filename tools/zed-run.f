\ zed-run.f - CLI probe over the ZED remote harness.
\
\ Usage: bin/hb --load tools/zed-run.f -- <remote command words...>
\
\ Joins the script arguments into one remote command, runs it on the ZED host
\ via the checked ssh harness (tools/zed-run-lib.f), prints captured stdout and
\ stderr, and exits nonzero (fail-closed) if the host is unreachable or the
\ remote command failed. When HABU_ZED is unset/0 it prints an explicit SKIP
\ line and exits 0 - the device is a policy opt-in, never a silent no-op.

require tools/zed-run-lib.f

package ZED

: CLI-BUILD ( -- )   \ join script argv into the remote command buffer
   CMD-RESET
   0 begin dup SCRIPT-ARGC < while
      dup SCRIPT-ARGV$ CMD-TOK
      1 +
   repeat drop ;

: CLI-REPORT ( -- )   \ echo captured remote output
   OUT$ type
   ERR$ type ;

: MAIN ( -- )
   AVAILABLE? 0= if s" zed-run needs HABU_ZED" SKIP exit then
   SCRIPT-ARGC 0= if s" zed-run: no command given" type cr E-ZED-ARG throw then
   PING
   CLI-BUILD
   CMD$ RUN {: rc:rc :}
   CLI-REPORT
   rc RUN-OK ;

MAIN

end-package
