\ zed-run.f - CLI probe over the ZED remote harness (tools/zed-run-lib.f).
\ Usage: bin/hb --load tools/zed-run.f -- <remote command words...>
\ Exits nonzero if the host is unreachable or the remote command failed. With
\ HABU_ZED unset or 0 it prints an explicit SKIP line and exits 0.

require tools/zed-run-lib.f

package ZED

: CLI-BUILD ( -- )
   CMD-RESET
   0 begin dup SCRIPT-ARGC < while
      dup SCRIPT-ARGV$ CMD-TOK
      1 +
   repeat drop ;

: CLI-REPORT ( rc CMD:command -- CMD:command )
   >r {: rc:rc :} r>
   CMD:OUT$ type
   CMD:ERR$ type
   rc RUN-OK ;

: MAIN ( -- )
   AVAILABLE? 0= if s" zed-run needs HABU_ZED" SKIP exit then
   SCRIPT-ARGC 0= if s" zed-run: no command given" type cr E-ZED-ARG throw then
   PING
   CLI-BUILD
   CMD$ [: CLI-REPORT ;] WITH-RUN ;

MAIN

;package
