\ request-test.f - build and validate the checked request fixture, then run it
\ against the real browser turn protocol and CLI through bun.

require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/test.f
require test/wasm/harness.f

package BROWSER-REQUEST-TEST
private

60000 constant DEADLINE-MS
4096 constant CAP
CAP BUFFER: OUT
CAP BUFFER: ERR
FS-PATH-CAP BUFFER: DIR
FS-PATH-CAP BUFFER: MOD
variable MOD-U
FS-PATH-CAP BUFFER: EXE

: ARG+ ( ptr u8 n -- ) >LEN PROC-ARGV+ ;

: SETUP ( -- )
   s" browser-request" HB-TMP-MKDIR {: a:ptr u:n :}
   a u s" request.wasm" MOD JOIN-PATH MOD-U ! ;

: SPAWN ( ptr u8 len -- n )
   OUT CAP >LEN ERR CAP >LEN DEADLINE-MS >MS
   RUN-ARGV-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N
   {: outu:len erru:len rc:n :}
   rc 0<> if ERR erru LEN>N type cr then
   rc ;

: BUILD ( -- n )
   PROC-ARGV-RESET
   s" --load" ARG+ s" tools/wasm-build.f" ARG+ s" --" ARG+
   s" test/browser/request.f" ARG+ s" REQUEST-TURN:TURN" ARG+
   MOD MOD-U @ ARG+
   s" bin/hb" >LEN SPAWN ;

: PEER ( -- n )
   PROC-ARGV-RESET
   s" test/browser/request-peer.mjs" ARG+ MOD MOD-U @ ARG+
   s" bun" >LEN EXE RESOLVE-EXECUTABLE {: u:len :}
   EXE u SPAWN ;

public

: RUN ( -- )
   T-RESET
   SETUP
   s" browser request fixture builds" T-LABEL
   BUILD {: rc:n :}
   rc 0 T=
   rc 0<> if T-REPORT exit then
   s" browser request fixture validates" T-LABEL
   MOD MOD-U @ WASM-HARNESS:VALID? TTRUE
   s" tagged requests and legacy events run through the real browser host" T-LABEL
   PEER 0 T=
   T-REPORT ;

;package

BROWSER-REQUEST-TEST:RUN
