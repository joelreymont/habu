\ display-test.f - checked Wasm fixture through the foreign display peer.
\ bin/hb --load test/browser/display-test.f
\ The printed directory retains the module and exact CLI output. When Chrome is
\ installed, macOS's application or google-chrome on PATH, the peer's --chrome
\ option also runs the real browser (src/lib/browser/host.f); the row prints
\ which Chrome ran, or that none was found.
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/test.f
require test/wasm/harness.f

package BROWSER-DISPLAY-TEST
private

60000 constant DEADLINE-MS
4096 constant CAP
CAP BUFFER: OUT
CAP BUFFER: ERR
variable OUT-U
variable ERR-U
FS-PATH-CAP BUFFER: EXE
FS-PATH-CAP BUFFER: DIR
variable DIR-U
FS-PATH-CAP BUFFER: MOD
variable MOD-U
FS-PATH-CAP BUFFER: CHROME
variable CHROME-U

: ARG+ ( ptr u8 n -- ) >LEN PROC-ARGV+ ;

: SPAWN ( ptr u8 len -- n )
   OUT CAP >LEN ERR CAP >LEN DEADLINE-MS >MS
   RUN-ARGV-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N
   {: outu:len erru:len rc:n :}
   outu LEN>N OUT-U ! erru LEN>N ERR-U ! rc ;

: SETUP ( -- )
   s" browser-display" TMPDIR-MKDIR {: a:ptr u:n :}
   a DIR u BYTE-COPY u DIR-U !
   s" browser display: module and output in " type DIR u type cr
   DIR u s" display.wasm" MOD JOIN-PATH MOD-U ! ;

: BUILD ( -- n )
   PROC-ARGV-RESET
   s" --load" ARG+ s" tools/wasm-build.f" ARG+ s" --" ARG+
   s" test/browser/display.f" ARG+ s" DISPLAY:TURN" ARG+ MOD MOD-U @ ARG+
   s" bin/hb" >LEN SPAWN ;

: MAC-CHROME ( -- ptr u8 n )
   s" /Applications/Google Chrome.app/Contents/MacOS/Google Chrome" ;

\ Chrome's executable in CHROME, CHROME-U 0 when it is not installed.
: FIND-CHROME ( -- )
   MAC-CHROME EXISTS? if
      MAC-CHROME {: a:ptr u:n :} a CHROME u BYTE-COPY u CHROME-U ! exit
   then
   s" google-chrome" >LEN CHROME FIND-EXECUTABLE MATCH option
      none OF 0 ENDOF
      some OF LEN>N ENDOF
   ;MATCH CHROME-U ! ;

: PEER ( -- n )
   PROC-ARGV-RESET
   s" test/browser/display-peer.mjs" ARG+ MOD MOD-U @ ARG+ DIR DIR-U @ ARG+
   FIND-CHROME CHROME-U @ 0<> if
      s" --chrome" ARG+ CHROME CHROME-U @ ARG+
      s" browser display: Chrome " type CHROME CHROME-U @ type cr
   else
      s" browser display: no Chrome installed; browser cases skipped" type cr
   then
   s" bun" >LEN EXE RESOLVE-EXECUTABLE {: u:len :}
   EXE u SPAWN ;

public

: RUN ( -- )
   T-RESET SETUP
   s" browser display fixture builds" T-LABEL
   BUILD {: rc:n :}
   rc 0 T=
   rc 0<> if ERR ERR-U @ type cr T-REPORT exit then
   s" browser display fixture validates" T-LABEL
   MOD MOD-U @ WASM-HARNESS:VALID? TTRUE
   s" SVG/input records, scenes, identities, refusals and CLI" T-LABEL
   PEER 0 T=
   OUT OUT-U @ type ERR ERR-U @ type
   T-REPORT ;

;package

BROWSER-DISPLAY-TEST:RUN
