\ display-test.f - checked Wasm fixture through the foreign display peer; the
\ printed directory retains the module and exact CLI output. Chrome runs only
\ when the test is loaded with `-- --chrome`: then --chrome also runs the real
\ browser (src/lib/browser/host.f), macOS's application or google-chrome on
\ PATH. Each delivery kind (bytes, load, focus, highlight) runs through
\ src/lib/browser/host-cli.mjs once and four times; both end with the same
\ memory because every run frees its page.
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/test/assert.f
require test/wasm/harness.f
require test/browser/memory.f

package BROWSER-DISPLAY-TEST
private

4096 constant CAP
CAP BUFFER: OUT
CAP BUFFER: ERR
variable OUT-U
variable ERR-U
PATH-CAP BUFFER: EXE
PATH-CAP BUFFER: DIR
variable DIR-U
PATH-CAP BUFFER: MOD
variable MOD-U
PATH-CAP BUFFER: CHROME
variable CHROME-U
PATH-CAP BUFFER: FILE
variable FILE-U

: ARG+ ( str -- ) PROC-ARGV+ ;

: SPAWN ( str -- n )
   OUT CAP >LEN ERR CAP >LEN PROCESS:WAIT-FOREVER
   RUN-ARGV-CAPTURE-OUTCOME PROCESS:ENDING>RC RC>N
   {: outu:len erru:len rc:n :}
   outu LEN>N OUT-U ! erru LEN>N ERR-U ! rc ;

: SETUP ( -- )
   s" browser-display" FS:MKTEMP-DIR {: d:str :}
   d STR:DATA DIR d STR:LEN BYTE-COPY d STR:LEN DIR-U !
   s" browser display: module and output in " type d type cr
   d s" display.wasm" MOD FS:JOIN-PATH MOD-U !
   d s" scene.bin" FILE FS:JOIN-PATH FILE-U !
   FILE FILE-U @ >str s" sixteen bytes ok" FS:WRITE-ALL ;

: BUILD ( -- n )
   s" test/browser/display.f" MOD MOD-U @ >str WASM-HARNESS:BUILD ;

: MAC-CHROME ( -- str )
   s" /Applications/Google Chrome.app/Contents/MacOS/Google Chrome" ;

: CHROME-ASKED? ( -- bool )
   SCRIPT-ARGC 0 ?do
      i SCRIPT-ARGV$ s" --chrome" STR:= if unloop true exit then
   loop false ;

\ Chrome's executable in CHROME, CHROME-U 0 when it is not installed.
: FIND-CHROME ( -- )
   MAC-CHROME FS:EXISTS? if
      MAC-CHROME {: m:str :}
      m STR:DATA CHROME m STR:LEN BYTE-COPY m STR:LEN CHROME-U ! exit
   then
   s" google-chrome" CHROME FIND-EXECUTABLE MATCH option
      none OF 0 ENDOF
      some OF ENDOF
   ;MATCH CHROME-U ! ;

: PEER ( -- n )
   PROC-ARGV-RESET
   s" test/browser/display-peer.mjs" ARG+
   MOD MOD-U @ >str ARG+ DIR DIR-U @ >str ARG+
   CHROME-ASKED? if
      FIND-CHROME CHROME-U @ 0<> if
         s" --chrome" ARG+ CHROME CHROME-U @ >str ARG+
         s" browser display: Chrome " type
         CHROME CHROME-U @ >str type cr
      else
         s" --chrome: Chrome is installed" T-LABEL CHROME-U @ 0<> TTRUE
      then
   else
      s" browser display: Chrome cases run only with -- --chrome" type cr
   then
   s" bun" EXE RESOLVE-EXECUTABLE {: u:n :}
   EXE u >str SPAWN ;

public

: RUN ( -- )
   T-RESET SETUP
   s" browser display fixture builds" T-LABEL
   BUILD {: rc:n :}
   rc 0 T=
   rc 0<> if ERR ERR-U @ >str type cr T-REPORT exit then
   s" browser display fixture validates" T-LABEL
   MOD MOD-U @ >str WASM-HARNESS:VALID? TTRUE
   PEER {: peer:n :}
   s" SVG/input records, scenes, identities, rejections and CLI" T-LABEL
   peer 0 T=
   OUT OUT-U @ >str type ERR ERR-U @ >str type
   s" four fetched answers end with the memory one does"
      MOD MOD-U @ >str [: \ the answer follows the typed "fetch" that asks for it
         s" --typed" ARG+ s" fetch" ARG+
         s" --answer" ARG+ s" 200" ARG+ FILE FILE-U @ >str ARG+ ;]
      BROWSER-MEMORY-TEST:FREED
   s" four loads end with the memory one does"
      MOD MOD-U @ >str [: s" --load" ARG+ FILE FILE-U @ >str ARG+ ;]
      BROWSER-MEMORY-TEST:FREED
   s" four focus turns end with the memory one does"
      MOD MOD-U @ >str [: s" --focus" ARG+ s" 000102030405060708090a0b0c0d0e0f" ARG+ ;]
      BROWSER-MEMORY-TEST:FREED
   s" four highlight turns end with the memory one does"
      MOD MOD-U @ >str [: s" --highlight" ARG+ s" 0f0e0d0c0b0a09080706050403020100" ARG+ ;]
      BROWSER-MEMORY-TEST:FREED
   T-REPORT ;

;package

BROWSER-DISPLAY-TEST:RUN
