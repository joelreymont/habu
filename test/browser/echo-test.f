\ echo-test.f - the browser host's turns under bun: src/lib/browser/host-cli.mjs
\ runs test/browser/echo.f's module through src/lib/browser/turn.js
\ (src/lib/browser/host.f), and test/browser/stale.f's, whose hello states
\ another turn protocol revision; the modules and files stay in the printed
\ directory.
\ test/wasm/device.f runs it, as does
\ `bin/hb --load test/browser/echo-test.f` from the tree's root.

require src/lib/string.f
require src/lib/string-option.f
require src/lib/fmt.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/test.f
require test/wasm/harness.f
require test/browser/turn-revision.f

package BROWSER-ECHO-TEST
private

$10000 constant PAGE-BYTES
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
PATH-CAP BUFFER: STALE-MOD
variable STALE-MOD-U
PATH-CAP BUFFER: FILE
DYNAMIC-BUFFER PAYLOAD u8

: SMALL$ ( -- str )  s" the fixture's bytes" ;
: LANDED$ ( -- str )  s" landed: plate/top is 6 mm deep" ;
: REFUSED$ ( -- str )  s" refused: no extrusion ends at that face" ;
: DROPPED$ ( -- str )  s" dropped" ;

: FILE! ( str str -- )
   {: s:str name:str :}
   DIR DIR-U @ >str name FILE FS:JOIN-PATH {: fu:n :}
   FILE fu >str s FS:WRITE-ALL ;

: SETUP ( -- )
   s" browser-echo" FS:MKTEMP-DIR {: d:str :}
   d STR:DATA DIR d STR:LEN BYTE-COPY  d STR:LEN DIR-U !
   s" browser echo: module and files in " type  d type cr
   d s" echo.wasm" MOD FS:JOIN-PATH MOD-U !
   d s" stale.wasm" STALE-MOD FS:JOIN-PATH STALE-MOD-U !
   SMALL$ s" small.bin" FILE!
   LANDED$ s" landed.txt" FILE!
   REFUSED$ s" refused.txt" FILE!
   DROPPED$ s" dropped.txt" FILE! ;

: ARG+ ( str -- )  PROC-ARGV+ ;

: N-ARG+ ( n -- )
   SB-RESET FMT:SB-INT SB$ ARG+ ;

\ The program at the path, run with the arguments staged after
\ PROC-ARGV-RESET; answers its exit status, its stdout in OUT and its stderr in
\ ERR.
: SPAWN ( str -- n )
   OUT CAP >LEN  ERR CAP >LEN  PROCESS:WAIT-FOREVER
   RUN-ARGV-CAPTURE-OUTCOME PROCESS:ENDING>RC RC>N {: outu:len erru:len rc:n :}
   outu LEN>N OUT-U !  erru LEN>N ERR-U !
   rc ;

: SUCCEEDED ( n -- )
   {: rc:n :}
   rc 0<> if  ERR ERR-U @ >str type cr  then
   rc 0 T= ;

: BUILD ( -- )
   s" test/browser/echo.f builds into a module that validates" T-LABEL
   s" test/browser/echo.f" str> MOD MOD-U @ WASM-HARNESS:BUILD 0 T=
   MOD MOD-U @ WASM-HARNESS:VALID? TTRUE ;

: HOST-OPEN ( -- )
   PROC-ARGV-RESET
   s" src/lib/browser/host-cli.mjs" ARG+  MOD MOD-U @ >str ARG+ ;

\ A step at x and y: --pointer, --move or --release.
: AT+ ( str n n n -- )
   {: flag:str x:n y:n buttons:n :}
   flag ARG+  x N-ARG+  y N-ARG+  buttons N-ARG+  0 N-ARG+ ;

: TYPED+ ( str -- )
   s" --typed" ARG+  ARG+ ;

\ The oldest waiting request answered with the status and the file of this
\ name in DIR.
: ANSWER+ ( n str -- )
   {: status:n name:str :}
   s" --answer" ARG+  status N-ARG+
   DIR DIR-U @ >str name FILE FS:JOIN-PATH {: fu:n :}
   FILE fu >str ARG+ ;

: HOST ( -- n )
   s" bun" EXE RESOLVE-EXECUTABLE {: u:n :}
   EXE u >str SPAWN ;

\ ---- the lines the host prints, built in SB ---------------------------------
: LINE ( str -- )
   SB-APPEND  10 SB-APPEND-C ;

: TEXT-LINE ( str -- )
   s" text " SB-APPEND  LINE ;

\ The checksum echo.f's module answers for bytes.
: SUM ( str -- n )
   {: s:str :}
   0  s STR:LEN 0 ?do  31 *  s STR:DATA i + c@ +  $FFFFFFFF and  loop ;

: SUM-LINE ( str -- )
   SUM  s" text " SB-APPEND  FMT:SB-U  10 SB-APPEND-C ;

\ Start's lines, printed before its fetch is answered.
: STARTED ( -- )
   SB-RESET  S\" hello\ntext loading\nfetch fixture\n" SB-APPEND ;

: NIBBLE ( n -- )
   {: d:n :}
   d 10 < if  d 48 +  else  d 87 +  then  SB-APPEND-C ;

: HEX ( str -- )
   {: s:str :}
   s STR:LEN 0 ?do  s STR:DATA i + c@ dup 4 rshift NIBBLE $F and NIBBLE  loop ;

\ The line of a post typed text T makes: its path T, its body T twice.
: POST-LINE ( str -- )
   {: s:str :}
   s" post " SB-APPEND  s SB-APPEND  32 SB-APPEND-C
   s STR:LEN 2 * FMT:SB-U  32 SB-APPEND-C  s HEX  s HEX  10 SB-APPEND-C ;

\ ---- the cases --------------------------------------------------------------
: SMALL-CASE ( -- )
   HOST-OPEN  200 s" small.bin" ANSWER+  s" --pointer" 3 4 1 AT+
   HOST SUCCEEDED
   STARTED  SMALL$ SUM-LINE  s" 3,4" TEXT-LINE
   s" a small file: hello, loading, fetch fixture, the copy's checksum, then 3,4" T-LABEL
   OUT OUT-U @ >str SB$ T$= ;

\ A file of several pages, which reserve grows memory for. Byte i is the low byte
\ of i + i/509 (509 is prime), so a page copied to the wrong place changes the sum.
: LARGE-CASE ( -- )
   PAGE-BYTES 8 * 13 + {: u:n :}
   u PAYLOAD-RESERVE
   u 0 ?do  i  i 509 / +  $FF and  i PAYLOAD c!  loop
   0 PAYLOAD u >str s" large.bin" FILE!
   HOST-OPEN  200 s" large.bin" ANSWER+  s" --pointer" 0 0 1 AT+  s" --pointer" 1279 719 1 AT+
   HOST SUCCEEDED
   STARTED  0 PAYLOAD u >str SUM-LINE  s" 0,0" TEXT-LINE  s" 1279,719" TEXT-LINE
   s" a file of several pages: its reserve grows memory and the module reads the copy" T-LABEL
   OUT OUT-U @ >str SB$ T$= ;

: HELD-CASE ( -- )
   HOST-OPEN  200 s" small.bin" ANSWER+  s" 12" TYPED+
   s" --pointer" 3 4 1 AT+  s" --move" -5 6 1 AT+  s" --release" 7 8 0 AT+  s" ab" TYPED+
   200 s" landed.txt" ANSWER+
   HOST SUCCEEDED
   STARTED  SMALL$ SUM-LINE
   s" 12" TEXT-LINE  s" 12" POST-LINE
   s" 3,4" TEXT-LINE  s" moved -5,6" TEXT-LINE  s" released 7,8" TEXT-LINE
   s" ab" TEXT-LINE  s" ab" POST-LINE
   s" 200" TEXT-LINE  LANDED$ TEXT-LINE  s" fetch fixture" LINE
   s" outstanding post ab" LINE  s" outstanding fetch fixture" LINE
   s" held: input runs while a post waits; its 200 then answers status, body and fetch" T-LABEL
   OUT OUT-U @ >str SB$ T$= ;

: COPIED-CASE ( -- )
   HOST-OPEN  200 s" small.bin" ANSWER+  s" first" TYPED+  s" second!" TYPED+
   409 s" dropped.txt" ANSWER+
   HOST SUCCEEDED
   STARTED  SMALL$ SUM-LINE
   s" first" TEXT-LINE  s" first" POST-LINE
   s" second!" TEXT-LINE  s" second!" POST-LINE
   s" 409" TEXT-LINE  DROPPED$ TEXT-LINE  s" first" POST-LINE
   s" outstanding post second!" LINE  s" outstanding post first" LINE
   s" copied: a waiting post's path and body survive a rewrite of their buffer and growth" T-LABEL
   OUT OUT-U @ >str SB$ T$= ;

: CHAINED-CASE ( -- )
   HOST-OPEN  200 s" small.bin" ANSWER+  s" dé" TYPED+
   200 s" landed.txt" ANSWER+  200 s" small.bin" ANSWER+  422 s" refused.txt" ANSWER+
   HOST SUCCEEDED
   STARTED  SMALL$ SUM-LINE
   s" dé" TEXT-LINE  s" dé" POST-LINE
   s" 200" TEXT-LINE  LANDED$ TEXT-LINE  s" fetch fixture" LINE
   SMALL$ SUM-LINE  s" dé" POST-LINE
   s" 422" TEXT-LINE  REFUSED$ TEXT-LINE
   s" chained: a post's answer fetches, and that fetch goes before the second post" T-LABEL
   OUT OUT-U @ >str SB$ T$= ;

: TRAP$ ( -- str )
   ERR ERR-U @ >str 10 STR:INDEX-OF MATCH option
      none OF ERR 0 >str ENDOF
      some OF IDX>N ERR swap >str ENDOF
   ;MATCH ;

: ENDED-LINE ( str -- )
   s" the module trapped and has ended: Error: " SB-APPEND  LINE ;

: TRAPPED-CASE ( -- )
   HOST-OPEN  s" trap" TYPED+  200 s" small.bin" ANSWER+  s" --pointer" 1 2 1 AT+
   s" x" TYPED+
   HOST 1 T=
   STARTED
   s" trapped: after a trap the module prints nothing more" T-LABEL
   OUT OUT-U @ >str SB$ T$=
   TRAP$ {: t:str :}
   s" trapped: the host names the fault kind the module recorded" T-LABEL
   t s" fault kind 102 at " STR:STARTS-WITH? TTRUE
   s" trapped: the waiting fetch's answer, a pointer and typed text are refused" T-LABEL
   SB-RESET  t LINE  t ENDED-LINE  t ENDED-LINE  t ENDED-LINE
   ERR ERR-U @ >str SB$ T$= ;

: FAILED-CASE ( -- )
   HOST-OPEN  404 s" small.bin" ANSWER+  s" x" TYPED+  s" --fail" ARG+
   s" long path" TYPED+  s" neg path" TYPED+
   s" --pointer" 1 2 1 AT+  s" --fail" ARG+
   HOST 1 T=
   STARTED  s" x" TEXT-LINE  s" x" POST-LINE  s" 1,2" TEXT-LINE
   s" failed: a 404 fetch, a failed post and a post's long or negative path end their turns" T-LABEL
   OUT OUT-U @ >str SB$ T$=
   s" failed: each failure is printed, a failure with no request waiting too" T-LABEL
   SB-RESET  s" fixture: 404" LINE  s" x: failed" LINE
   s" a post's path of 28 bytes in its span of 27" LINE
   s" a post's path of -1 bytes in its span of 24" LINE
   s" --fail: no request is outstanding" LINE
   ERR ERR-U @ >str SB$ T$= ;

\ A hello one revision past the fixtures' ends the module before any other
\ event, the host naming both revisions.
: STALE-CASE ( -- )
   s" test/browser/stale.f builds into a module" T-LABEL
   s" test/browser/stale.f" str> STALE-MOD STALE-MOD-U @
   WASM-HARNESS:BUILD 0 T=
   s" stale: a hello of another revision ends the module, exit status 1" T-LABEL
   PROC-ARGV-RESET
   s" src/lib/browser/host-cli.mjs" ARG+
   STALE-MOD STALE-MOD-U @ >str ARG+
   HOST 1 T=
   s" stale: the host names the module's revision and its own" T-LABEL
   SB-RESET  s" the module follows turn protocol " SB-APPEND
   TURN-REVISION:PROTOCOL 1+ FMT:SB-INT
   s"  and this host turn protocol " SB-APPEND  TURN-REVISION:PROTOCOL FMT:SB-INT
   10 SB-APPEND-C
   ERR ERR-U @ >str SB$ T$= ;

public

: RUN ( -- )
   T-RESET
   SETUP
   BUILD
   SMALL-CASE
   LARGE-CASE
   HELD-CASE
   COPIED-CASE
   CHAINED-CASE
   TRAPPED-CASE
   FAILED-CASE
   STALE-CASE
   T-REPORT ;

;package

BROWSER-ECHO-TEST:RUN
