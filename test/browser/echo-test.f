\ echo-test.f - the browser host's turns under bun: src/lib/browser/host-cli.mjs
\ runs test/browser/echo.f's module through src/lib/browser/turn.js
\ (src/lib/browser/host.f); the module and files stay in the printed directory.
\ bun and wasm-tools must be on PATH, so this
\ is no row of the ordinary gate: test/wasm/device.f runs it, as does
\ `bin/hb --load test/browser/echo-test.f` from the tree's root.

require src/lib/string.f
require src/lib/fmt.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/test.f
require src/arch/wasm/profile.f
require test/wasm/harness.f

package BROWSER-ECHO-TEST
private

60000 constant DEADLINE-MS
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
PATH-CAP BUFFER: FILE
DYNAMIC-BUFFER PAYLOAD u8

: SMALL$ ( -- ptr u8 n )  s" the fixture's bytes" ;
: LANDED$ ( -- ptr u8 n )  s" landed: plate/top is 6 mm deep" ;
: REFUSED$ ( -- ptr u8 n )  s" refused: no extrusion ends at that face" ;
: DROPPED$ ( -- ptr u8 n )  s" dropped" ;

: FILE! ( ptr u8 n ptr u8 n -- )
   {: a:ptr u:n name:ptr nu:n :}
   DIR DIR-U @ name nu FILE FS:JOIN-PATH {: fu:n :}
   FILE fu a u FS:WRITE-ALL ;

: SETUP ( -- )
   s" browser-echo" FS:MKTEMP-DIR {: a:ptr u:n :}
   a DIR u BYTE-COPY  u DIR-U !
   s" browser echo: module and files in " type  DIR u type cr
   DIR u s" echo.wasm" MOD FS:JOIN-PATH MOD-U !
   SMALL$ s" small.bin" FILE!
   LANDED$ s" landed.txt" FILE!
   REFUSED$ s" refused.txt" FILE!
   DROPPED$ s" dropped.txt" FILE! ;

: ARG+ ( ptr u8 n -- )  >LEN PROC-ARGV+ ;

: N-ARG+ ( n -- )
   SB-RESET FMT:SB-INT SB$ ARG+ ;

\ The program at the path, run with the arguments staged after
\ PROC-ARGV-RESET; answers its exit status, its stdout in OUT and its stderr in
\ ERR.
: SPAWN ( ptr u8 len -- n )
   OUT CAP >LEN  ERR CAP >LEN  DEADLINE-MS >MS
   RUN-ARGV-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N {: outu:len erru:len rc:n :}
   outu LEN>N OUT-U !  erru LEN>N ERR-U !
   rc ;

: SUCCEEDED ( n -- )
   {: rc:n :}
   rc 0<> if  ERR ERR-U @ type cr  then
   rc 0 T= ;

: BUILD ( -- )
   PROC-ARGV-RESET
   s" --load" ARG+  s" tools/wasm-build.f" ARG+  s" --" ARG+
   s" test/browser/echo.f" ARG+  s" ECHO:TURN" ARG+  MOD MOD-U @ ARG+
   s" test/browser/echo.f builds into a module that validates" T-LABEL
   s" bin/hb" >LEN SPAWN SUCCEEDED
   MOD MOD-U @ WASM-HARNESS:VALID? TTRUE ;

: HOST-OPEN ( -- )
   PROC-ARGV-RESET
   s" src/lib/browser/host-cli.mjs" ARG+  MOD MOD-U @ ARG+ ;

\ A step at x and y: --pointer, --move or --release.
: AT+ ( ptr u8 n n n n -- )
   {: flag:ptr fu:n x:n y:n buttons:n :}
   flag fu ARG+  x N-ARG+  y N-ARG+  buttons N-ARG+  0 N-ARG+ ;

: TYPED+ ( ptr u8 n -- )
   s" --typed" ARG+  ARG+ ;

\ The oldest waiting request answered with the status and the file of this
\ name in DIR.
: ANSWER+ ( n ptr u8 n -- )
   {: status:n name:ptr nu:n :}
   s" --answer" ARG+  status N-ARG+
   DIR DIR-U @ name nu FILE FS:JOIN-PATH {: fu:n :}
   FILE fu ARG+ ;

: HOST ( -- n )
   s" bun" >LEN EXE RESOLVE-EXECUTABLE {: u:len :}
   EXE u SPAWN ;

\ ---- the lines the host prints, built in SB ---------------------------------
: LINE ( ptr u8 n -- )
   SB-APPEND  10 SB-APPEND-C ;

: TEXT-LINE ( ptr u8 n -- )
   s" text " SB-APPEND  LINE ;

\ The checksum echo.f's module answers for bytes.
: SUM ( ptr u8 n -- n )
   {: a:ptr u:n :}
   0  u 0 ?do  31 *  a i + c@ +  $FFFFFFFF and  loop ;

: SUM-LINE ( ptr u8 n -- )
   SUM  s" text " SB-APPEND  FMT:SB-U  10 SB-APPEND-C ;

\ Start's lines, printed before its fetch is answered.
: STARTED ( -- )
   SB-RESET  S\" hello\ntext loading\nfetch fixture\n" SB-APPEND ;

: NIBBLE ( n -- )
   {: d:n :}
   d 10 < if  d 48 +  else  d 87 +  then  SB-APPEND-C ;

: HEX ( ptr u8 n -- )
   {: a:ptr u:n :}
   u 0 ?do  a i + c@ dup 4 rshift NIBBLE $F and NIBBLE  loop ;

\ The line of a post typed text T makes: its path T, its body T twice.
: POST-LINE ( ptr u8 n -- )
   {: t:ptr u:n :}
   s" post " SB-APPEND  t u SB-APPEND  32 SB-APPEND-C
   u 2 * FMT:SB-U  32 SB-APPEND-C  t u HEX  t u HEX  10 SB-APPEND-C ;

\ ---- the cases --------------------------------------------------------------
: SMALL-CASE ( -- )
   HOST-OPEN  200 s" small.bin" ANSWER+  s" --pointer" 3 4 1 AT+
   HOST SUCCEEDED
   STARTED  SMALL$ SUM-LINE  s" 3,4" TEXT-LINE
   s" a small file: hello, loading, fetch fixture, the copy's checksum, then 3,4" T-LABEL
   OUT OUT-U @ SB$ T$= ;

\ A module's memory is at most its data base plus its file's bytes rounded up to
\ a page, so this file is larger than all of it. Byte i is the low byte of
\ i + i/509; 509 is prime, so the pages differ and a page copied to the wrong
\ place changes the checksum.
: LARGE-CASE ( -- )
   WPROF:IMAGE-BASE  MOD MOD-U @ FS:FILE-SIZE +  PAGE-BYTES +  13 + {: u:n :}
   u PAYLOAD-RESERVE
   u 0 ?do  i  i 509 / +  $FF and  i PAYLOAD c!  loop
   0 PAYLOAD u s" large.bin" FILE!
   HOST-OPEN  200 s" large.bin" ANSWER+  s" --pointer" 0 0 1 AT+  s" --pointer" 1279 719 1 AT+
   HOST SUCCEEDED
   STARTED  0 PAYLOAD u SUM-LINE  s" 0,0" TEXT-LINE  s" 1279,719" TEXT-LINE
   s" a file larger than the module's memory: its reserve grows it and the module reads the copy" T-LABEL
   OUT OUT-U @ SB$ T$= ;

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
   OUT OUT-U @ SB$ T$= ;

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
   OUT OUT-U @ SB$ T$= ;

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
   OUT OUT-U @ SB$ T$= ;

: TRAP$ ( -- ptr u8 n )
   ERR ERR-U @ 10 INDEX-OF MATCH option
      none OF ERR 0 ENDOF
      some OF IDX>N ERR swap ENDOF
   ;MATCH ;

: ENDED-LINE ( ptr u8 n -- )
   s" the module trapped and has ended: RuntimeError: " SB-APPEND  LINE ;

: TRAPPED-CASE ( -- )
   HOST-OPEN  s" trap" TYPED+  200 s" small.bin" ANSWER+  s" --pointer" 1 2 1 AT+
   s" x" TYPED+
   HOST 1 T=
   STARTED
   s" trapped: after a trap the module prints nothing more" T-LABEL
   OUT OUT-U @ SB$ T$=
   s" trapped: the waiting fetch's answer, a pointer and typed text are refused" T-LABEL
   TRAP$ {: t:ptr u:n :}
   SB-RESET  t u LINE  t u ENDED-LINE  t u ENDED-LINE  t u ENDED-LINE
   ERR ERR-U @ SB$ T$= ;

: FAILED-CASE ( -- )
   HOST-OPEN  404 s" small.bin" ANSWER+  s" x" TYPED+  s" --fail" ARG+
   s" long path" TYPED+  s" neg path" TYPED+
   s" --pointer" 1 2 1 AT+  s" --fail" ARG+
   HOST 1 T=
   STARTED  s" x" TEXT-LINE  s" x" POST-LINE  s" 1,2" TEXT-LINE
   s" failed: a 404 fetch, a failed post and a post's long or negative path end their turns" T-LABEL
   OUT OUT-U @ SB$ T$=
   s" failed: each failure is printed, a failure with no request waiting too" T-LABEL
   SB-RESET  s" fixture: 404" LINE  s" x: failed" LINE
   s" a post's path of 28 bytes in its span of 27" LINE
   s" a post's path of -1 bytes in its span of 24" LINE
   s" --fail: no request is outstanding" LINE
   ERR ERR-U @ SB$ T$= ;

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
   T-REPORT ;

;package

BROWSER-ECHO-TEST:RUN
