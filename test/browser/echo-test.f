\ echo-test.f - the browser host's turns under bun: lib/browser/host-cli.mjs
\ runs test/browser/echo.f's module through lib/browser/turn.js
\ (docs/browser-host.md). The module is built by tools/wasm-build.f's command
\ line and validated by wasm-tools. bun and wasm-tools must be on PATH, so this
\ is no row of the ordinary gate: test/wasm/device.f runs it, as does
\ `bin/hb --load test/browser/echo-test.f` from the tree's root.
\
\ Start answers hello, `fetch fixture` and the text "loading" in the buffer the
\ bytes run writes its text to, and the host shows that text before it answers
\ the fetch. The fixture's bytes answer the text of the checksum the module
\ computes over the copy in its memory, which this test computes over the
\ file, and each pointer answers "x,y". One run answers the fetch with a small
\ file. The other answers it with a file larger than the module's whole
\ memory, which is at most its data base plus the module file's bytes rounded
\ up to a page, so the host grows the memory past what the module had and the
\ module reads every byte it copied there.
\
\ Typed text posts twice, the second post waiting behind the first, and the
\ host prints each post's path, length and body in hex as it makes it:
\ - held: while a post waits, a pointer, a move to a negative x, a release and
\   more typed text each answer their text, and the more typed text's post is
\   made at once. The held post's 200 then answers its status, its whole body
\   and the fetch that follows, and the requests still waiting are printed.
\ - copied: the second typed text rewrites the buffer the first one's posts
\   name and grows memory while the first post waits; the second post of the
\   first text still reaches the server with that text's path and body.
\ - chained: the first post's 200 shows its status and fetches; that fetch is
\   made, and answered, before the second post, and the status is shown before
\   the fetch is answered, whose run rewrites its buffer.
\ - trapped: typed text that traps while start's fetch waits ends the
\   instance: the fetch's answer, a pointer and more typed text are refused,
\   and nothing more comes from the module.
\ - failed: start's fetch answered 404 fails, as in the browser; a post that
\   fails ends its turn before its second post; a post whose path is negative
\   or longer than its span is refused with nothing of its run shown; a
\   pointer still answers; and a failure with no request waiting is refused.
\
\ The module and the files stay in the printed directory.

require lib/string.f
require lib/fmt.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/test.f
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
FS-PATH-CAP BUFFER: EXE
FS-PATH-CAP BUFFER: DIR
variable DIR-U
FS-PATH-CAP BUFFER: MOD
variable MOD-U
FS-PATH-CAP BUFFER: FILE
DYNAMIC-BUFFER PAYLOAD u8

: SMALL$ ( -- ptr u8 n )  s" the fixture's bytes" ;
: LANDED$ ( -- ptr u8 n )  s" landed: plate/top is 6 mm deep" ;
: REFUSED$ ( -- ptr u8 n )  s" refused: no extrusion ends at that face" ;
: DROPPED$ ( -- ptr u8 n )  s" dropped" ;

\ The bytes written as the file name in DIR.
: FILE! ( ptr u8 n ptr u8 n -- )
   {: a:ptr u:n name:ptr nu:n :}
   DIR DIR-U @ name nu FILE JOIN-PATH {: fu:n :}
   FILE fu a u WRITE-ALL ;

: SETUP ( -- )
   s" browser-echo" HB-TMP-MKDIR {: a:ptr u:n :}
   a DIR u BYTE-COPY  u DIR-U !
   s" browser echo: module and files in " type  DIR u type cr
   DIR u s" echo.wasm" MOD JOIN-PATH MOD-U !
   SMALL$ s" small.bin" FILE!
   LANDED$ s" landed.txt" FILE!
   REFUSED$ s" refused.txt" FILE!
   DROPPED$ s" dropped.txt" FILE! ;

: ARG+ ( ptr u8 n -- )  >LEN PROC-ARGV+ ;

\ A number staged as an argument.
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

\ A run that should have exited 0, its stderr shown when it did not.
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

\ host-cli.mjs on the module, its steps staged after it.
: HOST-OPEN ( -- )
   PROC-ARGV-RESET
   s" lib/browser/host-cli.mjs" ARG+  MOD MOD-U @ ARG+ ;

\ A step at x and y: --pointer, --move or --release.
: AT+ ( ptr u8 n n n -- )
   {: flag:ptr fu:n x:n y:n :}
   flag fu ARG+  x N-ARG+  y N-ARG+ ;

: TYPED+ ( ptr u8 n -- )
   s" --typed" ARG+  ARG+ ;

\ The oldest waiting request answered with the status and the file of this
\ name in DIR.
: ANSWER+ ( n ptr u8 n -- )
   {: status:n name:ptr nu:n :}
   s" --answer" ARG+  status N-ARG+
   DIR DIR-U @ name nu FILE JOIN-PATH {: fu:n :}
   FILE fu ARG+ ;

\ The staged host run by bun; answers its exit status.
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

\ Start's lines before its fetch is answered, SB reset first.
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
   HOST-OPEN  200 s" small.bin" ANSWER+  s" --pointer" 3 4 AT+
   HOST SUCCEEDED
   STARTED  SMALL$ SUM-LINE  s" 3,4" TEXT-LINE
   s" a small file: hello, loading, fetch fixture, the copy's checksum, then 3,4" T-LABEL
   OUT OUT-U @ SB$ T$= ;

\ Byte i is the low byte of i + i/509; 509 is prime, so the pages differ and a
\ page copied to the wrong place changes the checksum.
: LARGE-CASE ( -- )
   WPROF:DATA-BASE  MOD MOD-U @ FILE-SIZE +  PAGE-BYTES +  13 + {: u:n :}
   u PAYLOAD-RESERVE
   u 0 ?do  i  i 509 / +  $FF and  i PAYLOAD c!  loop
   0 PAYLOAD u s" large.bin" FILE!
   HOST-OPEN  200 s" large.bin" ANSWER+  s" --pointer" 0 0 AT+  s" --pointer" 1279 719 AT+
   HOST SUCCEEDED
   STARTED  0 PAYLOAD u SUM-LINE  s" 0,0" TEXT-LINE  s" 1279,719" TEXT-LINE
   s" a file larger than the module's memory: the host grows it and the module reads the copy" T-LABEL
   OUT OUT-U @ SB$ T$= ;

: HELD-CASE ( -- )
   HOST-OPEN  200 s" small.bin" ANSWER+  s" 12" TYPED+
   s" --pointer" 3 4 AT+  s" --move" -5 6 AT+  s" --release" 7 8 AT+  s" ab" TYPED+
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

\ ERR's first line, the trap's message.
: TRAP$ ( -- ptr u8 n )
   ERR ERR-U @ 10 INDEX-OF MATCH option
      none OF ERR 0 ENDOF
      some OF IDX>N ERR swap ENDOF
   ;MATCH ;

\ The line refusing a turn of the module the trap with this message ended.
: ENDED-LINE ( ptr u8 n -- )
   s" the module trapped and has ended: RuntimeError: " SB-APPEND  LINE ;

: TRAPPED-CASE ( -- )
   HOST-OPEN  s" trap" TYPED+  200 s" small.bin" ANSWER+  s" --pointer" 1 2 AT+
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
   s" --pointer" 1 2 AT+  s" --fail" ARG+
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
