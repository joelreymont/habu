\ app-test.f - the turn protocol's rpc, after and read records and its
\ notified, tick, file and chunk events under bun:
\ src/lib/browser/host-cli.mjs runs test/browser/app.f's module, built by
\ tools/hb-build.f, through src/lib/browser/turn.js. bun and wasm-tools must
\ be on PATH, so this is no row of the ordinary gate: test/wasm/device.f runs
\ it, as does `bin/hb --load test/browser/app-test.f` from the tree's root.
\ With Chrome installed (macOS's application or google-chrome on PATH) the
\ --chrome check writes MCP-APP:PAGE+ of the module and runs it in Chrome under
\ test/browser/app-peer.mjs, which plays ChatGPT's MCP Apps host.

require src/lib/error.f
require src/lib/string.f
require src/lib/span.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/test.f
require test/wasm/harness.f
require test/browser/memory.f
require src/lib/byte-buffer.f
require src/lib/browser/app.f

package BROWSER-APP-TEST
private

60000 constant DEADLINE-MS
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
PATH-CAP BUFFER: WANT-DIR
variable WANT-U
PATH-CAP BUFFER: GOT-DIR
variable GOT-U
PATH-CAP BUFFER: FILE
PATH-CAP BUFFER: CHROME
variable CHROME-U
BUF:HDR-BYTES BUFFER: WASM
BUF:HDR-BYTES BUFFER: PAGE
\ A refused case's expected output, kept apart from SB, which labels build in.
CAP BUFFER: BEFORE
variable BEFORE-U

: RESULT$ ( -- ptr u8 n )  s\" {\"result\":{\"content\":[]}}" ;
: ERROR$ ( -- ptr u8 n )  s\" {\"error\":{\"code\":-32601,\"message\":\"Method not found\"}}" ;
: PARAMS$ ( -- ptr u8 n )  s\" {\"structuredContent\":{\"name\":\"Zürich ✓\"}}" ;
\ A method of non-ASCII text, so its length counts bytes, not characters.
: METHOD$ ( -- ptr u8 n )  s" ui/notifications/tool-résult" ;
: DATA$ ( -- ptr u8 n )  s\" \x00\x01\x02\x03\x04\x05\x06\xFD\xFE\xFF" ;
: NAME$ ( -- ptr u8 n )  s" naïve.bin" ;
\ The composed page's title, which HTML must escape and the view must pass
\ to the host as it is, with its space run, LF and CR.
: TITLE$ ( -- ptr u8 n )  s\" Zürich  <part>\x0A& \"co\"\x0D" ;

: OUT$ ( -- ptr u8 n )  OUT OUT-U @ ;
: ERR$ ( -- ptr u8 n )  ERR ERR-U @ ;

: FILE! ( ptr u8 n ptr u8 n -- )
   {: a:ptr u:n name:ptr nu:n :}
   DIR DIR-U @ name nu FILE FS:JOIN-PATH {: fu:n :}
   FILE fu a u FS:WRITE-ALL ;


: SUBDIR ( ptr u8 n ptr u8 -- n )
   {: name:ptr nu:n dst:ptr :}
   DIR DIR-U @ name nu dst FS:JOIN-PATH {: u:n :}
   dst u FS:MAKE-DIR
   u ;


: SETUP ( -- )
   s" browser-app" FS:MKTEMP-DIR {: a:ptr u:n :}
   a DIR u BYTE-COPY  u DIR-U !
   s" browser app: module, answers and transcripts in " type  DIR u type cr
   DIR u s" app.wasm" MOD FS:JOIN-PATH MOD-U !
   s" expected" WANT-DIR SUBDIR WANT-U !
   s" actual" GOT-DIR SUBDIR GOT-U !
   RESULT$ s" result.json" FILE!
   ERROR$ s" error.json" FILE!
   PARAMS$ s" params.json" FILE!
   DATA$ s" data.bin" FILE! ;


: ARG+ ( ptr u8 n -- )  >LEN PROC-ARGV+ ;

: FILE-ARG+ ( ptr u8 n -- )
   {: name:ptr nu:n :}
   DIR DIR-U @ name nu FILE FS:JOIN-PATH {: fu:n :}
   FILE fu ARG+ ;


: SPAWN ( ptr u8 len -- n )
   OUT CAP >LEN  ERR CAP >LEN  DEADLINE-MS >MS
   RUN-ARGV-CAPTURE-OUTCOME PROCESS:ENDING>RC RC>N {: outu:len erru:len rc:n :}
   outu LEN>N OUT-U !  erru LEN>N ERR-U !
   rc ;


: SUCCEEDED ( n -- )
   {: rc:n :}
   rc 0<> if  ERR$ type cr  then
   rc 0 T= ;


: BUILD ( -- n )
   PROC-ARGV-RESET
   s" --load" ARG+  s" tools/hb-build.f" ARG+  s" --" ARG+  s" --target" ARG+  s" wasm" ARG+
   s" test/browser/app.f" ARG+  s" -o" ARG+  MOD MOD-U @ ARG+
   s" bin/hb" >LEN SPAWN ;


: HOST-OPEN ( -- )
   PROC-ARGV-RESET
   s" src/lib/browser/host-cli.mjs" ARG+  MOD MOD-U @ ARG+ ;


: HOST ( -- n )
   s" bun" >LEN EXE RESOLVE-EXECUTABLE {: u:len :}
   EXE u SPAWN ;


: TYPED+ ( ptr u8 n -- )  s" --typed" ARG+  ARG+ ;

\ The waiting rpc with this id answered with DIR's file of this name.
: ANSWER-RPC+ ( ptr u8 n ptr u8 n -- )
   {: id:ptr iu:n name:ptr nu:n :}
   s" --answer-rpc" ARG+  id iu ARG+  name nu FILE-ARG+ ;


: FAIL-RPC+ ( ptr u8 n -- )  s" --fail-rpc" ARG+  ARG+ ;
: TICK+ ( -- )  s" --tick" ARG+ ;
: NOTIFY+ ( -- )  s" --notify" ARG+  METHOD$ ARG+  s" params.json" FILE-ARG+ ;
: FILE+ ( -- )  s" --file" ARG+  NAME$ ARG+  s" data.bin" FILE-ARG+ ;

\ ---- the lines the host prints, built in SB ---------------------------------
: LINE ( ptr u8 n -- )
   SB-APPEND  10 SB-APPEND-C ;


: HELLO ( -- )
   SB-RESET  s" hello" LINE ;

\ The fixture's rpc, tools/call of get_project, as the host prints it.
: CALL-LINE ( ptr u8 n -- )
   {: id:ptr iu:n :}
   s" rpc tools/call " SB-APPEND  id iu SB-APPEND
   s\"  {\"name\":\"get_project\"}" LINE ;


\ The text an answered run makes: c, the id, then the response's bytes.
: ANSWERED-LINE ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: c:ptr cu:n id:ptr iu:n body:ptr bu:n :}
   s" text answered " SB-APPEND  c cu SB-APPEND  32 SB-APPEND-C
   id iu SB-APPEND  32 SB-APPEND-C  body bu LINE ;


: RESULT-BODY$ ( -- ptr u8 n )  s\" {\"content\":[]}" ;
: ERROR-BODY$ ( -- ptr u8 n )  s\" {\"code\":-32601,\"message\":\"Method not found\"}" ;

\ Writes the expected text and the actual bytes as this name in DIR's expected
\ and actual directories, then compares them.
: SAME ( ptr u8 n ptr u8 n -- )
   {: got:ptr gu:n name:ptr nu:n :}
   WANT-DIR WANT-U @ name nu FILE FS:JOIN-PATH {: wu:n :}
   FILE wu SB$ FS:WRITE-ALL
   GOT-DIR GOT-U @ name nu FILE FS:JOIN-PATH {: fu:n :}
   FILE fu got gu FS:WRITE-ALL
   got gu SB$ T$= ;

\ ---- the cases --------------------------------------------------------------

: RPC-CASE ( -- )
   HOST-OPEN
   s" rpc" TYPED+  s" 1" s" result.json" ANSWER-RPC+
   s" rpc" TYPED+  s" 1" s" error.json" ANSWER-RPC+
   s" rpc" TYPED+  s" 1" FAIL-RPC+
   s" rpc: every turn and answer runs" T-LABEL
   HOST SUCCEEDED
   HELLO
   s" 1" CALL-LINE  s" 1" s" 1" RESULT-BODY$ ANSWERED-LINE
   s" 1" CALL-LINE  s" 2" s" 1" ERROR-BODY$ ANSWERED-LINE
   s" 1" CALL-LINE  s" 0" s" 1" s" Error: rpc 1: failed" ANSWERED-LINE
   s" rpc: a result answers c 1, an error c 2 and a lost parent c 0, at the id" T-LABEL
   OUT$ s" rpc.out" SAME ;


: ORDER-CASE ( -- )
   HOST-OPEN  s" two" TYPED+
   s" 2" s" result.json" ANSWER-RPC+  s" 1" s" error.json" ANSWER-RPC+
   s" order: every turn and answer runs" T-LABEL
   HOST SUCCEEDED
   HELLO  s" 1" CALL-LINE  s" 2" CALL-LINE
   s" 1" s" 2" RESULT-BODY$ ANSWERED-LINE
   s" 2" s" 1" ERROR-BODY$ ANSWERED-LINE
   s" order: two rpcs answered in reverse order each land at their own id" T-LABEL
   OUT$ s" order.out" SAME ;


: AFTER-CASE ( -- )
   HOST-OPEN  s" after" TYPED+  TICK+
   s" after: every turn runs" T-LABEL
   HOST SUCCEEDED
   HELLO  s" after 250 7" LINE  s" text tick 7" LINE
   s" after: the host is asked for 250 ms and the tick carries the token" T-LABEL
   OUT$ s" after.out" SAME ;


: NOTIFY-CASE ( -- )
   HOST-OPEN  NOTIFY+
   s" notified: the turn runs" T-LABEL
   HOST SUCCEEDED
   HELLO  s" text notified " SB-APPEND  METHOD$ SB-APPEND  32 SB-APPEND-C  PARAMS$ LINE
   s" notified: the method and params arrive exact, non-ASCII included" T-LABEL
   OUT$ s" notified.out" SAME ;


: FILE-LINE ( -- )
   s" text file " SB-APPEND  NAME$ SB-APPEND  s"  10 1" LINE ;


: FILE-CASE ( -- )
   HOST-OPEN  FILE+  s" read start" TYPED+  s" read end" TYPED+
   s" file: every turn and read runs" T-LABEL
   HOST SUCCEEDED
   HELLO  FILE-LINE
   s" read 1 0 3" LINE  s" text chunk 1 0 1 0,1,2," LINE
   s" read 1 7 3" LINE  s" text chunk 1 7 1 253,254,255," LINE
   s" file: the name, size and number arrive; reads at 0 and at the end answer their bytes" T-LABEL
   OUT$ s" file.out" SAME ;


\ One round of each delivery four times ends with the memory one round does:
\ each answered, notified, file and chunk run frees its delivery.
: FREED-CASE ( -- )
   s" freed: four rounds of deliveries end with the memory one does"
   MOD MOD-U @ [: s" rpc" TYPED+  s" 1" s" result.json" ANSWER-RPC+
      NOTIFY+  FILE+  s" read start" TYPED+ ;]
   BROWSER-MEMORY-TEST:FREED ;


: MALFORMED ( ptr u8 n ptr u8 n -- )
   {: cmd:ptr cu:n what:ptr wu:n :}
   SB-RESET  s" refused " SB-APPEND  cmd cu SB-APPEND  s" : " SB-APPEND
   what wu SB-APPEND  SB$ T-LABEL ;


\ Typed text whose run leaves a malformed record, after the steps already
\ staged: the turn fails, and the host prints only the lines given and no rpc,
\ after or read.
: REFUSED ( ptr u8 n ptr u8 n -- )
   {: cmd:ptr cu:n before:ptr bu:n :}
   cmd cu TYPED+
   cmd cu s" the turn fails" MALFORMED
   HOST 1 T=
   cmd cu s" the record reaches no host" MALFORMED
   OUT$ before bu T$= ;


: REJECT ( ptr u8 n ptr u8 n -- )
   {: cmd:ptr cu:n why:ptr wu:n :}
   HOST-OPEN  cmd cu s\" hello\n" REFUSED
   cmd cu s" the failure names the reason" MALFORMED
   ERR$ why wu CONTAINS? TTRUE ;


\ A read after the file event: its line is printed before the refusal.
: REJECT-READ ( ptr u8 n ptr u8 n -- )
   {: cmd:ptr cu:n why:ptr wu:n :}
   HELLO  FILE-LINE
   SB$ {: a:ptr u:n :}
   a BEFORE u BYTE-COPY  u BEFORE-U !
   HOST-OPEN  FILE+  cmd cu BEFORE BEFORE-U @ REFUSED
   cmd cu s" the failure names the reason" MALFORMED
   ERR$ why wu CONTAINS? TTRUE ;


: MALFORMED-CASES ( -- )
   s" negative after" s" an after of -1 ms" REJECT
   s" short" s" method length needs 8 bytes" REJECT
   s" past" s" an rpc's method of 200 bytes" REJECT
   s" empty method" s" an rpc's method of 0 bytes" REJECT
   s" array params" s" params JSON must be an object" REJECT
   s" bad json" s" params JSON is invalid" REJECT
   s" zero id" s" an rpc needs a positive id" REJECT
   s" read past" s" outside file 1's 10" REJECT-READ
   s" read unknown" s" which no file event delivered" REJECT-READ
   \ TextDecoder's wording is the engine's.
   HOST-OPEN  s" bad utf8" s\" hello\n" REFUSED
   s" bad utf8" s" the failure names the reason" MALFORMED
   ERR$ s" Invalid byte sequence" CONTAINS?
   ERR$ s" encoded data was not valid" CONTAINS?  or TTRUE ;

\ ---- the --chrome check --------------------------------------------------------

: MAC-CHROME ( -- ptr u8 n )
   s" /Applications/Google Chrome.app/Contents/MacOS/Google Chrome" ;

\ Chrome's executable in CHROME, CHROME-U 0 when it is not installed.
: FIND-CHROME ( -- )
   MAC-CHROME FS:EXISTS? if
      MAC-CHROME {: a:ptr u:n :} a CHROME u BYTE-COPY u CHROME-U ! exit
   then
   s" google-chrome" >LEN CHROME FIND-EXECUTABLE MATCH option
      none OF 0 ENDOF
      some OF LEN>N ENDOF
   ;MATCH CHROME-U ! ;

\ MCP-APP:PAGE+ of the built module, written as DIR's page.html.
: PAGE! ( -- )
   MOD MOD-U @ FS:FILE-SIZE {: wu:n :}
   WASM wu BUF:N>BLEN BUF:INIT
   WASM wu BUF:N>BLEN BUF:GROW SPAN:$ {: w:ptr cap:n :}
   MOD MOD-U @ w cap FS:READ-ALL cap <> if E-FS-IO throw then
   PAGE wu BUF:N>BLEN BUF:INIT
   w cap TITLE$ PAGE MCP-APP:PAGE+
   PAGE BUF:SPAN$ BUF:BLEN>N s" page.html" FILE!
   PAGE BUF:DISPOSE  WASM BUF:DISPOSE ;


: CHROME-CASE ( -- )
   FIND-CHROME CHROME-U @ 0= if
      s" browser app: no Chrome installed; browser cases skipped" type cr exit
   then
   s" browser app: Chrome " type CHROME CHROME-U @ type cr
   PAGE!
   PROC-ARGV-RESET
   s" test/browser/app-peer.mjs" ARG+  s" page.html" FILE-ARG+  DIR DIR-U @ ARG+
   TITLE$ ARG+  s" --chrome" ARG+  CHROME CHROME-U @ ARG+
   s" --chrome: the composed page runs as an MCP App under ChatGPT's CSP" T-LABEL
   HOST {: rc:n :}
   OUT$ type  ERR$ type
   rc 0 T= ;

public

: RUN ( -- )
   T-RESET
   SETUP
   s" browser app fixture builds" T-LABEL
   BUILD {: rc:n :}
   rc 0 T=
   rc 0<> if  ERR$ type cr  T-REPORT exit  then
   s" browser app fixture validates" T-LABEL
   MOD MOD-U @ WASM-HARNESS:VALID? TTRUE
   RPC-CASE
   ORDER-CASE
   AFTER-CASE
   NOTIFY-CASE
   FILE-CASE
   FREED-CASE
   MALFORMED-CASES
   CHROME-CASE
   T-REPORT ;

;package

BROWSER-APP-TEST:RUN
