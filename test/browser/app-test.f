\ app-test.f - the turn protocol's rpc, after and read records and its notified,
\ tick, file and chunk events under bun: src/lib/browser/host-cli.mjs runs
\ test/browser/app.f's module through src/lib/browser/turn.js.
\ test/wasm/device.f runs it, as does `bin/hb --load test/browser/app-test.f`
\ from the tree's root. Chrome runs only when the test is loaded with
\ `-- --chrome`: then the --chrome check writes MCP-APP:PAGE+ as page.html
\ and MCP-APP:CODE+ of the module as code.json beside it, and runs the page in
\ Chrome (macOS's application or google-chrome on PATH) under
\ test/browser/app-peer.mjs, which plays ChatGPT's MCP Apps host and the
\ server's view socket.

require src/lib/error.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/test.f
require test/wasm/harness.f
require test/browser/memory.f
require src/lib/heap.f
require src/lib/vector.f
require src/lib/browser/app.f

package BROWSER-APP-TEST
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
PATH-CAP BUFFER: WANT-DIR
variable WANT-U
PATH-CAP BUFFER: GOT-DIR
variable GOT-U
PATH-CAP BUFFER: FILE
PATH-CAP BUFFER: CHROME
variable CHROME-U
TYPED-VARIABLE WASM ^VEC:vector<u8>
TYPED-VARIABLE PAGE ^VEC:vector<u8>
TYPED-VARIABLE CODE ^VEC:vector<u8>
\ A refused case's expected output, kept apart from SB, which labels build in.
CAP BUFFER: BEFORE
variable BEFORE-U

: RESULT$ ( -- str )  s\" {\"result\":{\"content\":[]}}" ;
: ERROR$ ( -- str )  s\" {\"error\":{\"code\":-32601,\"message\":\"Method not found\"}}" ;
: PARAMS$ ( -- str )  s\" {\"structuredContent\":{\"name\":\"Zürich ✓\"}}" ;
\ A method of non-ASCII text, so its length counts bytes, not characters.
: METHOD$ ( -- str )  s" ui/notifications/tool-résult" ;
: DATA$ ( -- str )  s\" \x00\x01\x02\x03\x04\x05\x06\xFD\xFE\xFF" ;
: NAME$ ( -- str )  s" naïve.bin" ;
\ The composed page's title, which HTML must escape and the view must pass
\ to the host as it is, with its space run, LF and CR.
: TITLE$ ( -- str )  s\" Zürich  <part>\x0A& \"co\"\x0D" ;

: OUT$ ( -- str )  OUT OUT-U @ >str ;
: ERR$ ( -- str )  ERR ERR-U @ >str ;

: FILE! ( str str -- )
   {: s:str name:str :}
   DIR DIR-U @ >str name FILE FS:JOIN-PATH {: fu:n :}
   FILE fu >str s FS:WRITE-ALL ;


: SUBDIR ( str ^u8 -- n )
   {: name:str dst:^ :}
   DIR DIR-U @ >str name dst FS:JOIN-PATH {: u:n :}
   dst u >str FS:MAKE-DIR
   u ;


: SETUP ( -- )
   s" browser-app" FS:MKTEMP-DIR {: d:str :}
   d STR:DATA DIR d STR:LEN BYTE-COPY  d STR:LEN DIR-U !
   s" browser app: module, answers and transcripts in " type  d type cr
   d s" app.wasm" MOD FS:JOIN-PATH MOD-U !
   s" expected" WANT-DIR SUBDIR WANT-U !
   s" actual" GOT-DIR SUBDIR GOT-U !
   RESULT$ s" result.json" FILE!
   ERROR$ s" error.json" FILE!
   PARAMS$ s" params.json" FILE!
   DATA$ s" data.bin" FILE! ;


: ARG+ ( str -- )  PROC-ARGV+ ;

: FILE-ARG+ ( str -- )
   {: name:str :}
   DIR DIR-U @ >str name FILE FS:JOIN-PATH {: fu:n :}
   FILE fu >str ARG+ ;


: SPAWN ( str -- n )
   OUT CAP >LEN  ERR CAP >LEN  PROCESS:WAIT-FOREVER
   RUN-ARGV-CAPTURE-OUTCOME PROCESS:ENDING>RC RC>N {: outu:len erru:len rc:n :}
   outu LEN>N OUT-U !  erru LEN>N ERR-U !
   rc ;


: SUCCEEDED ( n -- )
   {: rc:n :}
   rc 0<> if  ERR$ type cr  then
   rc 0 T= ;


: BUILD ( -- n )
   s" test/browser/app.f" str> MOD MOD-U @ WASM-HARNESS:BUILD ;


: HOST-OPEN ( -- )
   PROC-ARGV-RESET
   s" src/lib/browser/host-cli.mjs" ARG+  MOD MOD-U @ >str ARG+ ;


: HOST ( -- n )
   s" bun" EXE RESOLVE-EXECUTABLE {: u:n :}
   EXE u >str SPAWN ;


: TYPED+ ( str -- )  s" --typed" ARG+  ARG+ ;

: ANSWER-RPC+ ( str str -- )
   {: id:str name:str :}
   s" --answer-rpc" ARG+  id ARG+  name FILE-ARG+ ;


: FAIL-RPC+ ( str -- )  s" --fail-rpc" ARG+  ARG+ ;
: TICK+ ( -- )  s" --tick" ARG+ ;
: NOTIFY+ ( -- )  s" --notify" ARG+  METHOD$ ARG+  s" params.json" FILE-ARG+ ;
: FILE+ ( -- )  s" --file" ARG+  NAME$ ARG+  s" data.bin" FILE-ARG+ ;

: LINE ( str -- )
   SB-APPEND  10 SB-APPEND-C ;


: HELLO ( -- )
   SB-RESET  s" hello" LINE ;

: CALL-LINE ( str -- )
   {: id:str :}
   s" rpc tools/call " SB-APPEND  id SB-APPEND
   s\"  {\"name\":\"get_project\"}" LINE ;


: ANSWERED-LINE ( str str str -- )
   {: c:str id:str body:str :}
   s" text answered " SB-APPEND  c SB-APPEND  32 SB-APPEND-C
   id SB-APPEND  32 SB-APPEND-C  body LINE ;


: RESULT-BODY$ ( -- str )  s\" {\"content\":[]}" ;
: ERROR-BODY$ ( -- str )  s\" {\"code\":-32601,\"message\":\"Method not found\"}" ;

: SAME ( str str -- )
   {: got:str name:str :}
   WANT-DIR WANT-U @ >str name FILE FS:JOIN-PATH {: wu:n :}
   FILE wu >str SB$ FS:WRITE-ALL
   GOT-DIR GOT-U @ >str name FILE FS:JOIN-PATH {: fu:n :}
   FILE fu >str got FS:WRITE-ALL
   got SB$ T$= ;

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


\ Each answered, notified, file and chunk run frees its delivery.
: FREED-CASE ( -- )
   s" freed: four rounds of deliveries end with the memory one does"
   MOD MOD-U @ >str [: s" rpc" TYPED+  s" 1" s" result.json" ANSWER-RPC+
      NOTIFY+  FILE+  s" read start" TYPED+ ;]
   BROWSER-MEMORY-TEST:FREED ;


: MALFORMED ( str str -- )
   {: cmd:str what:str :}
   SB-RESET  s" refused " SB-APPEND  cmd SB-APPEND  s" : " SB-APPEND
   what SB-APPEND  SB$ T-LABEL ;


\ Typed text whose run leaves a malformed record, after the steps already
\ staged: the turn fails, and the host prints only the lines given and no rpc,
\ after or read.
: REFUSED ( str str -- )
   {: cmd:str before:str :}
   cmd TYPED+
   cmd s" the turn fails" MALFORMED
   HOST 1 T=
   cmd s" the record reaches no host" MALFORMED
   OUT$ before T$= ;


: REJECT ( str str -- )
   {: cmd:str why:str :}
   HOST-OPEN  cmd s\" hello\n" REFUSED
   cmd s" the failure names the reason" MALFORMED
   ERR$ why STR:CONTAINS? TTRUE ;


\ A read after the file event: its line is printed before the refusal.
: REJECT-READ ( str str -- )
   {: cmd:str why:str :}
   HELLO  FILE-LINE
   SB$ {: s:str :}
   s STR:DATA BEFORE s STR:LEN BYTE-COPY  s STR:LEN BEFORE-U !
   HOST-OPEN  FILE+  cmd BEFORE BEFORE-U @ >str REFUSED
   cmd s" the failure names the reason" MALFORMED
   ERR$ why STR:CONTAINS? TTRUE ;


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
   ERR$ s" Invalid byte sequence" STR:CONTAINS?
   ERR$ s" encoded data was not valid" STR:CONTAINS?  or TTRUE ;

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

: BYTES ( -- ^VEC:vector<u8> )  HEAP:ALLOCATOR 0 [type] u8 VEC:NEW ;

: PAGE! ( -- )
   MOD MOD-U @ >str FS:FILE-SIZE {: wu:n :}
   BYTES WASM !  WASM @ wu VEC:ENSURE
   WASM @ VEC:BYTES {: w:str :}
   MOD MOD-U @ >str w STR:DATA w STR:LEN FS:READ-ALL
   w STR:LEN <> if E-FS-IO throw then
   BYTES PAGE !
   TITLE$ PAGE @ MCP-APP:PAGE+
   PAGE @ VEC:BYTES s" page.html" FILE!
   PAGE @ VEC:DELETE
   BYTES CODE !
   w CODE @ MCP-APP:CODE+
   CODE @ VEC:BYTES s" code.json" FILE!
   CODE @ VEC:DELETE  WASM @ VEC:DELETE ;


: CHROME-CASE ( -- )
   CHROME-ASKED? 0= if
      s" browser app: Chrome cases run only with -- --chrome" type cr exit
   then
   FIND-CHROME CHROME-U @ 0= if
      s" --chrome: Chrome is installed" T-LABEL CHROME-U @ 0<> TTRUE exit
   then
   s" browser app: Chrome " type CHROME CHROME-U @ >str type cr
   PAGE!
   PROC-ARGV-RESET
   s" test/browser/app-peer.mjs" ARG+  s" page.html" FILE-ARG+
   DIR DIR-U @ >str ARG+
   TITLE$ ARG+  s" --chrome" ARG+  CHROME CHROME-U @ >str ARG+
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
