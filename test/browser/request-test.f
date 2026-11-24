\ request-test.f - the browser request protocol under bun:
\ src/lib/browser/host-cli.mjs runs test/browser/request.f's module, built by
\ tools/hb-build.f, through src/lib/browser/turn.js. test/wasm/device.f runs
\ it, as does `bin/hb --load test/browser/request-test.f` from the tree's root.

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
require test/browser/memory.f

package BROWSER-REQUEST-TEST
private

4096 constant CAP
CAP BUFFER: OUT
CAP BUFFER: ERR
variable OUT-U
variable ERR-U
CAP BUFFER: TRAP
variable TRAP-U
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

\ The fixture's request IDs, 2^53 + 1 to 2^53 + 3: past JavaScript's safe
\ integers, so only an exact BigInt keeps them apart.
: ID1$ ( -- str )  s" 9007199254740993" ;
: ID2$ ( -- str )  s" 9007199254740994" ;
: ID3$ ( -- str )  s" 9007199254740995" ;

: NONE$ ( -- str )  s" " ;
: ONE$ ( -- str )  s\" \x01" ;
: TWO$ ( -- str )  s\" \x02" ;
: THREE$ ( -- str )  s\" \x03" ;
: ZFF$ ( -- str )  s\" \x00\xFF" ;

: KIND99$ ( -- str )  s" a record of kind 99, which names no record" ;

: OUT$ ( -- str )  OUT OUT-U @ >str ;
: ERR$ ( -- str )  ERR ERR-U @ >str ;

: TRAP$ ( -- str )  TRAP TRAP-U @ >str ;

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
   s" browser-request" FS:MKTEMP-DIR {: d:str :}
   d STR:DATA DIR d STR:LEN BYTE-COPY  d STR:LEN DIR-U !
   s" browser request: module, answers and transcripts in " type  d type cr
   d s" request.wasm" MOD FS:JOIN-PATH MOD-U !
   s" expected" WANT-DIR SUBDIR WANT-U !
   s" actual" GOT-DIR SUBDIR GOT-U !
   NONE$ s" empty.bin" FILE!
   ONE$ s" one.bin" FILE!
   TWO$ s" two.bin" FILE!
   THREE$ s" three.bin" FILE!
   ZFF$ s" zff.bin" FILE! ;

: ARG+ ( str -- )  PROC-ARGV+ ;

: N-ARG+ ( n -- )
   SB-RESET FMT:SB-INT SB$ ARG+ ;

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
   s" test/browser/request.f" str> MOD MOD-U @ WASM-HARNESS:BUILD ;

: HOST-OPEN ( -- )
   PROC-ARGV-RESET
   s" src/lib/browser/host-cli.mjs" ARG+  MOD MOD-U @ >str ARG+ ;

: TYPED+ ( str -- )
   s" --typed" ARG+  ARG+ ;

\ Answers the oldest waiting request with the status and DIR's file of this
\ name. request.f's replies: 470 leaves a record of no kind, 471 traps, and 202
\ to a legacy post posts /followup.
: ANSWER+ ( n str -- )
   {: status:n name:str :}
   s" --answer" ARG+  status N-ARG+  name FILE-ARG+ ;

\ The waiting tagged request with this ID answered, ahead of older ones.
: ANSWER-ID+ ( str n str -- )
   {: id:str status:n name:str :}
   s" --answer-id" ARG+  id ARG+  status N-ARG+  name FILE-ARG+ ;

: RESIZE+ ( n n -- )
   {: w:n h:n :}
   s" --resize" ARG+  w N-ARG+  h N-ARG+ ;

\ The oldest waiting request failed, as a lost connection fails it.
: FAIL+ ( -- )
   s" --fail" ARG+ ;

: HOST ( -- n )
   s" bun" EXE RESOLVE-EXECUTABLE {: u:n :}
   EXE u >str SPAWN ;

\ ---- the lines the host prints, built in SB ---------------------------------
: LINE ( str -- )
   SB-APPEND  10 SB-APPEND-C ;

: HELLO ( -- )
   SB-RESET  s" hello" LINE ;

: DECIMALS ( str -- )
   {: s:str :}
   s STR:LEN 0 ?do  s STR:DATA i + c@ FMT:SB-U  44 SB-APPEND-C  loop ;

\ A tagged request as the host makes it: method and path, the ID, then the
\ body's length and hex.
: REQUEST-LINE ( str str str -- )
   {: what:str id:str body:str :}
   s" request " SB-APPEND  what SB-APPEND  32 SB-APPEND-C
   id SB-APPEND  32 SB-APPEND-C  body LINE ;

\ The fixture's requests: POST /state and PUT /view with the body 00 ff, and
\ GET /projects with none.
: STATE-LINE ( str -- )
   {: id:str :}
   s" POST /state" id s" 2 00ff" REQUEST-LINE ;

: PROJECTS-LINE ( -- )
   s" GET /projects" ID2$ s" 0 " REQUEST-LINE ;

: VIEW-LINE ( -- )
   s" PUT /view" ID3$ s" 2 00ff" REQUEST-LINE ;

\ The text a reply's run answers: the ID, the status and drawing size, then
\ the body's bytes.
: REPLY-LINE ( str str str -- )
   {: id:str what:str body:str :}
   s" text " SB-APPEND  id SB-APPEND  32 SB-APPEND-C
   what SB-APPEND  32 SB-APPEND-C  body DECIMALS  10 SB-APPEND-C ;

: ERR-LINE$ ( -- str )
   ERR ERR-U @ >str 10 STR:INDEX-OF MATCH option
      none OF ERR 0 >str ENDOF
      some OF IDX>N ERR swap >str ENDOF
   ;MATCH ;

: ENDED-LINE ( str -- )
   s" the module trapped and has ended: Error: " SB-APPEND  LINE ;

\ Writes the expected text and the actual bytes as this name in DIR's expected
\ and actual directories, then compares them.
: SAME ( str str -- )
   {: got:str name:str :}
   WANT-DIR WANT-U @ >str name FILE FS:JOIN-PATH {: wu:n :}
   FILE wu >str SB$ FS:WRITE-ALL
   GOT-DIR GOT-U @ >str name FILE FS:JOIN-PATH {: fu:n :}
   FILE fu >str got FS:WRITE-ALL
   got SB$ T$= ;

\ ---- the cases --------------------------------------------------------------

: TAGGED-CASE ( -- )
   HOST-OPEN  s" request" TYPED+  s" get" TYPED+
   ID2$ 404 s" three.bin" ANSWER-ID+  100 80 RESIZE+  ID1$ 201 s" zff.bin" ANSWER-ID+
   s" put" TYPED+  204 s" empty.bin" ANSWER+
   s" bom path" TYPED+  200 s" empty.bin" ANSWER+
   s" tagged: every turn and reply runs" T-LABEL
   HOST SUCCEEDED
   HELLO  ID1$ STATE-LINE  PROJECTS-LINE
   ID2$ s" 404 640,480" THREE$ REPLY-LINE
   ID1$ s" 201 100,80" ZFF$ REPLY-LINE
   VIEW-LINE  ID3$ s" 204 100,80" NONE$ REPLY-LINE
   s\" POST \xEF\xBB\xBF/state" ID1$ s" 0 " REQUEST-LINE
   ID1$ s" 200 100,80" NONE$ REPLY-LINE
   s" tagged: replies to two turns in reverse order, exact IDs, bodies, sizes, a BOM path" T-LABEL
   OUT$ s" tagged.out" SAME ;

: PARALLEL-CASE ( -- )
   HOST-OPEN  s" parallel" TYPED+
   ID2$ 404 s" three.bin" ANSWER-ID+  100 80 RESIZE+  FAIL+
   s" put" TYPED+  204 s" empty.bin" ANSWER+
   s" parallel: every turn and reply runs" T-LABEL
   HOST SUCCEEDED
   HELLO  ID1$ STATE-LINE  PROJECTS-LINE
   ID2$ s" 404 640,480" THREE$ REPLY-LINE
   ID1$ s" 0 100,80" s" Error: /state: failed" REPLY-LINE
   VIEW-LINE  ID3$ s" 204 100,80" NONE$ REPLY-LINE
   s" parallel: one run's replies in reverse order, a lost connection answers status 0" T-LABEL
   OUT$ s" parallel.out" SAME ;

\ A trap ends the module. Records the trap's message for the cases after it.
: TRAPPED-CASE ( -- )
   HOST-OPEN  s" request" TYPED+  s" trap" TYPED+  200 s" one.bin" ANSWER+
   s" trapped: the run fails" T-LABEL
   HOST 1 T=
   ERR-LINE$ {: t:str :}
   t STR:DATA TRAP t STR:LEN BYTE-COPY  t STR:LEN TRAP-U !
   s" trapped: the host names the fault the fixture's trap recorded" T-LABEL
   TRAP$ s" fault kind 102 at " STR:STARTS-WITH? TTRUE
   HELLO  ID1$ STATE-LINE
   s" trapped: after a trap the module prints nothing more" T-LABEL
   OUT$ s" trapped.out" SAME
   SB-RESET  TRAP$ LINE  TRAP$ ENDED-LINE
   s" trapped: the waiting request's reply is refused" T-LABEL
   ERR$ s" trapped.err" SAME ;

\ A turn that throws after its request still has the request made and answered:
\ the module stays live, and the throw is the run's only failure.
: THROWN-CASE ( -- )
   HOST-OPEN  s" throw" TYPED+  200 s" one.bin" ANSWER+
   s" thrown: the run fails" T-LABEL
   HOST 1 T=
   HELLO  ID1$ STATE-LINE  ID1$ s" 200 640,480" ONE$ REPLY-LINE
   s" thrown: the throwing turn's request is made and its reply runs" T-LABEL
   OUT$ s" thrown.out" SAME
   SB-RESET  s" throw 94" LINE
   s" thrown: the throw is the only failure" T-LABEL
   ERR$ s" thrown.err" SAME ;

\ A turn that throws after a fetch still makes the fetch, and its failed reply
\ reaches host.error once, after the throw.
: THROWN-FETCH-CASE ( -- )
   HOST-OPEN  s" fetch throw" TYPED+  FAIL+
   s" thrown fetch: the run fails" T-LABEL
   HOST 1 T=
   HELLO  s" fetch /state" LINE
   s" thrown fetch: the throwing turn's fetch is made" T-LABEL
   OUT$ s" thrown-fetch.out" SAME
   SB-RESET  s" throw 94" LINE  s" /state: failed" LINE
   s" thrown fetch: the throw, then the failed reply once" T-LABEL
   ERR$ s" thrown-fetch.err" SAME ;

: EARLY-TRAP-CASE ( -- )
   HOST-OPEN  s" parallel" TYPED+
   ID2$ 471 s" empty.bin" ANSWER-ID+  ID1$ 200 s" one.bin" ANSWER-ID+
   s" early trap: the run fails" T-LABEL
   HOST 1 T=
   HELLO  ID1$ STATE-LINE  PROJECTS-LINE
   s" early trap: neither reply answers text" T-LABEL
   OUT$ s" early-trap.out" SAME
   SB-RESET  TRAP$ LINE
   s" Error: " SB-APPEND  TRAP$ SB-APPEND  s" ; Error: " SB-APPEND  TRAP$ ENDED-LINE
   s" early trap: reported at once, then with the refusal of its sibling" T-LABEL
   ERR$ s" early-trap.err" SAME ;

: REPLY-FAILURE-CASE ( -- )
   HOST-OPEN  s" two" TYPED+  470 s" empty.bin" ANSWER+  471 s" empty.bin" ANSWER+
   s" put" TYPED+
   s" reply failure: the run fails" T-LABEL
   HOST 1 T=
   HELLO  ID1$ STATE-LINE  ID2$ STATE-LINE
   s" reply failure: no reply answers text and no later request is made" T-LABEL
   OUT$ s" reply-failure.out" SAME
   SB-RESET  KIND99$ LINE
   s" Error: " SB-APPEND  KIND99$ SB-APPEND  s" ; Error: " SB-APPEND  TRAP$ LINE
   TRAP$ ENDED-LINE
   s" reply failure: an invalid record, both reasons, then the later turn refused" T-LABEL
   ERR$ s" reply-failure.err" SAME ;

: INTERLEAVED-CASE ( -- )
   HOST-OPEN  s" interleaved" TYPED+
   ID2$ 200 s" one.bin" ANSWER-ID+  ID1$ 200 s" two.bin" ANSWER-ID+
   200 s" three.bin" ANSWER+
   s" interleaved: every reply runs" T-LABEL
   HOST SUCCEEDED
   HELLO  ID1$ STATE-LINE  PROJECTS-LINE  s" post /legacy 1 2a" LINE
   ID2$ s" 200 640,480" ONE$ REPLY-LINE
   ID1$ s" 200 640,480" TWO$ REPLY-LINE
   s" 0" s" 200 0,0" THREE$ REPLY-LINE
   s" interleaved: a legacy post does not hold back the tagged requests of its run" T-LABEL
   OUT$ s" interleaved.out" SAME ;

: MIXED-FAILURE-CASE ( -- )
   HOST-OPEN  s" interleaved" TYPED+
   470 s" empty.bin" ANSWER+  200 s" empty.bin" ANSWER+
   202 s" three.bin" ANSWER+  200 s" two.bin" ANSWER+
   s" put" TYPED+  204 s" empty.bin" ANSWER+
   s" mixed failure: the run fails" T-LABEL
   HOST 1 T=
   HELLO  ID1$ STATE-LINE  PROJECTS-LINE  s" post /legacy 1 2a" LINE
   ID2$ s" 200 640,480" NONE$ REPLY-LINE
   s" 0" s" 202 0,0" THREE$ REPLY-LINE
   s" post /followup 1 2a" LINE
   s" 0" s" 200 0,0" TWO$ REPLY-LINE
   VIEW-LINE  ID3$ s" 204 640,480" NONE$ REPLY-LINE
   s" mixed failure: the sibling, the legacy followup and a later turn still run" T-LABEL
   OUT$ s" mixed-failure.out" SAME
   SB-RESET  KIND99$ LINE
   s" mixed failure: only the invalid record is reported" T-LABEL
   ERR$ s" mixed-failure.err" SAME ;

\ The fetch waits for the legacy post's reply, whose 202 posts again first.
: LEGACY-CASE ( -- )
   HOST-OPEN  s" legacy two" TYPED+
   202 s" zff.bin" ANSWER+  203 s" zff.bin" ANSWER+  200 s" zff.bin" ANSWER+
   s" legacy: every reply runs" T-LABEL
   HOST SUCCEEDED
   HELLO  s" post /state 2 00ff" LINE
   s" 0" s" 202 0,0" ZFF$ REPLY-LINE
   s" post /followup 1 2a" LINE
   s" 0" s" 203 0,0" ZFF$ REPLY-LINE
   s" fetch /state" LINE  s" text 0,0" LINE
   s" legacy: records keep their order and their replies' zero fields" T-LABEL
   OUT$ s" legacy.out" SAME ;

\ Four rounds of a tagged request and its answer end with the memory one round
\ does: each typed and each answered run frees its delivery.
: FREED-CASE ( -- )
   s" freed: four answered requests end with the memory one does" str>
   MOD MOD-U @ [: s" x" TYPED+  ID1$ 201 s" zff.bin" ANSWER-ID+ ;]
   BROWSER-MEMORY-TEST:FREED ;

: MALFORMED ( str str -- )
   {: cmd:str what:str :}
   SB-RESET  s" malformed " SB-APPEND  cmd SB-APPEND  s" : " SB-APPEND
   what SB-APPEND  SB$ T-LABEL ;

\ Typed text whose run leaves a malformed request record: the turn fails before
\ any request reaches the host.
: REFUSED ( str -- )
   {: cmd:str :}
   HOST-OPEN  cmd TYPED+
   cmd s" the turn fails" MALFORMED
   HOST 1 T=
   cmd s" no request reaches the host" MALFORMED
   OUT$ s\" hello\n" T$= ;

: REJECT ( str str -- )
   {: cmd:str why:str :}
   cmd REFUSED
   cmd s" the failure names the reason" MALFORMED
   ERR$ why STR:CONTAINS? TTRUE ;

: MALFORMED-CASES ( -- )
   s" short header" s" request header" REJECT
   s" outside" s" outside the module" REJECT
   s" huge length" s" request lengths" REJECT
   s" bad length" s" request lengths" REJECT
   s" bad id" s" positive request ID" REJECT
   s" negative id" s" positive request ID" REJECT
   s" empty method" s" request lengths" REJECT
   s" bad token" s" HTTP method token" REJECT
   s" bad newline" s" HTTP method token" REJECT
   s" empty path" s" nonempty path" REJECT
   s" get body" s" request body is not allowed" REJECT
   s" bom method" s" HTTP method token" REJECT
   s" bom only method" s" HTTP method token" REJECT
   \ TextDecoder's wording is the engine's.
   s" bad utf8" REFUSED
   s" bad utf8" s" the failure names the reason" MALFORMED
   ERR$ s" Invalid byte sequence" STR:CONTAINS?
   ERR$ s" encoded data was not valid" STR:CONTAINS?  or TTRUE ;

public

: RUN ( -- )
   T-RESET
   SETUP
   s" browser request fixture builds" T-LABEL
   BUILD {: rc:n :}
   rc 0 T=
   rc 0<> if  ERR$ type cr  T-REPORT exit  then
   s" browser request fixture validates" T-LABEL
   MOD MOD-U @ WASM-HARNESS:VALID? TTRUE
   TAGGED-CASE
   PARALLEL-CASE
   TRAPPED-CASE
   THROWN-CASE
   THROWN-FETCH-CASE
   EARLY-TRAP-CASE
   REPLY-FAILURE-CASE
   INTERLEAVED-CASE
   MIXED-FAILURE-CASE
   LEGACY-CASE
   FREED-CASE
   MALFORMED-CASES
   T-REPORT ;

;package

BROWSER-REQUEST-TEST:RUN
