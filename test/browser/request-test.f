\ request-test.f - the browser request protocol under bun:
\ src/lib/browser/host-cli.mjs runs test/browser/request.f's module, built by
\ tools/wasm-build.f, through src/lib/browser/turn.js. bun and
\ wasm-tools must be on PATH, so this is no row of the ordinary gate:
\ test/wasm/device.f runs it, as does `bin/hb --load test/browser/request-test.f`
\ from the tree's root.

require src/lib/string.f
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

60000 constant DEADLINE-MS
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
: ID1$ ( -- ptr u8 n )  s" 9007199254740993" ;
: ID2$ ( -- ptr u8 n )  s" 9007199254740994" ;
: ID3$ ( -- ptr u8 n )  s" 9007199254740995" ;

: NONE$ ( -- ptr u8 n )  s" " ;
: ONE$ ( -- ptr u8 n )  s\" \x01" ;
: TWO$ ( -- ptr u8 n )  s\" \x02" ;
: THREE$ ( -- ptr u8 n )  s\" \x03" ;
: ZFF$ ( -- ptr u8 n )  s\" \x00\xFF" ;

: KIND99$ ( -- ptr u8 n )  s" a record of kind 99, which names no record" ;

: OUT$ ( -- ptr u8 n )  OUT OUT-U @ ;
: ERR$ ( -- ptr u8 n )  ERR ERR-U @ ;

: TRAP$ ( -- ptr u8 n )  TRAP TRAP-U @ ;

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
   s" browser-request" FS:MKTEMP-DIR {: a:ptr u:n :}
   a DIR u BYTE-COPY  u DIR-U !
   s" browser request: module, answers and transcripts in " type  DIR u type cr
   DIR u s" request.wasm" MOD FS:JOIN-PATH MOD-U !
   s" expected" WANT-DIR SUBDIR WANT-U !
   s" actual" GOT-DIR SUBDIR GOT-U !
   NONE$ s" empty.bin" FILE!
   ONE$ s" one.bin" FILE!
   TWO$ s" two.bin" FILE!
   THREE$ s" three.bin" FILE!
   ZFF$ s" zff.bin" FILE! ;

: ARG+ ( ptr u8 n -- )  >LEN PROC-ARGV+ ;

: N-ARG+ ( n -- )
   SB-RESET FMT:SB-INT SB$ ARG+ ;

: FILE-ARG+ ( ptr u8 n -- )
   {: name:ptr nu:n :}
   DIR DIR-U @ name nu FILE FS:JOIN-PATH {: fu:n :}
   FILE fu ARG+ ;

: SPAWN ( ptr u8 len -- n )
   OUT CAP >LEN  ERR CAP >LEN  DEADLINE-MS >MS
   RUN-ARGV-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N {: outu:len erru:len rc:n :}
   outu LEN>N OUT-U !  erru LEN>N ERR-U !
   rc ;

: SUCCEEDED ( n -- )
   {: rc:n :}
   rc 0<> if  ERR$ type cr  then
   rc 0 T= ;

: BUILD ( -- n )
   PROC-ARGV-RESET
   s" --load" ARG+  s" tools/wasm-build.f" ARG+  s" --" ARG+
   s" test/browser/request.f" ARG+  s" REQUEST-TURN:TURN" ARG+  MOD MOD-U @ ARG+
   s" bin/hb" >LEN SPAWN ;

: HOST-OPEN ( -- )
   PROC-ARGV-RESET
   s" src/lib/browser/host-cli.mjs" ARG+  MOD MOD-U @ ARG+ ;

: TYPED+ ( ptr u8 n -- )
   s" --typed" ARG+  ARG+ ;

\ Answers the oldest waiting request with the status and DIR's file of this
\ name. request.f's replies: 470 leaves a record of no kind, 471 traps, and 202
\ to a legacy post posts /followup.
: ANSWER+ ( n ptr u8 n -- )
   {: status:n name:ptr nu:n :}
   s" --answer" ARG+  status N-ARG+  name nu FILE-ARG+ ;

\ The waiting tagged request with this ID answered, ahead of older ones.
: ANSWER-ID+ ( ptr u8 n n ptr u8 n -- )
   {: id:ptr iu:n status:n name:ptr nu:n :}
   s" --answer-id" ARG+  id iu ARG+  status N-ARG+  name nu FILE-ARG+ ;

: RESIZE+ ( n n -- )
   {: w:n h:n :}
   s" --resize" ARG+  w N-ARG+  h N-ARG+ ;

\ The oldest waiting request failed, as a lost connection fails it.
: FAIL+ ( -- )
   s" --fail" ARG+ ;

: HOST ( -- n )
   s" bun" >LEN EXE RESOLVE-EXECUTABLE {: u:len :}
   EXE u SPAWN ;

\ ---- the lines the host prints, built in SB ---------------------------------
: LINE ( ptr u8 n -- )
   SB-APPEND  10 SB-APPEND-C ;

: HELLO ( -- )
   SB-RESET  s" hello" LINE ;

: DECIMALS ( ptr u8 n -- )
   {: a:ptr u:n :}
   u 0 ?do  a i + c@ FMT:SB-U  44 SB-APPEND-C  loop ;

\ A tagged request as the host makes it: method and path, the ID, then the
\ body's length and hex.
: REQUEST-LINE ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: what:ptr wu:n id:ptr iu:n body:ptr bu:n :}
   s" request " SB-APPEND  what wu SB-APPEND  32 SB-APPEND-C
   id iu SB-APPEND  32 SB-APPEND-C  body bu LINE ;

\ The fixture's requests: POST /state and PUT /view with the body 00 ff, and
\ GET /projects with none.
: STATE-LINE ( ptr u8 n -- )
   {: id:ptr iu:n :}
   s" POST /state" id iu s" 2 00ff" REQUEST-LINE ;

: PROJECTS-LINE ( -- )
   s" GET /projects" ID2$ s" 0 " REQUEST-LINE ;

: VIEW-LINE ( -- )
   s" PUT /view" ID3$ s" 2 00ff" REQUEST-LINE ;

\ The text a reply's run answers: the ID, the status and drawing size, then
\ the body's bytes.
: REPLY-LINE ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: id:ptr iu:n what:ptr wu:n body:ptr bu:n :}
   s" text " SB-APPEND  id iu SB-APPEND  32 SB-APPEND-C
   what wu SB-APPEND  32 SB-APPEND-C  body bu DECIMALS  10 SB-APPEND-C ;

: ERR-LINE$ ( -- ptr u8 n )
   ERR ERR-U @ 10 INDEX-OF MATCH option
      none OF ERR 0 ENDOF
      some OF IDX>N ERR swap ENDOF
   ;MATCH ;

: ENDED-LINE ( ptr u8 n -- )
   s" the module trapped and has ended: RuntimeError: " SB-APPEND  LINE ;

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
   ERR-LINE$ {: t:ptr u:n :}
   t TRAP u BYTE-COPY  u TRAP-U !
   \ The wording is the engine's: bun says Unreachable, V8 out of bounds.
   s" trapped: the first failure is the fixture's trap" T-LABEL
   TRAP$ s" Unreachable" CONTAINS?  TRAP$ s" memory access out of bounds" CONTAINS?  or TTRUE
   HELLO  ID1$ STATE-LINE
   s" trapped: after a trap the module prints nothing more" T-LABEL
   OUT$ s" trapped.out" SAME
   SB-RESET  TRAP$ LINE  TRAP$ ENDED-LINE
   s" trapped: the waiting request's reply is refused" T-LABEL
   ERR$ s" trapped.err" SAME ;

: EARLY-TRAP-CASE ( -- )
   HOST-OPEN  s" parallel" TYPED+
   ID2$ 471 s" empty.bin" ANSWER-ID+  ID1$ 200 s" one.bin" ANSWER-ID+
   s" early trap: the run fails" T-LABEL
   HOST 1 T=
   HELLO  ID1$ STATE-LINE  PROJECTS-LINE
   s" early trap: neither reply answers text" T-LABEL
   OUT$ s" early-trap.out" SAME
   SB-RESET  TRAP$ LINE
   s" RuntimeError: " SB-APPEND  TRAP$ SB-APPEND  s" ; Error: " SB-APPEND  TRAP$ ENDED-LINE
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
   s" Error: " SB-APPEND  KIND99$ SB-APPEND  s" ; RuntimeError: " SB-APPEND  TRAP$ LINE
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
   s" freed: four answered requests end with the memory one does"
   MOD MOD-U @ [: s" x" TYPED+  ID1$ 201 s" zff.bin" ANSWER-ID+ ;]
   BROWSER-MEMORY-TEST:FREED ;

: MALFORMED ( ptr u8 n ptr u8 n -- )
   {: cmd:ptr cu:n what:ptr wu:n :}
   SB-RESET  s" malformed " SB-APPEND  cmd cu SB-APPEND  s" : " SB-APPEND
   what wu SB-APPEND  SB$ T-LABEL ;

\ Typed text whose run leaves a malformed request record: the turn fails before
\ any request reaches the host.
: REFUSED ( ptr u8 n -- )
   {: cmd:ptr cu:n :}
   HOST-OPEN  cmd cu TYPED+
   cmd cu s" the turn fails" MALFORMED
   HOST 1 T=
   cmd cu s" no request reaches the host" MALFORMED
   OUT$ s\" hello\n" T$= ;

: REJECT ( ptr u8 n ptr u8 n -- )
   {: cmd:ptr cu:n why:ptr wu:n :}
   cmd cu REFUSED
   cmd cu s" the failure names the reason" MALFORMED
   ERR$ why wu CONTAINS? TTRUE ;

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
   ERR$ s" Invalid byte sequence" CONTAINS?
   ERR$ s" encoded data was not valid" CONTAINS?  or TTRUE ;

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
