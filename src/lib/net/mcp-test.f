split-str
\ mcp-test.f - MCP:HANDLE answered over a real loopback port: both eras' handshakes and
\ notifications, the protocol-version, _meta, Mcp-Method and Mcp-Name checks, the
\ transport check, unknown methods and tools, an unknown tool's name cut at 128 bytes, a
\ body's result and tool error, an object schema's result as structuredContent and a
\ non-object result under it, a result's _meta from RESULT-META in both eras and its
\ refusals, an image and an embedded resource from RESULT-CONTENT in both eras and a text
\ of the tool's own, an empty array of them and the refusals, bodies that give a bad
\ result or none, malformed envelopes, params, arguments and JSON, ping, tools/list showing each
\ descriptor as registered (annotations and _meta raw) with a non-object output schema's
\ wrapper in its place, and ADD-TOOL's rejections of a bad descriptor or inputSchema. A
\ tools/call whose arguments, given or absent, break the inputSchema is a tool error that
\ runs no body, from HANDLE and from ANSWER, and params that name a member twice are
\ 400 -32600 with no id. ui:// resources: the resources capability, resources/list and
\ resources/read in both eras, a read's Mcp-Name, a read without _meta, an unknown uri
\ and one that is no string, an empty resources/templates/list, and ADD-RESOURCE's
\ refusals. The info line a tools/call and a resources/read log, without arguments, and,
\ after MCP:LOG, each message's line after its answer with arguments and outcome.
\ MCP:ANSWER on a request worker into a vector holding old text: a tools/call written as
\ HANDLE sends it, a notification leaving it empty, a message that is not JSON-RPC or not
\ JSON, an unknown tool, and ANSWER off a worker.
\ A call its body HOLDs, FINISHed by a call SUBMIT queues for the sole worker the body
\ gave back, answers the client in each era as the same tool answering at once; HOLD
\ answers false under ANSWER; a body that throws after HOLD leaves the client a closed
\ connection and no answer.
\ MCP's client against the same server and plain routes: TOOLS across pages, CALL's
\ result, one holding content blocks, isError, refusals, a reply past the first buffer,
\ timeouts of CALL and TOOLS, TOOLS's over all its pages, the Mcp-Name encoding,
\ event-stream replies and one that is not UTF-8, a result naming isError twice, TOOLS's
\ limit shared by its pages, CLOSE after a halt mid-receive, a closed client refused once
\ its row is reused, REQUEST-STOP of TOOLS's second page, of an idle client and of a
\ CALL, after which CALL and TOOLS throw sending nothing, a client on the row a stopped
\ one left calling normally, and REQUEST-STOP of a closed one. Then /mcp guarded by
\ BEARER:ADMIT?: VISIBLE! hiding a tool by the caller's claim from tools/list and
\ tools/call, and refused while HTTP serves; a client's TOKEN!, E-DENIED for no token, an
\ empty one and a 403, E-UNAVAILABLE for a 503 and a 429, E-RESPONSE for a 418, and
\ TOKEN! of a closed client.
\ Run: bin/hb --load src/lib/net/mcp-test.f

require src/lib/test.f
require src/lib/test/wait.f
require src/lib/byte-buffer.f
require src/lib/heap.f
require src/lib/vector.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/test/stderr.f
require src/lib/memory.f
require src/lib/arena.f
require src/lib/span.f
require src/lib/string.f
require src/lib/string-option.f
require src/lib/fmt.f
require src/lib/aio.f
require src/lib/task.f
require src/lib/heap.f
require src/lib/vector.f
require src/lib/adt/result.f
require src/lib/reason.f
require src/lib/base64.f
require src/lib/crypto/evp.f
require src/lib/crypto/jws-fixture.f
require src/lib/net/curl.f
require src/lib/net/http.f
require src/lib/net/mcp.f
require src/lib/net/bearer.f

package MCP-TEST
private

$7F000001 constant LOOPBACK
1 constant ONE-WORKER
500 constant IDLE-MS
$4000 constant CAP
TYPED-VARIABLE BODY ^VEC:vector<u8>
1000000 constant NS-PER-MS
2000 constant WAIT-MS               \ how long /endless pages before its last
50 constant PAGE-MS                 \ /endless's delay before each page
300 constant LIST-MS                \ the TOOLS limit /endless runs past
1000 constant LIST-BOUND-MS         \ when that TOOLS must have thrown by
300 constant FIRST-PAGE-MS          \ /late's delay before its first page
500 constant LATE-MS                \ the TOOLS limit /late runs past
790 constant LATE-BOUND-MS          \ when that TOOLS must have thrown by

TYPED-VARIABLE CFG HTTP:config

: SERVE-ON ( n n -- )
   {: port:n workers:n :}
   LOOPBACK CFG HTTP-CONFIG:ADDRESS !
   port CFG HTTP-CONFIG:PORT !
   workers CFG HTTP-CONFIG:WORKERS !
   IDLE-MS >MS CFG HTTP-CONFIG:IDLE !
   CFG HTTP:START ;

\ Expected texts too long for SB.
CAP BUFFER: WANT
variable WANT-U
: W-RESET ( -- ) 0 WANT-U ! ;
: W+ ( ^u8 n -- )
   {: a:^ u:n :}
   WANT-U @ u + CAP > if s" W+ past WANT" T-FAIL-AS exit then
   a WANT WANT-U @ + u BYTE-COPY
   WANT-U @ u + WANT-U ! ;
: W$ ( -- str ) WANT WANT-U @ ;

\ A route's body, built in the HTTP worker.
$100 constant NB-CAP
NB-CAP BUFFER: NB
variable NB-U
: NB+ ( ^u8 n -- )
   {: a:^ u:n :}
   NB-U @ u + NB-CAP > if s" NB+ past NB" T-FAIL-AS exit then
   a NB NB-U @ + u BYTE-COPY
   NB-U @ u + NB-U ! ;

\ A JSON string past the client's first reply buffer ($1000 bytes).
$1400 constant BIG-U
BIG-U BUFFER: BIG
: BIG-FILL ( -- )
   BIG-U 1- 1 ?do 120 BIG i + c! loop  34 BIG c!  34 BIG BIG-U + 1- c! ;

TYPED-VARIABLE CLIENT MCP:client
TYPED-VARIABLE STALL-CLIENT MCP:client
TYPED-VARIABLE ROUTE-CLIENT MCP:client
\ A server waiter sets ENTERED as it starts waiting for RELEASED and LEFT once
\ it stops; a case that releases one SETTLEs, so the next case's ARM finds none.
variable ENTERED
variable SHAPE-CODE                 \ what shape's RESULT threw, 0 for nothing
variable META-CODE                  \ what framed's RESULT-META threw, 0 for nothing
variable BLOCKS-CODE                \ what pictured's RESULT-CONTENT threw, 0 for nothing
variable STRICT-RAN                 \ set once strict's body runs
\ Each code's text, kept from the body's task beside the code.
$100 constant KEPT-CAP
KEPT-CAP BUFFER: SHAPE-TEXT
variable SHAPE-LEN
KEPT-CAP BUFFER: META-TEXT
variable META-LEN
KEPT-CAP BUFFER: BLOCKS-TEXT
variable BLOCKS-LEN

: KEPT= ( ^u8 ^n ^u8 n -- ) {: buf:^ len:^ want:^ wu:n :}
   buf len atomic@ want wu T$= ;
variable RELEASED
variable LEFT
variable CLOSED
variable PAGES-END                  \ when /endless answers its last page, a mono-ns
variable CANCEL-CODE                \ what the cancelled TOOLS or CALL threw, 0 for none
1 constant RUNNING                  \ CANCEL-CODE until that call ends: no throw code
variable HITS                       \ the requests /counted has had
TASK:MIN-STACK TASK:TASK STALLER

\ WAIT-MS from now, an absolute mono-ns.
: DEADLINE ( -- n ) mono-ns WAIT-MS NS-PER-MS * + ;

: ARM ( -- ) 0 ENTERED atomic! 0 RELEASED atomic! 0 LEFT atomic! ;
: SETTLE ( -- ) [: LEFT atomic@ 0<> ;] TEST-WAIT:POLL ;

\ A setter's refusal throws: the case cannot make its request on that handle.
: OK ( CURL:status -- )
   MATCH CURL:status
      ok OF ENDOF
      failed OF drop CURL:E-STATE throw ENDOF
   ;MATCH ;

: BODY$ ( -- str ) BODY @ VEC:SPAN SPAN:$ ;

\ The response's status, its body in BODY; -1 for a body past CAP, -2 for a
\ failed transfer.
: PERFORM ( CURL:handle -- n )
   BODY @ 0 VEC:RESIZE
   CAP >LEN BODY @ CURL:PERFORM
   MATCH CURL:fetch-result
      response OF drop CURL:HTTP-STATUS>N ENDOF
      truncated OF drop drop -1 ENDOF
      failed OF drop -2 ENDOF
   ;MATCH ;


: ADD ( CURL:handle ^u8 n -- CURL:handle )
   {: h:CURL:handle a:^ u:n :}
   h a u CURL:HEADER+ OK h ;


: OPEN-PATH ( str str str -- CURL:handle )
   {: path:^ pu:n kind:^ ku:n accept:^ au:n :}
   CURL:INIT
   MATCH CURL:init-result
      ready OF ENDOF
      failed OF drop CURL:EMPTY-HANDLE ENDOF
   ;MATCH {: h:CURL:handle :}
   SB-RESET  s" http://127.0.0.1:" SB-APPEND  HTTP:PORT FMT:SB-U  path pu SB-APPEND
   h SB$ CURL:URL! OK
   h kind ku ADD accept au ADD ;

: OPEN-WITH ( str str -- CURL:handle )
   {: kind:^ ku:n accept:^ au:n :}
   s" /mcp" kind ku accept au OPEN-PATH ;

: OPEN ( -- CURL:handle )
   s" Content-Type: application/json"
   s" Accept: application/json, text/event-stream" OPEN-WITH ;

: LEGACY ( -- CURL:handle ) OPEN s" MCP-Protocol-Version: 2025-11-25" ADD ;
: MODERN ( -- CURL:handle ) OPEN s" MCP-Protocol-Version: 2026-07-28" ADD ;


\ Posts the body: answers the status, the response's body in BODY.
: POST ( CURL:handle ^u8 n -- n )
   {: h:CURL:handle a:^ u:n :}
   h a u CURL:BODY! OK
   h PERFORM {: status:n :}
   h CURL:CLEANUP
   status ;


: ANSWERED ( n n ^u8 n -- )
   {: status:n wanted:n want:^ wu:n :}
   BODY$ want wu T$=
   status wanted T= ;

: META ( -- )
   S\" \"_meta\":{\"io.modelcontextprotocol/protocolVersion\":\"2026-07-28\","
   SB-APPEND S\" \"io.modelcontextprotocol/clientCapabilities\":{}}" SB-APPEND ;


\ A tool body's fault reaches the host, which answers 500.
: FAULTED ( n -- )
   {: status:n :}
   BODY$ s" internal: the handler did not finish this request" STR:STARTS-WITH? TTRUE
   status 500 T= ;

: SERVER ( -- )
   S\" \"_meta\":{\"io.modelcontextprotocol/serverInfo\":" SB-APPEND
   S\" {\"name\":\"test-server\",\"version\":\"1.0\"}}" SB-APPEND ;

: GUIDE$ ( -- str ) S\" Echo text back.\n\"raw\" answers as given." ;

: GUIDE ( -- )
   S\" ,\"instructions\":\"Echo text back.\\n\\\"raw\\\" answers as given.\"" SB-APPEND ;

: W-SERVER ( -- )
   S\" \"_meta\":{\"io.modelcontextprotocol/serverInfo\":" W+
   S\" {\"name\":\"test-server\",\"version\":\"1.0\"}}" W+ ;


: LEGACY-INITIALIZE ( -- )
   s" legacy initialize answers the 2025-11-25 handshake with instructions" T-LABEL
   LEGACY S\" {\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{}}" POST
   SB-RESET S\" {\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"protocolVersion\":\"2025-11-25\"," SB-APPEND
   S\" \"capabilities\":{\"tools\":{},\"resources\":{}}," SB-APPEND
   S\" \"serverInfo\":{\"name\":\"test-server\",\"version\":\"1.0\"}"
   SB-APPEND GUIDE S\" }}" SB-APPEND
   200 SB$ ANSWERED ;


: LEGACY-INITIALIZED ( -- )
   s" notifications/initialized is accepted with no body" T-LABEL
   LEGACY S\" {\"jsonrpc\":\"2.0\",\"method\":\"notifications/initialized\"}" POST
   202 s" " ANSWERED ;


: MODERN-DISCOVER ( -- )
   s" modern server/discover answers versions, serverInfo, instructions and cache scope" T-LABEL
   MODERN s" Mcp-Method: server/discover" ADD
   SB-RESET S\" {\"jsonrpc\":\"2.0\",\"id\":\"d\",\"method\":\"server/discover\",\"params\":{" SB-APPEND
   META S\" }}" SB-APPEND SB$ POST
   SB-RESET S\" {\"jsonrpc\":\"2.0\",\"id\":\"d\",\"result\":{\"resultType\":\"complete\"," SB-APPEND
   S\" \"supportedVersions\":[\"2026-07-28\",\"2025-11-25\"]," SB-APPEND
   S\" \"capabilities\":{\"tools\":{},\"resources\":{}}," SB-APPEND
   SERVER GUIDE S\" ,\"ttlMs\":0,\"cacheScope\":\"private\"}}" SB-APPEND
   200 SB$ ANSWERED ;


: UNSUPPORTED ( -- )
   s" an unknown MCP-Protocol-Version is -32022 with the supported versions" T-LABEL
   OPEN s" MCP-Protocol-Version: 2099-01-01" ADD
   S\" {\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"tools/list\"}" POST
   SB-RESET S\" {\"jsonrpc\":\"2.0\",\"id\":2,\"error\":{\"code\":-32022," SB-APPEND
   S\" \"message\":\"Unsupported protocol version\",\"data\":{\"supported\":" SB-APPEND
   S\" [\"2026-07-28\",\"2025-11-25\"],\"requested\":\"2099-01-01\"}}}" SB-APPEND
   400 SB$ ANSWERED ;


: METHOD-MISMATCH ( -- )
   s" a modern Mcp-Method naming another method is -32020" T-LABEL
   MODERN s" Mcp-Method: tools/list" ADD
   SB-RESET S\" {\"jsonrpc\":\"2.0\",\"id\":3,\"method\":\"server/discover\",\"params\":{" SB-APPEND
   META S\" }}" SB-APPEND SB$ POST
   400 S\" {\"jsonrpc\":\"2.0\",\"id\":3,\"error\":{\"code\":-32020,\"message\":\"Header mismatch\"}}"
   ANSWERED ;


: UNACCEPTED ( -- )
   s" an Accept without text/event-stream is -32600" T-LABEL
   s" Content-Type: application/json" s" Accept: application/json" OPEN-WITH
   s" MCP-Protocol-Version: 2025-11-25" ADD
   S\" {\"jsonrpc\":\"2.0\",\"id\":4,\"method\":\"ping\"}" POST
   SB-RESET S\" {\"jsonrpc\":\"2.0\",\"error\":{\"code\":-32600," SB-APPEND
   S\" \"message\":\"MCP requires JSON and both Accept types\"}}" SB-APPEND
   400 SB$ ANSWERED ;


: UNKNOWN-METHOD ( -- )
   s" an unknown method is 404 -32601" T-LABEL
   LEGACY S\" {\"jsonrpc\":\"2.0\",\"id\":5,\"method\":\"prompts/list\"}" POST
   404 S\" {\"jsonrpc\":\"2.0\",\"id\":5,\"error\":{\"code\":-32601,\"message\":\"Method not found\"}}"
   ANSWERED ;


\ Reply 6 to a call of an unknown tool, for the name it names, in WANT.
: UNKNOWN-REPLY ( str -- str )
   {: name:^ nu:n :}
   W-RESET S\" {\"jsonrpc\":\"2.0\",\"id\":6,\"error\":{\"code\":-32602," W+
   S\" \"message\":\"Unknown tool: " W+ name nu W+
   S\" ; if the client's list of test-server's tools names it," W+
   S\"  that list is out of date\"}}" W+ W$ ;

: UNKNOWN-TOOL ( -- )
   s" an unregistered tool is 400 -32602 naming it and the server" T-LABEL
   LEGACY
   S\" {\"jsonrpc\":\"2.0\",\"id\":6,\"method\":\"tools/call\",\"params\":{\"name\":\"none\"}}" POST
   400 s" none" UNKNOWN-REPLY ANSWERED ;

: A-RUN ( -- ) 127 0 ?do s" a" SB-APPEND loop ;

\ 127 bytes, then a two-byte character over MCP's 128-byte cap.
: LONG-UNKNOWN ( -- )
   s" an unknown tool's name past 128 bytes is cut before the character crossing it" T-LABEL
   LEGACY
   SB-RESET S\" {\"jsonrpc\":\"2.0\",\"id\":6,\"method\":\"tools/call\",\"params\":{\"name\":\"" SB-APPEND
   A-RUN S\" é\"}}" SB-APPEND SB$ POST
   SB-RESET A-RUN
   400 SB$ UNKNOWN-REPLY ANSWERED ;


: TOOL-ERROR ( -- )
   s" a body's RESULT-ERROR is an isError result" T-LABEL
   LEGACY
   S\" {\"jsonrpc\":\"2.0\",\"id\":7,\"method\":\"tools/call\",\"params\":{\"name\":\"refuse\"}}" POST
   SB-RESET S\" {\"jsonrpc\":\"2.0\",\"id\":7,\"result\":{\"content\":" SB-APPEND
   S\" [{\"type\":\"text\",\"text\":\"nope\"}],\"isError\":true}}" SB-APPEND
   200 SB$ ANSWERED ;


\ A tools/call of strict with id n and the params after its name, in SB.
: STRICT-CALL ( n ^u8 n -- ^u8 n )
   {: id:n rest:^ ru:n :}
   SB-RESET S\" {\"jsonrpc\":\"2.0\",\"id\":" SB-APPEND id FMT:SB-U
   S\" ,\"method\":\"tools/call\",\"params\":{\"name\":\"strict\"" SB-APPEND
   rest ru SB-APPEND S\" }}" SB-APPEND SB$ ;

: FILTERS ( -- ^u8 n ) S\" ,\"arguments\":{\"filters\":\"x\"}" ;

\ strict's tool error for a filters member.
: FILTERS-TEXT ( -- ^u8 n )
   s" arguments: expected only the member `filter`, found `filters`; rename it to `filter` or remove it" ;

\ A legacy tool error with id n and the text.
: ERROR-REPLY ( n ^u8 n -- ^u8 n )
   {: id:n text:^ tu:n :}
   SB-RESET S\" {\"jsonrpc\":\"2.0\",\"id\":" SB-APPEND id FMT:SB-U
   S\" ,\"result\":{\"content\":[{\"type\":\"text\",\"text\":\"" SB-APPEND
   text tu SB-APPEND S\" \"}],\"isError\":true}}" SB-APPEND SB$ ;

: STRICT-REFUSED ( -- )
   s" arguments outside the inputSchema are a tool error and the body does not run" T-LABEL
   LEGACY 70 FILTERS STRICT-CALL POST
   200 70 FILTERS-TEXT ERROR-REPLY ANSWERED
   STRICT-RAN atomic@ 0 T=
   s" absent arguments are checked as an empty object" T-LABEL
   LEGACY 71 s" " STRICT-CALL POST
   200 71 s" arguments: expected a member `filter`, found none; add `filter`" ERROR-REPLY
   ANSWERED
   STRICT-RAN atomic@ 0 T=
   s" arguments the inputSchema admits run the body" T-LABEL
   LEGACY 72 S\" ,\"arguments\":{\"filter\":\"x\"}" STRICT-CALL POST
   SB-RESET S\" {\"jsonrpc\":\"2.0\",\"id\":72,\"result\":{\"content\":[]," SB-APPEND
   S\" \"structuredContent\":{\"data\":{\"ran\":true}},\"isError\":false}}" SB-APPEND
   200 SB$ ANSWERED
   STRICT-RAN atomic@ 1 T= ;


\ A modern call of echo with {"a":1}, in SB.
: ECHO-A1 ( -- str )
   SB-RESET S\" {\"jsonrpc\":\"2.0\",\"id\":8,\"method\":\"tools/call\",\"params\":" SB-APPEND
   S\" {\"name\":\"echo\",\"arguments\":{\"a\":1}," SB-APPEND
   META S\" }}" SB-APPEND SB$ ;

: TOOL-RESULT ( -- )
   s" a modern call with a base64 Mcp-Name answers structuredContent.data" T-LABEL
   MODERN s" Mcp-Method: tools/call" ADD s" Mcp-Name: =?base64?ZWNobw==?=" ADD
   ECHO-A1 POST
   SB-RESET S\" {\"jsonrpc\":\"2.0\",\"id\":8,\"result\":{\"resultType\":\"complete\"," SB-APPEND
   S\" \"content\":[]," SB-APPEND
   S\" \"structuredContent\":{\"data\":{\"a\":1}},\"isError\":false," SB-APPEND
   SERVER S\" }}" SB-APPEND
   200 SB$ ANSWERED ;


\ The -32600 reply to request 9 with jsonrpc "1.0".
: ENVELOPE-REPLY ( -- str )
   SB-RESET S\" {\"jsonrpc\":\"2.0\",\"id\":9,\"error\":{\"code\":-32600," SB-APPEND
   S\" \"message\":\"a jsonrpc member that is not \\\"2.0\\\"\"}}" SB-APPEND SB$ ;

: BAD-ENVELOPE ( -- )
   s" a jsonrpc other than 2.0 is -32600 with the request's id and JSON-RPC's why" T-LABEL
   LEGACY S\" {\"jsonrpc\":\"1.0\",\"id\":9,\"method\":\"ping\"}" POST
   400 ENVELOPE-REPLY ANSWERED ;

: NULL-ID ( -- )
   s" a request whose id is null is 400 -32600 with no id" T-LABEL
   LEGACY S\" {\"jsonrpc\":\"2.0\",\"id\":null,\"method\":\"ping\"}" POST
   SB-RESET S\" {\"jsonrpc\":\"2.0\",\"error\":{\"code\":-32600," SB-APPEND
   S\" \"message\":\"an id that is not a string or an integer\"}}" SB-APPEND
   400 SB$ ANSWERED ;

: RESPONSE-SENT ( -- )
   s" a response sent to the server is 400 -32600 with no id" T-LABEL
   LEGACY S\" {\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{}}" POST
   SB-RESET S\" {\"jsonrpc\":\"2.0\",\"error\":{\"code\":-32600,\"message\":" SB-APPEND
   S\" \"a response, where MCP takes a request or a notification\"}}" SB-APPEND
   400 SB$ ANSWERED ;


: SHAPE-RESULT ( -- )
   s" an object schema's result is the structuredContent itself" T-LABEL
   LEGACY
   S\" {\"jsonrpc\":\"2.0\",\"id\":21,\"method\":\"tools/call\",\"params\":{\"name\":\"shape\"}}" POST
   SB-RESET S\" {\"jsonrpc\":\"2.0\",\"id\":21,\"result\":{\"content\":[]," SB-APPEND
   S\" \"structuredContent\":{\"sources\":[\"a\"]}," SB-APPEND
   S\" \"isError\":false}}" SB-APPEND
   200 SB$ ANSWERED ;


: SHAPE-NOT-OBJECT ( -- )
   s" a body under an object schema giving a non-object result faults to the host" T-LABEL
   LEGACY
   SB-RESET S\" {\"jsonrpc\":\"2.0\",\"id\":22,\"method\":\"tools/call\"," SB-APPEND
   S\" \"params\":{\"name\":\"shape\",\"arguments\":{\"flat\":true}}}" SB-APPEND SB$ POST
   FAULTED
   SHAPE-CODE atomic@ MCP:E-SCHEMA T=
   SHAPE-TEXT SHAPE-LEN
   s" result 1:1: expected a JSON object, as the tool's outputSchema has type `object`, found `[`; answer one JSON object"
   KEPT= ;


\ framed's arguments, its result's _meta, with space a modern result trims.
: SPACED ( -- ^u8 n ) S\" { \"a\":1, \"b\":[2] }" ;


: LEGACY-FRAMED ( n str -- n )
   {: id:n args:^ au:n :}
   LEGACY
   SB-RESET S\" {\"jsonrpc\":\"2.0\",\"id\":" SB-APPEND id FMT:SB-U
   S\" ,\"method\":\"tools/call\",\"params\":{\"name\":\"framed\",\"arguments\":" SB-APPEND
   args au SB-APPEND S\" }}" SB-APPEND SB$ POST ;


: MODERN-FRAMED ( n str -- n )
   {: id:n args:^ au:n :}
   MODERN s" Mcp-Method: tools/call" ADD s" Mcp-Name: framed" ADD
   SB-RESET S\" {\"jsonrpc\":\"2.0\",\"id\":" SB-APPEND id FMT:SB-U
   S\" ,\"method\":\"tools/call\",\"params\":{\"name\":\"framed\",\"arguments\":" SB-APPEND
   args au SB-APPEND s" ," SB-APPEND META S\" }}" SB-APPEND SB$ POST ;


\ framed's reply to the given id through isError, in SB.
: FRAMED-RESULT ( n bool -- )
   {: id:n modern:bool :}
   SB-RESET S\" {\"jsonrpc\":\"2.0\",\"id\":" SB-APPEND id FMT:SB-U
   S\" ,\"result\":{" SB-APPEND
   modern if S\" \"resultType\":\"complete\"," SB-APPEND then
   S\" \"content\":[],\"structuredContent\":{\"ok\":true},\"isError\":false" SB-APPEND ;


: META-LEGACY ( -- )
   s" a legacy result's _meta is RESULT-META's meta as given" T-LABEL
   24 SPACED LEGACY-FRAMED
   24 false FRAMED-RESULT S\" ,\"_meta\":" SB-APPEND SPACED SB-APPEND S\" }}" SB-APPEND
   200 SB$ ANSWERED ;


: META-MODERN ( -- )
   s" a modern result's _meta holds RESULT-META's members, then serverInfo" T-LABEL
   25 SPACED MODERN-FRAMED
   25 true FRAMED-RESULT
   S\" ,\"_meta\":{\"a\":1, \"b\":[2],\"io.modelcontextprotocol/serverInfo\":" SB-APPEND
   S\" {\"name\":\"test-server\",\"version\":\"1.0\"}}}}" SB-APPEND
   200 SB$ ANSWERED ;


: META-EMPTY ( -- )
   s" a modern result whose meta has no members holds serverInfo alone" T-LABEL
   26 s" { }" MODERN-FRAMED
   26 true FRAMED-RESULT s" ," SB-APPEND SERVER S\" }}" SB-APPEND
   200 SB$ ANSWERED ;


: META-NOT-OBJECT ( -- )
   s" RESULT-META meta that is not an object faults to the host" T-LABEL
   0 META-CODE atomic!
   27 S\" {\"flat\":true}" LEGACY-FRAMED FAULTED
   META-CODE atomic@ MCP:E-SCHEMA T=
   META-TEXT META-LEN
   s" meta 1:1: expected a JSON object, found `[`; write it as one object"
   KEPT= ;


: META-REPEATED ( -- )
   s" RESULT-META meta that repeats a top-level key faults to the host" T-LABEL
   0 META-CODE atomic!
   29 S\" {\"twice\":true}" LEGACY-FRAMED FAULTED
   META-CODE atomic@ MCP:E-SCHEMA T=
   META-TEXT META-LEN
   S\" meta 1:8: expected each top-level member once, found `\"a\"`; remove the repeated member, keeping one"
   KEPT= ;


: META-SERVER-INFO ( -- )
   s" RESULT-META meta naming serverInfo, escaped, faults to the host" T-LABEL
   0 META-CODE atomic!
   28 S\" {\"io.modelcontextprotocol\\/serverInfo\":{}}" LEGACY-FRAMED FAULTED
   META-CODE atomic@ MCP:E-SCHEMA T=
   META-TEXT META-LEN
   S\" meta 1:2: expected no member `io.modelcontextprotocol/serverInfo`, found `\"io.modelcontextprotocol\\/server...`; remove it: the server writes its own"
   KEPT= ;


\ pictured's blocks: an image and an embedded resource's bytes, spaced as given.
: PAIR ( -- ^u8 n )
   S\" [{\"type\":\"image\",\"data\":\"iVBORw0KGgo=\",\"mimeType\":\"image/png\"}, {\"type\":\"resource\",\"resource\":{\"uri\":\"zen://versions/7/original\",\"mimeType\":\"application/pdf\",\"blob\":\"JVBERi0=\"}} ]" ;

\ pictured's blocks for said: a text of the tool's own.
: OWN-TEXT ( -- ^u8 n )
   S\" [{\"type\":\"text\",\"text\":\"500 of 568090 records\"}]" ;

\ The objects of a JSON array, as content holds them.
: INSIDE ( str -- str ) {: a:^ u:n :} a 1+ u 2 - ;

: PAIR-CONTENT ( -- str ) PAIR INSIDE ;


: PICTURED-CALL ( n str bool -- n )
   {: id:n args:^ au:n modern:bool :}
   modern if MODERN s" Mcp-Method: tools/call" ADD s" Mcp-Name: pictured" ADD else LEGACY then
   SB-RESET S\" {\"jsonrpc\":\"2.0\",\"id\":" SB-APPEND id FMT:SB-U
   S\" ,\"method\":\"tools/call\",\"params\":{\"name\":\"pictured\",\"arguments\":" SB-APPEND
   args au SB-APPEND
   modern if s" ," SB-APPEND META then
   S\" }}" SB-APPEND SB$ POST ;


\ pictured's reply to the given id, the content's objects as given, in SB.
: PICTURED-RESULT ( n bool str -- )
   {: id:n modern:bool blocks:^ bu:n :}
   SB-RESET S\" {\"jsonrpc\":\"2.0\",\"id\":" SB-APPEND id FMT:SB-U
   S\" ,\"result\":{" SB-APPEND
   modern if S\" \"resultType\":\"complete\"," SB-APPEND then
   S\" \"content\":[" SB-APPEND blocks bu SB-APPEND
   S\" ],\"structuredContent\":{\"ok\":true},\"isError\":false" SB-APPEND
   modern if s" ," SB-APPEND SERVER then
   S\" }}" SB-APPEND ;


: BLOCKS-LEGACY ( -- )
   s" a legacy result's content is RESULT-CONTENT's content blocks as given" T-LABEL
   41 S\" {\"pair\":1}" false PICTURED-CALL
   41 false PAIR-CONTENT PICTURED-RESULT
   200 SB$ ANSWERED ;


: BLOCKS-MODERN ( -- )
   s" a modern result's content is RESULT-CONTENT's content blocks as given" T-LABEL
   42 S\" {\"pair\":1}" true PICTURED-CALL
   42 true PAIR-CONTENT PICTURED-RESULT
   200 SB$ ANSWERED ;


: BLOCKS-OWN-TEXT ( -- )
   s" RESULT-CONTENT's text block is the whole content, its JSON structuredContent" T-LABEL
   45 S\" {\"said\":1}" true PICTURED-CALL
   45 true OWN-TEXT INSIDE PICTURED-RESULT
   200 SB$ ANSWERED ;


: BLOCKS-EMPTY ( -- )
   s" RESULT-CONTENT of an empty array gives the result RESULT gives" T-LABEL
   43 S\" {\"none\":1}" true PICTURED-CALL
   43 true s" " PICTURED-RESULT
   200 SB$ ANSWERED ;


: BLOCKS-REFUSED ( str str str -- )
   {: label:^ lu:n args:^ au:n want:^ wu:n :}
   label lu T-LABEL
   0 BLOCKS-CODE atomic!
   44 args au false PICTURED-CALL FAULTED
   BLOCKS-CODE atomic@ MCP:E-SCHEMA T=
   BLOCKS-TEXT BLOCKS-LEN want wu KEPT= ;


: BLOCKS-REFUSALS ( -- )
   s" RESULT-CONTENT of an object, not an array, faults to the host"
   S\" {\"flat\":1}"
   s" content 1:1: expected a JSON array of content blocks, found `{`; write the blocks in an array"
   BLOCKS-REFUSED
   s" RESULT-CONTENT of an array holding a number after a block faults to the host"
   S\" {\"scalar\":1}"
   s" content 1:29: expected a content block object, found `1`; write each block as an object"
   BLOCKS-REFUSED
   s" RESULT-CONTENT of an object without a type faults to the host"
   S\" {\"untyped\":1}"
   s" content 1:13: expected a member `type`, found `}`; give the block its `type`"
   BLOCKS-REFUSED
   s" RESULT-CONTENT of an object whose type is a number faults to the host"
   S\" {\"numbered\":1}"
   s" content 1:10: expected `type` as a string, found `5`; write `type` as a JSON string"
   BLOCKS-REFUSED
   s" RESULT-CONTENT of an object repeating its type faults to the host"
   S\" {\"twice\":1}"
   s" content 1:24: expected one member `type`, found `5`; remove the repeated `type`"
   BLOCKS-REFUSED
   s" RESULT-CONTENT of a text that is not JSON faults to the host"
   S\" {\"broken\":1}"
   s" content 1:10: expected a value, found the end of the input; write the rest of the document, closing each open array with `]` and object with `}`"
   BLOCKS-REFUSED ;


: BAD-JSON ( -- )
   s" a body that is not JSON is -32700 with no id" T-LABEL
   LEGACY S\" {\"jsonrpc\":" POST
   400 S\" {\"jsonrpc\":\"2.0\",\"error\":{\"code\":-32700,\"message\":\"1:12: expected a value, found the end of the input; write the rest of the document, closing each open array with `]` and object with `}`\"}}"
   ANSWERED ;


\ The advertised outputSchema wrapping a declared schema that is not an object's.
: WRAPPER ( ^u8 n -- )
   {: declared:^ du:n :}
   S\" \"outputSchema\":{\"type\":\"object\",\"properties\":{\"data\":" W+ declared du W+
   S\" },\"required\":[\"data\"]}" W+ ;

: OUTPUT ( ^u8 n -- ) WRAPPER S\" ,\"annotations\":{\"readOnlyHint\":" W+ ;

\ shape's declared output schema, advertised as declared.
: SOURCES ( -- ^u8 n )
   S\" {\"typ\\u0065\":\"object\",\"properties\":{\"sources\":{\"type\":\"array\"}}}" ;

\ silent's declared output schema: no string "type" "object", so it is wrapped.
: NULLABLE ( -- ^u8 n )
   S\" {\"type\":[\"object\",\"null\"],\"items\":{\"type\":\"object\"}}" ;

\ An app's tool, listed as registered: its outputSchema is an object's.
: WIDGET ( -- ^u8 n )
   S\" {\"name\":\"widget\",\"description\":\"Show the widget.\",\"inputSchema\":{\"type\":\"object\"},\"outputSchema\":{\"type\":\"object\"},\"annotations\":{\"readOnlyHint\":true,\"destructiveHint\":false,\"openWorldHint\":false},\"_meta\":{\"ui\":{\"resourceUri\":\"ui://test/widget.html\",\"visibility\":[\"model\",\"app\"]},\"openai/fileParams\":[\"file\"]}}" ;

\ A tool whose escaped outputSchema key and non-object schema sit between members.
: PICKER ( -- str )
   S\" {\"name\":\"pick\",\"title\": \"Pick one\",\"n\": 1.5e0,\"tags\": [ \"a\" ],\"ok\": true,\"outputSch\\u0065ma\":{\"type\":\"string\"},\"inputSchema\":{\"type\":\"object\"},\"_meta\":{\"ui\":{\"visibility\":[\"app\"]}}}" ;

\ pick's entry, members as registered and the wrapper in its schema's place.
: PICK-ITEM ( -- )
   S\" {\"name\":\"pick\",\"title\":\"Pick one\",\"n\":1.5e0,\"tags\":[ \"a\" ],\"ok\":true," W+
   S\" {\"type\":\"string\"}" WRAPPER
   S\" ,\"inputSchema\":{\"type\":\"object\"},\"_meta\":{\"ui\":{\"visibility\":[\"app\"]}}}" W+ ;

\ A tool whose body gives a result with a _meta, listed as registered.
: FRAMED ( -- ^u8 n )
   S\" {\"name\":\"framed\",\"description\":\"Answer with a _meta.\",\"inputSchema\":{\"type\":\"object\"},\"outputSchema\":{\"type\":\"object\"},\"annotations\":{\"readOnlyHint\":true}}" ;

\ A tool whose body gives a result with content blocks, listed as registered.
: PICTURED ( -- ^u8 n )
   S\" {\"name\":\"pictured\",\"description\":\"Answer with content blocks.\",\"inputSchema\":{\"type\":\"object\"},\"outputSchema\":{\"type\":\"object\"},\"annotations\":{\"readOnlyHint\":true}}" ;

\ echo's entry, the first tools/list answers.
: ECHO-ITEM ( -- )
   S\" {\"name\":\"echo\"," W+
   S\" \"description\":\"Echo the arguments.\",\"inputSchema\":{\"type\":\"object\"}," W+
   s" {}" OUTPUT s" true}}" W+ ;

\ The open phase's tools after echo, as tools/list answers them.
: LATER-ITEMS ( -- )
   S\" {\"name\":\"refuse\",\"description\":\"Refuse every call.\"," W+
   S\" \"inputSchema\":{\"type\":\"object\",\"properties\":{}}," W+
   s" {}" OUTPUT s" false}},{" W+
   S\" \"name\":\"raw\",\"description\":\"Answer text that is not JSON.\"," W+
   S\" \"inputSchema\":{\"type\":\"object\"}," W+
   s" {}" OUTPUT s" true}},{" W+
   S\" \"name\":\"silent\",\"description\":\"Never answer.\"," W+
   S\" \"inputSchema\":{\"type\":\"object\"}," W+
   NULLABLE OUTPUT s" true}},{" W+
   S\" \"name\":\"big\",\"description\":\"Answer a long string.\"," W+
   S\" \"inputSchema\":{\"type\":\"object\"}," W+
   S\" {\"type\":\"string\"}" OUTPUT s" true}},{" W+
   S\" \"name\":\"stall\",\"description\":\"Answer once released.\"," W+
   S\" \"inputSchema\":{\"type\":\"object\"}," W+
   s" {}" OUTPUT s" true}},{" W+
   S\" \"name\":\"shape\",\"description\":\"Answer sources.\"," W+
   S\" \"inputSchema\":{\"type\":\"object\"},\"outputSchema\":" W+ SOURCES W+
   S\" ,\"annotations\":{\"readOnlyHint\":true}}," W+ WIDGET W+ s" ," W+ PICK-ITEM
   s" ," W+ FRAMED W+ s" ," W+ PICTURED W+
   S\" ,{\"name\":\"wait\",\"description\":\"Answer once woken.\"," W+
   S\" \"inputSchema\":{\"type\":\"object\"}," W+
   s" {}" OUTPUT s" true}},{" W+
   S\" \"name\":\"strict\",\"description\":\"Answer a filter.\",\"inputSchema\":" W+
   S\" {\"type\":\"object\",\"properties\":{\"filter\":{\"type\":\"string\"}}," W+
   S\" \"required\":[\"filter\"],\"additionalProperties\":false}," W+
   s" {}" OUTPUT s" true}}" W+ ;

\ Every tool the open phase registers, as tools/list answers them, before the
\ array's end.
: TOOL-ITEMS ( -- ) s" [" W+ ECHO-ITEM s" ," W+ LATER-ITEMS ;

: TOOL-ARRAY ( -- ) TOOL-ITEMS s" ]" W+ ;


: LIST ( -- )
   s" tools/list shows each descriptor, a non-object outputSchema as the wrapper" T-LABEL
   LEGACY S\" {\"jsonrpc\":\"2.0\",\"id\":10,\"method\":\"tools/list\"}" POST
   W-RESET S\" {\"jsonrpc\":\"2.0\",\"id\":10,\"result\":{\"tools\":" W+
   TOOL-ARRAY s" }}" W+
   200 W$ ANSWERED ;


: MODERN-LIST ( -- )
   s" a modern tools/list adds resultType, cache scope and serverInfo" T-LABEL
   MODERN s" Mcp-Method: tools/list" ADD
   SB-RESET S\" {\"jsonrpc\":\"2.0\",\"id\":12,\"method\":\"tools/list\",\"params\":{" SB-APPEND
   META S\" }}" SB-APPEND SB$ POST
   W-RESET S\" {\"jsonrpc\":\"2.0\",\"id\":12,\"result\":{\"resultType\":\"complete\"," W+
   S\" \"tools\":" W+ TOOL-ARRAY S\" ,\"ttlMs\":0,\"cacheScope\":\"private\"," W+
   W-SERVER S\" }}" W+
   200 W$ ANSWERED ;


: ECHO-CALL ( n -- CURL:handle str )
   {: id:n :}
   MODERN s" Mcp-Method: tools/call" ADD
   SB-RESET S\" {\"jsonrpc\":\"2.0\",\"id\":" SB-APPEND id FMT:SB-U
   S\" ,\"method\":\"tools/call\",\"params\":{\"name\":\"echo\"," SB-APPEND
   META S\" }}" SB-APPEND SB$ ;


: NAME-MISMATCH ( -- )
   s" a modern Mcp-Name naming another tool is -32020" T-LABEL
   13 ECHO-CALL {: h:CURL:handle a:^ u:n :}
   h s" Mcp-Name: refuse" ADD a u POST
   400 S\" {\"jsonrpc\":\"2.0\",\"id\":13,\"error\":{\"code\":-32020,\"message\":\"Header mismatch\"}}"
   ANSWERED ;


: NAME-ABSENT ( -- )
   s" a modern tools/call without Mcp-Name is -32020" T-LABEL
   14 ECHO-CALL POST
   400 S\" {\"jsonrpc\":\"2.0\",\"id\":14,\"error\":{\"code\":-32020,\"message\":\"Header mismatch\"}}"
   ANSWERED ;


: META-MISMATCH ( -- )
   s" a _meta protocolVersion other than the header's is -32020" T-LABEL
   MODERN s" Mcp-Method: tools/list" ADD
   SB-RESET S\" {\"jsonrpc\":\"2.0\",\"id\":15,\"method\":\"tools/list\",\"params\":{" SB-APPEND
   S\" \"_meta\":{\"io.modelcontextprotocol/protocolVersion\":\"2025-11-25\"," SB-APPEND
   S\" \"io.modelcontextprotocol/clientCapabilities\":{}}}}" SB-APPEND SB$ POST
   400 S\" {\"jsonrpc\":\"2.0\",\"id\":15,\"error\":{\"code\":-32020,\"message\":\"Header mismatch\"}}"
   ANSWERED ;


: NO-CAPABILITIES ( -- )
   s" a modern request without clientCapabilities is 400 -32602" T-LABEL
   MODERN s" Mcp-Method: tools/list" ADD
   SB-RESET S\" {\"jsonrpc\":\"2.0\",\"id\":16,\"method\":\"tools/list\",\"params\":{" SB-APPEND
   S\" \"_meta\":{\"io.modelcontextprotocol/protocolVersion\":\"2026-07-28\"}}}" SB-APPEND
   SB$ POST
   SB-RESET S\" {\"jsonrpc\":\"2.0\",\"id\":16,\"error\":{\"code\":-32602," SB-APPEND
   S\" \"message\":\"Client capabilities required\"}}" SB-APPEND
   400 SB$ ANSWERED ;


: SCALAR-META ( -- )
   s" a modern request whose _meta is no object is 400 -32602 naming params._meta" T-LABEL
   MODERN s" Mcp-Method: tools/list" ADD
   S\" {\"jsonrpc\":\"2.0\",\"id\":85,\"method\":\"tools/list\",\"params\":{\"_meta\":5}}" POST
   SB-RESET S\" {\"jsonrpc\":\"2.0\",\"id\":85,\"error\":{\"code\":-32602,\"message\":" SB-APPEND
   S\" \"`params._meta`: expected a JSON object; send params._meta as one object\"}}" SB-APPEND
   400 SB$ ANSWERED ;

: NO-PARAMS ( -- )
   s" a modern request without params is 400 -32602" T-LABEL
   MODERN s" Mcp-Method: tools/list" ADD
   S\" {\"jsonrpc\":\"2.0\",\"id\":17,\"method\":\"tools/list\"}" POST
   SB-RESET S\" {\"jsonrpc\":\"2.0\",\"id\":17,\"error\":{\"code\":-32602," SB-APPEND
   S\" \"message\":\"Protocol version required\"}}" SB-APPEND
   400 SB$ ANSWERED ;


: MODERN-NOTIFICATION ( -- )
   s" a modern notification is 400 -32600 with no id" T-LABEL
   MODERN s" Mcp-Method: tools/list" ADD
   SB-RESET S\" {\"jsonrpc\":\"2.0\",\"method\":\"tools/list\",\"params\":{" SB-APPEND
   META S\" }}" SB-APPEND SB$ POST
   400 S\" {\"jsonrpc\":\"2.0\",\"error\":{\"code\":-32600,\"message\":\"Request id required\"}}"
   ANSWERED ;


: LEGACY-NOTIFIED ( -- )
   s" a legacy notification named initialize is accepted with no body" T-LABEL
   LEGACY S\" {\"jsonrpc\":\"2.0\",\"method\":\"initialize\",\"params\":{}}" POST
   202 s" " ANSWERED ;


: ARGUMENTS-NOT-OBJECT ( -- )
   s" arguments that are not an object are 400 -32602" T-LABEL
   LEGACY
   SB-RESET S\" {\"jsonrpc\":\"2.0\",\"id\":18,\"method\":\"tools/call\"," SB-APPEND
   S\" \"params\":{\"name\":\"echo\",\"arguments\":5}}" SB-APPEND SB$ POST
   SB-RESET S\" {\"jsonrpc\":\"2.0\",\"id\":18,\"error\":{\"code\":-32602," SB-APPEND
   S\" \"message\":\"`arguments`: expected a JSON object; send arguments as one object\"}}" SB-APPEND
   400 SB$ ANSWERED ;

: PARAMS-NOT-OBJECT ( -- )
   s" legacy params that are not an object are 400 -32602 naming params" T-LABEL
   LEGACY S\" {\"jsonrpc\":\"2.0\",\"id\":87,\"method\":\"ping\",\"params\":[1]}" POST
   SB-RESET S\" {\"jsonrpc\":\"2.0\",\"id\":87,\"error\":{\"code\":-32602," SB-APPEND
   S\" \"message\":\"`params`: expected a JSON object; send params as one object\"}}" SB-APPEND
   400 SB$ ANSWERED ;

\ A legacy tools/call with id n whose params name `name` twice.
: NAME-TWICE ( n -- str )
   {: id:n :}
   SB-RESET S\" {\"jsonrpc\":\"2.0\",\"id\":" SB-APPEND id FMT:SB-U
   S\" ,\"method\":\"tools/call\",\"params\":{\"name\":\"echo\",\"name\":\"echo\"}}" SB-APPEND
   SB$ ;

\ The reader's text for NAME-TWICE's body, either side of the repeated name.
: TWICE-HEAD ( -- str )
   s" 1:72: expected a member name not used before in this object, found `" ;
: TWICE-TAIL ( -- str ) s" `; remove the repeated member" ;

: NAME-REPEATED ( -- )
   s" params naming name twice are 400 -32600 with no id and the reader's text" T-LABEL
   LEGACY 83 NAME-TWICE POST
   SB-RESET S\" {\"jsonrpc\":\"2.0\",\"error\":{\"code\":-32600,\"message\":\"" SB-APPEND
   TWICE-HEAD SB-APPEND S\" \\\"name\\\"" SB-APPEND TWICE-TAIL SB-APPEND
   S\" \"}}" SB-APPEND
   400 SB$ ANSWERED ;


: NOT-JSON-RESULT ( -- )
   s" a body giving a result text that is not JSON faults to the host" T-LABEL
   LEGACY
   S\" {\"jsonrpc\":\"2.0\",\"id\":19,\"method\":\"tools/call\",\"params\":{\"name\":\"raw\"}}" POST
   FAULTED ;


: SILENT ( -- )
   s" a body that never gives its result faults to the host" T-LABEL
   LEGACY
   S\" {\"jsonrpc\":\"2.0\",\"id\":20,\"method\":\"tools/call\",\"params\":{\"name\":\"silent\"}}" POST
   FAULTED ;


: PING ( -- )
   s" a legacy ping answers an empty result" T-LABEL
   LEGACY S\" {\"jsonrpc\":\"2.0\",\"id\":11,\"method\":\"ping\"}" POST
   200 S\" {\"jsonrpc\":\"2.0\",\"id\":11,\"result\":{}}" ANSWERED ;


\ The widget resource's descriptor, as resources/list answers it.
: WIDGET-RESOURCE ( -- str )
   S\" {\"uri\":\"ui://test/widget.html\",\"name\":\"widget\",\"mimeType\":\"text/html;profile=mcp-app\",\"description\":\"A test widget.\",\"_meta\":{\"ui\":{\"csp\":{\"resourceDomains\":[\"https://cdn.test\"]}}}}" ;

\ A resource without _meta, as resources/list answers it.
: PLAIN ( -- ^u8 n )
   S\" {\"uri\":\"ui://test/plain.html\",\"name\":\"plain\",\"mimeType\":\"text/html\"}" ;

: RESOURCE-ITEMS ( -- ) WIDGET-RESOURCE W+ s" ," W+ PLAIN W+ ;

\ Its page: a quote and a newline show the text's escaping.
: PAGE ( -- ^u8 n ) S\" <p class=\"note\">Hi</p>\n" ;

\ The widget's resources/read contents, after the result's opening brace.
: W-CONTENTS ( -- )
   S\" \"contents\":[{\"uri\":\"ui://test/widget.html\",\"mimeType\":\"text/html;profile=mcp-app\"," W+
   S\" \"_meta\":{\"ui\":{\"csp\":{\"resourceDomains\":[\"https://cdn.test\"]}}}," W+
   S\" \"text\":\"<p class=\\\"note\\\">Hi</p>\\n\"}]" W+ ;


: RESOURCES-LIST ( -- )
   s" legacy resources/list shows each descriptor as registered" T-LABEL
   LEGACY S\" {\"jsonrpc\":\"2.0\",\"id\":30,\"method\":\"resources/list\"}" POST
   W-RESET S\" {\"jsonrpc\":\"2.0\",\"id\":30,\"result\":{\"resources\":[" W+
   RESOURCE-ITEMS s" ]}}" W+
   200 W$ ANSWERED ;


: MODERN-RESOURCES ( -- )
   s" a modern resources/list adds resultType, cache scope and serverInfo" T-LABEL
   MODERN s" Mcp-Method: resources/list" ADD
   SB-RESET S\" {\"jsonrpc\":\"2.0\",\"id\":31,\"method\":\"resources/list\",\"params\":{" SB-APPEND
   META S\" }}" SB-APPEND SB$ POST
   W-RESET S\" {\"jsonrpc\":\"2.0\",\"id\":31,\"result\":{\"resultType\":\"complete\"," W+
   S\" \"resources\":[" W+ RESOURCE-ITEMS S\" ],\"ttlMs\":0,\"cacheScope\":\"private\"," W+
   W-SERVER S\" }}" W+
   200 W$ ANSWERED ;


: RESOURCE-READ ( -- )
   s" legacy resources/read answers the page with the descriptor's uri, mimeType and _meta"
   T-LABEL
   LEGACY S\" {\"jsonrpc\":\"2.0\",\"id\":32,\"method\":\"resources/read\",\"params\":{\"uri\":\"ui://test/widget.html\"}}" POST
   W-RESET S\" {\"jsonrpc\":\"2.0\",\"id\":32,\"result\":{" W+ W-CONTENTS s" }}" W+
   200 W$ ANSWERED ;


: PLAIN-READ ( -- )
   s" resources/read of a descriptor without _meta answers uri, mimeType and text" T-LABEL
   LEGACY S\" {\"jsonrpc\":\"2.0\",\"id\":38,\"method\":\"resources/read\",\"params\":{\"uri\":\"ui://test/plain.html\"}}" POST
   W-RESET S\" {\"jsonrpc\":\"2.0\",\"id\":38,\"result\":{\"contents\":[{" W+
   S\" \"uri\":\"ui://test/plain.html\",\"mimeType\":\"text/html\",\"text\":\"<p></p>\"}]}}" W+
   200 W$ ANSWERED ;


: WIDGET-READ ( n str -- n )
   {: id:n name:^ nu:n :}
   MODERN s" Mcp-Method: resources/read" ADD name nu ADD
   SB-RESET S\" {\"jsonrpc\":\"2.0\",\"id\":" SB-APPEND id FMT:SB-U
   S\" ,\"method\":\"resources/read\",\"params\":{\"uri\":\"ui://test/widget.html\"," SB-APPEND
   META S\" }}" SB-APPEND SB$ POST ;


: MODERN-READ ( -- )
   s" a modern resources/read whose Mcp-Name names params.uri answers the page" T-LABEL
   33 s" Mcp-Name: ui://test/widget.html" WIDGET-READ
   W-RESET S\" {\"jsonrpc\":\"2.0\",\"id\":33,\"result\":{\"resultType\":\"complete\"," W+
   W-CONTENTS S\" ,\"ttlMs\":0,\"cacheScope\":\"private\"," W+ W-SERVER s" }}" W+
   200 W$ ANSWERED ;


: READ-MISMATCH ( -- )
   s" a modern Mcp-Name naming another resource is -32020" T-LABEL
   34 s" Mcp-Name: ui://test/other.html" WIDGET-READ
   400 S\" {\"jsonrpc\":\"2.0\",\"id\":34,\"error\":{\"code\":-32020,\"message\":\"Header mismatch\"}}"
   ANSWERED ;


: UNKNOWN-RESOURCE ( -- )
   s" an unregistered uri is 404 -32002 with the uri as data" T-LABEL
   LEGACY S\" {\"jsonrpc\":\"2.0\",\"id\":35,\"method\":\"resources/read\",\"params\":{\"uri\":\"ui://test/none.html\"}}" POST
   SB-RESET S\" {\"jsonrpc\":\"2.0\",\"id\":35,\"error\":{\"code\":-32002," SB-APPEND
   S\" \"message\":\"Resource not found\",\"data\":{\"uri\":\"ui://test/none.html\"}}}" SB-APPEND
   404 SB$ ANSWERED ;


: URI-NOT-STRING ( -- )
   s" a resources/read uri that is no string is 400 -32602" T-LABEL
   LEGACY S\" {\"jsonrpc\":\"2.0\",\"id\":36,\"method\":\"resources/read\",\"params\":{\"uri\":5}}" POST
   SB-RESET S\" {\"jsonrpc\":\"2.0\",\"id\":36,\"error\":{\"code\":-32602," SB-APPEND
   S\" \"message\":\"`uri`: expected a JSON string; send the resource's uri as a string\"}}" SB-APPEND
   400 SB$ ANSWERED ;


: TEMPLATES ( -- )
   s" a modern resources/templates/list answers no templates" T-LABEL
   MODERN s" Mcp-Method: resources/templates/list" ADD
   SB-RESET S\" {\"jsonrpc\":\"2.0\",\"id\":37,\"method\":\"resources/templates/list\",\"params\":{" SB-APPEND
   META S\" }}" SB-APPEND SB$ POST
   W-RESET S\" {\"jsonrpc\":\"2.0\",\"id\":37,\"result\":{\"resultType\":\"complete\"," W+
   S\" \"resourceTemplates\":[],\"ttlMs\":0,\"cacheScope\":\"private\"," W+ W-SERVER s" }}" W+
   200 W$ ANSWERED ;


: ADD-ECHO ( -- )
   S\" {\"name\":\"echo\",\"description\":\"Echo the arguments.\",\"inputSchema\":{\"type\":\"object\"},\"outputSchema\":{},\"annotations\":{\"readOnlyHint\":true}}"
   [: MCP:RESULT ;] MCP:ADD-TOOL ;

: ADD-REFUSE ( -- )
   S\" {\"name\":\"refuse\",\"description\":\"Refuse every call.\",\"inputSchema\":{\"type\":\"object\",\"properties\":{}},\"outputSchema\":{},\"annotations\":{\"readOnlyHint\":false}}"
   [: 2drop s" nope" MCP:RESULT-ERROR ;] MCP:ADD-TOOL ;

: ADD-RAW ( -- )
   S\" {\"name\":\"raw\",\"description\":\"Answer text that is not JSON.\",\"inputSchema\":{\"type\":\"object\"},\"outputSchema\":{},\"annotations\":{\"readOnlyHint\":true}}"
   [: 2drop s" not json" MCP:RESULT ;] MCP:ADD-TOOL ;

: ADD-SILENT ( -- )
   S\" {\"name\":\"silent\",\"description\":\"Never answer.\",\"inputSchema\":{\"type\":\"object\"},\"outputSchema\":{\"type\":[\"object\",\"null\"],\"items\":{\"type\":\"object\"}},\"annotations\":{\"readOnlyHint\":true}}"
   [: 2drop ;] MCP:ADD-TOOL ;

: ADD-BIG ( -- )
   S\" {\"name\":\"big\",\"description\":\"Answer a long string.\",\"inputSchema\":{\"type\":\"object\"},\"outputSchema\":{\"type\":\"string\"},\"annotations\":{\"readOnlyHint\":true}}"
   [: 2drop BIG BIG-U MCP:RESULT ;] MCP:ADD-TOOL ;

: ADD-STALL ( -- )
   S\" {\"name\":\"stall\",\"description\":\"Answer once released.\",\"inputSchema\":{\"type\":\"object\"},\"outputSchema\":{},\"annotations\":{\"readOnlyHint\":true}}"
   [: 2drop 1 ENTERED atomic!
      [: RELEASED atomic@ 0<> ;] TEST-WAIT:POLL
      1 LEFT atomic!
      s" {}" MCP:RESULT ;] MCP:ADD-TOOL ;

\ A tool whose arguments are one string filter.
: ADD-STRICT ( -- )
   S\" {\"name\":\"strict\",\"description\":\"Answer a filter.\",\"inputSchema\":{\"type\":\"object\",\"properties\":{\"filter\":{\"type\":\"string\"}},\"required\":[\"filter\"],\"additionalProperties\":false},\"outputSchema\":{},\"annotations\":{\"readOnlyHint\":true}}"
   [: 2drop 1 STRICT-RAN atomic! S\" {\"ran\":true}" MCP:RESULT ;] MCP:ADD-TOOL ;

: ADD-LATE ( -- )
   S\" {\"name\":\"late\",\"description\":\"Registered while serving.\",\"inputSchema\":{\"type\":\"object\"},\"outputSchema\":{},\"annotations\":{\"readOnlyHint\":true}}"
   [: MCP:RESULT ;] MCP:ADD-TOOL ;


\ Keeps code's text where a case on another task reads it.
: KEEP ( n ^u8 ^n -- ) {: code:n buf:^ len:^ :}
   code REASON:TEXT$ KEPT-CAP min {: a:^ u:n :}
   a buf u BYTE-COPY  u len atomic! ;

: SHAPE-TRY ( str -- )
   [: 2dup MCP:RESULT ;] catch {: code:n :} 2drop
   code SHAPE-TEXT SHAPE-LEN KEEP
   code SHAPE-CODE atomic!
   code 0<> if code throw then ;


: ADD-SHAPE ( -- )
   S\" {\"name\":\"shape\",\"description\":\"Answer sources.\",\"inputSchema\":{\"type\":\"object\"},\"outputSchema\":{\"typ\\u0065\":\"object\",\"properties\":{\"sources\":{\"type\":\"array\"}}},\"annotations\":{\"readOnlyHint\":true}}"
   [: s" flat" STR:CONTAINS? if s" []" else S\" {\"sources\":[\"a\"]}" then SHAPE-TRY ;]
   MCP:ADD-TOOL ;


: ADD-WIDGET ( -- ) WIDGET [: MCP:RESULT ;] MCP:ADD-TOOL ;
: ADD-PICK ( -- ) PICKER [: MCP:RESULT ;] MCP:ADD-TOOL ;
: ADD-DESCRIBED ( str -- ) [: MCP:RESULT ;] MCP:ADD-TOOL ;


: META-TRY ( str -- )
   [: 2dup S\" {\"ok\":true}" 2swap MCP:RESULT-META ;] catch {: code:n :} 2drop
   code META-TEXT META-LEN KEEP
   code META-CODE atomic!
   code 0<> if code throw then ;


\ The meta the framed tool returns for its arguments: a request cannot repeat a
\ member, so `twice` asks for a meta that does.
: FRAMED-META ( str -- str )
   2dup s" flat" STR:CONTAINS? if 2drop s" []" exit then
   2dup s" twice" STR:CONTAINS? if 2drop S\" {\"a\":1,\"a\":2}" exit then ;

: ADD-FRAMED ( -- )
   FRAMED [: FRAMED-META META-TRY ;] MCP:ADD-TOOL ;


: BLOCKS-TRY ( str -- )
   [: 2dup S\" {\"ok\":true}" 2swap MCP:RESULT-CONTENT ;] catch {: code:n :} 2drop
   code BLOCKS-TEXT BLOCKS-LEN KEEP
   code BLOCKS-CODE atomic!
   code 0<> if code throw then ;


: BLOCKS-OF ( str -- str )
   {: args:^ au:n :}
   args au s" pair" STR:CONTAINS? if PAIR exit then
   args au s" said" STR:CONTAINS? if OWN-TEXT exit then
   args au s" none" STR:CONTAINS? if s" [ ]" exit then
   args au s" flat" STR:CONTAINS? if S\" {\"type\":\"image\"}" exit then
   args au s" scalar" STR:CONTAINS? if S\" [{\"type\":\"text\",\"text\":\"a\"},1]" exit then
   args au s" untyped" STR:CONTAINS? if S\" [{\"data\":\"x\"}]" exit then
   args au s" numbered" STR:CONTAINS? if S\" [{\"type\":5}]" exit then
   args au s" twice" STR:CONTAINS? if S\" [{\"type\":\"text\",\"type\":5}]" exit then
   S\" [{\"type\":" ;


: ADD-PICTURED ( -- ) PICTURED [: BLOCKS-OF BLOCKS-TRY ;] MCP:ADD-TOOL ;


: ADD-PAGE ( str -- ) s" <p></p>" MCP:ADD-RESOURCE ;

\ The last E-SCHEMA's text is exactly the one given.
: SAID ( ^u8 n -- ) {: want:^ wu:n :}
   MCP:E-SCHEMA REASON:TEXT$ want wu T$=
   REASON:RELEASE ;

: RESOURCE-REFUSALS ( -- )
   s" ADD-RESOURCE refuses a descriptor without a uri" T-LABEL
   [: S\" {\"name\":\"x\",\"mimeType\":\"text/html\"}" ADD-PAGE ;] MCP:E-SCHEMA TTHROWSQ
   s" descriptor 1:35: expected a member `uri`, found `}`; add `uri`" SAID
   s" ADD-RESOURCE refuses an empty uri" T-LABEL
   [: S\" {\"uri\":\"\",\"name\":\"x\",\"mimeType\":\"text/html\"}" ADD-PAGE ;] MCP:E-SCHEMA TTHROWSQ
   S\" descriptor 1:8: expected `uri` as a non-empty string, found `\"\"`; give `uri` a value" SAID
   s" ADD-RESOURCE refuses a uri that is no string" T-LABEL
   [: S\" {\"uri\":5,\"name\":\"x\",\"mimeType\":\"text/html\"}" ADD-PAGE ;] MCP:E-SCHEMA TTHROWSQ
   s" descriptor 1:8: expected `uri` as a string, found `5`; write `uri` as a JSON string" SAID
   s" ADD-RESOURCE refuses a descriptor without a name" T-LABEL
   [: S\" {\"uri\":\"ui://x\",\"mimeType\":\"text/html\"}" ADD-PAGE ;] MCP:E-SCHEMA TTHROWSQ
   s" descriptor 1:39: expected a member `name`, found `}`; add `name`" SAID
   s" ADD-RESOURCE refuses a descriptor without a mimeType" T-LABEL
   [: S\" {\"uri\":\"ui://x\",\"name\":\"x\"}" ADD-PAGE ;] MCP:E-SCHEMA TTHROWSQ
   s" descriptor 1:27: expected a member `mimeType`, found `}`; add `mimeType`" SAID
   s" ADD-RESOURCE refuses a text that is not UTF-8" T-LABEL
   [: S\" {\"uri\":\"ui://x\",\"name\":\"x\",\"mimeType\":\"text/html\"}"
      S\" <p>\xff</p>" MCP:ADD-RESOURCE ;] MCP:E-SCHEMA TTHROWSQ
   s" page 1:4: expected UTF-8 text, found byte $FF; encode the page as UTF-8" SAID
   s" ADD-RESOURCE refuses a registered uri, escaped" T-LABEL
   [: S\" {\"uri\":\"ui://test/widget\\u002ehtml\",\"name\":\"x\",\"mimeType\":\"text/html\"}"
      ADD-PAGE ;] MCP:E-DUPLICATE TTHROWSQ ;


: URL ( ^u8 n -- ^u8 n )
   {: path:^ pu:n :}
   SB-RESET s" http://127.0.0.1:" SB-APPEND HTTP:PORT FMT:SB-U path pu SB-APPEND SB$ ;

: ASK ( ^u8 n ^u8 n -- ^u8 n bool )
   {: name:^ nu:n args:^ au:n :}
   CLIENT @ name nu args au 0 >MS MCP:CALL ;

: JSON-HEADER ( HTTP:response -- )
   s" Content-Type" s" application/json" HTTP:HEADER! ;

\ tools/list in three pages, the middle one empty.
: PAGED-ROUTE ( HTTP:request HTTP:response -- )
   {: asked:HTTP:request answer:HTTP:response :}
   answer JSON-HEADER
   asked HTTP:BODY$ S\" \"cursor\":\"p2\"" STR:CONTAINS? if
      answer S\" {\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"tools\":[ ],\"nextCursor\":\"p3\"}}"
      HTTP:BODY! exit
   then
   asked HTTP:BODY$ S\" \"cursor\":\"p3\"" STR:CONTAINS? if
      answer S\" {\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"tools\":[{\"name\":\"b\"}]}}"
      HTTP:BODY! exit
   then
   answer S\" {\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"tools\":[{\"name\":\"a\"}],\"nextCursor\":\"p2\"}}"
   HTTP:BODY! ;

\ A result naming the Mcp-Name header as it arrived.
: NAMES-ROUTE ( HTTP:request HTTP:response -- )
   {: asked:HTTP:request answer:HTTP:response :}
   asked s" Mcp-Name" HTTP:HEADER-OF$ drop {: v:^ vu:n :}
   0 NB-U !
   S\" {\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"name\":\"" NB+ v vu NB+ S\" \"}}" NB+
   answer JSON-HEADER
   answer NB NB-U @ HTTP:BODY! ;

\ A notification, then for a call a ping with the client's id, then the
\ response. "none" starts with a priming event, "twice" ends with a stray.
: EVENTS-ROUTE ( HTTP:request HTTP:response -- )
   {: asked:HTTP:request answer:HTTP:response :}
   answer s" Content-Type" s" text/event-stream" HTTP:HEADER!
   asked HTTP:BODY$ S\" \"name\":\"twice\"" STR:CONTAINS? if
      answer S\" data: {\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"content\":[],\"isError\":false,\"isError\":true}}\n\ndata: x\n\n"
      HTTP:BODY! exit
   then
   asked HTTP:BODY$ S\" \"name\":\"stray\"" STR:CONTAINS? if
      answer S\" data: x\n\ndata: {\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"content\":[],\"isError\":false}}\n\ndata: y\n\n"
      HTTP:BODY! exit
   then
   asked HTTP:BODY$ S\" \"name\":\"bad\"" STR:CONTAINS? if
      answer s\" data: \xFF\n\n" HTTP:BODY! exit
   then
   asked HTTP:BODY$ S\" \"name\":\"none\"" STR:CONTAINS? if
      answer S\" id: 0\ndata:\n\ndata: {\"jsonrpc\":\"2.0\",\"method\":\"notifications/message\"}\n\n"
      HTTP:BODY! exit
   then
   asked HTTP:BODY$ s" tools/list" STR:CONTAINS? if
      answer S\" data: {\"jsonrpc\":\"2.0\",\"method\":\"notifications/message\"}\n\nevent: message\ndata: {\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"tools\":[{\"name\":\"s\"}]}}\n\n"
      HTTP:BODY! exit
   then
   answer S\" data: {\"jsonrpc\":\"2.0\",\"method\":\"notifications/message\"}\n\ndata: {\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"ping\"}\n\ndata: {\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"content\":[],\"isError\":false}}\n\n"
   HTTP:BODY! ;

: TWICE-ROUTE ( HTTP:request HTTP:response -- )
   nip {: answer:HTTP:response :}
   answer JSON-HEADER
   answer S\" {\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"content\":[],\"isError\":false,\"isError\":true}}"
   HTTP:BODY! ;

\ A page with a nextCursor after PAGE-MS, or at once the last, empty page once
\ PAGES-END has passed, so a client that held each page to its own limit ends.
: ENDLESS-ROUTE ( HTTP:request HTTP:response -- )
   nip {: answer:HTTP:response :}
   answer JSON-HEADER
   mono-ns PAGES-END @ >= if
      answer S\" {\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"tools\":[]}}" HTTP:BODY! exit
   then
   mono-ns PAGE-MS NS-PER-MS * +
   [: dup mono-ns <= ;] TEST-WAIT:POLL drop
   answer S\" {\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"tools\":[],\"nextCursor\":\"more\"}}"
   HTTP:BODY! ;

\ A first page with a nextCursor after FIRST-PAGE-MS, then a last, empty page,
\ setting ENTERED as it is asked, once the test sets RELEASED.
: LATE-ROUTE ( HTTP:request HTTP:response -- )
   {: asked:HTTP:request answer:HTTP:response :}
   answer JSON-HEADER
   asked HTTP:BODY$ S\" \"cursor\":\"late\"" STR:CONTAINS? if
      1 ENTERED atomic!
      [: RELEASED atomic@ 0<> ;] TEST-WAIT:POLL
      1 LEFT atomic!
      answer S\" {\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"tools\":[]}}" HTTP:BODY! exit
   then
   mono-ns FIRST-PAGE-MS NS-PER-MS * +
   [: dup mono-ns <= ;] TEST-WAIT:POLL drop
   answer S\" {\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"tools\":[],\"nextCursor\":\"late\"}}"
   HTTP:BODY! ;

\ /mcp, counting each request in HITS.
: COUNTED-ROUTE ( HTTP:request HTTP:response -- )
   1 HITS atomic-add drop MCP:HANDLE ;

\ An empty tools/list answered once the test sets RELEASED.
: SLOW-ROUTE ( HTTP:request HTTP:response -- )
   nip {: answer:HTTP:response :}
   [: RELEASED atomic@ 0<> ;] TEST-WAIT:POLL
   1 LEFT atomic!
   answer JSON-HEADER
   answer S\" {\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"tools\":[]}}" HTTP:BODY! ;


: CLIENT-LIST ( -- )
   s" TOOLS answers every tool object in order" T-LABEL
   CLIENT @ 0 >MS MCP:TOOLS W-RESET TOOL-ARRAY W$ T$= ;

: CLIENT-RESULT ( -- )
   s" CALL answers the result object, isError false" T-LABEL
   s" echo" S\" {\"a\":1}" ASK TFALSE
   W-RESET S\" {\"resultType\":\"complete\",\"content\":[]," W+
   S\" \"structuredContent\":{\"data\":{\"a\":1}},\"isError\":false," W+
   W-SERVER s" }" W+ W$ T$= ;

: CLIENT-BLOCKS ( -- )
   s" CALL answers a result whose content holds RESULT-CONTENT's content blocks" T-LABEL
   s" pictured" S\" {\"pair\":1}" ASK TFALSE
   W-RESET S\" {\"resultType\":\"complete\",\"content\":[" W+ PAIR-CONTENT W+
   S\" ],\"structuredContent\":{\"ok\":true},\"isError\":false," W+
   W-SERVER s" }" W+ W$ T$= ;

: CLIENT-FAULT ( -- )
   s" CALL answers a tool error's result, isError true" T-LABEL
   s" refuse" s" {}" ASK TTRUE
   W-RESET S\" {\"resultType\":\"complete\",\"content\":[{\"type\":\"text\",\"text\":\"nope\"}]," W+
   S\" \"isError\":true," W+ W-SERVER s" }" W+ W$ T$= ;

: CLIENT-BIG ( -- )
   s" a result past the first reply buffer arrives whole" T-LABEL
   s" big" s" {}" ASK TFALSE
   W-RESET S\" {\"resultType\":\"complete\",\"content\":[]," W+
   S\" \"structuredContent\":{\"data\":" W+
   BIG BIG-U W+ S\" },\"isError\":false," W+ W-SERVER s" }" W+ W$ T$= ;

create LOG-PATH PATH-CAP allot      \ a case's stderr capture file
variable LOG-PATH-U
create LOG-B BUF:HDR-BYTES allot    \ the capture file's bytes, read back
$A constant LF

: LOG-PATH$ ( -- str ) LOG-PATH LOG-PATH-U @ ;

: LOG-INIT ( -- )
   s" mcp-test" FS:MKTEMP-DIR {: dir:^ dir-u:n :}
   dir dir-u FS:CLEANUP-TREE+
   dir dir-u s" stderr.txt" LOG-PATH FS:JOIN-PATH LOG-PATH-U !
   LOG-B 1 BUF:N>BLEN BUF:INIT ;

\ The whole capture file, valid until the next read.
: LOG$ ( -- str )
   LOG-PATH$ FS:FILE-SIZE {: size:n :}
   LOG-B BUF:CLEAR
   LOG-PATH$ LOG-B size BUF:N>BLEN BUF:GROW SPAN:$ FS:READ-ALL size T=
   LOG-B BUF:SPAN$ BUF:BLEN>N ;

\ The capture's lines, less each `<stamp> info mcp: ` line: the server
\ in this process logs one for each message it dispatches.
: LINES ( -- n )
   LOG$ {: a:^ u:n :}
   0 u 0 ?do
      a i + c@ LF = if 1+ then
      i 0= if true else a i 1- + c@ LF = then if
         a i DATE:TIME-LEN 1- + +  u i - DATE:TIME-LEN 1- -
         s" Z info mcp: " STR:STARTS-WITH? if 1- then
      then
   loop ;

: CAPTURED ( [ -- ] -- )
   LOG-PATH$ STDERR:CAPTURE
   [: STDERR:;CAPTURE ;] finally ;

\ The capture holds one line: MCP's account of a reply from this server, with
\ the rest after the host.
: REPLY-LOGGED ( str -- )
   {: rest:^ ru:n :}
   LINES 1 T=
   SB-RESET s"  error mcp: reply from 127.0.0.1:" SB-APPEND
   HTTP:PORT FMT:SB-U s"  " SB-APPEND rest ru SB-APPEND
   LOG$ SB$ STR:CONTAINS? TTRUE ;

\ The server shares this process's fd 2, so its lines land in the capture.
: SERVER-LOGGED ( -- )
   s" tools/call and resources/read log their method and name, not arguments"
   T-LABEL
   [: LEGACY S\" {\"jsonrpc\":\"2.0\",\"id\":40,\"method\":\"tools/call\",\"params\":{\"name\":\"echo\",\"arguments\":{\"secret\":\"kept\"}}}" POST 200 T=
      LEGACY S\" {\"jsonrpc\":\"2.0\",\"id\":41,\"method\":\"resources/read\",\"params\":{\"uri\":\"ui://test/widget.html\"}}" POST 200 T= ;]
   CAPTURED
   LOG$ S\"  info mcp: tools/call echo\n" STR:CONTAINS? TTRUE
   LOG$ S\"  info mcp: resources/read ui://test/widget.html\n" STR:CONTAINS? TTRUE
   LOG$ s" kept" STR:CONTAINS? TFALSE ;

: CLIENT-UNKNOWN ( -- )
   s" an unknown tool is E-RESPONSE and the client serves the next call" T-LABEL
   [: [: s" nosuch" s" {}" ASK 2drop drop ;] MCP:E-RESPONSE TTHROWSQ ;] CAPTURED
   s" echo" s" {}" ASK TFALSE 2drop ;

: CLIENT-TIMEOUT ( -- )
   s" a call past its limit is E-TRANSFER and the client serves the next call" T-LABEL
   ARM
   [: [: CLIENT @ s" stall" s" {}" 300 >MS MCP:CALL 2drop drop ;]
      MCP:E-TRANSFER TTHROWSQ ;] CAPTURED
   s" the call past its limit logs curl's timeout once, tagged mcp" T-LABEL
   LINES 1 T=
   SB-RESET s"  error mcp: exchange with 127.0.0.1:" SB-APPEND
   HTTP:PORT FMT:SB-U s"  failed: curl 28: Timeout was reached" SB-APPEND
   LOG$ SB$ STR:CONTAINS? TTRUE
   1 RELEASED atomic!
   CLIENT @ s" echo" s" {}" 0 >MS MCP:CALL TFALSE 2drop SETTLE ;

8000001 constant PAST-CURL-INPUT          \ CURL_MAX_INPUT_LENGTH + 1

\ The capture of one call on a client of the URL, which must fail with one
\ logged line; valid until the next read.
: FAILED-ON ( str -- str )
   MCP:OPEN ROUTE-CLIENT !
   [: [: ROUTE-CLIENT @ s" echo" s" {}" 0 >MS MCP:CALL 2drop drop ;]
      MCP:E-TRANSFER TTHROWSQ ;] CAPTURED
   ROUTE-CLIENT @ MCP:CLOSE
   LINES 1 T=  LOG$ ;

: SECRET-CUT ( str -- )
   {: a:^ u:n :}
   a u s"  error mcp: exchange with 127.0.0.1:1 failed: curl " STR:CONTAINS? TTRUE
   a u s" user" STR:CONTAINS? TFALSE
   a u s" SECRET" STR:CONTAINS? TFALSE
   a u s" api_key" STR:CONTAINS? TFALSE
   a u s" /mcp" STR:CONTAINS? TFALSE ;

: HOST-HIDDEN ( str -- )
   {: a:^ u:n :}
   a u s"  error mcp: exchange failed: curl " STR:CONTAINS? TTRUE
   a u s" user" STR:CONTAINS? TFALSE
   a u s" PASS" STR:CONTAINS? TFALSE
   a u s" WORD" STR:CONTAINS? TFALSE ;

\ Port 1 refuses the connection.
: CLIENT-SECRET ( -- )
   s" a failed exchange logs the URL's host, not its userinfo or query" T-LABEL
   s" http://user:SECRET@127.0.0.1:1/mcp?api_key=KEY" FAILED-ON SECRET-CUT
   s" an uppercase scheme's userinfo is cut too" T-LABEL
   s" HTTP://user:SECRET@127.0.0.1:1/mcp?api_key=KEY" FAILED-ON SECRET-CUT
   s" a password holding /, ? or # leaves the line without a host" T-LABEL
   s" http://user:PASS/WORD@127.0.0.1:1/mcp" FAILED-ON HOST-HIDDEN
   s" http://user:PASS?WORD@127.0.0.1:1/mcp" FAILED-ON HOST-HIDDEN
   s" http://user:PASS#WORD@127.0.0.1:1/mcp" FAILED-ON HOST-HIDDEN ;

\ http://u:LONGPW@127.0.0.1:1/ padded to PAST-CURL-INPUT bytes; the caller
\ releases it with MEM:FREE-SPAN.
: LONG-URL ( -- SPAN:span<u8> )
   PAST-CURL-INPUT MEM:BYTES-ALLOC-LEN MEM:ALLOC-SPAN {: url :}
   [char] a url SPAN:FILL
   s" http://u:LONGPW@127.0.0.1:1/" url SPAN:COPY
   url ;

: CLIENT-LONG ( -- )
   s" a URL libcurl refuses to set logs curl 43 with its host, not its userinfo"
   T-LABEL
   LONG-URL {: url :}
   url SPAN:$ FAILED-ON {: a:^ u:n :}
   a u s"  error mcp: exchange with 127.0.0.1:1 failed: curl 43: "
   STR:CONTAINS? TTRUE
   a u s" LONGPW" STR:CONTAINS? TFALSE
   url MEM:FREE-SPAN ;

: CLIENT-PAGES ( -- )
   s" TOOLS follows nextCursor across pages" T-LABEL
   s" /paged" URL MCP:OPEN {: paged:MCP:client :}
   paged 0 >MS MCP:TOOLS S\" [{\"name\":\"a\"},{\"name\":\"b\"}]" T$=
   paged MCP:CLOSE ;

: CLIENT-NAMES ( -- )
   s" Mcp-Name is plain for visible ASCII, else base64" T-LABEL
   s" /names" URL MCP:OPEN {: named:MCP:client :}
   named s" echo" s" {}" 0 >MS MCP:CALL TFALSE S\" {\"name\":\"echo\"}" T$=
   named s" say hi" s" {}" 0 >MS MCP:CALL TFALSE
   S\" {\"name\":\"=?base64?c2F5IGhp?=\"}" T$=
   named MCP:CLOSE ;

: CLIENT-EVENTS ( -- )
   s" /events" URL MCP:OPEN ROUTE-CLIENT !
   s" TOOLS takes the response past a notification event" T-LABEL
   ROUTE-CLIENT @ 0 >MS MCP:TOOLS S\" [{\"name\":\"s\"}]" T$=
   s" CALL takes the response past a notification and a request event" T-LABEL
   ROUTE-CLIENT @ s" echo" s" {}" 0 >MS MCP:CALL TFALSE
   S\" {\"content\":[],\"isError\":false}" T$=
   s" CALL takes the response past events that are not JSON-RPC" T-LABEL
   ROUTE-CLIENT @ s" stray" s" {}" 0 >MS MCP:CALL TFALSE
   S\" {\"content\":[],\"isError\":false}" T$=
   s" a stream with no response is E-RESPONSE and the client serves the next call" T-LABEL
   [: [: ROUTE-CLIENT @ s" none" s" {}" 0 >MS MCP:CALL 2drop drop ;]
      MCP:E-RESPONSE TTHROWSQ ;] CAPTURED
   s" has no event answering the request: status 200" REPLY-LOGGED
   ROUTE-CLIENT @ s" echo" s" {}" 0 >MS MCP:CALL TFALSE 2drop
   s" an event that is not UTF-8 is E-RESPONSE and the client serves the next call" T-LABEL
   [: [: ROUTE-CLIENT @ s" bad" s" {}" 0 >MS MCP:CALL 2drop drop ;]
      MCP:E-RESPONSE TTHROWSQ ;] CAPTURED
   s" is an event stream that is not UTF-8: status 200" REPLY-LOGGED
   ROUTE-CLIENT @ s" echo" s" {}" 0 >MS MCP:CALL TFALSE 2drop
   ROUTE-CLIENT @ MCP:CLOSE ;

: CLIENT-TWICE ( -- )
   s" a reply naming isError twice is E-RESPONSE, logged with the reader's text" T-LABEL
   s" /twice" URL MCP:OPEN ROUTE-CLIENT !
   [: [: ROUTE-CLIENT @ s" echo" s" {}" 0 >MS MCP:CALL 2drop drop ;]
      MCP:E-RESPONSE TTHROWSQ ;] CAPTURED
   W-RESET s" is not JSON-RPC: 1:64: expected a member name not used before in this object, " W+
   S\" found `\"isError\"`; remove the repeated member: status 200" W+
   W$ REPLY-LOGGED
   ROUTE-CLIENT @ MCP:CLOSE ;

: CLIENT-EVENT-TWICE ( -- )
   s" an event naming isError twice is E-RESPONSE, logged with the reader's text" T-LABEL
   s" /events" URL MCP:OPEN ROUTE-CLIENT !
   [: [: ROUTE-CLIENT @ s" twice" s" {}" 0 >MS MCP:CALL 2drop drop ;]
      MCP:E-RESPONSE TTHROWSQ ;] CAPTURED
   W-RESET s" is not JSON-RPC: 1:64: expected a member name not used before in this object, " W+
   S\" found `\"isError\"`; remove the repeated member: status 200" W+
   W$ REPLY-LOGGED
   ROUTE-CLIENT @ MCP:CLOSE ;

: CLIENT-LIST-TIMEOUT ( -- )
   s" a tools/list past its limit is E-TRANSFER and the client serves the next call" T-LABEL
   ARM
   s" /slow" URL MCP:OPEN ROUTE-CLIENT !
   [: ROUTE-CLIENT @ 300 >MS MCP:TOOLS 2drop ;] MCP:E-TRANSFER TTHROWSQ
   1 RELEASED atomic!
   ROUTE-CLIENT @ 0 >MS MCP:TOOLS s" []" T$=
   ROUTE-CLIENT @ MCP:CLOSE SETTLE ;

: CLIENT-LIST-PAGES-TIMEOUT ( -- )
   s" pages each inside its limit but past it together are E-TRANSFER" T-LABEL
   DEADLINE PAGES-END !
   s" /endless" URL MCP:OPEN ROUTE-CLIENT !
   mono-ns {: start:n :}
   [: ROUTE-CLIENT @ LIST-MS >MS MCP:TOOLS 2drop ;] MCP:E-TRANSFER TTHROWSQ
   mono-ns start - LIST-BOUND-MS NS-PER-MS * < TTRUE
   s" the client then serves a TOOLS to the last page" T-LABEL
   0 PAGES-END !
   ROUTE-CLIENT @ 0 >MS MCP:TOOLS s" []" T$=
   ROUTE-CLIENT @ MCP:CLOSE ;

: CLIENT-LIST-LATE-PAGE ( -- )
   s" a page after a slow one gets only what is left of TOOLS's limit" T-LABEL
   ARM
   s" /late" URL MCP:OPEN ROUTE-CLIENT !
   mono-ns {: start:n :}
   [: ROUTE-CLIENT @ LATE-MS >MS MCP:TOOLS 2drop ;] MCP:E-TRANSFER TTHROWSQ
   mono-ns start - LATE-BOUND-MS NS-PER-MS * < TTRUE
   1 RELEASED atomic!
   s" the client then serves a TOOLS to the last page" T-LABEL
   ROUTE-CLIENT @ 0 >MS MCP:TOOLS s" []" T$=
   ROUTE-CLIENT @ MCP:CLOSE SETTLE ;


: STALL-CALL ( -- ) STALL-CLIENT @ s" stall" s" {}" 0 >MS MCP:CALL 2drop drop ;
: STALL-CLOSE ( -- ) STALL-CLIENT @ MCP:CLOSE 1 CLOSED atomic! ;
: STALL-WORK ( -- ) [: STALL-CALL ;] [: STALL-CLOSE ;] finally ;

\ The task halted inside CALL closes its client in its own cleanup.
: CLIENT-HALTED ( -- )
   s" CLOSE of a call halted mid-receive" T-LABEL
   ARM 0 CLOSED atomic!
   s" /mcp" URL MCP:OPEN STALL-CLIENT !
   ['] STALL-WORK STALLER TASK:ACTIVATE
   ENTERED 1 STALLER TEST-WAIT:FOR-CELL
   STALLER TASK:HALT
   STALLER TASK:JOIN MATCH result ok OF ENDOF err OF ENDOF ;MATCH E-TASK-NO-RESULT T=
   1 RELEASED atomic! SETTLE
   CLOSED atomic@ 1 T=
   s" CLOSE refuses a closed client" T-LABEL
   [: STALL-CLIENT @ MCP:CLOSE ;] MCP:E-STATE TTHROWSQ
   s" a closed client is refused once OPEN reuses its row" T-LABEL
   s" /mcp" URL MCP:OPEN CLIENT !
   [: STALL-CLIENT @ MCP:CLOSE ;] MCP:E-STATE TTHROWSQ
   CLIENT @ MCP:CLOSE ;

: CANCEL-STORE ( [ -- ] -- ) catch CANCEL-CODE atomic! ;
: CANCEL-CALL ( -- ) [: STALL-CALL ;] CANCEL-STORE ;
: CANCEL-TOOLS ( -- ) [: ROUTE-CLIENT @ 0 >MS MCP:TOOLS 2drop ;] CANCEL-STORE ;

\ Runs the call on STALLER and stops the client once the server has its
\ request: the call ends E-TRANSFER while the server still holds it.
: CANCELLED ( [ -- ] MCP:client [ MCP:client -- ] -- )
   {: work who:MCP:client stop :}
   ARM RUNNING CANCEL-CODE atomic!
   work STALLER TASK:ACTIVATE
   ENTERED 1 STALLER TEST-WAIT:FOR-CELL
   who stop execute
   [: CANCEL-CODE atomic@ RUNNING <> ;] TEST-WAIT:POLL
   CANCEL-CODE atomic@ MCP:E-TRANSFER T=
   STALLER TASK:JOIN MATCH result ok OF drop ENDOF err OF drop ENDOF ;MATCH
   1 RELEASED atomic! SETTLE ;


\ The one HTTP worker stays in /late's second page until RELEASED:
\ REQUEST-STOP ends the client's side alone.
: CLIENT-STOP-PAGE ( -- )
   s" REQUEST-STOP during TOOLS's second page ends it with E-TRANSFER"
   T-LABEL
   s" /late" URL MCP:OPEN ROUTE-CLIENT !
   ['] CANCEL-TOOLS ROUTE-CLIENT @ ['] MCP:REQUEST-STOP CANCELLED
   ROUTE-CLIENT @ MCP:CLOSE ;

: STOP-CALL ( -- ) STALL-CLIENT @ s" echo" s" {}" 0 >MS MCP:CALL 2drop drop ;
: STOP-TOOLS ( -- ) STALL-CLIENT @ 0 >MS MCP:TOOLS 2drop ;

\ A stopped client's CALL and TOOLS throw at once: /counted has had no
\ request past the hits it had.
: STOP-REFUSED ( n -- )
   {: hits:n :}
   ['] STOP-CALL MCP:E-TRANSFER TTHROWSQ
   ['] STOP-TOOLS MCP:E-TRANSFER TTHROWSQ
   HITS atomic@ hits T= ;

\ Every row is free, so the second OPEN gives the stopped client's row.
: CLIENT-STOP ( -- )
   s" REQUEST-STOP of an idle client: CALL and TOOLS throw, sending nothing"
   T-LABEL
   0 HITS atomic!
   s" /counted" URL MCP:OPEN STALL-CLIENT !
   STALL-CLIENT @ MCP:REQUEST-STOP
   0 STOP-REFUSED
   STALL-CLIENT @ MCP:CLOSE
   s" a client OPEN gives the stopped one's row calls normally" T-LABEL
   s" /counted" URL MCP:OPEN STALL-CLIENT !
   STALL-CLIENT @ s" echo" s" {}" 0 >MS MCP:CALL TFALSE 2drop
   HITS atomic@ 1 T=
   s" REQUEST-STOP during a CALL ends it with E-TRANSFER" T-LABEL
   [: ['] CANCEL-CALL STALL-CLIENT @ ['] MCP:REQUEST-STOP CANCELLED ;] CAPTURED
   s" the stopped CALL logs nothing but the server's info line" T-LABEL
   LINES 0 T=
   s" then CALL and TOOLS throw, sending nothing" T-LABEL
   2 STOP-REFUSED
   STALL-CLIENT @ MCP:CLOSE
   s" REQUEST-STOP refuses a closed client" T-LABEL
   [: STALL-CLIENT @ MCP:REQUEST-STOP ;] MCP:E-STATE TTHROWSQ ;

TYPED-VARIABLE NEVER MCP:client

\ Every row is free, so a zero client must not match a free row's 0.
: CLIENT-NEVER ( -- )
   s" CLOSE refuses a client OPEN never gave" T-LABEL
   [: NEVER @ MCP:CLOSE ;] MCP:E-STATE TTHROWSQ ;


: CLIENT-CASES ( -- )
   CURL:LOOP-START LOG-INIT SERVER-LOGGED
   s" /mcp" URL MCP:OPEN CLIENT !
   CLIENT-LIST CLIENT-RESULT CLIENT-BLOCKS CLIENT-FAULT CLIENT-UNKNOWN
   CLIENT-BIG CLIENT-TIMEOUT CLIENT-SECRET CLIENT-LONG CLIENT-PAGES
   CLIENT-NAMES CLIENT-EVENTS CLIENT-TWICE CLIENT-EVENT-TWICE CLIENT-LIST-TIMEOUT CLIENT-LIST-PAGES-TIMEOUT
   CLIENT-LIST-LATE-PAGE
   CLIENT @ MCP:CLOSE
   CLIENT-HALTED CLIENT-STOP-PAGE CLIENT-STOP CLIENT-NEVER
   s" no transfer is left in flight" T-LABEL
   [: CURL:LOOP-STOP ;] catch 0 T= ;


2 constant TWO-WORKERS
600 constant LIFE-S                 \ how far past now a token's exp lies
variable PORT                       \ the open phase's port, which /keys's URL names
$40 constant URL-CAP
URL-CAP BUFFER: KEYS-URL
variable KEYS-URL-U

$1000 constant TOKEN-CAP
TOKEN-CAP SPAN-BUFFER: TOKEN
variable TOKEN-U
$800 constant KEY-SET-CAP
KEY-SET-CAP SPAN-BUFFER: KEY-SET
variable KEY-SET-U
CRYPTO:RS256-MAX-BYTES BUFFER: SIGNATURE

\ The bytes copied into the span at off; answers the offset past them.
: PUT ( ^u8 n SPAN:span<u8> n -- n )
   {: a:^ u:n row off:n :}
   a row off u SPAN:SUB SPAN:$ drop u BYTE-COPY
   off u + ;

: KEY-SET+ ( str -- ) KEY-SET KEY-SET-U @ PUT KEY-SET-U ! ;
: KEY-SET$ ( -- str ) KEY-SET 0 KEY-SET-U @ SPAN:SUB SPAN:$ ;

: KEY-SET-FILL ( -- )
   0 KEY-SET-U !
   S\" {\"keys\":[{\"kty\":\"RSA\",\"kid\":\"a\",\"use\":\"sig\",\"alg\":\"RS256\",\"n\":\"" KEY-SET+
   JWS-FIXTURE:MODULUS$ KEY-SET+
   S\" \",\"e\":\"" KEY-SET+ JWS-FIXTURE:EXPONENT$ KEY-SET+
   S\" \"}]}" KEY-SET+ ;

: TOKEN+ ( str -- ) TOKEN TOKEN-U @ PUT TOKEN-U ! ;
: TOKEN$ ( -- ^u8 n ) TOKEN 0 TOKEN-U @ SPAN:SUB SPAN:$ ;

: ENCODED+ ( str -- )
   TOKEN TOKEN-U @ TOKEN-CAP TOKEN-U @ - SPAN:SUB BASE64:ENCODE-URL
   TOKEN-U @ + TOKEN-U ! ;

\ A current token in TOKEN for the caller's azp and oid.
: MINT ( ^u8 n ^u8 n -- )
   {: azp:^ au:n oid:^ ou:n :}
   SB-RESET S\" {\"iss\":\"https://issuer.test\",\"aud\":\"api://mcp-test\",\"exp\":" SB-APPEND
   epoch-seconds LIFE-S + FMT:SB-INT
   S\" ,\"azp\":\"" SB-APPEND azp au SB-APPEND
   S\" \",\"oid\":\"" SB-APPEND oid ou SB-APPEND S\" \"}" SB-APPEND
   0 TOKEN-U !
   S\" {\"alg\":\"RS256\",\"kid\":\"a\"}" ENCODED+ s" ." TOKEN+ SB$ ENCODED+
   JWS-FIXTURE:PEM$ TOKEN$ SIGNATURE CRYPTO:RS256-MAX-BYTES CRYPTO:RS256-SIGN
   {: su:n :}
   s" ." TOKEN+ SIGNATURE su ENCODED+ ;

: ZEN ( -- ) s" zen-app" s" o-1" MINT ;
: OWNER ( -- ) s" owner-app" s" o-2" MINT ;

: BEARING ( CURL:handle -- CURL:handle )
   SB-RESET s" Authorization: Bearer " SB-APPEND TOKEN$ SB-APPEND SB$ ADD ;


: GUARDED-ROUTE ( HTTP:request HTTP:response -- )
   {: asked:HTTP:request answer:HTTP:response :}
   asked answer BEARER:ADMIT? if asked answer MCP:HANDLE then ;

: DENIED-ROUTE ( HTTP:request HTTP:response -- ) nip 403 HTTP:STATUS! ;
: BUSY-ROUTE ( HTTP:request HTTP:response -- ) nip 503 HTTP:STATUS! ;
: LIMITED-ROUTE ( HTTP:request HTTP:response -- ) nip 429 HTTP:STATUS! ;
: TEAPOT-ROUTE ( HTTP:request HTTP:response -- )
   nip {: answer:HTTP:response :}
   answer 418 HTTP:STATUS!  answer s" short and stout" HTTP:BODY! ;
: KEYS-ROUTE ( HTTP:request HTTP:response -- ) nip KEY-SET$ HTTP:BODY! ;

\ Hides echo, the first tool listed, and secret from every caller but owner-app.
: OWNER-ONLY ( ^u8 n -- bool )
   {: a:^ u:n :}
   a u s" echo" STR:= a u s" secret" STR:= or 0= if true exit then
   s" azp" BEARER:CLAIM$ 0= if 2drop false exit then
   s" owner-app" STR:= ;

: ADD-SECRET ( -- )
   S\" {\"name\":\"secret\",\"description\":\"Answer the owner.\",\"inputSchema\":{\"type\":\"object\"},\"outputSchema\":{},\"annotations\":{\"readOnlyHint\":true}}"
   [: 2drop s" {}" MCP:RESULT ;] MCP:ADD-TOOL ;

\ A claim the caller lacks shows as an empty string.
: CLAIM+ ( str -- ) BEARER:CLAIM$ drop NB+ ;

: ADD-WHOAMI ( -- )
   S\" {\"name\":\"whoami\",\"description\":\"Answer the caller's claims.\",\"inputSchema\":{\"type\":\"object\"},\"outputSchema\":{},\"annotations\":{\"readOnlyHint\":true}}"
   [: 2drop 0 NB-U !
      S\" {\"azp\":\"" NB+ s" azp" CLAIM+ S\" \",\"oid\":\"" NB+ s" oid" CLAIM+ S\" \"}" NB+
      NB NB-U @ MCP:RESULT ;] MCP:ADD-TOOL ;


: SECRET-ENTRY ( -- )
   S\" ,{\"name\":\"secret\",\"description\":\"Answer the owner.\"," W+
   S\" \"inputSchema\":{\"type\":\"object\"}," W+ s" {}" OUTPUT s" true}}" W+ ;

: WHOAMI-ENTRY ( -- )
   S\" ,{\"name\":\"whoami\",\"description\":\"Answer the caller's claims.\"," W+
   S\" \"inputSchema\":{\"type\":\"object\"}," W+ s" {}" OUTPUT s" true}}" W+ ;

: LIST-HEAD ( -- ) W-RESET S\" {\"jsonrpc\":\"2.0\",\"id\":10,\"result\":{\"tools\":[" W+ ;
: LIST-START ( -- ) LIST-HEAD ECHO-ITEM s" ," W+ LATER-ITEMS ;
: LISTED ( -- n ) LEGACY BEARING S\" {\"jsonrpc\":\"2.0\",\"id\":10,\"method\":\"tools/list\"}" POST ;

: HIDDEN-LIST ( -- )
   s" tools/list leaves out the tools the hook refuses the caller, the first listed among them" T-LABEL
   ZEN LISTED LIST-HEAD LATER-ITEMS WHOAMI-ENTRY s" ]}}" W+
   200 W$ ANSWERED ;

: HIDDEN-CALL ( -- )
   s" tools/call of a hidden tool answers as an unknown one" T-LABEL
   ZEN LEGACY BEARING
   S\" {\"jsonrpc\":\"2.0\",\"id\":6,\"method\":\"tools/call\",\"params\":{\"name\":\"secret\"}}" POST
   400 s" secret" UNKNOWN-REPLY ANSWERED ;

: OWNER-LIST ( -- )
   s" tools/list shows the tool to the caller the hook admits" T-LABEL
   OWNER LISTED LIST-START SECRET-ENTRY WHOAMI-ENTRY s" ]}}" W+
   200 W$ ANSWERED ;

: OWNER-CALL ( -- )
   s" tools/call runs the tool for the caller the hook admits" T-LABEL
   OWNER LEGACY BEARING
   S\" {\"jsonrpc\":\"2.0\",\"id\":23,\"method\":\"tools/call\",\"params\":{\"name\":\"secret\"}}" POST
   SB-RESET S\" {\"jsonrpc\":\"2.0\",\"id\":23,\"result\":{\"content\":[]," SB-APPEND
   S\" \"structuredContent\":{\"data\":{}},\"isError\":false}}" SB-APPEND
   200 SB$ ANSWERED ;


: WHOAMI-CALL ( -- ) CLIENT @ s" whoami" s" {}" 0 >MS MCP:CALL 2drop drop ;

: CAPTURED-CALL ( -- ) ['] WHOAMI-CALL CAPTURED ;

\ A call on a client of the URL throws the code and logs one line, the rest
\ after the host.
: URL-THROWS ( str n str -- )
   {: a:^ u:n code:n rest:^ ru:n :}
   a u MCP:OPEN CLIENT !
   ['] CAPTURED-CALL code TTHROWSQ
   CLIENT @ MCP:CLOSE
   rest ru REPLY-LOGGED ;

: PATH-THROWS ( str n str -- )
   {: a:^ u:n code:n rest:^ ru:n :}
   a u URL code rest ru URL-THROWS ;

: GUARDED-CLIENT ( -- )
   CURL:LOOP-START
   s" /mcp" URL MCP:OPEN CLIENT !
   s" a client without a token is E-DENIED" T-LABEL
   ['] CAPTURED-CALL MCP:E-DENIED TTHROWSQ
   s" refused the client: status 401" REPLY-LOGGED
   s" TOKEN! sends the token on TOOLS and CALL" T-LABEL
   ZEN CLIENT @ TOKEN$ MCP:TOKEN!
   OWNER
   CLIENT @ 0 >MS MCP:TOOLS W-RESET s" [" W+ LATER-ITEMS WHOAMI-ENTRY s" ]" W+ W$ T$=
   s" whoami" s" {}" ASK TFALSE
   W-RESET S\" {\"resultType\":\"complete\",\"content\":[]," W+
   S\" \"structuredContent\":{\"data\":{\"azp\":\"zen-app\",\"oid\":\"o-1\"}},\"isError\":false," W+
   W-SERVER s" }" W+ W$ T$=
   s" TOKEN! of an empty text sends none" T-LABEL
   CLIENT @ s" " MCP:TOKEN!
   ['] CAPTURED-CALL MCP:E-DENIED TTHROWSQ
   CLIENT @ MCP:CLOSE
   s" a 403 is E-DENIED" T-LABEL
   s" /denied" MCP:E-DENIED s" refused the client: status 403" PATH-THROWS
   s" a 503 is E-UNAVAILABLE" T-LABEL
   s" /busy" MCP:E-UNAVAILABLE s" reports the server unavailable: status 503" PATH-THROWS
   s" a 429 is E-UNAVAILABLE" T-LABEL
   s" /limited" MCP:E-UNAVAILABLE s" reports the server unavailable: status 429" PATH-THROWS
   s" a 418 is E-RESPONSE, logged with its body" T-LABEL
   s" /teapot" MCP:E-RESPONSE s" is not a success: status 418 body: short and stout" PATH-THROWS
   s" the log line leaves out the URL's userinfo" T-LABEL
   SB-RESET s" http://user:SECRET@127.0.0.1:" SB-APPEND HTTP:PORT FMT:SB-U
   s" /denied" SB-APPEND SB$ MCP:E-DENIED s" refused the client" URL-THROWS
   LOG$ s" user" STR:CONTAINS? TFALSE
   LOG$ s" SECRET" STR:CONTAINS? TFALSE
   s" TOKEN! refuses a closed client" T-LABEL
   [: CLIENT @ s" t" MCP:TOKEN! ;] MCP:E-STATE TTHROWSQ
   s" no transfer is left in flight" T-LABEL
   [: CURL:LOOP-STOP ;] catch 0 T= ;


\ Keeps the open phase's port and names /keys on it, while HTTP serves.
: KEEP-PORT ( -- )
   HTTP:PORT PORT !
   s" /keys" URL {: a:^ u:n :}
   a KEYS-URL u BYTE-COPY u KEYS-URL-U ! ;

\ ISSUER! and VISIBLE! are refused while HTTP serves, so the server starts again
\ on the port /keys's URL names, with two workers: ADMIT? fetches /keys from
\ this same server and waits for the answer, which a sole worker could not give.
: PROTECTED-CASES ( -- )
   KEEP-PORT
   HTTP:STOP
   HTTP:ROUTES-RESET
   s" GET" s" /keys" [: KEYS-ROUTE ;] HTTP:ROUTE
   s" POST" s" /mcp" [: GUARDED-ROUTE ;] HTTP:ROUTE
   s" POST" s" /denied" [: DENIED-ROUTE ;] HTTP:ROUTE
   s" POST" s" /busy" [: BUSY-ROUTE ;] HTTP:ROUTE
   s" POST" s" /limited" [: LIMITED-ROUTE ;] HTTP:ROUTE
   s" POST" s" /teapot" [: TEAPOT-ROUTE ;] HTTP:ROUTE
   KEY-SET-FILL
   s" https://issuer.test" s" api://mcp-test" KEYS-URL KEYS-URL-U @ BEARER:ISSUER!
   ['] OWNER-ONLY MCP:VISIBLE!
   ADD-SECRET ADD-WHOAMI
   PORT @ TWO-WORKERS SERVE-ON
   s" VISIBLE! while HTTP serves throws E-STATE" T-LABEL
   [: ['] OWNER-ONLY MCP:VISIBLE! ;] MCP:E-STATE TTHROWSQ
   HIDDEN-LIST HIDDEN-CALL OWNER-LIST OWNER-CALL GUARDED-CLIENT ;


TYPED-VARIABLE REPLIES ^VEC:vector<u8>   \ SERVE-CASES makes and deletes it

\ MCP:ANSWER of the body's message into REPLIES holding a mark, so the reply
\ shows the mark when ANSWER does not clear it.
: ANSWER-ROUTE ( HTTP:request HTTP:response -- )
   {: asked:HTTP:request answer:HTTP:response :}
   REPLIES @ {: v:^ :}
   v 0 VEC:RESIZE
   62 v VEC:PUSH c!
   asked HTTP:BODY$ v MCP:ANSWER
   answer JSON-HEADER
   answer v VEC:SPAN SPAN:$ HTTP:BODY! ;

: ANSWER-OPEN ( -- CURL:handle )
   s" /answer" s" Content-Type: application/json" s" Accept: application/json"
   OPEN-PATH ;

: ANSWER-CALL ( -- )
   s" ANSWER of a tools/call writes the text HANDLE sends for it" T-LABEL
   MODERN s" Mcp-Method: tools/call" ADD s" Mcp-Name: echo" ADD
   ECHO-A1 POST 200 T=
   W-RESET BODY$ W+
   ANSWER-OPEN ECHO-A1 POST
   200 W$ ANSWERED ;

\ A modern call of strict with a filters member, in SB.
: STRICT-MODERN ( -- ^u8 n )
   SB-RESET S\" {\"jsonrpc\":\"2.0\",\"id\":73,\"method\":\"tools/call\",\"params\":" SB-APPEND
   S\" {\"name\":\"strict\"" SB-APPEND FILTERS SB-APPEND s" ," SB-APPEND
   META S\" }}" SB-APPEND SB$ ;

: ANSWER-STRICT ( -- )
   s" ANSWER of a call whose arguments break the inputSchema writes HANDLE's tool error" T-LABEL
   0 STRICT-RAN atomic!
   MODERN s" Mcp-Method: tools/call" ADD s" Mcp-Name: strict" ADD
   STRICT-MODERN POST
   SB-RESET S\" {\"jsonrpc\":\"2.0\",\"id\":73,\"result\":{\"resultType\":\"complete\"," SB-APPEND
   S\" \"content\":[{\"type\":\"text\",\"text\":\"" SB-APPEND FILTERS-TEXT SB-APPEND
   S\" \"}],\"isError\":true," SB-APPEND SERVER S\" }}" SB-APPEND
   W-RESET SB$ W+
   200 W$ ANSWERED
   ANSWER-OPEN STRICT-MODERN POST
   200 W$ ANSWERED
   STRICT-RAN atomic@ 0 T= ;

: ANSWER-NOTIFIED ( -- )
   s" ANSWER of a notification empties the vector's old text" T-LABEL
   ANSWER-OPEN S\" {\"jsonrpc\":\"2.0\",\"method\":\"notifications/initialized\"}" POST
   200 s" " ANSWERED ;

: ANSWER-ENVELOPE ( -- )
   s" ANSWER of a message that is not JSON-RPC writes -32600" T-LABEL
   ANSWER-OPEN S\" {\"jsonrpc\":\"1.0\",\"id\":9,\"method\":\"ping\"}" POST
   200 ENVELOPE-REPLY ANSWERED
   s" ANSWER of a message that is not JSON writes -32700" T-LABEL
   ANSWER-OPEN S\" {\"jsonrpc\":" POST
   200 S\" {\"jsonrpc\":\"2.0\",\"error\":{\"code\":-32700,\"message\":\"1:12: expected a value, found the end of the input; write the rest of the document, closing each open array with `]` and object with `}`\"}}"
   ANSWERED ;

: ANSWER-UNKNOWN ( -- )
   s" ANSWER of a call of an unregistered tool writes -32602" T-LABEL
   ANSWER-OPEN
   S\" {\"jsonrpc\":\"2.0\",\"id\":6,\"method\":\"tools/call\",\"params\":{\"name\":\"none\"}}" POST
   200 s" none" UNKNOWN-REPLY ANSWERED ;

: ANSWER-OFF-WORKER ( -- )
   s" ANSWER off a request worker throws E-STATE" T-LABEL
   [: s" {}" REPLIES @ MCP:ANSWER ;] MCP:E-STATE TTHROWSQ ;

: ANSWER-CASES ( -- )
   ANSWER-CALL ANSWER-STRICT ANSWER-NOTIFIED ANSWER-ENVELOPE ANSWER-UNKNOWN ANSWER-OFF-WORKER ;


\ The wait tool's held calls, by their rows.
MCP:MAX-HELD TYPED-BUFFER HELD-CALLS MCP:held
variable WOKEN                      \ the held calls a submitted call has finished
variable HOLD-FALSE                 \ set when the wait tool's HOLD answered false

: WAIT-RESULT ( -- ) S\" {\"done\":true}" S\" {\"m\":1}" MCP:RESULT-META ;

\ A call SUBMIT runs on a request worker, as a host's job would wake it.
: WAKE ( n -- )
   1 WOKEN atomic-add drop
   HELD-CALLS @ [: WAIT-RESULT ;] MCP:FINISH ;

: HELD-WAIT ( MCP:held bool -- )
   0= if drop 1 HOLD-FALSE atomic! WAIT-RESULT exit then
   {: h :}
   h MCP:HELD-SLOT {: idx:n :}
   h idx HELD-CALLS !
   idx [: WAKE ;] HTTP:SUBMIT ;

\ A tool error whose text is in an arena released before the body returns.
: FREED-ERROR ( -- )
   s" freed text" {: t:^ tu:n :}
   ARENA:NEW {: a:^ :}
   a tu ARENA:ALLOC {: p:^ pu:n :}
   t p tu BYTE-COPY
   p pu MCP:RESULT-ERROR
   a ARENA:RELEASE ;

\ "freed" gives FREED-ERROR, a "late" key (quoted: "later" holds late)
\ throws after its result, "now" answers at once, "fail" throws after HOLD,
\ anything else HOLDs.
: WAIT-BODY ( str -- )
   {: args:^ au:n :}
   args au s" freed" STR:CONTAINS? if FREED-ERROR exit then
   args au S\" \"late\"" STR:CONTAINS? if WAIT-RESULT MCP:E-STATE throw then
   args au s" now" STR:CONTAINS? if WAIT-RESULT exit then
   args au s" fail" STR:CONTAINS? if MCP:HOLD 2drop MCP:E-STATE throw then
   MCP:HOLD HELD-WAIT ;

: ADD-WAIT ( -- )
   S\" {\"name\":\"wait\",\"description\":\"Answer once woken.\",\"inputSchema\":{\"type\":\"object\"},\"outputSchema\":{},\"annotations\":{\"readOnlyHint\":true}}"
   [: WAIT-BODY ;] MCP:ADD-TOOL ;

: WAIT-LEGACY ( str -- str )
   {: args:^ au:n :}
   SB-RESET S\" {\"jsonrpc\":\"2.0\",\"id\":60,\"method\":\"tools/call\",\"params\":" SB-APPEND
   S\" {\"name\":\"wait\",\"arguments\":" SB-APPEND args au SB-APPEND S\" }}" SB-APPEND SB$ ;

: WAIT-MODERN ( str -- str )
   {: args:^ au:n :}
   SB-RESET S\" {\"jsonrpc\":\"2.0\",\"id\":60,\"method\":\"tools/call\",\"params\":" SB-APPEND
   S\" {\"name\":\"wait\",\"arguments\":" SB-APPEND args au SB-APPEND
   s" ," SB-APPEND META S\" }}" SB-APPEND SB$ ;

: MODERN-WAIT ( -- CURL:handle )
   MODERN s" Mcp-Method: tools/call" ADD s" Mcp-Name: wait" ADD ;

\ The at-once answer in W, a success.
: KEPT-NOW ( n -- )
   {: status:n :}
   status 200 T=
   BODY$ S\" {\"jsonrpc\":\"2.0\",\"id\":60,\"result\":{" STR:STARTS-WITH? TTRUE
   W-RESET BODY$ W+ ;

\ The held call's answer is W, given by one FINISH.
: KEPT-LATER ( n -- )
   200 W$ ANSWERED
   WOKEN atomic@ 1 T=
   0 WOKEN atomic! ;

: HELD-LEGACY ( -- )
   s" a legacy call HELD and FINISHed answers as the tool at once" T-LABEL
   LEGACY S\" {\"now\":1}" WAIT-LEGACY POST KEPT-NOW
   LEGACY S\" {\"later\":1}" WAIT-LEGACY POST KEPT-LATER ;

: HELD-MODERN ( -- )
   s" a modern call HELD and FINISHed answers as the tool at once" T-LABEL
   MODERN-WAIT S\" {\"now\":1}" WAIT-MODERN POST KEPT-NOW
   MODERN-WAIT S\" {\"later\":1}" WAIT-MODERN POST KEPT-LATER ;

\ After HELD-MODERN, whose at-once answer W holds.
: HELD-ANSWER ( -- )
   s" HOLD under ANSWER answers false and the body answers at once" T-LABEL
   0 HOLD-FALSE atomic!
   ANSWER-OPEN S\" {\"later\":1}" WAIT-LEGACY POST
   200 W$ ANSWERED
   HOLD-FALSE atomic@ 1 T=
   WOKEN atomic@ 0 T= ;

: HELD-FAULT ( -- )
   s" a body that throws after HOLD closes the connection unanswered" T-LABEL
   LEGACY S\" {\"fail\":1}" WAIT-LEGACY POST -2 T= ;

: HELD-CASES ( -- ) HELD-LEGACY HELD-MODERN HELD-ANSWER HELD-FAULT ;


: REQ-HEAD ( -- str ) s" Z info mcp: req " ;

\ Where the line holding the offset starts.
: LINE-START ( ^u8 n -- n )
   {: a:^ at:n :}
   at begin dup 0 > while
      dup 1- a + c@ LF = if exit then
      1-
   repeat ;

\ Whether the text is digits and `-` after its `r`.
: REQ-ID? ( str -- bool )
   {: a:^ u:n :}
   u 2 < if false exit then
   a c@ [char] r <> if false exit then
   u 1 ?do
      a i + c@ dup [char] 0 >= over [char] 9 <= and swap [char] - = or
      0= if false unloop exit then
   loop true ;

\ The id of the capture's `<stamp> info mcp: req <id><rest>` line, rest from
\ the space after the id through LF; empty when there is none.
: REQ-ID$ ( str -- str )
   {: rest:^ ru:n :}
   LOG$ {: a:^ u:n :}
   a u rest ru STR:FIND-SUB MATCH option
      some OF {: at:idx :} at IDX>N ENDOF
      none OF -1 ENDOF
   ;MATCH {: at:n :}
   at 0< if s" no line holds the rest" T-FAIL-AS s" " exit then
   a at LINE-START DATE:TIME-LEN 1- + {: head:n :}
   a head + at head - REQ-HEAD STR:STARTS-WITH? TTRUE
   head REQ-HEAD nip + {: from:n :}
   a from + at from - 0 max
   2dup REQ-ID? TTRUE ;

: REQ-LOGGED ( str -- ) REQ-ID$ 2drop ;

: LOGGED-ECHO ( -- )
   s" MCP:LOG logs a tools/call with its arguments and ok" T-LABEL
   [: LEGACY S\" {\"jsonrpc\":\"2.0\",\"id\":80,\"method\":\"tools/call\",\"params\":{\"name\":\"echo\",\"arguments\":{\"a\":1}}}" POST 200 T= ;]
   CAPTURED
   S\"  tools/call echo {\"a\":1} -> ok\n" REQ-LOGGED
   LOG$ S\" Z info mcp: tools/call echo\n" STR:CONTAINS? TFALSE ;

: LOGGED-STRICT ( -- )
   s" MCP:LOG logs a schema refusal as a tool error" T-LABEL
   [: LEGACY 81 FILTERS STRICT-CALL POST 200 T= ;] CAPTURED
   W-RESET S\"  tools/call strict {\"filters\":\"x\"} -> tool error: " W+
   FILTERS-TEXT W+ S\" \n" W+
   W$ REQ-LOGGED ;

: LOGGED-UNKNOWN ( -- )
   s" MCP:LOG logs an unknown tool's JSON-RPC error" T-LABEL
   [: LEGACY S\" {\"jsonrpc\":\"2.0\",\"id\":6,\"method\":\"tools/call\",\"params\":{\"name\":\"none\"}}" POST 400 T= ;]
   CAPTURED
   W-RESET s"  tools/call none -> error -32602: Unknown tool: none; " W+
   S\" if the client's list of test-server's tools names it, that list is out of date\n" W+
   W$ REQ-LOGGED ;

: LOGGED-TWICE ( -- )
   s" MCP:LOG logs params naming name twice as -32600 with no method and passes no code" T-LABEL
   [: LEGACY 84 NAME-TWICE POST 400 T= ;] CAPTURED
   W-RESET s"  - -> error -32600: " W+
   TWICE-HEAD W+ S\" \"name\"" W+ TWICE-TAIL W+ S\" \n" W+
   W$ REQ-LOGGED
   LOG$ s" failed, throw" STR:CONTAINS? TFALSE ;

: LOGGED-NOTIFIED ( -- )
   s" MCP:LOG logs a legacy notification as notified" T-LABEL
   [: LEGACY S\" {\"jsonrpc\":\"2.0\",\"method\":\"notifications/initialized\"}" POST 202 T= ;]
   CAPTURED
   S\"  notifications/initialized -> notified\n" REQ-LOGGED ;

: LOGGED-HELD ( -- )
   [: HELD-LEGACY ;] CAPTURED
   s" MCP:LOG logs a held call as held and FINISH's line under its id" T-LABEL
   S\"  tools/call wait {\"later\":1} -> held\n" REQ-ID$
   W-RESET W+
   S\"  tools/call wait -> ok\n" REQ-ID$ W$ T$= ;

\ The sole worker logs the fault after the client sees the closed connection,
\ and before it serves the next request.
: FAULT-SEEN ( -- )
   HELD-FAULT
   LEGACY S\" {\"jsonrpc\":\"2.0\",\"method\":\"notifications/initialized\"}" POST 202 T= ;

: LOGGED-FAULT ( -- )
   [: FAULT-SEEN ;] CAPTURED
   s" MCP:LOG logs a throw passed to the host as a fault" T-LABEL
   W-RESET S\"  tools/call wait {\"fail\":1} -> fault " W+
   MCP:E-STATE FMT:INT$ W+ S\" \n" W+
   W$ REQ-LOGGED ;

: LOGGED-FREED ( -- )
   s" MCP:LOG logs a tool error whose text the body freed" T-LABEL
   [: LEGACY S\" {\"freed\":1}" WAIT-LEGACY POST 200 T= ;] CAPTURED
   S\"  tools/call wait {\"freed\":1} -> tool error: freed text\n" REQ-LOGGED ;

: LOGGED-LATE ( -- )
   s" MCP:LOG logs a throw after the result as a fault" T-LABEL
   [: LEGACY S\" {\"late\":1}" WAIT-LEGACY POST 500 T= ;] CAPTURED
   W-RESET S\"  tools/call wait {\"late\":1} -> fault " W+
   MCP:E-STATE FMT:INT$ W+ S\" \n" W+
   W$ REQ-LOGGED ;

: LOGGED-ANSWER ( -- )
   s" MCP:LOG logs ANSWER's message under the id -" T-LABEL
   [: ANSWER-OPEN ECHO-A1 POST 200 T= ;] CAPTURED
   LOG$ S\" Z info mcp: req - tools/call echo {\"a\":1} -> ok\n" STR:CONTAINS? TTRUE ;

\ An echo call whose arguments, {"b":<BIG>}, pass the 4096 bytes LOG writes of a text.
: LONG-CALL ( -- str )
   W-RESET S\" {\"jsonrpc\":\"2.0\",\"id\":82,\"method\":\"tools/call\",\"params\":" W+
   S\" {\"name\":\"echo\",\"arguments\":{\"b\":" W+ BIG BIG-U W+ s" }}}" W+ W$ ;

: LOGGED-LONG ( -- )
   s" MCP:LOG cuts arguments past 4096 bytes and gives their length" T-LABEL
   [: LEGACY LONG-CALL POST 200 T= ;] CAPTURED
   \ The first 4096 bytes: `{"b":` and 4091 of BIG.
   W-RESET S\"  tools/call echo {\"b\":" W+ BIG 4091 W+
   s"  [" W+ BIG-U 6 + FMT:INT$ W+ S\"  B] -> ok\n" W+
   W$ REQ-LOGGED ;

\ The server again, with LOG and every tool visible: LOG and VISIBLE! are
\ refused while HTTP serves.
: LOGGED-CASES ( -- )
   HTTP:STOP
   HTTP:ROUTES-RESET
   s" POST" s" /mcp" [: MCP:HANDLE ;] HTTP:ROUTE
   s" POST" s" /answer" [: ANSWER-ROUTE ;] HTTP:ROUTE
   [: 2drop true ;] MCP:VISIBLE!
   MCP:LOG
   0 ONE-WORKER SERVE-ON
   LOGGED-ECHO LOGGED-STRICT LOGGED-UNKNOWN LOGGED-TWICE LOGGED-NOTIFIED LOGGED-HELD
   LOGGED-FAULT LOGGED-FREED LOGGED-LATE LOGGED-ANSWER LOGGED-LONG ;


: SERVE-CASES ( -- )
   AIO:START
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW BODY !
   HTTP:ROUTES-RESET
   s" POST" s" /mcp" [: MCP:HANDLE ;] HTTP:ROUTE
   s" POST" s" /paged" [: PAGED-ROUTE ;] HTTP:ROUTE
   s" POST" s" /names" [: NAMES-ROUTE ;] HTTP:ROUTE
   s" POST" s" /events" [: EVENTS-ROUTE ;] HTTP:ROUTE
   s" POST" s" /twice" [: TWICE-ROUTE ;] HTTP:ROUTE
   s" POST" s" /slow" [: SLOW-ROUTE ;] HTTP:ROUTE
   s" POST" s" /endless" [: ENDLESS-ROUTE ;] HTTP:ROUTE
   s" POST" s" /late" [: LATE-ROUTE ;] HTTP:ROUTE
   s" POST" s" /counted" [: COUNTED-ROUTE ;] HTTP:ROUTE
   s" POST" s" /answer" [: ANSWER-ROUTE ;] HTTP:ROUTE
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW REPLIES !
   s" test-server" s" 1.0" MCP:SERVER-INFO!
   GUIDE$ MCP:INSTRUCTIONS!
   BIG-FILL
   ADD-ECHO ADD-REFUSE ADD-RAW ADD-SILENT ADD-BIG ADD-STALL ADD-SHAPE ADD-WIDGET ADD-PICK
   ADD-FRAMED ADD-PICTURED ADD-WAIT ADD-STRICT
   s" ADD-TOOL refuses a descriptor without a name" T-LABEL
   [: S\" {\"inputSchema\":{},\"outputSchema\":{}}" ADD-DESCRIBED ;] MCP:E-SCHEMA TTHROWSQ
   s" descriptor 1:36: expected a member `name`, found `}`; add `name`" SAID
   s" ADD-TOOL refuses a descriptor whose name is not a string" T-LABEL
   [: S\" {\"name\":5,\"inputSchema\":{},\"outputSchema\":{}}" ADD-DESCRIBED ;] MCP:E-SCHEMA TTHROWSQ
   s" descriptor 1:9: expected `name` as a string, found `5`; write `name` as a JSON string" SAID
   s" ADD-TOOL refuses a descriptor without an inputSchema" T-LABEL
   [: S\" {\"name\":\"x\",\"outputSchema\":{}}" ADD-DESCRIBED ;] MCP:E-SCHEMA TTHROWSQ
   s" descriptor 1:30: expected a member `inputSchema`, found `}`; add `inputSchema`" SAID
   s" ADD-TOOL refuses a descriptor whose inputSchema is not an object" T-LABEL
   [: S\" {\"name\":\"x\",\"inputSchema\":[],\"outputSchema\":{}}" ADD-DESCRIBED ;] MCP:E-SCHEMA TTHROWSQ
   s" descriptor 1:27: expected `inputSchema` as a JSON object, found `[`; write `inputSchema` as an object" SAID
   s" ADD-TOOL refuses a descriptor without an outputSchema" T-LABEL
   [: S\" {\"name\":\"x\",\"inputSchema\":{}}" ADD-DESCRIBED ;] MCP:E-SCHEMA TTHROWSQ
   s" descriptor 1:29: expected a member `outputSchema`, found `}`; add `outputSchema`" SAID
   s" ADD-TOOL refuses a descriptor whose outputSchema is not an object" T-LABEL
   [: S\" {\"name\":\"x\",\"inputSchema\":{},\"outputSchema\":[]}" ADD-DESCRIBED ;] MCP:E-SCHEMA TTHROWSQ
   s" outputSchema 1:1: expected a JSON object, found `[`; write it as one object" SAID
   s" ADD-TOOL refuses a descriptor repeating a top-level key, escaped" T-LABEL
   [: S\" {\"name\":\"x\",\"n\\u0061me\":\"y\",\"inputSchema\":{},\"outputSchema\":{}}" ADD-DESCRIBED ;] MCP:E-SCHEMA TTHROWSQ
   S\" descriptor 1:13: expected each top-level member once, found `\"n\\u0061me\"`; remove the repeated member, keeping one" SAID
   s" ADD-TOOL refuses a descriptor that is not JSON" T-LABEL
   [: s" not json" ADD-DESCRIBED ;] MCP:E-SCHEMA TTHROWSQ
   s" descriptor 1:1: expected `null`, found `not`; write the word in full and in lower case, or quote the text as a string" SAID
   s" ADD-TOOL refuses a registered name written with an escape" T-LABEL
   [: S\" {\"name\":\"\\u0065cho\",\"inputSchema\":{},\"outputSchema\":{}}" ADD-DESCRIBED ;] MCP:E-DUPLICATE TTHROWSQ
   s" ADD-TOOL refuses a descriptor without a top-level name, one nested" T-LABEL
   [: S\" {\"inputSchema\":{\"name\":\"x\"},\"outputSchema\":{}}" ADD-DESCRIBED ;] MCP:E-SCHEMA TTHROWSQ
   s" descriptor 1:46: expected a member `name`, found `}`; add `name`" SAID
   s" ADD-TOOL refuses an outputSchema repeating a top-level key" T-LABEL
   [: S\" {\"name\":\"t\",\"inputSchema\":{},\"outputSchema\":{\"type\":\"object\",\"type\":\"string\"}}" ADD-DESCRIBED ;] MCP:E-SCHEMA TTHROWSQ
   S\" outputSchema 1:18: expected each top-level member once, found `\"type\"`; remove the repeated member, keeping one" SAID
   s" ADD-TOOL rejects an inputSchema JSON-SCHEMA:COMPILE does not take" T-LABEL
   [: S\" {\"name\":\"t\",\"inputSchema\":{\"type\":\"object\",\"multipleOf\":2},\"outputSchema\":{}}" ADD-DESCRIBED ;] MCP:E-SCHEMA TTHROWSQ
   SB-RESET s" inputSchema 1:18: expected one of `$schema`, `type`, `enum`, `minimum`, `maximum`, " SB-APPEND
   s" `minLength`, `maxLength`, `pattern`, `properties`, `required`, `additionalProperties`, " SB-APPEND
   s" `maxProperties`, `items`, `minItems`, `maxItems`, `uniqueItems`, `anyOf`, `oneOf`, " SB-APPEND
   s" `description`, `title`, `default`, `examples`, `deprecated`, `readOnly`, `writeOnly`, " SB-APPEND
   S\" `$comment`, `format`, found `\"multipleOf\"`; rename the member to one of those, or remove it" SB-APPEND
   SB$ SAID
   s" ADD-TOOL rejects an inputSchema that repeats a key" T-LABEL
   [: S\" {\"name\":\"t\",\"inputSchema\":{\"type\":\"object\",\"type\":\"string\"},\"outputSchema\":{}}" ADD-DESCRIBED ;] MCP:E-SCHEMA TTHROWSQ
   S\" inputSchema 1:18: expected a member name not used before in this object, found `\"type\"`; remove the repeated member" SAID
   s" ADD-TOOL rejects an inputSchema without a type" T-LABEL
   [: S\" {\"name\":\"t\",\"inputSchema\":{},\"outputSchema\":{}}" ADD-DESCRIBED ;] MCP:E-SCHEMA TTHROWSQ
   S\" inputSchema 1:2: expected a member `type`, found `}`; add `\"type\":\"object\"`" SAID
   s" ADD-TOOL rejects an inputSchema typed other than object" T-LABEL
   [: S\" {\"name\":\"t\",\"inputSchema\":{\"type\":\"array\"},\"outputSchema\":{}}" ADD-DESCRIBED ;] MCP:E-SCHEMA TTHROWSQ
   S\" inputSchema 1:9: expected `type` as `\"object\"`, found `\"array\"`; write `type` as `\"object\"`" SAID
   WIDGET-RESOURCE PAGE MCP:ADD-RESOURCE
   PLAIN ADD-PAGE
   RESOURCE-REFUSALS
   0 ONE-WORKER SERVE-ON
   s" ADD-TOOL refuses while HTTP serves" T-LABEL
   [: ADD-LATE ;] MCP:E-STATE TTHROWSQ
   s" ADD-RESOURCE refuses while HTTP serves" T-LABEL
   [: S\" {\"uri\":\"ui://late\",\"name\":\"late\",\"mimeType\":\"text/html\"}" ADD-PAGE ;]
   MCP:E-STATE TTHROWSQ
   s" INSTRUCTIONS! refuses while HTTP serves" T-LABEL
   [: GUIDE$ MCP:INSTRUCTIONS! ;] MCP:E-STATE TTHROWSQ
   LEGACY-INITIALIZE LEGACY-INITIALIZED MODERN-DISCOVER UNSUPPORTED
   METHOD-MISMATCH UNACCEPTED UNKNOWN-METHOD UNKNOWN-TOOL LONG-UNKNOWN TOOL-ERROR
   STRICT-REFUSED
   TOOL-RESULT SHAPE-RESULT SHAPE-NOT-OBJECT META-LEGACY META-MODERN META-EMPTY
   META-NOT-OBJECT META-REPEATED META-SERVER-INFO
   BLOCKS-LEGACY BLOCKS-MODERN BLOCKS-OWN-TEXT BLOCKS-EMPTY BLOCKS-REFUSALS BAD-ENVELOPE NULL-ID RESPONSE-SENT
   BAD-JSON LIST PING
   MODERN-LIST NAME-MISMATCH NAME-ABSENT META-MISMATCH NO-CAPABILITIES SCALAR-META
   NO-PARAMS MODERN-NOTIFICATION LEGACY-NOTIFIED ARGUMENTS-NOT-OBJECT PARAMS-NOT-OBJECT
   NAME-REPEATED NOT-JSON-RESULT
   SILENT RESOURCES-LIST MODERN-RESOURCES RESOURCE-READ PLAIN-READ MODERN-READ READ-MISMATCH
   UNKNOWN-RESOURCE URI-NOT-STRING TEMPLATES
   ANSWER-CASES
   HELD-CASES
   CLIENT-CASES
   PROTECTED-CASES
   LOGGED-CASES
   HTTP:STOP
   REPLIES @ VEC:DELETE
   AIO:STOP ;

T-RESET
SERVE-CASES
T-REPORT

;package
