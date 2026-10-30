\ mcp-test.f - MCP:HANDLE answered over a real loopback port: both eras'
\ handshakes and notifications, the protocol-version, _meta, Mcp-Method and
\ Mcp-Name checks, the transport check, unknown methods and tools, a body's
\ result and tool error, an object schema's answer as structuredContent and a
\ non-object answer under it, bodies that answer badly or never, malformed
\ envelopes, arguments and JSON, tools/list and ping, and ADD-TOOL's refusals.
\ MCP's client against the same server and plain routes: TOOLS across pages,
\ CALL's result, isError, refusals, a reply past the first buffer, timeouts of
\ CALL and TOOLS, TOOLS's over all its pages, the Mcp-Name encoding,
\ event-stream replies and one that is not UTF-8, TOOLS's limit shared by its
\ pages, CLOSE after a halt mid-receive, a closed client refused once its row
\ is reused, and CANCEL of a CALL, of TOOLS's second page, of an idle client
\ and of a closed one. Then /mcp guarded by BEARER:ADMIT?: VISIBLE! hiding a
\ tool by the caller's claim from tools/list and tools/call, and refused while
\ HTTP serves; a client's TOKEN!, E-DENIED for no token, an empty one and a
\ 403, and TOKEN! of a closed client.
\ Run: bin/hb --load src/lib/net/mcp-test.f

require src/lib/test.f
require src/lib/test/wait.f
require src/lib/string.f
require src/lib/fmt.f
require src/lib/aio.f
require src/lib/task.f
require src/lib/adt/result.f
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
CAP BUFFER: BODY
variable BODY-U
1000000 constant NS-PER-MS
2000 constant WAIT-MS               \ the bound on every wait in this file
50 constant PAGE-MS                 \ /endless's delay before each page
300 constant LIST-MS                \ the TOOLS limit /endless runs past
1000 constant LIST-BOUND-MS         \ when that TOOLS must have thrown by
300 constant FIRST-PAGE-MS          \ /late's delay before its first page
1500 constant STALL-MS              \ the bound on /late's stall at its second page
500 constant LATE-MS                \ the TOOLS limit /late runs past
790 constant LATE-BOUND-MS          \ when that TOOLS must have thrown by

\ Expected texts too long for SB.
CAP BUFFER: WANT
variable WANT-U
: W-RESET ( -- ) 0 WANT-U ! ;
: W+ ( ptr u8 n -- )
   {: a:ptr u:n :}
   WANT-U @ u + CAP > if s" W+ past WANT" T-FAIL-AS exit then
   a WANT WANT-U @ + u BYTE-COPY
   WANT-U @ u + WANT-U ! ;
: W$ ( -- ptr u8 n ) WANT WANT-U @ ;

\ A route's body, built in the HTTP worker.
$100 constant NB-CAP
NB-CAP BUFFER: NB
variable NB-U
: NB+ ( ptr u8 n -- )
   {: a:ptr u:n :}
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
variable SHAPE-CODE                 \ what shape's ANSWER threw, 0 for nothing
variable RELEASED
variable LEFT
variable CLOSED
variable PAGES-END                  \ when /endless answers its last page, a mono-ns
variable CANCEL-CODE                \ what the cancelled TOOLS or CALL threw, 0 for none
1 constant RUNNING                  \ CANCEL-CODE until that call ends: no throw code
TASK:MIN-STACK TASK:TASK STALLER

\ The end of a wait's WAIT-MS window, an absolute mono-ns.
: DEADLINE ( -- n ) mono-ns WAIT-MS NS-PER-MS * + ;

: ARM ( -- ) 0 ENTERED atomic! 0 RELEASED atomic! 0 LEFT atomic! ;
: SETTLE ( -- ) [: LEFT atomic@ 0<> ;] DEADLINE s" mcp-test LEFT" TEST-WAIT:POLL ;

\ A setter's refusal throws: an assertion here would take the case's T-LABEL.
: OK ( CURL:status -- )
   MATCH CURL:status
      ok OF ENDOF
      failed OF drop CURL:E-STATE throw ENDOF
   ;MATCH ;

28 constant CURLE-TIMED-OUT             \ CURLE_OPERATION_TIMEDOUT

\ The response's status, its body in BODY; -1 for a body past CAP, -2 for a
\ failed transfer. CURLE 28 is the handle's WAIT-MS timeout, met by a starved
\ or stopped process: E-PROC-TIMEOUT, a timeout and not a FAIL.
: PERFORM ( CURL:handle -- n )
   BODY CAP >LEN CURL:PERFORM
   MATCH CURL:fetch-result
      response OF {: status:CURL:http-status got:len :}
         got LEN>N BODY-U ! status CURL:HTTP-STATUS>N ENDOF
      truncated OF drop drop -1 ENDOF
      failed OF {: rc:CURL:code :}
         rc CURL:CODE>N CURLE-TIMED-OUT = if
            s" mcp-test PERFORM" TEST-WAIT:EXPIRED
         then
         -2 ENDOF
   ;MATCH ;


: ADD ( CURL:handle ptr u8 n -- CURL:handle )
   {: h:CURL:handle a:ptr u:n :}
   h a u CURL:HEADER+ OK h ;


\ A POST to /mcp with the given Content-Type and Accept headers.
: OPEN-WITH ( ptr u8 n ptr u8 n -- CURL:handle )
   {: kind:ptr ku:n accept:ptr au:n :}
   CURL:INIT
   MATCH CURL:init-result
      ready OF ENDOF
      failed OF drop -1 CURL:>HANDLE ENDOF
   ;MATCH {: h:CURL:handle :}
   SB-RESET  s" http://127.0.0.1:" SB-APPEND  HTTP:PORT FMT:SB-U  s" /mcp" SB-APPEND
   h SB$ CURL:URL! OK
   h WAIT-MS >MS CURL:TIMEOUT! OK
   h kind ku ADD accept au ADD ;

: OPEN ( -- CURL:handle )
   s" Content-Type: application/json"
   s" Accept: application/json, text/event-stream" OPEN-WITH ;

: LEGACY ( -- CURL:handle ) OPEN s" MCP-Protocol-Version: 2025-11-25" ADD ;
: MODERN ( -- CURL:handle ) OPEN s" MCP-Protocol-Version: 2026-07-28" ADD ;


\ Posts the body: answers the status, the response's body in BODY.
: POST ( CURL:handle ptr u8 n -- n )
   {: h:CURL:handle a:ptr u:n :}
   h a u CURL:BODY! OK
   h PERFORM {: status:n :}
   h CURL:CLEANUP
   status ;


\ The response's body, then its status against the wanted one.
: ANSWERED ( n n ptr u8 n -- )
   {: status:n wanted:n want:ptr wu:n :}
   BODY BODY-U @ want wu T$=
   status wanted T= ;

: META ( -- )
   S\" \"_meta\":{\"io.modelcontextprotocol/protocolVersion\":\"2026-07-28\","
   SB-APPEND S\" \"io.modelcontextprotocol/clientCapabilities\":{}}" SB-APPEND ;


\ A tool body's fault reaches the host, which answers 500.
: FAULTED ( n -- )
   {: status:n :}
   BODY BODY-U @ s" internal: the handler did not finish this request" STARTS-WITH? TTRUE
   status 500 T= ;

: SERVER ( -- )
   S\" \"_meta\":{\"io.modelcontextprotocol/serverInfo\":" SB-APPEND
   S\" {\"name\":\"test-server\",\"version\":\"1.0\"}}" SB-APPEND ;

: W-SERVER ( -- )
   S\" \"_meta\":{\"io.modelcontextprotocol/serverInfo\":" W+
   S\" {\"name\":\"test-server\",\"version\":\"1.0\"}}" W+ ;


: LEGACY-INITIALIZE ( -- )
   s" legacy initialize answers the 2025-11-25 handshake" T-LABEL
   LEGACY S\" {\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{}}" POST
   SB-RESET S\" {\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"protocolVersion\":\"2025-11-25\"," SB-APPEND
   S\" \"capabilities\":{\"tools\":{}},\"serverInfo\":{\"name\":\"test-server\",\"version\":\"1.0\"}}}"
   SB-APPEND
   200 SB$ ANSWERED ;


: LEGACY-INITIALIZED ( -- )
   s" notifications/initialized is accepted with no body" T-LABEL
   LEGACY S\" {\"jsonrpc\":\"2.0\",\"method\":\"notifications/initialized\"}" POST
   202 s" " ANSWERED ;


: MODERN-DISCOVER ( -- )
   s" modern server/discover answers versions, serverInfo and cache scope" T-LABEL
   MODERN s" Mcp-Method: server/discover" ADD
   SB-RESET S\" {\"jsonrpc\":\"2.0\",\"id\":\"d\",\"method\":\"server/discover\",\"params\":{" SB-APPEND
   META S\" }}" SB-APPEND SB$ POST
   SB-RESET S\" {\"jsonrpc\":\"2.0\",\"id\":\"d\",\"result\":{\"resultType\":\"complete\"," SB-APPEND
   S\" \"supportedVersions\":[\"2026-07-28\",\"2025-11-25\"],\"capabilities\":{\"tools\":{}}," SB-APPEND
   SERVER S\" ,\"ttlMs\":0,\"cacheScope\":\"private\"}}" SB-APPEND
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
   LEGACY S\" {\"jsonrpc\":\"2.0\",\"id\":5,\"method\":\"resources/list\"}" POST
   404 S\" {\"jsonrpc\":\"2.0\",\"id\":5,\"error\":{\"code\":-32601,\"message\":\"Method not found\"}}"
   ANSWERED ;


: UNKNOWN-ANSWER ( -- ptr u8 n )
   S\" {\"jsonrpc\":\"2.0\",\"id\":6,\"error\":{\"code\":-32602,\"message\":\"Unknown tool\"}}" ;

: UNKNOWN-TOOL ( -- )
   s" an unregistered tool is 400 -32602 Unknown tool" T-LABEL
   LEGACY
   S\" {\"jsonrpc\":\"2.0\",\"id\":6,\"method\":\"tools/call\",\"params\":{\"name\":\"none\"}}" POST
   400 UNKNOWN-ANSWER ANSWERED ;


: TOOL-ERROR ( -- )
   s" a body's ANSWER-ERROR is an isError result" T-LABEL
   LEGACY
   S\" {\"jsonrpc\":\"2.0\",\"id\":7,\"method\":\"tools/call\",\"params\":{\"name\":\"refuse\"}}" POST
   SB-RESET S\" {\"jsonrpc\":\"2.0\",\"id\":7,\"result\":{\"content\":" SB-APPEND
   S\" [{\"type\":\"text\",\"text\":\"nope\"}],\"isError\":true}}" SB-APPEND
   200 SB$ ANSWERED ;


: TOOL-RESULT ( -- )
   s" a modern call with a base64 Mcp-Name answers structuredContent.data" T-LABEL
   MODERN s" Mcp-Method: tools/call" ADD s" Mcp-Name: =?base64?ZWNobw==?=" ADD
   SB-RESET S\" {\"jsonrpc\":\"2.0\",\"id\":8,\"method\":\"tools/call\",\"params\":" SB-APPEND
   S\" {\"name\":\"echo\",\"arguments\":{\"a\":1}," SB-APPEND
   META S\" }}" SB-APPEND SB$ POST
   SB-RESET S\" {\"jsonrpc\":\"2.0\",\"id\":8,\"result\":{\"resultType\":\"complete\"," SB-APPEND
   S\" \"content\":[{\"type\":\"text\",\"text\":\"{\\\"a\\\":1}\"}]," SB-APPEND
   S\" \"structuredContent\":{\"data\":{\"a\":1}},\"isError\":false," SB-APPEND
   SERVER S\" }}" SB-APPEND
   200 SB$ ANSWERED ;


: BAD-ENVELOPE ( -- )
   s" a jsonrpc other than 2.0 is -32600 with the request's id" T-LABEL
   LEGACY S\" {\"jsonrpc\":\"1.0\",\"id\":9,\"method\":\"ping\"}" POST
   400 S\" {\"jsonrpc\":\"2.0\",\"id\":9,\"error\":{\"code\":-32600,\"message\":\"Invalid MCP request\"}}"
   ANSWERED ;


: SHAPE-RESULT ( -- )
   s" an object schema's answer is the structuredContent itself" T-LABEL
   LEGACY
   S\" {\"jsonrpc\":\"2.0\",\"id\":21,\"method\":\"tools/call\",\"params\":{\"name\":\"shape\"}}" POST
   SB-RESET S\" {\"jsonrpc\":\"2.0\",\"id\":21,\"result\":{\"content\":[{\"type\":\"text\"," SB-APPEND
   S\" \"text\":\"{\\\"sources\\\":[\\\"a\\\"]}\"}],\"structuredContent\":{\"sources\":[\"a\"]}," SB-APPEND
   S\" \"isError\":false}}" SB-APPEND
   200 SB$ ANSWERED ;


: SHAPE-NOT-OBJECT ( -- )
   s" a body under an object schema answering a non-object faults to the host" T-LABEL
   LEGACY
   SB-RESET S\" {\"jsonrpc\":\"2.0\",\"id\":22,\"method\":\"tools/call\"," SB-APPEND
   S\" \"params\":{\"name\":\"shape\",\"arguments\":{\"flat\":true}}}" SB-APPEND SB$ POST
   FAULTED
   SHAPE-CODE atomic@ MCP:E-SCHEMA T= ;


: BAD-JSON ( -- )
   s" a body that is not JSON is -32700 with no id" T-LABEL
   LEGACY S\" {\"jsonrpc\":" POST
   400 S\" {\"jsonrpc\":\"2.0\",\"error\":{\"code\":-32700,\"message\":\"Invalid JSON\"}}"
   ANSWERED ;


\ The advertised outputSchema wrapping a declared schema that is not an object's.
: OUTPUT ( ptr u8 n -- )
   {: declared:ptr du:n :}
   S\" \"outputSchema\":{\"type\":\"object\",\"properties\":{\"data\":" W+ declared du W+
   S\" },\"required\":[\"data\"]},\"annotations\":{\"readOnlyHint\":" W+ ;

\ shape's declared output schema, advertised as declared.
: SOURCES ( -- ptr u8 n )
   S\" {\"typ\\u0065\":\"object\",\"properties\":{\"sources\":{\"type\":\"array\"}}}" ;

\ silent's declared output schema: no string "type" "object", so it is wrapped.
: NULLABLE ( -- ptr u8 n )
   S\" {\"type\":[\"object\",\"null\"],\"items\":{\"type\":\"object\"}}" ;

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
   S\" ,\"annotations\":{\"readOnlyHint\":true}}" W+ ;

\ Every tool the open phase registers, as tools/list answers them, before the
\ array's end.
: TOOL-ITEMS ( -- ) s" [" W+ ECHO-ITEM s" ," W+ LATER-ITEMS ;

: TOOL-ARRAY ( -- ) TOOL-ITEMS s" ]" W+ ;


: LIST ( -- )
   s" tools/list shows each tool's schema and read-only hint" T-LABEL
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


\ A modern tools/call for echo with an Mcp-Method header and the given id.
: ECHO-CALL ( n -- CURL:handle ptr u8 n )
   {: id:n :}
   MODERN s" Mcp-Method: tools/call" ADD
   SB-RESET S\" {\"jsonrpc\":\"2.0\",\"id\":" SB-APPEND id FMT:SB-U
   S\" ,\"method\":\"tools/call\",\"params\":{\"name\":\"echo\"," SB-APPEND
   META S\" }}" SB-APPEND SB$ ;


: NAME-MISMATCH ( -- )
   s" a modern Mcp-Name naming another tool is -32020" T-LABEL
   13 ECHO-CALL {: h:CURL:handle a:ptr u:n :}
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
   400 S\" {\"jsonrpc\":\"2.0\",\"id\":18,\"error\":{\"code\":-32602,\"message\":\"Invalid MCP request\"}}"
   ANSWERED ;


: NOT-JSON-ANSWER ( -- )
   s" a body answering text that is not JSON faults to the host" T-LABEL
   LEGACY
   S\" {\"jsonrpc\":\"2.0\",\"id\":19,\"method\":\"tools/call\",\"params\":{\"name\":\"raw\"}}" POST
   FAULTED ;


: SILENT ( -- )
   s" a body that never answers faults to the host" T-LABEL
   LEGACY
   S\" {\"jsonrpc\":\"2.0\",\"id\":20,\"method\":\"tools/call\",\"params\":{\"name\":\"silent\"}}" POST
   FAULTED ;


: PING ( -- )
   s" a legacy ping answers an empty result" T-LABEL
   LEGACY S\" {\"jsonrpc\":\"2.0\",\"id\":11,\"method\":\"ping\"}" POST
   200 S\" {\"jsonrpc\":\"2.0\",\"id\":11,\"result\":{}}" ANSWERED ;


: ADD-ECHO ( -- )
   s" echo" s" Echo the arguments." S\" {\"type\":\"object\"}" s" {}" true
   [: MCP:ANSWER ;] MCP:ADD-TOOL ;

: ADD-REFUSE ( -- )
   s" refuse" s" Refuse every call." S\" {\"type\":\"object\",\"properties\":{}}" s" {}"
   false
   [: 2drop s" nope" MCP:ANSWER-ERROR ;] MCP:ADD-TOOL ;

: ADD-RAW ( -- )
   s" raw" s" Answer text that is not JSON." S\" {\"type\":\"object\"}" s" {}" true
   [: 2drop s" not json" MCP:ANSWER ;] MCP:ADD-TOOL ;

: ADD-SILENT ( -- )
   s" silent" s" Never answer." S\" {\"type\":\"object\"}" NULLABLE true
   [: 2drop ;] MCP:ADD-TOOL ;

: ADD-BIG ( -- )
   s" big" s" Answer a long string." S\" {\"type\":\"object\"}"
   S\" {\"type\":\"string\"}" true
   [: 2drop BIG BIG-U MCP:ANSWER ;] MCP:ADD-TOOL ;

\ Answers once the test sets RELEASED.
: ADD-STALL ( -- )
   s" stall" s" Answer once released." S\" {\"type\":\"object\"}" s" {}" true
   [: 2drop 1 ENTERED atomic!
      [: RELEASED atomic@ 0<> ;] DEADLINE s" mcp-test STALL" TEST-WAIT:POLL
      1 LEFT atomic!
      s" {}" MCP:ANSWER ;] MCP:ADD-TOOL ;

: ADD-LATE ( -- )
   s" late" s" Registered while serving." S\" {\"type\":\"object\"}" s" {}" true
   [: MCP:ANSWER ;] MCP:ADD-TOOL ;


\ ANSWER, keeping in SHAPE-CODE what it threw.
: SHAPE-ANSWER ( ptr u8 n -- )
   [: 2dup MCP:ANSWER ;] catch {: code:n :} 2drop
   code SHAPE-CODE atomic!
   code 0<> if code throw then ;


\ Answers {"sources":["a"]}, or an array for arguments naming "flat".
: ADD-SHAPE ( -- )
   s" shape" s" Answer sources." S\" {\"type\":\"object\"}" SOURCES true
   [: s" flat" CONTAINS? if s" []" else S\" {\"sources\":[\"a\"]}" then SHAPE-ANSWER ;]
   MCP:ADD-TOOL ;


: ADD-SCHEMA ( ptr u8 n -- )
   {: output:ptr ou:n :}
   s" listed" s" Declare a schema." S\" {\"type\":\"object\"}" output ou true
   [: MCP:ANSWER ;] MCP:ADD-TOOL ;


\ The client's cases.

: URL ( ptr u8 n -- ptr u8 n )
   {: path:ptr pu:n :}
   SB-RESET s" http://127.0.0.1:" SB-APPEND HTTP:PORT FMT:SB-U path pu SB-APPEND SB$ ;

: ASK ( ptr u8 n ptr u8 n -- ptr u8 n bool )
   {: name:ptr nu:n args:ptr au:n :}
   CLIENT @ name nu args au 0 >MS MCP:CALL ;

: JSON-HEADER ( HTTP:response -- )
   s" Content-Type" s" application/json" HTTP:HEADER! ;

\ tools/list in three pages, the middle one empty.
: PAGED-ROUTE ( HTTP:request HTTP:response -- )
   {: asked:HTTP:request answer:HTTP:response :}
   answer JSON-HEADER
   asked HTTP:BODY$ S\" \"cursor\":\"p2\"" CONTAINS? if
      answer S\" {\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"tools\":[ ],\"nextCursor\":\"p3\"}}"
      HTTP:BODY! exit
   then
   asked HTTP:BODY$ S\" \"cursor\":\"p3\"" CONTAINS? if
      answer S\" {\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"tools\":[{\"name\":\"b\"}]}}"
      HTTP:BODY! exit
   then
   answer S\" {\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"tools\":[{\"name\":\"a\"}],\"nextCursor\":\"p2\"}}"
   HTTP:BODY! ;

\ A result naming the Mcp-Name header as it arrived.
: NAMES-ROUTE ( HTTP:request HTTP:response -- )
   {: asked:HTTP:request answer:HTTP:response :}
   asked s" Mcp-Name" HTTP:HEADER-OF$ drop {: v:ptr vu:n :}
   0 NB-U !
   S\" {\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"name\":\"" NB+ v vu NB+ S\" \"}}" NB+
   answer JSON-HEADER
   answer NB NB-U @ HTTP:BODY! ;

\ Event-stream replies: a notification and, for a call, a request with the
\ client's id before the response, except for a call of "none", whose stream
\ ends with no response, and of "bad", whose one event is not UTF-8.
: EVENTS-ROUTE ( HTTP:request HTTP:response -- )
   {: asked:HTTP:request answer:HTTP:response :}
   answer s" Content-Type" s" text/event-stream" HTTP:HEADER!
   asked HTTP:BODY$ S\" \"name\":\"bad\"" CONTAINS? if
      answer s\" data: \xFF\n\n" HTTP:BODY! exit
   then
   asked HTTP:BODY$ S\" \"name\":\"none\"" CONTAINS? if
      answer S\" data: {\"jsonrpc\":\"2.0\",\"method\":\"notifications/message\"}\n\n"
      HTTP:BODY! exit
   then
   asked HTTP:BODY$ s" tools/list" CONTAINS? if
      answer S\" data: {\"jsonrpc\":\"2.0\",\"method\":\"notifications/message\"}\n\nevent: message\ndata: {\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"tools\":[{\"name\":\"s\"}]}}\n\n"
      HTTP:BODY! exit
   then
   answer S\" data: {\"jsonrpc\":\"2.0\",\"method\":\"notifications/message\"}\n\ndata: {\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"ping\"}\n\ndata: {\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"content\":[],\"isError\":false}}\n\n"
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
   [: dup mono-ns <= ;] DEADLINE s" mcp-test PAGE" TEST-WAIT:POLL drop
   answer S\" {\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"tools\":[],\"nextCursor\":\"more\"}}"
   HTTP:BODY! ;

\ A first page with a nextCursor after FIRST-PAGE-MS, then a last, empty page,
\ setting ENTERED as it is asked, once the test sets RELEASED or STALL-MS has
\ passed, so a client that held its second page to the whole limit ends.
: LATE-ROUTE ( HTTP:request HTTP:response -- )
   {: asked:HTTP:request answer:HTTP:response :}
   answer JSON-HEADER
   asked HTTP:BODY$ S\" \"cursor\":\"late\"" CONTAINS? if
      1 ENTERED atomic!
      mono-ns STALL-MS NS-PER-MS * +
      [: RELEASED atomic@ 0<> over mono-ns <= or ;] DEADLINE s" mcp-test STALL-PAGE"
      TEST-WAIT:POLL drop
      1 LEFT atomic!
      answer S\" {\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"tools\":[]}}" HTTP:BODY! exit
   then
   mono-ns FIRST-PAGE-MS NS-PER-MS * +
   [: dup mono-ns <= ;] DEADLINE s" mcp-test FIRST-PAGE" TEST-WAIT:POLL drop
   answer S\" {\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"tools\":[],\"nextCursor\":\"late\"}}"
   HTTP:BODY! ;

\ An empty tools/list answered once the test sets RELEASED.
: SLOW-ROUTE ( HTTP:request HTTP:response -- )
   nip {: answer:HTTP:response :}
   [: RELEASED atomic@ 0<> ;] DEADLINE s" mcp-test SLOW" TEST-WAIT:POLL
   1 LEFT atomic!
   answer JSON-HEADER
   answer S\" {\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"tools\":[]}}" HTTP:BODY! ;


: CLIENT-LIST ( -- )
   s" TOOLS answers every tool object in order" T-LABEL
   CLIENT @ 0 >MS MCP:TOOLS W-RESET TOOL-ARRAY W$ T$= ;

: CLIENT-RESULT ( -- )
   s" CALL answers the result object, isError false" T-LABEL
   s" echo" S\" {\"a\":1}" ASK TFALSE
   W-RESET S\" {\"resultType\":\"complete\",\"content\":[{\"type\":\"text\",\"text\":" W+
   S\" \"{\\\"a\\\":1}\"}],\"structuredContent\":{\"data\":{\"a\":1}},\"isError\":false," W+
   W-SERVER s" }" W+ W$ T$= ;

: CLIENT-FAULT ( -- )
   s" CALL answers a tool error's result, isError true" T-LABEL
   s" refuse" s" {}" ASK TTRUE
   W-RESET S\" {\"resultType\":\"complete\",\"content\":[{\"type\":\"text\",\"text\":\"nope\"}]," W+
   S\" \"isError\":true," W+ W-SERVER s" }" W+ W$ T$= ;

: CLIENT-UNKNOWN ( -- )
   s" an unknown tool is E-RESPONSE and the client serves the next call" T-LABEL
   [: s" nosuch" s" {}" ASK 2drop drop ;] MCP:E-RESPONSE TTHROWSQ
   s" echo" s" {}" ASK TFALSE 2drop ;

: CLIENT-BIG ( -- )
   s" a result past the first reply buffer arrives whole" T-LABEL
   s" big" s" {}" ASK TFALSE
   W-RESET S\" {\"resultType\":\"complete\",\"content\":[{\"type\":\"text\",\"text\":\"\\\"" W+
   BIG 1+ BIG-U 2 - W+ S\" \\\"\"}],\"structuredContent\":{\"data\":" W+
   BIG BIG-U W+ S\" },\"isError\":false," W+ W-SERVER s" }" W+ W$ T$= ;

: CLIENT-TIMEOUT ( -- )
   s" a call past its limit is E-TRANSFER and the client serves the next call" T-LABEL
   ARM
   [: CLIENT @ s" stall" s" {}" 300 >MS MCP:CALL 2drop drop ;] MCP:E-TRANSFER TTHROWSQ
   1 RELEASED atomic!
   CLIENT @ s" echo" s" {}" 300 >MS MCP:CALL TFALSE 2drop SETTLE ;

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
   s" a stream with no response is E-RESPONSE and the client serves the next call" T-LABEL
   [: ROUTE-CLIENT @ s" none" s" {}" 0 >MS MCP:CALL 2drop drop ;] MCP:E-RESPONSE TTHROWSQ
   ROUTE-CLIENT @ s" echo" s" {}" 0 >MS MCP:CALL TFALSE 2drop
   s" an event that is not UTF-8 is E-RESPONSE and the client serves the next call" T-LABEL
   [: ROUTE-CLIENT @ s" bad" s" {}" 0 >MS MCP:CALL 2drop drop ;] MCP:E-RESPONSE TTHROWSQ
   ROUTE-CLIENT @ s" echo" s" {}" 0 >MS MCP:CALL TFALSE 2drop
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
   ROUTE-CLIENT @ LATE-MS >MS MCP:TOOLS s" []" T$=
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
   [: ENTERED atomic@ 0<> ;] DEADLINE s" mcp-test ENTERED" TEST-WAIT:POLL
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

\ The task's TOOLS or CALL, its throw code kept in CANCEL-CODE.
: CANCEL-STORE ( [ -- ] -- ) catch CANCEL-CODE atomic! ;
: CANCEL-CALL ( -- ) [: STALL-CALL ;] CANCEL-STORE ;
: CANCEL-TOOLS ( -- ) [: ROUTE-CLIENT @ 0 >MS MCP:TOOLS 2drop ;] CANCEL-STORE ;

\ Runs the call on STALLER and cancels the client once the server has its
\ request: the call ends E-TRANSFER while the server still holds it.
: CANCELLED ( [ -- ] MCP:client -- )
   {: work who:MCP:client :}
   ARM RUNNING CANCEL-CODE atomic!
   work STALLER TASK:ACTIVATE
   [: ENTERED atomic@ 0<> ;] DEADLINE s" mcp-test ENTERED" TEST-WAIT:POLL
   who MCP:CANCEL
   [: CANCEL-CODE atomic@ RUNNING <> ;] DEADLINE s" mcp-test CANCEL" TEST-WAIT:POLL
   CANCEL-CODE atomic@ MCP:E-TRANSFER T=
   STALLER TASK:JOIN MATCH result ok OF drop ENDOF err OF drop ENDOF ;MATCH
   1 RELEASED atomic! SETTLE ;


\ The one HTTP worker stays inside the stall tool, or /late's second page,
\ until RELEASED: CANCEL ends the client's side alone.
: CLIENT-CANCEL ( -- )
   s" CANCEL from another task ends a CALL in flight with E-TRANSFER" T-LABEL
   s" /mcp" URL MCP:OPEN STALL-CLIENT !
   ['] CANCEL-CALL STALL-CLIENT @ CANCELLED
   s" the cancelled client then serves a CALL" T-LABEL
   STALL-CLIENT @ s" echo" s" {}" 0 >MS MCP:CALL TFALSE 2drop
   s" CANCEL of an idle client leaves its next CALL to run" T-LABEL
   STALL-CLIENT @ MCP:CANCEL
   STALL-CLIENT @ s" echo" s" {}" 0 >MS MCP:CALL TFALSE 2drop
   STALL-CLIENT @ MCP:CLOSE
   s" CANCEL refuses a closed client" T-LABEL
   [: STALL-CLIENT @ MCP:CANCEL ;] MCP:E-STATE TTHROWSQ
   s" CANCEL during TOOLS's second page ends it with E-TRANSFER" T-LABEL
   s" /late" URL MCP:OPEN ROUTE-CLIENT !
   ['] CANCEL-TOOLS ROUTE-CLIENT @ CANCELLED
   ROUTE-CLIENT @ MCP:CLOSE ;

TYPED-VARIABLE NEVER MCP:client

\ Every row is free, so a zero client must not match a free row's 0.
: CLIENT-NEVER ( -- )
   s" CLOSE refuses a client OPEN never gave" T-LABEL
   [: NEVER @ MCP:CLOSE ;] MCP:E-STATE TTHROWSQ ;


: CLIENT-CASES ( -- )
   CURL:LOOP-START OK
   s" /mcp" URL MCP:OPEN CLIENT !
   CLIENT-LIST CLIENT-RESULT CLIENT-FAULT CLIENT-UNKNOWN CLIENT-BIG CLIENT-TIMEOUT
   CLIENT-PAGES CLIENT-NAMES CLIENT-EVENTS CLIENT-LIST-TIMEOUT
   CLIENT-LIST-PAGES-TIMEOUT CLIENT-LIST-LATE-PAGE
   CLIENT @ MCP:CLOSE
   CLIENT-HALTED CLIENT-CANCEL CLIENT-NEVER
   s" no transfer is left in flight" T-LABEL
   [: CURL:LOOP-STOP ;] catch 0 T= ;


\ The protected phase: /mcp guarded by BEARER:ADMIT?, tokens signed by the
\ fixture key under kid a.

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
: PUT ( ptr u8 n SPAN:span<u8> n -- n )
   {: a:ptr u:n row off:n :}
   a row off u SPAN:SUB SPAN:$ drop u BYTE-COPY
   off u + ;

: KEY-SET+ ( ptr u8 n -- ) KEY-SET KEY-SET-U @ PUT KEY-SET-U ! ;
: KEY-SET$ ( -- ptr u8 n ) KEY-SET 0 KEY-SET-U @ SPAN:SUB SPAN:$ ;

: KEY-SET-FILL ( -- )
   0 KEY-SET-U !
   S\" {\"keys\":[{\"kty\":\"RSA\",\"kid\":\"a\",\"use\":\"sig\",\"alg\":\"RS256\",\"n\":\"" KEY-SET+
   JWS-FIXTURE:MODULUS$ KEY-SET+
   S\" \",\"e\":\"" KEY-SET+ JWS-FIXTURE:EXPONENT$ KEY-SET+
   S\" \"}]}" KEY-SET+ ;

: TOKEN+ ( ptr u8 n -- ) TOKEN TOKEN-U @ PUT TOKEN-U ! ;
: TOKEN$ ( -- ptr u8 n ) TOKEN 0 TOKEN-U @ SPAN:SUB SPAN:$ ;

: ENCODED+ ( ptr u8 n -- )
   TOKEN TOKEN-U @ TOKEN-CAP TOKEN-U @ - SPAN:SUB BASE64:ENCODE-URL
   TOKEN-U @ + TOKEN-U ! ;

\ A current token in TOKEN for the caller's azp and oid.
: MINT ( ptr u8 n ptr u8 n -- )
   {: azp:ptr au:n oid:ptr ou:n :}
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

\ The Authorization carrying the minted token.
: BEARING ( CURL:handle -- CURL:handle )
   SB-RESET s" Authorization: Bearer " SB-APPEND TOKEN$ SB-APPEND SB$ ADD ;


: GUARDED-ROUTE ( HTTP:request HTTP:response -- )
   {: asked:HTTP:request answer:HTTP:response :}
   asked answer BEARER:ADMIT? if asked answer MCP:HANDLE then ;

: DENIED-ROUTE ( HTTP:request HTTP:response -- ) nip 403 HTTP:STATUS! ;
: KEYS-ROUTE ( HTTP:request HTTP:response -- ) nip KEY-SET$ HTTP:BODY! ;

\ Hides echo, the first tool listed, and secret from every caller but owner-app.
: OWNER-ONLY ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   a u s" echo" STR= a u s" secret" STR= or 0= if true exit then
   s" azp" BEARER:CLAIM$ 0= if 2drop false exit then
   s" owner-app" STR= ;

: ADD-SECRET ( -- )
   s" secret" s" Answer the owner." S\" {\"type\":\"object\"}" s" {}" true
   [: 2drop s" {}" MCP:ANSWER ;] MCP:ADD-TOOL ;

\ A claim the caller lacks shows as an empty string.
: CLAIM+ ( ptr u8 n -- ) BEARER:CLAIM$ drop NB+ ;

: ADD-WHOAMI ( -- )
   s" whoami" s" Answer the caller's claims." S\" {\"type\":\"object\"}" s" {}" true
   [: 2drop 0 NB-U !
      S\" {\"azp\":\"" NB+ s" azp" CLAIM+ S\" \",\"oid\":\"" NB+ s" oid" CLAIM+ S\" \"}" NB+
      NB NB-U @ MCP:ANSWER ;] MCP:ADD-TOOL ;


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
   400 UNKNOWN-ANSWER ANSWERED ;

: OWNER-LIST ( -- )
   s" tools/list shows the tool to the caller the hook admits" T-LABEL
   OWNER LISTED LIST-START SECRET-ENTRY WHOAMI-ENTRY s" ]}}" W+
   200 W$ ANSWERED ;

: OWNER-CALL ( -- )
   s" tools/call runs the tool for the caller the hook admits" T-LABEL
   OWNER LEGACY BEARING
   S\" {\"jsonrpc\":\"2.0\",\"id\":23,\"method\":\"tools/call\",\"params\":{\"name\":\"secret\"}}" POST
   SB-RESET S\" {\"jsonrpc\":\"2.0\",\"id\":23,\"result\":{\"content\":[{\"type\":\"text\"," SB-APPEND
   S\" \"text\":\"{}\"}],\"structuredContent\":{\"data\":{}},\"isError\":false}}" SB-APPEND
   200 SB$ ANSWERED ;


: DENIED-CALL ( -- ) CLIENT @ s" whoami" s" {}" 0 >MS MCP:CALL 2drop drop ;

: GUARDED-CLIENT ( -- )
   CURL:LOOP-START OK
   s" /mcp" URL MCP:OPEN CLIENT !
   s" a client without a token is E-DENIED" T-LABEL
   ['] DENIED-CALL MCP:E-DENIED TTHROWSQ
   s" TOKEN! sends the token on TOOLS and CALL" T-LABEL
   ZEN CLIENT @ TOKEN$ MCP:TOKEN!
   OWNER
   CLIENT @ 0 >MS MCP:TOOLS W-RESET s" [" W+ LATER-ITEMS WHOAMI-ENTRY s" ]" W+ W$ T$=
   s" whoami" s" {}" ASK TFALSE
   W-RESET S\" {\"resultType\":\"complete\",\"content\":[{\"type\":\"text\",\"text\":" W+
   S\" \"{\\\"azp\\\":\\\"zen-app\\\",\\\"oid\\\":\\\"o-1\\\"}\"}]," W+
   S\" \"structuredContent\":{\"data\":{\"azp\":\"zen-app\",\"oid\":\"o-1\"}},\"isError\":false," W+
   W-SERVER s" }" W+ W$ T$=
   s" TOKEN! of an empty text sends none" T-LABEL
   CLIENT @ s" " MCP:TOKEN!
   ['] DENIED-CALL MCP:E-DENIED TTHROWSQ
   CLIENT @ MCP:CLOSE
   s" a 403 is E-DENIED" T-LABEL
   s" /denied" URL MCP:OPEN CLIENT !
   ['] DENIED-CALL MCP:E-DENIED TTHROWSQ
   CLIENT @ MCP:CLOSE
   s" TOKEN! refuses a closed client" T-LABEL
   [: CLIENT @ s" t" MCP:TOKEN! ;] MCP:E-STATE TTHROWSQ
   s" no transfer is left in flight" T-LABEL
   [: CURL:LOOP-STOP ;] catch 0 T= ;


\ Keeps the open phase's port and names /keys on it, while HTTP serves.
: KEEP-PORT ( -- )
   HTTP:PORT PORT !
   s" /keys" URL {: a:ptr u:n :}
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
   KEY-SET-FILL
   s" https://issuer.test" s" api://mcp-test" KEYS-URL KEYS-URL-U @ BEARER:ISSUER!
   ['] OWNER-ONLY MCP:VISIBLE!
   ADD-SECRET ADD-WHOAMI
   LOOPBACK PORT @ TWO-WORKERS IDLE-MS HTTP:START
   s" VISIBLE! while HTTP serves throws E-STATE" T-LABEL
   [: ['] OWNER-ONLY MCP:VISIBLE! ;] MCP:E-STATE TTHROWSQ
   HIDDEN-LIST HIDDEN-CALL OWNER-LIST OWNER-CALL GUARDED-CLIENT ;


: SERVE-CASES ( -- )
   AIO:START
   HTTP:ROUTES-RESET
   s" POST" s" /mcp" [: MCP:HANDLE ;] HTTP:ROUTE
   s" POST" s" /paged" [: PAGED-ROUTE ;] HTTP:ROUTE
   s" POST" s" /names" [: NAMES-ROUTE ;] HTTP:ROUTE
   s" POST" s" /events" [: EVENTS-ROUTE ;] HTTP:ROUTE
   s" POST" s" /slow" [: SLOW-ROUTE ;] HTTP:ROUTE
   s" POST" s" /endless" [: ENDLESS-ROUTE ;] HTTP:ROUTE
   s" POST" s" /late" [: LATE-ROUTE ;] HTTP:ROUTE
   s" test-server" s" 1.0" MCP:SERVER-INFO!
   BIG-FILL
   ADD-ECHO ADD-REFUSE ADD-RAW ADD-SILENT ADD-BIG ADD-STALL ADD-SHAPE
   s" ADD-TOOL refuses a registered name" T-LABEL
   [: ADD-ECHO ;] MCP:E-DUPLICATE TTHROWSQ
   s" ADD-TOOL refuses an output schema that is not an object" T-LABEL
   [: s" []" ADD-SCHEMA ;] MCP:E-SCHEMA TTHROWSQ
   s" ADD-TOOL refuses an output schema that is not JSON" T-LABEL
   [: s" not json" ADD-SCHEMA ;] MCP:E-SCHEMA TTHROWSQ
   s" ADD-TOOL refuses an empty output schema" T-LABEL
   [: s" " ADD-SCHEMA ;] MCP:E-SCHEMA TTHROWSQ
   s" ADD-TOOL refuses an output schema repeating a top-level key, escaped" T-LABEL
   [: S\" {\"type\":\"object\",\"typ\\u0065\":\"array\"}" ADD-SCHEMA ;] MCP:E-SCHEMA TTHROWSQ
   LOOPBACK 0 ONE-WORKER IDLE-MS HTTP:START
   s" ADD-TOOL refuses while HTTP serves" T-LABEL
   [: ADD-LATE ;] MCP:E-STATE TTHROWSQ
   LEGACY-INITIALIZE LEGACY-INITIALIZED MODERN-DISCOVER UNSUPPORTED
   METHOD-MISMATCH UNACCEPTED UNKNOWN-METHOD UNKNOWN-TOOL TOOL-ERROR
   TOOL-RESULT SHAPE-RESULT SHAPE-NOT-OBJECT BAD-ENVELOPE BAD-JSON LIST PING
   MODERN-LIST NAME-MISMATCH NAME-ABSENT META-MISMATCH NO-CAPABILITIES NO-PARAMS MODERN-NOTIFICATION
   LEGACY-NOTIFIED ARGUMENTS-NOT-OBJECT NOT-JSON-ANSWER SILENT
   CLIENT-CASES
   PROTECTED-CASES
   HTTP:STOP
   AIO:STOP ;

T-RESET
SERVE-CASES
T-REPORT

;package
