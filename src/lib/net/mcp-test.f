\ mcp-test.f - MCP:HANDLE answered over a real loopback port: both eras'
\ handshakes and notifications, the protocol-version, _meta, Mcp-Method and
\ Mcp-Name checks, the transport check, unknown methods and tools, a body's
\ result and tool error, an object schema's answer as structuredContent and a
\ non-object answer under it, a result's _meta from ANSWER-WITH in both eras and its
\ refusals, bodies that answer badly or never, malformed envelopes, arguments and JSON,
\ ping, tools/list showing each descriptor as registered (annotations and _meta raw) with
\ a non-object output schema's wrapper in its place, and ADD-TOOL's refusals of a bad
\ descriptor. ui:// resources: the resources capability, resources/list and
\ resources/read in both eras, a read's Mcp-Name, a read without _meta, an unknown uri
\ and one that is no string, an empty resources/templates/list, and ADD-RESOURCE's
\ refusals.
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
variable META-CODE                  \ what framed's ANSWER-WITH threw, 0 for nothing
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

: GUIDE$ ( -- ptr u8 n ) S\" Echo text back.\n\"raw\" answers as given." ;

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


\ framed's arguments, its result's _meta, with space a modern answer trims.
: SPACED ( -- ptr u8 n ) S\" { \"a\":1, \"b\":[2] }" ;


\ A legacy tools/call of framed with the given id and arguments.
: LEGACY-FRAMED ( n ptr u8 n -- n )
   {: id:n args:ptr au:n :}
   LEGACY
   SB-RESET S\" {\"jsonrpc\":\"2.0\",\"id\":" SB-APPEND id FMT:SB-U
   S\" ,\"method\":\"tools/call\",\"params\":{\"name\":\"framed\",\"arguments\":" SB-APPEND
   args au SB-APPEND S\" }}" SB-APPEND SB$ POST ;


\ A modern tools/call of framed, Mcp-Name included, with the given id and
\ arguments.
: MODERN-FRAMED ( n ptr u8 n -- n )
   {: id:n args:ptr au:n :}
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
   S\" \"content\":[{\"type\":\"text\",\"text\":\"{\\\"ok\\\":true}\"}]," SB-APPEND
   S\" \"structuredContent\":{\"ok\":true},\"isError\":false" SB-APPEND ;


: META-LEGACY ( -- )
   s" a legacy result's _meta is ANSWER-WITH's meta as given" T-LABEL
   24 SPACED LEGACY-FRAMED
   24 false FRAMED-RESULT S\" ,\"_meta\":" SB-APPEND SPACED SB-APPEND S\" }}" SB-APPEND
   200 SB$ ANSWERED ;


: META-MODERN ( -- )
   s" a modern result's _meta holds ANSWER-WITH's members, then serverInfo" T-LABEL
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
   s" ANSWER-WITH meta that is not an object faults to the host" T-LABEL
   0 META-CODE atomic!
   27 S\" {\"flat\":true}" LEGACY-FRAMED FAULTED
   META-CODE atomic@ MCP:E-SCHEMA T= ;


: META-REPEATED ( -- )
   s" ANSWER-WITH meta that repeats a top-level key faults to the host" T-LABEL
   0 META-CODE atomic!
   29 S\" {\"a\":1,\"a\":2}" LEGACY-FRAMED FAULTED
   META-CODE atomic@ MCP:E-SCHEMA T= ;


: META-SERVER-INFO ( -- )
   s" ANSWER-WITH meta naming serverInfo, escaped, faults to the host" T-LABEL
   0 META-CODE atomic!
   28 S\" {\"io.modelcontextprotocol\\/serverInfo\":{}}" LEGACY-FRAMED FAULTED
   META-CODE atomic@ MCP:E-SCHEMA T= ;


: BAD-JSON ( -- )
   s" a body that is not JSON is -32700 with no id" T-LABEL
   LEGACY S\" {\"jsonrpc\":" POST
   400 S\" {\"jsonrpc\":\"2.0\",\"error\":{\"code\":-32700,\"message\":\"Invalid JSON\"}}"
   ANSWERED ;


\ The advertised outputSchema wrapping a declared schema that is not an object's.
: WRAPPER ( ptr u8 n -- )
   {: declared:ptr du:n :}
   S\" \"outputSchema\":{\"type\":\"object\",\"properties\":{\"data\":" W+ declared du W+
   S\" },\"required\":[\"data\"]}" W+ ;

: OUTPUT ( ptr u8 n -- ) WRAPPER S\" ,\"annotations\":{\"readOnlyHint\":" W+ ;

\ shape's declared output schema, advertised as declared.
: SOURCES ( -- ptr u8 n )
   S\" {\"typ\\u0065\":\"object\",\"properties\":{\"sources\":{\"type\":\"array\"}}}" ;

\ silent's declared output schema: no string "type" "object", so it is wrapped.
: NULLABLE ( -- ptr u8 n )
   S\" {\"type\":[\"object\",\"null\"],\"items\":{\"type\":\"object\"}}" ;

\ An app's tool, listed as registered: its outputSchema is an object's.
: WIDGET ( -- ptr u8 n )
   S\" {\"name\":\"widget\",\"description\":\"Show the widget.\",\"inputSchema\":{\"type\":\"object\"},\"outputSchema\":{\"type\":\"object\"},\"annotations\":{\"readOnlyHint\":true,\"destructiveHint\":false,\"openWorldHint\":false},\"_meta\":{\"ui\":{\"resourceUri\":\"ui://test/widget.html\",\"visibility\":[\"model\",\"app\"]},\"openai/fileParams\":[\"file\"]}}" ;

\ A tool whose escaped outputSchema key and non-object schema sit between members.
: PICKER ( -- ptr u8 n )
   S\" {\"name\":\"pick\",\"title\": \"Pick one\",\"n\": 1.5e0,\"tags\": [ \"a\" ],\"ok\": true,\"outputSch\\u0065ma\":{\"type\":\"string\"},\"inputSchema\":{\"type\":\"object\"},\"_meta\":{\"ui\":{\"visibility\":[\"app\"]}}}" ;

\ pick's entry, members as registered and the wrapper in its schema's place.
: PICK-ITEM ( -- )
   S\" {\"name\":\"pick\",\"title\":\"Pick one\",\"n\":1.5e0,\"tags\":[ \"a\" ],\"ok\":true," W+
   S\" {\"type\":\"string\"}" WRAPPER
   S\" ,\"inputSchema\":{\"type\":\"object\"},\"_meta\":{\"ui\":{\"visibility\":[\"app\"]}}}" W+ ;

\ A tool whose body answers with a _meta, listed as registered.
: FRAMED ( -- ptr u8 n )
   S\" {\"name\":\"framed\",\"description\":\"Answer with a _meta.\",\"inputSchema\":{\"type\":\"object\"},\"outputSchema\":{\"type\":\"object\"},\"annotations\":{\"readOnlyHint\":true}}" ;

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
   s" ," W+ FRAMED W+ ;

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


\ The widget resource's descriptor, as resources/list answers it.
: WIDGET-RESOURCE ( -- ptr u8 n )
   S\" {\"uri\":\"ui://test/widget.html\",\"name\":\"widget\",\"mimeType\":\"text/html;profile=mcp-app\",\"description\":\"A test widget.\",\"_meta\":{\"ui\":{\"csp\":{\"resourceDomains\":[\"https://cdn.test\"]}}}}" ;

\ A resource without _meta, as resources/list answers it.
: PLAIN ( -- ptr u8 n )
   S\" {\"uri\":\"ui://test/plain.html\",\"name\":\"plain\",\"mimeType\":\"text/html\"}" ;

\ The resources/list array's members.
: RESOURCE-ITEMS ( -- ) WIDGET-RESOURCE W+ s" ," W+ PLAIN W+ ;

\ Its page: a quote and a newline show the text's escaping.
: PAGE ( -- ptr u8 n ) S\" <p class=\"note\">Hi</p>\n" ;

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


\ A modern resources/read of the widget with the given id and Mcp-Name header.
: WIDGET-READ ( n ptr u8 n -- n )
   {: id:n name:ptr nu:n :}
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
   400 S\" {\"jsonrpc\":\"2.0\",\"id\":36,\"error\":{\"code\":-32602,\"message\":\"Invalid MCP request\"}}"
   ANSWERED ;


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
   [: MCP:ANSWER ;] MCP:ADD-TOOL ;

: ADD-REFUSE ( -- )
   S\" {\"name\":\"refuse\",\"description\":\"Refuse every call.\",\"inputSchema\":{\"type\":\"object\",\"properties\":{}},\"outputSchema\":{},\"annotations\":{\"readOnlyHint\":false}}"
   [: 2drop s" nope" MCP:ANSWER-ERROR ;] MCP:ADD-TOOL ;

: ADD-RAW ( -- )
   S\" {\"name\":\"raw\",\"description\":\"Answer text that is not JSON.\",\"inputSchema\":{\"type\":\"object\"},\"outputSchema\":{},\"annotations\":{\"readOnlyHint\":true}}"
   [: 2drop s" not json" MCP:ANSWER ;] MCP:ADD-TOOL ;

: ADD-SILENT ( -- )
   S\" {\"name\":\"silent\",\"description\":\"Never answer.\",\"inputSchema\":{\"type\":\"object\"},\"outputSchema\":{\"type\":[\"object\",\"null\"],\"items\":{\"type\":\"object\"}},\"annotations\":{\"readOnlyHint\":true}}"
   [: 2drop ;] MCP:ADD-TOOL ;

: ADD-BIG ( -- )
   S\" {\"name\":\"big\",\"description\":\"Answer a long string.\",\"inputSchema\":{\"type\":\"object\"},\"outputSchema\":{\"type\":\"string\"},\"annotations\":{\"readOnlyHint\":true}}"
   [: 2drop BIG BIG-U MCP:ANSWER ;] MCP:ADD-TOOL ;

\ Answers once the test sets RELEASED.
: ADD-STALL ( -- )
   S\" {\"name\":\"stall\",\"description\":\"Answer once released.\",\"inputSchema\":{\"type\":\"object\"},\"outputSchema\":{},\"annotations\":{\"readOnlyHint\":true}}"
   [: 2drop 1 ENTERED atomic!
      [: RELEASED atomic@ 0<> ;] DEADLINE s" mcp-test STALL" TEST-WAIT:POLL
      1 LEFT atomic!
      s" {}" MCP:ANSWER ;] MCP:ADD-TOOL ;

: ADD-LATE ( -- )
   S\" {\"name\":\"late\",\"description\":\"Registered while serving.\",\"inputSchema\":{\"type\":\"object\"},\"outputSchema\":{},\"annotations\":{\"readOnlyHint\":true}}"
   [: MCP:ANSWER ;] MCP:ADD-TOOL ;


\ ANSWER, keeping in SHAPE-CODE what it threw.
: SHAPE-ANSWER ( ptr u8 n -- )
   [: 2dup MCP:ANSWER ;] catch {: code:n :} 2drop
   code SHAPE-CODE atomic!
   code 0<> if code throw then ;


\ Answers {"sources":["a"]}, or an array for arguments naming "flat".
: ADD-SHAPE ( -- )
   S\" {\"name\":\"shape\",\"description\":\"Answer sources.\",\"inputSchema\":{\"type\":\"object\"},\"outputSchema\":{\"typ\\u0065\":\"object\",\"properties\":{\"sources\":{\"type\":\"array\"}}},\"annotations\":{\"readOnlyHint\":true}}"
   [: s" flat" CONTAINS? if s" []" else S\" {\"sources\":[\"a\"]}" then SHAPE-ANSWER ;]
   MCP:ADD-TOOL ;


: ADD-WIDGET ( -- ) WIDGET [: MCP:ANSWER ;] MCP:ADD-TOOL ;
: ADD-PICK ( -- ) PICKER [: MCP:ANSWER ;] MCP:ADD-TOOL ;
: ADD-DESCRIBED ( ptr u8 n -- ) [: MCP:ANSWER ;] MCP:ADD-TOOL ;


\ ANSWER-WITH {"ok":true} and meta, keeping in META-CODE what it threw.
: META-ANSWER ( ptr u8 n -- )
   [: 2dup S\" {\"ok\":true}" 2swap MCP:ANSWER-WITH ;] catch {: code:n :} 2drop
   code META-CODE atomic!
   code 0<> if code throw then ;


\ Answers with the arguments as the result's _meta, or [] for arguments naming
\ "flat".
: ADD-FRAMED ( -- )
   FRAMED [: 2dup s" flat" CONTAINS? if 2drop s" []" then META-ANSWER ;] MCP:ADD-TOOL ;


\ A resource with the descriptor and a small page.
: ADD-PAGE ( ptr u8 n -- ) s" <p></p>" MCP:ADD-RESOURCE ;

: RESOURCE-REFUSALS ( -- )
   s" ADD-RESOURCE refuses a descriptor without a uri" T-LABEL
   [: S\" {\"name\":\"x\",\"mimeType\":\"text/html\"}" ADD-PAGE ;] MCP:E-SCHEMA TTHROWSQ
   s" ADD-RESOURCE refuses an empty uri" T-LABEL
   [: S\" {\"uri\":\"\",\"name\":\"x\",\"mimeType\":\"text/html\"}" ADD-PAGE ;] MCP:E-SCHEMA TTHROWSQ
   s" ADD-RESOURCE refuses a uri that is no string" T-LABEL
   [: S\" {\"uri\":5,\"name\":\"x\",\"mimeType\":\"text/html\"}" ADD-PAGE ;] MCP:E-SCHEMA TTHROWSQ
   s" ADD-RESOURCE refuses a descriptor without a name" T-LABEL
   [: S\" {\"uri\":\"ui://x\",\"mimeType\":\"text/html\"}" ADD-PAGE ;] MCP:E-SCHEMA TTHROWSQ
   s" ADD-RESOURCE refuses a descriptor without a mimeType" T-LABEL
   [: S\" {\"uri\":\"ui://x\",\"name\":\"x\"}" ADD-PAGE ;] MCP:E-SCHEMA TTHROWSQ
   s" ADD-RESOURCE refuses a text that is not UTF-8" T-LABEL
   [: S\" {\"uri\":\"ui://x\",\"name\":\"x\",\"mimeType\":\"text/html\"}"
      S\" <p>\xff</p>" MCP:ADD-RESOURCE ;] MCP:E-SCHEMA TTHROWSQ
   s" ADD-RESOURCE refuses a registered uri, escaped" T-LABEL
   [: S\" {\"uri\":\"ui://test/widget\\u002ehtml\",\"name\":\"x\",\"mimeType\":\"text/html\"}"
      ADD-PAGE ;] MCP:E-DUPLICATE TTHROWSQ ;


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
   S\" {\"name\":\"secret\",\"description\":\"Answer the owner.\",\"inputSchema\":{\"type\":\"object\"},\"outputSchema\":{},\"annotations\":{\"readOnlyHint\":true}}"
   [: 2drop s" {}" MCP:ANSWER ;] MCP:ADD-TOOL ;

\ A claim the caller lacks shows as an empty string.
: CLAIM+ ( ptr u8 n -- ) BEARER:CLAIM$ drop NB+ ;

: ADD-WHOAMI ( -- )
   S\" {\"name\":\"whoami\",\"description\":\"Answer the caller's claims.\",\"inputSchema\":{\"type\":\"object\"},\"outputSchema\":{},\"annotations\":{\"readOnlyHint\":true}}"
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
   GUIDE$ MCP:INSTRUCTIONS!
   BIG-FILL
   ADD-ECHO ADD-REFUSE ADD-RAW ADD-SILENT ADD-BIG ADD-STALL ADD-SHAPE ADD-WIDGET ADD-PICK
   ADD-FRAMED
   s" ADD-TOOL refuses a descriptor without a name" T-LABEL
   [: S\" {\"inputSchema\":{},\"outputSchema\":{}}" ADD-DESCRIBED ;] MCP:E-SCHEMA TTHROWSQ
   s" ADD-TOOL refuses a descriptor whose name is not a string" T-LABEL
   [: S\" {\"name\":5,\"inputSchema\":{},\"outputSchema\":{}}" ADD-DESCRIBED ;] MCP:E-SCHEMA TTHROWSQ
   s" ADD-TOOL refuses a descriptor without an inputSchema" T-LABEL
   [: S\" {\"name\":\"x\",\"outputSchema\":{}}" ADD-DESCRIBED ;] MCP:E-SCHEMA TTHROWSQ
   s" ADD-TOOL refuses a descriptor whose inputSchema is not an object" T-LABEL
   [: S\" {\"name\":\"x\",\"inputSchema\":[],\"outputSchema\":{}}" ADD-DESCRIBED ;] MCP:E-SCHEMA TTHROWSQ
   s" ADD-TOOL refuses a descriptor without an outputSchema" T-LABEL
   [: S\" {\"name\":\"x\",\"inputSchema\":{}}" ADD-DESCRIBED ;] MCP:E-SCHEMA TTHROWSQ
   s" ADD-TOOL refuses a descriptor whose outputSchema is not an object" T-LABEL
   [: S\" {\"name\":\"x\",\"inputSchema\":{},\"outputSchema\":[]}" ADD-DESCRIBED ;] MCP:E-SCHEMA TTHROWSQ
   s" ADD-TOOL refuses a descriptor repeating a top-level key, escaped" T-LABEL
   [: S\" {\"name\":\"x\",\"n\\u0061me\":\"y\",\"inputSchema\":{},\"outputSchema\":{}}" ADD-DESCRIBED ;] MCP:E-SCHEMA TTHROWSQ
   s" ADD-TOOL refuses a descriptor that is not JSON" T-LABEL
   [: s" not json" ADD-DESCRIBED ;] MCP:E-SCHEMA TTHROWSQ
   s" ADD-TOOL refuses a registered name written with an escape" T-LABEL
   [: S\" {\"name\":\"\\u0065cho\",\"inputSchema\":{},\"outputSchema\":{}}" ADD-DESCRIBED ;] MCP:E-DUPLICATE TTHROWSQ
   s" ADD-TOOL refuses a descriptor without a top-level name, one nested" T-LABEL
   [: S\" {\"inputSchema\":{\"name\":\"x\"},\"outputSchema\":{}}" ADD-DESCRIBED ;] MCP:E-SCHEMA TTHROWSQ
   s" ADD-TOOL refuses an outputSchema repeating a top-level key" T-LABEL
   [: S\" {\"name\":\"t\",\"inputSchema\":{},\"outputSchema\":{\"type\":\"object\",\"type\":\"string\"}}" ADD-DESCRIBED ;] MCP:E-SCHEMA TTHROWSQ
   WIDGET-RESOURCE PAGE MCP:ADD-RESOURCE
   PLAIN ADD-PAGE
   RESOURCE-REFUSALS
   LOOPBACK 0 ONE-WORKER IDLE-MS HTTP:START
   s" ADD-TOOL refuses while HTTP serves" T-LABEL
   [: ADD-LATE ;] MCP:E-STATE TTHROWSQ
   s" ADD-RESOURCE refuses while HTTP serves" T-LABEL
   [: S\" {\"uri\":\"ui://late\",\"name\":\"late\",\"mimeType\":\"text/html\"}" ADD-PAGE ;]
   MCP:E-STATE TTHROWSQ
   s" INSTRUCTIONS! refuses while HTTP serves" T-LABEL
   [: GUIDE$ MCP:INSTRUCTIONS! ;] MCP:E-STATE TTHROWSQ
   LEGACY-INITIALIZE LEGACY-INITIALIZED MODERN-DISCOVER UNSUPPORTED
   METHOD-MISMATCH UNACCEPTED UNKNOWN-METHOD UNKNOWN-TOOL TOOL-ERROR
   TOOL-RESULT SHAPE-RESULT SHAPE-NOT-OBJECT META-LEGACY META-MODERN META-EMPTY
   META-NOT-OBJECT META-REPEATED META-SERVER-INFO BAD-ENVELOPE BAD-JSON LIST PING
   MODERN-LIST NAME-MISMATCH NAME-ABSENT META-MISMATCH NO-CAPABILITIES NO-PARAMS MODERN-NOTIFICATION
   LEGACY-NOTIFIED ARGUMENTS-NOT-OBJECT NOT-JSON-ANSWER SILENT
   RESOURCES-LIST MODERN-RESOURCES RESOURCE-READ PLAIN-READ MODERN-READ READ-MISMATCH
   UNKNOWN-RESOURCE URI-NOT-STRING TEMPLATES
   CLIENT-CASES
   PROTECTED-CASES
   HTTP:STOP
   AIO:STOP ;

T-RESET
SERVE-CASES
T-REPORT

;package
