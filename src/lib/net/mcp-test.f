\ mcp-test.f - MCP:HANDLE answered over a real loopback port: both eras'
\ handshakes and notifications, the protocol-version, _meta, Mcp-Method and
\ Mcp-Name checks, the transport check, unknown methods and tools, a body's
\ result and tool error, bodies that answer badly or never, malformed
\ envelopes, arguments and JSON, tools/list and ping, and ADD-TOOL's refusals.
\ Run: bin/hb --load src/lib/net/mcp-test.f

require src/lib/test.f
require src/lib/test/wait.f
require src/lib/string.f
require src/lib/fmt.f
require src/lib/aio.f
require src/lib/net/curl.f
require src/lib/net/http.f
require src/lib/net/mcp.f

package MCP-TEST
private

$7F000001 constant LOOPBACK
1 constant ONE-WORKER
500 constant IDLE-MS
$4000 constant CAP
CAP BUFFER: BODY
variable BODY-U

: OK ( CURL:status -- )
   MATCH CURL:status
      ok OF true ENDOF
      failed OF drop false ENDOF
   ;MATCH TTRUE ;

28 constant CURLE-TIMED-OUT             \ CURLE_OPERATION_TIMEDOUT

\ The response's status, its body in BODY; -1 for a body past CAP, -2 for a
\ failed transfer. CURLE 28 is libcurl's default connect timeout, met by a
\ starved or stopped process: E-PROC-TIMEOUT, a timeout and not a FAIL.
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


: UNKNOWN-TOOL ( -- )
   s" an unregistered tool is 400 -32602 Unknown tool" T-LABEL
   LEGACY
   S\" {\"jsonrpc\":\"2.0\",\"id\":6,\"method\":\"tools/call\",\"params\":{\"name\":\"none\"}}" POST
   400 S\" {\"jsonrpc\":\"2.0\",\"id\":6,\"error\":{\"code\":-32602,\"message\":\"Unknown tool\"}}"
   ANSWERED ;


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


: BAD-JSON ( -- )
   s" a body that is not JSON is -32700 with no id" T-LABEL
   LEGACY S\" {\"jsonrpc\":" POST
   400 S\" {\"jsonrpc\":\"2.0\",\"error\":{\"code\":-32700,\"message\":\"Invalid JSON\"}}"
   ANSWERED ;


: OUTPUT ( -- )
   S\" \"outputSchema\":{\"type\":\"object\",\"properties\":{\"data\":{}}," SB-APPEND
   S\" \"required\":[\"data\"]},\"annotations\":{\"readOnlyHint\":" SB-APPEND ;

\ The registered tools as tools/list shows them.
: TOOLS ( -- )
   S\" \"tools\":[{\"name\":\"echo\"," SB-APPEND
   S\" \"description\":\"Echo the arguments.\",\"inputSchema\":{\"type\":\"object\"}," SB-APPEND
   OUTPUT s" true}},{" SB-APPEND
   S\" \"name\":\"refuse\",\"description\":\"Refuse every call.\"," SB-APPEND
   S\" \"inputSchema\":{\"type\":\"object\",\"properties\":{}}," SB-APPEND
   OUTPUT s" false}},{" SB-APPEND
   S\" \"name\":\"raw\",\"description\":\"Answer text that is not JSON.\"," SB-APPEND
   S\" \"inputSchema\":{\"type\":\"object\"}," SB-APPEND
   OUTPUT s" true}},{" SB-APPEND
   S\" \"name\":\"silent\",\"description\":\"Never answer.\"," SB-APPEND
   S\" \"inputSchema\":{\"type\":\"object\"}," SB-APPEND
   OUTPUT s" true}}]" SB-APPEND ;


: LIST ( -- )
   s" tools/list shows each tool's schema and read-only hint" T-LABEL
   LEGACY S\" {\"jsonrpc\":\"2.0\",\"id\":10,\"method\":\"tools/list\"}" POST
   SB-RESET S\" {\"jsonrpc\":\"2.0\",\"id\":10,\"result\":{" SB-APPEND
   TOOLS s" }}" SB-APPEND
   200 SB$ ANSWERED ;


: MODERN-LIST ( -- )
   s" a modern tools/list adds resultType, cache scope and serverInfo" T-LABEL
   MODERN s" Mcp-Method: tools/list" ADD
   SB-RESET S\" {\"jsonrpc\":\"2.0\",\"id\":12,\"method\":\"tools/list\",\"params\":{" SB-APPEND
   META S\" }}" SB-APPEND SB$ POST
   SB-RESET S\" {\"jsonrpc\":\"2.0\",\"id\":12,\"result\":{\"resultType\":\"complete\"," SB-APPEND
   TOOLS S\" ,\"ttlMs\":0,\"cacheScope\":\"private\"," SB-APPEND
   SERVER S\" }}" SB-APPEND
   200 SB$ ANSWERED ;


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
   s" echo" s" Echo the arguments." S\" {\"type\":\"object\"}" true
   [: MCP:ANSWER ;] MCP:ADD-TOOL ;

: ADD-REFUSE ( -- )
   s" refuse" s" Refuse every call." S\" {\"type\":\"object\",\"properties\":{}}" false
   [: 2drop s" nope" MCP:ANSWER-ERROR ;] MCP:ADD-TOOL ;

: ADD-RAW ( -- )
   s" raw" s" Answer text that is not JSON." S\" {\"type\":\"object\"}" true
   [: 2drop s" not json" MCP:ANSWER ;] MCP:ADD-TOOL ;

: ADD-SILENT ( -- )
   s" silent" s" Never answer." S\" {\"type\":\"object\"}" true
   [: 2drop ;] MCP:ADD-TOOL ;

: ADD-LATE ( -- )
   s" late" s" Registered while serving." S\" {\"type\":\"object\"}" true
   [: MCP:ANSWER ;] MCP:ADD-TOOL ;


: SERVE-CASES ( -- )
   AIO:START
   HTTP:ROUTES-RESET
   s" POST" s" /mcp" [: MCP:HANDLE ;] HTTP:ROUTE
   s" test-server" s" 1.0" MCP:SERVER-INFO!
   ADD-ECHO ADD-REFUSE ADD-RAW ADD-SILENT
   s" ADD-TOOL refuses a registered name" T-LABEL
   [: ADD-ECHO ;] MCP:E-DUPLICATE TTHROWSQ
   LOOPBACK 0 ONE-WORKER IDLE-MS HTTP:START
   s" ADD-TOOL refuses while HTTP serves" T-LABEL
   [: ADD-LATE ;] MCP:E-STATE TTHROWSQ
   LEGACY-INITIALIZE LEGACY-INITIALIZED MODERN-DISCOVER UNSUPPORTED
   METHOD-MISMATCH UNACCEPTED UNKNOWN-METHOD UNKNOWN-TOOL TOOL-ERROR
   TOOL-RESULT BAD-ENVELOPE BAD-JSON LIST PING MODERN-LIST NAME-MISMATCH
   NAME-ABSENT META-MISMATCH NO-CAPABILITIES NO-PARAMS MODERN-NOTIFICATION
   LEGACY-NOTIFIED ARGUMENTS-NOT-OBJECT NOT-JSON-ANSWER SILENT
   HTTP:STOP
   AIO:STOP ;

T-RESET
SERVE-CASES
T-REPORT

;package
