\ mcp-routes-test.f - MCP:ROUTES over a real loopback port, whose own routes
\ are the token issuer's: a person's call answered by the quotation, the 401
\ of no token, the 403s of a token of neither kind and of an Origin unlisted,
\ null, several, or spelled past, short of or across listed ones, with a token
\ and without, a listed Origin's call, the metadata answered at METADATA!'s
\ path and at the well-known one, GET of /mcp, what ORIGIN! and ROUTES reject,
\ and, started again, the metadata of a resource with no path.
\ Run: bin/hb --load src/lib/net/mcp-routes-test.f

require src/lib/test/assert.f
require src/lib/string.f
require src/lib/fmt.f
require src/lib/aio.f
require src/lib/task.f
require src/lib/heap.f
require src/lib/vector.f
require src/lib/json.f
require src/lib/reason.f
require src/lib/net/curl.f
require src/lib/net/http.f
require src/lib/net/bearer.f
require src/lib/net/mcp.f
require src/lib/net/mcp-routes.f
require src/lib/net/test-issuer.f
require src/lib/net/http-router.f

package MCP-ROUTES-TEST
private

$7F000001 constant LOOPBACK
\ CALLER? fetches /keys from this same server.
2 constant TWO-WORKERS
500 constant IDLE-MS
600 constant LIFE-S                 \ how far past now a token's exp lies
$2000 constant CAP

TYPED-VARIABLE CFG HTTP:config
TYPED-VARIABLE BODY ^VEC:vector<u8>
TYPED-VARIABLE HEADS ^VEC:vector<u8>
TYPED-VARIABLE TOKEN ^VEC:vector<u8>
TYPED-VARIABLE KEYS-URL ^VEC:vector<u8>
TYPED-VARIABLE ANSWER ^VEC:vector<u8>  \ the tool's JSON
variable PORT
variable CALLS                      \ requests the quotation was given

: NEW-VEC ( -- ^VEC:vector<u8> ) HEAP:ALLOCATOR 0 [type] u8 VEC:NEW ;

: VECTORS ( -- )
   NEW-VEC BODY ! NEW-VEC HEADS ! NEW-VEC TOKEN !
   NEW-VEC KEYS-URL ! NEW-VEC ANSWER ! ;

: SERVE-ON ( n -- )
   {: port:n :}
   LOOPBACK CFG HTTP-CONFIG:ADDRESS !
   port CFG HTTP-CONFIG:PORT !
   TWO-WORKERS CFG HTTP-CONFIG:WORKERS !
   IDLE-MS >MS CFG HTTP-CONFIG:IDLE !
   CFG HTTP:START ;


\ The tool answers who BEARER says calls.
: WHO ( str -- )
   drop
   ANSWER @ {: out:^ :}
   out 0 VEC:RESIZE
   S\" \"" out VEC:APPEND
   out BEARER:CALLER-KEY
   S\" \"" out VEC:APPEND
   out VEC:BYTES MCP:RESULT ;

: ADD-WHO ( -- )
   S\" {\"name\":\"who\",\"inputSchema\":{\"type\":\"object\"},\"outputSchema\":{}}"
   ['] WHO MCP:ADD-TOOL ;

: HOST ( HTTP:request HTTP:response -- )
   1 CALLS atomic-add drop
   MCP:HANDLE ;


\ A setter's failure throws: the case cannot make its request on that handle.
: OK ( CURL:status -- )
   MATCH CURL:status
      ok OF ENDOF
      failed OF drop CURL:E-STATE throw ENDOF
   ;MATCH ;

: URL ( str -- str )
   {: path:str :}
   SB-RESET s" http://127.0.0.1:" SB-APPEND HTTP:PORT FMT:SB-U
   path SB-APPEND SB$ ;

: OPEN ( str -- CURL:handle )
   {: path:str :}
   CURL:INIT
   MATCH CURL:init-result
      ready OF ENDOF
      failed OF drop CURL:EMPTY-HANDLE ENDOF
   ;MATCH {: h:CURL:handle :}
   h path URL CURL:URL! OK h ;

: ADD ( CURL:handle str -- CURL:handle )
   {: h:CURL:handle s:str :}
   h s CURL:HEADER+ OK h ;

: BEARING ( CURL:handle -- CURL:handle )
   SB-RESET s" Authorization: Bearer " SB-APPEND
   TOKEN @ VEC:BYTES SB-APPEND SB$ ADD ;

: BODY$ ( -- str ) BODY @ VEC:BYTES ;

\ The response's status, its body in BODY and its head in HEADS; -1 for a body
\ past CAP, -2 for a failed transfer.
: PERFORM ( CURL:handle -- n )
   {: h:CURL:handle :}
   BODY @ 0 VEC:RESIZE
   HEADS @ 0 VEC:RESIZE
   h CAP >LEN BODY @ CURL:PERFORM
   MATCH CURL:fetch-result
      response OF drop CURL:HTTP-STATUS>N h HEADS @ CURL:HEADERS ENDOF
      truncated OF drop drop -1 ENDOF
      failed OF drop -2 ENDOF
   ;MATCH ;

: FINISH ( CURL:handle -- n )
   {: h:CURL:handle :}
   h PERFORM h CURL:CLEANUP ;

: GET ( str -- n ) OPEN FINISH ;

: MCP-OPEN ( -- CURL:handle )
   s" /mcp" OPEN
   s" Content-Type: application/json" ADD
   s" Accept: application/json, text/event-stream" ADD
   s" MCP-Protocol-Version: 2025-11-25" ADD ;

: POST ( CURL:handle -- n )
   {: h:CURL:handle :}
   h S\" {\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"tools/call\",\"params\":{\"name\":\"who\"}}"
   CURL:BODY! OK
   h FINISH ;


: CLAIMS ( str -- str )
   {: who:str :}
   SB-RESET
   S\" {\"iss\":\"https://issuer.test\",\"aud\":\"api://mcp-routes-test\",\"exp\":" SB-APPEND
   epoch-seconds LIFE-S + FMT:SB-INT
   who SB-APPEND SB$ ;

: MINT ( str -- ) CLAIMS TOKEN @ TEST-ISSUER:MINT drop ;

: PERSON ( -- )
   S\" ,\"azp\":\"app-2\",\"oid\":\"o-1\",\"tid\":\"t-1\",\"scp\":\"mcp.call\"}" MINT ;

: NEITHER ( -- ) S\" ,\"azp\":\"app-2\",\"oid\":\"o-1\",\"tid\":\"t-1\"}" MINT ;


\ The person's call reached the quotation and the tool answered it.
: CALLED ( n -- )
   {: status:n :}
   BODY$ S\" \"structuredContent\":{\"data\":\"entra:t-1:o-1\"}" STR:CONTAINS? TTRUE
   CALLS atomic@ 1 T=
   status 200 T= ;

\ A rejection that ran no quotation.
: REJECTED ( n n -- )
   {: status:n want:n :}
   CALLS atomic@ 0 T=
   status want T= ;

: FORBIDDEN ( n str -- )
   {: status:n code:str :}
   BODY$ code STR:CONTAINS? TTRUE
   status 403 REJECTED ;

: FRESH ( -- ) 0 CALLS atomic! ;

: CALLERS ( -- )
   s" a person's call reaches the quotation and is answered" T-LABEL
   FRESH PERSON MCP-OPEN BEARING POST CALLED
   s" no token is 401" T-LABEL
   FRESH MCP-OPEN POST 401 REJECTED
   s" a token of neither kind of caller is 403 caller_rejected" T-LABEL
   FRESH NEITHER MCP-OPEN BEARING POST s" caller_rejected" FORBIDDEN ;

\ The Origin headers the quotation adds, with a person's token and with none.
: ORIGIN-REJECTED ( [ CURL:handle -- CURL:handle ] -- )
   {: origins :}
   FRESH PERSON
   MCP-OPEN BEARING origins execute POST s" origin_rejected" FORBIDDEN
   MCP-OPEN origins execute POST s" origin_rejected" FORBIDDEN ;

: ORIGIN-CASES ( -- )
   s" an unlisted Origin is 403 origin_rejected" T-LABEL
   [: s" Origin: https://evil.example" ADD ;] ORIGIN-REJECTED
   s" a null Origin is 403 origin_rejected" T-LABEL
   [: s" Origin: null" ADD ;] ORIGIN-REJECTED
   s" two listed Origins are 403 origin_rejected" T-LABEL
   [: s" Origin: https://chat.example" ADD
      s" Origin: https://app.example:8443" ADD ;] ORIGIN-REJECTED
   s" an Origin a listed one starts is 403 origin_rejected" T-LABEL
   [: s" Origin: https://chat.example:8443" ADD ;] ORIGIN-REJECTED
   s" an Origin that starts a listed one is 403 origin_rejected" T-LABEL
   [: s" Origin: https://chat.exam" ADD ;] ORIGIN-REJECTED
   s" an Origin of two listed ones is 403 origin_rejected" T-LABEL
   [: s" Origin: https://chat.example https://app.example:8443" ADD ;]
   ORIGIN-REJECTED
   s" a listed Origin's call with a person's token is answered" T-LABEL
   FRESH PERSON
   MCP-OPEN BEARING s" Origin: https://chat.example" ADD POST CALLED
   s" so is the other listed Origin's" T-LABEL
   FRESH
   MCP-OPEN BEARING s" Origin: https://app.example:8443" ADD POST CALLED ;

: METADATA ( n str -- )
   {: status:n resource:str :}
   SB-RESET S\" {\"resource\":\"" SB-APPEND resource SB-APPEND
   S\" \",\"authorization_servers\":[\"https://issuer.test\"]" SB-APPEND
   S\" ,\"scopes_supported\":[\"mcp.call\",\"offline_access\"]}" SB-APPEND
   BODY$ SB$ T$=
   status 200 T= ;

: GETS ( -- )
   s" GET of METADATA!'s path answers the metadata" T-LABEL
   s" /.well-known/oauth-protected-resource/mcp" GET
   s" https://mcp.example/mcp" METADATA
   s" GET of the bare well-known path answers the same" T-LABEL
   s" /.well-known/oauth-protected-resource" GET
   s" https://mcp.example/mcp" METADATA
   s" GET of /mcp is 405 allowing POST" T-LABEL
   s" /mcp" GET 405 T=
   HEADS @ VEC:BYTES S\" Allow: POST\r\n" STR:CONTAINS? TTRUE ;

: BARE ( -- )
   s" a resource with no path has its metadata at the bare path" T-LABEL
   s" /.well-known/oauth-protected-resource" GET
   s" https://mcp.example" METADATA
   s" and at no other" T-LABEL
   s" /.well-known/oauth-protected-resource/mcp" GET 404 T= ;


: ADD-ROUTES ( -- ) ['] HOST MCP:ROUTES ;

: SAID ( str -- ) MCP:E-SCHEMA REASON:TEXT$ swap T$= ;

: SETTER-CASES ( -- )
   s" ROUTES with no METADATA! URL stated throws E-HTTP-STATE" T-LABEL
   ['] ADD-ROUTES E-HTTP-STATE TTHROWSQ
   s" ORIGIN! rejects an origin with a path" T-LABEL
   [: s" https://x.example/" MCP:ORIGIN! ;] MCP:E-SCHEMA TTHROWSQ
   s" origin `https://x.example/`: expected http or https, a host and an optional port, alone; write it as a browser's Origin header does, like `https://chat.example`" SAID
   s" ORIGIN! rejects an empty text" T-LABEL
   [: s" " MCP:ORIGIN! ;] MCP:E-SCHEMA TTHROWSQ ;

: SERVING-CASES ( -- )
   s" ORIGIN! while HTTP serves throws E-STATE" T-LABEL
   [: s" https://late.example" MCP:ORIGIN! ;] MCP:E-STATE TTHROWSQ
   s" ROUTES while HTTP serves throws E-STATE" T-LABEL
   ['] ADD-ROUTES MCP:E-STATE TTHROWSQ ;

\ Learns a port, which the key set's URL then names: ISSUER! is rejected while
\ HTTP serves.
: KEEP-PORT ( -- )
   0 SERVE-ON
   HTTP:PORT PORT !
   s" /keys" URL KEYS-URL @ VEC:APPEND
   HTTP:STOP ;

: STATE ( str -- )
   {: url:str :}
   url s" mcp.call" BEARER:METADATA!
   HTTP:ROUTES-RESET
   TEST-ISSUER:ROUTES
   ADD-ROUTES
   PORT @ SERVE-ON ;

: SERVE-CASES ( -- )
   AIO:START
   VECTORS
   s" mcp-routes-test" s" 1.0" MCP:SERVER-INFO!
   ADD-WHO
   KEEP-PORT
   s" https://issuer.test" s" api://mcp-routes-test" KEYS-URL @ VEC:BYTES
   BEARER:ISSUER!
   SETTER-CASES
   s" https://chat.example" MCP:ORIGIN!
   s" https://app.example:8443" MCP:ORIGIN!
   s" https://mcp.example/.well-known/oauth-protected-resource" STATE
   BARE
   HTTP:STOP
   s" https://mcp.example/.well-known/oauth-protected-resource/mcp" STATE
   SERVING-CASES CALLERS ORIGIN-CASES GETS
   HTTP:STOP
   AIO:STOP ;

T-RESET
SERVE-CASES
T-REPORT

;package
