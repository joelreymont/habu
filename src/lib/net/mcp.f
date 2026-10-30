\ mcp.f - an MCP tool server on Habu's HTTP server: the stateless, JSON-response
\ form of streamable HTTP for MCP 2026-07-28 with server/discover, and the
\ 2025-11-25 initialize flow. No session ID, no event stream. The host states
\ its name with SERVER-INFO! and its tools with ADD-TOOL before HTTP:START,
\ authenticates each request, validates its Origin (403 when one is present and
\ not allowed) and passes it to HANDLE.
\ STORAGE CLASS. PROCESS-WIDE: the server info and tool registry, written before
\ HTTP serves, and tables indexed by HTTP:WORKER-SLOT holding each request's
\ state.
\ https://modelcontextprotocol.io/specification/2026-07-28/basic/transports/streamable-http

require src/lib/string.f
require src/lib/span.f
require src/lib/base64.f
require src/lib/json-read.f
require src/lib/json-write.f
require src/lib/json-rpc.f
require src/lib/net/http.f

package MCP
public
-9480 constant E-FIRST
-9489 constant E-LAST
\ SERVER-INFO! or ADD-TOOL while HTTP serves, HANDLE before SERVER-INFO!, or a
\ body that does not answer exactly once.
-9481 constant E-STATE
-9482 constant E-DUPLICATE   \ ADD-TOOL of a registered name
private

-9480 constant E-INPUT       \ params or arguments that are not objects: -32602

0 constant MODERN            \ 2026-07-28
1 constant LEGACY            \ 2025-11-25

0 constant OPEN
1 constant CALLING           \ a tool's body runs and may answer
2 constant SENT              \ the response began; a later fault is the host's

0 constant DISCOVER-REPLY
1 constant INIT-REPLY
2 constant LIST-REPLY
3 constant CALL-REPLY
4 constant PING-REPLY
5 constant ERROR-REPLY

-32020 constant HEADER-MISMATCH
-32022 constant UNSUPPORTED-VERSION

TYPED-VARIABLE SERVER-NAME SPAN:span<u8>
TYPED-VARIABLE SERVER-VERSION SPAN:span<u8>

\ The registry: a row to each tool, in ADD-TOOL order.
variable TOOLS
DYNAMIC-BUFFER TOOL-NAME SPAN:span<u8>
DYNAMIC-BUFFER TOOL-INFO SPAN:span<u8>
DYNAMIC-BUFFER TOOL-SCHEMA SPAN:span<u8>
DYNAMIC-BUFFER TOOL-HINT bool
DYNAMIC-BUFFER TOOL-BODY [ ptr u8 n -- ]

\ A row to each HTTP worker; spans borrow the request's body and headers.
HTTP:MAX-WORKERS TYPED-BUFFER READER JR:cursor
HTTP:MAX-WORKERS TYPED-BUFFER FROM HTTP:request
HTTP:MAX-WORKERS TYPED-BUFFER TO HTTP:response
HTTP:MAX-WORKERS TYPED-BUFFER ID SPAN:span<u8>        \ empty when the id is not known
HTTP:MAX-WORKERS TYPED-BUFFER METHOD SPAN:span<u8>    \ JSON text, quotes kept
HTTP:MAX-WORKERS TYPED-BUFFER PARAMS SPAN:span<u8>    \ an object's JSON text, or empty
HTTP:MAX-WORKERS TYPED-BUFFER ERA n
HTTP:MAX-WORKERS TYPED-BUFFER STAGE n
HTTP:MAX-WORKERS TYPED-BUFFER REPLY n
HTTP:MAX-WORKERS TYPED-BUFFER STATUS n
HTTP:MAX-WORKERS TYPED-BUFFER CODE n
HTTP:MAX-WORKERS TYPED-BUFFER TEXT SPAN:span<u8>      \ an error's message, or a tool's result
HTTP:MAX-WORKERS TYPED-BUFFER FAULT bool              \ the tool result is isError
HTTP:MAX-WORKERS TYPED-BUFFER ASKED SPAN:span<u8>     \ an unsupported MCP-Protocol-Version
\ A decoded Mcp-Name is no longer than the header block that carried it.
HTTP:MAX-WORKERS HTTP:HEAD-CAP * SPAN-BUFFER: NAMES

: SLOT ( -- n ) HTTP:WORKER-SLOT ;
: CURSOR ( -- ptr JR:cursor ) SLOT READER ;
: ID$ ( -- ptr u8 n ) SLOT ID @ SPAN:$ ;
: METHOD$ ( -- ptr u8 n ) SLOT METHOD @ SPAN:$ ;
: PARAMS$ ( -- ptr u8 n ) SLOT PARAMS @ SPAN:$ ;
: TEXT$ ( -- ptr u8 n ) SLOT TEXT @ SPAN:$ ;
: LEGACY? ( -- bool ) SLOT ERA @ LEGACY = ;


\ A value's JSON text at the reader: a container whole, a string with quotes.
: RAW$ ( JR:reader -- JR:reader ptr u8 n )
   JR:TOKEN dup JR:T-OBJ = over JR:T-ARR = or if
      drop JR:VALUE-SPAN$ exit
   then
   JR:T-STR = if
      JR:SPAN$ {: a:ptr u:n :} a 1- u 2 + exit
   then
   JR:SPAN$ ;


\ The token kind of a non-empty JSON text's first value.
: KIND ( ptr u8 n -- n )
   {: a:ptr u:n :}
   CURSOR a u JR:INIT
   JR:NEXT {: k:n :}
   JR:CLOSE k ;

: OBJECT? ( ptr u8 n -- bool ) KIND JR:T-OBJ = ;


\ A member of an object's JSON text, and whether it is there; text that is not
\ an object, absent params included, is E-INPUT.
: FIELD? ( ptr u8 n ptr u8 n -- ptr u8 n bool )
   {: data:ptr len:n key:ptr ku:n :}
   len 0= if E-INPUT throw then
   CURSOR data len JR:INIT
   JR:NEXT JR:T-OBJ <> if JR:CLOSE E-INPUT throw then
   key ku JR:FIND-KEY if
      RAW$ {: a:ptr u:n :} JR:CLOSE a u true
   else JR:CLOSE s" " false then ;


\ Whether a JSON text is a string that decodes to the wanted bytes.
: STRING= ( ptr u8 n ptr u8 n -- bool )
   {: raw:ptr len:n wanted:ptr wu:n :}
   CURSOR raw len JR:INIT
   JR:NEXT JR:T-STR <> if JR:CLOSE false exit then
   wanted wu JR:STR-EQ? {: same:bool :}
   JR:CLOSE same ;

: METHOD= ( ptr u8 n -- bool ) METHOD$ 2swap STRING= ;


: BAD ( n n ptr u8 n -- )
   {: status:n code:n why:ptr wu:n :}
   status SLOT STATUS !
   code SLOT CODE !
   why wu SPAN:MAKE SLOT TEXT !
   ERROR-REPLY SLOT REPLY ! ;

: GOOD ( n -- ) SLOT REPLY !  200 SLOT STATUS ! ;
: MISMATCH ( -- ) 400 HEADER-MISMATCH s" Header mismatch" BAD ;
: NO-METHOD ( -- ) 404 -32601 s" Method not found" BAD ;
: BAD-INPUT ( -- ) 400 -32602 s" Invalid MCP request" BAD ;
: BAD-ENVELOPE ( -- ) 400 -32600 s" Invalid MCP request" BAD ;
: UNKNOWN-TOOL ( -- ) 400 -32602 s" Unknown tool" BAD ;

: SERVER-INFO ( ptr JSON-WRITE:writer -- ptr JSON-WRITE:writer )
   JSON-WRITE:OBJECT-START
   s" name" SERVER-NAME @ SPAN:$ JSON-WRITE:FIELD-S
   JSON-WRITE:COMMA s" version" SERVER-VERSION @ SPAN:$ JSON-WRITE:FIELD-S
   JSON-WRITE:OBJECT-END ;


: MODERN-META ( ptr JSON-WRITE:writer -- ptr JSON-WRITE:writer )
   JSON-WRITE:COMMA s" _meta" JSON-WRITE:KEY
   JSON-WRITE:OBJECT-START
   s" io.modelcontextprotocol/serverInfo" JSON-WRITE:KEY
   SERVER-INFO JSON-WRITE:OBJECT-END ;


: VERSIONS ( ptr JSON-WRITE:writer -- ptr JSON-WRITE:writer )
   JSON-WRITE:ARRAY-START s" 2026-07-28" JSON-WRITE:STRING
   JSON-WRITE:COMMA s" 2025-11-25" JSON-WRITE:STRING JSON-WRITE:ARRAY-END ;


: TOOLS-CAPABILITY ( ptr JSON-WRITE:writer -- ptr JSON-WRITE:writer )
   JSON-WRITE:COMMA s" capabilities" JSON-WRITE:KEY
   JSON-WRITE:OBJECT-START s" tools" JSON-WRITE:KEY
   JSON-WRITE:OBJECT-START JSON-WRITE:OBJECT-END JSON-WRITE:OBJECT-END ;


\ Discovery and tool lists are private with zero cache lifetime.
: PRIVATE-CACHE ( ptr JSON-WRITE:writer -- ptr JSON-WRITE:writer )
   JSON-WRITE:COMMA s" ttlMs" 0 JSON-WRITE:FIELD-U
   JSON-WRITE:COMMA s" cacheScope" s" private" JSON-WRITE:FIELD-S ;


\ https://modelcontextprotocol.io/specification/2026-07-28/server/discover
: DISCOVER-BODY ( ptr JSON-WRITE:writer -- ptr JSON-WRITE:writer )
   JSON-WRITE:OBJECT-START
   s" resultType" s" complete" JSON-WRITE:FIELD-S
   JSON-WRITE:COMMA s" supportedVersions" JSON-WRITE:KEY VERSIONS
   TOOLS-CAPABILITY MODERN-META PRIVATE-CACHE
   JSON-WRITE:OBJECT-END ;


: INIT-BODY ( ptr JSON-WRITE:writer -- ptr JSON-WRITE:writer )
   JSON-WRITE:OBJECT-START
   s" protocolVersion" s" 2025-11-25" JSON-WRITE:FIELD-S
   TOOLS-CAPABILITY
   JSON-WRITE:COMMA s" serverInfo" JSON-WRITE:KEY SERVER-INFO
   JSON-WRITE:OBJECT-END ;


\ A tool's structuredContent is {"data": the body's JSON}.
: OUTPUT-SCHEMA ( ptr JSON-WRITE:writer -- ptr JSON-WRITE:writer )
   JSON-WRITE:OBJECT-START s" type" s" object" JSON-WRITE:FIELD-S
   JSON-WRITE:COMMA s" properties" JSON-WRITE:KEY
   JSON-WRITE:OBJECT-START s" data" JSON-WRITE:KEY
   JSON-WRITE:OBJECT-START JSON-WRITE:OBJECT-END JSON-WRITE:OBJECT-END
   JSON-WRITE:COMMA s" required" JSON-WRITE:KEY
   JSON-WRITE:ARRAY-START s" data" JSON-WRITE:STRING JSON-WRITE:ARRAY-END
   JSON-WRITE:OBJECT-END ;


: TOOL-ENTRY ( ptr JSON-WRITE:writer n -- ptr JSON-WRITE:writer )
   {: idx:n :}
   JSON-WRITE:OBJECT-START
   s" name" idx TOOL-NAME @ SPAN:$ JSON-WRITE:FIELD-S
   JSON-WRITE:COMMA s" description" idx TOOL-INFO @ SPAN:$ JSON-WRITE:FIELD-S
   JSON-WRITE:COMMA s" inputSchema" idx TOOL-SCHEMA @ SPAN:$ JSON-WRITE:FIELD-RAW
   JSON-WRITE:COMMA s" outputSchema" JSON-WRITE:KEY OUTPUT-SCHEMA
   JSON-WRITE:COMMA s" annotations" JSON-WRITE:KEY
   JSON-WRITE:OBJECT-START s" readOnlyHint" idx TOOL-HINT @ JSON-WRITE:FIELD-BOOL
   JSON-WRITE:OBJECT-END JSON-WRITE:OBJECT-END ;


\ https://modelcontextprotocol.io/specification/2026-07-28/server/tools
: LIST-BODY ( ptr JSON-WRITE:writer -- ptr JSON-WRITE:writer )
   JSON-WRITE:OBJECT-START
   LEGACY? 0= if
      s" resultType" s" complete" JSON-WRITE:FIELD-S JSON-WRITE:COMMA
   then
   s" tools" JSON-WRITE:KEY JSON-WRITE:ARRAY-START
   TOOLS @ 0 ?do
      i 0<> if JSON-WRITE:COMMA then
      i TOOL-ENTRY
   loop
   JSON-WRITE:ARRAY-END
   LEGACY? 0= if PRIVATE-CACHE MODERN-META then
   JSON-WRITE:OBJECT-END ;


\ A tool result: TEXT as readable text and, unless isError, TEXT's JSON as
\ structuredContent.data.
: CALL-BODY ( ptr JSON-WRITE:writer -- ptr JSON-WRITE:writer )
   JSON-WRITE:OBJECT-START
   LEGACY? 0= if
      s" resultType" s" complete" JSON-WRITE:FIELD-S JSON-WRITE:COMMA
   then
   s" content" JSON-WRITE:KEY JSON-WRITE:ARRAY-START
   JSON-WRITE:OBJECT-START s" type" s" text" JSON-WRITE:FIELD-S
   JSON-WRITE:COMMA s" text" TEXT$ JSON-WRITE:FIELD-S JSON-WRITE:OBJECT-END
   JSON-WRITE:ARRAY-END
   SLOT FAULT @ 0= if
      JSON-WRITE:COMMA s" structuredContent" JSON-WRITE:KEY
      JSON-WRITE:OBJECT-START s" data" TEXT$ JSON-WRITE:FIELD-RAW JSON-WRITE:OBJECT-END
   then
   JSON-WRITE:COMMA s" isError" SLOT FAULT @ JSON-WRITE:FIELD-BOOL
   LEGACY? 0= if MODERN-META then
   JSON-WRITE:OBJECT-END ;


: PING-BODY ( ptr JSON-WRITE:writer -- ptr JSON-WRITE:writer )
   JSON-WRITE:OBJECT-START JSON-WRITE:OBJECT-END ;


: RESULT-BODY ( ptr JSON-WRITE:writer -- ptr JSON-WRITE:writer )
   ID$ JSON--RPC-ID:MAKE JSON-RPC:RESULT
   SLOT REPLY @ {: kind:n :}
   kind DISCOVER-REPLY = if DISCOVER-BODY else
   kind INIT-REPLY = if INIT-BODY else
   kind LIST-REPLY = if LIST-BODY else
   kind CALL-REPLY = if CALL-BODY else PING-BODY
   then then then then
   JSON-RPC:END ;


\ -32022's data: the supported versions and the one requested.
: VERSION-DATA ( ptr JSON-WRITE:writer -- ptr JSON-WRITE:writer )
   JSON-WRITE:OBJECT-START s" supported" JSON-WRITE:KEY VERSIONS
   JSON-WRITE:COMMA s" requested" SLOT ASKED @ SPAN:$ JSON-WRITE:FIELD-S
   JSON-WRITE:OBJECT-END ;


: ID-FIELD ( ptr JSON-WRITE:writer -- ptr JSON-WRITE:writer )
   ID$ {: id:ptr iu:n :}
   iu 0= if exit then
   JSON-WRITE:COMMA s" id" id iu JSON-WRITE:FIELD-RAW ;


\ An error reply. Both eras' schemas make its id optional and never null, so
\ an unknown id is left out; JSON-RPC:ERROR's null id is JSON-RPC's own rule.
: ERROR-BODY ( ptr JSON-WRITE:writer -- ptr JSON-WRITE:writer )
   JSON-WRITE:OBJECT-START s" jsonrpc" s" 2.0" JSON-WRITE:FIELD-S ID-FIELD
   JSON-WRITE:COMMA s" error" JSON-WRITE:KEY JSON-WRITE:OBJECT-START
   s" code" SLOT CODE @ JSON-WRITE:FIELD-INT
   JSON-WRITE:COMMA s" message" TEXT$ JSON-WRITE:FIELD-S
   SLOT CODE @ UNSUPPORTED-VERSION = if
      JSON-WRITE:COMMA s" data" JSON-WRITE:KEY VERSION-DATA
   then
   JSON-WRITE:OBJECT-END JSON-WRITE:OBJECT-END ;


: RESPONSE-BODY ( ptr JSON-WRITE:writer -- ptr JSON-WRITE:writer )
   SLOT REPLY @ ERROR-REPLY = if ERROR-BODY else RESULT-BODY then ;


: SEND ( -- )
   SENT SLOT STAGE !
   SLOT TO @ {: answer:HTTP:response :}
   answer SLOT STATUS @ HTTP:STATUS!
   answer [: RESPONSE-BODY ;] HTTP:JSON! ;


\ A header's value when it arrived exactly once; otherwise empty and false.
: HEADER$ ( HTTP:request ptr u8 n -- ptr u8 n bool )
   {: asked:HTTP:request name:ptr len:n :}
   asked name len HTTP:HEADER-COUNT-OF 1 <> if s" " false exit then
   asked name len HTTP:HEADER-OF$ ;


\ A media type without its parameters.
: MEDIA-ESSENCE ( ptr u8 n -- ptr u8 n )
   {: media:ptr size:n :}
   media size 59 INDEX-OF MATCH option
      some OF IDX>N media swap ENDOF
      none OF media size ENDOF
   ;MATCH TRIM ;


\ Whether a comma-separated header value lists the media type.
: ACCEPT-HAS? ( ptr u8 n ptr u8 n -- bool )
   {: value:ptr len:n wanted:ptr wu:n :}
   0 len 1+ 0 ?do
      i len = if true else value i + c@ 44 = then if
         {: start:n :}
         value start + i start - MEDIA-ESSENCE wanted wu STR=CI if
            unloop true exit
         then
         i 1+
      then
   loop drop false ;


: ACCEPT? ( HTTP:request -- bool )
   s" Accept" HEADER$ 0= if 2drop false exit then
   {: value:ptr len:n :}
   value len s" application/json" ACCEPT-HAS?
   value len s" text/event-stream" ACCEPT-HAS? and ;

: CONTENT-TYPE? ( HTTP:request -- bool )
   s" Content-Type" HEADER$ 0= if 2drop false exit then
   MEDIA-ESSENCE s" application/json" STR=CI ;


\ A JSON body, and Accept offering both application/json and text/event-stream
\ though every answer is JSON.
: TRANSPORT? ( HTTP:request -- bool )
   {: asked:HTTP:request :}
   asked CONTENT-TYPE? asked ACCEPT? and if true exit then
   400 -32600 s" MCP requires JSON and both Accept types" BAD false ;


\ MCP ids are strings or integers, never null.
: MCP-ID? ( ptr u8 n -- bool )
   KIND {: k:n :}
   k JR:T-STR = k JR:T-INT = or ;


\ A request, or a notification with an empty id: params, when present, are an
\ object.
: CALL? ( ptr u8 n ptr u8 n ptr u8 n -- bool )
   {: id:ptr iu:n m:ptr mu:n p:ptr pu:n :}
   iu 0<> if
      id iu MCP-ID? 0= if BAD-ENVELOPE false exit then
   then
   id iu SPAN:MAKE SLOT ID !
   m mu SPAN:MAKE SLOT METHOD !
   p pu SPAN:MAKE SLOT PARAMS !
   pu 0<> if
      p pu OBJECT? 0= if BAD-INPUT false exit then
   then true ;


\ A JSON-RPC response is no request: answered without an id.
: RESPONSE? ( JSON-RPC:id ptr u8 n -- bool )
   2drop drop BAD-ENVELOPE false ;


\ The reader's id is the reply's when it is an MCP id; its null id is unknown.
: INVALID? ( n ptr u8 n JSON-RPC:id -- bool )
   {: code:n why:ptr wu:n reply:JSON-RPC:id :}
   reply JSON-RPC:ID$ {: id:ptr iu:n :}
   id iu MCP-ID? if id iu SPAN:MAKE SLOT ID ! then
   code JSON-RPC:PARSE-ERROR = if 400 -32700 s" Invalid JSON" BAD false exit then
   BAD-ENVELOPE false ;


: ENVELOPE? ( ptr u8 n -- bool )
   {: body:ptr len:n :}
   CURSOR body len JSON-RPC:>MESSAGE
   MATCH JSON-RPC:message
      request OF {: i m:ptr mu:n p:ptr pu:n :}
         i JSON-RPC:ID$ m mu p pu CALL? ENDOF
      notification OF {: m:ptr mu:n p:ptr pu:n :}
         s" " m mu p pu CALL? ENDOF
      success OF RESPONSE? ENDOF
      failure OF RESPONSE? ENDOF
      invalid OF INVALID? ENDOF
   ;MATCH ;


\ A member of params._meta, and whether it is there.
: META ( ptr u8 n -- ptr u8 n bool )
   {: key:ptr ku:n :}
   PARAMS$ dup 0= if false exit then
   s" _meta" FIELD? 0= if false exit then
   key ku FIELD? ;


\ Requests carry params._meta with protocolVersion and clientCapabilities: one
\ missing is -32602 (2026-07-28 basic "Per-request protocol fields"), a version
\ other than the header's -32020.
: MODERN? ( ptr u8 n -- bool )
   {: version:ptr vu:n :}
   s" io.modelcontextprotocol/protocolVersion" META 0= if
      2drop 400 -32602 s" Protocol version required" BAD false exit
   then
   version vu STRING= 0= if MISMATCH false exit then
   s" io.modelcontextprotocol/clientCapabilities" META
   if OBJECT? else 2drop false then
   0= if 400 -32602 s" Client capabilities required" BAD false exit then
   true ;


: ERA? ( ptr u8 n -- bool )
   {: value:ptr vu:n :}
   value vu s" 2026-07-28" STR= if
      MODERN SLOT ERA ! value vu MODERN? exit
   then
   value vu s" 2025-11-25" STR= if LEGACY SLOT ERA ! true exit then
   value vu SPAN:MAKE SLOT ASKED !
   400 UNSUPPORTED-VERSION s" Unsupported protocol version" BAD false ;


\ The era from one MCP-Protocol-Version: 2026-07-28 also requires MODERN?;
\ 2025-11-25, or
\ no header on initialize, is the legacy lifecycle
\ (https://modelcontextprotocol.io/specification/2025-11-25/basic/lifecycle).
: VERSION? ( HTTP:request -- bool )
   {: asked:HTTP:request :}
   asked s" MCP-Protocol-Version" HTTP:HEADER-COUNT-OF 1 > if MISMATCH false exit then
   asked s" MCP-Protocol-Version" HEADER$ if ERA? exit then
   2drop
   s" initialize" METHOD= if LEGACY SLOT ERA ! true exit then
   MISMATCH false ;


\ An Mcp-Name value, decoded into the worker's row when it is =?base64?...?=.
: DECODED-NAME$ ( ptr u8 n -- ptr u8 n )
   {: header:ptr len:n :}
   header len s" =?base64?" STARTS-WITH? 0= if header len exit then
   len 11 < if E-INPUT throw then
   header len 2 - + 2 s" ?=" STR= 0= if E-INPUT throw then
   NAMES SLOT HTTP:HEAD-CAP * HTTP:HEAD-CAP SPAN:SUB {: room :}
   header 9 + len 11 - room BASE64:DECODE {: u:n :}
   room 0 SPAN:AT u ;


\ The codes of an Mcp-Name that is not a valid encoded name.
: NAME-FAULT? ( n -- bool )
   {: code:n :}
   code E-INPUT = code E-BASE64-FIRST <= code E-BASE64-LAST >= and or ;


\ A modern tools/call's Mcp-Name names params.name.
: NAME-HEADER? ( HTTP:request -- bool )
   {: asked:HTTP:request :}
   PARAMS$ s" name" FIELD? 0= if
      2drop 400 -32602 s" Tool name required" BAD false exit
   then
   {: name:ptr nu:n :}
   name nu KIND JR:T-STR <> if 400 -32602 s" Invalid tool name" BAD false exit then
   asked s" Mcp-Name" HEADER$ 0= if 2drop MISMATCH false exit then
   [: DECODED-NAME$ ;] catch {: code:n :}
   code 0<> if
      code NAME-FAULT? 0= if code throw then
      2drop MISMATCH false exit
   then
   name nu 2swap STRING= dup 0= if MISMATCH then ;


\ The modern era's Mcp-Method names the body's method, -32020 otherwise.
: HEADERS? ( HTTP:request -- bool )
   {: asked:HTTP:request :}
   LEGACY? if true exit then
   asked s" Mcp-Method" HEADER$ 0= if 2drop MISMATCH false exit then
   METHOD$ 2swap STRING= 0= if MISMATCH false exit then
   s" tools/call" METHOD= 0= if true exit then
   asked NAME-HEADER? ;


\ The registered tool a JSON name text names, else -1.
: TOOL-OF ( ptr u8 n -- n )
   {: raw:ptr ru:n :}
   TOOLS @ 0 ?do
      raw ru i TOOL-NAME @ SPAN:$ STRING= if i unloop exit then
   loop -1 ;


: ARGUMENTS ( -- ptr u8 n )
   PARAMS$ s" arguments" FIELD? 0= if 2drop s" {}" exit then
   2dup OBJECT? 0= if E-INPUT throw then ;


\ The body answers through ANSWER or ANSWER-ERROR before it returns.
: CALL ( -- )
   PARAMS$ s" name" FIELD? 0= if E-INPUT throw then
   TOOL-OF {: idx:n :}
   idx 0< if UNKNOWN-TOOL SEND exit then
   ARGUMENTS
   CALLING SLOT STAGE !
   idx TOOL-BODY @ execute
   SLOT STAGE @ SENT <> if E-STATE throw then ;


: INITIALIZE ( -- )
   LEGACY? if INIT-REPLY GOOD else NO-METHOD then SEND ;

: DISCOVER ( -- )
   LEGACY? if NO-METHOD else DISCOVER-REPLY GOOD then SEND ;

\ A 2025-11-25 notification is accepted, 202 with no body, before any method
\ runs (2025-11-25 transports); 2026-07-28 takes only requests.
: NOTIFIED ( -- )
   LEGACY? if SLOT TO @ 202 HTTP:STATUS! exit then
   400 -32600 s" Request id required" BAD SEND ;


: DISPATCH ( -- )
   ID$ nip 0= if NOTIFIED exit then
   s" initialize" METHOD= if INITIALIZE exit then
   s" server/discover" METHOD= if DISCOVER exit then
   s" tools/list" METHOD= if LIST-REPLY GOOD SEND exit then
   s" ping" METHOD= LEGACY? and if PING-REPLY GOOD SEND exit then
   s" tools/call" METHOD= if CALL exit then
   NO-METHOD SEND ;


: PROCESS ( -- )
   SLOT FROM @ {: asked:HTTP:request :}
   asked TRANSPORT? 0= if SEND exit then
   asked HTTP:BODY$ ENVELOPE? 0= if SEND exit then
   asked VERSION? 0= if SEND exit then
   asked HEADERS? 0= if SEND exit then
   DISPATCH ;


: REGISTERED? ( ptr u8 n -- bool )
   {: name:ptr nu:n :}
   TOOLS @ 0 ?do
      name nu i TOOL-NAME @ SPAN:$ STR= if true unloop exit then
   loop false ;


: TOOLS-RESERVE ( n -- )
   {: n:n :}
   n TOOL-NAME-RESERVE
   n TOOL-INFO-RESERVE
   n TOOL-SCHEMA-RESERVE
   n TOOL-HINT-RESERVE
   n TOOL-BODY-RESERVE ;

: RESULT! ( ptr u8 n bool -- )
   {: text:ptr u:n fault:bool :}
   SLOT STAGE @ CALLING <> if E-STATE throw then
   fault 0= if CURSOR text u JR:INIT JR:FINISH then
   text u SPAN:MAKE SLOT TEXT !
   fault SLOT FAULT !
   CALL-REPLY GOOD SEND ;

public

\ The server's name and version in serverInfo. The texts are borrowed and
\ outlive the server, as literals do. E-STATE while HTTP serves.
: SERVER-INFO! ( ptr u8 n ptr u8 n -- )
   {: name:ptr nu:n version:ptr vu:n :}
   HTTP:RUNNING? if E-STATE throw then
   name nu SPAN:MAKE SERVER-NAME !
   version vu SPAN:MAKE SERVER-VERSION ! ;


\ A tool: its name, description, input JSON Schema (an object's JSON text),
\ read-only hint and body. The body gets the arguments' JSON object, borrowed for
\ the call only, and answers once through ANSWER, or through ANSWER-ERROR for a
\ tool error, arguments it refuses included. Bodies may run at once on several
\ HTTP workers. The texts are borrowed and outlive the server, as literals do.
\ A schema's x-mcp-header is not supported: the server does not validate the
\ headers it names. E-STATE while HTTP serves; E-DUPLICATE for a registered
\ name.
: ADD-TOOL ( ptr u8 n ptr u8 n ptr u8 n bool [ ptr u8 n -- ] -- )
   {: name:ptr nu:n info:ptr iu:n schema:ptr su:n hint:bool body :}
   HTTP:RUNNING? if E-STATE throw then
   name nu REGISTERED? if E-DUPLICATE throw then
   TOOLS @ {: idx:n :}
   idx 1+ TOOLS-RESERVE
   name nu SPAN:MAKE idx TOOL-NAME !
   info iu SPAN:MAKE idx TOOL-INFO !
   schema su SPAN:MAKE idx TOOL-SCHEMA !
   hint idx TOOL-HINT !
   body idx TOOL-BODY !
   idx 1+ TOOLS ! ;


\ A body's result: one JSON value as structuredContent.data and as the text
\ content. E-STATE outside a body or after its answer; text that is not one
\ JSON value throws the reader's E-JR-* code, which HANDLE passes to the host.
: ANSWER ( ptr u8 n -- ) false RESULT! ;

\ A body's tool error: the message as text content, isError true.
: ANSWER-ERROR ( ptr u8 n -- ) true RESULT! ;


\ Answers one MCP request whose authentication and Origin the host has checked.
\ A malformed envelope is -32600, missing or malformed params -32602, invalid
\ JSON -32700. Any other fault, a tool body's included, is rethrown to the
\ host. E-STATE before SERVER-INFO!.
: HANDLE ( HTTP:request HTTP:response -- )
   {: asked:HTTP:request answer:HTTP:response :}
   SERVER-NAME @ SPAN:LEN 0= if E-STATE throw then
   asked SLOT FROM !
   answer SLOT TO !
   OPEN SLOT STAGE !
   s" " SPAN:MAKE SLOT ID !
   [: PROCESS ;] catch {: code:n :}
   code 0= if exit then
   SLOT STAGE @ OPEN <> code E-INPUT <> or if code throw then
   BAD-INPUT SEND ;

;package
