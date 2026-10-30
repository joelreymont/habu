\ mcp.f - an MCP tool server on Habu's HTTP server: the stateless, JSON-response
\ form of streamable HTTP for MCP 2026-07-28 with server/discover, and the
\ 2025-11-25 initialize flow. No session ID, no event stream. The host states
\ its name with SERVER-INFO! and its tools with ADD-TOOL before HTTP:START,
\ authenticates each request, validates its Origin (403 when one is present and
\ not allowed) and passes it to HANDLE. A tool body gets the arguments' object,
\ borrowed for the call, and answers once through ANSWER, or ANSWER-ERROR for a
\ tool error, refused arguments included; bodies may run at once on several
\ HTTP workers. An input schema's x-mcp-header is not supported: the server
\ does not validate the headers it names. A tool's answer is its result's
\ structuredContent when its output schema's "type" is "object"; any other
\ schema describes the answer as structuredContent's "data". The client side,
\ OPEN, TOOLS, CALL and CLOSE, speaks the 2026-07-28 era to one server over
\ CURL and takes a JSON or an event-stream reply.
\ STORAGE CLASS. PROCESS-WIDE: the server info and tool registry, written before
\ HTTP serves; tables indexed by HTTP:WORKER-SLOT holding each request's state;
\ and the client table, a row per open client, read and written under its own
\ lock.
\ https://modelcontextprotocol.io/specification/2026-07-28/basic/transports/streamable-http

require src/lib/string.f
require src/lib/span.f
require src/lib/base64.f
require src/lib/json-read.f
require src/lib/json-write.f
require src/lib/json-rpc.f
require src/lib/memory.f
require src/lib/byte-buffer.f
require src/lib/task.f
require src/lib/time.f
require src/lib/net/curl.f
require src/lib/net/event-stream.f
require src/lib/net/http.f

package MCP
public
-9480 constant E-FIRST
-9489 constant E-LAST
\ SERVER-INFO! or ADD-TOOL while HTTP serves, HANDLE before SERVER-INFO!, a
\ body that does not answer exactly once, or a client word given a closed
\ client.
-9481 constant E-STATE
-9482 constant E-DUPLICATE   \ ADD-TOOL of a registered name
-9483 constant E-TRANSFER    \ a client's request could not be sent or its reply received
\ A reply that is not a 2xx JSON-RPC success for the request, or whose result
\ lacks the shape its method answers.
-9484 constant E-RESPONSE
\ An ADD-TOOL output schema that is not one JSON object or repeats a top-level
\ key, or an answer under an object schema that is not an object.
-9485 constant E-SCHEMA

\ A client of one server's endpoint, from OPEN until CLOSE.
NEWTYPE client
private

-9480 constant E-INPUT       \ params or arguments that are not objects: -32602

0 constant MODERN            \ 2026-07-28
1 constant LEGACY            \ 2025-11-25

0 constant IDLE
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
variable TOOL-COUNT
DYNAMIC-BUFFER TOOL-NAME SPAN:span<u8>
DYNAMIC-BUFFER TOOL-INFO SPAN:span<u8>
DYNAMIC-BUFFER TOOL-SCHEMA SPAN:span<u8>
DYNAMIC-BUFFER TOOL-OUTPUT SPAN:span<u8>
DYNAMIC-BUFFER TOOL-WHOLE bool              \ TOOL-OUTPUT's "type" is "object"
DYNAMIC-BUFFER TOOL-HINT bool
DYNAMIC-BUFFER TOOL-BODY [ ptr u8 n -- ]
TYPED-VARIABLE EARLIER JR:cursor            \ SHAPE's second reader over a schema

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
HTTP:MAX-WORKERS TYPED-BUFFER CALLED n                \ the tool a tools/call runs
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


\ The declared schema, or one whose structuredContent is {"data": the answer}.
: OUTPUT-SCHEMA ( ptr JSON-WRITE:writer n -- ptr JSON-WRITE:writer )
   {: idx:n :}
   idx TOOL-OUTPUT @ SPAN:$ {: text:ptr tu:n :}
   idx TOOL-WHOLE @ if s" outputSchema" text tu JSON-WRITE:FIELD-RAW exit then
   s" outputSchema" JSON-WRITE:KEY
   JSON-WRITE:OBJECT-START s" type" s" object" JSON-WRITE:FIELD-S
   JSON-WRITE:COMMA s" properties" JSON-WRITE:KEY
   JSON-WRITE:OBJECT-START s" data" text tu JSON-WRITE:FIELD-RAW JSON-WRITE:OBJECT-END
   JSON-WRITE:COMMA s" required" JSON-WRITE:KEY
   JSON-WRITE:ARRAY-START s" data" JSON-WRITE:STRING JSON-WRITE:ARRAY-END
   JSON-WRITE:OBJECT-END ;


: TOOL-ENTRY ( ptr JSON-WRITE:writer n -- ptr JSON-WRITE:writer )
   {: idx:n :}
   JSON-WRITE:OBJECT-START
   s" name" idx TOOL-NAME @ SPAN:$ JSON-WRITE:FIELD-S
   JSON-WRITE:COMMA s" description" idx TOOL-INFO @ SPAN:$ JSON-WRITE:FIELD-S
   JSON-WRITE:COMMA s" inputSchema" idx TOOL-SCHEMA @ SPAN:$ JSON-WRITE:FIELD-RAW
   JSON-WRITE:COMMA idx OUTPUT-SCHEMA
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
   TOOL-COUNT @ 0 ?do
      i 0<> if JSON-WRITE:COMMA then
      i TOOL-ENTRY
   loop
   JSON-WRITE:ARRAY-END
   LEGACY? 0= if PRIVATE-CACHE MODERN-META then
   JSON-WRITE:OBJECT-END ;


\ A tool result: TEXT as readable text and, unless isError, TEXT's JSON as
\ structuredContent, or as its "data" for a tool without an object schema.
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
      JSON-WRITE:COMMA
      SLOT CALLED @ TOOL-WHOLE @ if
         s" structuredContent" TEXT$ JSON-WRITE:FIELD-RAW
      else
         s" structuredContent" JSON-WRITE:KEY
         JSON-WRITE:OBJECT-START s" data" TEXT$ JSON-WRITE:FIELD-RAW JSON-WRITE:OBJECT-END
      then
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
   TOOL-COUNT @ 0 ?do
      raw ru i TOOL-NAME @ SPAN:$ STRING= if i unloop exit then
   loop -1 ;


: ARGUMENTS ( -- ptr u8 n )
   PARAMS$ s" arguments" FIELD? 0= if 2drop s" {}" exit then
   2dup OBJECT? 0= if E-INPUT throw then ;


\ The body answers through ANSWER or ANSWER-ERROR before it returns.
: RUN-TOOL ( -- )
   PARAMS$ s" name" FIELD? 0= if E-INPUT throw then
   TOOL-OF {: idx:n :}
   idx 0< if UNKNOWN-TOOL SEND exit then
   ARGUMENTS
   idx SLOT CALLED !
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
   s" tools/call" METHOD= if RUN-TOOL exit then
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
   TOOL-COUNT @ 0 ?do
      name nu i TOOL-NAME @ SPAN:$ STR= if true unloop exit then
   loop false ;


: TOOLS-RESERVE ( n -- )
   {: n:n :}
   n TOOL-NAME-RESERVE
   n TOOL-INFO-RESERVE
   n TOOL-SCHEMA-RESERVE
   n TOOL-OUTPUT-RESERVE
   n TOOL-WHOLE-RESERVE
   n TOOL-HINT-RESERVE
   n TOOL-BODY-RESERVE ;

\ Whether a JSON text, read whole, is an object; text that is not one JSON value
\ throws the reader's E-JR-* code.
: OBJECT-TEXT? ( ptr u8 n -- bool )
   {: text:ptr u:n :}
   CURSOR text u JR:INIT JR:NEXT JR:T-OBJ = {: object:bool :}
   JR:FINISH object ;


\ Whether text's first top-level key that decodes to key holds a value other
\ than the one at at.
: EARLIER? ( ptr u8 n ptr u8 n ptr u8 -- bool )
   {: text:ptr u:n key:ptr ku:n at:ptr :}
   EARLIER text u JR:INIT JR:NEXT drop
   key ku JR:FIND-KEY drop JR:SPAN$ drop at <> {: earlier:bool :}
   JR:CLOSE earlier ;


\ On a top-level key of text: E-SCHEMA when an earlier key decodes the same,
\ else the reader moves past the key's value. room holds the decoded key.
: ONCE ( JR:reader ptr u8 n ptr u8 n -- JR:reader )
   {: text:ptr u:n room:ptr cap:n :}
   room cap JR:STR {: ku:n :}
   JR:NEXT drop JR:SPAN$ drop {: at:ptr :}
   text u room ku at EARLIER? if E-SCHEMA throw then
   JR:SKIP-VALUE ;


\ A key decodes to no more bytes than the text holds.
: KEYS ( ptr u8 n ptr u8 NUM:alloc-byte-len -- )
   MEM:ALLOCATION>SPAN SPAN:$ {: text:ptr u:n room:ptr cap:n :}
   CURSOR text u JR:INIT JR:NEXT drop
   begin JR:NEXT JR:T-KEY = while text u room cap ONCE repeat
   JR:FINISH ;


\ E-SCHEMA unless one JSON object with no top-level key repeated once decoded:
\ JSON.parse keeps a repeated key's last value, so a client would read another.
: SHAPE ( ptr u8 n -- )
   {: text:ptr u:n :}
   text u [: 2dup OBJECT-TEXT? 0= if E-SCHEMA throw then ;] catch {: code:n :} 2drop
   code E-JR-FIRST <= code E-JR-LAST >= and if E-SCHEMA throw then
   code 0<> if code throw then
   text u u MEM:BYTES-ALLOC-LEN [: KEYS ;] MEM:WITH-BYTES ;


\ Whether a schema object's "type" is the string "object".
: WHOLE? ( ptr u8 n -- bool )
   {: text:ptr u:n :}
   CURSOR text u JR:INIT JR:NEXT drop
   s" type" JR:FIND-KEY if
      JR:TOKEN JR:T-STR = if s" object" JR:STR-EQ? else false then
   else false then {: whole:bool :}
   JR:FINISH whole ;


: RESULT! ( ptr u8 n bool -- )
   {: text:ptr u:n fault:bool :}
   SLOT STAGE @ CALLING <> if E-STATE throw then
   fault 0= if
      text u OBJECT-TEXT? 0= SLOT CALLED @ TOOL-WHOLE @ and if E-SCHEMA throw then
   then
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


\ The texts are borrowed and outlive the server, as literals do. E-SCHEMA on a
\ repeated top-level key; "#" in a schema without "type":"object" is the wrapper.
: ADD-TOOL ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n bool [ ptr u8 n -- ] -- )
   {: name:ptr nu:n info:ptr iu:n schema:ptr su:n output:ptr ou:n hint:bool body :}
   HTTP:RUNNING? if E-STATE throw then
   name nu REGISTERED? if E-DUPLICATE throw then
   output ou SHAPE
   output ou WHOLE? {: whole:bool :}
   TOOL-COUNT @ {: idx:n :}
   idx 1+ TOOLS-RESERVE
   name nu SPAN:MAKE idx TOOL-NAME !
   info iu SPAN:MAKE idx TOOL-INFO !
   schema su SPAN:MAKE idx TOOL-SCHEMA !
   output ou SPAN:MAKE idx TOOL-OUTPUT !
   whole idx TOOL-WHOLE !
   hint idx TOOL-HINT !
   body idx TOOL-BODY !
   idx 1+ TOOL-COUNT ! ;


\ A body's result: one JSON value. E-STATE outside a body or after its answer;
\ else the reader's E-JR-* code or E-SCHEMA, which HANDLE passes to the host.
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
   IDLE SLOT STAGE !
   s" " SPAN:MAKE SLOT ID !
   [: PROCESS ;] catch {: code:n :}
   code 0= if exit then
   SLOT STAGE @ IDLE <> code E-INPUT <> or if code throw then
   BAD-INPUT SEND ;


\ The client. A client is a row of a process-wide table that grows with the
\ clients open; a free row is an empty region and is reused. The table is read
\ and written only under LOCK, since growth moves it; a row's region never
\ moves. A client's value is the generation OPEN gave its row from a
\ process-wide count, and CLOSE clears the row's generation, so a closed
\ client's value names no row even after another OPEN reuses it. The region
\ holds a record, five BUFs and the SSE decoder's storage. The BUFs are the URL;
\ the request, then, once BODY! has copied it, a header line and the reply's
\ header block; the reply; the tool list TOOLS assembles; and the response an
\ event-stream reply carried. The handle of the exchange in flight, none between
\ exchanges, is the table's: a record that held it could not be cast from its
\ region.

private

CAST: >CLIENT ( n -- client )
CAST: CLIENT>N ( client -- n )

\ A row's record: the cursor parses a reply and the writer builds a request;
\ status is the HTTP status the last exchange ended with, and stream whether
\ its reply was an event stream.
STRUCTURE peer DERIVE addr
   FIELD cursor JR:cursor
   FIELD writer JSON-WRITE:writer
   FIELD status n
   FIELD stream bool
;STRUCTURE

CAST: >PEER ( ptr u8 -- ptr peer )

$1000 constant FIRST-CAP
PEER-BYTES constant URL-OFF
URL-OFF BUF:HDR-BYTES + constant REQUEST-OFF
REQUEST-OFF BUF:HDR-BYTES + constant REPLY-OFF
REPLY-OFF BUF:HDR-BYTES + constant LIST-OFF
LIST-OFF BUF:HDR-BYTES + constant MESSAGE-OFF
MESSAGE-OFF BUF:HDR-BYTES + constant DECODER-OFF
DECODER-OFF SSE:STORAGE-BYTES + constant REGION-BYTES

\ No task is live while this file loads, so the lock is made ready here.
TASK:FACILITY LOCK
LOCK TASK:FACILITY-INIT
variable ROW-COUNT
variable GENERATION                 \ the last generation OPEN gave a row
DYNAMIC-BUFFER ROW-REGION SPAN:span<u8>
DYNAMIC-BUFFER ROW-HANDLE option<CURL:handle>
DYNAMIC-BUFFER ROW-GEN n            \ an open row's generation, 0 for a free one

: REGION ( n -- SPAN:span<u8> ) LOCK TASK:GET ROW-REGION @ LOCK TASK:RELEASE ;
: HANDLE@ ( n -- option<CURL:handle> ) LOCK TASK:GET ROW-HANDLE @ LOCK TASK:RELEASE ;
: HANDLE! ( option<CURL:handle> n -- ) LOCK TASK:GET ROW-HANDLE ! LOCK TASK:RELEASE ;

: ROW ( n -- ptr peer ) REGION SPAN:$ drop >PEER ;

: BUF-AT ( n n -- ptr n )
   {: idx:n off:n :}
   idx REGION off BUF:HDR-BYTES SPAN:SUB SPAN:$ drop CELL-VIEW ;

: URL-BUF ( n -- ptr n ) URL-OFF BUF-AT ;
: REQUEST-BUF ( n -- ptr n ) REQUEST-OFF BUF-AT ;
: REPLY-BUF ( n -- ptr n ) REPLY-OFF BUF-AT ;
: LIST-BUF ( n -- ptr n ) LIST-OFF BUF-AT ;
: MESSAGE-BUF ( n -- ptr n ) MESSAGE-OFF BUF-AT ;

\ A decoder opened on the row's storage.
: DECODER ( n -- SSE:decoder )
   REGION DECODER-OFF SSE:STORAGE-BYTES SPAN:SUB SPAN:$ {: a:ptr u:n :}
   a CELL-VIEW u SSE:OPEN ;

: BUF$ ( ptr n -- ptr u8 n ) BUF:SPAN$ BUF:BLEN>N ;
: BUF+ ( ptr u8 n ptr n -- ) {: a:ptr u:n buf:ptr :} a u BUF:N>BLEN buf BUF:APPEND-SPAN ;


\ The first free row, else -1; under LOCK.
: FREE-ROW ( -- n )
   ROW-COUNT @ 0 ?do
      i ROW-REGION @ SPAN:LEN 0= if i unloop exit then
   loop -1 ;

\ A free row at the table's end; under LOCK.
: GROW ( -- )
   ROW-COUNT @ {: count:n :}
   count 1+ ROW-REGION-RESERVE
   count 1+ ROW-HANDLE-RESERVE
   count 1+ ROW-GEN-RESERVE
   s" " SPAN:MAKE count ROW-REGION !
   0 count ROW-GEN !
   count 1+ ROW-COUNT ! ;

\ Draws the next generation into GENERATION and makes sure a row is free;
\ under LOCK.
: READY ( -- )
   GENERATION @ 1+ GENERATION !
   FREE-ROW 0< if GROW then ;

\ A client whose row now holds the region. When no row can be had, the region
\ is freed and the throw passes on.
: CLAIM ( SPAN:span<u8> -- client )
   {: region :}
   LOCK TASK:GET
   ['] READY catch {: code:n :}
   code 0<> if LOCK TASK:RELEASE region MEM:FREE-SPAN code throw then
   FREE-ROW {: idx:n :}
   GENERATION @ {: gen:n :}
   region idx ROW-REGION !
   OPTION:NONE idx ROW-HANDLE !
   gen idx ROW-GEN !
   LOCK TASK:RELEASE
   gen >CLIENT ;

\ The row holding the generation, else -1; under LOCK.
: GEN-ROW ( n -- n )
   {: gen:n :}
   ROW-COUNT @ 0 ?do
      i ROW-GEN @ gen = if i unloop exit then
   loop -1 ;

\ An open client's row: E-STATE unless a row holds the client's generation. A
\ free row's 0 is no generation.
: LIVE ( client -- n )
   CLIENT>N {: gen:n :}
   gen 1 < if E-STATE throw then
   LOCK TASK:GET
   gen GEN-ROW {: idx:n :}
   LOCK TASK:RELEASE
   idx 0< if E-STATE throw then
   idx ;

\ Gives the row's buffers and region back and frees the row.
: RELEASE ( n -- )
   {: idx:n :}
   idx URL-BUF BUF:DISPOSE
   idx REQUEST-BUF BUF:DISPOSE
   idx REPLY-BUF BUF:DISPOSE
   idx LIST-BUF BUF:DISPOSE
   idx MESSAGE-BUF BUF:DISPOSE
   idx REGION {: region :}
   LOCK TASK:GET
   s" " SPAN:MAKE idx ROW-REGION !
   0 idx ROW-GEN !
   LOCK TASK:RELEASE
   region MEM:FREE-SPAN ;

\ Readies a claimed row's buffers and copies the URL; answers its operands.
: FILL-ROW ( n ptr u8 n -- n ptr u8 n )
   {: idx:n url:ptr uu:n :}
   idx URL-BUF FIRST-CAP BUF:N>BLEN BUF:INIT
   idx REQUEST-BUF FIRST-CAP BUF:N>BLEN BUF:INIT
   idx REPLY-BUF FIRST-CAP BUF:N>BLEN BUF:INIT
   idx LIST-BUF FIRST-CAP BUF:N>BLEN BUF:INIT
   idx MESSAGE-BUF FIRST-CAP BUF:N>BLEN BUF:INIT
   url uu idx URL-BUF BUF+
   idx url uu ;


: CHECK ( CURL:status -- )
   MATCH CURL:status
      ok OF ENDOF
      failed OF drop E-TRANSFER throw ENDOF
   ;MATCH ;


\ Room in a BUF past the bytes it holds.
: ROOM ( ptr n -- ptr u8 n )
   {: buf:ptr :}
   buf BUF$ {: a:ptr u:n :}
   a u + buf BUF:CAP@ BUF:BLEN>N u - ;


\ Collects the end of a transfer CANCEL has stopped.
: DRAIN ( n CURL:handle -- )
   {: idx:n h:CURL:handle :}
   idx REPLY-BUF {: buf:ptr :}
   begin
      h buf BUF:SPAN$ drop buf BUF:CAP@ BUF:BLEN>N >LEN CURL:NEXT
      MATCH CURL:chunk-result
         chunk OF 2drop false ENDOF
         response OF drop true ENDOF
         failed OF drop true ENDOF
      ;MATCH
   until ;


\ Lets the row's exchange go: a transfer still in flight is cancelled and
\ drained, then its handle cleaned up, and a reader a throw left is disposed.
: RETIRE ( n -- )
   {: idx:n :}
   idx HANDLE@ MATCH option
      none OF ENDOF
      some OF {: h:CURL:handle :}
         h [: dup CURL:CANCEL ;] catch {: code:n :} drop
         \ CANCEL's only E-STATE is a handle with no transfer in flight.
         code 0= if idx h DRAIN else
            code CURL:E-STATE <> if code throw then
         then
         h CURL:CLEANUP
      ENDOF
   ;MATCH
   OPTION:NONE idx HANDLE!
   idx ROW PEER-CURSOR JR:DISPOSE ;


: META-PARAM ( ptr JSON-WRITE:writer -- ptr JSON-WRITE:writer )
   s" _meta" JSON-WRITE:KEY JSON-WRITE:OBJECT-START
   s" io.modelcontextprotocol/protocolVersion" s" 2026-07-28" JSON-WRITE:FIELD-S
   JSON-WRITE:COMMA s" io.modelcontextprotocol/clientCapabilities" JSON-WRITE:KEY
   JSON-WRITE:OBJECT-START JSON-WRITE:OBJECT-END
   JSON-WRITE:OBJECT-END ;

\ The id of every request a client sends, as its JSON text.
: REQUEST-ID$ ( -- ptr u8 n ) s" 1" ;

\ A request with the request id and the method, up to the params' members.
: REQUEST-START ( ptr JSON-WRITE:writer ptr u8 n -- ptr JSON-WRITE:writer )
   {: m:ptr mu:n :}
   JSON-WRITE:OBJECT-START s" jsonrpc" s" 2.0" JSON-WRITE:FIELD-S
   JSON-WRITE:COMMA s" id" REQUEST-ID$ JSON-WRITE:FIELD-RAW
   JSON-WRITE:COMMA s" method" m mu JSON-WRITE:FIELD-S
   JSON-WRITE:COMMA s" params" JSON-WRITE:KEY JSON-WRITE:OBJECT-START ;

: REQUEST-END ( ptr JSON-WRITE:writer -- ptr JSON-WRITE:writer )
   META-PARAM JSON-WRITE:OBJECT-END JSON-WRITE:OBJECT-END ;

: WRITER ( n -- ptr JSON-WRITE:writer )
   {: idx:n :}
   idx ROW PEER-WRITER idx REQUEST-BUF JSON-WRITE:OPEN-BUF ;

\ tools/list from the cursor's JSON string, or from the start when it is empty.
: LIST-REQUEST ( n ptr u8 n -- )
   {: idx:n next:ptr nu:n :}
   idx WRITER s" tools/list" REQUEST-START
   nu 0<> if s" cursor" next nu JSON-WRITE:FIELD-RAW JSON-WRITE:COMMA then
   REQUEST-END JSON-WRITE:CLOSE ;

: CALL-REQUEST ( n ptr u8 n ptr u8 n -- )
   {: idx:n name:ptr nu:n args:ptr au:n :}
   idx WRITER s" tools/call" REQUEST-START
   s" name" name nu JSON-WRITE:FIELD-S JSON-WRITE:COMMA
   s" arguments" args au JSON-WRITE:FIELD-RAW JSON-WRITE:COMMA
   REQUEST-END JSON-WRITE:CLOSE ;


: VISIBLE? ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   u 0 ?do
      a i + c@ dup $21 < swap $7E > or if false unloop exit then
   loop true ;

\ The Mcp-Name line, built in the request buffer: the name as it is when every
\ byte is visible ASCII, else =?base64?...?=.
: NAME-LINE ( n ptr u8 n -- ptr u8 n )
   {: idx:n name:ptr nu:n :}
   idx REQUEST-BUF {: buf:ptr :}
   buf BUF:CLEAR
   s" Mcp-Name: " buf BUF+
   name nu VISIBLE? if name nu buf BUF+ else
      s" =?base64?" buf BUF+
      name nu buf nu 2 + 3 / 4 * BUF:N>BLEN BUF:GROW BASE64:ENCODE drop
      s" ?=" buf BUF+
   then
   buf BUF$ ;


\ A fresh handle, held by the row, posting the request buffer to the URL with
\ the 2026-07-28 headers and the Mcp-Method line. A handle a halt left is
\ retired first.
: PREPARE ( n ptr u8 n ms -- CURL:handle )
   {: idx:n line:ptr lu:n limit:ms :}
   idx RETIRE
   CURL:INIT
   MATCH CURL:init-result
      ready OF ENDOF
      failed OF drop E-TRANSFER throw ENDOF
   ;MATCH {: h:CURL:handle :}
   h OPTION:SOME idx HANDLE!
   h idx URL-BUF BUF$ CURL:URL! CHECK
   h idx REQUEST-BUF BUF$ CURL:BODY! CHECK
   h limit CURL:TIMEOUT! CHECK
   h s" Content-Type: application/json" CURL:HEADER+ CHECK
   h s" Accept: application/json, text/event-stream" CURL:HEADER+ CHECK
   h s" MCP-Protocol-Version: 2026-07-28" CURL:HEADER+ CHECK
   h line lu CURL:HEADER+ CHECK
   h ;


\ The finished reply's header block, in the request buffer, which grows to it.
: HEAD$ ( n CURL:handle -- ptr u8 n )
   {: idx:n h:CURL:handle :}
   idx REQUEST-BUF {: buf:ptr :}
   buf BUF:CLEAR
   begin
      buf BUF:SPAN$ drop {: at:ptr :}
      h at buf BUF:CAP@ BUF:BLEN>N >LEN CURL:HEADERS
      MATCH CURL:header-result
         complete OF LEN>N at swap buf BUF+ true ENDOF
         truncated OF LEN>N BUF:N>BLEN buf swap BUF:ENSURE false ENDOF
      ;MATCH
   until
   buf BUF$ ;

\ Whether a header line is Content-Type naming text/event-stream.
: EVENT-TYPE? ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   a u 58 INDEX-OF MATCH option
      none OF -1 ENDOF
      some OF IDX>N ENDOF
   ;MATCH {: at:n :}
   at 0< if false exit then
   a at TRIM s" Content-Type" STR=CI 0= if false exit then
   a at + 1+ u at - 1- MEDIA-ESSENCE s" text/event-stream" STR=CI ;

\ Whether the finished reply is an event stream, by its header block's lines.
: STREAM? ( n CURL:handle -- bool )
   HEAD$ {: a:ptr u:n :}
   0 begin dup u < while
      {: at:n :}
      a at + u at - 10 INDEX-OF MATCH option
         none OF u at - ENDOF
         some OF IDX>N ENDOF
      ;MATCH {: len:n :}
      a at + len EVENT-TYPE? if true exit then
      at len + 1+
   repeat drop false ;


\ Starts the transfer and receives the whole reply into the reply buffer,
\ which grows with it: NEXT writes each chunk into the room past the bytes
\ received, and appending the chunk from where it lies keeps it. NEXT follows
\ STREAM-START with no PAUSE between, so a halt is read in NEXT's wait, which
\ gives the transfer up.
: TRANSFER ( n CURL:handle -- )
   {: idx:n h:CURL:handle :}
   idx REPLY-BUF {: buf:ptr :}
   buf BUF:CLEAR
   h CURL:STREAM-START CHECK
   begin
      buf buf BUF:LEN@ BUF:BLEN>N 1+ BUF:N>BLEN BUF:ENSURE
      buf ROOM {: room:ptr ru:n :}
      h room ru >LEN CURL:NEXT
      MATCH CURL:chunk-result
         chunk OF nip LEN>N room swap buf BUF+ false ENDOF
         response OF CURL:HTTP-STATUS>N idx ROW PEER-STATUS ! true ENDOF
         failed OF drop E-TRANSFER throw ENDOF
      ;MATCH
   until
   idx h STREAM? idx ROW PEER-STREAM ! ;

: LIST-EXCHANGE ( n ms -- n ms )
   {: idx:n limit:ms :}
   idx idx s" Mcp-Method: tools/list" limit PREPARE TRANSFER
   idx limit ;

: CALL-EXCHANGE ( n ptr u8 n ms -- n ptr u8 n ms )
   {: idx:n name:ptr nu:n limit:ms :}
   idx s" Mcp-Method: tools/call" limit PREPARE {: h:CURL:handle :}
   h idx name nu NAME-LINE CURL:HEADER+ CHECK
   idx h TRANSFER
   idx name nu limit ;

\ A guarded exchange's code: the row's exchange is let go whether or not it
\ threw, and a throw passes on, so the client serves its next call. A halt
\ passes catch by, leaving the row for CLOSE.
: SETTLE ( n n -- )
   {: idx:n code:n :}
   idx RETIRE
   code 0<> if code throw then ;


\ The id of a response, empty for any other message.
: RESPONSE-ID$ ( JSON-RPC:message -- ptr u8 n )
   MATCH JSON-RPC:message
      success OF 2drop JSON-RPC:ID$ ENDOF
      failure OF 2drop JSON-RPC:ID$ ENDOF
      request OF 2drop 2drop drop s" " ENDOF
      notification OF 2drop 2drop s" " ENDOF
      invalid OF 2drop 2drop s" " ENDOF
   ;MATCH ;

\ An event of an event-stream reply, given the row's region: the first whose
\ data is the response to the client's request is kept in the message buffer,
\ and every other event is passed over.
: EVENT ( ptr u8 ptr u8 n ptr u8 n -- )
   {: base:ptr kind:ptr ku:n data:ptr du:n :}
   base MESSAGE-OFF + CELL-VIEW {: buf:ptr :}
   buf BUF:LEN@ BUF:BLEN>N 0<> if exit then
   base >PEER PEER-CURSOR data du JSON-RPC:>MESSAGE RESPONSE-ID$ REQUEST-ID$ STR=
   if data du buf BUF+ then ;

\ Feeds the whole reply to the decoder and finishes it; answers its operands.
: DECODE ( SSE:decoder ptr u8 n ptr u8 -- SSE:decoder ptr u8 n ptr u8 )
   {: d:SSE:decoder a:ptr u:n base:ptr :}
   d a u base [: EVENT ;] SSE:FEED
   d SSE:FINISH
   d a u base ;

\ The reply's JSON-RPC text: its body, or for an event stream the data of the
\ event that answers the request, E-RESPONSE when no event does or when the
\ stream is not UTF-8. The decoder is closed before this answers or throws.
: MESSAGE$ ( n -- ptr u8 n )
   {: idx:n :}
   idx REPLY-BUF BUF$ {: a:ptr u:n :}
   idx ROW PEER-STREAM @ 0= if a u exit then
   idx MESSAGE-BUF {: buf:ptr :}
   buf BUF:CLEAR
   idx DECODER {: d:SSE:decoder :}
   d a u idx REGION SPAN:$ drop [: DECODE ;] catch {: code:n :} 2drop 2drop
   d SSE:CLOSE
   code SSE:E-UTF8 = if E-RESPONSE throw then
   code 0<> if code throw then
   buf BUF$ dup 0= if E-RESPONSE throw then ;


\ A member of a JSON object text: its text (a string with quotes) and token
\ kind, and whether it is there. Text that is not an object is E-RESPONSE.
: MEMBER ( n ptr u8 n ptr u8 n -- ptr u8 n n bool )
   {: idx:n data:ptr len:n key:ptr ku:n :}
   idx ROW PEER-CURSOR data len JR:INIT
   JR:NEXT JR:T-OBJ <> if JR:CLOSE E-RESPONSE throw then
   key ku JR:FIND-KEY if
      JR:TOKEN {: k:n :} RAW$ {: a:ptr u:n :} JR:CLOSE a u k true
   else JR:CLOSE s" " JR:T-END false then ;


\ The result object's text of a 2xx reply whose message answers the request.
: RESULT$ ( n -- ptr u8 n )
   {: idx:n :}
   idx ROW PEER-STATUS @ {: status:n :}
   status 200 < status 299 > or if E-RESPONSE throw then
   idx MESSAGE$ {: m:ptr mu:n :}
   idx ROW PEER-CURSOR m mu JSON-RPC:>MESSAGE
   MATCH JSON-RPC:message
      success OF {: id value:ptr vu:n :}
         id JSON-RPC:ID$ REQUEST-ID$ STR= 0= if E-RESPONSE throw then
         value vu ENDOF
      request OF 2drop 2drop drop E-RESPONSE throw ENDOF
      notification OF 2drop 2drop E-RESPONSE throw ENDOF
      failure OF 2drop drop E-RESPONSE throw ENDOF
      invalid OF 2drop 2drop E-RESPONSE throw ENDOF
   ;MATCH ;


\ Adds a tools array's members to the list buffer's array.
: GATHER ( n ptr u8 n -- )
   {: idx:n raw:ptr ru:n :}
   idx LIST-BUF {: buf:ptr :}
   raw 1+ ru 2 - TRIM {: a:ptr u:n :}
   u 0= if exit then
   buf BUF:LEN@ BUF:BLEN>N 1 > if 44 buf BUF:APPEND-BYTE then
   a u buf BUF+ ;

1000000 constant NS-PER-MS

\ What is left of a TOOLS limit taken at the MONO-NS reading, rounded down to
\ whole ms; E-TRANSFER when none is. No limit (0), and a negative one that
\ TIMEOUT! refuses, pass through.
: LEFT ( ms n -- ms )
   {: limit:ms start:n :}
   limit MS>N {: total:n :}
   total 0 <= if limit exit then
   TIME:MONO-NS start - NS-PER-MS 1 - + NS-PER-MS / {: spent:n :}
   total spent - dup 0 <= if E-TRANSFER throw then >MS ;

\ One page of tools/list from the cursor, its transfer within ms, the time its
\ TOOLS has left: answers the next page's cursor and true, or an empty text and
\ false after the last.
: PAGE ( n ptr u8 n ms -- ptr u8 n bool )
   {: idx:n next:ptr nu:n limit:ms :}
   idx next nu LIST-REQUEST
   idx limit [: LIST-EXCHANGE ;] catch {: code:n :} 2drop
   idx code SETTLE
   idx RESULT$ {: result:ptr ru:n :}
   idx result ru s" tools" MEMBER 0= if E-RESPONSE throw then
   JR:T-ARR <> if E-RESPONSE throw then
   {: tools:ptr tu:n :}
   idx result ru s" nextCursor" MEMBER {: more:bool :} {: k:n :}
   more if k JR:T-STR <> if E-RESPONSE throw then then
   idx tools tu GATHER
   more ;

public

\ A client of the MCP endpoint at the URL, which is copied. Each exchange, a
\ CALL or one page of TOOLS, runs on a fresh CURL handle; connections are
\ reused through the CURL loop, which the caller has started (CURL:LOOP-START)
\ and keeps running while the client is used. Answers borrow the client's
\ buffers until its next TOOLS, CALL or CLOSE. A throw leaves the client ready
\ for the next call. OPEN that throws holds nothing.
: OPEN ( ptr u8 n -- client )
   {: url:ptr uu:n :}
   REGION-BYTES MEM:BYTES-ALLOC-LEN MEM:ALLOC-SPAN {: region :}
   0 region SPAN:FILL
   region CLAIM {: who:client :}
   who LIVE {: idx:n :}
   idx url uu [: FILL-ROW ;] catch {: code:n :} 2drop drop
   code 0<> if idx RELEASE code throw then
   who ;


\ Ends a client in any state a throw or a TASK:HALT left it in: a transfer
\ still in flight is cancelled and drained, then the handle, reader and buffers
\ go. A caller that must survive a halt keeps the client in its own task
\ storage and closes it in its own finally. E-STATE for a closed client.
: CLOSE ( client -- )
   LIVE {: idx:n :}
   idx RETIRE
   idx RELEASE ;


\ Every tool the server lists, following nextCursor to the last page, the
\ whole call within ms (0: no limit): one JSON array of the tool objects in
\ order. E-TRANSFER when an exchange fails or the pages run past ms,
\ E-RESPONSE for a reply that is no tools/list result.
: TOOLS ( client ms -- ptr u8 n )
   {: who:client limit:ms :}
   TIME:MONO-NS {: start:n :}
   who LIVE {: idx:n :}
   idx LIST-BUF {: buf:ptr :}
   buf BUF:CLEAR
   91 buf BUF:APPEND-BYTE
   s" " begin idx -rot limit start LEFT PAGE while repeat 2drop
   93 buf BUF:APPEND-BYTE
   buf BUF$ ;


\ tools/call of the named tool with the arguments' JSON object, within ms for
\ the whole transfer (0: no limit): answers the result object's JSON text and
\ its isError. E-TRANSFER when the exchange fails or runs past ms, E-RESPONSE
\ for a JSON-RPC error, an unknown tool included, or a reply that is no result.
: CALL ( client ptr u8 n ptr u8 n ms -- ptr u8 n bool )
   {: who:client name:ptr nu:n args:ptr au:n limit:ms :}
   who LIVE {: idx:n :}
   idx name nu args au CALL-REQUEST
   idx name nu limit [: CALL-EXCHANGE ;] catch {: code:n :} 2drop 2drop
   idx code SETTLE
   idx RESULT$ {: result:ptr ru:n :}
   idx result ru s" isError" MEMBER if
      {: k:n :} 2drop
      k JR:T-TRUE <> k JR:T-FALSE <> and if E-RESPONSE throw then
      k JR:T-TRUE =
   else drop 2drop false then
   {: fault:bool :}
   result ru fault ;

;package
