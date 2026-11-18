split-str
\ mcp.f - an MCP server on Habu's HTTP server: the stateless, JSON-response form of
\ streamable HTTP for MCP 2026-07-28 with server/discover, and the 2025-11-25 initialize
\ flow. No session ID, no event stream. Before HTTP:START the host states its name with
\ SERVER-INFO!, any guidance for the model with INSTRUCTIONS!, each tool with ADD-TOOL as
\ the MCP Tool object tools/list shows, annotations and _meta included, and each
\ resource, such as an app's ui:// page, with ADD-RESOURCE. It authenticates each
\ request, validates its Origin (403 when one is present and not allowed) and passes it
\ to HANDLE; a route guarded by bearer tokens is
\ `req res BEARER:ADMIT? if req res MCP:HANDLE then`. HANDLE logs each message it
\ dispatches, a notification included, as one LOG:INFO line `mcp: <method>`, then a
\ tools/call's params.name or a resources/read's params.uri when it is a string,
\ written between its quotes as sent, never the arguments. ANSWER answers one JSON-RPC
\ message that came by another transport into a BUF, as HANDLE answers a 2026-07-28
\ request with no transport headers, its log line included. VISIBLE! hides a tool
\ from a caller its hook refuses; it does not apply to resources, which every caller may list
\ and read. A tools/call of a tool the caller cannot call, unknown or hidden, is -32602,
\ as UNKNOWN-TOOL states.
\ A tool body gets the arguments' object, borrowed for the call, and gives its
\ result once through RESULT, RESULT-META to give the result a _meta object, RESULT-CONTENT
\ to add content blocks, such as an image or an embedded resource, after the result's
\ text, or RESULT-ERROR for a tool error, refused arguments included; bodies may run at
\ once on several HTTP workers. A body that must wait gives its worker back:
\ HOLD takes the call's connection and keeps the call in a row of the held
\ table, and the body returns with no result; later FINISH, on any request
\ worker, runs a quotation as that call's body and sends the response HANDLE
\ would have sent on the kept connection.
\ ADD-TOOL compiles the inputSchema with JSON-SCHEMA:COMPILE, its root typed "object", and
\ a tools/call whose arguments break it is a tool error before the body runs: `arguments`
\ and VALID?'s text, such as `arguments.top: expected a number of at most `500`, found
\ `501`; send a number in that range`. A tool result's JSON is its structuredContent
\ when its output schema's "type" is "object"; any other schema describes that JSON
\ as structuredContent's "data", and tools/list shows that wrapper in the schema's
\ place. The client side, OPEN, TOOLS, CALL and CLOSE, speaks the 2026-07-28 era to one
\ server over CURL and takes a JSON or an event-stream reply. TOKEN! sets the bearer
\ token a client sends, and REQUEST-STOP stops the client from another task.
\ STORAGE CLASS. PROCESS-WIDE: the server info and instructions, tool and resource
\ registries, the input schemas each compiled into an arena of its own kept for the
\ process, and
\ visibility hook, written before HTTP serves; tables indexed by HTTP:WORKER-SLOT holding
\ each request's state; the held table, a row per held call and its id's BUF: a row's
\ phase is read and written under its own lock, and a finishing row's other fields are
\ its finisher's; and the client table, a row per open client, read and written under its
\ own lock.
\ https://modelcontextprotocol.io/specification/2026-07-28/basic/transports/streamable-http
\
\ The client. A client is a row of a process-wide table that grows with the
\ clients open; a free row's record holds an empty region and is reused. The
\ table is read and written only under LOCK, since growth moves it; a row's
\ record, taken from an arena when the row is made, and its region never move.
\ A client's value is the generation OPEN gave its row from a process-wide
\ count, and CLOSE clears the row's generation, so a closed client's value
\ names no row even after another OPEN reuses it. The region holds six BUFs and
\ the SSE decoder's storage. The BUFs are the URL; the request, then, once
\ BODY! has copied it, a header line and the reply's header block; the reply;
\ the tool list TOOLS assembles; the response an event-stream reply carried;
\ and the token TOKEN! set, empty for none. The handle of the exchange in
\ flight, none between exchanges, and the row's run state, which REQUEST-STOP
\ sets from another task (ROW-RUN), are read and written under LOCK. So is a
\ record's region, except that its own client reads it without LOCK: OPEN
\ sets it only on a free row, and only that client's CLOSE empties it.

require src/lib/string.f
require src/lib/span.f
require src/lib/base64.f
require src/lib/json.f
require src/lib/json-rpc.f
require src/lib/json-schema.f
require src/lib/log.f
require src/lib/memory.f
require src/lib/arena.f
require src/lib/utf8-scalar.f
require src/lib/adt/option.f
require src/lib/fmt.f
require src/lib/reason.f
require src/core/rejection.f
require src/lib/byte-buffer.f
require src/lib/task.f
require src/lib/time.f
require src/lib/net/curl.f
require src/lib/net/event-stream.f
require src/lib/net/tcp4.f
require src/lib/net/http.f

package MCP
public
-9480 constant E-FIRST
-9489 constant E-LAST
-9481 s" an MCP word ran out of turn: SERVER-INFO!, INSTRUCTIONS!, ADD-TOOL, ADD-RESOURCE or VISIBLE! while HTTP serves, HANDLE or ANSWER before SERVER-INFO!, ANSWER or FINISH off a request worker, a body that does not give its result exactly once, HOLD outside a body before its result, FINISH of a call not held, or a client word given a closed client"
   ERROR:FAULT E-STATE
-9482 s" ADD-TOOL was given a registered name or ADD-RESOURCE a registered uri, compared decoded"
   ERROR:CODE E-DUPLICATE
-9483 s" a client's request could not be sent or its reply received" ERROR:CODE E-TRANSFER
-9484 s" the server's reply has a status other than 2xx that is neither E-DENIED's nor E-UNAVAILABLE's, is not a JSON-RPC success for the request, or holds a result without its method's shape"
   ERROR:CODE E-RESPONSE
-9485 s" a descriptor, inputSchema, outputSchema, meta, result, content blocks or a page outside what MCP takes: correct it where its reason text says"
   ERROR:CODE E-SCHEMA
-9486 s" the server refused a client's call with 401 or 403" ERROR:CODE E-DENIED
-9487 s" the server answered a status CURL:UNAVAILABLE? accepts" ERROR:CODE E-UNAVAILABLE

\ A client of one server's endpoint, from OPEN until CLOSE.
NEWTYPE client

\ The most tool calls held at once.
256 constant MAX-HELD
\ A tool call HOLD kept: its value names one row of the held table from HOLD
\ until FINISH frees it.
NEWTYPE held
private

-9480 s" a request's params or arguments are not objects (JSON-RPC -32602)" ERROR:CODE E-INPUT

0 constant MODERN            \ 2026-07-28
1 constant LEGACY            \ 2025-11-25

0 constant IDLE
1 constant CALLING           \ a tool's body runs and may give its result
2 constant SENT              \ the response began; a later fault is the host's
3 constant HELD              \ HOLD kept the call, which FINISH answers

0 constant DISCOVER-REPLY
1 constant INIT-REPLY
2 constant LIST-REPLY
3 constant CALL-REPLY
4 constant PING-REPLY
5 constant ERROR-REPLY
6 constant RESOURCES-REPLY
7 constant READ-REPLY
8 constant TEMPLATES-REPLY
9 constant STALE-REPLY       \ an error whose TEXT is the unknown tool's name

-32002 constant RESOURCE-MISSING
-32020 constant HEADER-MISMATCH
-32022 constant UNSUPPORTED-VERSION

TYPED-VARIABLE SERVER-NAME SPAN:span<u8>
TYPED-VARIABLE SERVER-VERSION SPAN:span<u8>
TYPED-VARIABLE INSTRUCTIONS SPAN:span<u8>

\ The registry: a row to each tool, in ADD-TOOL order.
variable TOOL-COUNT
DYNAMIC-BUFFER TOOL-NAME SPAN:span<u8>       \ decoded
DYNAMIC-BUFFER TOOL-TEXT SPAN:span<u8>       \ the descriptor
DYNAMIC-BUFFER TOOL-OUTPUT SPAN:span<u8>     \ the descriptor's outputSchema
DYNAMIC-BUFFER TOOL-WHOLE bool              \ TOOL-OUTPUT's "type" is "object"
DYNAMIC-BUFFER TOOL-INPUT JSON-SCHEMA:schema \ the descriptor's inputSchema
DYNAMIC-BUFFER TOOL-BODY [ ^u8 n -- ]

\ A row to each resource, in ADD-RESOURCE order.
variable RESOURCE-COUNT
DYNAMIC-BUFFER RESOURCE-URI SPAN:span<u8>     \ decoded
DYNAMIC-BUFFER RESOURCE-TEXT SPAN:span<u8>    \ the descriptor
DYNAMIC-BUFFER RESOURCE-PAGE SPAN:span<u8>

: EVERY ( ^u8 n -- bool ) 2drop true ;
TYPED-VARIABLE VISIBILITY [ ^u8 n -- bool ]   \ VISIBLE!'s hook
' EVERY VISIBILITY !

\ ADD-TOOL's and ADD-RESOURCE's: both run before HTTP serves.
TYPED-VARIABLE SCHEMA-READER JSON:cursor

\ A row to each HTTP worker; spans borrow the request's body and headers.
HTTP:MAX-WORKERS TYPED-BUFFER READER JSON:cursor
HTTP:MAX-WORKERS TYPED-BUFFER FROM HTTP:request
HTTP:MAX-WORKERS TYPED-BUFFER TO HTTP:response
\ Where the reply goes: HANDLE's HTTP response in TO, or BUF-W, the writer
\ ANSWER holds open on its caller's BUF.
ENUM sink response buf ;ENUM
HTTP:MAX-WORKERS TYPED-BUFFER REPLY-TO sink
HTTP:MAX-WORKERS TYPED-BUFFER BUF-W JSON:writer
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
HTTP:MAX-WORKERS TYPED-BUFFER META-SPAN SPAN:span<u8>     \ the tool result's _meta, or empty
\ The tool result's content after its text block: content blocks and the commas
\ between them, or empty.
HTTP:MAX-WORKERS TYPED-BUFFER CONTENT-SPAN SPAN:span<u8>
HTTP:MAX-WORKERS TYPED-BUFFER CALLED n                \ the tool a tools/call runs
HTTP:MAX-WORKERS TYPED-BUFFER OPENED n                \ the resource a resources/read reads
HTTP:MAX-WORKERS TYPED-BUFFER ASKED SPAN:span<u8>     \ an unsupported MCP-Protocol-Version
\ A decoded Mcp-Name is no longer than the header block that carried it.
HTTP:MAX-WORKERS HTTP:HEAD-CAP * SPAN-BUFFER: NAMES
HTTP:MAX-WORKERS TYPED-BUFFER HOLDING n               \ the held row of a body at HELD
\ Each worker's BUF, where FINISH writes a held call's response.
HTTP:MAX-WORKERS BUF:HDR-BYTES * BUFFER: FINISHED
\ Each worker's BUF, where a tools/call's arguments' violation is written.
HTTP:MAX-WORKERS BUF:HDR-BYTES * BUFFER: VIOLATIONS

\ The held table: a row is free, kept by HOLD, or finishing while FINISH runs.
\ A kept row holds the call's connection, era and tool, and its id's JSON text
\ in the row's own BUF, which stays for the row's next call.
ENUM keeping free kept finishing ;ENUM

STRUCTURE keep DERIVE addr
   FIELD phase keeping
   FIELD conn TCP4:connection
   FIELD era n
   FIELD tool n
;STRUCTURE

MAX-HELD TYPED-BUFFER KEEPS keep
MAX-HELD BUF:HDR-BYTES * BUFFER: KEEP-IDS
TASK:FACILITY KEEP-LOCK
KEEP-LOCK TASK:FACILITY-INIT

CAST: >HELD ( n -- held )
CAST: HELD>N ( held -- n )

: SLOT ( -- n ) HTTP:WORKER-SLOT ;
: CURSOR ( -- ^JSON:cursor ) SLOT READER ;
: ID$ ( -- ^u8 n ) SLOT ID @ SPAN:$ ;
: METHOD$ ( -- str ) SLOT METHOD @ SPAN:$ ;
: PARAMS$ ( -- str ) SLOT PARAMS @ SPAN:$ ;
: TEXT$ ( -- ^u8 n ) SLOT TEXT @ SPAN:$ ;
: LEGACY? ( -- bool ) SLOT ERA @ LEGACY = ;


\ A value's JSON text at the reader: a container whole, a string with quotes.
: RAW$ ( JSON:reader -- JSON:reader str )
   JSON:READ-TOKEN dup JSON:T-OBJ = over JSON:T-ARR = or if
      drop JSON:READ-VALUE-SPAN$ exit
   then
   JSON:T-STR = if
      JSON:READ-SPAN$ {: a:^ u:n :} a 1- u 2 + exit
   then
   JSON:READ-SPAN$ ;


\ The token kind of a non-empty JSON text's first value.
: KIND ( ^u8 n -- n )
   {: a:^ u:n :}
   CURSOR a u JSON:READ-INIT
   JSON:READ-NEXT {: k:n :}
   JSON:READ-CLOSE k ;

: OBJECT? ( str -- bool ) KIND JSON:T-OBJ = ;


\ A member of an object's JSON text, and whether it is there; text that is not
\ an object, absent params included, is E-INPUT.
: FIELD? ( str str -- str bool )
   {: data:^ len:n key:^ ku:n :}
   len 0= if E-INPUT throw then
   CURSOR data len JSON:READ-INIT
   JSON:READ-NEXT JSON:T-OBJ <> if JSON:READ-CLOSE E-INPUT throw then
   key ku JSON:READ-FIND-KEY if
      RAW$ {: a:^ u:n :} JSON:READ-CLOSE a u true
   else JSON:READ-CLOSE s" " false then ;


\ Whether a JSON text is a string that decodes to the wanted bytes.
: STRING= ( str str -- bool )
   {: raw:^ len:n wanted:^ wu:n :}
   CURSOR raw len JSON:READ-INIT
   JSON:READ-NEXT JSON:T-STR <> if JSON:READ-CLOSE false exit then
   wanted wu JSON:READ-STR-EQ? {: same:bool :}
   JSON:READ-CLOSE same ;

: METHOD= ( str -- bool ) METHOD$ 2swap STRING= ;


: BAD ( n n ^u8 n -- )
   {: status:n code:n why:^ wu:n :}
   status SLOT STATUS !
   code SLOT CODE !
   why wu SPAN:MAKE SLOT TEXT !
   ERROR-REPLY SLOT REPLY ! ;

: GOOD ( n -- ) SLOT REPLY !  200 SLOT STATUS ! ;
: MISMATCH ( -- ) 400 HEADER-MISMATCH s" Header mismatch" BAD ;
: NO-METHOD ( -- ) 404 -32601 s" Method not found" BAD ;
: BAD-INPUT ( -- ) 400 -32602 s" Invalid MCP request" BAD ;
: BAD-ENVELOPE ( -- ) 400 -32600 s" Invalid MCP request" BAD ;
: UNKNOWN-RESOURCE ( -- ) 404 RESOURCE-MISSING s" Resource not found" BAD ;

: SERVER-INFO ( ^JSON:writer -- ^JSON:writer )
   JSON:WRITE-OBJECT-START
   s" name" SERVER-NAME @ SPAN:$ JSON:WRITE-FIELD-S
   JSON:WRITE-COMMA s" version" SERVER-VERSION @ SPAN:$ JSON:WRITE-FIELD-S
   JSON:WRITE-OBJECT-END ;


: INSTRUCTIONS-FIELD ( ^JSON:writer -- ^JSON:writer )
   INSTRUCTIONS @ SPAN:LEN 0= if exit then
   JSON:WRITE-COMMA s" instructions" INSTRUCTIONS @ SPAN:$ JSON:WRITE-FIELD-S ;


\ An object's JSON text without its braces and the space around them.
: MEMBERS ( ^u8 n -- ^u8 n )
   TRIM {: a:^ u:n :}
   a 1+ u 2 - TRIM ;


\ _meta: the members of meta, empty or an object's text, then serverInfo.
: MODERN-META ( ^JSON:writer str -- ^JSON:writer )
   {: meta:^ mu:n :}
   JSON:WRITE-COMMA s" _meta" JSON:WRITE-KEY
   JSON:WRITE-OBJECT-START
   mu 0<> if
      meta mu MEMBERS {: a:^ u:n :}
      u 0<> if a u JSON:WRITE-RAW JSON:WRITE-COMMA then
   then
   s" io.modelcontextprotocol/serverInfo" JSON:WRITE-KEY
   SERVER-INFO JSON:WRITE-OBJECT-END ;


: VERSIONS ( ^JSON:writer -- ^JSON:writer )
   JSON:WRITE-ARRAY-START s" 2026-07-28" JSON:WRITE-STRING
   JSON:WRITE-COMMA s" 2025-11-25" JSON:WRITE-STRING JSON:WRITE-ARRAY-END ;


: CAPABILITIES ( ^JSON:writer -- ^JSON:writer )
   JSON:WRITE-COMMA s" capabilities" JSON:WRITE-KEY
   JSON:WRITE-OBJECT-START s" tools" JSON:WRITE-KEY
   JSON:WRITE-OBJECT-START JSON:WRITE-OBJECT-END
   RESOURCE-COUNT @ 0<> if
      JSON:WRITE-COMMA s" resources" JSON:WRITE-KEY
      JSON:WRITE-OBJECT-START JSON:WRITE-OBJECT-END
   then
   JSON:WRITE-OBJECT-END ;


: PRIVATE-CACHE ( ^JSON:writer -- ^JSON:writer )
   JSON:WRITE-COMMA s" ttlMs" 0 JSON:WRITE-FIELD-U
   JSON:WRITE-COMMA s" cacheScope" s" private" JSON:WRITE-FIELD-S ;


\ https://modelcontextprotocol.io/specification/2026-07-28/server/discover
: DISCOVER-BODY ( ^JSON:writer -- ^JSON:writer )
   JSON:WRITE-OBJECT-START
   s" resultType" s" complete" JSON:WRITE-FIELD-S
   JSON:WRITE-COMMA s" supportedVersions" JSON:WRITE-KEY VERSIONS
   CAPABILITIES s" " MODERN-META INSTRUCTIONS-FIELD PRIVATE-CACHE
   JSON:WRITE-OBJECT-END ;


: INIT-BODY ( ^JSON:writer -- ^JSON:writer )
   JSON:WRITE-OBJECT-START
   s" protocolVersion" s" 2025-11-25" JSON:WRITE-FIELD-S
   CAPABILITIES
   JSON:WRITE-COMMA s" serverInfo" JSON:WRITE-KEY SERVER-INFO
   INSTRUCTIONS-FIELD
   JSON:WRITE-OBJECT-END ;


: OUTPUT-SCHEMA ( ^JSON:writer n -- ^JSON:writer )
   TOOL-OUTPUT @ SPAN:$ {: text:^ tu:n :}
   s" outputSchema" JSON:WRITE-KEY
   JSON:WRITE-OBJECT-START s" type" s" object" JSON:WRITE-FIELD-S
   JSON:WRITE-COMMA s" properties" JSON:WRITE-KEY
   JSON:WRITE-OBJECT-START s" data" text tu JSON:WRITE-FIELD-RAW JSON:WRITE-OBJECT-END
   JSON:WRITE-COMMA s" required" JSON:WRITE-KEY
   JSON:WRITE-ARRAY-START s" data" JSON:WRITE-STRING JSON:WRITE-ARRAY-END
   JSON:WRITE-OBJECT-END ;


: WRITE-MEMBER ( JSON:reader ^JSON:writer n -- JSON:reader )
   {: w idx:n :}
   s" outputSchema" JSON:READ-STR-EQ? {: output:bool :}
   JSON:READ-SPAN$ {: key:^ ku:n :}
   JSON:READ-NEXT drop RAW$ {: value:^ vu:n :}
   output if w idx OUTPUT-SCHEMA drop exit then
   w key 1- ku 2 + JSON:WRITE-RAW s" :" JSON:WRITE-RAW value vu JSON:WRITE-RAW drop ;


: TOOL-ENTRY ( ^JSON:writer n -- ^JSON:writer )
   {: w idx:n :}
   idx TOOL-TEXT @ SPAN:$ {: text:^ tu:n :}
   idx TOOL-WHOLE @ if w text tu JSON:WRITE-RAW exit then
   w JSON:WRITE-OBJECT-START drop
   CURSOR text tu JSON:READ-INIT JSON:READ-NEXT drop JSON:READ-NEXT drop
   w idx WRITE-MEMBER
   begin JSON:READ-NEXT JSON:T-KEY = while
      w JSON:WRITE-COMMA drop  w idx WRITE-MEMBER
   repeat JSON:READ-CLOSE
   w JSON:WRITE-OBJECT-END ;


\ Whether the request's caller may see and call the registered tool.
: SHOWN? ( n -- bool ) TOOL-NAME @ SPAN:$ VISIBILITY @ execute ;


: RESULT-START ( ^JSON:writer -- ^JSON:writer )
   JSON:WRITE-OBJECT-START
   LEGACY? 0= if
      s" resultType" s" complete" JSON:WRITE-FIELD-S JSON:WRITE-COMMA
   then ;

: CACHED-END ( ^JSON:writer -- ^JSON:writer )
   LEGACY? 0= if PRIVATE-CACHE s" " MODERN-META then
   JSON:WRITE-OBJECT-END ;


\ https://modelcontextprotocol.io/specification/2026-07-28/server/tools
: LIST-BODY ( ^JSON:writer -- ^JSON:writer )
   RESULT-START
   s" tools" JSON:WRITE-KEY JSON:WRITE-ARRAY-START
   false TOOL-COUNT @ 0 ?do
      i SHOWN? if
         if JSON:WRITE-COMMA then
         i TOOL-ENTRY true
      then
   loop drop
   JSON:WRITE-ARRAY-END CACHED-END ;


\ https://modelcontextprotocol.io/specification/2025-11-25/server/resources
: RESOURCES-BODY ( ^JSON:writer -- ^JSON:writer )
   RESULT-START
   s" resources" JSON:WRITE-KEY JSON:WRITE-ARRAY-START
   RESOURCE-COUNT @ 0 ?do
      i 0<> if JSON:WRITE-COMMA then
      i RESOURCE-TEXT @ SPAN:$ JSON:WRITE-RAW
   loop
   JSON:WRITE-ARRAY-END CACHED-END ;


\ The resource's text with its descriptor's uri and mimeType, strings ADD-RESOURCE
\ checked, and any _meta.
: READ-BODY ( ^JSON:writer -- ^JSON:writer )
   SLOT OPENED @ {: idx:n :}
   idx RESOURCE-TEXT @ SPAN:$ {: desc:^ du:n :}
   RESULT-START
   s" contents" JSON:WRITE-KEY JSON:WRITE-ARRAY-START JSON:WRITE-OBJECT-START
   s" uri" desc du s" uri" FIELD? drop JSON:WRITE-FIELD-RAW
   JSON:WRITE-COMMA s" mimeType" desc du s" mimeType" FIELD? drop JSON:WRITE-FIELD-RAW
   desc du s" _meta" FIELD? if
      {: meta:^ mu:n :}
      JSON:WRITE-COMMA s" _meta" meta mu JSON:WRITE-FIELD-RAW
   else 2drop then
   JSON:WRITE-COMMA s" text" idx RESOURCE-PAGE @ SPAN:$ JSON:WRITE-FIELD-S
   JSON:WRITE-OBJECT-END JSON:WRITE-ARRAY-END CACHED-END ;


: TEMPLATES-BODY ( ^JSON:writer -- ^JSON:writer )
   RESULT-START s" resourceTemplates" JSON:WRITE-KEY
   JSON:WRITE-ARRAY-START JSON:WRITE-ARRAY-END CACHED-END ;


: CALL-BODY ( ^JSON:writer -- ^JSON:writer )
   JSON:WRITE-OBJECT-START
   LEGACY? 0= if
      s" resultType" s" complete" JSON:WRITE-FIELD-S JSON:WRITE-COMMA
   then
   s" content" JSON:WRITE-KEY JSON:WRITE-ARRAY-START
   JSON:WRITE-OBJECT-START s" type" s" text" JSON:WRITE-FIELD-S
   JSON:WRITE-COMMA s" text" TEXT$ JSON:WRITE-FIELD-S JSON:WRITE-OBJECT-END
   SLOT CONTENT-SPAN @ SPAN:$ {: blocks:^ bu:n :}
   bu 0<> if JSON:WRITE-COMMA blocks bu JSON:WRITE-RAW then
   JSON:WRITE-ARRAY-END
   SLOT FAULT @ 0= if
      JSON:WRITE-COMMA
      SLOT CALLED @ TOOL-WHOLE @ if
         s" structuredContent" TEXT$ JSON:WRITE-FIELD-RAW
      else
         s" structuredContent" JSON:WRITE-KEY
         JSON:WRITE-OBJECT-START s" data" TEXT$ JSON:WRITE-FIELD-RAW JSON:WRITE-OBJECT-END
      then
   then
   JSON:WRITE-COMMA s" isError" SLOT FAULT @ JSON:WRITE-FIELD-BOOL
   SLOT META-SPAN @ SPAN:$ {: meta:^ mu:n :}
   LEGACY? if
      mu 0<> if JSON:WRITE-COMMA s" _meta" meta mu JSON:WRITE-FIELD-RAW then
   else meta mu MODERN-META then
   JSON:WRITE-OBJECT-END ;


: PING-BODY ( ^JSON:writer -- ^JSON:writer )
   JSON:WRITE-OBJECT-START JSON:WRITE-OBJECT-END ;


: RESULT-BODY ( ^JSON:writer -- ^JSON:writer )
   ID$ JSON--RPC-ID:MAKE JSON-RPC:RESULT
   SLOT REPLY @ {: kind:n :}
   kind DISCOVER-REPLY = if DISCOVER-BODY else
   kind INIT-REPLY = if INIT-BODY else
   kind LIST-REPLY = if LIST-BODY else
   kind CALL-REPLY = if CALL-BODY else
   kind RESOURCES-REPLY = if RESOURCES-BODY else
   kind READ-REPLY = if READ-BODY else
   kind TEMPLATES-REPLY = if TEMPLATES-BODY else PING-BODY
   then then then then then then then
   JSON-RPC:END ;


\ -32022's data: the supported versions and the one requested.
: VERSION-DATA ( ^JSON:writer -- ^JSON:writer )
   JSON:WRITE-OBJECT-START s" supported" JSON:WRITE-KEY VERSIONS
   JSON:WRITE-COMMA s" requested" SLOT ASKED @ SPAN:$ JSON:WRITE-FIELD-S
   JSON:WRITE-OBJECT-END ;


\ -32002's data: the uri the request named, which READ-RESOURCE checked.
: URI-DATA ( ^JSON:writer -- ^JSON:writer )
   JSON:WRITE-OBJECT-START
   s" uri" PARAMS$ s" uri" FIELD? drop JSON:WRITE-FIELD-RAW
   JSON:WRITE-OBJECT-END ;


: ID-FIELD ( ^JSON:writer -- ^JSON:writer )
   ID$ {: id:^ iu:n :}
   iu 0= if exit then
   JSON:WRITE-COMMA s" id" id iu JSON:WRITE-FIELD-RAW ;


\ An error's message; a STALE-REPLY's names the tool and the server.
: MESSAGE ( ^JSON:writer -- ^JSON:writer )
   s" message" JSON:WRITE-KEY
   SLOT REPLY @ STALE-REPLY <> if TEXT$ JSON:WRITE-STRING exit then
   JSON:WRITE-STRING-START
   s" Unknown tool: " JSON:WRITE-STRING-PART
   TEXT$ JSON:WRITE-STRING-PART
   s" ; if the client's list of " JSON:WRITE-STRING-PART
   SERVER-NAME @ SPAN:$ JSON:WRITE-STRING-PART
   s" 's tools names it, that list is out of date"
   JSON:WRITE-STRING-PART JSON:WRITE-STRING-END ;


\ An error reply. Both eras' schemas make its id optional and never null, so
\ an unknown id is left out; JSON-RPC:ERROR's null id is JSON-RPC's own rule.
: ERROR-BODY ( ^JSON:writer -- ^JSON:writer )
   JSON:WRITE-OBJECT-START s" jsonrpc" s" 2.0" JSON:WRITE-FIELD-S ID-FIELD
   JSON:WRITE-COMMA s" error" JSON:WRITE-KEY JSON:WRITE-OBJECT-START
   s" code" SLOT CODE @ JSON:WRITE-FIELD-INT
   JSON:WRITE-COMMA MESSAGE
   SLOT CODE @ UNSUPPORTED-VERSION = if
      JSON:WRITE-COMMA s" data" JSON:WRITE-KEY VERSION-DATA
   then
   SLOT CODE @ RESOURCE-MISSING = if
      JSON:WRITE-COMMA s" data" JSON:WRITE-KEY URI-DATA
   then
   JSON:WRITE-OBJECT-END JSON:WRITE-OBJECT-END ;


: RESPONSE-BODY ( ^JSON:writer -- ^JSON:writer )
   SLOT REPLY @ {: kind:n :}
   kind ERROR-REPLY = kind STALE-REPLY = or
   if ERROR-BODY else RESULT-BODY then ;


: HTTP-REPLY ( -- )
   SLOT TO @ {: answer:HTTP:response :}
   answer SLOT STATUS @ HTTP:STATUS!
   answer [: RESPONSE-BODY ;] HTTP:JSON! ;

: BUF-REPLY ( -- ) SLOT BUF-W RESPONSE-BODY drop ;

: SEND ( -- )
   SENT SLOT STAGE !
   SLOT REPLY-TO @ MATCH sink
      response OF HTTP-REPLY ENDOF
      buf OF BUF-REPLY ENDOF
   ;MATCH ;


\ A header's value when it arrived exactly once; otherwise empty and false.
: HEADER$ ( HTTP:request str -- str bool )
   {: asked:HTTP:request name:^ len:n :}
   asked name len HTTP:HEADER-COUNT-OF 1 <> if s" " false exit then
   asked name len HTTP:HEADER-OF$ ;


\ A media type without its parameters.
: MEDIA-ESSENCE ( str -- str )
   {: media:^ size:n :}
   media size 59 INDEX-OF MATCH option
      some OF IDX>N media swap ENDOF
      none OF media size ENDOF
   ;MATCH TRIM ;


\ Whether a comma-separated header value lists the media type.
: ACCEPT-HAS? ( str str -- bool )
   {: value:^ len:n wanted:^ wu:n :}
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
   {: value:^ len:n :}
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
: MCP-ID? ( str -- bool )
   KIND {: k:n :}
   k JSON:T-STR = k JSON:T-INT = or ;


\ A request, or a notification with an empty id: params, when present, are an
\ object.
: CALL? ( str str str -- bool )
   {: id:^ iu:n m:^ mu:n p:^ pu:n :}
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
: RESPONSE? ( JSON-RPC:id str -- bool )
   2drop drop BAD-ENVELOPE false ;


\ The reader's id is the reply's when it is an MCP id; its null id is unknown.
: INVALID? ( n str JSON-RPC:id -- bool )
   {: code:n why:^ wu:n reply:JSON-RPC:id :}
   reply JSON-RPC:ID$ {: id:^ iu:n :}
   id iu MCP-ID? if id iu SPAN:MAKE SLOT ID ! then
   code JSON-RPC:PARSE-ERROR = if 400 -32700 why wu BAD false exit then
   BAD-ENVELOPE false ;


: ENVELOPE? ( str -- bool )
   {: body:^ len:n :}
   CURSOR body len JSON-RPC:>MESSAGE
   MATCH JSON-RPC:message
      request OF {: i m:^ mu:n p:^ pu:n :}
         i JSON-RPC:ID$ m mu p pu CALL? ENDOF
      notification OF {: m:^ mu:n p:^ pu:n :}
         s" " m mu p pu CALL? ENDOF
      success OF RESPONSE? ENDOF
      failure OF RESPONSE? ENDOF
      invalid OF INVALID? ENDOF
   ;MATCH ;


\ A member of params._meta, and whether it is there.
: META ( ^u8 n -- ^u8 n bool )
   {: key:^ ku:n :}
   PARAMS$ dup 0= if false exit then
   s" _meta" FIELD? 0= if false exit then
   key ku FIELD? ;


\ Requests carry params._meta with protocolVersion and clientCapabilities: one
\ missing is -32602 (2026-07-28 basic "Per-request protocol fields"), a version
\ other than the header's -32020.
: MODERN? ( str -- bool )
   {: version:^ vu:n :}
   s" io.modelcontextprotocol/protocolVersion" META 0= if
      2drop 400 -32602 s" Protocol version required" BAD false exit
   then
   version vu STRING= 0= if MISMATCH false exit then
   s" io.modelcontextprotocol/clientCapabilities" META
   if OBJECT? else 2drop false then
   0= if 400 -32602 s" Client capabilities required" BAD false exit then
   true ;


: ERA? ( str -- bool )
   {: value:^ vu:n :}
   value vu s" 2026-07-28" STR= if
      MODERN SLOT ERA ! value vu MODERN? exit
   then
   value vu s" 2025-11-25" STR= if LEGACY SLOT ERA ! true exit then
   value vu SPAN:MAKE SLOT ASKED !
   400 UNSUPPORTED-VERSION s" Unsupported protocol version" BAD false ;


\ The era from one MCP-Protocol-Version: 2026-07-28 also requires MODERN?;
\ 2025-11-25, or no header on initialize, is the legacy lifecycle
\ (https://modelcontextprotocol.io/specification/2025-11-25/basic/lifecycle).
: VERSION? ( HTTP:request -- bool )
   {: asked:HTTP:request :}
   asked s" MCP-Protocol-Version" HTTP:HEADER-COUNT-OF 1 > if MISMATCH false exit then
   asked s" MCP-Protocol-Version" HEADER$ if ERA? exit then
   2drop
   s" initialize" METHOD= if LEGACY SLOT ERA ! true exit then
   MISMATCH false ;


\ An Mcp-Name value, decoded into the worker's row when it is =?base64?...?=.
: DECODED-NAME$ ( str -- str )
   {: header:^ len:n :}
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


\ A modern request's Mcp-Name names the params member at key, a string: the
\ messages are -32602's when the member is absent and when it is no string.
: NAME-HEADER? ( HTTP:request str str str -- bool )
   {: asked:HTTP:request key:^ ku:n absent:^ au:n invalid:^ iu:n :}
   PARAMS$ key ku FIELD? 0= if
      2drop 400 -32602 absent au BAD false exit
   then
   {: name:^ nu:n :}
   name nu KIND JSON:T-STR <> if 400 -32602 invalid iu BAD false exit then
   asked s" Mcp-Name" HEADER$ 0= if 2drop MISMATCH false exit then
   [: DECODED-NAME$ ;] catch {: code:n :}
   code 0<> if
      code NAME-FAULT? 0= if code throw then
      2drop MISMATCH false exit
   then
   name nu 2swap STRING= dup 0= if MISMATCH then ;


\ The modern era's Mcp-Method names the body's method, -32020 otherwise; a
\ tools/call's Mcp-Name names params.name, a resources/read's params.uri.
: HEADERS? ( HTTP:request -- bool )
   {: asked:HTTP:request :}
   LEGACY? if true exit then
   asked s" Mcp-Method" HEADER$ 0= if 2drop MISMATCH false exit then
   METHOD$ 2swap STRING= 0= if MISMATCH false exit then
   s" tools/call" METHOD= if
      asked s" name" s" Tool name required" s" Invalid tool name" NAME-HEADER? exit
   then
   s" resources/read" METHOD= if
      asked s" uri" s" Resource URI required" s" Invalid resource URI" NAME-HEADER? exit
   then
   true ;


\ The registered tool a JSON name text names, else -1.
: TOOL-OF ( str -- n )
   {: raw:^ ru:n :}
   TOOL-COUNT @ 0 ?do
      raw ru i TOOL-NAME @ SPAN:$ STRING= if i unloop exit then
   loop -1 ;


: ARGUMENTS ( -- ^u8 n )
   PARAMS$ s" arguments" FIELD? 0= if 2drop s" {}" exit then
   2dup OBJECT? 0= if E-INPUT throw then ;


\ A JSON string's text between its quotes, as sent, its escapes kept.
: UNQUOTED$ ( str -- str )
   {: a:^ u:n :}
   a 1+ u 2 - ;


128 constant NAME-MAX    \ MCP's longest tool name, in bytes

\ params.name, a string, between its quotes as sent, cut to NAME-MAX bytes at
\ a character's start.
: NAMED$ ( -- str )
   PARAMS$ s" name" FIELD? drop UNQUOTED$
   2dup NAME-MAX UTF8:FIT nip ;


\ -32602 for a tool the caller cannot call, unknown or hidden: `Unknown tool:
\ <name>; if the client's list of <title>'s tools names it, that list is out of
\ date`, <title> serverInfo's name. TEXT borrows the name from the request,
\ and MESSAGE writes the message from it as the reply is written.
: UNKNOWN-TOOL ( -- )
   400 -32602 NAMED$ BAD  STALE-REPLY SLOT REPLY ! ;


\ A held row's id BUF.
: KEEP-ID ( n -- ^n ) BUF:HDR-BYTES * KEEP-IDS + CELL-VIEW ;
: KEEP-UNLOCK ( -- ) KEEP-LOCK TASK:RELEASE ;

\ The text written into the BUF, which grows to hold it.
: TEXT-SET ( str ^n -- )
   {: a:^ u:n buf:^ :}
   buf BUF:LIVE? 0= if buf 64 BUF:N>BLEN BUF:INIT then
   a u BUF:N>BLEN buf BUF:REPLACE ;

: KEEP-FREE? ( n -- bool )
   KEEPS KEEP-PHASE @ MATCH keeping
      free OF true ENDOF
      kept OF false ENDOF
      finishing OF false ENDOF
   ;MATCH ;

\ Under KEEP-LOCK: the first free row, or -1.
: FREE-KEEP ( -- n )
   MAX-HELD 0 ?do i KEEP-FREE? if i unloop exit then loop
   -1 ;

\ Under KEEP-LOCK: a free row kept for the worker's call, its connection
\ taken last, so a throw leaves the row free and the connection the worker's;
\ -1 when every row is kept.
: KEEP-CALL ( -- n )
   FREE-KEEP {: idx:n :}
   idx 0< if -1 exit then
   ID$ idx KEEP-ID TEXT-SET
   SLOT ERA @ idx KEEPS KEEP-ERA !
   SLOT CALLED @ idx KEEPS KEEP-TOOL !
   SLOT FROM @ HTTP:TAKE-OVER 2drop idx KEEPS KEEP-CONN !
   construct keeping kept idx KEEPS KEEP-PHASE !
   idx ;

\ The row's connection closed with no answer.
: CLOSE-KEPT ( n -- )
   KEEPS KEEP-CONN @ TCP4:CLOSE MATCH TCP4:status
      ok OF ENDOF
      failed OF drop ENDOF
   ;MATCH ;

: FREE-KEPT ( n -- )
   {: idx:n :}
   KEEP-LOCK TASK:GET
   construct keeping free idx KEEPS KEEP-PHASE !
   KEEP-UNLOCK ;

: DROP-KEPT ( n -- ) dup CLOSE-KEPT FREE-KEPT ;

\ A body's throw after HOLD closes the connection it took and frees its row.
: BODY-FAULT ( n -- )
   {: code:n :}
   code 0= if exit then
   SLOT STAGE @ HELD = if SLOT HOLDING @ DROP-KEPT then
   code throw ;


: REGISTERED? ( str -- bool )
   {: name:^ nu:n :}
   TOOL-COUNT @ 0 ?do
      name nu i TOOL-NAME @ SPAN:$ STR= if true unloop exit then
   loop false ;


: TOOLS-RESERVE ( n -- )
   {: n:n :}
   n TOOL-NAME-RESERVE
   n TOOL-TEXT-RESERVE
   n TOOL-OUTPUT-RESERVE
   n TOOL-WHOLE-RESERVE
   n TOOL-INPUT-RESERVE
   n TOOL-BODY-RESERVE ;

: RESOURCES-RESERVE ( n -- )
   {: n:n :}
   n RESOURCE-URI-RESERVE
   n RESOURCE-TEXT-RESERVE
   n RESOURCE-PAGE-RESERVE ;

\ E-SCHEMA's text names the text judged (descriptor, inputSchema, outputSchema, meta,
\ result, content or page), then the place in it.

: PUT ( ^u8 ^u8 n -- ^u8 ) {: dst:^ src:^ u:n :}
   src dst u BYTE-COPY  dst u + ;

: SAY-SCOPE ( ^u8 n ^u8 n ^u8 NUM:alloc-byte-len -- )
   drop {: wa:^ wu:n ta:^ tu:n buf:^ :}
   buf wa wu PUT s"  " PUT ta tu PUT drop
   buf wu tu + 1+ E-SCHEMA REASON:THROW ;

\ Throws E-SCHEMA with the name of the text judged, then the text.
: SAY ( ^u8 n ^u8 n -- )
   {: wa:^ wu:n ta:^ tu:n :}
   wa wu ta tu  wu tu + 1+ MEM:BYTES-ALLOC-LEN
   [: SAY-SCOPE ;] MEM:WITH-BYTES ;

\ Rejects the reader's token in the text judged, given the expected text
\ and the fix: READ-REJECT's E-MEMBER text leaves as E-SCHEMA.
: REJECT ( JSON:reader ^u8 n ^u8 n ^u8 n -- )
   {: wa:^ wu:n ea:^ eu:n fa:^ fu:n :}
   [: ea eu fa fu JSON:READ-REJECT ;] catch {: code:n :}
   code JSON:E-MEMBER = if wa wu code REASON:TEXT$ SAY then
   code throw ;

\ Puts the text with the key between its first two backticks, "``".
: PUT-KEYED ( ^u8 ^u8 n ^u8 n -- ^u8 )
   {: dst:^ ta:^ tu:n ka:^ ku:n :}
   ta tu s" ``" FIND-SUB MATCH option
      none OF tu ENDOF
      some OF IDX>N 1+ ENDOF
   ;MATCH {: at:n :}
   dst ta at PUT ka ku PUT ta at + tu at - PUT ;

: KEY-SCOPE ( JSON:reader ^u8 n ^u8 n ^u8 n ^u8 NUM:alloc-byte-len -- JSON:reader )
   drop {: ka:^ ku:n ea:^ eu:n fa:^ fu:n buf:^ :}
   buf ea eu ka ku PUT-KEYED {: fix:^ :}
   fix fa fu ka ku PUT-KEYED drop
   s" descriptor" buf eu ku + fix fu ku + REJECT ;

\ Rejects the reader's token in a descriptor, the key put as PUT-KEYED puts it.
: KEY-REJECT ( JSON:reader ^u8 n ^u8 n ^u8 n -- )
   {: ka:^ ku:n ea:^ eu:n fa:^ fu:n :}
   ka ku ea eu fa fu  eu fu + ku 2 * + MEM:BYTES-ALLOC-LEN
   [: KEY-SCOPE ;] MEM:WITH-BYTES ;

\ A reader's E-JSON-READ-* code leaves as E-SCHEMA, its text after the name of the text
\ judged; E-READ-SOURCE is a caller's wrong source and stays, as every other code does.
: SCHEMA-CODE ( ^u8 n n -- n )
   {: wa:^ wu:n code:n :}
   code E-JSON-READ-FIRST <=  code E-JSON-READ-LAST >=  and
   code JSON:E-READ-SOURCE <> and if
      wa wu code REASON:TEXT$ SAY
   then
   code ;

\ A reader on the text's first token.
: FIRST-TOKEN ( ^u8 n ^JSON:cursor -- JSON:reader )
   {: text:^ u:n reader :}
   reader text u JSON:READ-INIT JSON:READ-NEXT drop ;

\ Whether a JSON text, read whole by reader, is an object; text that is not one
\ JSON value throws the reader's E-JSON-READ-* code.
: OBJECT-TEXT? ( str ^JSON:cursor -- bool )
   {: text:^ u:n reader :}
   reader text u JSON:READ-INIT JSON:READ-NEXT JSON:T-OBJ = {: object:bool :}
   JSON:READ-FINISH object ;


\ Rejects the text's k-th top-level key, counted from 0, as a repeat.
: REPEAT-REJECT ( n ^u8 n ^JSON:cursor ^u8 n -- )
   {: k:n text:^ u:n reader wa:^ wu:n :}
   text u reader FIRST-TOKEN
   k 0 ?do
      JSON:READ-NEXT drop JSON:READ-NEXT drop JSON:READ-SKIP-VALUE
   loop JSON:READ-NEXT drop
   wa wu s" each top-level member once"
   s" remove the repeated member, keeping one" REJECT ;

: SHAPE-IN ( ^u8 n ^JSON:cursor ^u8 n -- )
   {: text:^ u:n reader wa:^ wu:n :}
   text u reader OBJECT-TEXT? 0= if
      text u reader FIRST-TOKEN wa wu
      s" a JSON object" s" write it as one object" REJECT
   then
   text u JSON:READ-REPEAT MATCH option
      none OF ENDOF
      some OF text u reader wa wu REPEAT-REJECT ENDOF
   ;MATCH ;

\ E-SCHEMA unless the text wa wu names is one JSON object with no top-level key repeated
\ once decoded: JSON.parse keeps a repeated key's last value, so a client would read another.
: SHAPE ( ^u8 n ^JSON:cursor ^u8 n -- )
   {: text:^ u:n reader wa:^ wu:n :}
   [: text u reader wa wu SHAPE-IN ;] catch {: code:n :}
   wa wu code SCHEMA-CODE {: fault:n :}
   fault 0<> if fault throw then ;


\ Whether a schema object's "type" is the string "object".
: WHOLE? ( str -- bool )
   {: text:^ u:n :}
   SCHEMA-READER text u JSON:READ-INIT JSON:READ-NEXT drop
   s" type" JSON:READ-FIND-KEY if
      JSON:READ-TOKEN JSON:T-STR = if s" object" JSON:READ-STR-EQ? else false then
   else false then {: whole:bool :}
   JSON:READ-FINISH whole ;


\ The reader on the value of a top-level member of a descriptor SHAPE
\ admitted; an absent member is rejected at the descriptor's `}`.
: MEMBER-AT ( ^u8 n ^u8 n -- JSON:reader )
   {: text:^ tu:n key:^ ku:n :}
   text tu SCHEMA-READER FIRST-TOKEN
   key ku JSON:READ-FIND-KEY 0= if
      key ku s" a member ``" s" add ``" KEY-REJECT
   then ;

: PART ( ^u8 n ^u8 n -- ^u8 n )
   MEMBER-AT RAW$ {: a:^ u:n :} JSON:READ-CLOSE a u ;


: OBJECT-PART ( ^u8 n ^u8 n -- ^u8 n )
   {: text:^ tu:n key:^ ku:n :}
   text tu key ku MEMBER-AT
   JSON:READ-TOKEN JSON:T-OBJ <> if
      key ku s" `` as a JSON object" s" write `` as an object" KEY-REJECT
   then
   RAW$ {: a:^ u:n :} JSON:READ-CLOSE a u ;


\ The reader on a descriptor member's string value.
: STRING-AT ( ^u8 n ^u8 n -- JSON:reader )
   {: text:^ tu:n key:^ ku:n :}
   text tu key ku MEMBER-AT
   JSON:READ-TOKEN JSON:T-STR <> if
      key ku s" `` as a string" s" write `` as a JSON string" KEY-REJECT
   then ;

: REQUIRE-STRING ( ^u8 n ^u8 n -- )
   STRING-AT JSON:READ-CLOSE ;


\ Whether the reader's token is the JSON string "object".
: OBJECT-STRING? ( JSON:reader -- JSON:reader bool )
   JSON:READ-TOKEN JSON:T-STR = if
      s" object" JSON:READ-STR-EQ?
   else false then ;

\ Rejects an inputSchema whose root `type` is not "object", as MCP's Tool
\ type requires; an absent `type` at the root's `}`.
: OBJECT-ROOT ( ^u8 n -- )
   SCHEMA-READER FIRST-TOKEN
   s" type" JSON:READ-FIND-KEY if
      OBJECT-STRING? if JSON:READ-CLOSE else
         s" inputSchema" S\" `type` as `\"object\"`"
         S\" write `type` as `\"object\"`" REJECT
      then
   else
      s" inputSchema" s" a member `type`"
      S\" add `\"type\":\"object\"`" REJECT
   then ;

\ A throw may leave row idx on the released arena: ADD-TOOL has not counted it.
: INPUT-SCHEMA ( ^u8 n n -- )
   {: in:^ iu:n idx:n :}
   ARENA:NEW {: ar:^ :}
   [: in iu ar JSON-SCHEMA:COMPILE idx TOOL-INPUT !  in iu OBJECT-ROOT ;] catch
   {: code:n :}
   code REASON:TEXT$ {: ta:^ tu:n :}
   code 0<> if ar ARENA:RELEASE then
   code JSON-SCHEMA:E-SCHEMA = if s" inputSchema" ta tu SAY then
   code 0<> if code throw then ;


\ The offset of the first byte UTF8:NEXT answers raw-byte for, else -1.
: BAD-BYTE ( ^u8 n -- n ) {: a:^ u:n :}
   0 begin dup u < while
      a u rot UTF8:NEXT MATCH UTF8:scalar-step
         scalar OF nip ENDOF
         raw-byte OF nip 1- exit ENDOF
      ;MATCH
   repeat drop -1 ;

\ The longest page rejection: two numbers of at most 20 digits and its
\ literals.
128 constant PAGE-TEXT-LEN

: PAGE-SCOPE ( ^u8 n ^u8 NUM:alloc-byte-len -- )
   drop {: page:^ at:n buf:^ :}
   page page at + 1 1 0 REJECTION:POSITION drop {: line:n col:n :}
   buf s" page " PUT line FMT:INT$ PUT s" :" PUT col FMT:INT$ PUT
   s" : expected UTF-8 text, found byte $" PUT
   page at + c@ 2 FMT:HEX-WIDTH$ PUT
   s" ; encode the page as UTF-8" PUT
   buf - {: tu:n :}
   buf tu E-SCHEMA REASON:THROW ;

: REQUIRE-UTF8 ( ^u8 n -- ) {: page:^ pu:n :}
   page pu BAD-BYTE {: at:n :}
   at 0 < if exit then
   page at PAGE-TEXT-LEN MEM:BYTES-ALLOC-LEN
   [: PAGE-SCOPE ;] MEM:WITH-BYTES ;


: REGISTERED-URI? ( str -- bool )
   {: uri:^ uu:n :}
   RESOURCE-COUNT @ 0 ?do
      uri uu i RESOURCE-URI @ SPAN:$ STR= if true unloop exit then
   loop false ;


: DECODE-SCOPE ( JSON:reader n ^u8 NUM:alloc-byte-len -- JSON:reader SPAN:span<u8> )
   drop {: ru:n room:^ :}
   room ru JSON:READ-STR {: u:n :}
   u MEM:BYTES-ALLOC-LEN MEM:ALLOC-SPAN {: name :}
   room name SPAN:$ drop u BYTE-COPY  name ;

\ The storage is kept for the process, as the registry is; the descriptor passed SHAPE.
: DECODED-MEMBER ( str str [ str -- bool ] -- SPAN:span<u8> )
   {: text:^ tu:n key:^ ku:n registered? :}
   text tu key ku STRING-AT JSON:READ-SPAN$ nip {: ru:n :}
   ru 0= if
      key ku s" `` as a non-empty string" s" give `` a value" KEY-REJECT
   then
   ru ru MEM:BYTES-ALLOC-LEN [: DECODE-SCOPE ;] MEM:WITH-BYTES {: name :}
   JSON:READ-CLOSE
   name SPAN:$ registered? execute if name MEM:FREE-SPAN E-DUPLICATE throw then
   name ;


\ On a content block's open: E-SCHEMA unless it has a string "type" and no
\ second one, which a client would read instead; the reader moves past its close.
: BLOCK ( JSON:reader -- JSON:reader )
   JSON:READ-TOKEN JSON:T-OBJ <> if
      s" content" s" a content block object"
      s" write each block as an object" REJECT
   then
   s" type" JSON:READ-FIND-KEY 0= if
      s" content" s" a member `type`" s" give the block its `type`" REJECT
   then
   JSON:READ-TOKEN JSON:T-STR <> if
      s" content" s" `type` as a string"
      s" write `type` as a JSON string" REJECT
   then
   s" type" JSON:READ-FIND-KEY if
      s" content" s" one member `type`" s" remove the repeated `type`" REJECT
   then ;


: BLOCKS-IN ( str -- str )
   {: text:^ u:n :}
   CURSOR text u JSON:READ-INIT
   JSON:READ-NEXT JSON:T-ARR <> if
      s" content" s" a JSON array of content blocks"
      s" write the blocks in an array" REJECT
   then
   JSON:READ-NEXT JSON:T-ARR-END = if JSON:READ-FINISH s" " exit then
   JSON:READ-SPAN$ drop {: first:^ :}
   begin BLOCK JSON:READ-NEXT JSON:T-ARR-END = until
   JSON:READ-SPAN$ drop {: close:^ :}
   JSON:READ-FINISH first close first - ;


\ The objects of a JSON array of MCP content blocks, from the first's open
\ through the space before the array's close; empty for an empty array.
: BLOCKS ( ^u8 n -- ^u8 n )
   [: BLOCKS-IN ;] catch {: code:n :}
   code 0= if exit then
   2drop s" content" code SCHEMA-CODE throw ;


\ A result that is not one JSON object is rejected when the tool's
\ outputSchema has type object; text that is not JSON throws the reader's code.
: RESULT-SHAPE ( ^u8 n -- ) {: text:^ u:n :}
   text u CURSOR OBJECT-TEXT? if exit then
   SLOT CALLED @ TOOL-WHOLE @ 0= if exit then
   text u CURSOR FIRST-TOKEN s" result"
   s" a JSON object, as the tool's outputSchema has type `object`"
   s" answer one JSON object" REJECT ;


\ Rejects meta naming serverInfo at that key; meta passed SHAPE.
: NO-SERVER-INFO ( ^u8 n -- ) {: meta:^ mu:n :}
   meta mu CURSOR FIRST-TOKEN
   begin JSON:READ-NEXT JSON:T-KEY = while
      s" io.modelcontextprotocol/serverInfo" JSON:READ-STR-EQ? if
         s" meta" s" no member `io.modelcontextprotocol/serverInfo`"
         s" remove it: the server writes its own" REJECT
      then
      JSON:READ-NEXT drop JSON:READ-SKIP-VALUE
   repeat JSON:READ-CLOSE ;


\ Bodies run on several workers at once, so meta and blocks are read by the worker's CURSOR.
: RESULT! ( str str str bool -- )
   {: text:^ u:n meta:^ mu:n extra:^ eu:n fault:bool :}
   SLOT STAGE @ CALLING <> if E-STATE throw then
   fault 0= if text u RESULT-SHAPE then
   mu 0<> if
      meta mu CURSOR s" meta" SHAPE
      meta mu NO-SERVER-INFO
   then
   extra eu BLOCKS {: blocks:^ bu:n :}
   text u SPAN:MAKE SLOT TEXT !
   meta mu SPAN:MAKE SLOT META-SPAN !
   blocks bu SPAN:MAKE SLOT CONTENT-SPAN !
   fault SLOT FAULT !
   CALL-REPLY GOOD SEND ;

: TOOL-ERROR ( ^u8 n -- ) s" " s" []" true RESULT! ;

\ The worker's BUF in a table of BUF headers, made live the first time.
: WORKER-BUF ( ^u8 -- ^n )
   {: table:^ :}
   SLOT BUF:HDR-BYTES * table + CELL-VIEW {: buf:^ :}
   buf BUF:LIVE? 0= if buf 1024 BUF:N>BLEN BUF:INIT then
   buf ;

\ Whether the arguments satisfy the tool's inputSchema; false gives the tool
\ error `arguments` and the violation, which is not logged.
: ADMITTED? ( ^u8 n n -- bool )
   {: args:^ au:n idx:n :}
   VIOLATIONS WORKER-BUF {: buf:^ :}
   s" arguments" BUF:N>BLEN buf BUF:REPLACE
   args au idx TOOL-INPUT @ buf JSON-SCHEMA:VALID?
   dup if exit then
   buf BUF:SPAN$ BUF:BLEN>N TOOL-ERROR ;


\ The body gives its result once, through RESULT, RESULT-META, RESULT-CONTENT or
\ RESULT-ERROR, before it returns, or HOLDs the call.
: RUN-TOOL ( -- )
   PARAMS$ s" name" FIELD? 0= if E-INPUT throw then
   2dup KIND JSON:T-STR <> if E-INPUT throw then
   TOOL-OF {: idx:n :}
   idx 0< if UNKNOWN-TOOL SEND exit then
   idx SHOWN? 0= if UNKNOWN-TOOL SEND exit then
   ARGUMENTS {: args:^ au:n :}
   idx SLOT CALLED !
   CALLING SLOT STAGE !
   args au idx ADMITTED? 0= if exit then
   [: args au idx TOOL-BODY @ execute ;] catch BODY-FAULT
   SLOT STAGE @ CALLING = if E-STATE throw then ;


\ The registered resource a JSON uri text names, else -1.
: RESOURCE-OF ( str -- n )
   {: raw:^ ru:n :}
   RESOURCE-COUNT @ 0 ?do
      raw ru i RESOURCE-URI @ SPAN:$ STRING= if i unloop exit then
   loop -1 ;


\ params.uri not a string is E-INPUT, which HANDLE answers -32602.
: READ-RESOURCE ( -- )
   PARAMS$ s" uri" FIELD? 0= if E-INPUT throw then
   2dup KIND JSON:T-STR <> if E-INPUT throw then
   RESOURCE-OF {: idx:n :}
   idx 0< if UNKNOWN-RESOURCE SEND exit then
   idx SLOT OPENED !
   READ-REPLY GOOD SEND ;


: INITIALIZE ( -- )
   LEGACY? if INIT-REPLY GOOD else NO-METHOD then SEND ;

: DISCOVER ( -- )
   LEGACY? if NO-METHOD else DISCOVER-REPLY GOOD then SEND ;

\ Over HTTP a 2025-11-25 notification is accepted, 202 with no body, before
\ any method runs (2025-11-25 transports); 2026-07-28 takes only requests.
: HTTP-NOTIFIED ( -- )
   LEGACY? if SLOT TO @ 202 HTTP:STATUS! exit then
   400 -32600 s" Request id required" BAD SEND ;

\ ANSWER's notification has no reply, as JSON-RPC's never has.
: NOTIFIED ( -- )
   SLOT REPLY-TO @ MATCH sink
      response OF HTTP-NOTIFIED ENDOF
      buf OF ENDOF
   ;MATCH ;


\ The params member that names what the method acts on; empty for none.
: TARGET-KEY$ ( -- str )
   s" tools/call" METHOD= if s" name" exit then
   s" resources/read" METHOD= if s" uri" exit then
   s" " ;


: TARGET+ ( -- )
   TARGET-KEY$ {: key:^ ku:n :}
   ku 0= PARAMS$ nip 0= or if exit then
   \ JSON-RPC's params may be an array, which FIELD? refuses.
   PARAMS$ KIND JSON:T-OBJ <> if exit then
   PARAMS$ key ku FIELD? 0= if 2drop exit then
   2dup KIND JSON:T-STR <> if 2drop exit then
   s"  " LOG:TEXT+  UNQUOTED$ LOG:TEXT+ ;


: LOGGED ( -- )
   LOG:INFO s" mcp" LOG:LINE
   METHOD$ UNQUOTED$ LOG:TEXT+  TARGET+
   LOG:;LINE ;


: DISPATCH ( -- )
   LOGGED
   ID$ nip 0= if NOTIFIED exit then
   s" initialize" METHOD= if INITIALIZE exit then
   s" server/discover" METHOD= if DISCOVER exit then
   s" tools/list" METHOD= if LIST-REPLY GOOD SEND exit then
   s" ping" METHOD= LEGACY? and if PING-REPLY GOOD SEND exit then
   s" tools/call" METHOD= if RUN-TOOL exit then
   s" resources/list" METHOD= if RESOURCES-REPLY GOOD SEND exit then
   s" resources/read" METHOD= if READ-RESOURCE exit then
   s" resources/templates/list" METHOD= if TEMPLATES-REPLY GOOD SEND exit then
   NO-METHOD SEND ;


: PROCESS ( -- )
   SLOT FROM @ {: asked:HTTP:request :}
   asked TRANSPORT? 0= if SEND exit then
   asked HTTP:BODY$ ENVELOPE? 0= if SEND exit then
   asked VERSION? 0= if SEND exit then
   asked HEADERS? 0= if SEND exit then
   DISPATCH ;


\ The reply's sink and a fresh message on the worker's slot.
: OPENED-TO ( sink -- )
   SLOT REPLY-TO !
   IDLE SLOT STAGE !
   s" " SPAN:MAKE SLOT ID ! ;


\ A message's caught code: E-INPUT before the reply began is -32602; any
\ other fault is rethrown.
: FAULTED ( n -- )
   {: code:n :}
   code 0= if exit then
   SLOT STAGE @ IDLE <> code E-INPUT <> or if code throw then
   BAD-INPUT SEND ;


\ WORKER-SLOT's E-HTTP-HANDLE, on a task that is no request worker, as E-STATE.
: ON-WORKER ( -- )
   [: HTTP:WORKER-SLOT drop ;] catch {: code:n :}
   code 0= if exit then
   code E-HTTP-HANDLE = if E-STATE throw then
   code throw ;


\ The worker's BUF for a held call's response.
: FINISH-BUF ( -- ^n ) FINISHED WORKER-BUF ;

\ Under KEEP-LOCK: whether the held row was kept, now finishing.
: CLAIM-ROW ( n -- bool )
   {: idx:n :}
   idx KEEPS KEEP-PHASE @ MATCH keeping
      free OF false ENDOF
      kept OF true ENDOF
      finishing OF false ENDOF
   ;MATCH {: ok:bool :}
   ok if construct keeping finishing idx KEEPS KEEP-PHASE ! then
   ok ;

\ The worker's slot set as for the held call's body, its reply written into
\ FINISH-BUF.
: RESUME ( n -- )
   {: idx:n :}
   construct sink buf OPENED-TO
   idx KEEP-ID BUF:SPAN$ BUF:BLEN>N SPAN:MAKE SLOT ID !
   idx KEEPS KEEP-ERA @ SLOT ERA !
   idx KEEPS KEEP-TOOL @ SLOT CALLED !
   CALLING SLOT STAGE ! ;

\ The quotation run as the body; E-STATE when it gives no result.
: COMPOSE ( [ -- ] -- )
   {: q :}
   SLOT BUF-W FINISH-BUF JSON:WRITE-OPEN-BUF drop
   q [: SLOT BUF-W JSON:WRITE-CLOSE ;] finally
   SLOT STAGE @ SENT <> if E-STATE throw then ;

\ The response sent on the row's connection, which a throw closes first.
: REPLY-HELD ( n [ -- ] -- )
   {: idx:n q :}
   idx RESUME
   [: q COMPOSE ;] catch {: code:n :}
   code 0<> if idx CLOSE-KEPT code throw then
   idx KEEPS KEEP-CONN @ 200 s" application/json"
   FINISH-BUF BUF:SPAN$ BUF:BLEN>N HTTP:REPLY drop ;


\ The message's envelope checked, then dispatched; the text is kept for catch.
: CARRIED ( ^u8 n -- ^u8 n )
   2dup ENVELOPE? if DISPATCH else SEND then ;


\ The message answered into BUF-W, the writer ANSWER holds open on the
\ caller's BUF.
: BUF-ANSWER ( str -- )
   [: CARRIED ;] catch {: code:n :} 2drop
   code FAULTED ;

public

\ The server's name and version in serverInfo. The texts are borrowed and
\ outlive the server, as literals do. E-STATE while HTTP serves.
: SERVER-INFO! ( str str -- )
   {: name:^ nu:n version:^ vu:n :}
   HTTP:RUNNING? if E-STATE throw then
   name nu SPAN:MAKE SERVER-NAME !
   version vu SPAN:MAKE SERVER-VERSION ! ;


\ Guidance for the model on using this server: "instructions" in initialize and
\ server/discover results, left out while the text is empty. The text is
\ borrowed and outlives the server, as literals do. E-STATE while HTTP serves.
: INSTRUCTIONS! ( ^u8 n -- )
   HTTP:RUNNING? if E-STATE throw then
   SPAN:MAKE INSTRUCTIONS ! ;


\ False hides the named tool from HANDLE's or ANSWER's caller as an unknown one; the
\ default admits every tool; it may ask BEARER:CLAIM$. _meta.ui.visibility ["app"]
\ keeps a tool listed but widget-only.
: VISIBLE! ( [ ^u8 n -- bool ] -- )
   HTTP:RUNNING? if E-STATE throw then
   VISIBILITY ! ;


\ A borrowed MCP Tool descriptor, which must outlive the server, and its body. It needs a
\ non-empty string name and object schemas; an outputSchema typed "object" needs object results.
: ADD-TOOL ( ^u8 n [ ^u8 n -- ] -- )
   {: text:^ tu:n body :}
   HTTP:RUNNING? if E-STATE throw then
   text tu SCHEMA-READER s" descriptor" SHAPE
   text tu s" inputSchema" OBJECT-PART {: input:^ iu:n :}
   text tu s" outputSchema" PART {: output:^ ou:n :}
   output ou SCHEMA-READER s" outputSchema" SHAPE
   output ou WHOLE? {: whole:bool :}
   TOOL-COUNT @ {: idx:n :}
   idx 1+ TOOLS-RESERVE
   text tu s" name" [: REGISTERED? ;] DECODED-MEMBER {: name :}
   [: input iu idx INPUT-SCHEMA ;] catch {: code:n :}
   code 0<> if name MEM:FREE-SPAN code throw then
   name idx TOOL-NAME !
   text tu SPAN:MAKE idx TOOL-TEXT !
   output ou SPAN:MAKE idx TOOL-OUTPUT !
   whole idx TOOL-WHOLE !
   body idx TOOL-BODY !
   idx 1+ TOOL-COUNT ! ;


\ A borrowed MCP Resource descriptor, with a non-empty string uri and string name and mimeType,
\ and the UTF-8 page resources/read answers; both must outlive the server.
: ADD-RESOURCE ( ^u8 n ^u8 n -- )
   {: text:^ tu:n page:^ pu:n :}
   HTTP:RUNNING? if E-STATE throw then
   text tu SCHEMA-READER s" descriptor" SHAPE
   text tu s" name" REQUIRE-STRING
   text tu s" mimeType" REQUIRE-STRING
   page pu REQUIRE-UTF8
   RESOURCE-COUNT @ {: idx:n :}
   idx 1+ RESOURCES-RESERVE
   text tu s" uri" [: REGISTERED-URI? ;] DECODED-MEMBER {: uri :}
   uri idx RESOURCE-URI !
   text tu SPAN:MAKE idx RESOURCE-TEXT !
   page pu SPAN:MAKE idx RESOURCE-PAGE !
   idx 1+ RESOURCE-COUNT ! ;


\ A body's result: one JSON value. E-STATE outside a body or after its result;
\ else the reader's E-JSON-READ-* code or E-SCHEMA, which HANDLE passes to the host.
: RESULT ( ^u8 n -- ) s" " s" []" false RESULT! ;

\ RESULT, plus the result's _meta object, which reaches the host's widget and not the
\ model (OpenAI Apps SDK); empty meta adds none, and E-SCHEMA names the meta it refuses.
: RESULT-META ( ^u8 n ^u8 n -- ) s" []" false RESULT! ;


\ RESULT, plus a JSON array of MCP content blocks, such as an image or an embedded resource,
\ that follow the result's text block in content as given; E-SCHEMA unless blocks is one
\ JSON array of objects, each with one string "type".
: RESULT-CONTENT ( ^u8 n ^u8 n -- )
   {: text:^ u:n extra:^ eu:n :}
   text u s" " extra eu false RESULT! ;


\ A body's tool error: the message as text content, isError true.
: RESULT-ERROR ( ^u8 n -- ) TOOL-ERROR ;


\ In a body before its result. True: the call's connection is taken and the
\ call kept, with its id, era and tool, until FINISH; the body returns with no
\ result, and a throw from it closes the connection and frees the call. False,
\ nothing kept, under ANSWER or when MAX-HELD calls are held: the body gives
\ its result as usual. Once the body has handed the held value on, it must not
\ throw: FINISH may have run by then and the row be another call's.
: HOLD ( -- held bool )
   ON-WORKER
   SLOT STAGE @ CALLING <> if E-STATE throw then
   SLOT REPLY-TO @ MATCH sink
      response OF false ENDOF
      buf OF true ENDOF
   ;MATCH if 0 >HELD false exit then
   KEEP-LOCK TASK:GET
   [: KEEP-CALL ;] [: KEEP-UNLOCK ;] finally {: idx:n :}
   idx 0< if 0 >HELD false exit then
   idx SLOT HOLDING !
   HELD SLOT STAGE !
   idx >HELD true ;


\ The held call's row, below MAX-HELD, by which a host keys rows of its own.
: HELD-SLOT ( held -- n ) HELD>N ;


\ On a request worker, not inside HANDLE or ANSWER: the quotation runs as
\ the held call's body, in its era, and gives its result once through RESULT,
\ RESULT-META, RESULT-CONTENT or RESULT-ERROR. The response HANDLE would have
\ sent goes out with HTTP:REPLY, status 200, which closes the connection; a
\ peer that left gets nothing. A quotation that gives no result is E-STATE,
\ and that or its throw comes after the connection is closed. The row is free
\ again on every way out.
: FINISH ( held [ -- ] -- )
   {: h q :}
   ON-WORKER
   h HELD>N {: idx:n :}
   KEEP-LOCK TASK:GET
   [: idx CLAIM-ROW ;] [: KEEP-UNLOCK ;] finally
   0= if E-STATE throw then
   [: idx q REPLY-HELD ;] [: idx FREE-KEPT ;] finally ;


\ Answers one MCP request whose authentication and Origin the host has checked.
\ A malformed envelope is -32600, missing or malformed params -32602, invalid
\ JSON -32700. Any other fault, a tool body's included, is rethrown to the
\ host. E-STATE before SERVER-INFO!.
: HANDLE ( HTTP:request HTTP:response -- )
   {: asked:HTTP:request answer:HTTP:response :}
   SERVER-NAME @ SPAN:LEN 0= if E-STATE throw then
   asked SLOT FROM !
   answer SLOT TO !
   construct sink response OPENED-TO
   [: PROCESS ;] catch FAULTED ;


\ Answers one JSON-RPC message whose caller the host has authenticated and
\ which came by a transport other than HTTP, on a request worker: as HANDLE
\ answers a 2026-07-28 request, with no transport, version or header checks.
\ ANSWER clears the BUF and writes the response's JSON text there, nothing for
\ a notification; after a throw its text is no reply to send. Any fault but
\ HANDLE's JSON-RPC errors is rethrown, a tool body's included.
\ The VISIBLE! hook is called with no HTTP request, so it judges by what the
\ host set for the message's caller: BEARER:CLAIM$ speaks only for a request
\ the worker serves. E-STATE before SERVER-INFO! and off a request worker.
: ANSWER ( ^u8 n ^a -- )
   SERVER-NAME @ SPAN:LEN 0= if E-STATE throw then
   ON-WORKER
   construct sink buf OPENED-TO
   MODERN SLOT ERA !
   SLOT BUF-W swap BYTE-VIEW CELL-VIEW JSON:WRITE-OPEN-BUF drop
   [: BUF-ANSWER ;] [: SLOT BUF-W JSON:WRITE-CLOSE ;] finally ;


private

CAST: >CLIENT ( n -- client )
CAST: CLIENT>N ( client -- n )

\ A row's record, kept by the row for its next client: the cursor parses a
\ reply and the writer builds a request; status is the HTTP status the last
\ exchange ended with, and stream whether its reply was an event stream; region
\ is the client's memory, empty for a free row.
STRUCTURE peer DERIVE addr
   FIELD cursor JSON:cursor
   FIELD writer JSON:writer
   FIELD status CURL:http-status
   FIELD stream bool
   FIELD region SPAN:span<u8>
;STRUCTURE

ARENA PEERS peer
\ The rows' arena, made by the first GROW so a saved image maps none; null until
\ then, and never released: the rows live as long as the process.
TYPED-VARIABLE RECORDS ^ARENA:arena

$1000 constant FIRST-CAP
0 constant URL-OFF
URL-OFF BUF:HDR-BYTES + constant REQUEST-OFF
REQUEST-OFF BUF:HDR-BYTES + constant REPLY-OFF
REPLY-OFF BUF:HDR-BYTES + constant LIST-OFF
LIST-OFF BUF:HDR-BYTES + constant MESSAGE-OFF
MESSAGE-OFF BUF:HDR-BYTES + constant TOKEN-OFF
TOKEN-OFF BUF:HDR-BYTES + constant DECODER-OFF
DECODER-OFF SSE:STORAGE-BYTES + constant REGION-BYTES

\ No task is live while this file loads, so the lock is made ready here.
TASK:FACILITY LOCK
LOCK TASK:FACILITY-INIT
variable ROW-COUNT
variable GENERATION                 \ the last generation OPEN gave a row
DYNAMIC-BUFFER ROW-PEER ^peer
DYNAMIC-BUFFER ROW-HANDLE option<CURL:handle>
\ A row's run state, changed only under LOCK: CLAIM makes it running and
\ REQUEST-STOP stopped, which only a reuse of the row ends.
ENUM run running stopped ;ENUM
DYNAMIC-BUFFER ROW-RUN run
DYNAMIC-BUFFER ROW-GEN n            \ an open row's generation, 0 for a free one

: ROW ( n -- ^peer ) LOCK TASK:GET ROW-PEER @ LOCK TASK:RELEASE ;
: REGION ( n -- SPAN:span<u8> ) ROW PEER-REGION @ ;
: HANDLE! ( option<CURL:handle> n -- ) LOCK TASK:GET ROW-HANDLE ! LOCK TASK:RELEASE ;

: STOPPED? ( run -- bool )
   MATCH run
      running OF false ENDOF
      stopped OF true ENDOF
   ;MATCH ;

\ Whether REQUEST-STOP has stopped the row.
: STOP@ ( n -- bool )
   LOCK TASK:GET ROW-RUN @ STOPPED? LOCK TASK:RELEASE ;

\ Throws E-TRANSFER, sending nothing, once REQUEST-STOP has stopped the row.
: REFUSE-STOPPED ( n -- )
   {: idx:n :}
   LOCK TASK:GET
   idx ROW-RUN @ STOPPED? if LOCK TASK:RELEASE E-TRANSFER throw then
   LOCK TASK:RELEASE ;

\ The row's handle, left none in the same hold, so no other task can reach a
\ handle RETIRE is about to free.
: HANDLE-TAKE ( n -- option<CURL:handle> )
   {: idx:n :}
   LOCK TASK:GET
   idx ROW-HANDLE @
   OPTION:NONE idx ROW-HANDLE !
   LOCK TASK:RELEASE ;

: BUF-AT ( n n -- ^n )
   {: idx:n off:n :}
   idx REGION off BUF:HDR-BYTES SPAN:SUB SPAN:$ drop CELL-VIEW ;

: URL-BUF ( n -- ^n ) URL-OFF BUF-AT ;
: REQUEST-BUF ( n -- ^n ) REQUEST-OFF BUF-AT ;
: REPLY-BUF ( n -- ^n ) REPLY-OFF BUF-AT ;
: LIST-BUF ( n -- ^n ) LIST-OFF BUF-AT ;
: MESSAGE-BUF ( n -- ^n ) MESSAGE-OFF BUF-AT ;
: TOKEN-BUF ( n -- ^n ) TOKEN-OFF BUF-AT ;

: DECODER ( n -- SSE:decoder )
   REGION DECODER-OFF SSE:STORAGE-BYTES SPAN:SUB SPAN:$ {: a:^ u:n :}
   a CELL-VIEW u SSE:OPEN ;

: BUF$ ( ^n -- ^u8 n ) BUF:SPAN$ BUF:BLEN>N ;
: BUF+ ( str ^n -- ) {: a:^ u:n buf:^ :} a u BUF:N>BLEN buf BUF:APPEND-SPAN ;


\ The first free row, else -1; under LOCK.
: FREE-ROW ( -- n )
   ROW-COUNT @ 0 ?do
      i ROW-PEER @ PEER-REGION @ SPAN:LEN 0= if i unloop exit then
   loop -1 ;

\ A free row at the table's end; under LOCK.
: GROW ( -- )
   ROW-COUNT @ {: count:n :}
   count 1+ ROW-PEER-RESERVE
   count 1+ ROW-HANDLE-RESERVE
   count 1+ ROW-RUN-RESERVE
   count 1+ ROW-GEN-RESERVE
   RECORDS @ NULL-PTR = if ARENA:NEW RECORDS ! then
   RECORDS @ PEERS-NEW {: rec:^ :}
   s" " SPAN:MAKE rec PEER-REGION !
   rec count ROW-PEER !
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
   region idx ROW-PEER @ PEER-REGION !
   OPTION:NONE idx ROW-HANDLE !
   construct run running idx ROW-RUN !
   gen idx ROW-GEN !
   LOCK TASK:RELEASE
   gen >CLIENT ;

\ The row holding the generation, else -1; a free row's 0 is no generation.
\ Under LOCK.
: GEN-ROW ( n -- n )
   {: gen:n :}
   gen 1 < if -1 exit then
   ROW-COUNT @ 0 ?do
      i ROW-GEN @ gen = if i unloop exit then
   loop -1 ;

\ An open client's row, LOCK held: E-STATE, LOCK released, unless a row holds
\ the client's generation.
: HELD-ROW ( client -- n )
   CLIENT>N {: gen:n :}
   LOCK TASK:GET
   gen GEN-ROW {: idx:n :}
   idx 0< if LOCK TASK:RELEASE E-STATE throw then
   idx ;

\ An open client's row: E-STATE unless a row holds the client's generation.
: LIVE ( client -- n ) HELD-ROW LOCK TASK:RELEASE ;

: RELEASE ( n -- )
   {: idx:n :}
   idx URL-BUF BUF:DISPOSE
   idx REQUEST-BUF BUF:DISPOSE
   idx REPLY-BUF BUF:DISPOSE
   idx LIST-BUF BUF:DISPOSE
   idx MESSAGE-BUF BUF:DISPOSE
   idx TOKEN-BUF BUF:DISPOSE
   idx REGION {: region :}
   LOCK TASK:GET
   s" " SPAN:MAKE idx ROW-PEER @ PEER-REGION !
   0 idx ROW-GEN !
   LOCK TASK:RELEASE
   region MEM:FREE-SPAN ;

\ Readies a claimed row's buffers and copies the URL; answers its operands.
: FILL-ROW ( n str -- n str )
   {: idx:n url:^ uu:n :}
   idx URL-BUF FIRST-CAP BUF:N>BLEN BUF:INIT
   idx REQUEST-BUF FIRST-CAP BUF:N>BLEN BUF:INIT
   idx REPLY-BUF FIRST-CAP BUF:N>BLEN BUF:INIT
   idx LIST-BUF FIRST-CAP BUF:N>BLEN BUF:INIT
   idx MESSAGE-BUF FIRST-CAP BUF:N>BLEN BUF:INIT
   idx TOKEN-BUF FIRST-CAP BUF:N>BLEN BUF:INIT
   url uu idx URL-BUF BUF+
   idx url uu ;


: SCHEME-LEN ( str -- n )
   {: a:^ u:n :}
   u 8 < 0= if a 8 s" https://" STR=CI if 8 exit then then
   u 7 < 0= if a 7 s" http://" STR=CI if 7 exit then then
   0 ;

: AUTHORITY-BYTE? ( n -- bool )
   {: b:n :}
   b [char] / <> b [char] ? <> and b [char] # <> and ;

\ The URL's host and port, its userinfo cut; empty when an @ follows the
\ authority, where a password holding /, ? or # may have ended it early.
: HOST$ ( str -- str )
   {: a:^ u:n :}
   a u SCHEME-LEN {: from:n :}
   from begin
      dup u < if dup a + c@ AUTHORITY-BYTE? else false then
   while 1+ repeat {: end:n :}
   a end + u end - s" @" CONTAINS? if a 0 exit then
   from end from ?do a i + c@ [char] @ = if drop i 1+ then loop {: start:n :}
   a start + end start - ;

\ Curl's failure on the row's exchange, logged with the row's host, unless
\ REQUEST-STOP stopped the row: then the program ended it.
: CURL-FAULT ( CURL:code n -- )
   {: rc:CURL:code idx:n :}
   idx STOP@ if E-TRANSFER throw then
   idx URL-BUF BUF$ HOST$ {: host:^ hu:n :}
   LOG:ERROR s" mcp" LOG:LINE  s" exchange" LOG:TEXT+
   hu 0<> if s"  with " LOG:TEXT+  host hu LOG:TEXT+ then
   s"  failed: curl " LOG:TEXT+  rc CURL:CODE>N LOG:INT+
   s" : " LOG:TEXT+  rc CURL:ERROR$ LOG:TEXT+
   LOG:;LINE
   E-TRANSFER throw ;

: BAD-REPLY ( str n n -- )
   {: what:^ wu:n idx:n code:n :}
   idx URL-BUF BUF$ HOST$ {: host:^ hu:n :}
   idx REPLY-BUF BUF$ {: body:^ bu:n :}
   LOG:ERROR s" mcp" LOG:LINE  s" reply" LOG:TEXT+
   hu 0<> if s"  from " LOG:TEXT+  host hu LOG:TEXT+ then
   s"  " LOG:TEXT+  what wu LOG:TEXT+
   s" : status " LOG:TEXT+  idx ROW PEER-STATUS @ CURL:HTTP-STATUS>N LOG:INT+
   bu 0<> if s"  body: " LOG:TEXT+  body bu LOG:TEXT+ then
   LOG:;LINE
   code throw ;

: CHECK ( CURL:status n -- )
   {: idx:n :}
   MATCH CURL:status
      ok OF ENDOF
      failed OF idx CURL-FAULT ENDOF
   ;MATCH ;


\ Room in a BUF past the bytes it holds.
: ROOM ( ^n -- ^u8 n )
   {: buf:^ :}
   buf BUF$ {: a:^ u:n :}
   a u + buf BUF:CAP@ BUF:BLEN>N u - ;


\ Collects the end of a transfer CURL:CANCEL has stopped.
: DRAIN ( n CURL:handle -- )
   {: idx:n h:CURL:handle :}
   idx REPLY-BUF {: buf:^ :}
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
   idx HANDLE-TAKE MATCH option
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
   idx ROW PEER-CURSOR JSON:READ-DISPOSE ;


: META-PARAM ( ^JSON:writer -- ^JSON:writer )
   s" _meta" JSON:WRITE-KEY JSON:WRITE-OBJECT-START
   s" io.modelcontextprotocol/protocolVersion" s" 2026-07-28" JSON:WRITE-FIELD-S
   JSON:WRITE-COMMA s" io.modelcontextprotocol/clientCapabilities" JSON:WRITE-KEY
   JSON:WRITE-OBJECT-START JSON:WRITE-OBJECT-END
   JSON:WRITE-OBJECT-END ;

\ The id of every request a client sends, as its JSON text.
: REQUEST-ID$ ( -- str ) s" 1" ;

\ A request with the request id and the method, up to the params' members.
: REQUEST-START ( ^JSON:writer str -- ^JSON:writer )
   {: m:^ mu:n :}
   JSON:WRITE-OBJECT-START s" jsonrpc" s" 2.0" JSON:WRITE-FIELD-S
   JSON:WRITE-COMMA s" id" REQUEST-ID$ JSON:WRITE-FIELD-RAW
   JSON:WRITE-COMMA s" method" m mu JSON:WRITE-FIELD-S
   JSON:WRITE-COMMA s" params" JSON:WRITE-KEY JSON:WRITE-OBJECT-START ;

: REQUEST-END ( ^JSON:writer -- ^JSON:writer )
   META-PARAM JSON:WRITE-OBJECT-END JSON:WRITE-OBJECT-END ;

: WRITER ( n -- ^JSON:writer )
   {: idx:n :}
   idx ROW PEER-WRITER idx REQUEST-BUF JSON:WRITE-OPEN-BUF ;

\ tools/list from the cursor's JSON string, or from the start when it is empty.
: LIST-REQUEST ( n str -- )
   {: idx:n next:^ nu:n :}
   idx WRITER s" tools/list" REQUEST-START
   nu 0<> if s" cursor" next nu JSON:WRITE-FIELD-RAW JSON:WRITE-COMMA then
   REQUEST-END JSON:WRITE-CLOSE ;

: CALL-REQUEST ( n str str -- )
   {: idx:n name:^ nu:n args:^ au:n :}
   idx WRITER s" tools/call" REQUEST-START
   s" name" name nu JSON:WRITE-FIELD-S JSON:WRITE-COMMA
   s" arguments" args au JSON:WRITE-FIELD-RAW JSON:WRITE-COMMA
   REQUEST-END JSON:WRITE-CLOSE ;


: PLAIN-NAME? ( str -- bool )
   {: a:^ u:n :}
   u 0 ?do
      a i + c@ dup $21 < swap $7E > or if false unloop exit then
   loop true ;

\ The Mcp-Name line, built in the request buffer: the name as it is when every
\ byte is visible ASCII, else =?base64?...?=.
: NAME-LINE ( n str -- str )
   {: idx:n name:^ nu:n :}
   idx REQUEST-BUF {: buf:^ :}
   buf BUF:CLEAR
   s" Mcp-Name: " buf BUF+
   name nu PLAIN-NAME? if name nu buf BUF+ else
      s" =?base64?" buf BUF+
      name nu buf nu 2 + 3 / 4 * BUF:N>BLEN BUF:GROW BASE64:ENCODE drop
      s" ?=" buf BUF+
   then
   buf BUF$ ;

\ The Authorization line carrying the row's token, built in the request buffer.
: BEARER-LINE ( n -- str )
   {: idx:n :}
   idx REQUEST-BUF {: buf:^ :}
   buf BUF:CLEAR
   s" Authorization: Bearer " buf BUF+
   idx TOKEN-BUF BUF$ buf BUF+
   buf BUF$ ;


\ A fresh handle, held by the row, posting the request buffer to the URL; a
\ handle a halt left is retired first. E-TRANSFER once REQUEST-STOP stopped the row.
: PREPARE ( n ^u8 n ms -- CURL:handle )
   {: idx:n line:^ lu:n limit:ms :}
   idx RETIRE
   idx STOP@ if E-TRANSFER throw then
   CURL:INIT
   MATCH CURL:init-result
      ready OF ENDOF
      failed OF idx CURL-FAULT ENDOF
   ;MATCH {: h:CURL:handle :}
   h OPTION:SOME idx HANDLE!
   h idx URL-BUF BUF$ CURL:URL! idx CHECK
   h idx REQUEST-BUF BUF$ CURL:BODY! idx CHECK
   idx TOKEN-BUF BUF$ nip 0<> if h idx BEARER-LINE CURL:HEADER+ idx CHECK then
   h limit CURL:TIMEOUT! idx CHECK
   h s" Content-Type: application/json" CURL:HEADER+ idx CHECK
   h s" Accept: application/json, text/event-stream" CURL:HEADER+ idx CHECK
   h s" MCP-Protocol-Version: 2026-07-28" CURL:HEADER+ idx CHECK
   h line lu CURL:HEADER+ idx CHECK
   h ;


\ The finished reply's header block, in the request buffer, which grows to it.
: HEAD$ ( n CURL:handle -- str )
   {: idx:n h:CURL:handle :}
   idx REQUEST-BUF {: buf:^ :}
   buf BUF:CLEAR
   begin
      buf BUF:SPAN$ drop {: at:^ :}
      h at buf BUF:CAP@ BUF:BLEN>N >LEN CURL:HEADERS
      MATCH CURL:header-result
         complete OF LEN>N at swap buf BUF+ true ENDOF
         truncated OF LEN>N BUF:N>BLEN buf swap BUF:ENSURE false ENDOF
      ;MATCH
   until
   buf BUF$ ;

\ Whether a header line is Content-Type naming text/event-stream.
: EVENT-TYPE? ( str -- bool )
   {: a:^ u:n :}
   a u 58 INDEX-OF MATCH option
      none OF -1 ENDOF
      some OF IDX>N ENDOF
   ;MATCH {: at:n :}
   at 0< if false exit then
   a at TRIM s" Content-Type" STR=CI 0= if false exit then
   a at + 1+ u at - 1- MEDIA-ESSENCE s" text/event-stream" STR=CI ;

\ Whether the finished reply is an event stream, by its header block's lines.
: STREAM? ( n CURL:handle -- bool )
   HEAD$ {: a:^ u:n :}
   0 begin dup u < while
      {: at:n :}
      a at + u at - 10 INDEX-OF MATCH option
         none OF u at - ENDOF
         some OF IDX>N ENDOF
      ;MATCH {: len:n :}
      a at + len EVENT-TYPE? if true exit then
      at len + 1+
   repeat drop false ;

\ The state is read and the transfer started in one LOCK hold: a REQUEST-STOP
\ before it is refused here, one after it finds the transfer for CANCEL-ROW.
: QUEUE ( n CURL:handle -- CURL:status )
   {: idx:n h:CURL:handle :}
   LOCK TASK:GET
   idx ROW-RUN @ STOPPED? if LOCK TASK:RELEASE E-TRANSFER throw then
   [: h CURL:STREAM-START ;] [: LOCK TASK:RELEASE ;] finally ;


\ Starts the transfer and receives the whole reply into the reply buffer,
\ which grows with it: NEXT writes each chunk into the room past the bytes
\ received, and appending the chunk from where it lies keeps it. NEXT follows
\ STREAM-START with no PAUSE between, so NEXT reads a pending halt and gives
\ the transfer up.
: TRANSFER ( n CURL:handle -- )
   {: idx:n h:CURL:handle :}
   idx REPLY-BUF {: buf:^ :}
   buf BUF:CLEAR
   idx h QUEUE idx CHECK
   begin
      buf buf BUF:LEN@ BUF:BLEN>N 1+ BUF:N>BLEN BUF:ENSURE
      buf ROOM {: room:^ ru:n :}
      h room ru >LEN CURL:NEXT
      MATCH CURL:chunk-result
         chunk OF nip LEN>N room swap buf BUF+ false ENDOF
         response OF idx ROW PEER-STATUS ! true ENDOF
         failed OF idx CURL-FAULT ENDOF
      ;MATCH
   until
   idx h STREAM? idx ROW PEER-STREAM ! ;

: LIST-EXCHANGE ( n ms -- n ms )
   {: idx:n limit:ms :}
   idx idx s" Mcp-Method: tools/list" limit PREPARE TRANSFER
   idx limit ;

: CALL-EXCHANGE ( n str ms -- n str ms )
   {: idx:n name:^ nu:n limit:ms :}
   idx s" Mcp-Method: tools/call" limit PREPARE {: h:CURL:handle :}
   h idx name nu NAME-LINE CURL:HEADER+ idx CHECK
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
: RESPONSE-ID$ ( JSON-RPC:message -- str )
   MATCH JSON-RPC:message
      success OF 2drop JSON-RPC:ID$ ENDOF
      failure OF 2drop JSON-RPC:ID$ ENDOF
      request OF 2drop 2drop drop s" " ENDOF
      notification OF 2drop 2drop s" " ENDOF
      invalid OF 2drop 2drop s" " ENDOF
   ;MATCH ;

\ An event of an event-stream reply, given the row's record: the first whose
\ data is the response to the client's request is kept in the message buffer,
\ and every other event is passed over.
: EVENT ( ^peer ^u8 n ^u8 n -- )
   {: rec:^ kind:^ ku:n data:^ du:n :}
   rec PEER-REGION @ SPAN:$ drop MESSAGE-OFF + CELL-VIEW {: buf:^ :}
   buf BUF:LEN@ BUF:BLEN>N 0<> if exit then
   rec PEER-CURSOR data du JSON-RPC:>MESSAGE RESPONSE-ID$ REQUEST-ID$ STR=
   if data du buf BUF+ then ;

\ Feeds the whole reply to the decoder and finishes it; answers its operands.
: DECODE ( SSE:decoder ^u8 n ^peer -- SSE:decoder ^u8 n ^peer )
   {: d:SSE:decoder a:^ u:n rec:^ :}
   d a u rec [: EVENT ;] SSE:FEED
   d SSE:FINISH
   d a u rec ;

\ The reply's JSON-RPC text: its body, or the data of the stream's event that
\ answers the request, else a logged E-RESPONSE; the decoder is always closed.
: MESSAGE$ ( n -- str )
   {: idx:n :}
   idx REPLY-BUF BUF$ {: a:^ u:n :}
   idx ROW PEER-STREAM @ 0= if a u exit then
   idx MESSAGE-BUF {: buf:^ :}
   buf BUF:CLEAR
   idx DECODER {: d:SSE:decoder :}
   d a u idx ROW [: DECODE ;] catch {: code:n :} 2drop 2drop
   d SSE:CLOSE
   code SSE:E-UTF8 = if
      s" is an event stream that is not UTF-8" idx E-RESPONSE BAD-REPLY
   then
   code 0<> if code throw then
   buf BUF$ dup 0= if
      s" has no event answering the request" idx E-RESPONSE BAD-REPLY
   then ;


\ A member of a JSON object text: its text (a string with quotes), token kind
\ and whether it is there; text that is not an object is a logged E-RESPONSE.
: MEMBER ( n ^u8 n ^u8 n -- ^u8 n n bool )
   {: idx:n data:^ len:n key:^ ku:n :}
   idx ROW PEER-CURSOR data len JSON:READ-INIT
   JSON:READ-NEXT JSON:T-OBJ <> if
      JSON:READ-CLOSE
      s" has a result that is not a JSON object" idx E-RESPONSE BAD-REPLY
   then
   key ku JSON:READ-FIND-KEY if
      JSON:READ-TOKEN {: k:n :} RAW$ {: a:^ u:n :} JSON:READ-CLOSE a u k true
   else JSON:READ-CLOSE s" " JSON:T-END false then ;


\ The result text of a 2xx reply answering the request, else a logged E-DENIED
\ (401, 403), E-UNAVAILABLE (CURL:UNAVAILABLE?) or E-RESPONSE (any other).
: RESULT$ ( n -- str )
   {: idx:n :}
   idx ROW PEER-STATUS @ {: status:CURL:http-status :}
   status CURL:HTTP-STATUS>N {: code:n :}
   code 401 = code 403 = or if
      s" refused the client" idx E-DENIED BAD-REPLY
   then
   status CURL:UNAVAILABLE? if
      s" reports the server unavailable" idx E-UNAVAILABLE BAD-REPLY
   then
   code 200 < code 299 > or if
      s" is not a success" idx E-RESPONSE BAD-REPLY
   then
   idx MESSAGE$ {: m:^ mu:n :}
   idx ROW PEER-CURSOR m mu JSON-RPC:>MESSAGE
   MATCH JSON-RPC:message
      success OF {: id value:^ vu:n :}
         id JSON-RPC:ID$ REQUEST-ID$ STR= 0= if
            s" answers another request" idx E-RESPONSE BAD-REPLY
         then
         value vu ENDOF
      request OF 2drop 2drop drop
         s" is a request, not a response" idx E-RESPONSE BAD-REPLY ENDOF
      notification OF 2drop 2drop
         s" is a notification, not a response" idx E-RESPONSE BAD-REPLY ENDOF
      failure OF 2drop drop
         s" is a JSON-RPC error" idx E-RESPONSE BAD-REPLY ENDOF
      invalid OF 2drop 2drop
         s" is not JSON-RPC" idx E-RESPONSE BAD-REPLY ENDOF
   ;MATCH ;


: GATHER ( n ^u8 n -- )
   {: idx:n raw:^ ru:n :}
   idx LIST-BUF {: buf:^ :}
   raw ru MEMBERS {: a:^ u:n :}
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
: PAGE ( n ^u8 n ms -- ^u8 n bool )
   {: idx:n next:^ nu:n limit:ms :}
   idx next nu LIST-REQUEST
   idx limit [: LIST-EXCHANGE ;] catch {: code:n :} 2drop
   idx code SETTLE
   idx RESULT$ {: result:^ ru:n :}
   idx result ru s" tools" MEMBER 0= if
      s" has no tools member" idx E-RESPONSE BAD-REPLY
   then
   JSON:T-ARR <> if
      s" has tools that are not an array" idx E-RESPONSE BAD-REPLY
   then
   {: tools:^ tu:n :}
   idx result ru s" nextCursor" MEMBER {: more:bool :} {: k:n :}
   more if k JSON:T-STR <> if
      s" has a nextCursor that is not a string" idx E-RESPONSE BAD-REPLY
   then then
   idx tools tu GATHER
   more ;

\ Cancels the row's transfer in flight; under LOCK. Answers CURL:CANCEL's
\ throw code, 0 when there was no handle.
: CANCEL-ROW ( n -- n )
   {: idx:n :}
   idx ROW-HANDLE @ MATCH option
      none OF 0 ENDOF
      some OF [: dup CURL:CANCEL ;] catch nip ENDOF
   ;MATCH ;

\ CANCEL-ROW's code passed on; CURL's E-STATE means no transfer was in flight,
\ not started or its end taken.
: STOP-CODE ( n -- )
   {: code:n :}
   code 0= code CURL:E-STATE = or if exit then
   code throw ;

public

\ A client of the MCP endpoint at the URL, which is copied. Each exchange, a
\ CALL or one page of TOOLS, runs on a fresh CURL handle; connections are
\ reused through the CURL loop, which the caller has started (CURL:LOOP-START)
\ and keeps running while the client is used. Answers borrow the client's
\ buffers until its next TOOLS, CALL or CLOSE. A throw leaves the client ready
\ for the next call. OPEN that throws holds nothing.
: OPEN ( ^u8 n -- client )
   {: url:^ uu:n :}
   REGION-BYTES MEM:BYTES-ALLOC-LEN MEM:ALLOC-SPAN {: region :}
   0 region SPAN:FILL
   region CLAIM {: who:client :}
   who LIVE {: idx:n :}
   idx url uu [: FILL-ROW ;] catch {: code:n :} 2drop drop
   code 0<> if idx RELEASE code throw then
   who ;


\ Ends a client in any state a throw or a TASK:HALT left it in: a transfer
\ still in flight is cancelled and drained, then the handle, reader and buffers
\ go, the token and the request buffer, which carries the Authorization line,
\ wiped first. A caller that must survive a halt keeps the client in its own
\ task storage and closes it in its own finally. E-STATE for a closed client.
: CLOSE ( client -- )
   LIVE {: idx:n :}
   idx RETIRE
   idx TOKEN-BUF BUF:WIPE
   idx REQUEST-BUF BUF:WIPE
   idx RELEASE ;


\ The token the client sends as `Authorization: Bearer <token>` on every
\ exchange until the next TOKEN! or CLOSE; the text is copied, the token it
\ replaces wiped, and an empty one sends none. A token holding CR or LF makes
\ each exchange throw CURL:E-OPERAND. E-STATE for a closed client.
: TOKEN! ( client ^u8 n -- )
   {: who:client token:^ tu:n :}
   who LIVE TOKEN-BUF {: buf:^ :}
   buf BUF:WIPE
   token tu buf BUF+ ;


\ Every tool the server lists, following nextCursor to the last page, as one JSON
\ array in order, the whole call within ms (0: no limit); E-TRANSFER past ms.
: TOOLS ( client ms -- ^u8 n )
   {: who:client limit:ms :}
   TIME:MONO-NS {: start:n :}
   who LIVE {: idx:n :}
   idx REFUSE-STOPPED
   idx LIST-BUF {: buf:^ :}
   buf BUF:CLEAR
   91 buf BUF:APPEND-BYTE
   s" " begin idx -rot limit start LEFT PAGE while repeat 2drop
   93 buf BUF:APPEND-BYTE
   buf BUF$ ;


\ tools/call of the named tool with the arguments' JSON object, within ms for the
\ whole transfer (0: no limit): the result object's JSON text and its isError.
: CALL ( client ^u8 n ^u8 n ms -- ^u8 n bool )
   {: who:client name:^ nu:n args:^ au:n limit:ms :}
   who LIVE {: idx:n :}
   idx REFUSE-STOPPED
   idx name nu args au CALL-REQUEST
   idx name nu limit [: CALL-EXCHANGE ;] catch {: code:n :} 2drop 2drop
   idx code SETTLE
   idx RESULT$ {: result:^ ru:n :}
   idx result ru s" isError" MEMBER if
      {: k:n :} 2drop
      k JSON:T-TRUE <> k JSON:T-FALSE <> and if
         s" has an isError that is not true or false" idx E-RESPONSE BAD-REPLY
      then
      k JSON:T-TRUE =
   else drop 2drop false then
   {: fault:bool :}
   result ru fault ;


\ From any task, stops the client until CLOSE: a TOOLS or CALL whose transfer has not
\ ended throws E-TRANSFER, a later one at once, sending nothing. E-STATE once closed.
: REQUEST-STOP ( client -- )
   HELD-ROW {: idx:n :}
   construct run stopped idx ROW-RUN !
   idx CANCEL-ROW LOCK TASK:RELEASE STOP-CODE ;

;package
