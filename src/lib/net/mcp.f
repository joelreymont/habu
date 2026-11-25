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
\ written between its quotes as sent, never the arguments, without LOG; with it,
\ LOG's line after the answer. ANSWER answers one JSON-RPC
\ message that came by another transport into a vector, as HANDLE answers a 2026-07-28
\ request with no transport headers, its log line included. VISIBLE! hides a tool
\ from a caller its hook refuses; it does not apply to resources, which every caller may list
\ and read. A tools/call of a tool the caller cannot call, unknown or hidden, is -32602,
\ as UNKNOWN-TOOL states.
\ A tool body gets the arguments' object, borrowed for the call, and gives its
\ result once through RESULT, RESULT-META to give the result a _meta object, RESULT-CONTENT
\ to give the result's content blocks, or RESULT-ERROR for a tool error, refused arguments
\ included; bodies may run at once on several HTTP workers. A body that must wait gives its
\ worker back: HOLD takes the call's connection and keeps the call in a row of the held
\ table, and the body returns with no result; later FINISH, on any request
\ worker, runs a quotation as that call's body and sends the response HANDLE
\ would have sent on the kept connection.
\ ADD-TOOL compiles the inputSchema with JSON-SCHEMA:COMPILE, its root typed
\ "object", and a tools/call whose arguments break it is a tool error before
\ the body runs, its text `arguments` and VALID?'s. A tool result's JSON goes
\ only in structuredContent, as itself when its output schema's "type" is
\ "object"; any other schema describes that JSON as structuredContent's
\ "data", and tools/list shows that wrapper in the schema's place. The client
\ side, OPEN, TOOLS, CALL and CLOSE, speaks the 2026-07-28 era to one server
\ over CURL and takes a JSON or an event-stream reply. TOKEN! sets the bearer
\ token a client sends, and REQUEST-STOP stops the client from another task.
\ STORAGE CLASS. PROCESS-WIDE: the server info and instructions, tool and resource
\ registries, the input schemas each compiled into an arena of its own kept for the
\ process, and the visibility hook, written before HTTP serves; tables indexed by
\ HTTP:WORKER-SLOT holding each request's state; the held table, a row per held call and
\ its id's and its holding request id's BUFs: a row's phase is read and written under its
\ own lock, and a finishing row's other fields are its finisher's; and the client table,
\ a row per open client, read and written under its own lock.
\ https://modelcontextprotocol.io/specification/2026-07-28/basic/transports/streamable-http
\
\ The client. A client is a row of a process-wide table that grows with the
\ clients open; a free row's record holds an empty region and is reused. The
\ table is read and written only under LOCK, since growth moves it; a row's
\ record, taken from an arena when the row is made, and its region never move.
\ A client's value is the generation OPEN gave its row from a process-wide
\ count, and CLOSE clears the row's generation, so a closed client's value
\ names no row even after another OPEN reuses it. The region holds five BUFs:
\ the URL; the reply; the tool list TOOLS assembles; the response an
\ event-stream reply carried; and the token TOKEN! set, empty for none. The
\ record's request vector holds the request, then, once BODY! has copied it, a
\ header line, and is made and deleted with the region's BUFs. The handle of
\ the exchange in flight, none between exchanges, and the row's run state,
\ which REQUEST-STOP sets from another task (ROW-RUN), are read and written
\ under LOCK. So is a record's region, except that its own client reads it
\ without LOCK: OPEN sets it only on a free row, and only that client's CLOSE
\ empties it.

require src/lib/string.f
require src/lib/string-option.f
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
require src/lib/reason.f
require src/core/rejection.f
require src/lib/byte-buffer.f
require src/lib/heap.f
require src/lib/vector.f
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
-9481 s" an MCP word ran out of turn: SERVER-INFO!, INSTRUCTIONS!, LOG, ADD-TOOL, ADD-RESOURCE or VISIBLE! while HTTP serves, HANDLE or ANSWER before SERVER-INFO!, ANSWER or FINISH off a request worker, a body that does not give its result exactly once, HOLD outside a body before its result, FINISH of a call not held, or a client word given a closed client"
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

\ Thrown only through REASON, its text the why FAULTED answers.
-9480 s" input a client sent that MCP's params rules refuse (JSON-RPC -32602 with its reason)" ERROR:CODE E-INPUT

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

variable LOGGING                              \ LOG's: nonzero once it ran

\ ADD-TOOL's and ADD-RESOURCE's: both run before HTTP serves.
TYPED-VARIABLE SCHEMA-READER JSON:cursor

\ A row to each HTTP worker; spans borrow the request's body and headers.
HTTP:MAX-WORKERS TYPED-BUFFER READER JSON:cursor
HTTP:MAX-WORKERS TYPED-BUFFER FROM HTTP:request
HTTP:MAX-WORKERS TYPED-BUFFER TO HTTP:response
\ Where the reply goes: HANDLE's HTTP response in TO, or ANSWER-W, the writer
\ ANSWER holds open on its caller's vector.
ENUM sink response vector ;ENUM
HTTP:MAX-WORKERS TYPED-BUFFER REPLY-TO sink
HTTP:MAX-WORKERS TYPED-BUFFER ANSWER-W JSON:writer
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
\ The content of a result that is not a tool error: content blocks and the commas
\ between them, or empty.
HTTP:MAX-WORKERS TYPED-BUFFER CONTENT-SPAN SPAN:span<u8>
HTTP:MAX-WORKERS TYPED-BUFFER CALLED n                \ the tool a tools/call runs
HTTP:MAX-WORKERS TYPED-BUFFER OPENED n                \ the resource a resources/read reads
HTTP:MAX-WORKERS TYPED-BUFFER ASKED SPAN:span<u8>     \ an unsupported MCP-Protocol-Version
\ A decoded Mcp-Name is no longer than the header block that carried it.
HTTP:MAX-WORKERS HTTP:HEAD-CAP * SPAN-BUFFER: NAMES
HTTP:MAX-WORKERS TYPED-BUFFER HOLDING n               \ the held row of a body at HELD
\ Each worker's vector, where FINISH writes a held call's response; NULL-PTR
\ until its first FINISH.
HTTP:MAX-WORKERS TYPED-BUFFER FINISHED ^VEC:vector<u8>
\ Each worker's vector, where a tools/call's arguments' violation is written;
\ NULL-PTR until its first call.
HTTP:MAX-WORKERS TYPED-BUFFER VIOLATIONS ^VEC:vector<u8>
\ Each worker's BUF, where a tool error's text is kept for LOG's line.
HTTP:MAX-WORKERS BUF:HDR-BYTES * BUFFER: TOOL-ERRORS

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
\ A held row's holding request's HTTP:REQUEST-ID$, in the row's own BUF.
MAX-HELD BUF:HDR-BYTES * BUFFER: KEEP-REQS
TASK:FACILITY KEEP-LOCK
KEEP-LOCK TASK:FACILITY-INIT

CAST: >HELD ( n -- held )
CAST: HELD>N ( held -- n )

: SLOT ( -- n ) HTTP:WORKER-SLOT ;
: >SPAN ( str -- SPAN:span<u8> ) {: s:str :} s STR:DATA s STR:LEN SPAN:MAKE ;
: CURSOR ( -- ^JSON:cursor ) SLOT READER ;
: ID$ ( -- str ) SLOT ID @ SPAN:$ ;
: METHOD$ ( -- str ) SLOT METHOD @ SPAN:$ ;
: PARAMS$ ( -- str ) SLOT PARAMS @ SPAN:$ ;
: TEXT$ ( -- str ) SLOT TEXT @ SPAN:$ ;
: LEGACY? ( -- bool ) SLOT ERA @ LEGACY = ;


\ A value's JSON text at the reader: a container whole, a string with quotes.
: RAW$ ( JSON:reader -- JSON:reader str )
   JSON:READ-TOKEN dup JSON:T-OBJ = over JSON:T-ARR = or if
      drop JSON:READ-VALUE-SPAN$ exit
   then
   JSON:T-STR = if
      JSON:READ-SPAN$ {: s:str :} s STR:DATA 1- s STR:LEN 2 + >str exit
   then
   JSON:READ-SPAN$ ;


\ The token kind of a non-empty JSON text's first value.
: KIND ( str -- n )
   {: text:str :}
   CURSOR text JSON:READ-INIT
   JSON:READ-NEXT {: k:n :}
   JSON:READ-CLOSE k ;

: OBJECT? ( str -- bool ) KIND JSON:T-OBJ = ;


\ Throws E-INPUT with `member`: expected <e>; <fix>.
: INPUT-ERROR ( str str str -- )
   {: ms:str es:str fs:str :}
   REASON:OPEN ms false REASON:QUOTE s" : " REASON:APPEND
   REASON:EXPECTED es REASON:APPEND REASON:FIX fs REASON:APPEND
   E-INPUT REASON:STOP ;

\ Throws E-INPUT with `name`: expected a JSON object; send name as one object.
: OBJECT-ERROR ( str -- )
   {: ns:str :}
   REASON:OPEN ns false REASON:QUOTE s" : " REASON:APPEND
   REASON:EXPECTED s" a JSON object" REASON:APPEND
   REASON:FIX s" send " REASON:APPEND ns REASON:APPEND s"  as one object" REASON:APPEND
   E-INPUT REASON:STOP ;

\ A member of the object called name and whether it is there; text that is
\ no object, empty included, is E-INPUT naming it.
: FIELD? ( str str str -- str bool )
   {: data:str ns:str key:str :}
   data STR:LEN 0= if ns OBJECT-ERROR then
   CURSOR data JSON:READ-INIT
   JSON:READ-NEXT JSON:T-OBJ <> if JSON:READ-CLOSE ns OBJECT-ERROR then
   key JSON:READ-FIND-KEY 0= if JSON:READ-CLOSE s" " false exit then
   RAW$ {: value:str :}
   JSON:READ-CLOSE value true ;

\ Whether a JSON text is a string that decodes to the wanted bytes.
: STRING= ( str str -- bool )
   {: raw:str wanted:str :}
   CURSOR raw JSON:READ-INIT
   JSON:READ-NEXT JSON:T-STR <> if JSON:READ-CLOSE false exit then
   wanted JSON:READ-STR-EQ? {: same:bool :}
   JSON:READ-CLOSE same ;

: METHOD= ( str -- bool ) METHOD$ swap STRING= ;


: BAD ( n n str -- )
   {: status:n code:n why:str :}
   status SLOT STATUS !
   code SLOT CODE !
   why >SPAN SLOT TEXT !
   ERROR-REPLY SLOT REPLY ! ;

: GOOD ( n -- ) SLOT REPLY !  200 SLOT STATUS ! ;
: MISMATCH ( -- ) 400 HEADER-MISMATCH s" Header mismatch" BAD ;
: NO-METHOD ( -- ) 404 -32601 s" Method not found" BAD ;
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
: MEMBERS ( str -- str )
   STR:TRIM {: s:str :}
   s STR:DATA 1+ s STR:LEN 2 - >str STR:TRIM ;


\ _meta: the members of meta, empty or an object's text, then serverInfo.
: MODERN-META ( ^JSON:writer str -- ^JSON:writer )
   {: meta:str :}
   JSON:WRITE-COMMA s" _meta" JSON:WRITE-KEY
   JSON:WRITE-OBJECT-START
   meta STR:LEN 0<> if
      meta MEMBERS {: inner:str :}
      inner STR:LEN 0<> if inner JSON:WRITE-RAW JSON:WRITE-COMMA then
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
   TOOL-OUTPUT @ SPAN:$ {: text:str :}
   s" outputSchema" JSON:WRITE-KEY
   JSON:WRITE-OBJECT-START s" type" s" object" JSON:WRITE-FIELD-S
   JSON:WRITE-COMMA s" properties" JSON:WRITE-KEY
   JSON:WRITE-OBJECT-START s" data" text JSON:WRITE-FIELD-RAW JSON:WRITE-OBJECT-END
   JSON:WRITE-COMMA s" required" JSON:WRITE-KEY
   JSON:WRITE-ARRAY-START s" data" JSON:WRITE-STRING JSON:WRITE-ARRAY-END
   JSON:WRITE-OBJECT-END ;


: WRITE-MEMBER ( JSON:reader ^JSON:writer n -- JSON:reader )
   {: w idx:n :}
   s" outputSchema" JSON:READ-STR-EQ? {: output:bool :}
   JSON:READ-SPAN$ {: key:str :}
   JSON:READ-NEXT drop RAW$ {: value:str :}
   output if w idx OUTPUT-SCHEMA drop exit then
   w key STR:DATA 1- key STR:LEN 2 + >str JSON:WRITE-RAW s" :" JSON:WRITE-RAW value JSON:WRITE-RAW drop ;


: TOOL-ENTRY ( ^JSON:writer n -- ^JSON:writer )
   {: w idx:n :}
   idx TOOL-TEXT @ SPAN:$ {: text:str :}
   idx TOOL-WHOLE @ if w text JSON:WRITE-RAW exit then
   w JSON:WRITE-OBJECT-START drop
   CURSOR text JSON:READ-INIT JSON:READ-NEXT drop JSON:READ-NEXT drop
   w idx WRITE-MEMBER
   begin JSON:READ-NEXT JSON:T-KEY = while
      w JSON:WRITE-COMMA drop  w idx WRITE-MEMBER
   repeat JSON:READ-CLOSE
   w JSON:WRITE-OBJECT-END ;


\ Whether the request's caller may see and call the registered tool.
: SHOWN? ( n -- bool )
   TOOL-NAME @ SPAN:$ dup STR:DATA swap STR:LEN VISIBILITY @ execute ;


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
   idx RESOURCE-TEXT @ SPAN:$ {: desc:str :}
   RESULT-START
   s" contents" JSON:WRITE-KEY JSON:WRITE-ARRAY-START JSON:WRITE-OBJECT-START
   s" uri" desc s" descriptor" s" uri" FIELD? drop JSON:WRITE-FIELD-RAW
   JSON:WRITE-COMMA s" mimeType"
   desc s" descriptor" s" mimeType" FIELD? drop JSON:WRITE-FIELD-RAW
   desc s" descriptor" s" _meta" FIELD? if
      {: meta:str :}
      JSON:WRITE-COMMA s" _meta" meta JSON:WRITE-FIELD-RAW
   else drop then
   JSON:WRITE-COMMA s" text" idx RESOURCE-PAGE @ SPAN:$ JSON:WRITE-FIELD-S
   JSON:WRITE-OBJECT-END JSON:WRITE-ARRAY-END CACHED-END ;


: TEMPLATES-BODY ( ^JSON:writer -- ^JSON:writer )
   RESULT-START s" resourceTemplates" JSON:WRITE-KEY
   JSON:WRITE-ARRAY-START JSON:WRITE-ARRAY-END CACHED-END ;


: TEXT-BLOCK ( ^JSON:writer -- ^JSON:writer )
   JSON:WRITE-OBJECT-START s" type" s" text" JSON:WRITE-FIELD-S
   JSON:WRITE-COMMA s" text" TEXT$ JSON:WRITE-FIELD-S
   JSON:WRITE-OBJECT-END ;


: RESULT-BLOCKS ( ^JSON:writer -- ^JSON:writer )
   SLOT CONTENT-SPAN @ SPAN:$ {: blocks:str :}
   blocks STR:LEN 0<> if blocks JSON:WRITE-RAW then ;


: CALL-BODY ( ^JSON:writer -- ^JSON:writer )
   JSON:WRITE-OBJECT-START
   LEGACY? 0= if
      s" resultType" s" complete" JSON:WRITE-FIELD-S JSON:WRITE-COMMA
   then
   s" content" JSON:WRITE-KEY JSON:WRITE-ARRAY-START
   SLOT FAULT @ if TEXT-BLOCK else RESULT-BLOCKS then
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
   SLOT META-SPAN @ SPAN:$ {: meta:str :}
   LEGACY? if
      meta STR:LEN 0<> if JSON:WRITE-COMMA s" _meta" meta JSON:WRITE-FIELD-RAW then
   else meta MODERN-META then
   JSON:WRITE-OBJECT-END ;


: PING-BODY ( ^JSON:writer -- ^JSON:writer )
   JSON:WRITE-OBJECT-START JSON:WRITE-OBJECT-END ;


: RESULT-BODY ( ^JSON:writer -- ^JSON:writer )
   ID$ JSON:PARSE JSON-RPC:RESULT
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
   s" uri" PARAMS$ s" params" s" uri" FIELD? drop JSON:WRITE-FIELD-RAW
   JSON:WRITE-OBJECT-END ;


: ID-FIELD ( ^JSON:writer -- ^JSON:writer )
   ID$ {: id:str :}
   id STR:LEN 0= if exit then
   JSON:WRITE-COMMA s" id" id JSON:WRITE-FIELD-RAW ;


\ A STALE-REPLY's message, part by part: it names the tool and the server.
: STALE-PARTS ( R [ R str -- R ] -- R )
   {: q :}
   s" Unknown tool: " q execute
   TEXT$ q execute
   s" ; if the client's list of " q execute
   SERVER-NAME @ SPAN:$ q execute
   s" 's tools names it, that list is out of date" q execute ;

: MESSAGE ( ^JSON:writer -- ^JSON:writer )
   s" message" JSON:WRITE-KEY
   SLOT REPLY @ STALE-REPLY <> if TEXT$ JSON:WRITE-STRING exit then
   JSON:WRITE-STRING-START
   [: JSON:WRITE-STRING-PART ;] STALE-PARTS
   JSON:WRITE-STRING-END ;


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

: VEC-REPLY ( -- ) SLOT ANSWER-W RESPONSE-BODY drop ;

: SEND ( -- )
   SENT SLOT STAGE !
   SLOT REPLY-TO @ MATCH sink
      response OF HTTP-REPLY ENDOF
      vector OF VEC-REPLY ENDOF
   ;MATCH ;


\ A header's value when it arrived exactly once; otherwise empty and false.
: HEADER$ ( HTTP:request str -- str bool )
   {: asked:HTTP:request name:str :}
   asked name HTTP:HEADER-COUNT-OF 1 <> if s" " false exit then
   asked name HTTP:HEADER-OF$ ;


\ A media type without its parameters.
: MEDIA-ESSENCE ( str -- str )
   {: media:str :}
   media 59 STR:INDEX-OF MATCH option
      some OF IDX>N media STR:DATA swap >str ENDOF
      none OF media ENDOF
   ;MATCH STR:TRIM ;


\ Whether a comma-separated header value lists the media type.
: ACCEPT-HAS? ( str str -- bool )
   {: value:str wanted:str :}
   0 value STR:LEN 1+ 0 ?do
      i value STR:LEN = if true else value STR:DATA i + c@ 44 = then if
         {: start:n :}
         value STR:DATA start + i start - >str MEDIA-ESSENCE
         wanted STR:=CI if
            unloop true exit
         then
         i 1+
      then
   loop drop false ;


: ACCEPT? ( HTTP:request -- bool )
   s" Accept" HEADER$ 0= if drop false exit then
   {: value:str :}
   value s" application/json" ACCEPT-HAS?
   value s" text/event-stream" ACCEPT-HAS? and ;

: CONTENT-TYPE? ( HTTP:request -- bool )
   s" Content-Type" HEADER$ 0= if drop false exit then
   MEDIA-ESSENCE s" application/json" STR:=CI ;


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
   {: id:str m:str p:str :}
   id STR:LEN 0<> if
      id MCP-ID? 0= if
         400 -32600 s" an id that is not a string or an integer" BAD
         false exit
      then
   then
   id >SPAN SLOT ID !
   m >SPAN SLOT METHOD !
   p >SPAN SLOT PARAMS !
   p STR:LEN 0<> if p OBJECT? 0= if s" params" OBJECT-ERROR then then
   true ;


\ A JSON-RPC response is no request: answered without an id.
: RESPONSE? ( JSON:value JSON:value -- bool )
   drop drop
   400 -32600 s" a response, where MCP takes a request or a notification" BAD
   false ;


\ The reader's code and why answer; its id is the reply's when it is an MCP
\ id, and its null id is unknown.
: INVALID? ( n str JSON:value -- bool )
   {: code:n why:str reply :}
   reply JSON:TEXT$ {: id:str :}
   id MCP-ID? if id >SPAN SLOT ID ! then
   400 code why BAD false ;


\ The text of params a message carries, empty for none.
: GIVEN$ ( option<JSON:value> -- str )
   MATCH option
      none OF s" " ENDOF
      some OF JSON:TEXT$ ENDOF
   ;MATCH ;


: ENVELOPE? ( str -- bool )
   {: body:str :}
   body JSON-RPC:>MESSAGE
   MATCH JSON-RPC:message
      request OF {: i m p :}
         i JSON:TEXT$ m JSON:TEXT$ p GIVEN$ CALL? ENDOF
      notification OF {: m p :}
         s" " m JSON:TEXT$ p GIVEN$ CALL? ENDOF
      success OF RESPONSE? ENDOF
      failure OF RESPONSE? ENDOF
      invalid OF {: why:^ wu:n reply :} why wu >str reply INVALID? ENDOF
   ;MATCH ;


\ A member of params._meta, and whether it is there.
: META ( str -- str bool )
   {: key:str :}
   PARAMS$ dup STR:LEN 0= if false exit then
   s" params" s" _meta" FIELD? 0= if false exit then
   s" params._meta" key FIELD? ;


\ Requests carry params._meta with protocolVersion and clientCapabilities: one
\ missing is -32602 (2026-07-28 basic "Per-request protocol fields"), a version
\ other than the header's -32020.
: MODERN? ( str -- bool )
   {: version:str :}
   s" io.modelcontextprotocol/protocolVersion" META 0= if
      drop 400 -32602 s" Protocol version required" BAD false exit
   then
   version STRING= 0= if MISMATCH false exit then
   s" io.modelcontextprotocol/clientCapabilities" META
   if OBJECT? else drop false then
   0= if 400 -32602 s" Client capabilities required" BAD false exit then
   true ;


: ERA? ( str -- bool )
   {: value:str :}
   value s" 2026-07-28" STR:= if
      MODERN SLOT ERA ! value MODERN? exit
   then
   value s" 2025-11-25" STR:= if LEGACY SLOT ERA ! true exit then
   value >SPAN SLOT ASKED !
   400 UNSUPPORTED-VERSION s" Unsupported protocol version" BAD false ;


\ The era from one MCP-Protocol-Version: 2026-07-28 also requires MODERN?;
\ 2025-11-25, or no header on initialize, is the legacy lifecycle
\ (https://modelcontextprotocol.io/specification/2025-11-25/basic/lifecycle).
: VERSION? ( HTTP:request -- bool )
   {: asked:HTTP:request :}
   asked s" MCP-Protocol-Version" HTTP:HEADER-COUNT-OF 1 > if MISMATCH false exit then
   asked s" MCP-Protocol-Version" HEADER$ if ERA? exit then
   drop
   s" initialize" METHOD= if LEGACY SLOT ERA ! true exit then
   MISMATCH false ;


\ An Mcp-Name value, decoded into the worker's row when it is =?base64?...?=.
: DECODED-NAME$ ( str -- str )
   {: header:str :}
   header s" =?base64?" STR:STARTS-WITH? 0= if header exit then
   header STR:LEN 11 <
   header STR:DATA header STR:LEN 2 - + 2 >str s" ?=" STR:= 0=  or if
      s" Mcp-Name" s" an encoded name ending in `?=`"
      s" end the `=?base64?` name with `?=`" INPUT-ERROR
   then
   NAMES SLOT HTTP:HEAD-CAP * HTTP:HEAD-CAP SPAN:SUB {: room :}
   header STR:DATA 9 + header STR:LEN 11 - >str room BASE64:DECODE {: u:n :}
   room 0 SPAN:AT u >str ;


\ The codes of an Mcp-Name that is not a valid encoded name.
: NAME-FAULT? ( n -- bool )
   {: code:n :}
   code E-INPUT = code E-BASE64-FIRST <= code E-BASE64-LAST >= and or ;


\ A modern request's Mcp-Name names the params member at key, a string: the
\ messages are -32602's when the member is absent and when it is no string.
: NAME-HEADER? ( HTTP:request str str str -- bool )
   {: asked:HTTP:request key:str absent:str invalid:str :}
   PARAMS$ s" params" key FIELD? 0= if
      drop 400 -32602 absent BAD false exit
   then
   {: name:str :}
   name KIND JSON:T-STR <> if 400 -32602 invalid BAD false exit then
   asked s" Mcp-Name" HEADER$ 0= if drop MISMATCH false exit then
   [: DECODED-NAME$ ;] catch {: code:n :}
   code 0<> if
      code NAME-FAULT? 0= if code throw then
      drop MISMATCH false exit
   then
   name swap STRING= dup 0= if MISMATCH then ;


\ The modern era's Mcp-Method names the body's method, -32020 otherwise; a
\ tools/call's Mcp-Name names params.name, a resources/read's params.uri.
: HEADERS? ( HTTP:request -- bool )
   {: asked:HTTP:request :}
   LEGACY? if true exit then
   asked s" Mcp-Method" HEADER$ 0= if drop MISMATCH false exit then
   METHOD$ swap STRING= 0= if MISMATCH false exit then
   s" tools/call" METHOD= if
      asked s" name" s" Tool name required" s" Invalid tool name" NAME-HEADER? exit
   then
   s" resources/read" METHOD= if
      asked s" uri" s" Resource URI required" s" Invalid resource URI" NAME-HEADER? exit
   then
   true ;


\ The registered tool a JSON name text names, else -1.
: TOOL-OF ( str -- n )
   {: raw:str :}
   TOOL-COUNT @ 0 ?do
      raw i TOOL-NAME @ SPAN:$ STRING= if i unloop exit then
   loop -1 ;


: ARGUMENTS ( -- str )
   PARAMS$ s" params" s" arguments" FIELD? 0= if drop s" {}" exit then
   dup OBJECT? 0= if
      s" arguments" OBJECT-ERROR
   then ;


\ A JSON string's text between its quotes, as sent, its escapes kept.
: UNQUOTED$ ( str -- str )
   {: s:str :}
   s STR:DATA 1+ s STR:LEN 2 - >str ;


128 constant NAME-MAX    \ MCP's longest tool name, in bytes

\ params.name, a string, between its quotes as sent, cut to NAME-MAX bytes at
\ a character's start.
: NAMED$ ( -- str )
   PARAMS$ s" params" s" name" FIELD? drop UNQUOTED$ {: name:str :}
   name STR:DATA name NAME-MAX UTF8:FIT >str ;


\ -32602 for a tool the caller cannot call, unknown or hidden: `Unknown tool:
\ <name>; if the client's list of <title>'s tools names it, that list is out of
\ date`, <title> serverInfo's name. TEXT borrows the name from the request,
\ and MESSAGE writes the message from it as the reply is written.
: UNKNOWN-TOOL ( -- )
   400 -32602 NAMED$ BAD  STALE-REPLY SLOT REPLY ! ;


\ A held row's id BUF.
: KEEP-ID ( n -- ^n ) BUF:HDR-BYTES * KEEP-IDS + CELL-VIEW ;
: KEEP-REQ ( n -- ^n ) BUF:HDR-BYTES * KEEP-REQS + CELL-VIEW ;
: KEEP-UNLOCK ( -- ) KEEP-LOCK TASK:RELEASE ;

\ The text written into the BUF, which grows to hold it.
: TEXT-SET ( str ^n -- )
   {: s:str buf:^ :}
   buf BUF:LIVE? 0= if buf 64 BUF:N>BLEN BUF:INIT then
   s STR:DATA s STR:LEN BUF:N>BLEN buf BUF:REPLACE ;

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
   SLOT TO @ HTTP:REQUEST-ID$ idx KEEP-REQ TEXT-SET
   SLOT ERA @ idx KEEPS KEEP-ERA !
   SLOT CALLED @ idx KEEPS KEEP-TOOL !
   SLOT FROM @ HTTP:TAKE-OVER drop idx KEEPS KEEP-CONN !
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
   {: name:str :}
   TOOL-COUNT @ 0 ?do
      name i TOOL-NAME @ SPAN:$ STR:= if true unloop exit then
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

\ Throws E-SCHEMA with the name of the text judged, then the text.
: SAY ( str str -- )
   {: what:str text:str :}
   REASON:OPEN what REASON:APPEND s"  " REASON:APPEND text REASON:APPEND
   E-SCHEMA REASON:STOP ;

\ Rejects the reader's token in the text judged, given the expected text
\ and the fix: READ-REJECT's E-MEMBER text leaves as E-SCHEMA.
: REJECT ( JSON:reader str str str -- )
   {: what:str want:str fix:str :}
   [: want fix JSON:READ-REJECT ;]
   catch {: code:n :}
   code JSON:E-MEMBER = if what code REASON:TEXT$ SAY then
   code throw ;

\ Puts the text with the key between its first two backticks, "``".
: PUT-KEYED ( ^u8 ^u8 n ^u8 n -- ^u8 )
   {: dst:^ ta:^ tu:n ka:^ ku:n :}
   ta tu >str s" ``" STR:FIND-SUB MATCH option
      none OF tu ENDOF
      some OF IDX>N 1+ ENDOF
   ;MATCH {: at:n :}
   dst ta at PUT ka ku PUT ta at + tu at - PUT ;

: KEY-SCOPE ( JSON:reader ^u8 n ^u8 n ^u8 n ^u8 NUM:alloc-byte-len -- JSON:reader )
   drop {: ka:^ ku:n ea:^ eu:n fa:^ fu:n buf:^ :}
   buf ea eu ka ku PUT-KEYED {: fix:^ :}
   fix fa fu ka ku PUT-KEYED drop
   s" descriptor" buf eu ku + >str fix fu ku + >str REJECT ;

\ Rejects the reader's token in a descriptor, the key put as PUT-KEYED puts it.
: KEY-REJECT ( JSON:reader str str str -- )
   {: key:str want:str fix:str :}
   key STR:DATA key STR:LEN want STR:DATA want STR:LEN fix STR:DATA fix STR:LEN
   want STR:LEN fix STR:LEN + key STR:LEN 2 * + MEM:BYTES-ALLOC-LEN
   [: KEY-SCOPE ;] MEM:WITH-BYTES ;

\ An input's JSON fault leaves as E-SCHEMA, its text after the name of the text
\ judged; any other code stays.
: SCHEMA-CODE ( str n -- n )
   {: what:str code:n :}
   code JSON:INPUT-FAULT? if
      what code REASON:TEXT$ SAY
   then
   code ;

\ A reader on the text's first token.
: FIRST-TOKEN ( str ^JSON:cursor -- JSON:reader )
   {: text:str reader :}
   reader text JSON:READ-INIT JSON:READ-NEXT drop ;

\ Whether a JSON text, read whole by reader, is an object; text that is not one
\ JSON value throws the reader's E-JSON-READ-* code.
: OBJECT-TEXT? ( str ^JSON:cursor -- bool )
   {: text:str reader :}
   reader text JSON:READ-INIT JSON:READ-NEXT JSON:T-OBJ = {: object:bool :}
   JSON:READ-FINISH object ;


\ Rejects the text's k-th top-level key, counted from 0, as a repeat.
: REPEAT-REJECT ( n str ^JSON:cursor str -- )
   {: k:n text:str reader what:str :}
   text reader FIRST-TOKEN
   k 0 ?do
      JSON:READ-NEXT drop JSON:READ-NEXT drop JSON:READ-SKIP-VALUE
   loop JSON:READ-NEXT drop
   what s" each top-level member once"
   s" remove the repeated member, keeping one" REJECT ;

: SHAPE-IN ( str ^JSON:cursor str -- )
   {: text:str reader what:str :}
   text reader OBJECT-TEXT? 0= if
      text reader FIRST-TOKEN what
      s" a JSON object" s" write it as one object" REJECT
   then
   text JSON:READ-REPEAT MATCH option
      none OF ENDOF
      some OF text reader what REPEAT-REJECT ENDOF
   ;MATCH ;

\ E-SCHEMA unless the text what names is one JSON object with no top-level key repeated
\ once decoded: JSON.parse keeps a repeated key's last value, so a client would read another.
: SHAPE ( str ^JSON:cursor str -- )
   {: text:str reader what:str :}
   [: text reader what SHAPE-IN ;] catch {: code:n :}
   what code SCHEMA-CODE {: fault:n :}
   fault 0<> if fault throw then ;


\ Whether a schema object's "type" is the string "object".
: WHOLE? ( str -- bool )
   {: text:str :}
   SCHEMA-READER text JSON:READ-INIT JSON:READ-NEXT drop
   s" type" JSON:READ-FIND-KEY if
      JSON:READ-TOKEN JSON:T-STR = if s" object" JSON:READ-STR-EQ? else false then
   else false then {: whole:bool :}
   JSON:READ-FINISH whole ;


\ The reader on the value of a top-level member of a descriptor SHAPE
\ admitted; an absent member is rejected at the descriptor's `}`.
: MEMBER-AT ( str str -- JSON:reader )
   {: text:str key:str :}
   text SCHEMA-READER FIRST-TOKEN
   key JSON:READ-FIND-KEY 0= if
      key s" a member ``" s" add ``" KEY-REJECT
   then ;

: PART ( str str -- str )
   MEMBER-AT RAW$ {: value:str :} JSON:READ-CLOSE value ;


: OBJECT-PART ( str str -- str )
   {: text:str key:str :}
   text key MEMBER-AT
   JSON:READ-TOKEN JSON:T-OBJ <> if
      key s" `` as a JSON object" s" write `` as an object" KEY-REJECT
   then
   RAW$ {: value:str :} JSON:READ-CLOSE value ;


\ The reader on a descriptor member's string value.
: STRING-AT ( str str -- JSON:reader )
   {: text:str key:str :}
   text key MEMBER-AT
   JSON:READ-TOKEN JSON:T-STR <> if
      key s" `` as a string" s" write `` as a JSON string" KEY-REJECT
   then ;

: REQUIRE-STRING ( str str -- )
   STRING-AT JSON:READ-CLOSE ;


\ Whether the reader's token is the JSON string "object".
: OBJECT-STRING? ( JSON:reader -- JSON:reader bool )
   JSON:READ-TOKEN JSON:T-STR = if
      s" object" JSON:READ-STR-EQ?
   else false then ;

\ Rejects an inputSchema whose root `type` is not "object", as MCP's Tool
\ type requires; an absent `type` at the root's `}`.
: OBJECT-ROOT ( str -- )
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
: INPUT-SCHEMA ( str n -- )
   {: in:str idx:n :}
   ARENA:NEW {: ar:^ :}
   [: in JSON:PARSE ar JSON-SCHEMA:COMPILE idx TOOL-INPUT !
      in OBJECT-ROOT ;] catch
   {: code:n :}
   code REASON:TEXT$ {: text:str :}
   code 0<> if ar ARENA:RELEASE then
   code JSON-SCHEMA:E-SCHEMA = if s" inputSchema" text SAY then
   s" inputSchema" code SCHEMA-CODE {: rest:n :}
   rest 0<> if rest throw then ;


\ The offset of the first byte UTF8:NEXT answers raw-byte for, else -1.
: BAD-BYTE ( str -- n ) {: s:str :}
   0 begin dup s STR:LEN < while
      s swap UTF8:NEXT MATCH UTF8:scalar-step
         scalar OF nip ENDOF
         raw-byte OF nip 1- exit ENDOF
      ;MATCH
   repeat drop -1 ;

: REQUIRE-UTF8 ( str -- ) {: page:str :}
   page BAD-BYTE {: at:n :}
   at 0 < if exit then
   REASON:OPEN s" page " REASON:APPEND
   page STR:DATA dup at + 1 1 0 REJECTION:POSITION drop REASON:AT
   REASON:EXPECTED s" UTF-8 text" REASON:APPEND
   REASON:FOUND page STR:DATA at + c@ REASON:BYTE
   REASON:FIX s" encode the page as UTF-8" REASON:APPEND
   E-SCHEMA REASON:STOP ;


: REGISTERED-URI? ( str -- bool )
   {: uri:str :}
   RESOURCE-COUNT @ 0 ?do
      uri i RESOURCE-URI @ SPAN:$ STR:= if true unloop exit then
   loop false ;


: DECODE-COPY ( JSON:reader ^VEC:vector<u8> -- JSON:reader SPAN:span<u8> )
   {: v:^ :}
   v JSON:READ-STR {: s:str :}
   s STR:LEN MEM:BYTES-ALLOC-LEN MEM:ALLOC-SPAN {: name :}
   s name SPAN:COPY  name ;

\ The storage is kept for the process, as the registry is; the descriptor passed SHAPE.
: DECODED-MEMBER ( str str [ str -- bool ] -- SPAN:span<u8> )
   {: text:str key:str registered? :}
   text key STRING-AT JSON:READ-SPAN$ STR:LEN {: ru:n :}
   ru 0= if
      key s" `` as a non-empty string" s" give `` a value" KEY-REJECT
   then
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW {: v:^ :}
   [: v DECODE-COPY ;] [: v VEC:DELETE ;] finally {: name :}
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
   {: text:str :}
   CURSOR text JSON:READ-INIT
   JSON:READ-NEXT JSON:T-ARR <> if
      s" content" s" a JSON array of content blocks"
      s" write the blocks in an array" REJECT
   then
   JSON:READ-NEXT JSON:T-ARR-END = if JSON:READ-FINISH s" " exit then
   JSON:READ-SPAN$ STR:DATA {: first:^ :}
   begin BLOCK JSON:READ-NEXT JSON:T-ARR-END = until
   JSON:READ-SPAN$ STR:DATA {: close:^ :}
   JSON:READ-FINISH first close first - >str ;


\ The objects of a JSON array of MCP content blocks, from the first's open
\ through the space before the array's close; empty for an empty array.
: BLOCKS ( str -- str )
   [: BLOCKS-IN ;] catch {: code:n :}
   code 0= if exit then
   drop s" content" code SCHEMA-CODE throw ;


\ A result that is not one JSON object is rejected when the tool's
\ outputSchema has type object; text that is not JSON throws the reader's code.
: RESULT-SHAPE ( str -- ) {: text:str :}
   text CURSOR OBJECT-TEXT? if exit then
   SLOT CALLED @ TOOL-WHOLE @ 0= if exit then
   text CURSOR FIRST-TOKEN s" result"
   s" a JSON object, as the tool's outputSchema has type `object`"
   s" answer one JSON object" REJECT ;


\ Rejects meta naming serverInfo at that key; meta passed SHAPE.
: NO-SERVER-INFO ( str -- ) {: meta:str :}
   meta CURSOR FIRST-TOKEN
   begin JSON:READ-NEXT JSON:T-KEY = while
      s" io.modelcontextprotocol/serverInfo" JSON:READ-STR-EQ? if
         s" meta" s" no member `io.modelcontextprotocol/serverInfo`"
         s" remove it: the server writes its own" REJECT
      then
      JSON:READ-NEXT drop JSON:READ-SKIP-VALUE
   repeat JSON:READ-CLOSE ;


\ Bodies run on several workers at once, so meta and blocks are read by the worker's CURSOR.
: RESULT! ( str str str bool -- )
   {: text:str meta:str extra:str fault:bool :}
   SLOT STAGE @ CALLING <> if E-STATE throw then
   fault 0= if text RESULT-SHAPE then
   meta STR:LEN 0<> if
      meta CURSOR s" meta" SHAPE
      meta NO-SERVER-INFO
   then
   extra BLOCKS {: blocks:str :}
   text >SPAN SLOT TEXT !
   meta >SPAN SLOT META-SPAN !
   blocks >SPAN SLOT CONTENT-SPAN !
   fault SLOT FAULT !
   CALL-REPLY GOOD SEND ;

\ The worker's BUF in a table of BUF headers, made live the first time.
: WORKER-BUF ( ^u8 -- ^n )
   {: table:^ :}
   SLOT BUF:HDR-BYTES * table + CELL-VIEW {: buf:^ :}
   buf BUF:LIVE? 0= if buf 1024 BUF:N>BLEN BUF:INIT then
   buf ;

\ Under LOG the text is copied: the line is written after the body returns.
: TOOL-ERROR ( str -- )
   {: text:str :}
   LOGGING @ 0= if text s" " s" []" true RESULT! exit then
   TOOL-ERRORS WORKER-BUF {: buf:^ :}
   text STR:DATA text STR:LEN BUF:N>BLEN buf BUF:REPLACE
   buf BUF:SPAN$ BUF:BLEN>N >str s" " s" []" true RESULT! ;

\ The vector in a worker's row cell, made the first time.
: WORKER-VEC ( ^^VEC:vector<u8> -- ^VEC:vector<u8> )
   {: c:^ :}
   c @ NULL-PTR = if HEAP:ALLOCATOR 0 [type] u8 VEC:NEW c ! then
   c @ ;

\ The worker's violation vector.
: VIOLATION ( -- ^VEC:vector<u8> ) SLOT VIOLATIONS WORKER-VEC ;

\ Whether the arguments satisfy the tool's inputSchema; false gives the tool
\ error `arguments` and the violation.
: ADMITTED? ( str n -- bool )
   {: args:str idx:n :}
   VIOLATION {: v:^ :}
   v 0 VEC:RESIZE  s" arguments" v VEC:APPEND
   args JSON:PARSE idx TOOL-INPUT @ v JSON-SCHEMA:VALID?
   dup if exit then
   v VEC:BYTES TOOL-ERROR ;


\ The body gives its result once, through RESULT, RESULT-META, RESULT-CONTENT or
\ RESULT-ERROR, before it returns, or HOLDs the call.
: RUN-TOOL ( -- )
   PARAMS$ s" params" s" name" FIELD? 0= if
      s" name" s" a member naming the tool" s" add `name` to params" INPUT-ERROR
   then
   dup KIND JSON:T-STR <> if
      s" name" s" a JSON string" s" send the tool's name as a string" INPUT-ERROR
   then
   TOOL-OF {: idx:n :}
   idx 0< if UNKNOWN-TOOL SEND exit then
   idx SHOWN? 0= if UNKNOWN-TOOL SEND exit then
   ARGUMENTS {: args:str :}
   idx SLOT CALLED !
   CALLING SLOT STAGE !
   args idx ADMITTED? 0= if exit then
   [: args STR:DATA args STR:LEN idx TOOL-BODY @ execute ;] catch BODY-FAULT
   SLOT STAGE @ CALLING = if E-STATE throw then ;


\ The registered resource a JSON uri text names, else -1.
: RESOURCE-OF ( str -- n )
   {: raw:str :}
   RESOURCE-COUNT @ 0 ?do
      raw i RESOURCE-URI @ SPAN:$ STRING= if i unloop exit then
   loop -1 ;


\ params.uri not a string is E-INPUT, which HANDLE answers -32602.
: READ-RESOURCE ( -- )
   PARAMS$ s" params" s" uri" FIELD? 0= if
      s" uri" s" a member naming the resource" s" add `uri` to params" INPUT-ERROR
   then
   dup KIND JSON:T-STR <> if
      s" uri" s" a JSON string" s" send the resource's uri as a string" INPUT-ERROR
   then
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
      vector OF ENDOF
   ;MATCH ;


\ The params member that names what the method acts on; empty for none.
: TARGET-KEY$ ( -- str )
   s" tools/call" METHOD= if s" name" exit then
   s" resources/read" METHOD= if s" uri" exit then
   s" " ;


: TARGET+ ( -- )
   TARGET-KEY$ {: key:str :}
   key STR:LEN 0= PARAMS$ STR:LEN 0= or if exit then
   \ JSON-RPC's params may be an array, which FIELD? refuses.
   PARAMS$ KIND JSON:T-OBJ <> if exit then
   PARAMS$ s" params" key FIELD? 0= if drop exit then
   dup KIND JSON:T-STR <> if drop exit then
   s"  " LOG:TEXT+  UNQUOTED$ LOG:TEXT+ ;


: LOGGED ( -- )
   LOGGING @ 0<> if exit then
   LOG:INFO s" mcp" LOG:LINE
   METHOD$ UNQUOTED$ LOG:TEXT+  TARGET+
   LOG:;LINE ;


4096 constant TEXT-MAX    \ the most bytes of one text on a LOG line

\ The text, or its first bytes up to TEXT-MAX cut at a character's start and
\ then ` [<n> B]`, n its length.
: TEXT-CUT+ ( str -- )
   {: s:str :}
   s STR:LEN TEXT-MAX <= if s LOG:TEXT+ exit then
   s TEXT-MAX UTF8:FIT {: cut:n :}
   s STR:DATA cut >str LOG:TEXT+
   s"  [" LOG:TEXT+  s STR:LEN LOG:INT+  s"  B]" LOG:TEXT+ ;


\ A tools/call's params.arguments as sent, when params is an object that has
\ them.
: ARGUMENTS+ ( -- )
   s" tools/call" METHOD= 0= if exit then
   PARAMS$ STR:LEN 0= if exit then
   PARAMS$ OBJECT? 0= if exit then
   PARAMS$ s" params" s" arguments" FIELD? 0= if drop exit then
   s"  " LOG:TEXT+  TEXT-CUT+ ;

\ The method, target and arguments; `-` when no method was read.
: CALL+ ( -- )
   METHOD$ STR:LEN 0= if s" -" LOG:TEXT+ exit then
   METHOD$ UNQUOTED$ LOG:TEXT+  TARGET+  ARGUMENTS+ ;


: ERROR+ ( -- )
   s" error " LOG:TEXT+  SLOT CODE @ LOG:INT+  s" : " LOG:TEXT+
   SLOT REPLY @ STALE-REPLY = if [: LOG:TEXT+ ;] STALE-PARTS exit then
   TEXT$ TEXT-CUT+ ;

\ The reply SEND began.
: REPLY+ ( -- )
   SLOT REPLY @ {: kind:n :}
   kind ERROR-REPLY = kind STALE-REPLY = or if ERROR+ exit then
   kind CALL-REPLY = SLOT FAULT @ and if
      s" tool error: " LOG:TEXT+  TEXT$ TEXT-CUT+ exit
   then
   s" ok" LOG:TEXT+ ;

\ The slot's outcome, given the code passed to the host.
: OUTCOME+ ( n -- )
   {: code:n :}
   code 0<> if s" fault " LOG:TEXT+  code LOG:INT+ exit then
   SLOT STAGE @ SENT = if REPLY+ exit then
   SLOT STAGE @ HELD = if s" held" LOG:TEXT+ exit then
   s" notified" LOG:TEXT+ ;

\ A LOG line opened with `req <id> `.
: REQ-LINE ( str -- )
   {: id:str :}
   LOG:INFO s" mcp" LOG:LINE
   s" req " LOG:TEXT+  id LOG:TEXT+  s"  " LOG:TEXT+ ;

\ With LOG, the message's line under the id text, given the code passed to
\ the host.
: MESSAGE-LOGGED ( str n -- )
   {: id:str code:n :}
   LOGGING @ 0= if exit then
   id REQ-LINE
   CALL+  s"  -> " LOG:TEXT+  code OUTCOME+
   LOG:;LINE ;

\ With LOG, FINISH's line for the held row, under the id of the request
\ that held it, given the code passed to the host.
: HELD-LOGGED ( n n -- )
   {: idx:n code:n :}
   LOGGING @ 0= if exit then
   idx KEEP-REQ BUF:SPAN$ BUF:BLEN>N >str REQ-LINE
   s" tools/call " LOG:TEXT+
   idx KEEPS KEEP-TOOL @ TOOL-NAME @ SPAN:$ LOG:TEXT+
   s"  -> " LOG:TEXT+  code OUTCOME+
   LOG:;LINE ;


: DISPATCH ( -- )
   LOGGED
   ID$ STR:LEN 0= if NOTIFIED exit then
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
   s" " >SPAN SLOT ID !
   s" " >SPAN SLOT METHOD !
   s" " >SPAN SLOT PARAMS ! ;


\ The code a message's caught code passes to the host: none for E-INPUT
\ before the reply began, which FAULTED answers -32602.
: PASSED ( n -- n )
   {: code:n :}
   SLOT STAGE @ IDLE = code E-INPUT = and if 0 exit then
   code ;

\ A message's caught code: E-INPUT before the reply began is -32602 with its
\ reason's text; any other fault is rethrown.
: FAULTED ( n -- )
   {: code:n :}
   code 0= if exit then
   code REASON:TEXT$ {: why:str :}
   code PASSED {: passed:n :}
   passed 0<> if passed throw then
   400 -32602 why BAD SEND ;


\ WORKER-SLOT's E-HTTP-HANDLE, on a task that is no request worker, as E-STATE.
: ON-WORKER ( -- )
   [: HTTP:WORKER-SLOT drop ;] catch {: code:n :}
   code 0= if exit then
   code E-HTTP-HANDLE = if E-STATE throw then
   code throw ;


\ The worker's vector for a held call's response.
: FINISH-VEC ( -- ^VEC:vector<u8> ) SLOT FINISHED WORKER-VEC ;

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
\ FINISH-VEC.
: RESUME ( n -- )
   {: idx:n :}
   construct sink vector OPENED-TO
   idx KEEP-ID BUF:SPAN$ BUF:BLEN>N SPAN:MAKE SLOT ID !
   idx KEEPS KEEP-ERA @ SLOT ERA !
   idx KEEPS KEEP-TOOL @ SLOT CALLED !
   CALLING SLOT STAGE ! ;

\ The quotation run as the body; E-STATE when it gives no result.
: COMPOSE ( [ -- ] -- )
   {: q :}
   SLOT ANSWER-W FINISH-VEC JSON:OPEN drop
   q [: SLOT ANSWER-W JSON:CLOSE ;] finally
   SLOT STAGE @ SENT <> if E-STATE throw then ;

\ The response sent on the row's connection, which a throw closes first.
: REPLY-HELD ( n [ -- ] -- )
   {: idx:n q :}
   idx RESUME
   [: q COMPOSE ;] catch {: code:n :}
   idx code HELD-LOGGED
   code 0<> if idx CLOSE-KEPT code throw then
   idx KEEPS KEEP-CONN @ 200 s" application/json"
   FINISH-VEC VEC:BYTES HTTP:REPLY drop ;


\ The message's envelope checked, then dispatched; the text is kept for catch.
: CARRIED ( str -- str )
   dup ENVELOPE? if DISPATCH else SEND then ;


\ The message answered into ANSWER-W, the writer ANSWER holds open on the
\ caller's vector.
: VEC-ANSWER ( str -- )
   [: CARRIED ;] catch {: code:n :} drop
   code PASSED {: passed:n :}
   [: code FAULTED ;] [: s" -" passed MESSAGE-LOGGED ;] finally ;

public

\ The server's name and version in serverInfo. The texts are borrowed and
\ outlive the server, as literals do. E-STATE while HTTP serves.
: SERVER-INFO! ( str str -- )
   {: name:str version:str :}
   HTTP:RUNNING? if E-STATE throw then
   name >SPAN SERVER-NAME !
   version >SPAN SERVER-VERSION ! ;


\ Guidance for the model on using this server: "instructions" in initialize and
\ server/discover results, left out while the text is empty. The text is
\ borrowed and outlives the server, as literals do. E-STATE while HTTP serves.
: INSTRUCTIONS! ( ^u8 n -- )
   HTTP:RUNNING? if E-STATE throw then
   SPAN:MAKE INSTRUCTIONS ! ;


\ From the next HTTP:START on, each message HANDLE or ANSWER takes writes one
\ LOG INFO line, tag mcp, once it is answered, in place of the line before
\ dispatch: `req <id> <method> <target> <arguments> -> <outcome>`. id is the
\ HTTP:REQUEST-ID$ of HANDLE's request, `-` under ANSWER; method and target
\ are as without LOG, `-` when no method was read; arguments are a tools/call's
\ params.arguments as sent, left out when absent. outcome is `ok`, `tool
\ error: <text>` (a schema refusal's included), `error <code>: <message>` for
\ a JSON-RPC error (UNKNOWN-TOOL's included), `held` for a call HOLD kept,
\ `notified` for a notification, or `fault <code>` for a throw passed to the
\ host. FINISH writes a held call's own line, `req <id> tools/call <name> ->
\ <outcome>`, with the id of the request that held it. A text past 4096 bytes
\ is cut at a character boundary and followed by ` [<n> B]`, n its length.
\ It lasts across STOP; E-STATE while HTTP serves.
: LOG ( -- )
   HTTP:RUNNING? if E-STATE throw then
   1 LOGGING ! ;


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
   text tu >str {: desc:str :}
   HTTP:RUNNING? if E-STATE throw then
   desc SCHEMA-READER s" descriptor" SHAPE
   desc s" inputSchema" OBJECT-PART {: input:str :}
   desc s" outputSchema" PART {: output:str :}
   output SCHEMA-READER s" outputSchema" SHAPE
   output WHOLE? {: whole:bool :}
   TOOL-COUNT @ {: idx:n :}
   idx 1+ TOOLS-RESERVE
   desc s" name" [: REGISTERED? ;] DECODED-MEMBER {: name :}
   [: input idx INPUT-SCHEMA ;] catch {: code:n :}
   code 0<> if name MEM:FREE-SPAN code throw then
   name idx TOOL-NAME !
   text tu SPAN:MAKE idx TOOL-TEXT !
   output >SPAN idx TOOL-OUTPUT !
   whole idx TOOL-WHOLE !
   body idx TOOL-BODY !
   idx 1+ TOOL-COUNT ! ;


\ A borrowed MCP Resource descriptor, with a non-empty string uri and string name and mimeType,
\ and the UTF-8 page resources/read answers; both must outlive the server.
: ADD-RESOURCE ( ^u8 n ^u8 n -- )
   {: text:^ tu:n page:^ pu:n :}
   text tu >str {: desc:str :}
   HTTP:RUNNING? if E-STATE throw then
   desc SCHEMA-READER s" descriptor" SHAPE
   desc s" name" REQUIRE-STRING
   desc s" mimeType" REQUIRE-STRING
   page pu >str REQUIRE-UTF8
   RESOURCE-COUNT @ {: idx:n :}
   idx 1+ RESOURCES-RESERVE
   desc s" uri" [: REGISTERED-URI? ;] DECODED-MEMBER {: uri :}
   uri idx RESOURCE-URI !
   text tu SPAN:MAKE idx RESOURCE-TEXT !
   page pu SPAN:MAKE idx RESOURCE-PAGE !
   idx 1+ RESOURCE-COUNT ! ;


\ A body's result: one JSON value. E-STATE outside a body or after its result;
\ else the reader's E-JSON-READ-* code or E-SCHEMA, which HANDLE passes to the host.
: RESULT ( ^u8 n -- ) >str s" " s" []" false RESULT! ;

\ RESULT, plus the result's _meta object, which reaches the host's widget and not the
\ model (OpenAI Apps SDK); empty meta adds none, and E-SCHEMA names the meta it refuses.
: RESULT-META ( ^u8 n ^u8 n -- )
   {: text:^ u:n meta:^ mu:n :}
   text u >str meta mu >str s" []" false RESULT! ;


\ RESULT, plus a JSON array of MCP content blocks, such as a text of the tool's own, an
\ image or an embedded resource, that is the result's content as given; E-SCHEMA unless
\ blocks is one JSON array of objects, each with one string "type".
: RESULT-CONTENT ( ^u8 n ^u8 n -- )
   {: text:^ u:n extra:^ eu:n :}
   text u >str s" " extra eu >str false RESULT! ;


\ A body's tool error: the message as text content, isError true.
: RESULT-ERROR ( ^u8 n -- ) >str TOOL-ERROR ;


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
      vector OF true ENDOF
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
   [: PROCESS ;] catch {: code:n :}
   code PASSED {: passed:n :}
   [: code FAULTED ;]
   [: answer HTTP:REQUEST-ID$ passed MESSAGE-LOGGED ;] finally ;


\ Answers one JSON-RPC message whose caller the host has authenticated and
\ which came by a transport other than HTTP, on a request worker: as HANDLE
\ answers a 2026-07-28 request, with no transport, version or header checks.
\ ANSWER empties the vector and writes the response's JSON text there, nothing for
\ a notification; after a throw its text is no reply to send. Any fault but
\ HANDLE's JSON-RPC errors is rethrown, a tool body's included.
\ The VISIBLE! hook is called with no HTTP request, so it judges by what the
\ host set for the message's caller: BEARER:CLAIM$ speaks only for a request
\ the worker serves. E-STATE before SERVER-INFO! and off a request worker.
: ANSWER ( str ^VEC:vector<u8> -- )
   SERVER-NAME @ SPAN:LEN 0= if E-STATE throw then
   ON-WORKER
   construct sink vector OPENED-TO
   MODERN SLOT ERA !
   SLOT ANSWER-W swap JSON:OPEN drop
   [: VEC-ANSWER ;] [: SLOT ANSWER-W JSON:CLOSE ;] finally ;


private

CAST: >CLIENT ( n -- client )
CAST: CLIENT>N ( client -- n )

\ What MESSAGE$'s buffer holds.
ENUM found nothing stray response ;ENUM
\ A row's record, kept by the row for its next client: the cursor parses a
\ reply and the writer builds a request in request, the client's vector, NULL
\ for a free row; status is the HTTP status the last exchange ended with, and
\ stream whether its reply was an event stream, which sse decodes; region is
\ the client's memory, empty for a free row.
STRUCTURE peer DERIVE addr
   FIELD cursor JSON:cursor
   FIELD writer JSON:writer
   FIELD request ^VEC:vector<u8>
   FIELD status CURL:http-status
   FIELD stream bool
   FIELD sse SSE:state
   FIELD region SPAN:span<u8>
   FIELD found found
;STRUCTURE

ARENA PEERS peer
\ The rows' arena, made by the first GROW so a saved image maps none; null until
\ then, and never released: the rows live as long as the process.
TYPED-VARIABLE RECORDS ^ARENA:arena

$1000 constant FIRST-CAP
0 constant URL-OFF
URL-OFF BUF:HDR-BYTES + constant REPLY-OFF
REPLY-OFF BUF:HDR-BYTES + constant LIST-OFF
LIST-OFF BUF:HDR-BYTES + constant MESSAGE-OFF
MESSAGE-OFF BUF:HDR-BYTES + constant TOKEN-OFF
TOKEN-OFF BUF:HDR-BYTES + constant REGION-BYTES

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
   idx REGION off BUF:HDR-BYTES SPAN:SUB SPAN:$ STR:DATA CELL-VIEW ;

: URL-BUF ( n -- ^n ) URL-OFF BUF-AT ;
: REQUEST-VEC ( n -- ^VEC:vector<u8> ) ROW PEER-REQUEST @ ;
: REPLY-BUF ( n -- ^n ) REPLY-OFF BUF-AT ;
: LIST-BUF ( n -- ^n ) LIST-OFF BUF-AT ;
: MESSAGE-BUF ( n -- ^n ) MESSAGE-OFF BUF-AT ;
: TOKEN-BUF ( n -- ^n ) TOKEN-OFF BUF-AT ;

: DECODER ( n -- SSE:decoder ) ROW PEER-SSE SSE:OPEN ;

: BUF$ ( ^n -- str ) BUF:SPAN$ BUF:BLEN>N >str ;
: BUF+ ( str ^n -- )
   {: s:str buf:^ :} s STR:DATA s STR:LEN BUF:N>BLEN buf BUF:APPEND-SPAN ;
: VEC$ ( ^VEC:vector<u8> -- str ) VEC:BYTES ;

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
   s" " >SPAN rec PEER-REGION !
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
   idx REQUEST-VEC VEC:DELETE
   NULL-PTR idx ROW PEER-REQUEST !
   idx REPLY-BUF BUF:DISPOSE
   idx LIST-BUF BUF:DISPOSE
   idx MESSAGE-BUF BUF:DISPOSE
   idx TOKEN-BUF BUF:DISPOSE
   idx REGION {: region :}
   LOCK TASK:GET
   s" " >SPAN idx ROW-PEER @ PEER-REGION !
   0 idx ROW-GEN !
   LOCK TASK:RELEASE
   region MEM:FREE-SPAN ;

\ The row's request vector, held before its room is reserved, so RELEASE
\ deletes it whatever the reserve does.
: MAKE-REQUEST ( n -- )
   {: idx:n :}
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW {: v:^ :}
   v idx ROW PEER-REQUEST !
   v FIRST-CAP VEC:RESERVE ;

\ Readies a claimed row's buffers and copies the URL; answers its operands.
: FILL-ROW ( n str -- n str )
   {: idx:n url:str :}
   idx URL-BUF FIRST-CAP BUF:N>BLEN BUF:INIT
   idx MAKE-REQUEST
   idx REPLY-BUF FIRST-CAP BUF:N>BLEN BUF:INIT
   idx LIST-BUF FIRST-CAP BUF:N>BLEN BUF:INIT
   idx MESSAGE-BUF FIRST-CAP BUF:N>BLEN BUF:INIT
   idx TOKEN-BUF FIRST-CAP BUF:N>BLEN BUF:INIT
   url idx URL-BUF BUF+
   idx url ;


: SCHEME-LEN ( str -- n )
   {: s:str :}
   s STR:LEN 8 < 0= if s STR:DATA 8 >str s" https://" STR:=CI if 8 exit then then
   s STR:LEN 7 < 0= if s STR:DATA 7 >str s" http://" STR:=CI if 7 exit then then
   0 ;

: AUTHORITY-BYTE? ( n -- bool )
   {: b:n :}
   b [char] / <> b [char] ? <> and b [char] # <> and ;

\ The URL's host and port, its userinfo cut; empty when an @ follows the
\ authority, where a password holding /, ? or # may have ended it early.
: HOST$ ( str -- str )
   {: s:str :}
   s SCHEME-LEN {: from:n :}
   from begin
      dup s STR:LEN < if dup s STR:DATA + c@ AUTHORITY-BYTE? else false then
   while 1+ repeat {: end:n :}
   s STR:DATA end + s STR:LEN end - >str s" @" STR:CONTAINS? if
      s STR:DATA 0 >str exit
   then
   from end from ?do s STR:DATA i + c@ [char] @ = if drop i 1+ then loop {: start:n :}
   s STR:DATA start + end start - >str ;

\ Curl's failure on the row's exchange, logged with the row's host, unless
\ REQUEST-STOP stopped the row: then the program ended it.
: CURL-FAULT ( CURL:code n -- )
   {: rc:CURL:code idx:n :}
   idx STOP@ if E-TRANSFER throw then
   idx URL-BUF BUF$ HOST$ {: host:str :}
   LOG:ERROR s" mcp" LOG:LINE  s" exchange" LOG:TEXT+
   host STR:LEN 0<> if s"  with " LOG:TEXT+  host LOG:TEXT+ then
   s"  failed: curl " LOG:TEXT+  rc CURL:CODE>N LOG:INT+
   s" : " LOG:TEXT+  rc CURL:ERROR$ LOG:TEXT+
   LOG:;LINE
   E-TRANSFER throw ;

\ The row's reply logged with what is wrong with it, then why when there is
\ a why, its status and its body.
: REPLY-LINE ( str str n -- )
   {: what:str why:str idx:n :}
   idx URL-BUF BUF$ HOST$ {: host:str :}
   idx REPLY-BUF BUF$ {: body:str :}
   LOG:ERROR s" mcp" LOG:LINE  s" reply" LOG:TEXT+
   host STR:LEN 0<> if s"  from " LOG:TEXT+  host LOG:TEXT+ then
   s"  " LOG:TEXT+  what LOG:TEXT+
   why STR:LEN 0<> if s" : " LOG:TEXT+  why LOG:TEXT+ then
   s" : status " LOG:TEXT+  idx ROW PEER-STATUS @ CURL:HTTP-STATUS>N LOG:INT+
   body STR:LEN 0<> if s"  body: " LOG:TEXT+  body LOG:TEXT+ then
   LOG:;LINE ;

: BAD-REPLY ( str n n -- )
   {: what:str idx:n code:n :}
   what s" " idx REPLY-LINE  code throw ;

: CHECK ( CURL:status n -- )
   {: idx:n :}
   MATCH CURL:status
      ok OF ENDOF
      failed OF idx CURL-FAULT ENDOF
   ;MATCH ;


\ Room in a BUF past the bytes it holds.
: ROOM ( ^n -- ^u8 n )
   {: buf:^ :}
   buf BUF$ {: held:str :}
   held STR:DATA held STR:LEN + buf BUF:CAP@ BUF:BLEN>N held STR:LEN - ;


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
         [: h CURL:CANCEL ;] catch {: code:n :}
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
   {: m:str :}
   JSON:WRITE-OBJECT-START s" jsonrpc" s" 2.0" JSON:WRITE-FIELD-S
   JSON:WRITE-COMMA s" id" REQUEST-ID$ JSON:WRITE-FIELD-RAW
   JSON:WRITE-COMMA s" method" m JSON:WRITE-FIELD-S
   JSON:WRITE-COMMA s" params" JSON:WRITE-KEY JSON:WRITE-OBJECT-START ;

: REQUEST-END ( ^JSON:writer -- ^JSON:writer )
   META-PARAM JSON:WRITE-OBJECT-END JSON:WRITE-OBJECT-END ;

: WRITER ( n -- ^JSON:writer )
   {: idx:n :}
   idx ROW PEER-WRITER idx REQUEST-VEC JSON:OPEN ;

\ tools/list from the cursor's JSON string, or from the start when it is empty.
: LIST-REQUEST ( n str -- )
   {: idx:n next:str :}
   idx WRITER s" tools/list" REQUEST-START
   next STR:LEN 0<> if s" cursor" next JSON:WRITE-FIELD-RAW JSON:WRITE-COMMA then
   REQUEST-END JSON:CLOSE ;

: CALL-REQUEST ( n str str -- )
   {: idx:n name:str args:str :}
   idx WRITER s" tools/call" REQUEST-START
   s" name" name JSON:WRITE-FIELD-S JSON:WRITE-COMMA
   s" arguments" args JSON:WRITE-FIELD-RAW JSON:WRITE-COMMA
   REQUEST-END JSON:CLOSE ;


: PLAIN-NAME? ( str -- bool )
   {: s:str :}
   s STR:LEN 0 ?do
      s STR:DATA i + c@ dup $21 < swap $7E > or if false unloop exit then
   loop true ;

\ The Mcp-Name line, built in the request vector: the name as it is when every
\ byte is visible ASCII, else =?base64?...?=.
: NAME-LINE ( n str -- str )
   {: idx:n name:str :}
   idx REQUEST-VEC {: v:^ :}
   v 0 VEC:RESIZE
   s" Mcp-Name: " v VEC:APPEND
   name PLAIN-NAME? if name v VEC:APPEND else
      s" =?base64?" v VEC:APPEND
      name v name STR:LEN 2 + 3 / 4 * VEC:EXTEND BASE64:ENCODE drop
      s" ?=" v VEC:APPEND
   then
   v VEC$ ;

\ The Authorization line carrying the row's token, built in the request vector.
: BEARER-LINE ( n -- str )
   {: idx:n :}
   idx REQUEST-VEC {: v:^ :}
   v 0 VEC:RESIZE
   s" Authorization: Bearer " v VEC:APPEND
   idx TOKEN-BUF BUF$ v VEC:APPEND
   v VEC$ ;


\ A fresh handle, held by the row, posting the request vector to the URL; a
\ handle a halt left is retired first. E-TRANSFER once REQUEST-STOP stopped the row.
: PREPARE ( n str ms -- CURL:handle )
   {: idx:n line:str limit:ms :}
   idx RETIRE
   idx STOP@ if E-TRANSFER throw then
   CURL:INIT
   MATCH CURL:init-result
      ready OF ENDOF
      failed OF idx CURL-FAULT ENDOF
   ;MATCH {: h:CURL:handle :}
   h OPTION:SOME idx HANDLE!
   h idx URL-BUF BUF$ CURL:URL! idx CHECK
   h idx REQUEST-VEC VEC$ CURL:BODY! idx CHECK
   idx TOKEN-BUF BUF$ STR:LEN 0<> if h idx BEARER-LINE CURL:HEADER+ idx CHECK then
   h limit CURL:TIMEOUT! idx CHECK
   h s" Content-Type: application/json" CURL:HEADER+ idx CHECK
   h s" Accept: application/json, text/event-stream" CURL:HEADER+ idx CHECK
   h s" MCP-Protocol-Version: 2026-07-28" CURL:HEADER+ idx CHECK
   h line CURL:HEADER+ idx CHECK
   h ;


\ Whether a header line is Content-Type naming text/event-stream.
: EVENT-TYPE? ( str -- bool )
   {: s:str :}
   s 58 STR:INDEX-OF MATCH option
      none OF -1 ENDOF
      some OF IDX>N ENDOF
   ;MATCH {: at:n :}
   at 0< if false exit then
   s STR:DATA at >str STR:TRIM s" Content-Type" STR:=CI 0= if false exit then
   s STR:DATA at + 1+ s STR:LEN at - 1- >str MEDIA-ESSENCE
   s" text/event-stream" STR:=CI ;

: EVENT-HEAD? ( str -- bool )
   {: s:str :}
   0 begin dup s STR:LEN < while
      {: at:n :}
      s STR:DATA at + s STR:LEN at - >str 10 STR:INDEX-OF MATCH option
         none OF s STR:LEN at - ENDOF
         some OF IDX>N ENDOF
      ;MATCH {: len:n :}
      s STR:DATA at + len >str EVENT-TYPE? if true exit then
      at len + 1+
   repeat drop false ;

\ Whether the finished reply is an event stream, by its header block.
: STREAM? ( CURL:handle -- bool )
   {: h:CURL:handle :}
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW {: v:^ :}
   [: h v CURL:HEADERS v VEC:BYTES EVENT-HEAD? ;]
   [: v VEC:DELETE ;] finally ;

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
         chunk OF nip LEN>N room swap >str buf BUF+ false ENDOF
         response OF idx ROW PEER-STATUS ! true ENDOF
         failed OF idx CURL-FAULT ENDOF
      ;MATCH
   until
   h STREAM? idx ROW PEER-STREAM ! ;

: LIST-EXCHANGE ( n ms -- n ms )
   {: idx:n limit:ms :}
   idx idx s" Mcp-Method: tools/list" limit PREPARE TRANSFER
   idx limit ;

: CALL-EXCHANGE ( n str ms -- n str ms )
   {: idx:n name:str limit:ms :}
   idx s" Mcp-Method: tools/call" limit PREPARE {: h:CURL:handle :}
   h idx name NAME-LINE CURL:HEADER+ idx CHECK
   idx h TRANSFER
   idx name limit ;

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
      success OF drop JSON:TEXT$ ENDOF
      failure OF drop JSON:TEXT$ ENDOF
      request OF drop drop drop s" " ENDOF
      notification OF drop drop s" " ENDOF
      invalid OF 2drop 2drop s" " ENDOF
   ;MATCH ;

\ Empty data, which a server's priming event carries, is no message.
: >FOUND ( str -- found )
   {: s:str :}
   s STR:LEN 0= if construct found nothing exit then
   s JSON-RPC:>MESSAGE {: m :}
   m RESPONSE-ID$ REQUEST-ID$ STR:= if construct found response exit then
   m MATCH JSON-RPC:message
      invalid OF 2drop 2drop construct found stray ENDOF
      success OF 2drop construct found nothing ENDOF
      failure OF 2drop construct found nothing ENDOF
      request OF drop 2drop construct found nothing ENDOF
      notification OF 2drop construct found nothing ENDOF
   ;MATCH ;

: FOUND! ( ^peer str found -- )
   {: rec:^ data:str k:found :}
   rec PEER-REGION @ SPAN:$ STR:DATA MESSAGE-OFF + CELL-VIEW {: buf:^ :}
   buf BUF:CLEAR data buf BUF+
   k rec PEER-FOUND ! ;

: WHILE-NOTHING ( ^peer str -- )
   {: rec:^ data:str :}
   data >FOUND {: k:found :}
   k MATCH found
      response OF rec data k FOUND! ENDOF
      stray OF rec data k FOUND! ENDOF
      nothing OF ENDOF
   ;MATCH ;

: WHILE-STRAY ( ^peer str -- )
   {: rec:^ data:str :}
   data >FOUND {: k:found :}
   k MATCH found
      response OF rec data k FOUND! ENDOF
      stray OF ENDOF
      nothing OF ENDOF
   ;MATCH ;

\ Keeps in the row's message buffer the first event answering the request,
\ and until one does, the first that is not JSON-RPC.
: EVENT ( ^peer str str -- )
   {: rec:^ kind:str data:str :}
   rec PEER-FOUND @ MATCH found
      nothing OF rec data WHILE-NOTHING ENDOF
      stray OF rec data WHILE-STRAY ENDOF
      response OF ENDOF
   ;MATCH ;

\ Feeds the whole reply to the decoder and finishes it; answers its operands.
: DECODE ( SSE:decoder str ^peer -- SSE:decoder str ^peer )
   {: d:SSE:decoder reply:str rec:^ :}
   d reply rec [: EVENT ;] SSE:FEED
   d SSE:FINISH
   d reply rec ;

\ The reply's JSON-RPC text: its body, or the event EVENT keeps, else a logged
\ E-RESPONSE; the decoder is always closed.
: MESSAGE$ ( n -- str )
   {: idx:n :}
   idx REPLY-BUF BUF$ {: reply:str :}
   idx ROW PEER-STREAM @ 0= if reply exit then
   idx MESSAGE-BUF {: buf:^ :}
   buf BUF:CLEAR
   idx ROW {: rec :}
   construct found nothing rec PEER-FOUND !
   idx DECODER {: d:SSE:decoder :}
   d reply rec [: DECODE ;] catch {: code:n :} drop drop drop
   \ Nothing since OPEN pauses, so a halt cannot skip this CLOSE.
   d SSE:CLOSE
   code SSE:E-UTF8 = if
      s" is an event stream that is not UTF-8" idx E-RESPONSE BAD-REPLY
   then
   code 0<> if code throw then
   rec PEER-FOUND @ MATCH found
      nothing OF
         s" has no event answering the request" idx E-RESPONSE BAD-REPLY
      ENDOF
      stray OF ENDOF
      response OF ENDOF
   ;MATCH
   buf BUF$ ;


\ A member of a JSON object text: its text (a string with quotes), token kind
\ and whether it is there; no object is a logged E-RESPONSE.
: MEMBER ( n str str -- str n bool )
   {: idx:n data:str key:str :}
   idx ROW PEER-CURSOR data JSON:READ-INIT
   JSON:READ-NEXT JSON:T-OBJ <> if
      JSON:READ-CLOSE
      s" has a result that is not a JSON object" idx E-RESPONSE BAD-REPLY
   then
   key JSON:READ-FIND-KEY 0= if
      JSON:READ-CLOSE s" " JSON:T-END false exit
   then
   JSON:READ-TOKEN {: k:n :} RAW$ {: value:str :}
   JSON:READ-CLOSE value k true ;


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
   idx MESSAGE$ {: m:str :}
   m JSON-RPC:>MESSAGE
   MATCH JSON-RPC:message
      success OF {: id value :}
         id JSON:TEXT$ REQUEST-ID$ STR:= 0= if
            s" answers another request" idx E-RESPONSE BAD-REPLY
         then
         value JSON:TEXT$ ENDOF
      request OF drop drop drop
         s" is a request, not a response" idx E-RESPONSE BAD-REPLY ENDOF
      notification OF drop drop
         s" is a notification, not a response" idx E-RESPONSE BAD-REPLY ENDOF
      failure OF drop drop
         s" is a JSON-RPC error" idx E-RESPONSE BAD-REPLY ENDOF
      invalid OF drop {: why:^ wu:n :} drop
         s" is not JSON-RPC" why wu >str idx REPLY-LINE
         E-RESPONSE throw ENDOF
   ;MATCH ;


: GATHER ( n str -- )
   {: idx:n raw:str :}
   idx LIST-BUF {: buf:^ :}
   raw MEMBERS {: inner:str :}
   inner STR:LEN 0= if exit then
   buf BUF:LEN@ BUF:BLEN>N 1 > if 44 buf BUF:APPEND-BYTE then
   inner buf BUF+ ;

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
: PAGE ( n str ms -- str bool )
   {: idx:n next:str limit:ms :}
   idx next LIST-REQUEST
   idx limit [: LIST-EXCHANGE ;] catch {: code:n :} 2drop
   idx code SETTLE
   idx RESULT$ {: result:str :}
   idx result s" tools" MEMBER 0= if
      s" has no tools member" idx E-RESPONSE BAD-REPLY
   then
   JSON:T-ARR <> if
      s" has tools that are not an array" idx E-RESPONSE BAD-REPLY
   then
   {: tools:str :}
   idx result s" nextCursor" MEMBER {: more:bool :} {: k:n :}
   more if k JSON:T-STR <> if
      s" has a nextCursor that is not a string" idx E-RESPONSE BAD-REPLY
   then then
   idx tools GATHER
   more ;

\ Cancels the row's transfer in flight; under LOCK. Answers CURL:CANCEL's
\ throw code, 0 when there was no handle.
: CANCEL-ROW ( n -- n )
   {: idx:n :}
   idx ROW-HANDLE @ MATCH option
      none OF 0 ENDOF
      some OF {: h:CURL:handle :} [: h CURL:CANCEL ;] catch ENDOF
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
   idx url uu >str [: FILL-ROW ;] catch {: code:n :} drop drop
   code 0<> if idx RELEASE code throw then
   who ;


\ Ends a client in any state a throw or a TASK:HALT left it in: a transfer
\ still in flight is cancelled and drained, then the handle, reader and buffers
\ go, the token and the request vector, which carries the Authorization line,
\ wiped first. A caller that must survive a halt keeps the client in its own
\ task storage and closes it in its own finally. E-STATE for a closed client.
: CLOSE ( client -- )
   LIVE {: idx:n :}
   idx RETIRE
   idx TOKEN-BUF BUF:WIPE
   \ RESIZE zeroes the bytes it cuts, and VEC keeps those past COUNT zero.
   idx REQUEST-VEC 0 VEC:RESIZE
   idx RELEASE ;


\ The token the client sends as `Authorization: Bearer <token>` on every
\ exchange until the next TOKEN! or CLOSE; the text is copied, the token it
\ replaces wiped, and an empty one sends none. A token holding CR or LF makes
\ each exchange throw CURL:E-OPERAND. E-STATE for a closed client.
: TOKEN! ( client ^u8 n -- )
   {: who:client token:^ tu:n :}
   who LIVE TOKEN-BUF {: buf:^ :}
   buf BUF:WIPE
   token tu >str buf BUF+ ;


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
   s" " begin idx swap limit start LEFT PAGE while repeat drop
   93 buf BUF:APPEND-BYTE
   buf BUF$ dup STR:DATA swap STR:LEN ;


\ tools/call of the named tool with the arguments' JSON object, within ms for the
\ whole transfer (0: no limit): the result object's JSON text and its isError.
: CALL ( client ^u8 n ^u8 n ms -- ^u8 n bool )
   {: who:client name:^ nu:n args:^ au:n limit:ms :}
   who LIVE {: idx:n :}
   idx REFUSE-STOPPED
   idx name nu >str args au >str CALL-REQUEST
   idx name nu >str limit [: CALL-EXCHANGE ;] catch {: code:n :}
   drop drop drop
   idx code SETTLE
   idx RESULT$ {: result:str :}
   idx result s" isError" MEMBER if
      {: k:n :} drop
      k JSON:T-TRUE <> k JSON:T-FALSE <> and if
         s" has an isError that is not true or false" idx E-RESPONSE BAD-REPLY
      then
      k JSON:T-TRUE =
   else drop drop false then
   {: fault:bool :}
   result STR:DATA result STR:LEN fault ;


\ From any task, stops the client until CLOSE: a TOOLS or CALL whose transfer has not
\ ended throws E-TRANSFER, a later one at once, sending nothing. E-STATE once closed.
: REQUEST-STOP ( client -- )
   HELD-ROW {: idx:n :}
   construct run stopped idx ROW-RUN !
   idx CANCEL-ROW LOCK TASK:RELEASE STOP-CODE ;

;package
