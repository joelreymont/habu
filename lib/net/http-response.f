\ http-response.f - the response a handler builds in its worker's slot: a
\ status, header lines, a borrowed body span or a filed document, and the error
\ answer every refusal shares, rendered by the one hook ON-ERROR installs
\ (lib/net/http.f).
\
\ lib/json-write.f keeps no state of its own: the caller declares the writer and
\ owns the bytes it writes into (docs/stdlib.md, "JSON Write"). Each worker
\ therefore holds one writer of its own over its own JSON buffer, opened at the
\ start of a response body and closed after it, so two workers write JSON at the
\ same time without a lock between them.
require lib/net/http-request.f
require lib/json-write.f
require lib/string.f
require lib/errors.f
require lib/span.f
require lib/aio.f

package HTTP

private

$2D constant DASH-BYTE
10 constant DECIMAL
$30 constant ZERO-BYTE

MAX-WORKERS TYPED-BUFFER JSON-W JSON-WRITE:writer
MAX-WORKERS TYPED-BUFFER JSON-BODY [ ptr JSON-WRITE:writer -- ptr JSON-WRITE:writer ]
$10000 constant FILE-CHUNK
MAX-WORKERS TYPED-BUFFER FILE-OFFSET n
MAX-WORKERS TYPED-BUFFER FILE-TRANSFER AIO:xfer


\ ---- appending to the slot's buffers ----------------------------------------

: OUT+ ( ptr u8 n n -- ) {: src:ptr len:n idx:n :}
   src len idx OUT-BUF idx OUT-U@ SPAN:SKIP SPAN:COPY
   idx OUT-U@ len + idx OUT-U! ;


: OUT+C ( n n -- ) {: byte:n idx:n :}
   byte idx OUT-BUF idx OUT-U@ SPAN:U8!
   idx OUT-U@ 1+ idx OUT-U! ;


: HDR+ ( ptr u8 n n -- ) {: src:ptr len:n idx:n :}
   src len idx HDR-BUF idx HDR-U@ SPAN:SKIP SPAN:COPY
   idx HDR-U@ len + idx HDR-U! ;


: HDR+C ( n n -- ) {: byte:n idx:n :}
   byte idx HDR-BUF idx HDR-U@ SPAN:U8!
   idx HDR-U@ 1+ idx HDR-U! ;


: CRLF+ ( n -- ) {: idx:n :}
   CR idx OUT+C
   LF idx OUT+C ;


: NUM-DIGITS ( n n n -- n ) {: value:n idx:n at:n :}
   value DECIMAL >= if value DECIMAL / idx at RECURSE else at then {: next:n :}
   value DECIMAL mod ZERO-BYTE + idx NUM-BUF next SPAN:U8!
   next 1+ ;


\ One nonnegative number as text in the slot's number scratch, which the next
\ number in the same slot overwrites.
: NUM$ ( n n -- ptr u8 n ) {: value:n idx:n :}
   value 0 < if E-CAPACITY throw then
   value idx 0 NUM-DIGITS {: len:n :}
   idx NUM-BUF len SPAN:TAKE SPAN:$ ;


\ ---- the request id ---------------------------------------------------------

: ID+C ( n n -- ) {: byte:n idx:n :}
   byte idx ID-BUF idx ID-U@ SPAN:U8!
   idx ID-U@ 1+ idx ID-U! ;


: ID+ ( ptr u8 n n -- ) {: src:ptr len:n idx:n :}
   src len idx ID-BUF idx ID-U@ SPAN:SKIP SPAN:COPY
   idx ID-U@ len + idx ID-U! ;


\ The identity one request is reported under: the worker that served it and how
\ many requests that worker has served, which no two live requests share.
: MAKE-ID ( n -- ) {: idx:n :}
   0 idx ID-U!
   s" r" idx ID+
   idx idx NUM$ idx ID+
   DASH-BYTE idx ID+C
   idx NEXT-SEQ idx NUM$ idx ID+ ;


\ ---- status lines -----------------------------------------------------------

: REASON$ ( n -- ptr u8 n ) {: status:n :}
   status 200 = if s" OK" exit then
   status 204 = if s" No Content" exit then
   status 304 = if s" Not Modified" exit then
   status 400 = if s" Bad Request" exit then
   status 404 = if s" Not Found" exit then
   status 405 = if s" Method Not Allowed" exit then
   status 413 = if s" Content Too Large" exit then
   status 426 = if s" Upgrade Required" exit then
   status 431 = if s" Request Header Fields Too Large" exit then
   status 500 = if s" Internal Server Error" exit then
   status 501 = if s" Not Implemented" exit then
   s" Status" ;


: STATUS-LINE ( n -- ) {: idx:n :}
   s" HTTP/1.1 " idx OUT+
   idx SLOT-STATUS@ idx NUM$ idx OUT+
   SP idx OUT+C
   idx SLOT-STATUS@ REASON$ idx OUT+
   idx CRLF+ ;


: CONNECTION-HEADER ( n -- ) {: idx:n :}
   s" Connection: " idx OUT+
   idx KEEP-ALIVE@ 0 <> if s" keep-alive" else s" close" then idx OUT+
   idx CRLF+ ;


\ A 304 carries no body and no length: the cached representation supplies both.
\ A 204 has nothing to send at all, so it carries neither.
: BODYLESS? ( n -- bool ) {: status:n :}
   status 204 = if true exit then
   status 304 = ;


: LENGTH-HEADER ( n -- ) {: idx:n :}
   idx SLOT-STATUS@ BODYLESS? if exit then
   s" Content-Length: " idx OUT+
   idx BODY-LEN@ idx NUM$ idx OUT+
   idx CRLF+ ;


: HEAD-BYTES ( n -- ) {: idx:n :}
   0 idx OUT-U!
   idx STATUS-LINE
   idx HDR-BUF idx HDR-U@ SPAN:TAKE SPAN:$ idx OUT+
   idx CONNECTION-HEADER
   idx LENGTH-HEADER
   idx CRLF+ ;


: WRITE-SPAN ( TCP4:connection ptr u8 n -- bool )
   {: conn:TCP4:connection bytes:ptr len:n :}
   len 0= if true exit then
   conn bytes len TCP4:TRANSFER-BYTES TCP4:WRITE
   MATCH TCP4:status
      ok OF true ENDOF
      failed OF drop false ENDOF
   ;MATCH ;


\ The transfer owns the allocation between READ and AWAIT-XFER. If a worker
\ ends there, AIO's task cleanup cancels the operation and releases its bytes
\ at completion; CLOSE-FILE must only free bytes the worker owns again.
: SUBMIT-FILE-READ ( n -- n ) {: idx:n :}
   idx FILE-FD @ idx FILE-BUFFER @ idx FILE-ALLOCATION @
   idx BODY-LEN@ idx FILE-OFFSET @ - FILE-CHUNK min
   idx FILE-OFFSET @ AIO:READ idx FILE-TRANSFER !
   idx ;


: FILE-READ ( n -- n ) {: idx:n :}
   0 idx FILE-OWNED !
   idx [: SUBMIT-FILE-READ ;] catch {: code:n :} drop
   code 0<> if
      code E-AIO-ENTER <> if 1 idx FILE-OWNED ! then
      -1 exit
   then
   idx FILE-TRANSFER @ AIO:AWAIT-XFER
   {: bytes cap:NUM:alloc-byte-len outcome:AIO:outcome :}
   bytes idx FILE-BUFFER !
   cap idx FILE-ALLOCATION !
   1 idx FILE-OWNED !
   outcome MATCH AIO:outcome
      ready OF ENDOF
      timed-out OF -1 ENDOF
      cancelled OF -1 ENDOF
      refused OF drop -1 ENDOF
   ;MATCH ;


: SEND-FILE ( TCP4:connection n -- bool ) {: conn:TCP4:connection idx:n :}
   0 idx FILE-OFFSET !
   begin idx FILE-OFFSET @ idx BODY-LEN@ < while
      idx FILE-READ {: got:n :}
      got 0 <= if false exit then
      conn idx FILE-BUFFER @ got WRITE-SPAN 0= if false exit then
      idx FILE-OFFSET @ got + idx FILE-OFFSET !
   repeat
   true ;


\ The head is followed by either a borrowed span or a streamed file. HEAD has
\ the same length as GET and sends no body. A short file or a failed read closes
\ the connection rather than presenting a truncated document as complete.
: SEND-BODY ( TCP4:connection response -- bool ) {: conn:TCP4:connection subject:response :}
   subject SLOT-OF-RESPONSE {: idx:n :}
   idx HEAD-BYTES
   conn idx OUT-BUF idx OUT-U@ SPAN:TAKE SPAN:$ WRITE-SPAN 0= if false exit then
   idx SLOT-STATUS@ BODYLESS? if true exit then
   idx SLOT-METHOD$ s" HEAD" STR= if true exit then
   idx FILE-LIVE @ 0<> if conn idx SEND-FILE exit then
   conn idx BODY-PTR@ idx BODY-LEN@ WRITE-SPAN ;


: SEND ( TCP4:connection response -- bool )
   [: SEND-BODY ;] [: SELF-SLOT CLOSE-FILE ;] finally ;


\ ---- JSON -------------------------------------------------------------------

\ The slot's writer over the slot's JSON bytes, empty and ready to be chained.
: OPEN-JSON ( n -- ) {: idx:n :}
   idx JSON-W idx JSON-BUF SPAN:$ JSON-WRITE:OPEN drop ;


\ Whatever the body left behind, the writer is done with: a closed writer
\ refuses every emitter, so a body quotation that kept its writer cannot write
\ into the next request's answer.
: CLOSE-JSON ( n -- ) {: idx:n :}
   idx JSON-W JSON-WRITE:CLOSE ;


\ The body's chain, from the slot's writer to the bytes it wrote, which become
\ the response body in the slot's own buffer. A quotation cannot read a local,
\ so the body travels through the slot.
: WRITE-JSON ( -- )
   SELF-SLOT {: idx:n :}
   idx JSON-W idx JSON-BODY @ execute
   JSON-WRITE:$ idx RESPONSE-BODY! ;


\ A body larger than the slot's JSON buffer is the writer's E-JW-CAPACITY, which
\ is this package's own E-RESPONSE seen from the other side of the boundary: a
\ handler that lets it through is answered 500 response_too_large by the
\ worker, never a truncated body.
: JSON-FAULT ( n -- ) {: code:n :}
   code 0= if exit then
   code E-JW-CAPACITY = if E-RESPONSE throw then
   code throw ;


public

: STATUS! ( response n -- ) {: subject:response status:n :}
   subject SLOT-OF-RESPONSE {: idx:n :}
   status idx SLOT-STATUS! ;


\ One header line. The caller owns the name and the value only for this call.
: HEADER! ( response ptr u8 n ptr u8 n -- )
   {: subject:response name:ptr namelen:n value:ptr valuelen:n :}
   subject SLOT-OF-RESPONSE {: idx:n :}
   name namelen idx HDR+
   COLON idx HDR+C
   SP idx HDR+C
   value valuelen idx HDR+
   CR idx HDR+C
   LF idx HDR+C ;


\ The body is BORROWED: it must stay valid until the response has been sent,
\ which a literal, a loaded static file and the slot's own JSON buffer all are.
: BODY! ( response ptr u8 n -- ) {: subject:response bytes:ptr len:n :}
   subject SLOT-OF-RESPONSE {: idx:n :}
   bytes len idx RESPONSE-BODY! ;


\ A filed document is immutable while it is served. The response takes the
\ descriptor its caller opened and checked, with the byte count to send, and
\ owns it and one bounded buffer until SEND finishes or the worker exits: what
\ is sent is read from the file the caller checked, never from a path opened
\ again. The caller supplies the content type and download name.
: FILE! ( response fd n -- ) {: subject:response source:fd size:n :}
   subject SLOT-OF-RESPONSE {: idx:n :}
   idx SELF-SLOT <> if E-HANDLE throw then
   idx CLOSE-FILE
   source idx FILE-FD !
   1 idx FILE-LIVE !
   size idx BODY-LEN !
   FILE-CHUNK MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES
   idx FILE-ALLOCATION ! idx FILE-BUFFER !
   1 idx FILE-OWNED ! ;


\ A JSON body written by `body` through the worker's own writer and kept in the
\ worker's own buffer, with the Content-Type this word adds itself. Only the
\ worker that owns the slot may build its response. The writer is closed however
\ the body ended, and a body that did not finish is no answer at all.
: JSON! ( response [ ptr JSON-WRITE:writer -- ptr JSON-WRITE:writer ] -- )
   {: subject:response body :}
   subject SLOT-OF-RESPONSE {: idx:n :}
   idx SELF-SLOT <> if E-HANDLE throw then
   body idx JSON-BODY !
   idx OPEN-JSON
   [: WRITE-JSON ;] catch {: code:n :}
   idx CLOSE-JSON
   code JSON-FAULT
   subject s" Content-Type" s" application/json" HEADER! ;


private

\ The whole error answer as text in the slot's body buffer: the code, the
\ message and the request id the fault was reported under, on one line.
: TEXT-AT ( n ptr u8 n n -- n ) {: at:n src:ptr len:n idx:n :}
   src len idx JSON-BUF at SPAN:SKIP SPAN:COPY
   at len + ;


: PLAIN-ERROR ( response n ptr u8 n ptr u8 n -- )
   {: subject:response status:n code:ptr codelen:n message:ptr messagelen:n :}
   subject SLOT-OF-RESPONSE {: idx:n :}
   idx SELF-SLOT <> if E-HANDLE throw then
   subject status STATUS!
   0 code codelen idx TEXT-AT
   s" : " idx TEXT-AT
   message messagelen idx TEXT-AT
   s"  (request " idx TEXT-AT
   idx SLOT-ID$ idx TEXT-AT
   S\" )\n" idx TEXT-AT {: len:n :}
   subject s" Content-Type" s" text/plain; charset=utf-8" HEADER!
   idx JSON-BUF len SPAN:TAKE SPAN:$ idx RESPONSE-BODY! ;


\ The quotation lives in a row declared to hold one, the checker's proven
\ quotation store (docs/threads.md): an xt in a plain cell would lose the effect.
1 TYPED-BUFFER ERROR-HOOK [ response n ptr u8 n ptr u8 n -- ]


: PLAIN-ERRORS ( -- )
   [: PLAIN-ERROR ;] 0 ERROR-HOOK ! ;

PLAIN-ERRORS


public

\ The id this request is reported under, which is what an error answer quotes
\ so a client's report and the server's own line name one request.
: REQUEST-ID$ ( response -- ptr u8 n )
   SLOT-OF-RESPONSE SLOT-ID$ ;


\ Every refusal and fault this server answers, and any a handler answers
\ itself, as the installed hook renders it; plain text until ON-ERROR says
\ otherwise.
: ERROR! ( response n ptr u8 n ptr u8 n -- )
   0 ERROR-HOOK @ execute ;


;package
