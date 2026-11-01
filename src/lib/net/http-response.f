\ http-response.f - the response a handler builds in its worker's slot: a
\ status, header lines, a borrowed body span or a filed document, and the error
\ answer every refusal shares, rendered by the one hook ON-ERROR installs
\ (src/lib/net/http.f).
require src/lib/net/http-request.f
require src/lib/json-write.f
require src/lib/string.f
require src/lib/errors.f
require src/lib/span.f
require src/lib/aio.f

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


: OUT+ ( ptr u8 n n -- ) {: src:ptr len:n idx:n :}
   src len BUF:N>BLEN idx OUT-BUF BUF:APPEND-SPAN ;


: OUT+C ( n n -- ) {: byte:n idx:n :}
   byte idx OUT-BUF BUF:APPEND-BYTE ;


: HDR+ ( ptr u8 n n -- ) {: src:ptr len:n idx:n :}
   src len BUF:N>BLEN idx HDR-BUF BUF:APPEND-SPAN ;


: HDR+C ( n n -- ) {: byte:n idx:n :}
   byte idx HDR-BUF BUF:APPEND-BYTE ;


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


\ The reason phrase of each code in IANA's HTTP status code registry. An
\ unregistered code gets an empty phrase, which RFC 9112 section 4 allows.
: REASON$ ( n -- ptr u8 n )
   case
      100 of s" Continue" endof
      101 of s" Switching Protocols" endof
      102 of s" Processing" endof
      103 of s" Early Hints" endof
      200 of s" OK" endof
      201 of s" Created" endof
      202 of s" Accepted" endof
      203 of s" Non-Authoritative Information" endof
      204 of s" No Content" endof
      205 of s" Reset Content" endof
      206 of s" Partial Content" endof
      207 of s" Multi-Status" endof
      208 of s" Already Reported" endof
      226 of s" IM Used" endof
      300 of s" Multiple Choices" endof
      301 of s" Moved Permanently" endof
      302 of s" Found" endof
      303 of s" See Other" endof
      304 of s" Not Modified" endof
      305 of s" Use Proxy" endof
      307 of s" Temporary Redirect" endof
      308 of s" Permanent Redirect" endof
      400 of s" Bad Request" endof
      401 of s" Unauthorized" endof
      402 of s" Payment Required" endof
      403 of s" Forbidden" endof
      404 of s" Not Found" endof
      405 of s" Method Not Allowed" endof
      406 of s" Not Acceptable" endof
      407 of s" Proxy Authentication Required" endof
      408 of s" Request Timeout" endof
      409 of s" Conflict" endof
      410 of s" Gone" endof
      411 of s" Length Required" endof
      412 of s" Precondition Failed" endof
      413 of s" Content Too Large" endof
      414 of s" URI Too Long" endof
      415 of s" Unsupported Media Type" endof
      416 of s" Range Not Satisfiable" endof
      417 of s" Expectation Failed" endof
      421 of s" Misdirected Request" endof
      422 of s" Unprocessable Content" endof
      423 of s" Locked" endof
      424 of s" Failed Dependency" endof
      425 of s" Too Early" endof
      426 of s" Upgrade Required" endof
      428 of s" Precondition Required" endof
      429 of s" Too Many Requests" endof
      431 of s" Request Header Fields Too Large" endof
      451 of s" Unavailable For Legal Reasons" endof
      500 of s" Internal Server Error" endof
      501 of s" Not Implemented" endof
      502 of s" Bad Gateway" endof
      503 of s" Service Unavailable" endof
      504 of s" Gateway Timeout" endof
      505 of s" HTTP Version Not Supported" endof
      506 of s" Variant Also Negotiates" endof
      507 of s" Insufficient Storage" endof
      508 of s" Loop Detected" endof
      511 of s" Network Authentication Required" endof
      s" " rot
   endcase ;


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
   idx OUT-BUF BUF:CLEAR
   idx STATUS-LINE
   idx HDR-BUF BUF$ idx OUT+
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
   conn idx OUT-BUF BUF$ WRITE-SPAN 0= if false exit then
   idx SLOT-STATUS@ BODYLESS? if true exit then
   idx SLOT-METHOD$ s" HEAD" STR= if true exit then
   idx FILE-LIVE @ 0<> if conn idx SEND-FILE exit then
   conn idx BODY-PTR@ idx BODY-LEN@ WRITE-SPAN ;


: SEND ( TCP4:connection response -- bool )
   [: SEND-BODY ;] [: SELF-SLOT CLOSE-FILE ;] finally ;


\ The slot's writer over the slot's JSON buffer, empty and ready to be chained.
\ Each worker holds its own writer and buffer (src/lib/json-write.f keeps no
\ state), so two workers write JSON at once without a lock between them.
\ The writer may move the bytes a previous body borrowed, so that body goes
\ first: a body that does not finish leaves none.
: OPEN-JSON ( n -- ) {: idx:n :}
   idx DROP-BODY
   idx JSON-W idx JSON-BUF JSON-WRITE:OPEN-BUF drop ;


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
   [: WRITE-JSON ;] [: SELF-SLOT CLOSE-JSON ;] finally
   subject s" Content-Type" s" application/json" HEADER! ;


private

\ The whole error answer as text in the slot's JSON buffer: the code, the
\ message and the request id the fault was reported under, on one line.
: ERROR-TEXT+ ( ptr u8 n n -- ) {: src:ptr len:n idx:n :}
   src len BUF:N>BLEN idx JSON-BUF BUF:APPEND-SPAN ;


: PLAIN-ERROR ( response n ptr u8 n ptr u8 n -- )
   {: subject:response status:n code:ptr codelen:n message:ptr messagelen:n :}
   subject SLOT-OF-RESPONSE {: idx:n :}
   idx SELF-SLOT <> if E-HANDLE throw then
   subject status STATUS!
   idx DROP-BODY
   idx JSON-BUF BUF:CLEAR
   code codelen idx ERROR-TEXT+
   s" : " idx ERROR-TEXT+
   message messagelen idx ERROR-TEXT+
   s"  (request " idx ERROR-TEXT+
   idx SLOT-ID$ idx ERROR-TEXT+
   S\" )\n" idx ERROR-TEXT+
   subject s" Content-Type" s" text/plain; charset=utf-8" HEADER!
   idx JSON-BUF BUF$ idx RESPONSE-BODY! ;


\ The quotation lives in a row declared to hold one, the checker's proven
\ quotation store (src/lib/task.f): an xt in a plain cell would lose the effect.
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
