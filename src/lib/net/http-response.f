\ http-response.f - the response a handler builds in its worker's slot: a
\ status, header lines, a borrowed body span, and the error answer every
\ rejection shares, rendered by the one hook ON-ERROR installs
\ (src/lib/net/http.f).
require src/lib/net/http-request.f
require src/lib/json.f
require src/lib/vector.f
require src/lib/string.f
require src/lib/error.f
require src/lib/span.f
require src/lib/net/http-arena.f
require src/lib/byte-buffer.f
require src/lib/json/write.f

package HTTP

private

$2D constant DASH-BYTE
10 constant DECIMAL
$30 constant ZERO-BYTE

MAX-WORKERS TYPED-BUFFER JSON-W JSON:writer SCRATCH-ONLY


: OUT+ ( str n -- ) {: src:str idx:n :}
   src STR:DATA src STR:LEN BUF:N>BLEN idx OUT-BUF BUF:APPEND-SPAN ;


: OUT+C ( n n -- ) {: byte:n idx:n :}
   byte idx OUT-BUF BUF:APPEND-BYTE ;


: HDR+ ( str n -- ) {: src:str idx:n :}
   src STR:DATA src STR:LEN BUF:N>BLEN idx HDR-BUF BUF:APPEND-SPAN ;


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
: NUM$ ( n n -- str ) {: value:n idx:n :}
   value 0 < if E-CAPACITY throw then
   value idx 0 NUM-DIGITS {: len:n :}
   idx NUM-BUF len SPAN:TAKE SPAN:$ ;


: ID+C ( n n -- ) {: byte:n idx:n :}
   byte idx ID-BUF idx ID-U@ SPAN:U8!
   idx ID-U@ 1+ idx ID-U! ;


: ID+ ( str n -- ) {: src:str idx:n :}
   src idx ID-BUF idx ID-U@ SPAN:SKIP SPAN:COPY
   idx ID-U@ src STR:LEN + idx ID-U! ;


\ The identity one request is reported under: the worker that served it and how
\ many requests that worker has served, which no two live requests share.
: MAKE-ID ( n -- ) {: idx:n :}
   mono-ns idx START-NS!
   construct access unanswered idx ACCESS-STATE!
   0 idx ID-U!
   s" r" idx ID+
   idx idx NUM$ idx ID+
   DASH-BYTE idx ID+C
   idx NEXT-SEQ idx NUM$ idx ID+ ;


\ The reason phrase of each code in IANA's HTTP status code registry. An
\ unregistered code gets an empty phrase, which RFC 9112 section 4 allows.
: REASON$ ( n -- str )
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
      s" " swap
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


\ An answer sent as its head alone: a bodyless status, or HEAD, which has the
\ same length as GET.
: NO-BODY? ( n -- bool ) {: idx:n :}
   idx SLOT-STATUS@ BODYLESS? if true exit then
   idx SLOT-METHOD$ s" HEAD" STR:= ;


: SEND ( TCP4:connection response -- bool ) {: conn:TCP4:connection subject:response :}
   subject SLOT-OF-RESPONSE {: idx:n :}
   idx HEAD-BYTES
   conn idx OUT-BUF BUF$ WRITE-SPAN 0= if false exit then
   idx NO-BODY? if true exit then
   conn idx BODY-PTR@ idx BODY-LEN@ >str WRITE-SPAN ;


\ The slot's writer over the slot's JSON vector, empty and ready to be chained.
\ Each worker holds its own writer and vector (src/lib/json/write.f keeps no
\ state), so two workers write JSON at once without a lock between them.
\ The writer may move the bytes a previous body borrowed, so that body goes
\ first: a body that does not finish leaves none.
: OPEN-JSON ( n -- ) {: idx:n :}
   idx DROP-BODY
   idx JSON-W idx JSON-OUT @ JSON:OPEN drop ;


\ Whatever the body left behind, the writer is done with: a closed writer
\ rejects every emitter, so a body quotation that kept its writer cannot write
\ into the next request's answer.
: CLOSE-JSON ( n -- ) {: idx:n :}
   idx JSON-W JSON:CLOSE ;


public

: STATUS! ( response n -- ) {: subject:response status:n :}
   subject SLOT-OF-RESPONSE {: idx:n :}
   status idx SLOT-STATUS! ;


\ One header line. The caller owns the name and the value only for this call.
: HEADER! ( response str str -- )
   {: subject:response name:str value:str :}
   subject SLOT-OF-RESPONSE {: idx:n :}
   name idx HDR+
   COLON idx HDR+C
   SP idx HDR+C
   value idx HDR+
   CR idx HDR+C
   LF idx HDR+C ;


\ The body is BORROWED: it must stay valid until the response has been sent,
\ which a literal, a loaded static file and the slot's own JSON vector all are.
: BODY! ( response str -- ) {: subject:response bytes:str :}
   subject SLOT-OF-RESPONSE {: idx:n :}
   bytes idx RESPONSE-BODY! ;


\ A JSON body written by `body` through the worker's own writer and kept in the
\ worker's own vector, with the Content-Type this word adds itself. Only the
\ worker that owns the slot may build its response. The writer is closed however
\ the body ended, and a body that did not finish is no answer at all.
: JSON! ( response [ ^JSON:writer -- ^JSON:writer ] -- )
   {: subject:response body :}
   subject SLOT-OF-RESPONSE {: idx:n :}
   idx SELF-SLOT <> if E-HANDLE throw then
   idx OPEN-JSON
   [: idx JSON-W body execute
      JSON:WRITTEN$ idx RESPONSE-BODY! ;]
   [: idx CLOSE-JSON ;] finally
   subject s" Content-Type" s" application/json" HEADER! ;


private

\ The whole error answer as text in the slot's JSON vector: the code, the
\ message and the request id the fault was reported under, on one line.
: ERROR-TEXT+ ( str n -- ) {: src:str idx:n :}
   src idx JSON-OUT @ VEC:APPEND ;


: PLAIN-ERROR ( response n str str -- )
   {: subject:response status:n code:str message:str :}
   subject SLOT-OF-RESPONSE {: idx:n :}
   subject status STATUS!
   idx JSON-OUT @ 0 VEC:RESIZE
   code idx ERROR-TEXT+
   s" : " idx ERROR-TEXT+
   message idx ERROR-TEXT+
   s"  (request " idx ERROR-TEXT+
   idx SLOT-ID$ idx ERROR-TEXT+
   S\" )\n" idx ERROR-TEXT+
   subject s" Content-Type" s" text/plain; charset=utf-8" HEADER!
   idx JSON-OUT @ VEC:BYTES idx RESPONSE-BODY! ;


\ The quotation lives in a row declared to hold one, the checker's proven
\ quotation store (src/lib/task.f): an xt in a plain cell would lose the effect.
1 TYPED-BUFFER ERROR-HOOK [ response n str str -- ]


: PLAIN-ERRORS ( -- )
   [: PLAIN-ERROR ;] 0 ERROR-HOOK ! ;

PLAIN-ERRORS


\ A header line that describes the body: its name begins `Content-`, in any
\ case.
: BODY-HEADER? ( str -- bool ) {: line:str :}
   s" Content-" {: prefix:str :}
   line STR:LEN prefix STR:LEN < if false exit then
   line STR:DATA prefix STR:LEN >str prefix STR:=CI ;


\ The slot's header lines from `from` on, each moved down to `to` unless it
\ describes the body; answers the length kept. A move only goes down, so the
\ lines still to read are intact.
: KEEP-LINES ( n n n -- n ) {: idx:n from:n to:n :}
   idx HDR-BUF BUF:SPAN {: lines :}
   from lines SPAN:LEN >= if to exit then
   lines SPAN:$ LF from STR:SPLIT-NEXT drop {: line:str next:n :}
   line BODY-HEADER? if idx next to RECURSE exit then
   next lines SPAN:LEN min from - {: len:n :}
   lines from len SPAN:SUB SPAN:$ lines to len SPAN:SUB SPAN:COPY
   idx next to len + RECURSE ;


: DROP-BODY-HEADERS ( n -- ) {: idx:n :}
   idx 0 0 KEEP-LINES BUF:N>BLEN idx HDR-BUF BUF:LEN! ;


: IN-REQUEST? ( n -- bool )
   ACCESS-STATE@
   MATCH access
      unrequested OF false ENDOF
      unanswered OF true ENDOF
      undelivered OF true ENDOF
      delivered OF true ENDOF
      handed-off OF true ENDOF
   ;MATCH ;


public

\ The id this request is reported under, which is what an error answer quotes
\ so a client's report and the server's own line name one request.
: REQUEST-ID$ ( response -- str )
   SLOT-OF-RESPONSE SLOT-ID$ ;


\ The TIME:MONO-NS reading the request began at, which HTTP's access line
\ counts its milliseconds from.
: REQUEST-START-NS ( response -- n )
   SLOT-OF-RESPONSE START-NS@ ;


\ On a worker serving a request, handed off or not, that request's REQUEST-ID$
\ and true; anywhere else, a SUBMIT call included, an empty string and false.
: CURRENT-ID$ ( -- str bool )
   MY-SLOT @ 1- {: idx:n :}
   idx 0 < if s" " false exit then
   idx IN-REQUEST? 0= if s" " false exit then
   idx SLOT-ID$ true ;


\ Every rejection and fault this server answers, and any a handler answers
\ itself, as the installed hook renders it; plain text until ON-ERROR says
\ otherwise. Every hook renders on the answer less its body and the `Content-*`
\ headers that describe it; the other headers the handler wrote stay.
: ERROR! ( response n str str -- )
   {: subject:response status:n code:str message:str :}
   subject SLOT-OF-RESPONSE {: idx:n :}
   idx SELF-SLOT <> if E-HANDLE throw then
   idx DROP-BODY
   idx DROP-BODY-HEADERS
   subject status code message 0 ERROR-HOOK @ execute ;


;package
