\ http-request.f - one HTTP/1.1 request read off a connection into a worker's
\ slot: the request line, the header block, and a Content-Length or chunked
\ body. Every span the parser records addresses the slot's own input buffer,
\ so the request survives until the worker starts the next one.
\
\ The limits are answered, not thrown: a header block past HEAD-CAP is 431, a
\ body past BODY-CAP is 413, a transfer coding other than chunked is 501, and
\ anything the grammar refuses is 400. One absolute deadline covers the whole
\ request, and every wait for the peer is a readiness poll on the AIO loop
\ (TCP4's READABLE-WITHIN? is an AIO:POLL and an AIO:AWAIT, docs/aio.md),
\ which parks the task and not its thread until the peer speaks or that
\ deadline passes.
require lib/net/http-arena.f
require lib/string.f
require lib/adt/option.f
require lib/net/tcp4.f
require lib/json-read.f
require lib/num-types.f
require lib/span.f

package HTTP

private

\ How one request ended: complete, refused with a status (400, 431, 413, 501),
\ or over before a request arrived (the peer left, the idle deadline passed,
\ the connection failed).
SUMTYPE parse-result 0
   VARIANT complete ;VARIANT
   VARIANT malformed ;VARIANT
   VARIANT head-too-large ;VARIANT
   VARIANT body-too-large ;VARIANT
   VARIANT unsupported ;VARIANT
   VARIANT ended ;VARIANT
   VARIANT late ;VARIANT
   VARIANT broken ;VARIANT
;SUMTYPE


: COMPLETE? ( parse-result -- bool )
   MATCH parse-result
      complete OF true ENDOF
      malformed OF false ENDOF
      head-too-large OF false ENDOF
      body-too-large OF false ENDOF
      unsupported OF false ENDOF
      ended OF false ENDOF
      late OF false ENDOF
      broken OF false ENDOF
   ;MATCH ;


\ The status a refused request is answered with; a result nothing answers
\ (the peer left, or the connection failed) has no status.
: REFUSAL-STATUS ( parse-result -- n )
   MATCH parse-result
      complete OF 0 ENDOF
      malformed OF 400 ENDOF
      head-too-large OF 431 ENDOF
      body-too-large OF 413 ENDOF
      unsupported OF 501 ENDOF
      ended OF 0 ENDOF
      late OF 0 ENDOF
      broken OF 0 ENDOF
   ;MATCH ;


ENUM fill-outcome filled ended late broken ;ENUM

CAST: BLEN>N ( NUM:byte-len -- n )

13 constant CR
10 constant LF
32 constant SP
9 constant TAB
58 constant COLON
37 constant PERCENT
47 constant SLASH
63 constant QUESTION
44 constant CH-COMMA
59 constant SEMICOLON
1000000 constant NS-PER-MS
16 constant HEX-BASE
$7FFFFFF constant MAX-CHUNK        \ a chunk size larger than any body we accept


: OK ( -- parse-result )          construct parse-result complete ;
: BAD ( -- parse-result )         construct parse-result malformed ;
: TOO-MUCH-HEAD ( -- parse-result ) construct parse-result head-too-large ;
: TOO-MUCH-BODY ( -- parse-result ) construct parse-result body-too-large ;
: NOT-SUPPORTED ( -- parse-result ) construct parse-result unsupported ;
: CLOSED ( -- parse-result )      construct parse-result ended ;
: TOO-LATE ( -- parse-result )    construct parse-result late ;
: FAULTED ( -- parse-result )     construct parse-result broken ;


: FLAG>N ( bool -- n )
   if 1 else 0 then ;


: AT-BYTE ( n n -- n ) {: idx:n at:n :}
   idx IN-BUF at SPAN:U8@ ;


: SPAN$ ( n n n -- ptr u8 n ) {: idx:n at:n len:n :}
   idx IN-BUF at len SPAN:SUB SPAN:$ ;


\ ---- reading ----------------------------------------------------------------

: DEADLINE-AT ( n -- n ) {: ms:n :}
   mono-ns ms NS-PER-MS * + ;


\ What is left of the request's deadline, as the timed poll takes it: a
\ deadline already passed asks the question once without waiting.
: REMAINING ( n -- ms ) {: deadline:n :}
   deadline mono-ns - {: left:n :}
   left 0 <= if 0 >MS exit then
   left NS-PER-MS / >MS ;


\ Parks the task in poll(2) until the peer speaks or the deadline passes.
: WAIT-READABLE ( TCP4:connection n -- bool )
   {: conn:TCP4:connection deadline:n :}
   conn deadline REMAINING TCP4:READABLE-WITHIN?
   MATCH TCP4:ready-result
      ready OF true ENDOF
      idle OF false ENDOF
      failed OF drop true ENDOF
   ;MATCH ;


\ Consumed bytes are dropped, but the head the handler still reads is not: the
\ stream window starts where the current head ends.
: COMPACT ( n -- ) {: idx:n :}
   idx HEAD-U@ {: base:n :}
   idx IN-AT@ base <= if exit then
   idx IN-U@ idx IN-AT@ - {: left:n :}
   idx IN-BUF idx IN-AT@ left SPAN:SUB SPAN:$ idx IN-BUF base SPAN:SKIP SPAN:COPY
   base idx IN-AT!
   base left + idx IN-U! ;


: READ-INTO ( TCP4:connection n -- fill-outcome ) {: conn:TCP4:connection idx:n :}
   conn idx IN-BUF idx IN-U@ SPAN:SKIP SPAN:$ TCP4:TRANSFER-BYTES TCP4:READ
   MATCH TCP4:read-result
      data OF BLEN>N idx IN-U@ + idx IN-U! construct fill-outcome filled ENDOF
      closed OF drop construct fill-outcome ended ENDOF
      failed OF drop construct fill-outcome broken ENDOF
   ;MATCH ;


\ One transfer into the slot's window. The caller proves there is room for it.
: FILL ( TCP4:connection n n -- fill-outcome ) {: conn:TCP4:connection idx:n deadline:n :}
   idx IN-U@ IN-CAP >= if E-CAPACITY throw then
   conn deadline WAIT-READABLE 0= if construct fill-outcome late exit then
   conn idx READ-INTO ;


: AVAIL ( n -- n ) {: idx:n :}
   idx IN-U@ idx IN-AT@ - ;


\ ---- bytes and tokens -------------------------------------------------------

: CRLF-AT? ( n n -- bool ) {: idx:n at:n :}
   at 1+ idx IN-U@ >= if false exit then
   idx at AT-BYTE CR <> if false exit then
   idx at 1+ AT-BYTE LF = ;


\ The offset just past the blank line that ends the head, or -1 while it is
\ absent. The scan restarts at the first byte after every fill, over a head
\ HEAD-CAP already bounds.
: HEAD-END ( n -- n ) {: idx:n :}
   idx IN-U@ 3 - {: limit:n :}
   limit 0 < if -1 exit then
   limit 1+ 0 ?do
      idx i CRLF-AT? if
         idx i 2 + CRLF-AT? if i 4 + unloop exit then
      then
   loop
   -1 ;


: TOKEN-BYTE? ( n -- bool ) {: c:n :}
   c $41 >= c $5A <= and if true exit then
   c $61 >= c $7A <= and if true exit then
   c $30 >= c $39 <= and if true exit then
   c $21 = c $23 = or c $24 = or c $25 = or c $26 = or if true exit then
   c $27 = c $2A = or c $2B = or c $2D = or c $2E = or if true exit then
   c $5E = c $5F = or c $60 = or c $7C = or c $7E = or ;


: TOKEN? ( n n n -- bool ) {: idx:n at:n len:n :}
   len 0 <= if false exit then
   len 0 ?do
      idx at i + AT-BYTE TOKEN-BYTE? 0= if false unloop exit then
   loop
   true ;


: HEX-BYTE? ( n -- bool ) {: c:n :}
   c $30 >= c $39 <= and if true exit then
   c $41 >= c $46 <= and if true exit then
   c $61 >= c $66 <= and ;


: HEX-VALUE ( n -- n ) {: c:n :}
   c $39 <= if c $30 - exit then
   c $46 <= if c $37 - exit then
   c $57 - ;


\ Percent escapes are validated once, here, so a route that decodes a bound
\ segment later can trust every escape it meets.
: ESCAPES-OK? ( n n n -- bool ) {: idx:n at:n len:n :}
   len 0 ?do
      idx at i + AT-BYTE PERCENT = if
         i 2 + len >= if false unloop exit then
         idx at i + 1+ AT-BYTE HEX-BYTE? 0= if false unloop exit then
         idx at i + 2 + AT-BYTE HEX-BYTE? 0= if false unloop exit then
      then
   loop
   true ;


: BYTE-INDEX ( n n n n -- n ) {: idx:n at:n len:n c:n :}
   len 0 ?do
      idx at i + AT-BYTE c = if i unloop exit then
   loop
   -1 ;


: TRIM-LEFT ( n n n -- n n ) {: idx:n at:n len:n :}
   len 0 <= if at len exit then
   idx at AT-BYTE {: c:n :}
   c SP <> c TAB <> and if at len exit then
   idx at 1+ len 1- RECURSE ;


: TRIM-RIGHT ( n n n -- n n ) {: idx:n at:n len:n :}
   len 0 <= if at len exit then
   idx at len + 1- AT-BYTE {: c:n :}
   c SP <> c TAB <> and if at len exit then
   idx at len 1- RECURSE ;


: TRIM-OWS ( n n n -- n n ) {: idx:n at:n len:n :}
   idx at len TRIM-LEFT {: la:n ll:n :}
   idx la ll TRIM-RIGHT ;


\ ---- the request line -------------------------------------------------------

: LINE-END ( n n -- n ) {: idx:n at:n :}
   idx HEAD-U@ 1- {: limit:n :}
   limit at ?do
      idx i CRLF-AT? if i unloop exit then
   loop
   -1 ;


: VERSION-OK? ( n n n -- bool ) {: idx:n at:n len:n :}
   idx at len SPAN$ s" HTTP/1.1" STR= if true exit then
   idx at len SPAN$ s" HTTP/1.0" STR= ;


: SPLIT-TARGET ( n n n -- ) {: idx:n at:n len:n :}
   idx at len QUESTION BYTE-INDEX {: mark:n :}
   mark 0 < if
      at len idx PATH!
      at len + 0 idx QUERY!
      exit
   then
   at mark idx PATH!
   at mark + 1+ len mark - 1- idx QUERY! ;


: TARGET-OK? ( n n n -- bool ) {: idx:n at:n len:n :}
   len 0 <= if false exit then
   idx at AT-BYTE SLASH <> if false exit then
   idx at len ESCAPES-OK? ;


\ METHOD SP TARGET SP VERSION: two spaces exactly, and a version this server
\ speaks. A third space leaves the version unequal to either spelling.
: PARSE-REQUEST-LINE ( n n -- bool ) {: idx:n end:n :}
   idx 0 end SP BYTE-INDEX {: first:n :}
   first 1 < if false exit then
   idx first 1+ end first - 1- SP BYTE-INDEX {: gap:n :}
   gap 1 < if false exit then
   first 1+ gap + {: second:n :}
   idx 0 first TOKEN? 0= if false exit then
   idx first 1+ second first - 1- TARGET-OK? 0= if false exit then
   idx second 1+ end second - 1- VERSION-OK? 0= if false exit then
   0 first idx METHOD!
   idx first 1+ second first - 1- SPLIT-TARGET
   second 1+ end second - 1- idx VERSION!
   true ;


\ ---- the header block -------------------------------------------------------

\ name ":" OWS value OWS, with no space before the colon and no obsolete fold.
: PARSE-HEADER ( n n n -- bool ) {: idx:n at:n end:n :}
   idx at AT-BYTE SP = idx at AT-BYTE TAB = or if false exit then
   idx at end at - COLON BYTE-INDEX {: mark:n :}
   mark 1 < if false exit then
   idx at mark TOKEN? 0= if false exit then
   idx at mark + 1+ end at - mark - 1- TRIM-OWS {: va:n vl:n :}
   at mark va vl idx HEADER+
   true ;


: PARSE-HEADERS-FROM ( n n -- parse-result ) {: idx:n at:n :}
   idx at LINE-END {: end:n :}
   end 0 < if BAD exit then
   end at = if OK exit then
   idx HEADER-N@ MAX-HEADERS >= if TOO-MUCH-HEAD exit then
   idx at end PARSE-HEADER 0= if BAD exit then
   idx end 2 + RECURSE ;


: PARSE-HEAD ( n -- parse-result ) {: idx:n :}
   idx 0 LINE-END {: end:n :}
   end 0 < if BAD exit then
   idx end PARSE-REQUEST-LINE 0= if BAD exit then
   idx end 2 + PARSE-HEADERS-FROM ;


\ ---- named headers ----------------------------------------------------------

: HEADER-MATCH ( n n ptr u8 n -- n ) {: idx:n at:n name:ptr len:n :}
   at idx HEADER-N@ >= if -1 exit then
   idx at HEADER-NAME$ name len STR=CI if at exit then
   idx at 1+ name len RECURSE ;


: FIND-HEADER ( n ptr u8 n -- n ) {: idx:n name:ptr len:n :}
   idx 0 name len HEADER-MATCH ;


: COUNT-FROM ( n n ptr u8 n -- n ) {: idx:n at:n name:ptr len:n :}
   at idx HEADER-N@ >= if 0 exit then
   idx at 1+ name len RECURSE {: rest:n :}
   idx at HEADER-NAME$ name len STR=CI if rest 1+ exit then
   rest ;


: COUNT-HEADER ( n ptr u8 n -- n ) {: idx:n name:ptr len:n :}
   idx 0 name len COUNT-FROM ;


: HEADER$ ( n ptr u8 n -- ptr u8 n bool ) {: idx:n name:ptr len:n :}
   idx name len FIND-HEADER {: at:n :}
   at 0 < if idx 0 0 SPAN$ false exit then
   idx at HEADER-VALUE$ true ;


\ ---- the Host field ---------------------------------------------------------

$2E constant DOT
$5D constant CLOSE-BRACKET
8 constant H16-GROUPS              \ the 16-bit groups of an IPv6 address


\ How many bytes come before the first c: all of them when none is c.
: BEFORE ( ptr u8 n n -- n )
   {: a u:n c:n :}
   u 0 ?do
      a i + c@ c = if i unloop exit then
   loop
   u ;


\ True when every byte passes the test; an empty run passes.
: EVERY? ( ptr u8 n [ n -- bool ] -- bool )
   {: a u:n test :}
   u 0 ?do
      a i + c@ test execute 0= if false unloop exit then
   loop
   true ;


\ unreserved and sub-delims (RFC 3986 2.2, 2.3): what a host name holds as is.
: HOST-BYTE? ( n -- bool )
   {: c:n :}
   c $41 >= c $5A <= and if true exit then
   c $61 >= c $7A <= and if true exit then
   c STR-DIGIT? if true exit then
   s" -._~!$&'()*+,;=" c COUNT-CHAR 0 > ;


\ The "%" at that offset begins an escape: two hex digits follow it.
: ESCAPE? ( ptr u8 n n -- bool )
   {: a u:n at:n :}
   at 2 + u >= if false exit then
   a at + 1+ 2 [: HEX-BYTE? ;] EVERY? ;


\ reg-name (RFC 3986 3.2.2), which an IPv4address is too, never empty (RFC
\ 9110 4.2.1).
: REG-NAME? ( ptr u8 n -- bool )
   {: a u:n :}
   u 0= if false exit then
   u 0 ?do
      a i + c@ PERCENT = if
         a u i ESCAPE? 0= if false unloop exit then
      else
         a i + c@ HOST-BYTE? 0= if false unloop exit then
      then
   loop
   true ;


\ What follows the host: nothing, or ":" and a port, whose digits may be none
\ (RFC 3986 3.2.3).
: PORT-PART? ( ptr u8 n -- bool )
   {: a u:n :}
   u 0= if true exit then
   a c@ COLON <> if false exit then
   a 1+ u 1- [: STR-DIGIT? ;] EVERY? ;


\ h16: one to four hex digits.
: HEX16? ( ptr u8 n -- bool )
   {: a u:n :}
   u 1 < u 4 > or if false exit then
   a u [: HEX-BYTE? ;] EVERY? ;


\ dec-octet: 0 to 255, with no leading zero.
: DEC-OCTET? ( ptr u8 n -- bool )
   {: a u:n :}
   u 1 < u 3 > or if false exit then
   a u STR-DIGITS? 0= if false exit then
   u 1 > a c@ STR-ZERO = and if false exit then
   a u s" 255" STR-DIGITS<= ;


\ IPv4address: four dec-octets split by ".".
: IPV4? ( ptr u8 n -- bool )
   {: a u:n :}
   a u DOT COUNT-CHAR 3 <> if false exit then
   0
   4 0 ?do
      {: at:n :}
      a at + u at - DOT BEFORE {: k:n :}
      a at + k DEC-OCTET? 0= if false unloop exit then
      at k + 1+
   loop
   drop true ;


\ The total with a run's last group added: an h16 counts one and a dotted quad
\ two; -1 when it is neither.
: LAST-GROUP ( n ptr u8 n -- n )
   {: total:n a u:n :}
   a u HEX16? if total 1+ exit then
   a u IPV4? if total 2 + exit then
   -1 ;


\ The 16-bit groups a run split by single ":" stands for, or -1 when a group
\ is malformed; only the last may be a dotted quad, and an empty run has none.
: GROUPS ( ptr u8 n -- n )
   {: a u:n :}
   u 0= if 0 exit then
   0 0
   begin
      {: total:n at:n :}
      a at + u at - COLON BEFORE {: k:n :}
      at k + u = if total a at + k LAST-GROUP exit then
      a at + k HEX16? 0= if -1 exit then
      total 1+ at k + 1+
   again ;


\ The groups on each side of the "::" at that offset: fewer than eight in all,
\ since it stands for one at least, and no dotted quad before it.
: ELIDED? ( ptr u8 n n -- bool )
   {: a u:n at:n :}
   a at DOT COUNT-CHAR 0 <> if false exit then
   a at GROUPS {: left:n :}
   a at + 2 + u at - 2 - GROUPS {: right:n :}
   left 0 < right 0 < or if false exit then
   left right + H16-GROUPS < ;


\ IPv6address (RFC 3986 3.2.2): eight 16-bit groups, or fewer around the one
\ "::" that stands for the rest.
: IPV6? ( ptr u8 n -- bool )
   {: a u:n :}
   a u s" ::" FIND-SUB
   MATCH option
      none OF a u GROUPS H16-GROUPS = ENDOF
      some OF IDX>N {: at:n :} a u at ELIDED? ENDOF
   ;MATCH ;


: FUTURE-BYTE? ( n -- bool )
   {: c:n :}
   c COLON = if true exit then
   c HOST-BYTE? ;


\ IPvFuture past its "v": a hex version, ".", then at least one unreserved,
\ sub-delims or ":" byte.
: FUTURE? ( ptr u8 n -- bool )
   {: a u:n :}
   a u DOT BEFORE {: k:n :}
   k 0= k 1+ u >= or if false exit then
   a k [: HEX-BYTE? ;] EVERY? 0= if false exit then
   a k + 1+ u k - 1- [: FUTURE-BYTE? ;] EVERY? ;


\ What an IP-literal's brackets hold: IPvFuture after a "v", else IPv6.
: IP-LITERAL? ( ptr u8 n -- bool )
   {: a u:n :}
   u 0= if false exit then
   a 1 s" v" STR=CI if a 1+ u 1- FUTURE? exit then
   a u IPV6? ;


\ An IP-literal past its "[": the literal up to the "]", then the port part.
: LITERAL-HOST? ( ptr u8 n -- bool )
   {: a u:n :}
   a u CLOSE-BRACKET BEFORE {: k:n :}
   k u = if false exit then
   a k IP-LITERAL? 0= if false exit then
   a k + 1+ u k - 1- PORT-PART? ;


\ Host = uri-host [ ":" port ] (RFC 9110 7.2): an IP-literal in brackets, else
\ a reg-name up to the first ":".
: HOST-VALUE? ( ptr u8 n -- bool )
   {: a u:n :}
   a u s" [" STARTS-WITH? if a 1+ u 1- LITERAL-HOST? exit then
   a u COLON BEFORE {: k:n :}
   a k REG-NAME? 0= if false exit then
   a k + u k - PORT-PART? ;


\ RFC 9112 3.2: an HTTP/1.1 request carries one Host and an HTTP/1.0 request
\ one at most, and a Host that arrives is valid.
: HOST-OK? ( n -- bool )
   {: idx:n :}
   idx s" host" COUNT-HEADER {: count:n :}
   count 1 > if false exit then
   count 0= if idx SLOT-VERSION$ s" HTTP/1.0" STR= exit then
   idx s" host" HEADER$ drop HOST-VALUE? ;


\ ---- connection tokens ------------------------------------------------------

: LIST-HAS? ( ptr u8 n ptr u8 n n -- bool )
   {: list:ptr listu:n token:ptr tokenu:n start:n :}
   list listu CH-COMMA start SPLIT-NEXT {: fa:ptr fu:n next:n found:bool :}
   found 0= if false exit then
   fa fu TRIM token tokenu STR=CI if true exit then
   next start <= if false exit then
   list listu token tokenu next RECURSE ;


: KEEP-ALIVE-DEFAULT ( n -- bool ) {: idx:n :}
   idx SLOT-VERSION$ s" HTTP/1.1" STR= ;


: DECIDE-KEEP-ALIVE ( n -- ) {: idx:n :}
   idx KEEP-ALIVE-DEFAULT {: alive:bool :}
   idx s" connection" HEADER$ {: va:ptr vu:n found:bool :}
   found 0= if alive FLAG>N idx KEEP-ALIVE! exit then
   va vu s" close" 0 LIST-HAS? if 0 idx KEEP-ALIVE! exit then
   va vu s" keep-alive" 0 LIST-HAS? if 1 idx KEEP-ALIVE! exit then
   alive FLAG>N idx KEEP-ALIVE! ;


\ ---- bodies -----------------------------------------------------------------

: FILLED? ( fill-outcome -- bool )
   MATCH fill-outcome
      filled OF true ENDOF
      ended OF false ENDOF
      late OF false ENDOF
      broken OF false ENDOF
   ;MATCH ;


: ENDED? ( fill-outcome -- bool )
   MATCH fill-outcome
      filled OF false ENDOF
      ended OF true ENDOF
      late OF false ENDOF
      broken OF false ENDOF
   ;MATCH ;


\ What a transfer that did not deliver bytes means inside a request: a peer that
\ ends the stream mid-body has sent a request the grammar cannot complete.
: OUTCOME>RESULT ( fill-outcome -- parse-result )
   MATCH fill-outcome
      filled OF OK ENDOF
      ended OF BAD ENDOF
      late OF TOO-LATE ENDOF
      broken OF FAULTED ENDOF
   ;MATCH ;


: TAKE-WINDOW ( n n -- ) {: idx:n end:n :}
   end idx BODY-U@ - idx AVAIL min {: take:n :}
   take 0 <= if exit then
   idx IN-BUF idx IN-AT@ take SPAN:SUB SPAN:$ idx BODY-BUF idx BODY-U@ SPAN:SKIP SPAN:COPY
   idx IN-AT@ take + idx IN-AT!
   idx BODY-U@ take + idx BODY-U! ;


: READ-BODY-CHUNK ( TCP4:connection n n -- fill-outcome )
   {: conn:TCP4:connection idx:n end:n :}
   conn idx BODY-BUF idx BODY-U@ end idx BODY-U@ - SPAN:SUB SPAN:$ TCP4:TRANSFER-BYTES TCP4:READ
   MATCH TCP4:read-result
      data OF BLEN>N idx BODY-U@ + idx BODY-U! construct fill-outcome filled ENDOF
      closed OF drop construct fill-outcome ended ENDOF
      failed OF drop construct fill-outcome broken ENDOF
   ;MATCH ;


\ A Content-Length body: the window first, then straight into the body buffer,
\ never one byte past the length the head declared.
: PUMP-BODY ( TCP4:connection n n n -- parse-result )
   {: conn:TCP4:connection idx:n end:n deadline:n :}
   begin
      idx BODY-U@ end >= if OK exit then
      idx AVAIL 0 > if
         idx end TAKE-WINDOW
      else
         conn deadline WAIT-READABLE 0= if TOO-LATE exit then
         conn idx end READ-BODY-CHUNK {: outcome:fill-outcome :}
         outcome FILLED? 0= if outcome OUTCOME>RESULT exit then
      then
   again ;


\ ---- chunked bodies ---------------------------------------------------------

: WINDOW-LINE ( n -- n ) {: idx:n :}
   idx IN-U@ 1- {: limit:n :}
   limit idx IN-AT@ ?do
      idx i CRLF-AT? if i unloop exit then
   loop
   -1 ;


\ The offset of the CRLF that ends the next framing line, with the window
\ refilled until it holds one. A line that cannot fit the window is malformed.
: ENSURE-LINE ( TCP4:connection n n -- parse-result n )
   {: conn:TCP4:connection idx:n deadline:n :}
   begin
      idx WINDOW-LINE {: at:n :}
      at 0 >= if OK at exit then
      idx COMPACT
      idx IN-U@ IN-CAP >= if BAD -1 exit then
      conn idx deadline FILL {: outcome:fill-outcome :}
      outcome FILLED? 0= if outcome OUTCOME>RESULT -1 exit then
   again ;


: HEX-SCAN ( n n n n -- n ) {: idx:n at:n end:n value:n :}
   at end >= if value exit then
   idx at AT-BYTE {: c:n :}
   c SEMICOLON = if value exit then
   c HEX-BYTE? 0= if -1 exit then
   value HEX-BASE * c HEX-VALUE + {: next:n :}
   next MAX-CHUNK > if -1 exit then
   idx at 1+ end next RECURSE ;


\ The chunk size is the hex prefix of its framing line, up to any extension.
: CHUNK-SIZE ( n n n -- n ) {: idx:n at:n end:n :}
   end at <= if -1 exit then
   idx at end 0 HEX-SCAN ;


\ One chunk's data moved through the window into the body buffer.
: PUMP-CHUNK ( TCP4:connection n n n -- parse-result )
   {: conn:TCP4:connection idx:n end:n deadline:n :}
   begin
      idx BODY-U@ end >= if OK exit then
      idx AVAIL 0 > if
         idx end TAKE-WINDOW
      else
         idx COMPACT
         conn idx deadline FILL {: outcome:fill-outcome :}
         outcome FILLED? 0= if outcome OUTCOME>RESULT exit then
      then
   again ;


\ The CRLF that closes a chunk's data, and nothing else.
: EXPECT-CRLF ( TCP4:connection n n -- parse-result )
   {: conn:TCP4:connection idx:n deadline:n :}
   conn idx deadline ENSURE-LINE {: outcome:parse-result at:n :}
   outcome COMPLETE? 0= if outcome exit then
   at idx IN-AT@ <> if BAD exit then
   at 2 + idx IN-AT!
   OK ;


: READ-TRAILERS ( TCP4:connection n n -- parse-result )
   {: conn:TCP4:connection idx:n deadline:n :}
   MAX-HEADERS 0 ?do
      conn idx deadline ENSURE-LINE {: outcome:parse-result at:n :}
      outcome COMPLETE? 0= if outcome unloop exit then
      at idx IN-AT@ = if at 2 + idx IN-AT! OK unloop exit then
      at 2 + idx IN-AT!
   loop
   TOO-MUCH-HEAD ;


: READ-CHUNKED ( TCP4:connection n n -- parse-result )
   {: conn:TCP4:connection idx:n deadline:n :}
   begin
      conn idx deadline ENSURE-LINE {: outcome:parse-result at:n :}
      outcome COMPLETE? 0= if outcome exit then
      idx idx IN-AT@ at CHUNK-SIZE {: size:n :}
      size 0 < if BAD exit then
      at 2 + idx IN-AT!
      size 0= if conn idx deadline READ-TRAILERS exit then
      idx BODY-U@ size + BODY-CAP > if TOO-MUCH-BODY exit then
      conn idx idx BODY-U@ size + deadline PUMP-CHUNK {: moved:parse-result :}
      moved COMPLETE? 0= if moved exit then
      conn idx deadline EXPECT-CRLF {: closed:parse-result :}
      closed COMPLETE? 0= if closed exit then
   again ;


\ ---- choosing the body ------------------------------------------------------

: CONTENT-LENGTH ( n -- n ) {: idx:n :}
   idx s" content-length" COUNT-HEADER {: count:n :}
   count 0= if -1 exit then
   count 1 > if -2 exit then
   idx s" content-length" HEADER$ drop {: va:ptr vu:n :}
   vu 0= if -2 exit then
   va vu STR-DIGITS? 0= if -2 exit then
   va vu STR>NUMBER?
   MATCH option
      some OF ENDOF
      none OF -2 ENDOF
   ;MATCH ;


: CHUNKED? ( n -- bool ) {: idx:n :}
   idx s" transfer-encoding" HEADER$ {: va:ptr vu:n found:bool :}
   found 0= if false exit then
   va vu TRIM s" chunked" STR=CI ;


: READ-BODY ( TCP4:connection n n -- parse-result )
   {: conn:TCP4:connection idx:n deadline:n :}
   idx s" transfer-encoding" HEADER$ {: tea:ptr teu:n coded:bool :}
   idx CONTENT-LENGTH {: declared:n :}
   coded declared -1 <> and if BAD exit then
   coded if
      idx CHUNKED? 0= if NOT-SUPPORTED exit then
      conn idx deadline READ-CHUNKED exit
   then
   declared -2 = if BAD exit then
   declared -1 = if OK exit then
   declared BODY-CAP > if TOO-MUCH-BODY exit then
   conn idx declared deadline PUMP-BODY ;


\ ---- one request ------------------------------------------------------------

: READ-HEAD ( TCP4:connection n n -- parse-result )
   {: conn:TCP4:connection idx:n deadline:n :}
   begin
      idx HEAD-END {: end:n :}
      end 0 >= if
         end HEAD-CAP > if TOO-MUCH-HEAD exit then
         end idx HEAD-U!
         end idx IN-AT!
         OK exit
      then
      idx IN-U@ HEAD-CAP >= if TOO-MUCH-HEAD exit then
      conn idx deadline FILL {: outcome:fill-outcome :}
      outcome FILLED? 0= if
         outcome ENDED? if
            idx IN-U@ 0= if CLOSED else BAD then exit
         then
         outcome OUTCOME>RESULT exit
      then
   again ;


\ One whole request into the slot: its head, its limits and its body, all
\ within one idle deadline. The head spans stay valid until the next call on
\ the same slot.
: READ-REQUEST ( TCP4:connection n n -- parse-result )
   {: conn:TCP4:connection idx:n ms:n :}
   ms DEADLINE-AT {: deadline:n :}
   idx SLOT-RESET
   idx COMPACT
   conn idx deadline READ-HEAD {: head:parse-result :}
   head COMPLETE? 0= if head exit then
   idx PARSE-HEAD {: parsed:parse-result :}
   parsed COMPLETE? 0= if parsed exit then
   idx HOST-OK? 0= if BAD exit then
   idx DECIDE-KEEP-ALIVE
   conn idx deadline READ-BODY ;


public

: METHOD$ ( request -- ptr u8 n )
   SLOT-OF-REQUEST SLOT-METHOD$ ;


: PATH$ ( request -- ptr u8 n )
   SLOT-OF-REQUEST SLOT-PATH$ ;


: QUERY$ ( request -- ptr u8 n )
   SLOT-OF-REQUEST SLOT-QUERY$ ;


\ The version the request line named, as sent: `HTTP/1.1`, `HTTP/1.0`.
: VERSION$ ( request -- ptr u8 n )
   SLOT-OF-REQUEST SLOT-VERSION$ ;


: BODY$ ( request -- ptr u8 n )
   SLOT-OF-REQUEST SLOT-BODY$ ;


: HEADER-OF$ ( request ptr u8 n -- ptr u8 n bool ) {: subject:request name:ptr len:n :}
   subject SLOT-OF-REQUEST name len HEADER$ ;


\ How many of a named header the request carries. A header whose value is
\ trusted is read only when exactly one arrived.
: HEADER-COUNT-OF ( request ptr u8 n -- n ) {: subject:request name:ptr len:n :}
   subject SLOT-OF-REQUEST name len COUNT-HEADER ;


\ A reader over the request body, on the slot's own reader storage, so two
\ workers reading two bodies share nothing. The caller closes it.
: BODY-READER ( request -- JR:reader ) {: subject:request :}
   subject SLOT-OF-REQUEST {: idx:n :}
   idx READER-CELLS idx SLOT-BODY$ JR:INIT ;


;package
