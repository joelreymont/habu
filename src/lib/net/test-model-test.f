\ test-model-test.f - MODEL:CHAT against a loopback HTTP server whose
\ /responses routes answer through TEST-MODEL: a completed reply gives its text,
\ JSON escapes included; an incomplete one MODEL:E-INCOMPLETE with its reason;
\ a refusal MODEL:E-REFUSED. Every key item reads as one key from a store the
\ test binds with SECRET:STORE!, so none reaches the desktop store.
\ Run: bin/hb --load src/lib/net/test-model-test.f

require src/lib/test.f
require src/lib/string.f
require src/lib/span.f
require src/lib/fmt.f
require src/lib/aio.f
require src/lib/secret.f
require src/lib/net/curl.f
require src/lib/net/http.f
require src/lib/net/model.f
require src/lib/net/test-model.f

package TEST-MODEL-TEST
private

$7F000001 constant LOOPBACK
2 constant TWO-WORKERS
500 constant IDLE-MS

1 TYPED-BUFFER CALL MODEL:call
: M ( -- ptr MODEL:call ) 0 CALL ;

: KEY$ ( -- ptr u8 n ) s" test-model-key" ;
\ The completed reply's text, a JSON object whose string holds escaped quotes.
: TEXT$ ( -- ptr u8 n ) S\" {\qanswer\q:\qsay \\\qhi\\\q\q}" ;
: REASON$ ( -- ptr u8 n ) s" max_output_tokens" ;

: SCHEMA$ ( -- ptr u8 n )
   S\" {\qtype\q:\qobject\q,\qproperties\q:{\qanswer\q:{\qtype\q:\qstring\q}},\qrequired\q:[\qanswer\q]}" ;


\ Every item holds KEY$; a span too short for it is SECRET:E-CAPACITY, which
\ MODEL answers by reading again into more room.
: STORE-READ ( ptr u8 n ptr u8 n SPAN:span<u8> -- n bool )
   {: dest:SPAN:span<u8> :}
   2drop 2drop
   KEY$ nip dest SPAN:LEN > if SECRET:E-CAPACITY throw then
   KEY$ dest SPAN:COPY
   KEY$ nip true ;


: STORE-WRITE ( ptr u8 n ptr u8 n ptr u8 n -- )
   2drop 2drop 2drop SECRET:E-UNAVAILABLE throw ;


: STORE-ERASE ( ptr u8 n ptr u8 n -- bool )
   2drop 2drop SECRET:E-UNAVAILABLE throw ;


: ROUTES ( -- )
   HTTP:ROUTES-RESET
   s" POST" s" /completed/responses" [: nip TEXT$ TEST-MODEL:COMPLETED! ;] HTTP:ROUTE
   s" POST" s" /incomplete/responses" [: nip REASON$ TEST-MODEL:INCOMPLETE! ;] HTTP:ROUTE
   s" POST" s" /refusal/responses" [: nip s" cannot comply" TEST-MODEL:REFUSAL! ;] HTTP:ROUTE ;


\ Points the call at this server's routes under the path and asks one text.
: AT ( ptr u8 n -- )
   {: path:ptr pu:n :}
   SB-RESET s" http://127.0.0.1:" SB-APPEND HTTP:PORT FMT:SB-U path pu SB-APPEND
   SB$ M MODEL:BASE-URL!
   M MODEL:PARTS-RESET
   s" Reply." M MODEL:TEXT+ ;


: SETUP ( -- )
   M MODEL:OPEN
   s" test-generation" M MODEL:GENERATION-MODEL!
   s" test-model" s" key" M MODEL:API-KEY!
   s" Reply in JSON." M MODEL:SYSTEM!
   SCHEMA$ M MODEL:SCHEMA! ;


: COMPLETED ( -- )
   s" a completed reply gives its text" T-LABEL
   s" /completed" AT
   M MODEL:CHAT TEXT$ T$= ;


: INCOMPLETE ( -- )
   s" an incomplete reply is E-INCOMPLETE with its reason" T-LABEL
   s" /incomplete" AT
   [: M MODEL:CHAT 2drop ;] MODEL:E-INCOMPLETE TTHROWSQ
   M MODEL:INCOMPLETE-REASON$ REASON$ T$= ;


: REFUSAL ( -- )
   s" a refusal is E-REFUSED" T-LABEL
   s" /refusal" AT
   [: M MODEL:CHAT 2drop ;] MODEL:E-REFUSED TTHROWSQ ;


: CURL-OK ( CURL:status -- )
   MATCH CURL:status
      ok OF true ENDOF
      failed OF drop false ENDOF
   ;MATCH TTRUE ;


: CASES ( -- )
   [: STORE-READ ;] [: STORE-WRITE ;] [: STORE-ERASE ;] SECRET:STORE!
   AIO:START
   CURL:LOOP-START CURL-OK
   ROUTES
   LOOPBACK 0 TWO-WORKERS IDLE-MS HTTP:START
   SETUP
   COMPLETED INCOMPLETE REFUSAL
   M MODEL:CLOSE
   HTTP:STOP
   CURL:LOOP-STOP
   AIO:STOP ;

T-RESET
CASES
T-REPORT

;package
