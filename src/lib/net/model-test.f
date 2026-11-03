\ model-test.f - MODEL's HTTP transport, input bounds and journal against one
\ in-process OpenAI- and Ollama-shaped peer on a loopback port. Its key and
\ settings items live in a store the test binds with SECRET:STORE!, so no item
\ reaches the desktop store; secret-test.f covers the native stores. The run's
\ stderr goes to a file in a private temp directory, where cases read MODEL's
\ log lines. The first failed assertion ends the run.
\ Run: bin/hb --load src/lib/net/model-test.f
require src/lib/aio.f
require src/lib/byte-buffer.f
require src/lib/date.f
require src/lib/fmt.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/json.f
require src/lib/memory.f
require src/lib/property.f
require src/lib/secret.f
require src/lib/span.f
require src/lib/string.f
require src/lib/task.f
require src/lib/test.f
require src/lib/test/stderr.f
require src/lib/test/wait.f
require src/lib/net/curl.f
require src/lib/net/http.f
require src/lib/net/model.f
require src/lib/net/tcp4.f

package MODEL-FIXTURE
private
\ The store holds ITEMS items; an empty service marks a free one, since SECRET
\ refuses an empty identifier before calling the store.
8 constant ITEMS
64 constant ID-CAP
4096 constant VALUE-CAP
ITEMS ID-CAP * BUFFER: SERVICES
ITEMS ID-CAP * BUFFER: ACCOUNTS
ITEMS VALUE-CAP * BUFFER: VALUES
ITEMS TYPED-BUFFER SERVICE-US n
ITEMS TYPED-BUFFER ACCOUNT-US n
ITEMS TYPED-BUFFER VALUE-US n

: SERVICE-AT ( n -- ptr u8 n ) {: i:n :} SERVICES i ID-CAP * + i SERVICE-US @ ;
: ACCOUNT-AT ( n -- ptr u8 n ) {: i:n :} ACCOUNTS i ID-CAP * + i ACCOUNT-US @ ;
: VALUE-AT ( n -- ptr u8 n ) {: i:n :} VALUES i VALUE-CAP * + i VALUE-US @ ;


\ The index of the item filed under service and account, or -1.
: FIND ( ptr u8 n ptr u8 n -- n )
   {: s:ptr su:n a:ptr au:n :}
   ITEMS 0 ?do
      i SERVICE-AT s su STR= i ACCOUNT-AT a au STR= and if i unloop exit then
   loop
   -1 ;


: FREE ( -- n )
   ITEMS 0 ?do i SERVICE-US @ 0= if i unloop exit then loop
   -1 ;


: STORE-READ ( ptr u8 n ptr u8 n SPAN:span<u8> -- n bool )
   {: s:ptr su:n a:ptr au:n dest:SPAN:span<u8> :}
   s su a au FIND {: i:n :}
   i 0< if 0 false exit then
   i VALUE-AT {: v:ptr vu:n :}
   vu dest SPAN:LEN > if SECRET:E-CAPACITY throw then
   v vu dest SPAN:COPY
   vu true ;


: STORE-WRITE ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: s:ptr su:n a:ptr au:n v:ptr vu:n :}
   s su a au FIND dup 0< if drop FREE then {: i:n :}
   i 0< su ID-CAP > or au ID-CAP > or vu VALUE-CAP > or
   if SECRET:E-CAPACITY throw then
   s SERVICES i ID-CAP * + su BYTE-COPY su i SERVICE-US !
   a ACCOUNTS i ID-CAP * + au BYTE-COPY au i ACCOUNT-US !
   v VALUES i VALUE-CAP * + vu BYTE-COPY vu i VALUE-US ! ;


: STORE-ERASE ( ptr u8 n ptr u8 n -- bool )
   FIND {: i:n :}
   i 0< if false exit then
   0 i SERVICE-US ! true ;

public

: ASSERT ( bool -- ) dup T-ASSERT 0= if T-REPORT then ;
: FAIL ( -- ) false ASSERT ;

private

: CURL-READY ( CURL:status -- )
   MATCH CURL:status
      ok OF ENDOF
      failed OF drop FAIL ENDOF
   ;MATCH ;

public

: KEY-SERVICE$ ( -- ptr u8 n ) s" habu-test" ;
: KEY-ACCOUNT$ ( -- ptr u8 n ) s" model" ;
: KEY$ ( -- ptr u8 n ) s" local-test-key" ;
: SETTINGS-SERVICE$ ( -- ptr u8 n ) s" habu-test-settings" ;


\ Binds the store for the process and files KEY$ under the key's service and
\ account.
: STORE-INSTALL ( -- )
   [: STORE-READ ;] [: STORE-WRITE ;] [: STORE-ERASE ;] SECRET:STORE!
   KEY-SERVICE$ KEY-ACCOUNT$ KEY$ SECRET:WRITE ;


\ Files model, base-url base and api-key KEY$ under SETTINGS-SERVICE$.
: SETTINGS! ( ptr u8 n ptr u8 n -- )
   {: m:ptr mu:n b:ptr bu:n :}
   SETTINGS-SERVICE$ s" model" m mu SECRET:WRITE
   SETTINGS-SERVICE$ s" base-url" b bu SECRET:WRITE
   SETTINGS-SERVICE$ s" api-key" KEY$ SECRET:WRITE ;


: WITH-CURL ( [ -- ] -- )
   {: body :}
   AIO:START CURL:LOOP-START CURL-READY
   body [: CURL:LOOP-STOP AIO:STOP ;] finally ;
;package


package MODEL-PEER
using MODEL-FIXTURE
private
create BASE-URL 64 allot
create OLLAMA-URL 64 allot
variable BASE-URL-U
variable OLLAMA-URL-U
variable CALLS
PTR-VARIABLE BIG
\ The last chat or embedding request body, or -1 after one past SENT-CAP.
65536 constant SENT-CAP
create SENT-BODY SENT-CAP allot
variable SENT-U
\ Stream resumes, provider cancels, resumes of resp_alive, and resumes of the
\ last resp_dead or resp_mute stream.
variable RESUMES
variable CANCELS
variable ALIVE-RESUMES
variable DEAD-RESUMES
TASK:SEMAPHORE HELD
\ An event stream being written. Requests reach the peer one at a time apart
\ from the held stream's and its cancel's, and a cancel writes no stream.
create STREAM-B BUF:HDR-BYTES allot
TYPED-VARIABLE STREAM-W JSON:writer
\ Requests reach the peer one at a time, so one parser and one reply buffer
\ serve them all.
TYPED-VARIABLE PARSER JSON:cursor
\ A second parser reads one content part apart from the request.
TYPED-VARIABLE PART JSON:cursor
create ECHO 256 allot
variable ECHO-U
variable IMAGES
\ The decoded inputs of one embedding request; the largest is 16384 bytes.
16384 constant RECEIVED-CAP
create RECEIVED RECEIVED-CAP allot
variable RECEIVED-U
\ Three mebibytes of content: a reply grows with its data, whatever its size.
$300000 constant BIG-U
: BIG$ ( -- ptr u8 n ) BIG @ BIG-U ;
: ECHO$ ( -- ptr u8 n ) ECHO ECHO-U @ ;
: RECEIVED$ ( -- ptr u8 n ) RECEIVED RECEIVED-U @ ;


: ECHO+ ( ptr u8 n -- )
   {: a:ptr u:n :}
   ECHO-U @ u + 256 <= ASSERT
   a u ECHO ECHO-U @ + 256 ECHO-U @ - SPAN:MAKE SPAN:COPY
   u ECHO-U +! ;


\ Takes a count of the semaphore within ten seconds, else throws E-PROC-TIMEOUT
\ (TEST-WAIT:POLL): a timeout, no assertion result.
10000000000 constant TAKE-NS
: TAKE ( TASK:sem -- )
   [: dup TASK:TRY-WAIT-SEM ;] mono-ns TAKE-NS + s" model-test TAKE" TEST-WAIT:POLL
   drop ;


: AUTHORIZED? ( HTTP:request -- bool )
   s" Authorization" HTTP:HEADER-OF$ if
      s" Bearer local-test-key" STR=
   else 2drop false then ;


\ A reader, on the parser record, at the value of member `key` of the JSON
\ object a u.
: MEMBER ( ptr JSON:cursor ptr u8 n ptr u8 n -- JSON:reader )
   {: storage:ptr a:ptr u:n key:ptr key-u:n :}
   storage a u JSON:READ-INIT
   JSON:READ-NEXT JSON:T-OBJ = ASSERT
   key key-u JSON:READ-FIND-KEY ASSERT ;


\ A reader at the value of the request body's member `key`.
: BODY-MEMBER ( HTTP:request ptr u8 n -- JSON:reader )
   {: r:HTTP:request key:ptr key-u:n :}
   PARSER r HTTP:BODY$ key key-u MEMBER ;


\ A reader at the content array of a chat request's user input.
: CONTENT ( HTTP:request -- JSON:reader )
   s" input" BODY-MEMBER
   JSON:READ-NEXT JSON:T-OBJ = ASSERT
   s" content" JSON:READ-FIND-KEY ASSERT ;


\ The text of a chat request's first part.
: FIRST-TEXT ( HTTP:request -- ptr u8 n )
   CONTENT
   JSON:READ-NEXT JSON:T-OBJ = ASSERT
   s" text" JSON:READ-FIND-KEY ASSERT
   JSON:READ-SPAN$ {: a:ptr u:n :}
   JSON:READ-CLOSE a u ;


\ Decodes an embedding request's input array into RECEIVED, one input per line
\ in request order.
: INPUT-LINES ( HTTP:request -- )
   0 RECEIVED-U !
   s" input" BODY-MEMBER
   JSON:READ-TOKEN JSON:T-ARR = ASSERT
   begin JSON:READ-NEXT dup JSON:T-ARR-END <> while
      JSON:T-STR = ASSERT
      RECEIVED-U @ 0<> if
         RECEIVED-U @ RECEIVED-CAP < ASSERT
         10 RECEIVED RECEIVED-U @ + c! 1 RECEIVED-U +!
      then
      RECEIVED RECEIVED-U @ + RECEIVED-CAP RECEIVED-U @ - JSON:READ-STR RECEIVED-U +!
   repeat drop JSON:READ-CLOSE ;


\ Echoes the decoded url of the content part a u when its type is input_image.
: PART+ ( ptr u8 n -- )
   {: a:ptr u:n :}
   PART a u s" type" MEMBER
   s" input_image" JSON:READ-STR-EQ? {: image:bool :}
   JSON:READ-CLOSE
   image 0= if exit then
   PART a u s" image_url" MEMBER
   IMAGES @ 0<> if s" ," ECHO+ then
   S\" \q" ECHO+
   ECHO ECHO-U @ + 256 ECHO-U @ - JSON:READ-STR ECHO-U +!
   JSON:READ-CLOSE
   S\" \q" ECHO+
   1 IMAGES +! ;


\ The content {"images":[...]} echoing the request's image URLs in order.
: IMAGE-ECHO ( HTTP:request -- )
   0 ECHO-U ! 0 IMAGES !
   S\" {\qimages\q:[" ECHO+
   CONTENT
   begin JSON:READ-NEXT dup JSON:T-ARR-END <> while
      JSON:T-OBJ = ASSERT
      JSON:READ-VALUE-SPAN$ PART+
   repeat drop JSON:READ-CLOSE
   s" ]}" ECHO+ ;


\ The reply of a background response still queued or running, or ended with
\ no output.
: PENDING ( ptr JSON:writer ptr u8 n ptr u8 n -- ptr JSON:writer )
   {: id:ptr id-u:n status:ptr status-u:n :}
   s" {" JSON:WRITE-RAW
   s" id" id id-u JSON:WRITE-FIELD-S JSON:WRITE-COMMA
   s" status" status status-u JSON:WRITE-FIELD-S
   S\" ,\qoutput\q:[]}" JSON:WRITE-RAW ;


\ Opens the data of an event of the type numbered n, after its type and number.
: EVENT-OPEN ( ptr JSON:writer ptr u8 n n -- ptr JSON:writer )
   {: type:ptr type-u:n seq:n :}
   s" event: " JSON:WRITE-RAW type type-u JSON:WRITE-RAW
   S\" \ndata: {" JSON:WRITE-RAW
   s" type" type type-u JSON:WRITE-FIELD-S
   S\" ,\qsequence_number\q:" JSON:WRITE-RAW seq JSON:WRITE-U ;


: EVENT-CLOSE ( ptr JSON:writer -- ptr JSON:writer )
   S\" }\n\n" JSON:WRITE-RAW ;


\ EVENT-OPEN for an event that carries a response, which comes next.
: RESPONSE-OPEN ( ptr JSON:writer ptr u8 n n -- ptr JSON:writer )
   EVENT-OPEN S\" ,\qresponse\q:" JSON:WRITE-RAW ;


\ The created event of the response with the id.
: CREATED ( ptr JSON:writer ptr u8 n -- ptr JSON:writer )
   {: id:ptr id-u:n :}
   s" response.created" 0 RESPONSE-OPEN id id-u s" queued" PENDING EVENT-CLOSE ;


: GOING ( ptr JSON:writer ptr u8 n -- ptr JSON:writer )
   {: id:ptr id-u:n :}
   s" response.in_progress" 1 RESPONSE-OPEN id id-u s" in_progress" PENDING EVENT-CLOSE ;


\ The created and in_progress events of the response with the id.
: OPENING ( ptr JSON:writer ptr u8 n -- ptr JSON:writer )
   {: id:ptr id-u:n :}
   id id-u CREATED id id-u GOING ;


\ OPENING, a delta, then a keepalive numbered past them: a resume starts after
\ the delta, LAST-SEQ.
2 constant LAST-SEQ
: PRELUDE ( ptr JSON:writer ptr u8 n -- ptr JSON:writer )
   OPENING
   s" response.output_text.delta" LAST-SEQ EVENT-OPEN
   S\" ,\qdelta\q:\q{\\\qans\q" JSON:WRITE-RAW EVENT-CLOSE
   s" keepalive" 9 EVENT-OPEN EVENT-CLOSE ;


: STREAM-OPEN ( -- ptr JSON:writer ) STREAM-W STREAM-B JSON:WRITE-OPEN-BUF ;
: STREAM$ ( -- ptr u8 n ) STREAM-B BUF:SPAN$ BUF:BLEN>N ;


\ Answers with the written events as the whole body.
: STREAM! ( HTTP:response ptr JSON:writer -- )
   JSON:WRITE-CLOSE {: response:HTTP:response :}
   response s" Content-Type" s" text/event-stream" HTTP:HEADER!
   response STREAM$ HTTP:BODY! ;


\ A stream of resp_1 that ends with the terminal event of the type carrying
\ the response the body writes.
: STREAMED ( HTTP:response [ ptr JSON:writer -- ptr JSON:writer ] ptr u8 n -- )
   {: response:HTTP:response body type:ptr type-u:n :}
   STREAM-OPEN s" resp_1" PRELUDE
   type type-u LAST-SEQ 1+ RESPONSE-OPEN body execute EVENT-CLOSE
   response swap STREAM! ;


: RAW-WRITE ( TCP4:connection ptr u8 n -- )
   TCP4:TRANSFER-BYTES TCP4:WRITE
   MATCH TCP4:status
      ok OF ENDOF
      failed OF TCP4:ERRNO>N drop FAIL ENDOF
   ;MATCH ;


\ Takes the request's connection over and sends a chunked event-stream head
\ and the written events as one chunk, leaving the stream open.
: RAW-STREAM ( HTTP:request ptr JSON:writer -- TCP4:connection )
   JSON:WRITE-CLOSE
   HTTP:TAKE-OVER 2drop {: conn:TCP4:connection :}
   STREAM$ {: a:ptr u:n :}
   SB-RESET
   S\" HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\nTransfer-Encoding: chunked\r\n\r\n" SB-APPEND
   u FMT:SB-HEX S\" \r\n" SB-APPEND
   conn SB$ RAW-WRITE
   conn a u RAW-WRITE
   conn S\" \r\n" RAW-WRITE
   conn ;


: RAW-CLOSE ( TCP4:connection -- )
   TCP4:CLOSE
   MATCH TCP4:status
      ok OF ENDOF
      failed OF TCP4:ERRNO>N drop FAIL ENDOF
   ;MATCH ;


\ The longest a held stream waits for its client to close it.
10000 constant HOLD-MS
\ Signals HELD and keeps the connection open until the client closes it.
: HOLD-OPEN ( TCP4:connection -- )
   {: conn:TCP4:connection :}
   HELD TASK:SIGNAL-SEM
   conn HOLD-MS >MS TCP4:READABLE-WITHIN?
   MATCH TCP4:ready-result
      ready OF ENDOF
      idle OF FAIL ENDOF
      failed OF TCP4:ERRNO>N drop FAIL ENDOF
   ;MATCH
   conn RAW-CLOSE ;


\ resp_hold's resumed stream sends in_progress and stays open until the client
\ closes it.
: HOLD-STREAM ( HTTP:request -- )
   STREAM-OPEN s" resp_hold" GOING RAW-STREAM HOLD-OPEN ;


\ A reply object's members through its first output item, a reasoning item.
: REPLY-HEAD ( ptr JSON:writer ptr u8 n -- ptr JSON:writer )
   {: status:ptr status-u:n :}
   S\" {\qid\q:\qresp_1\q,\qobject\q:\qresponse\q," JSON:WRITE-RAW
   s" status" status status-u JSON:WRITE-FIELD-S
   S\" ,\qoutput\q:[{\qtype\q:\qreasoning\q,\qid\q:\qrs_1\q,\qsummary\q:[]}" JSON:WRITE-RAW ;


\ A completed reply whose message holds the text, the type of the message and
\ of its part after their other members.
: COMPLETED ( ptr JSON:writer ptr u8 n -- ptr JSON:writer )
   {: content:ptr content-u:n :}
   s" completed" REPLY-HEAD
   S\" ,{\qid\q:\qmsg_1\q,\qrole\q:\qassistant\q,\qcontent\q:[{" JSON:WRITE-RAW
   s" text" content content-u JSON:WRITE-FIELD-S
   S\" ,\qtype\q:\qoutput_text\q,\qannotations\q:[]}],\qtype\q:\qmessage\q}],\qincomplete_details\q:null,\qerror\q:null}" JSON:WRITE-RAW ;


: INCOMPLETE ( ptr JSON:writer ptr u8 n -- ptr JSON:writer )
   {: reason:ptr reason-u:n :}
   s" incomplete" REPLY-HEAD
   S\" ],\qincomplete_details\q:{" JSON:WRITE-RAW
   s" reason" reason reason-u JSON:WRITE-FIELD-S
   S\" },\qerror\q:null}" JSON:WRITE-RAW ;


: FAILED ( ptr JSON:writer ptr u8 n ptr u8 n -- ptr JSON:writer )
   {: code:ptr code-u:n message:ptr message-u:n :}
   s" failed" REPLY-HEAD
   S\" ],\qincomplete_details\q:null,\qerror\q:{" JSON:WRITE-RAW
   s" code" code code-u JSON:WRITE-FIELD-S JSON:WRITE-COMMA
   s" message" message message-u JSON:WRITE-FIELD-S
   S\" }}" JSON:WRITE-RAW ;


\ The big content, a JSON object whose one string fills it.
: BIG-FILL ( -- )
   BIG @ {: a:ptr :}
   BIG-U 0 ?do [char] x a i + c! loop
   S\" {\qpad\q:\q" a BIG-U SPAN:MAKE SPAN:COPY
   S\" \q}" a BIG-U + 2 - 2 SPAN:MAKE SPAN:COPY ;


\ A 40-byte reason; the call names a reason of any length whole.
: LONG-FINISH$ ( -- ptr u8 n ) s" finish-reason-forty-bytes-long-for-tests" ;


: CHAT-RESPONSE ( ptr JSON:writer -- ptr JSON:writer )
   S\" {\qanswer\q:true}" COMPLETED ;


\ A completed reply's members through its output, whose message, type first,
\ holds {"answer":true}.
: TRUE-HEAD ( ptr JSON:writer -- ptr JSON:writer )
   S\" {\qid\q:\qresp_1\q,\qobject\q:\qresponse\q,\qstatus\q:\qcompleted\q,\qoutput\q:[{\qtype\q:\qreasoning\q,\qid\q:\qrs_1\q,\qsummary\q:[]},{\qtype\q:\qmessage\q,\qid\q:\qmsg_1\q,\qrole\q:\qassistant\q,\qcontent\q:[{\qtype\q:\qoutput_text\q,\qtext\q:\q{\\\qanswer\\\q:true}\q,\qannotations\q:[]}]}]" JSON:WRITE-RAW ;


: USAGE-RESPONSE ( ptr JSON:writer -- ptr JSON:writer )
   TRUE-HEAD
   S\" ,\qusage\q:{\qinput_tokens\q:11,\qinput_tokens_details\q:{\qcached_tokens\q:4},\qoutput_tokens\q:6,\qoutput_tokens_details\q:{\qreasoning_tokens\q:2},\qtotal_tokens\q:17}}" JSON:WRITE-RAW ;


\ A response whose service_tier member is the JSON value text.
: TIERED-RESPONSE ( ptr JSON:writer ptr u8 n -- ptr JSON:writer )
   {: tier:ptr tier-u:n :}
   TRUE-HEAD
   S\" ,\qservice_tier\q:" JSON:WRITE-RAW
   tier tier-u JSON:WRITE-RAW
   s" }" JSON:WRITE-RAW ;


\ A completed reply whose message part is a refusal.
: REFUSAL$ ( -- ptr u8 n )
   S\" {\qid\q:\qresp_1\q,\qobject\q:\qresponse\q,\qstatus\q:\qcompleted\q,\qoutput\q:[{\qtype\q:\qreasoning\q,\qid\q:\qrs_1\q,\qsummary\q:[]},{\qtype\q:\qmessage\q,\qid\q:\qmsg_1\q,\qrole\q:\qassistant\q,\qcontent\q:[{\qtype\q:\qrefusal\q,\qrefusal\q:\qcannot comply\q}]}],\qincomplete_details\q:null,\qerror\q:null}" ;


\ A completed reply whose content is not JSON.
: BAD-CONTENT$ ( -- ptr u8 n )
   S\" {\qid\q:\qresp_1\q,\qobject\q:\qresponse\q,\qstatus\q:\qcompleted\q,\qoutput\q:[{\qtype\q:\qreasoning\q,\qid\q:\qrs_1\q,\qsummary\q:[]},{\qtype\q:\qmessage\q,\qid\q:\qmsg_1\q,\qrole\q:\qassistant\q,\qcontent\q:[{\qtype\q:\qoutput_text\q,\qtext\q:\qnot JSON\q,\qannotations\q:[]}]}],\qincomplete_details\q:null,\qerror\q:null}" ;


: COMPLETED$ ( -- ptr u8 n ) s" response.completed" ;
: INCOMPLETE$ ( -- ptr u8 n ) s" response.incomplete" ;
: FAILED$ ( -- ptr u8 n ) s" response.failed" ;


\ A 200 reply that is JSON, not an event stream.
: PLAIN$ ( -- ptr u8 n )
   S\" {\qid\q:\qresp_1\q,\qstatus\q:\qcompleted\q,\qoutput\q:[]}" ;
\ Event data the call cannot read: a type that is not a string, no sequence
\ number, and a response with no id.
: BAD-TYPE$ ( -- ptr u8 n ) S\" {\qtype\q:5,\qsequence_number\q:2}" ;
: NO-SEQUENCE$ ( -- ptr u8 n ) S\" {\qtype\q:\qresponse.output_text.delta\q,\qdelta\q:\qx\q}" ;
: NO-ID$ ( -- ptr u8 n )
   S\" {\qtype\q:\qresponse.created\q,\qsequence_number\q:0,\qresponse\q:{\qstatus\q:\qqueued\q,\qoutput\q:[]}}" ;
\ resp_1's created event, then an event whose data is not UTF-8. MODEL-TEST
\ writes the line logging it.
: BAD-UTF8$ ( -- ptr u8 n )
   S\" event: response.created\ndata: {\qtype\q:\qresponse.created\q,\qsequence_number\q:0,\qresponse\q:{\qid\q:\qresp_1\q,\qstatus\q:\qqueued\q,\qoutput\q:[]}}\n\ndata: \xff\n\n" ;


\ A stream whose last event's data is the text, after resp_1's opening events
\ when asked.
: BAD-STREAM ( HTTP:response ptr u8 n bool -- )
   {: response:HTTP:response a:ptr u:n opened:bool :}
   STREAM-OPEN
   opened if s" resp_1" OPENING then
   s" data: " JSON:WRITE-RAW a u JSON:WRITE-RAW S\" \n\n" JSON:WRITE-RAW
   response swap STREAM! ;


\ Responds by the request's first text, which names its scenario.
: REPLY ( HTTP:request HTTP:response -- )
   {: r:HTTP:request response:HTTP:response :}
   r FIRST-TEXT {: t:ptr t-u:n :}
   t t-u s" image" STR= if
      r IMAGE-ECHO
      response [: ECHO$ COMPLETED ;] COMPLETED$ STREAMED exit
   then
   t t-u s" refusal" STR= if response [: REFUSAL$ JSON:WRITE-RAW ;] COMPLETED$ STREAMED exit then
   t t-u s" usage" STR= if response [: USAGE-RESPONSE ;] COMPLETED$ STREAMED exit then
   t t-u s" tiered" STR= if response [: S\" \qflex\q" TIERED-RESPONSE ;] COMPLETED$ STREAMED exit then
   t t-u s" tier-null" STR= if response [: s" null" TIERED-RESPONSE ;] COMPLETED$ STREAMED exit then
   t t-u s" tier-number" STR= if response [: s" 7" TIERED-RESPONSE ;] COMPLETED$ STREAMED exit then
   t t-u s" tier-empty" STR= if response [: S\" \q\q" TIERED-RESPONSE ;] COMPLETED$ STREAMED exit then
   t t-u s" length" STR= if
      response [: s" max_output_tokens" INCOMPLETE ;] INCOMPLETE$ STREAMED exit
   then
   t t-u s" filtered" STR= if
      response [: s" content_filter" INCOMPLETE ;] INCOMPLETE$ STREAMED
      exit
   then
   t t-u s" long-finish" STR= if
      response [: LONG-FINISH$ INCOMPLETE ;] INCOMPLETE$ STREAMED
      exit
   then
   t t-u s" bad-content" STR= if
      response [: BAD-CONTENT$ JSON:WRITE-RAW ;] COMPLETED$ STREAMED exit
   then
   t t-u s" failed-server" STR= if
      response [: s" server_error" s" The server had an error." FAILED ;] FAILED$ STREAMED
      exit
   then
   t t-u s" failed-size" STR= if
      response [: s" context_length_exceeded" s" Your input exceeds the context window of this model." FAILED ;] FAILED$ STREAMED
      exit
   then
   t t-u s" failed-other" STR= if
      response [: s" invalid_prompt" s" bad prompt" FAILED ;] FAILED$ STREAMED exit
   then
   \ A cancelled response's terminal event is read by its status.
   t t-u s" cancelled" STR= if
      response [: s" resp_1" s" cancelled" PENDING ;] s" response.cancelled" STREAMED exit
   then
   t t-u s" error-event" STR= if
      STREAM-OPEN s" resp_1" PRELUDE
      s" error" LAST-SEQ 1+ EVENT-OPEN
      S\" ,\qcode\q:\qserver_error\q,\qmessage\q:\qThe server had an error.\q,\qparam\q:null" JSON:WRITE-RAW
      EVENT-CLOSE response swap STREAM! exit
   then
   t t-u s" plain-json" STR= if response [: PLAIN$ JSON:WRITE-RAW ;] HTTP:JSON! exit then
   t t-u s" bad-type" STR= if response BAD-TYPE$ true BAD-STREAM exit then
   t t-u s" no-sequence" STR= if response NO-SEQUENCE$ true BAD-STREAM exit then
   t t-u s" no-id" STR= if response NO-ID$ false BAD-STREAM exit then
   t t-u s" bad-utf8" STR= if STREAM-OPEN BAD-UTF8$ JSON:WRITE-RAW response swap STREAM! exit then
   \ resp_cut's cancel is held.
   t t-u s" cut-cancel" STR= if
      STREAM-OPEN s" resp_cut" CREATED
      s" data: " JSON:WRITE-RAW BAD-TYPE$ JSON:WRITE-RAW S\" \n\n" JSON:WRITE-RAW
      response swap STREAM! exit
   then
   \ The terminal event arrives and then the connection drops mid-body.
   t t-u s" done-drop" STR= if
      r STREAM-OPEN s" resp_1" PRELUDE
      COMPLETED$ LAST-SEQ 1+ RESPONSE-OPEN CHAT-RESPONSE EVENT-CLOSE RAW-STREAM RAW-CLOSE exit
   then
   \ The streams that end early; their ids name them to the resume and cancel
   \ routes. The connections of resp_resume and resp_alive drop mid-body, the
   \ others end.
   t t-u s" resume" STR= if
      r STREAM-OPEN s" resp_resume" PRELUDE RAW-STREAM RAW-CLOSE exit
   then
   t t-u s" keepalive-resume" STR= if
      r STREAM-OPEN s" resp_alive" OPENING RAW-STREAM RAW-CLOSE exit
   then
   t t-u s" dead-resume" STR= if
      0 DEAD-RESUMES !
      STREAM-OPEN s" resp_dead" CREATED response swap STREAM! exit
   then
   t t-u s" mute-cancel" STR= if
      0 DEAD-RESUMES !
      STREAM-OPEN s" resp_mute" CREATED response swap STREAM! exit
   then
   \ A keepalive and then a drop, before an event names the response.
   t t-u s" early-drop" STR= if
      r STREAM-OPEN s" keepalive" 0 EVENT-OPEN EVENT-CLOSE RAW-STREAM RAW-CLOSE exit
   then
   t t-u s" resume-refused" STR= if
      STREAM-OPEN s" resp_refused" PRELUDE response swap STREAM! exit
   then
   t t-u s" no-progress" STR= if
      STREAM-OPEN s" resp_quiet" CREATED response swap STREAM! exit
   then
   \ The held stream is the resume, which names the response: a cancel before
   \ the call has read the created event leaves it no response to cancel.
   t t-u s" hold" STR= if
      STREAM-OPEN s" resp_hold" CREATED response swap STREAM! exit
   then
   t t-u s" http-error" STR= if
      response 429 HTTP:STATUS!
      response [: S\" {\qerror\q:{\qmessage\q:\qrate limit\q}}" JSON:WRITE-RAW ;] HTTP:JSON!
      exit
   then
   t t-u s" http-bad" STR= if
      response 400 HTTP:STATUS!
      response [: S\" {\qerror\q:{\qmessage\q:\qbad request\q}}" JSON:WRITE-RAW ;] HTTP:JSON!
      exit
   then
   \ Ollama's error member is the message itself.
   t t-u s" http-text" STR= if
      response 404 HTTP:STATUS!
      response [: S\" {\qerror\q:\qmodel not found\q}" JSON:WRITE-RAW ;] HTTP:JSON!
      exit
   then
   \ A FastAPI server's error names its reason outside an error member.
   t t-u s" http-detail" STR= if
      response 422 HTTP:STATUS!
      response [: S\" {\qdetail\q:\qbad field\q}" JSON:WRITE-RAW ;] HTTP:JSON!
      exit
   then
   t t-u s" forbidden" STR= if response 403 HTTP:STATUS! exit then
   \ OpenAI's refusal of a chat request over the model's context.
   t t-u s" too-large" STR= if
      response 400 HTTP:STATUS!
      response [: S\" {\qerror\q:{\qmessage\q:\qYour input exceeds the context window of this model.\q,\qtype\q:\qinvalid_request_error\q,\qparam\q:\qinput\q,\qcode\q:\qcontext_length_exceeded\q}}" JSON:WRITE-RAW ;] HTTP:JSON!
      exit
   then
   t t-u s" oversized" STR= if response 413 HTTP:STATUS! exit then
   t t-u s" big" STR= if response [: BIG$ COMPLETED ;] COMPLETED$ STREAMED exit then
   \ Every other scenario, the journal's long text included, starts "valid".
   t t-u 5 min s" valid" STR= if
      response [: CHAT-RESPONSE ;] COMPLETED$ STREAMED exit
   then
   response 400 HTTP:STATUS! ;


: KEEP-BODY ( HTTP:request -- )
   HTTP:BODY$ {: a:ptr u:n :}
   u SENT-CAP <= if a u SENT-BODY SENT-CAP SPAN:MAKE SPAN:COPY u else -1 then
   SENT-U ! ;


: CHAT ( HTTP:request HTTP:response -- )
   {: r:HTTP:request response:HTTP:response :}
   1 CALLS atomic-add drop
   r KEEP-BODY
   r AUTHORIZED? 0= if response 401 HTTP:STATUS! exit then
   r response REPLY ;


\ OpenAI's refusal to resume a stream older than five minutes.
: OLD-STREAM$ ( -- ptr u8 n )
   s" This response can no longer be streamed because it is more than 5 minutes old" ;


\ Asserts the resume request's query asks for the stream after event n.
: AFTER-IS ( HTTP:request n -- )
   {: r:HTTP:request n:n :}
   SB-RESET s" stream=true&starting_after=" SB-APPEND n FMT:SB-INT
   r HTTP:QUERY$ {: q:ptr q-u:n :}
   q q-u SB$ STR= dup 0= if q q-u type cr then ASSERT ;


\ A resumed stream: resp_resume sends the rest and completes, resp_refused is
\ too old to stream, resp_quiet sends no event and resp_hold holds. resp_alive's
\ first resume sends keepalives and drops, its second completes; resp_dead's
\ and resp_mute's resumes close before a byte, which libcurl retries once on a
\ fresh connection; past those two they answer 404, so a call that resumes
\ them again ends.
: RESUME ( HTTP:request HTTP:response -- )
   {: r:HTTP:request response:HTTP:response :}
   r AUTHORIZED? 0= if response 401 HTTP:STATUS! exit then
   1 RESUMES atomic-add drop
   r s" id" HTTP:SEGMENT-OF$ {: id:ptr id-u:n found:bool :}
   found ASSERT
   id id-u s" resp_quiet" STR= if
      r 0 AFTER-IS
      STREAM-OPEN response swap STREAM! exit
   then
   id id-u s" resp_hold" STR= if r 0 AFTER-IS r HOLD-STREAM exit then
   id id-u s" resp_dead" STR= id id-u s" resp_mute" STR= or if
      r 0 AFTER-IS
      1 DEAD-RESUMES +!
      DEAD-RESUMES @ 2 > if response 404 HTTP:STATUS! exit then
      r HTTP:TAKE-OVER 2drop RAW-CLOSE exit
   then
   id id-u s" resp_alive" STR= if
      r 1 AFTER-IS
      1 ALIVE-RESUMES +!
      ALIVE-RESUMES @ 1 = if
         r STREAM-OPEN s" keepalive" 7 EVENT-OPEN EVENT-CLOSE
         s" keepalive" 8 EVENT-OPEN EVENT-CLOSE RAW-STREAM RAW-CLOSE exit
      then
      STREAM-OPEN s" response.completed" 2 RESPONSE-OPEN CHAT-RESPONSE EVENT-CLOSE
      response swap STREAM! exit
   then
   r LAST-SEQ AFTER-IS
   id id-u s" resp_resume" STR= if
      STREAM-OPEN
      s" response.output_text.delta" LAST-SEQ 1+ EVENT-OPEN
      S\" ,\qdelta\q:\qwer\\\q:true}\q" JSON:WRITE-RAW EVENT-CLOSE
      s" response.completed" LAST-SEQ 2 + RESPONSE-OPEN CHAT-RESPONSE EVENT-CLOSE
      response swap STREAM! exit
   then
   id id-u s" resp_refused" STR= if
      response 400 HTTP:STATUS!
      response [: S\" {\qerror\q:{\qmessage\q:\q" JSON:WRITE-RAW OLD-STREAM$ JSON:WRITE-RAW S\" \q,\qtype\q:\qinvalid_request_error\q,\qparam\q:null,\qcode\q:null}}" JSON:WRITE-RAW ;] HTTP:JSON!
      exit
   then
   response 404 HTTP:STATUS! ;


: CANCEL ( HTTP:request HTTP:response -- )
   {: r:HTTP:request response:HTTP:response :}
   r AUTHORIZED? 0= if response 401 HTTP:STATUS! exit then
   r s" id" HTTP:SEGMENT-OF$ {: id:ptr id-u:n found:bool :}
   found ASSERT
   1 CANCELS atomic-add drop
   \ resp_dead's provider fails the cancel too, resp_mute's closes it
   \ unanswered; resp_cut's holds it.
   id id-u s" resp_dead" STR= if response 500 HTTP:STATUS! exit then
   id id-u s" resp_mute" STR= if r HTTP:TAKE-OVER 2drop RAW-CLOSE exit then
   id id-u s" resp_cut" STR= if r HTTP:TAKE-OVER 2drop HOLD-OPEN exit then
   response [: S\" {\qstatus\q:\qcancelled\q}" JSON:WRITE-RAW ;] HTTP:JSON! ;


\ One vector whose first component is the input's byte length.
: LENGTH-VECTOR ( ptr JSON:writer -- ptr JSON:writer )
   S\" {\qobject\q:\qlist\q,\qdata\q:[{\qindex\q:0,\qembedding\q:[" JSON:WRITE-RAW
   RECEIVED-U @ JSON:WRITE-U
   s" ,0,0]}]}" JSON:WRITE-RAW ;


\ A vector's dimension count past any one provider's; the embedding past it
\ counts up from 0.
4096 constant WIDE-DIMS
: WIDE-VECTOR ( ptr JSON:writer -- ptr JSON:writer )
   S\" {\qobject\q:\qlist\q,\qdata\q:[{\qindex\q:0,\qembedding\q:[0" JSON:WRITE-RAW
   WIDE-DIMS 1 ?do s" ," JSON:WRITE-RAW i JSON:WRITE-U loop
   s" ]}]}" JSON:WRITE-RAW ;


\ Responds by the request's inputs: a bound input is all x.
: EMBEDDINGS ( HTTP:request HTTP:response -- )
   {: r:HTTP:request response:HTTP:response :}
   1 CALLS atomic-add drop
   r KEEP-BODY
   r AUTHORIZED? 0= if response 401 HTTP:STATUS! exit then
   r INPUT-LINES RECEIVED$ {: i:ptr i-u:n :}
   i i-u 1 min s" x" STR= if
      response [: LENGTH-VECTOR ;] HTTP:JSON! exit
   then
   i i-u S\" one\ntwo" STR= if
      \ The provider may list them out of order; index decides each slot.
      response [: S\" {\qobject\q:\qlist\q,\qdata\q:[{\qembedding\q:[0.4,0.5,0.6],\qindex\q:1},{\qindex\q:0,\qembedding\q:[0.1,0.2,0.3]}],\qusage\q:{\qprompt_tokens\q:2,\qtotal_tokens\q:2}}" JSON:WRITE-RAW ;] HTTP:JSON!
      exit
   then
   i i-u s" wide" STR= if
      response [: WIDE-VECTOR ;] HTTP:JSON! exit
   then
   i i-u s" wrong-dims" STR= if
      response [: S\" {\qobject\q:\qlist\q,\qdata\q:[{\qembedding\q:[0.1,0.2],\qindex\q:0}]}" JSON:WRITE-RAW ;] HTTP:JSON!
      exit
   then
   i i-u s" wrong-index" STR= if
      response [: S\" {\qobject\q:\qlist\q,\qdata\q:[{\qembedding\q:[0.1,0.2,0.3],\qindex\q:1}]}" JSON:WRITE-RAW ;] HTTP:JSON!
      exit
   then
   \ OpenAI's embeddings refusal names its cause only by the message.
   i i-u s" too-large" STR= if
      response 400 HTTP:STATUS!
      response [: S\" {\qerror\q:{\qmessage\q:\qThis model's maximum context length is 8192 tokens, however you requested 9000 tokens (9000 in your prompt; 0 for the completion). Please reduce your prompt; or completion length.\q,\qtype\q:\qinvalid_request_error\q,\qparam\q:null,\qcode\q:null}}" JSON:WRITE-RAW ;] HTTP:JSON!
      exit
   then
   response 400 HTTP:STATUS! ;


\ Ollama's /api/embed: no Authorization header, and truncation refused.
: OLLAMA ( HTTP:request HTTP:response -- )
   {: r:HTTP:request response:HTTP:response :}
   1 CALLS atomic-add drop
   r s" Authorization" HTTP:HEADER-OF$ {: a:ptr u:n present:bool :}
   present if response 401 HTTP:STATUS! exit then
   r s" truncate" BODY-MEMBER JSON:READ-TOKEN JSON:T-FALSE = ASSERT JSON:READ-CLOSE
   r INPUT-LINES RECEIVED$ {: i:ptr i-u:n :}
   i i-u S\" one\ntwo" STR= if
      response [: S\" {\qembeddings\q:[[0.1,0.2,0.3],[0,1,2]]}" JSON:WRITE-RAW ;] HTTP:JSON!
      exit
   then
   i i-u s" bad-count" STR= if
      response [: S\" {\qembeddings\q:[]}" JSON:WRITE-RAW ;] HTTP:JSON! exit
   then
   i i-u s" bad-dims" STR= if
      response [: S\" {\qembeddings\q:[[0,1]]}" JSON:WRITE-RAW ;] HTTP:JSON! exit
   then
   i i-u s" bad-finite" STR= if
      response [: S\" {\qembeddings\q:[[1e309,0,1]]}" JSON:WRITE-RAW ;] HTTP:JSON! exit
   then
   i i-u s" too-large" STR= if
      response 400 HTTP:STATUS!
      response [: S\" {\qerror\q:\qthe input length exceeds the context length\q}" JSON:WRITE-RAW ;] HTTP:JSON!
      exit
   then
   response 400 HTTP:STATUS! ;


: URL! ( ptr u8 ptr u8 n -- n )
   {: dst:ptr path:ptr path-u:n :}
   SB-RESET s" http://127.0.0.1:" SB-APPEND HTTP:PORT FMT:SB-INT path path-u SB-APPEND
   SB$ {: a:ptr u:n :}
   u 64 <= ASSERT
   a u dst 64 SPAN:MAKE SPAN:COPY u ;


public
EXPORT LONG-FINISH$
EXPORT REFUSAL$
EXPORT BAD-CONTENT$
EXPORT BIG$
EXPORT OLD-STREAM$
EXPORT PLAIN$
EXPORT BAD-TYPE$
EXPORT NO-SEQUENCE$
EXPORT NO-ID$
EXPORT WIDE-DIMS
: BASE-URL$ ( -- ptr u8 n ) BASE-URL BASE-URL-U @ ;
: OLLAMA-URL$ ( -- ptr u8 n ) OLLAMA-URL OLLAMA-URL-U @ ;
\ Requests the peer received, refused or served.
: CALLS@ ( -- n ) CALLS atomic@ ;
\ The last chat or embedding request the peer received; one past SENT-CAP
\ fails the test.
: SENT$ ( -- ptr u8 n ) SENT-U @ dup 0< if FAIL then SENT-BODY swap ;
\ Resumes of the streams that end early.
: RESUMES@ ( -- n ) RESUMES atomic@ ;
\ Provider cancels.
: CANCELS@ ( -- n ) CANCELS atomic@ ;
\ Waits until the peer has sent the held response's in_progress event, or
\ holds resp_cut's cancel.
: AWAIT-HOLD ( -- ) HELD TAKE ;


: START ( -- )
   BIG-U MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop BIG !
   BIG-FILL
   0 HELD TASK:SEMAPHORE-INIT
   0 CALLS atomic!
   0 RESUMES atomic!
   STREAM-B 1 BUF:N>BLEN BUF:INIT
   0 CANCELS atomic!
   0 ALIVE-RESUMES !
   HTTP:ROUTES-RESET
   s" POST" s" /responses" [: CHAT ;] HTTP:ROUTE
   s" GET" s" /responses/{id}" [: RESUME ;] HTTP:ROUTE
   s" POST" s" /responses/{id}/cancel" [: CANCEL ;] HTTP:ROUTE
   s" POST" s" /embeddings" [: EMBEDDINGS ;] HTTP:ROUTE
   s" POST" s" /api/embed" [: OLLAMA ;] HTTP:ROUTE
   $7F000001 0 2 1000 HTTP:START
   BASE-URL s" " URL! BASE-URL-U !
   OLLAMA-URL s" /api/embed" URL! OLLAMA-URL-U ! ;


: STOP ( -- )
   HTTP:STOP
   HELD TASK:SEMAPHORE-DESTROY
   STREAM-B BUF:DISPOSE
   BIG$ MEM:BYTES-ALLOC-LEN MEM:RELEASE-BYTES ;
;using
;package


package MODEL-TEST
using MODEL-FIXTURE
private
1 TYPED-BUFFER CALL MODEL:call
$10000 TASK:TASK WORK-TASK
TASK:SEMAPHORE STOP-READY
TASK:SEMAPHORE STOP-GO
1 TYPED-BUFFER SETS MODEL:settings
\ A 16 KiB embedding input.
16384 constant PAD-U
create PAD PAD-U allot
\ "valid" and 20,000 bytes: a journal input past 16 KiB.
20005 constant JOURNAL-U
create JOURNAL-TEXT JOURNAL-U allot
create JOURNAL-INPUT 32768 allot
create JOURNAL-RECORD 65536 allot
variable JOURNAL-INPUT-U
variable JOURNAL-RECORD-U
variable JOURNAL-READY
\ Random dimension counts between their deterministic bounds.
32 constant BOUND-TRIALS
\ A strict schema for {"answer":true}.
: SCHEMA$ ( -- ptr u8 n )
   S\" {\qtype\q:\qobject\q,\qadditionalProperties\q:false,\qproperties\q:{\qanswer\q:{\qtype\q:\qboolean\q}},\qrequired\q:[\qanswer\q]}" ;
: M ( -- ptr MODEL:call ) 0 CALL ;
: TRUE-CONTENT$ ( -- ptr u8 n ) S\" {\qanswer\q:true}" ;
: CODE-OF ( n [ n -- n ] -- n ) catch {: code:n :} drop code ;
: CHAT-CODE ( -- n ) M [: dup MODEL:CHAT 2drop ;] catch {: code:n :} drop code ;
: EMBED-CODE ( -- n ) M [: dup MODEL:EMBED ;] catch {: code:n :} drop code ;
: PREP ( ptr u8 n -- ) M MODEL:PARTS-RESET M MODEL:TEXT+ ;
: INPUTS ( ptr u8 n -- ) M MODEL:INPUT-RESET M MODEL:INPUT+ ;
: CHAT ( -- ) s" valid" PREP M MODEL:CHAT TRUE-CONTENT$ STR= ASSERT ;
: FAILS ( ptr u8 n n -- ) {: a:ptr u:n want:n :} a u PREP CHAT-CODE want = ASSERT ;
: INPUT-LEN ( n -- n ) {: u:n :} PAD u M MODEL:INPUT+ u ;
: DIMS ( n -- n ) {: d:n :} s" test-embedding" d M MODEL:EMBEDDING-MODEL! d ;


: XS ( ptr u8 n -- ) {: a:ptr u:n :} u 0 ?do [char] x a i + c! loop ;


$A constant LF
create LOG-PATH PATH-CAP allot      \ the run's stderr capture file
variable LOG-PATH-U
create LOG-B BUF:HDR-BYTES allot    \ the capture file's bytes, read back
create REFUSED-BASE 64 allot        \ a base URL whose port nothing listens on
variable REFUSED-BASE-U

: LOG-PATH$ ( -- ptr u8 n ) LOG-PATH LOG-PATH-U @ ;
: REFUSED-BASE$ ( -- ptr u8 n ) REFUSED-BASE REFUSED-BASE-U @ ;


: LOG-INIT ( -- )
   s" model-test" FS:MKTEMP-DIR {: dir:ptr dir-u:n :}
   dir dir-u FS:CLEANUP-TREE+
   dir dir-u s" stderr.txt" LOG-PATH FS:JOIN-PATH LOG-PATH-U !
   LOG-B 1 BUF:N>BLEN BUF:INIT ;


\ The whole capture file, valid until the next read.
: LOG$ ( -- ptr u8 n )
   LOG-PATH$ FS:FILE-SIZE {: size:n :}
   LOG-B BUF:CLEAR
   LOG-PATH$ LOG-B size BUF:N>BLEN BUF:GROW SPAN:$ FS:READ-ALL size = ASSERT
   LOG-B BUF:SPAN$ BUF:BLEN>N ;


: LINES ( -- n )
   LOG$ {: a:ptr u:n :}
   0 u 0 ?do a i + c@ LF = if 1+ then loop ;


\ Whether a captured line, after its stamp, is exactly the text.
: LINE? ( ptr u8 n ptr u8 n -- bool )
   {: line:ptr line-u:n want:ptr want-u:n :}
   line-u DATE:TIME-LEN < if false exit then
   line DATE:TIME-LEN + line-u DATE:TIME-LEN - want want-u STR= ;


: LOGGED? ( ptr u8 n -- bool )
   {: want:ptr want-u:n :}
   LOG$ {: a:ptr u:n :}
   0 u 0 ?do
      a i + c@ LF = if
         i over - over a swap + swap want want-u LINE? if drop true unloop exit then
         drop i 1+
      then
   loop drop false ;


\ SB holds a log line's text up to its code: ` <level> model: <kind> <model>
\ <base><path> `.
: SB-LINE ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   {: level:ptr level-u:n who:ptr who-u:n base:ptr base-u:n path:ptr path-u:n :}
   SB-RESET s"  " SB-APPEND level level-u SB-APPEND s"  model: " SB-APPEND
   who who-u SB-APPEND s"  " SB-APPEND
   base base-u SB-APPEND path path-u SB-APPEND s"  " SB-APPEND ;


: CHAT-LINE ( ptr u8 n -- )
   {: level:ptr level-u:n :}
   level level-u s" chat test-generation" MODEL-PEER:BASE-URL$ s" /responses" SB-LINE ;


: EMBED-LINE ( ptr u8 n -- )
   {: level:ptr level-u:n :}
   level level-u s" embed test-embedding" MODEL-PEER:BASE-URL$ s" /embeddings" SB-LINE ;


\ CHAT-LINE for a reply that named its response id.
: CHAT-ID-LINE ( ptr u8 n -- )
   {: level:ptr level-u:n :}
   level level-u s" chat test-generation" MODEL-PEER:BASE-URL$ s" /responses resp_1" SB-LINE ;


\ A failed check prints the capture, which the failed run's exit deletes.
: SB-LOGGED ( -- )
   SB$ LOGGED? dup 0= if SB$ type cr LOG$ type then ASSERT ;


: CHAT-LOGGED ( ptr u8 n ptr u8 n -- )
   {: level:ptr level-u:n tail:ptr tail-u:n :}
   level level-u CHAT-LINE tail tail-u SB-APPEND SB-LOGGED ;


\ CHAT-LOGGED for a reply that named its response id.
: CHAT-ID-LOGGED ( ptr u8 n ptr u8 n -- )
   {: level:ptr level-u:n tail:ptr tail-u:n :}
   level level-u CHAT-ID-LINE tail tail-u SB-APPEND SB-LOGGED ;


\ The body logs no line.
: QUIET ( [ -- ] -- )
   LINES {: before:n :}
   execute
   LINES before = dup 0= if LOG$ type then ASSERT ;


: FIXTURE-KEY ( -- ) KEY-SERVICE$ KEY-ACCOUNT$ M MODEL:API-KEY! ;


: SETUP ( -- )
   M MODEL:OPEN
   MODEL-PEER:BASE-URL$ M MODEL:BASE-URL!
   s" test-generation" M MODEL:GENERATION-MODEL!
   s" test-embedding" 3 M MODEL:EMBEDDING-MODEL!
   FIXTURE-KEY
   s" Read source evidence." M MODEL:SYSTEM!
   SCHEMA$ M MODEL:SCHEMA! ;


: IMAGE ( -- )
   s" image" PREP
   s" image/png" s" aGVsbG8=" M MODEL:IMAGE+
   M MODEL:CHAT S\" {\qimages\q:[\qdata:image/png;base64,aGVsbG8=\q]}" STR= ASSERT ;


: SERVICE$ ( -- ptr u8 n ) KEY-SERVICE$ ;
\ The second key item's account.
: ACCOUNT$ ( -- ptr u8 n ) s" model-store" ;
: STORE ( ptr u8 n -- ) {: v:ptr vu:n :} SERVICE$ ACCOUNT$ v vu SECRET:WRITE ;
: STORED-FAILS ( ptr u8 n -- ) STORE s" valid" MODEL:E-KEY FAILS ;


\ A missing item, then the peer's key, then an empty and a non-printable
\ value, then a 4096-byte value that is accepted and sent: the peer's 401
\ refuses it, also E-KEY.
: KEY-CASES ( -- )
   SERVICE$ ACCOUNT$ M MODEL:API-KEY!
   [: s" valid" MODEL:E-KEY FAILS ;] QUIET
   s" local-test-key" STORE
   CHAT
   s" " STORED-FAILS
   s\" local\x01test-key" STORED-FAILS
   PAD 4096 XS
   PAD 4096 STORE
   s" valid" MODEL:E-KEY FAILS
   M MODEL:HTTP-STATUS 401 = ASSERT ;


\ API-KEY! reads a second item; the fixture key is then back in force.
: STORE-KEY ( -- ) KEY-CASES FIXTURE-KEY ;


\ A negative length is refused before the setter touches the key, so the prior
\ key stays in force.
: NEGATIVE-ACCOUNT ( n -- n ) SERVICE$ s" account" drop -1 M MODEL:API-KEY! ;
: NEGATIVE-KEY ( -- )
   0 [: NEGATIVE-ACCOUNT ;] CODE-OF MODEL:E-CONFIG = ASSERT
   CHAT ;


: BAD-IMAGE ( n -- n ) s" image/png" s" invalid!" M MODEL:IMAGE+ ;


: INVALID-IMAGE ( -- )
   M MODEL:PARTS-RESET
   0 [: BAD-IMAGE ;] CODE-OF MODEL:E-CONFIG = ASSERT ;


: INVALID-SCHEMA ( -- )
   s" {bad" M MODEL:SCHEMA!
   [: s" valid" MODEL:E-CONFIG FAILS ;] QUIET
   SCHEMA$ M MODEL:SCHEMA! ;


\ The resume requests the peer serves while the body runs.
: RESUMED ( [ -- ] -- n )
   MODEL-PEER:RESUMES@ {: before:n :}
   execute
   MODEL-PEER:RESUMES@ before - ;


\ The provider cancels the peer serves while the body runs.
: CANCEL-POSTS ( [ -- ] -- n )
   MODEL-PEER:CANCELS@ {: before:n :}
   execute
   MODEL-PEER:CANCELS@ before - ;


: REPLIES ( -- )
   s" refusal" MODEL:E-REFUSED FAILS
   s" error" CHAT-ID-LINE s" MODEL:E-REFUSED status 200 body: " SB-APPEND
   MODEL-PEER:REFUSAL$ SB-APPEND
   SB-LOGGED
   s" length" MODEL:E-INCOMPLETE FAILS
   M MODEL:INCOMPLETE-REASON$ s" max_output_tokens" STR= ASSERT
   s" warn" s" MODEL:E-INCOMPLETE incomplete max_output_tokens" CHAT-ID-LOGGED
   s" filtered" MODEL:E-INCOMPLETE FAILS
   M MODEL:INCOMPLETE-REASON$ s" content_filter" STR= ASSERT
   s" long-finish" MODEL:E-INCOMPLETE FAILS
   M MODEL:INCOMPLETE-REASON$ MODEL-PEER:LONG-FINISH$ STR= ASSERT
   s" bad-content" MODEL:E-RESPONSE FAILS
   M MODEL:INCOMPLETE-REASON$ nip 0= ASSERT
   s" error" CHAT-ID-LINE s" MODEL:E-RESPONSE status 200 body: " SB-APPEND
   MODEL-PEER:BAD-CONTENT$ SB-APPEND
   SB-LOGGED
   s" failed-server" MODEL:E-UNAVAILABLE FAILS
   s" error" s" MODEL:E-UNAVAILABLE status 200: The server had an error." CHAT-ID-LOGGED
   s" failed-size" MODEL:E-CAPACITY FAILS
   s" failed-other" MODEL:E-HTTP FAILS
   M MODEL:ERROR$ s" bad prompt" STR= ASSERT
   [: s" error-event" MODEL:E-UNAVAILABLE FAILS ;] CANCEL-POSTS 1 = ASSERT
   M MODEL:ERROR$ s" The server had an error." STR= ASSERT
   s" cancelled" MODEL:E-CANCELLED FAILS
   s" big" PREP M MODEL:CHAT MODEL-PEER:BIG$ STR= ASSERT
   s" http-bad" MODEL:E-HTTP FAILS
   M MODEL:HTTP-STATUS 400 = ASSERT
   M MODEL:ERROR$ s" bad request" STR= ASSERT
   s" error" s" MODEL:E-HTTP status 400: bad request" CHAT-LOGGED
   CHAT M MODEL:ERROR$ nip 0= ASSERT
   s" http-text" MODEL:E-CONFIG FAILS
   M MODEL:HTTP-STATUS 404 = ASSERT
   M MODEL:ERROR$ s" model not found" STR= ASSERT
   s" error" s" MODEL:E-CONFIG status 404: model not found" CHAT-LOGGED
   s" http-detail" MODEL:E-HTTP FAILS
   s" error" S\" MODEL:E-HTTP status 422 body: {\qdetail\q:\qbad field\q}" CHAT-LOGGED
   s" forbidden" MODEL:E-KEY FAILS
   M MODEL:HTTP-STATUS 403 = ASSERT
   s" error" s" MODEL:E-KEY status 403" CHAT-LOGGED
   \ A fault before sending reports no earlier call's status: a builder's,
   \ and one in a chat call made again over the same parts.
   INVALID-IMAGE
   M MODEL:HTTP-STATUS 0= ASSERT
   s" forbidden" MODEL:E-KEY FAILS
   s" {bad" M MODEL:SCHEMA!
   [: CHAT-CODE MODEL:E-CONFIG = ASSERT ;] QUIET
   M MODEL:HTTP-STATUS 0= ASSERT
   SCHEMA$ M MODEL:SCHEMA!
   s" too-large" MODEL:E-CAPACITY FAILS
   M MODEL:HTTP-STATUS 400 = ASSERT
   s" warn" s" MODEL:E-CAPACITY status 400: Your input exceeds the context window of this model." CHAT-LOGGED
   s" oversized" MODEL:E-CAPACITY FAILS
   M MODEL:HTTP-STATUS 413 = ASSERT
   M MODEL:ERROR$ nip 0= ASSERT ;


\ Streams that end early resume, or end the call and cancel a known response;
\ a failed cancel logs its status, or libcurl's code when closed unanswered.
: EARLY-ENDS ( -- )
   [: s" resume" PREP M MODEL:CHAT TRUE-CONTENT$ STR= ASSERT ;] RESUMED 1 = ASSERT
   [: s" done-drop" PREP M MODEL:CHAT TRUE-CONTENT$ STR= ASSERT ;] CANCEL-POSTS 0= ASSERT
   M MODEL:HTTP-STATUS 200 = ASSERT
   [: s" early-drop" MODEL:E-TRANSPORT FAILS ;] CANCEL-POSTS 0= ASSERT
   s" error" CHAT-LINE s" MODEL:E-TRANSPORT curl 18: " SB-APPEND
   18 CURL:>CODE CURL:ERROR$ SB-APPEND SB-LOGGED
   [: s" keepalive-resume" PREP CHAT-CODE 0= ASSERT ;] RESUMED 2 = ASSERT
   [: s" resume-refused" MODEL:E-HTTP FAILS ;] CANCEL-POSTS 1 = ASSERT
   M MODEL:ERROR$ MODEL-PEER:OLD-STREAM$ STR= ASSERT
   s" error" s" chat test-generation" MODEL-PEER:BASE-URL$ s" /responses resp_refused" SB-LINE
   s" MODEL:E-HTTP status 400: " SB-APPEND MODEL-PEER:OLD-STREAM$ SB-APPEND SB-LOGGED
   [: s" dead-resume" MODEL:E-TRANSPORT FAILS ;] CANCEL-POSTS 1 = ASSERT
   SB-RESET s"  warn model: cancel resp_dead MODEL:E-UNAVAILABLE status 500" SB-APPEND SB-LOGGED
   s" mute-cancel" MODEL:E-TRANSPORT FAILS
   SB-RESET s"  warn model: cancel resp_mute MODEL:E-TRANSPORT curl 52: " SB-APPEND
   52 CURL:>CODE CURL:ERROR$ SB-APPEND SB-LOGGED
   [: s" no-progress" MODEL:E-RESPONSE FAILS ;] RESUMED 1 = ASSERT ;


\ SB holds the log line's text up to its code; the line has the unusable
\ reply's status 200 and the whole body.
: BODY-LOGGED ( ptr u8 n -- )
   {: a:ptr u:n :}
   s" MODEL:E-RESPONSE status 200 body: " SB-APPEND a u SB-APPEND SB-LOGGED ;


\ A reply the call cannot read is logged whole, an event that is not UTF-8 as
\ its chunk with line feeds escaped; one after the response id cancels it.
: UNREADABLE ( -- )
   s" plain-json" MODEL:E-RESPONSE FAILS
   s" error" CHAT-LINE MODEL-PEER:PLAIN$ BODY-LOGGED
   [: s" bad-type" MODEL:E-RESPONSE FAILS ;] CANCEL-POSTS 1 = ASSERT
   s" error" CHAT-ID-LINE MODEL-PEER:BAD-TYPE$ BODY-LOGGED
   s" no-sequence" MODEL:E-RESPONSE FAILS
   s" error" CHAT-ID-LINE MODEL-PEER:NO-SEQUENCE$ BODY-LOGGED
   s" no-id" MODEL:E-RESPONSE FAILS
   s" error" CHAT-LINE MODEL-PEER:NO-ID$ BODY-LOGGED
   s" bad-utf8" MODEL:E-RESPONSE FAILS
   s" error" CHAT-ID-LINE
   S\" event: response.created\\x0adata: {\qtype\q:\qresponse.created\q,\qsequence_number\q:0,\qresponse\q:{\qid\q:\qresp_1\q,\qstatus\q:\qqueued\q,\qoutput\q:[]}}\\x0a\\x0adata: \xff\\x0a\\x0a" BODY-LOGGED ;


\ A loopback port nothing listens on: bound, then closed.
: CLOSED-PORT ( -- n )
   $7F000001 TCP4:ADDRESS 0 TCP4:PORT TCP4:BIND
   MATCH TCP4:bind-result
      bound OF TCP4:LISTENER>N ENDOF
      failed OF TCP4:ERRNO>N drop FAIL -1 ENDOF
   ;MATCH {: fd:n :}
   fd TCP4:>LISTENER TCP4:LOCAL
   MATCH TCP4:endpoint-result
      endpoint OF TCP4:PORT>N swap TCP4:ADDRESS>N drop ENDOF
      failed OF TCP4:ERRNO>N drop FAIL 0 ENDOF
   ;MATCH {: port:n :}
   fd TCP4:>LISTENER TCP4:CLOSE-LISTENER
   MATCH TCP4:status
      ok OF ENDOF
      failed OF TCP4:ERRNO>N drop FAIL ENDOF
   ;MATCH
   port ;


\ A refused connection is the transport's failure, logged with libcurl's code
\ 7 and its text.
: REFUSED ( -- )
   SB-RESET s" http://127.0.0.1:" SB-APPEND CLOSED-PORT FMT:SB-INT
   SB$ {: a:ptr u:n :}
   u 64 <= ASSERT
   a u REFUSED-BASE 64 SPAN:MAKE SPAN:COPY u REFUSED-BASE-U !
   REFUSED-BASE$ M MODEL:BASE-URL!
   s" valid" MODEL:E-TRANSPORT FAILS
   MODEL-PEER:BASE-URL$ M MODEL:BASE-URL!
   s" error" s" chat test-generation" REFUSED-BASE$ s" /responses" SB-LINE
   s" MODEL:E-TRANSPORT curl 7: " SB-APPEND 7 CURL:>CODE CURL:ERROR$ SB-APPEND SB-LOGGED ;


: CHAT-WORK ( -- )
   M MODEL:CHAT 2drop
   0 TASK:RETURN ;


: CANCELLED ( -- )
   WORK-TASK TASK:JOIN MATCH result
      ok OF drop FAIL ENDOF
      err OF MODEL:E-CANCELLED = ASSERT ENDOF
   ;MATCH ;


\ Starts the hold scenario's call on the worker and waits until the peer has
\ sent its in_progress event; answers the provider cancels so far.
: HOLD ( -- n )
   s" hold" PREP
   MODEL-PEER:CANCELS@
   ['] CHAT-WORK WORK-TASK TASK:ACTIVATE
   MODEL-PEER:AWAIT-HOLD ;


\ A stop ends the held stream's transfer at once.
1000000000 constant PROMPT-NS


\ The held call, stopped at mono-ns t0, ended cancelled at once and cancelled
\ its response once.
: HOLD-ENDED ( n n -- )
   {: before:n t0:n :}
   CANCELLED
   mono-ns t0 - PROMPT-NS < ASSERT
   MODEL-PEER:CANCELS@ before 1+ = ASSERT ;


: CANCELLATION ( -- )
   HOLD mono-ns
   M MODEL:CANCEL ASSERT
   HOLD-ENDED
   M MODEL:CANCEL 0= ASSERT ;


\ A CANCEL while the call cancels its response cuts the provider cancel, which
\ is logged cancelled with libcurl's code 42 and its text.
: CANCEL-CUT ( -- )
   s" cut-cancel" PREP
   ['] CHAT-WORK WORK-TASK TASK:ACTIVATE
   MODEL-PEER:AWAIT-HOLD
   M MODEL:CANCEL ASSERT
   CANCELLED
   SB-RESET s"  warn model: cancel resp_cut MODEL:E-CANCELLED curl 42: " SB-APPEND
   42 CURL:>CODE CURL:ERROR$ SB-APPEND SB-LOGGED ;


\ The first CHAT has returned before STOP-READY is signalled. The worker
\ cannot begin either following call until the main task releases STOP-GO.
: STOP-WORK ( -- )
   CHAT-CODE {: first-code:n :}
   STOP-READY TASK:SIGNAL-SEM
   STOP-GO TASK:WAIT-SEM
   first-code 0<> if first-code throw then
   CHAT-CODE MODEL:E-CANCELLED = ASSERT
   EMBED-CODE MODEL:E-CANCELLED = ASSERT
   0 TASK:RETURN ;


\ A stop between calls refuses both following calls without a provider call.
: STOP-BETWEEN ( -- )
   s" valid" PREP
   0 STOP-READY TASK:SEMAPHORE-INIT
   0 STOP-GO TASK:SEMAPHORE-INIT
   ['] STOP-WORK WORK-TASK TASK:ACTIVATE
   STOP-READY TASK:WAIT-SEM
   MODEL-PEER:CALLS@ {: calls:n :}
   s" stopped-chat" PREP
   s" stopped-embed" INPUTS
   M MODEL:REQUEST-STOP
   M MODEL:CANCEL 0= ASSERT
   STOP-GO TASK:SIGNAL-SEM
   WORK-TASK TASK:JOIN MATCH result
      ok OF 0= ASSERT ENDOF
      err OF drop FAIL ENDOF
   ;MATCH
   MODEL-PEER:CALLS@ calls = ASSERT
   STOP-GO TASK:SEMAPHORE-DESTROY
   STOP-READY TASK:SEMAPHORE-DESTROY ;


: STOP-ACTIVE ( -- )
   HOLD mono-ns
   M MODEL:REQUEST-STOP
   HOLD-ENDED
   CHAT-CODE MODEL:E-CANCELLED = ASSERT ;


: EMBEDDINGS ( -- )
   s" one" INPUTS
   s" two" M MODEL:INPUT+
   M MODEL:EMBED
   0 M MODEL:VECTOR@ s" [0.1,0.2,0.3]" STR= ASSERT
   1 M MODEL:VECTOR@ s" [0.4,0.5,0.6]" STR= ASSERT ;


: BAD-VECTORS ( -- )
   s" wrong-dims" INPUTS
   EMBED-CODE MODEL:E-RESPONSE = ASSERT
   s" error" EMBED-LINE s" MODEL:E-RESPONSE status 200 body: " SB-APPEND
   S\" {\qobject\q:\qlist\q,\qdata\q:[{\qembedding\q:[0.1,0.2],\qindex\q:0}]}" SB-APPEND
   SB-LOGGED
   s" wrong-index" INPUTS
   EMBED-CODE MODEL:E-RESPONSE = ASSERT ;


\ The provider's refusal for size fails the call with its status.
: EMBED-TOO-LARGE ( -- )
   s" too-large" INPUTS
   EMBED-CODE MODEL:E-CAPACITY = ASSERT
   M MODEL:HTTP-STATUS 400 = ASSERT
   0 [: s" " INPUTS ;] CODE-OF MODEL:E-CONFIG = ASSERT
   M MODEL:HTTP-STATUS 0= ASSERT ;


\ A 16 KiB input reaches the provider whole; a call
\ holds eight inputs, none empty, and returns vectors only for its inputs.
: INPUT-BOUNDS ( -- )
   PAD PAD-U XS
   M MODEL:INPUT-RESET
   PAD-U INPUT-LEN drop
   M MODEL:EMBED
   SB-RESET s" [" SB-APPEND PAD-U FMT:SB-INT s" ,0,0]" SB-APPEND
   0 M MODEL:VECTOR@ SB$ STR= ASSERT
   0 [: drop 1 M MODEL:VECTOR@ 2drop 0 ;] CODE-OF MODEL:E-CONFIG = ASSERT
   M MODEL:INPUT-RESET
   0 [: INPUT-LEN ;] CODE-OF MODEL:E-CONFIG = ASSERT
   8 0 ?do 1 INPUT-LEN drop loop
   1 [: INPUT-LEN ;] CODE-OF MODEL:E-CAPACITY = ASSERT ;


\ The vector text is the WIDE-VECTOR components, 0 up.
: WIDE-IS ( ptr u8 n -- )
   {: v:ptr v-u:n :}
   v-u 0 > ASSERT
   v c@ [char] [ = ASSERT
   1 MODEL-PEER:WIDE-DIMS 0 ?do
      {: at:n :}
      SB-RESET i FMT:SB-INT SB$ {: d:ptr d-u:n :}
      at d-u + v-u < ASSERT
      v at + d-u d d-u STR= ASSERT
      v at + d-u + c@
         i MODEL-PEER:WIDE-DIMS 1- = if [char] ] else [char] , then = ASSERT
      at d-u + 1+
   loop
   v-u = ASSERT ;


\ Every positive dimension count is accepted and 0 is refused; a
\ MODEL-PEER:WIDE-DIMS vector embeds whole. The embeds that follow run at
\ three.
: DIMS-PROPERTY ( -- )
   0 [: DIMS ;] CODE-OF MODEL:E-CONFIG = ASSERT
   1 DIMS drop
   PROP:DEFAULT-SEED BOUND-TRIALS PROP:RUN-RESET
   PROP:COUNT@ 0 ?do MODEL-PEER:WIDE-DIMS PROP:RND% 1+ DIMS drop loop
   MODEL-PEER:WIDE-DIMS DIMS drop
   s" wide" INPUTS
   M MODEL:EMBED
   0 M MODEL:VECTOR@ WIDE-IS
   3 DIMS drop ;


\ Refused bases: another scheme, no host, a blank, a trailing slash, and
\ userinfo or a query that could carry a credential, in a base or an Ollama
\ endpoint. A refusal keeps the configured base.
: FTP-BASE ( n -- n ) s" ftp://x" M MODEL:BASE-URL! ;
: BARE-BASE ( n -- n ) s" http://" M MODEL:BASE-URL! ;
: BLANK-BASE ( n -- n ) s" http://x y" M MODEL:BASE-URL! ;
: SLASH-BASE ( n -- n ) s" http://x/" M MODEL:BASE-URL! ;
: USER-BASE ( n -- n ) s" https://user:token@x" M MODEL:BASE-URL! ;
: QUERY-BASE ( n -- n ) s" https://x/v1?key=k" M MODEL:BASE-URL! ;
: USER-OLLAMA ( n -- n ) s" http://user:token@x/api/embed" s" bge-m3" s" bge-m3" 3 M MODEL:OLLAMA! ;
: BASES ( -- )
   0 [: FTP-BASE ;] CODE-OF MODEL:E-CONFIG = ASSERT
   0 [: BARE-BASE ;] CODE-OF MODEL:E-CONFIG = ASSERT
   0 [: BLANK-BASE ;] CODE-OF MODEL:E-CONFIG = ASSERT
   0 [: SLASH-BASE ;] CODE-OF MODEL:E-CONFIG = ASSERT
   0 [: USER-BASE ;] CODE-OF MODEL:E-CONFIG = ASSERT
   0 [: QUERY-BASE ;] CODE-OF MODEL:E-CONFIG = ASSERT
   0 [: USER-OLLAMA ;] CODE-OF MODEL:E-CONFIG = ASSERT ;


\ A hit requires the stored journal input, so a hit is that exact request.
: JOURNAL-GET ( n ptr u8 n ptr n -- bool )
   {: ctx:n input:ptr input-u:n out:ptr :}
   JOURNAL-READY @ 0= if false exit then
   input input-u JOURNAL-INPUT JOURNAL-INPUT-U @ STR= ASSERT
   JOURNAL-RECORD JOURNAL-RECORD-U @ BUF:N>BLEN out BUF:REPLACE
   true ;


: JOURNAL-PUT ( n ptr u8 n ptr u8 n -- )
   {: ctx:n input:ptr input-u:n record:ptr record-u:n :}
   JOURNAL-READY @ 0= ASSERT
   input-u 32768 <= record-u 65536 <= and ASSERT
   input input-u JOURNAL-INPUT 32768 SPAN:MAKE SPAN:COPY
   input-u JOURNAL-INPUT-U !
   record record-u JOURNAL-RECORD 65536 SPAN:MAKE SPAN:COPY
   record-u JOURNAL-RECORD-U !
   true JOURNAL-READY ! ;


\ A journal that never hits and keeps the last input and record put.
: CAPTURE-GET ( n ptr u8 n ptr n -- bool ) 2drop 2drop false ;


: CAPTURE-PUT ( n ptr u8 n ptr u8 n -- )
   {: ctx:n input:ptr input-u:n record:ptr record-u:n :}
   input-u 32768 <= record-u 65536 <= and ASSERT
   input input-u JOURNAL-INPUT 32768 SPAN:MAKE SPAN:COPY
   input-u JOURNAL-INPUT-U !
   record record-u JOURNAL-RECORD 65536 SPAN:MAKE SPAN:COPY
   record-u JOURNAL-RECORD-U ! ;


: CAPTURE ( -- )
   0 [: CAPTURE-GET ;] [: CAPTURE-PUT ;] M MODEL:JOURNAL!
   0 JOURNAL-RECORD-U ! ;
: RECORD$ ( -- ptr u8 n ) JOURNAL-RECORD JOURNAL-RECORD-U @ ;
: INPUT$ ( -- ptr u8 n ) JOURNAL-INPUT JOURNAL-INPUT-U @ ;


TYPED-VARIABLE PARSER JSON:cursor


\ A reader inside a JSON object.
: READER ( ptr u8 n -- JSON:reader )
   {: a:ptr u:n :}
   PARSER a u JSON:READ-INIT
   JSON:READ-NEXT JSON:T-OBJ = ASSERT ;


\ A reader at the value of a JSON object's member `key`.
: MEMBER ( ptr u8 n ptr u8 n -- JSON:reader )
   {: a:ptr u:n key:ptr key-u:n :}
   a u READER key key-u JSON:READ-FIND-KEY ASSERT ;


\ A reader at the value of the journal record's usage member `key`.
: USAGE ( ptr u8 n -- JSON:reader )
   {: key:ptr key-u:n :}
   RECORD$ s" usage" MEMBER key key-u JSON:READ-FIND-KEY ASSERT ;


: STR-IS ( JSON:reader ptr u8 n -- ) JSON:READ-STR-EQ? ASSERT JSON:READ-CLOSE ;
: INT-IS ( JSON:reader n -- ) {: want:n :} JSON:READ-INT want = ASSERT JSON:READ-CLOSE ;
: NULL-IS ( JSON:reader -- ) JSON:READ-TOKEN JSON:T-NULL = ASSERT JSON:READ-CLOSE ;
: NO-USAGE ( -- ) RECORD$ READER s" usage" JSON:READ-FIND-KEY 0= ASSERT JSON:READ-CLOSE ;


\ A recorder that numbers the rows it records from 1, keeps the last record
\ and the last rejected id; with ROW-FAIL set, recording throws E-RECORDER.
-9960 constant E-RECORDER        \ this file's own fixture refusal, outside every lib block
create ROW-TEXT 65536 allot
variable ROW-U
variable ROWS-MADE
variable REJECTED-ROW
variable ROW-FAIL


: TEST-RECORD ( n ptr u8 n -- n )
   {: ctx:n record:ptr record-u:n :}
   ROW-FAIL @ if E-RECORDER throw then
   record-u 65536 <= ASSERT
   record record-u ROW-TEXT 65536 SPAN:MAKE SPAN:COPY
   record-u ROW-U !
   1 ROWS-MADE +!
   ROWS-MADE @ ;


: TEST-REJECT ( n n -- ) nip REJECTED-ROW ! ;
: ROW$ ( -- ptr u8 n ) ROW-TEXT ROW-U @ ;
\ A reader at the value of the last recorded row's member `key`.
: ROW ( ptr u8 n -- JSON:reader ) {: key:ptr key-u:n :} ROW$ key key-u MEMBER ;
: BLANK-PURPOSE ( n -- n ) s"  " M MODEL:PURPOSE! ;
: PURPOSE ( -- ) s" assemble" M MODEL:PURPOSE! ;
\ The first generation row's prefix hash, 64 hex digits.
create HASH 64 allot


\ Every request the backend receives is recorded once, served or failed,
\ under the purpose set for it; a call refused before it is sent is not. Its
\ journal record names the call's kind, configured model and the reply's usage
\ counts, null for each the reply omits, and no usage when the reply has none.
\ Generation rows share a prefix hash across user text; another system text
\ changes it. A recording fault fails a successful call after its reply is
\ journaled, so the same call then replays with no provider call, and it stays
\ behind the fault of a failed one.
: RECORDED-CALLS ( -- )
   CAPTURE
   0 [: TEST-RECORD ;] [: TEST-REJECT ;] M MODEL:RECORDER!
   0 ROWS-MADE ! 0 REJECTED-ROW ! false ROW-FAIL !
   0 [: BLANK-PURPOSE ;] CODE-OF MODEL:E-CONFIG = ASSERT
   MODEL-PEER:CALLS@ {: calls:n :}
   [: s" valid" MODEL:E-STATE FAILS ;] QUIET
   MODEL-PEER:CALLS@ calls = ASSERT
   ROWS-MADE @ 0= ASSERT
   SERVICE$ s" missing" M MODEL:API-KEY!
   PURPOSE s" valid" MODEL:E-KEY FAILS
   MODEL-PEER:CALLS@ calls = ASSERT
   ROWS-MADE @ 0= ASSERT
   FIXTURE-KEY
   PURPOSE
   s" usage" PREP M MODEL:CHAT TRUE-CONTENT$ STR= ASSERT
   ROWS-MADE @ 1 = ASSERT
   M MODEL:CALL-ID 1 = ASSERT
   s" kind" ROW s" generation" STR-IS
   s" backend" ROW s" openai" STR-IS
   s" model" ROW s" test-generation" STR-IS
   s" purpose" ROW s" assemble" STR-IS
   s" outcome" ROW s" ok" STR-IS
   s" fault" ROW NULL-IS
   s" http_status" ROW 200 INT-IS
   s" incomplete_reason" ROW NULL-IS
   s" input_tokens" ROW 11 INT-IS
   s" cached_tokens" ROW 4 INT-IS
   s" output_tokens" ROW 6 INT-IS
   s" reasoning_tokens" ROW 2 INT-IS
   s" prefix_hash" ROW HASH 64 JSON:READ-STR 64 = ASSERT JSON:READ-CLOSE
   RECORD$ s" call_id" MEMBER 1 INT-IS
   s" kind" USAGE s" generation" STR-IS
   s" model" USAGE s" test-generation" STR-IS
   s" input_tokens" USAGE 11 INT-IS
   s" cached_tokens" USAGE 4 INT-IS
   s" output_tokens" USAGE 6 INT-IS
   s" reasoning_tokens" USAGE 2 INT-IS
   s" usage" MODEL:E-STATE FAILS
   s" read-pdf" M MODEL:PURPOSE!
   s" http-error" MODEL:E-UNAVAILABLE FAILS
   M MODEL:HTTP-STATUS 429 = ASSERT
   s" error" s" MODEL:E-UNAVAILABLE status 429: rate limit" CHAT-LOGGED
   ROWS-MADE @ 2 = ASSERT
   s" purpose" ROW s" read-pdf" STR-IS
   s" outcome" ROW s" failed" STR-IS
   s" fault" ROW MODEL:E-UNAVAILABLE INT-IS
   s" http_status" ROW 429 INT-IS
   s" incomplete_reason" ROW NULL-IS
   s" prefix_hash" ROW HASH 64 STR-IS
   M MODEL:RECORD-FAULT 0= ASSERT
   \ A call that got no reply records no HTTP status, not zero.
   PURPOSE REFUSED
   ROWS-MADE @ 3 = ASSERT
   s" fault" ROW MODEL:E-TRANSPORT INT-IS
   s" http_status" ROW NULL-IS
   s" embed" M MODEL:PURPOSE!
   s" one" INPUTS s" two" M MODEL:INPUT+ M MODEL:EMBED
   ROWS-MADE @ 4 = ASSERT
   s" kind" ROW s" embedding" STR-IS
   s" purpose" ROW s" embed" STR-IS
   s" incomplete_reason" ROW NULL-IS
   s" kind" USAGE s" embedding" STR-IS
   s" model" USAGE s" test-embedding" STR-IS
   s" input_tokens" USAGE 2 INT-IS
   s" cached_tokens" USAGE NULL-IS
   s" output_tokens" USAGE NULL-IS
   s" reasoning_tokens" USAGE NULL-IS
   s" Read other evidence." M MODEL:SYSTEM!
   PURPOSE CHAT
   s" prefix_hash" ROW HASH 64 JSON:READ-STR-EQ? 0= ASSERT JSON:READ-CLOSE
   s" Read source evidence." M MODEL:SYSTEM!
   NO-USAGE
   M MODEL:REJECTED
   REJECTED-ROW @ 5 = ASSERT
   true ROW-FAIL !
   false JOURNAL-READY !
   0 [: JOURNAL-GET ;] [: JOURNAL-PUT ;] M MODEL:JOURNAL!
   PURPOSE s" valid" E-RECORDER FAILS
   M MODEL:RECORD-FAULT E-RECORDER = ASSERT
   MODEL-PEER:CALLS@ {: sent:n :}
   PURPOSE CHAT
   MODEL-PEER:CALLS@ sent = ASSERT
   CAPTURE
   PURPOSE s" http-error" MODEL:E-UNAVAILABLE FAILS
   M MODEL:RECORD-FAULT E-RECORDER = ASSERT
   false ROW-FAIL !
   PURPOSE CHAT
   M MODEL:RECORD-FAULT 0= ASSERT
   PURPOSE s" length" MODEL:E-INCOMPLETE FAILS
   s" incomplete_reason" ROW s" max_output_tokens" STR-IS ;


\ A reader at the value of the last request's member `key`.
: SENT ( ptr u8 n -- JSON:reader ) {: key:ptr key-u:n :} MODEL-PEER:SENT$ key key-u MEMBER ;
\ A reader at the value of the journal input's configuration member `key`.
: CONFIG ( ptr u8 n -- JSON:reader )
   {: key:ptr key-u:n :}
   INPUT$ s" configuration" MEMBER key key-u JSON:READ-FIND-KEY ASSERT ;
\ A reader at the last chat request's reasoning effort.
: SENT-EFFORT ( -- JSON:reader ) s" reasoning" SENT s" effort" JSON:READ-FIND-KEY ASSERT ;
\ Neither the last chat request nor its journal input names an effort.
: NO-EFFORT ( -- )
   MODEL-PEER:SENT$ READER s" reasoning" JSON:READ-FIND-KEY 0= ASSERT JSON:READ-CLOSE
   INPUT$ s" configuration" MEMBER s" effort" JSON:READ-FIND-KEY 0= ASSERT JSON:READ-CLOSE ;
\ A chat request is not stored, carries the system text as its instructions
\ and asks for an answer in the configured JSON schema.
: CHAT-REQUEST ( -- )
   CHAT
   s" store" SENT JSON:READ-TOKEN JSON:T-FALSE = ASSERT JSON:READ-CLOSE
   s" background" SENT JSON:READ-TOKEN JSON:T-TRUE = ASSERT JSON:READ-CLOSE
   s" stream" SENT JSON:READ-TOKEN JSON:T-TRUE = ASSERT JSON:READ-CLOSE
   s" instructions" SENT s" Read source evidence." STR-IS
   s" text" SENT s" format" JSON:READ-FIND-KEY ASSERT
   s" type" JSON:READ-FIND-KEY ASSERT s" json_schema" STR-IS ;
create ROW-NAME 1 allot
\ A one-letter purpose row from "a"; its model and effort do not matter.
: NTH-ROW ( n -- )
   [char] a + ROW-NAME c!
   ROW-NAME 1 s" row-model" s" " s" " M MODEL:PURPOSE-MODEL! ;
: SAME-ROW ( n -- n ) s" extract" s" again" s" " s" " M MODEL:PURPOSE-MODEL! ;
: EMPTY-MODEL ( n -- n ) s" y" s" " s" " s" " M MODEL:PURPOSE-MODEL! ;


\ An effort reaches the request, the journal input and the record only when
\ set. A purpose's row sets the model and effort of that purpose's calls
\ alone, however many rows there are; a repeated purpose is refused.
: EFFORTS ( -- )
   CAPTURE
   0 [: TEST-RECORD ;] [: TEST-REJECT ;] M MODEL:RECORDER!
   s" high" M MODEL:EFFORT!
   PURPOSE CHAT
   SENT-EFFORT s" high" STR-IS
   s" effort" CONFIG s" high" STR-IS
   s" effort" ROW s" high" STR-IS
   s" " M MODEL:EFFORT!
   PURPOSE CHAT
   NO-EFFORT
   s" effort" ROW NULL-IS
   s" extract" s" other-model" s" low" s" " M MODEL:PURPOSE-MODEL!
   s" extract" M MODEL:PURPOSE! CHAT
   s" model" SENT s" other-model" STR-IS
   SENT-EFFORT s" low" STR-IS
   s" model" CONFIG s" other-model" STR-IS
   s" effort" CONFIG s" low" STR-IS
   s" model" ROW s" other-model" STR-IS
   s" effort" ROW s" low" STR-IS
   PURPOSE CHAT
   s" model" SENT s" test-generation" STR-IS
   NO-EFFORT
   s" model" ROW s" test-generation" STR-IS
   0 [: SAME-ROW ;] CODE-OF MODEL:E-CONFIG = ASSERT
   0 [: EMPTY-MODEL ;] CODE-OF MODEL:E-CONFIG = ASSERT
   16 0 ?do i NTH-ROW loop
   s" z" s" last-model" s" " s" " M MODEL:PURPOSE-MODEL!
   s" z" M MODEL:PURPOSE! CHAT
   s" model" SENT s" last-model" STR-IS
   s" a" M MODEL:PURPOSE! CHAT
   s" model" SENT s" row-model" STR-IS ;


\ The last request names no prompt cache key.
: NO-CACHE-KEY ( -- )
   MODEL-PEER:SENT$ READER s" prompt_cache_key" JSON:READ-FIND-KEY 0= ASSERT JSON:READ-CLOSE ;
32768 constant KEYLESS-CAP
create KEYLESS KEYLESS-CAP allot
variable KEYLESS-U


\ A cache key reaches every chat request while set and no embedding request;
\ the journal input is the same with or without it.
: CACHE-KEYS ( -- )
   CAPTURE
   PURPOSE CHAT
   NO-CACHE-KEY
   INPUT$ {: a:ptr u:n :}
   u KEYLESS-CAP <= ASSERT
   a u KEYLESS KEYLESS-CAP SPAN:MAKE SPAN:COPY u KEYLESS-U !
   s" test:extract:7" M MODEL:CACHE-KEY!
   PURPOSE CHAT
   s" prompt_cache_key" SENT s" test:extract:7" STR-IS
   INPUT$ KEYLESS KEYLESS-U @ STR= ASSERT
   PURPOSE CHAT
   s" prompt_cache_key" SENT s" test:extract:7" STR-IS
   s" embed" M MODEL:PURPOSE!
   s" one" INPUTS s" two" M MODEL:INPUT+ M MODEL:EMBED
   NO-CACHE-KEY
   s" " M MODEL:CACHE-KEY!
   PURPOSE CHAT
   NO-CACHE-KEY ;


\ The last chat request names no service tier.
: NO-TIER ( -- )
   MODEL-PEER:SENT$ READER s" service_tier" JSON:READ-FIND-KEY 0= ASSERT JSON:READ-CLOSE ;


: NEGATIVE-TIER ( n -- n ) s" y" s" m" s" " s" " drop -1 M MODEL:PURPOSE-MODEL! ;


\ A service tier reaches a generation request only when set. The record
\ names the tier the reply names, NULL when it names none or is null, and the
\ tier a generation request sent, NULL when it sent none; an embedding sends
\ none whatever tier generation requests, and names none after a tiered reply. An
\ empty or non-string reply tier is refused. A purpose's row sets the tier of
\ that purpose's calls in place of the call's, an empty one sending none; a
\ negative tier length is refused.
: TIERS ( -- )
   CAPTURE
   0 [: TEST-RECORD ;] [: TEST-REJECT ;] M MODEL:RECORDER!
   PURPOSE CHAT
   NO-TIER
   s" service_tier" ROW NULL-IS
   s" requested_tier" ROW NULL-IS
   s" flex" M MODEL:SERVICE-TIER!
   PURPOSE CHAT
   s" service_tier" SENT s" flex" STR-IS
   s" service_tier" ROW NULL-IS
   s" requested_tier" ROW s" flex" STR-IS
   PURPOSE s" http-error" MODEL:E-UNAVAILABLE FAILS
   s" service_tier" ROW NULL-IS
   s" requested_tier" ROW s" flex" STR-IS
   s" auto" M MODEL:SERVICE-TIER!
   PURPOSE s" tiered" PREP M MODEL:CHAT TRUE-CONTENT$ STR= ASSERT
   s" service_tier" SENT s" auto" STR-IS
   s" service_tier" ROW s" flex" STR-IS
   s" requested_tier" ROW s" auto" STR-IS
   s" embed" M MODEL:PURPOSE!
   s" one" INPUTS s" two" M MODEL:INPUT+ M MODEL:EMBED
   s" kind" ROW s" embedding" STR-IS
   s" service_tier" ROW NULL-IS
   s" requested_tier" ROW NULL-IS
   PURPOSE s" tier-null" PREP M MODEL:CHAT TRUE-CONTENT$ STR= ASSERT
   s" service_tier" ROW NULL-IS
   s" requested_tier" ROW s" auto" STR-IS
   PURPOSE s" tier-number" MODEL:E-RESPONSE FAILS
   PURPOSE s" tier-empty" MODEL:E-RESPONSE FAILS
   s" " M MODEL:SERVICE-TIER!
   PURPOSE CHAT
   NO-TIER
   s" service_tier" ROW NULL-IS
   s" requested_tier" ROW NULL-IS
   s" flex" M MODEL:SERVICE-TIER!
   s" reason" s" test-generation" s" " s" priority" M MODEL:PURPOSE-MODEL!
   s" translate" s" test-generation" s" " s" " M MODEL:PURPOSE-MODEL!
   s" reason" M MODEL:PURPOSE! CHAT
   s" service_tier" SENT s" priority" STR-IS
   s" requested_tier" ROW s" priority" STR-IS
   s" translate" M MODEL:PURPOSE! CHAT
   NO-TIER
   s" requested_tier" ROW NULL-IS
   PURPOSE CHAT
   s" service_tier" SENT s" flex" STR-IS
   s" requested_tier" ROW s" flex" STR-IS
   0 [: NEGATIVE-TIER ;] CODE-OF MODEL:E-CONFIG = ASSERT ;


: LONG-CHAT ( -- )
   PURPOSE JOURNAL-TEXT JOURNAL-U PREP
   M MODEL:CHAT TRUE-CONTENT$ STR= ASSERT ;


\ A journal hit, here of a journal input past 16 KiB, returns the stored
\ content; it sends nothing and records nothing, and restores the call id its
\ live call recorded, so a refusal of the replayed reply marks that row. A
\ service tier set after the live call is not journal input, so the call
\ still replays.
: REPLAYED-CALLS ( -- )
   s" valid" JOURNAL-TEXT 5 SPAN:MAKE SPAN:COPY
   JOURNAL-TEXT 5 + JOURNAL-U 5 - XS
   false JOURNAL-READY !
   0 [: JOURNAL-GET ;] [: JOURNAL-PUT ;] M MODEL:JOURNAL!
   LONG-CHAT
   M MODEL:CALL-ID {: id:n :}
   ROWS-MADE @ {: rows:n :}
   MODEL-PEER:CALLS@ {: calls:n :}
   s" flex" M MODEL:SERVICE-TIER!
   LONG-CHAT
   MODEL-PEER:CALLS@ calls = ASSERT
   ROWS-MADE @ rows = ASSERT
   M MODEL:CALL-ID id = ASSERT
   0 REJECTED-ROW !
   M MODEL:REJECTED
   REJECTED-ROW @ id = ASSERT
   \ A refusal its live call journaled replays with no request and no line.
   false JOURNAL-READY !
   PURPOSE s" too-large" MODEL:E-CAPACITY FAILS
   MODEL-PEER:CALLS@ {: refused:n :}
   [: PURPOSE s" too-large" MODEL:E-CAPACITY FAILS ;] QUIET
   MODEL-PEER:CALLS@ refused = ASSERT ;


\ Both producer configurations name this base.
: BASE-IS ( ptr u8 n -- )
   {: want:ptr want-u:n :}
   M MODEL:GENERATION-CONFIG$ s" base_url" MEMBER want want-u STR-IS
   M MODEL:EMBEDDING-CONFIG$ s" base_url" MEMBER want want-u STR-IS ;


: OPENAI-GET ( n ptr u8 n ptr n -- bool )
   {: ctx:n input:ptr input-u:n out:ptr :}
   input input-u s" configuration" MEMBER
   s" endpoint" JSON:READ-FIND-KEY ASSERT
   s" https://api.openai.com/v1/responses" STR-IS
   JOURNAL-RECORD JOURNAL-RECORD-U @ BUF:N>BLEN out BUF:REPLACE
   true ;


\ Producer configurations name the effective base. An empty base restores
\ OpenAI's: the journal input names its chat route and the hit returns content
\ with no provider call.
: OPENAI-BASE ( -- )
   MODEL-PEER:BASE-URL$ BASE-IS
   s" " M MODEL:BASE-URL!
   s" https://api.openai.com/v1" BASE-IS
   0 [: OPENAI-GET ;] [: JOURNAL-PUT ;] M MODEL:JOURNAL!
   MODEL-PEER:CALLS@ {: calls:n :}
   JOURNAL-TEXT JOURNAL-U PREP
   M MODEL:CHAT TRUE-CONTENT$ STR= ASSERT
   MODEL-PEER:CALLS@ calls = ASSERT ;


: IDENTITY$ ( -- ptr u8 n ) s" 7907646426070047a77226ac3e684fbbe8410524f7b4a74d02837e43f2146bab" ;


: VECTOR-CODE ( -- n ) M [: dup 0 swap MODEL:VECTOR@ 2drop ;] catch {: code:n :} drop code ;


\ A malformed Ollama reply fails the call and leaves no vectors.
: OLLAMA-ERROR ( ptr u8 n -- )
   INPUTS
   EMBED-CODE MODEL:E-RESPONSE = ASSERT
   VECTOR-CODE MODEL:E-STATE = ASSERT ;


: OLLAMA-DIMS ( n -- n )
   {: d:n :}
   MODEL-PEER:OLLAMA-URL$ s" bge-m3:567m" IDENTITY$ d M MODEL:OLLAMA! d ;


: OLLAMA-CASES ( -- )
   3 OLLAMA-DIMS drop
   M MODEL:EMBEDDING-CONFIG$ s" identity" MEMBER IDENTITY$ STR-IS
   s" one" INPUTS
   s" two" M MODEL:INPUT+
   M MODEL:EMBED
   0 M MODEL:VECTOR@ s" [0.1,0.2,0.3]" STR= ASSERT
   1 M MODEL:VECTOR@ s" [0,1,2]" STR= ASSERT
   s" bad-count" OLLAMA-ERROR
   s" bad-dims" OLLAMA-ERROR
   s" bad-finite" OLLAMA-ERROR
   s" too-large" INPUTS
   EMBED-CODE MODEL:E-CAPACITY = ASSERT
   M MODEL:HTTP-STATUS 400 = ASSERT
   0 [: OLLAMA-DIMS ;] CODE-OF MODEL:E-CONFIG = ASSERT ;


: ST ( -- ptr MODEL:settings ) 0 SETS ;
: SVC$ ( -- ptr u8 n ) SETTINGS-SERVICE$ ;
: FILE ( ptr u8 n ptr u8 n -- ) {: a:ptr u:n v:ptr vu:n :} SVC$ a u v vu SECRET:WRITE ;
: UNFILE ( ptr u8 n -- ) {: a:ptr u:n :} SVC$ a u SECRET:ERASE drop ;
: ST-OPEN ( n -- n ) SVC$ ST MODEL:SETTINGS-OPEN ;
: ST-APPLY ( n -- n ) ST M MODEL:SETTINGS-APPLY ;
: ST-SERVICE ( n -- n ) ST MODEL:SETTINGS-SERVICE$ 2drop ;
: ST-MODEL ( n -- n ) ST MODEL:SETTINGS-MODEL$ 2drop ;
: ST-BASE ( n -- n ) ST MODEL:SETTINGS-BASE$ 2drop ;
: GEN-IS ( ptr u8 n ptr u8 n -- )
   {: k:ptr ku:n want:ptr want-u:n :}
   M MODEL:GENERATION-CONFIG$ k ku MEMBER want want-u STR-IS ;


\ Opens the settings, checks that they read base, applies them to the call
\ and closes them.
: APPLIED ( ptr u8 n -- )
   SVC$ ST MODEL:SETTINGS-OPEN
   ST MODEL:SETTINGS-BASE$ 2swap STR= ASSERT
   ST M MODEL:SETTINGS-APPLY
   ST MODEL:SETTINGS-CLOSE ;


\ Settings filed under their own service point a freshly opened call at their
\ model, base and key item, which stay in force after the settings close: the
\ call goes through the peer. Open settings read back the filed service,
\ model and base, an absent base-url as empty. A second OPEN, and an APPLY or
\ read after CLOSE, are E-STATE; an absent base-url gives OpenAI's; a missing
\ model or an ftp base is E-CONFIG and leaves the settings closed.
: SETTINGS-CASES ( -- )
   M MODEL:OPEN
   SVC$ ST MODEL:SETTINGS-OPEN
   0 [: ST-OPEN ;] CODE-OF MODEL:E-STATE = ASSERT
   ST MODEL:SETTINGS-SERVICE$ SVC$ STR= ASSERT
   ST MODEL:SETTINGS-MODEL$ s" settings-generation" STR= ASSERT
   ST MODEL:SETTINGS-BASE$ MODEL-PEER:BASE-URL$ STR= ASSERT
   ST M MODEL:SETTINGS-APPLY
   ST MODEL:SETTINGS-CLOSE
   0 [: ST-APPLY ;] CODE-OF MODEL:E-STATE = ASSERT
   0 [: ST-SERVICE ;] CODE-OF MODEL:E-STATE = ASSERT
   0 [: ST-MODEL ;] CODE-OF MODEL:E-STATE = ASSERT
   0 [: ST-BASE ;] CODE-OF MODEL:E-STATE = ASSERT
   s" Read source evidence." M MODEL:SYSTEM!
   SCHEMA$ M MODEL:SCHEMA!
   CHAT
   s" model" s" settings-generation" GEN-IS
   s" base_url" MODEL-PEER:BASE-URL$ GEN-IS
   s" base-url" UNFILE
   s" " APPLIED
   s" base_url" s" https://api.openai.com/v1" GEN-IS
   s" base-url" s" ftp://x" FILE
   0 [: ST-OPEN ;] CODE-OF MODEL:E-CONFIG = ASSERT
   0 [: ST-APPLY ;] CODE-OF MODEL:E-STATE = ASSERT
   s" base-url" UNFILE
   s" model" UNFILE
   0 [: ST-OPEN ;] CODE-OF MODEL:E-CONFIG = ASSERT
   0 [: ST-APPLY ;] CODE-OF MODEL:E-STATE = ASSERT
   M MODEL:CLOSE ;


: SETTINGS ( -- )
   s" settings-generation" MODEL-PEER:BASE-URL$ SETTINGS!
   SETTINGS-CASES ;


: CASES ( -- )
   SETUP
   M MODEL:CANCEL 0= ASSERT
   BASES CHAT-REQUEST IMAGE STORE-KEY NEGATIVE-KEY INVALID-IMAGE INVALID-SCHEMA
   REPLIES EARLY-ENDS UNREADABLE REFUSED [: CANCELLATION ;] QUIET CANCEL-CUT
   EMBEDDINGS BAD-VECTORS EMBED-TOO-LARGE INPUT-BOUNDS DIMS-PROPERTY
   STOP-BETWEEN
   M MODEL:CLOSE
   SETUP
   CHAT EMBEDDINGS
   STOP-ACTIVE
   M MODEL:CLOSE
   SETUP RECORDED-CALLS REPLAYED-CALLS
   M MODEL:CLOSE
   SETUP EFFORTS CACHE-KEYS
   M MODEL:CLOSE
   SETUP TIERS
   M MODEL:CLOSE
   SETUP OPENAI-BASE
   M MODEL:CLOSE
   SETUP OLLAMA-CASES
   M MODEL:CLOSE
   SETTINGS ;


: PEERED ( -- )
   MODEL-PEER:START
   ['] CASES [: MODEL-PEER:STOP ;] finally ;


: CAPTURED ( -- )
   LOG-PATH$ STDERR:CAPTURE
   ['] PEERED [: STDERR:;CAPTURE ;] finally ;


public
: RUN ( -- )
   T-RESET
   STORE-INSTALL
   LOG-INIT
   [: CAPTURED ;] WITH-CURL
   LOG$ KEY$ CONTAINS? 0= ASSERT
   LOG-B BUF:DISPOSE
   s" model-test: " type T-CASES FMT:.INT s"  checks" type cr
   T-REPORT ;
;using
;package

MODEL-TEST:RUN
