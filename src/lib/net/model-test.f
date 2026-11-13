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
require src/lib/float32.f
require src/lib/float32-buffer.f
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
require src/lib/time.f
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
variable READ-FAULT    \ the code each read throws, 0 for none

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
   READ-FAULT @ {: fault:n :}
   fault 0<> if fault throw then
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

\ Each store read throws the code until it is 0 again.
: READ-FAULT! ( n -- ) READ-FAULT ! ;
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
\ The last chat, count, compaction or embedding request body, or -1 after one
\ past SENT-CAP.
65536 constant SENT-CAP
create SENT-BODY SENT-CAP allot
variable SENT-U
\ The chat scenario FAIL-NEXT named for the next request, whatever its text,
\ on any route but Ollama's; none while its length is 0.
32 constant FAILURE-CAP
create FAILURE FAILURE-CAP allot
variable FAILURE-U
TASK:SEMAPHORE HELD
\ The handlers share this package's buffers and parsers, so one runs at a time
\ (ALONE); the held stream gives its turn up while it waits.
TASK:SEMAPHORE TURN
\ LET-GO lets a gated reply go.
TASK:SEMAPHORE GO
\ An event stream being written.
create STREAM-B BUF:HDR-BYTES allot
TYPED-VARIABLE STREAM-W JSON:writer
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
\ How many inputs that request held.
variable RECEIVED-N
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


: TAKE ( TASK:sem -- )
   [: dup TASK:TRY-WAIT-SEM ;] TEST-WAIT:POLL drop ;


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


\ Whether a request body names a service tier.
: TIERED? ( HTTP:request -- bool )
   {: r:HTTP:request :}
   PARSER r HTTP:BODY$ JSON:READ-INIT
   JSON:READ-NEXT JSON:T-OBJ = ASSERT
   s" service_tier" JSON:READ-FIND-KEY {: named:bool :}
   JSON:READ-CLOSE named ;


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
   0 RECEIVED-N !
   s" input" BODY-MEMBER
   JSON:READ-TOKEN JSON:T-ARR = ASSERT
   begin JSON:READ-NEXT dup JSON:T-ARR-END <> while
      JSON:T-STR = ASSERT
      1 RECEIVED-N +!
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


\ The reply of a response still queued or running, or ended with no output.
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


\ OPENING, a delta numbered LAST-SEQ, then a keepalive.
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


\ Signals HELD and keeps the connection open until the client closes it, with
\ the turn given up.
: HOLD-OPEN ( TCP4:connection -- )
   {: conn:TCP4:connection :}
   HELD TASK:SIGNAL-SEM
   TURN TASK:SIGNAL-SEM
   conn [: TCP4:CONNECTION-FD AIO:READABLE -1 >MS AIO:POLL AIO:AWAIT ;]
   [: TURN TASK:WAIT-SEM ;] finally
   MATCH AIO:outcome
      ready OF drop ENDOF
      timed-out OF FAIL ENDOF
      cancelled OF FAIL ENDOF
      refused OF drop FAIL ENDOF
   ;MATCH
   conn RAW-CLOSE ;


\ Signals HELD and waits, with the turn given up, until LET-GO.
: GATE ( -- )
   HELD TASK:SIGNAL-SEM
   TURN TASK:SIGNAL-SEM
   [: GO TAKE ;] [: TURN TASK:WAIT-SEM ;] finally ;


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


\ A reply's usage member, its fixed counts, after a member before it.
: FIXED-USAGE ( ptr JSON:writer -- ptr JSON:writer )
   S\" ,\qusage\q:{\qinput_tokens\q:11,\qinput_tokens_details\q:{\qcached_tokens\q:4,\qcache_write_tokens\q:3},\qoutput_tokens\q:6,\qoutput_tokens_details\q:{\qreasoning_tokens\q:2},\qtotal_tokens\q:17}" JSON:WRITE-RAW ;


: USAGE-RESPONSE ( ptr JSON:writer -- ptr JSON:writer )
   TRUE-HEAD FIXED-USAGE s" }" JSON:WRITE-RAW ;


\ A response whose service_tier member is the JSON value text.
: TIERED-RESPONSE ( ptr JSON:writer ptr u8 n -- ptr JSON:writer )
   {: tier:ptr tier-u:n :}
   TRUE-HEAD
   S\" ,\qservice_tier\q:" JSON:WRITE-RAW
   tier tier-u JSON:WRITE-RAW
   s" }" JSON:WRITE-RAW ;


\ A reply cut at the output limit, its message holding the text streamed
\ before the cut.
: CUT$ ( -- ptr u8 n )
   S\" {\qid\q:\qresp_1\q,\qobject\q:\qresponse\q,\qstatus\q:\qincomplete\q,\qoutput\q:[{\qtype\q:\qreasoning\q,\qid\q:\qrs_1\q,\qsummary\q:[]},{\qtype\q:\qmessage\q,\qid\q:\qmsg_1\q,\qstatus\q:\qincomplete\q,\qrole\q:\qassistant\q,\qcontent\q:[{\qtype\q:\qoutput_text\q,\qtext\q:\q{\\\qans\q,\qannotations\q:[]}]}],\qincomplete_details\q:{\qreason\q:\qmax_output_tokens\q},\qerror\q:null}" ;


\ A completed reply whose message part is a refusal.
: REFUSAL$ ( -- ptr u8 n )
   S\" {\qid\q:\qresp_1\q,\qobject\q:\qresponse\q,\qstatus\q:\qcompleted\q,\qoutput\q:[{\qtype\q:\qreasoning\q,\qid\q:\qrs_1\q,\qsummary\q:[]},{\qtype\q:\qmessage\q,\qid\q:\qmsg_1\q,\qrole\q:\qassistant\q,\qcontent\q:[{\qtype\q:\qrefusal\q,\qrefusal\q:\qcannot comply\q}]}],\qincomplete_details\q:null,\qerror\q:null}" ;


\ A completed reply whose content is not JSON.
: BAD-CONTENT$ ( -- ptr u8 n )
   S\" {\qid\q:\qresp_1\q,\qobject\q:\qresponse\q,\qstatus\q:\qcompleted\q,\qoutput\q:[{\qtype\q:\qreasoning\q,\qid\q:\qrs_1\q,\qsummary\q:[]},{\qtype\q:\qmessage\q,\qid\q:\qmsg_1\q,\qrole\q:\qassistant\q,\qcontent\q:[{\qtype\q:\qoutput_text\q,\qtext\q:\qnot JSON\q,\qannotations\q:[]}]}],\qincomplete_details\q:null,\qerror\q:null}" ;


: REPLY-TAIL ( ptr JSON:writer -- ptr JSON:writer )
   S\" ],\qincomplete_details\q:null,\qerror\q:null}" JSON:WRITE-RAW ;


\ Tool reply output items: reasoning with encrypted content, a function call,
\ a plain message, a refusal message and an item of a type no tools call reads.
: REASONING$ ( -- ptr u8 n )
   S\" {\qtype\q:\qreasoning\q,\qid\q:\qrs_1\q,\qsummary\q:[],\qencrypted_content\q:\qgAAAAB\q}" ;
: FUNCTION-CALL$ ( -- ptr u8 n )
   S\" {\qtype\q:\qfunction_call\q,\qid\q:\qfc_1\q,\qcall_id\q:\qcall_1\q,\qname\q:\qlookup\q,\qarguments\q:\q{\\\qq\\\q:\\\qx\\\q}\q,\qstatus\q:\qcompleted\q}" ;
: TEXT-ITEM$ ( -- ptr u8 n )
   S\" {\qtype\q:\qmessage\q,\qid\q:\qmsg_1\q,\qrole\q:\qassistant\q,\qcontent\q:[{\qtype\q:\qoutput_text\q,\qtext\q:\q{\\\qanswer\\\q:true}\q,\qannotations\q:[]}]}" ;
: REFUSAL-ITEM$ ( -- ptr u8 n )
   S\" {\qtype\q:\qmessage\q,\qid\q:\qmsg_1\q,\qrole\q:\qassistant\q,\qcontent\q:[{\qtype\q:\qrefusal\q,\qrefusal\q:\qcannot comply\q}]}" ;
: OTHER-ITEM$ ( -- ptr u8 n )
   S\" {\qtype\q:\qweb_search_call\q,\qid\q:\qws_1\q,\qstatus\q:\qcompleted\q}" ;


\ A completed reply's members through its first output item, REASONING$.
: TOOL-HEAD ( ptr JSON:writer -- ptr JSON:writer )
   S\" {\qid\q:\qresp_1\q,\qobject\q:\qresponse\q,\qstatus\q:\qcompleted\q,\qoutput\q:[" JSON:WRITE-RAW
   REASONING$ JSON:WRITE-RAW ;
: NEXT-ITEM ( ptr JSON:writer ptr u8 n -- ptr JSON:writer )
   {: a:ptr u:n :}
   s" ," JSON:WRITE-RAW a u JSON:WRITE-RAW ;


\ A message whose output_text holds the content and whose phase member, after
\ its content as the provider sends it, is the JSON value text.
: PHASED ( ptr JSON:writer ptr u8 n ptr u8 n -- ptr JSON:writer )
   {: content:ptr content-u:n phase:ptr phase-u:n :}
   S\" ,{\qid\q:\qmsg_1\q,\qtype\q:\qmessage\q,\qstatus\q:\qcompleted\q,\qcontent\q:[{\qtype\q:\qoutput_text\q,\qannotations\q:[]," JSON:WRITE-RAW
   s" text" content content-u JSON:WRITE-FIELD-S
   S\" }],\qrole\q:\qassistant\q,\qphase\q:" JSON:WRITE-RAW
   phase phase-u JSON:WRITE-RAW
   s" }" JSON:WRITE-RAW ;


\ A progress note, a JSON object so that only its phase keeps it from being the
\ content, and the answer {"answer":true}.
: NOTE-MESSAGE ( ptr JSON:writer -- ptr JSON:writer )
   S\" {\qnote\q:\qI'll check the knowledge base.\q}" S\" \qcommentary\q" PHASED ;
: ANSWER-MESSAGE ( ptr JSON:writer -- ptr JSON:writer )
   S\" {\qanswer\q:true}" S\" \qfinal_answer\q" PHASED ;


\ A completed reply whose one message holds {"answer":true} under the phase,
\ a JSON value text.
: ONE-PHASE ( ptr JSON:writer ptr u8 n -- ptr JSON:writer )
   {: phase:ptr phase-u:n :}
   s" completed" REPLY-HEAD
   S\" {\qanswer\q:true}" phase phase-u PHASED
   REPLY-TAIL ;


: COMPLETED$ ( -- ptr u8 n ) s" response.completed" ;
: INCOMPLETE$ ( -- ptr u8 n ) s" response.incomplete" ;
: FAILED$ ( -- ptr u8 n ) s" response.failed" ;


\ Answers a tool scenario by its name, or answers false.
: TOOL-REPLY? ( HTTP:response ptr u8 n -- bool )
   {: response:HTTP:response t:ptr t-u:n :}
   t t-u s" tool-call" STR= if
      response [: TOOL-HEAD FUNCTION-CALL$ NEXT-ITEM REPLY-TAIL ;] COMPLETED$ STREAMED
      true exit
   then
   t t-u s" tool-message" STR= if
      response [: TOOL-HEAD TEXT-ITEM$ NEXT-ITEM FUNCTION-CALL$ NEXT-ITEM REPLY-TAIL ;]
      COMPLETED$ STREAMED true exit
   then
   t t-u s" tool-none" STR= if
      response [: TOOL-HEAD REPLY-TAIL ;] COMPLETED$ STREAMED true exit
   then
   t t-u s" tool-two" STR= if
      response [: TOOL-HEAD FUNCTION-CALL$ NEXT-ITEM FUNCTION-CALL$ NEXT-ITEM REPLY-TAIL ;]
      COMPLETED$ STREAMED true exit
   then
   t t-u s" tool-refusal" STR= if
      response [: TOOL-HEAD REFUSAL-ITEM$ NEXT-ITEM FUNCTION-CALL$ NEXT-ITEM REPLY-TAIL ;]
      COMPLETED$ STREAMED true exit
   then
   t t-u s" tool-other" STR= if
      response [: TOOL-HEAD OTHER-ITEM$ NEXT-ITEM FUNCTION-CALL$ NEXT-ITEM REPLY-TAIL ;]
      COMPLETED$ STREAMED true exit
   then
   false ;


\ A 200 reply that is JSON, not an event stream.
: PLAIN$ ( -- ptr u8 n )
   S\" {\qid\q:\qresp_1\q,\qstatus\q:\qcompleted\q,\qoutput\q:[]}" ;
\ Event data the call cannot read: a type that is not a string, and a response
\ with no id.
: BAD-TYPE$ ( -- ptr u8 n ) S\" {\qtype\q:5,\qsequence_number\q:2}" ;
: NO-ID$ ( -- ptr u8 n )
   S\" {\qtype\q:\qresponse.created\q,\qsequence_number\q:0,\qresponse\q:{\qstatus\q:\qqueued\q,\qoutput\q:[]}}" ;
\ An error event's data with its code and message at its top, and under error.
: FLAT-ERROR$ ( -- ptr u8 n )
   S\" {\qtype\q:\qerror\q,\qsequence_number\q:2,\qcode\q:\qserver_error\q,\qmessage\q:\qThe server had an error.\q,\qparam\q:null}" ;
: NESTED-ERROR$ ( -- ptr u8 n )
   S\" {\qtype\q:\qerror\q,\qsequence_number\q:2,\qerror\q:{\qtype\q:\qserver_error\q,\qcode\q:\qserver_error\q,\qmessage\q:\qThe server had an error.\q,\qparam\q:null}}" ;
\ OpenAI's error event when the flex tier has no room for the request.
: FLEX-OUT$ ( -- ptr u8 n )
   S\" {\qtype\q:\qerror\q,\qerror\q:{\qtype\q:\qresource_unavailable\q,\qcode\q:\qflex_unavailable\q,\qheaders\q:{\qretry-after\q:\q300\q,\qretry-after-ms\q:\q300000\q,\qx-retry-metadata\q:\qNO_MORE_RETRY\q},\qmessage\q:\qFlex processing is temporarily unavailable. Please try again later or use standard processing.\q,\qparam\q:null},\qsequence_number\q:2}" ;
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


\ OpenAI's refusal of a chat or count request over the model's context.
: TOO-LARGE ( HTTP:response -- )
   {: response:HTTP:response :}
   response 400 HTTP:STATUS!
   response [: S\" {\qerror\q:{\qmessage\q:\qYour input exceeds the context window of this model.\q,\qtype\q:\qinvalid_request_error\q,\qparam\q:\qinput\q,\qcode\q:\qcontext_length_exceeded\q}}" JSON:WRITE-RAW ;] HTTP:JSON! ;


\ Responds as the scenario the text names, a chat request's first text.
: REPLY ( HTTP:request HTTP:response ptr u8 n -- )
   {: r:HTTP:request response:HTTP:response t:ptr t-u:n :}
   response t t-u TOOL-REPLY? if exit then
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
      response [: CUT$ JSON:WRITE-RAW ;] INCOMPLETE$ STREAMED exit
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
   t t-u s" note-first" STR= if
      response [: s" completed" REPLY-HEAD NOTE-MESSAGE ANSWER-MESSAGE REPLY-TAIL ;]
      COMPLETED$ STREAMED exit
   then
   t t-u s" note-last" STR= if
      response [: s" completed" REPLY-HEAD ANSWER-MESSAGE NOTE-MESSAGE REPLY-TAIL ;]
      COMPLETED$ STREAMED exit
   then
   t t-u s" note-only" STR= if
      response [: s" completed" REPLY-HEAD NOTE-MESSAGE REPLY-TAIL ;] COMPLETED$ STREAMED
      exit
   then
   t t-u s" answer-twice" STR= if
      response [: s" completed" REPLY-HEAD ANSWER-MESSAGE ANSWER-MESSAGE REPLY-TAIL ;]
      COMPLETED$ STREAMED exit
   then
   t t-u s" phase-null" STR= if response [: s" null" ONE-PHASE ;] COMPLETED$ STREAMED exit then
   t t-u s" phase-other" STR= if
      response [: S\" \qanalysis\q" ONE-PHASE ;] COMPLETED$ STREAMED exit
   then
   t t-u s" answer-other" STR= if
      response [: s" completed" REPLY-HEAD ANSWER-MESSAGE
         S\" {\qnote\q:true}" S\" \qanalysis\q" PHASED REPLY-TAIL ;]
      COMPLETED$ STREAMED exit
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
   t t-u s" error-event" STR= if response FLAT-ERROR$ true BAD-STREAM exit then
   t t-u s" error-nested" STR= if response NESTED-ERROR$ true BAD-STREAM exit then
   t t-u s" flex-out" STR= if response FLEX-OUT$ true BAD-STREAM exit then
   \ flex-out to a request that names a service tier, else valid's reply.
   t t-u s" flex-then" STR= if
      r TIERED? if response FLEX-OUT$ true BAD-STREAM exit then
      response [: CHAT-RESPONSE ;] COMPLETED$ STREAMED exit
   then
   \ flex-out to a request that names a service tier, else valid's reply
   \ once the gate lets it go.
   t t-u s" flex-gate" STR= if
      r TIERED? if response FLEX-OUT$ true BAD-STREAM exit then
      GATE
      response [: CHAT-RESPONSE ;] COMPLETED$ STREAMED exit
   then
   \ flex-out to a request that names a service tier, else hold's stream.
   t t-u s" flex-hold" STR= if
      r TIERED? if response FLEX-OUT$ true BAD-STREAM exit then
      r STREAM-OPEN s" resp_hold" OPENING RAW-STREAM HOLD-OPEN exit
   then
   t t-u s" plain-json" STR= if response [: PLAIN$ JSON:WRITE-RAW ;] HTTP:JSON! exit then
   t t-u s" bad-type" STR= if response BAD-TYPE$ true BAD-STREAM exit then
   t t-u s" no-id" STR= if response NO-ID$ false BAD-STREAM exit then
   t t-u s" bad-utf8" STR= if STREAM-OPEN BAD-UTF8$ JSON:WRITE-RAW response swap STREAM! exit then
   \ The terminal event arrives and then the connection drops mid-body.
   t t-u s" done-drop" STR= if
      r STREAM-OPEN s" resp_1" PRELUDE
      COMPLETED$ LAST-SEQ 1+ RESPONSE-OPEN CHAT-RESPONSE EVENT-CLOSE RAW-STREAM RAW-CLOSE exit
   then
   \ A keepalive and then a drop, before an event names the response.
   t t-u s" early-drop" STR= if
      r STREAM-OPEN s" keepalive" 0 EVENT-OPEN EVENT-CLOSE RAW-STREAM RAW-CLOSE exit
   then
   \ A keepalive, resp_1's OPENING, a keepalive and then a drop.
   t t-u s" twice-drop" STR= if
      r STREAM-OPEN s" keepalive" 0 EVENT-OPEN EVENT-CLOSE s" resp_1" OPENING
      s" keepalive" 2 EVENT-OPEN EVENT-CLOSE
      RAW-STREAM RAW-CLOSE exit
   then
   \ resp_1's PRELUDE and then a drop, before the response ends.
   t t-u s" id-drop" STR= if
      r STREAM-OPEN s" resp_1" PRELUDE RAW-STREAM RAW-CLOSE exit
   then
   \ resp_quiet's stream ends after its created event.
   t t-u s" no-progress" STR= if
      STREAM-OPEN s" resp_quiet" CREATED response swap STREAM! exit
   then
   \ The held stream sends its created and in_progress events, then stays
   \ open until the client closes it.
   t t-u s" hold" STR= if
      r STREAM-OPEN s" resp_hold" OPENING RAW-STREAM HOLD-OPEN exit
   then
   t t-u s" http-error" STR= if
      response 429 HTTP:STATUS!
      response [: S\" {\qerror\q:{\qmessage\q:\qrate limit\q}}" JSON:WRITE-RAW ;] HTTP:JSON!
      exit
   then
   t t-u s" http-busy" STR= if
      response 503 HTTP:STATUS!
      response [: S\" {\qerror\q:{\qmessage\q:\qoverloaded\q}}" JSON:WRITE-RAW ;] HTTP:JSON!
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
   t t-u s" too-large" STR= if response TOO-LARGE exit then
   t t-u s" oversized" STR= if response 413 HTTP:STATUS! exit then
   t t-u s" big" STR= if response [: BIG$ COMPLETED ;] COMPLETED$ STREAMED exit then
   \ Every other scenario, the journal's long text included, starts "valid".
   t t-u 5 min s" valid" STR= if
      response [: CHAT-RESPONSE ;] COMPLETED$ STREAMED exit
   then
   response 400 HTTP:STATUS! ;


\ Responds to a request, of any route, as the scenario FAIL-NEXT named, once:
\ true when one was named.
: FAILED-NEXT? ( HTTP:request HTTP:response -- bool )
   {: r:HTTP:request response:HTTP:response :}
   FAILURE-U atomic@ {: u:n :}
   u 0= if false exit then
   0 FAILURE-U atomic!
   r response FAILURE u REPLY
   true ;


: KEEP-BODY ( HTTP:request -- )
   HTTP:BODY$ {: a:ptr u:n :}
   u SENT-CAP <= if a u SENT-BODY SENT-CAP SPAN:MAKE SPAN:COPY u else -1 then
   SENT-U ! ;


: CHAT ( HTTP:request HTTP:response -- )
   {: r:HTTP:request response:HTTP:response :}
   1 CALLS atomic-add drop
   r KEEP-BODY
   r AUTHORIZED? 0= if response 401 HTTP:STATUS! exit then
   r response FAILED-NEXT? if exit then
   r response r FIRST-TEXT REPLY ;


variable COUNTED-U    \ the byte length of the last count request's body


\ OpenAI's count reply; this peer's count is COUNTED-U, so two inputs count
\ apart and a test computes the count from SENT$.
: COUNTED ( ptr JSON:writer -- ptr JSON:writer )
   S\" {\qobject\q:\qresponse.input_tokens\q,\qinput_tokens\q:" JSON:WRITE-RAW
   COUNTED-U @ JSON:WRITE-U s" }" JSON:WRITE-RAW ;


\ Counts a request's input by its first text: too-large past the model's
\ context, any other its body's byte length.
: INPUT-TOKENS ( HTTP:request HTTP:response -- )
   {: r:HTTP:request response:HTTP:response :}
   1 CALLS atomic-add drop
   r KEEP-BODY
   r AUTHORIZED? 0= if response 401 HTTP:STATUS! exit then
   r response FAILED-NEXT? if exit then
   r FIRST-TEXT s" too-large" STR= if response TOO-LARGE exit then
   r HTTP:BODY$ nip COUNTED-U !
   response [: COUNTED ;] HTTP:JSON! ;


variable COMPACTED-U    \ the byte length of the last compaction request's body
create KEPT 64 allot    \ the last compaction request's first text
variable KEPT-U
: KEPT$ ( -- ptr u8 n ) KEPT KEPT-U @ ;


\ A compaction reply's members through its output's opener.
: WINDOW-OPEN ( ptr JSON:writer -- ptr JSON:writer )
   S\" {\qid\q:\qcmp_1\q,\qobject\q:\qresponse.compaction\q,\qcreated_at\q:0,\qoutput\q:[" JSON:WRITE-RAW ;


\ A compaction reply's members through its output's first item, the kept user
\ message of KEPT$, as a provider keeps the user messages it compacts.
: WINDOW-HEAD ( ptr JSON:writer -- ptr JSON:writer )
   WINDOW-OPEN
   S\" {\qtype\q:\qmessage\q,\qrole\q:\quser\q,\qcontent\q:[{\qtype\q:\qinput_text\q,\qtext\q:\q" JSON:WRITE-RAW
   KEPT$ JSON:WRITE-RAW S\" \q}]}" JSON:WRITE-RAW ;


\ A compaction item after an item, whose content names COMPACTED-U, so two
\ inputs compact apart.
: COMPACTION-ITEM ( ptr JSON:writer -- ptr JSON:writer )
   S\" ,{\qtype\q:\qcompaction\q,\qid\q:\qcmp_item\q,\qencrypted_content\q:\qlen-" JSON:WRITE-RAW
   COMPACTED-U @ JSON:WRITE-U S\" \q}" JSON:WRITE-RAW ;


\ OpenAI's compaction reply: the kept message, then a compaction item.
: WINDOW ( ptr JSON:writer -- ptr JSON:writer )
   WINDOW-HEAD COMPACTION-ITEM s" ]" JSON:WRITE-RAW
   FIXED-USAGE s" }" JSON:WRITE-RAW ;


\ A compaction reply whose output holds no compaction item, naming flex as the
\ tier that served it.
: NO-WINDOW ( ptr JSON:writer -- ptr JSON:writer )
   WINDOW-HEAD s" ]" JSON:WRITE-RAW
   FIXED-USAGE S\" ,\qservice_tier\q:\qflex\q}" JSON:WRITE-RAW ;


\ A compaction reply whose output is empty.
: EMPTY-WINDOW ( ptr JSON:writer -- ptr JSON:writer )
   WINDOW-OPEN s" ]" JSON:WRITE-RAW FIXED-USAGE s" }" JSON:WRITE-RAW ;


\ A compaction reply whose output holds a number after the kept message.
: NUMBER-WINDOW ( ptr JSON:writer -- ptr JSON:writer )
   WINDOW-HEAD s" ,7]" JSON:WRITE-RAW FIXED-USAGE s" }" JSON:WRITE-RAW ;


\ A compaction reply whose compaction item has a message after it.
: LATE-WINDOW ( ptr JSON:writer -- ptr JSON:writer )
   WINDOW-HEAD COMPACTION-ITEM
   S\" ,{\qtype\q:\qmessage\q,\qrole\q:\qassistant\q,\qcontent\q:[{\qtype\q:\qoutput_text\q,\qtext\q:\qlate\q}]}]" JSON:WRITE-RAW
   FIXED-USAGE s" }" JSON:WRITE-RAW ;


\ Compacts a request's input by its first text: too-large past the model's
\ context, bad-compact with no compaction item, empty-window, number-window and
\ late-window into their writers, any other into WINDOW.
: COMPACTS ( HTTP:request HTTP:response -- )
   {: r:HTTP:request response:HTTP:response :}
   1 CALLS atomic-add drop
   r KEEP-BODY
   r AUTHORIZED? 0= if response 401 HTTP:STATUS! exit then
   r response FAILED-NEXT? if exit then
   r FIRST-TEXT {: t:ptr t-u:n :}
   t t-u s" too-large" STR= if response TOO-LARGE exit then
   t-u 64 > if FAIL then
   t t-u KEPT 64 SPAN:MAKE SPAN:COPY t-u KEPT-U !
   r HTTP:BODY$ nip COMPACTED-U !
   t t-u s" bad-compact" STR= if response [: NO-WINDOW ;] HTTP:JSON! exit then
   t t-u s" empty-window" STR= if response [: EMPTY-WINDOW ;] HTTP:JSON! exit then
   t t-u s" number-window" STR= if response [: NUMBER-WINDOW ;] HTTP:JSON! exit then
   t t-u s" late-window" STR= if response [: LATE-WINDOW ;] HTTP:JSON! exit then
   response [: WINDOW ;] HTTP:JSON! ;


\ One vector whose first component is the input's byte length.
: LENGTH-VECTOR ( ptr JSON:writer -- ptr JSON:writer )
   S\" {\qobject\q:\qlist\q,\qdata\q:[{\qindex\q:0,\qembedding\q:[" JSON:WRITE-RAW
   RECEIVED-U @ JSON:WRITE-U
   s" ,0,0]}]}" JSON:WRITE-RAW ;


\ One vector per input, input k's first component k.
: INDEX-VECTORS ( ptr JSON:writer -- ptr JSON:writer )
   S\" {\qobject\q:\qlist\q,\qdata\q:[" JSON:WRITE-RAW
   RECEIVED-N @ 0 ?do
      i 0<> if s" ," JSON:WRITE-RAW then
      S\" {\qindex\q:" JSON:WRITE-RAW i JSON:WRITE-U
      S\" ,\qembedding\q:[" JSON:WRITE-RAW i JSON:WRITE-U
      s" ,0,0]}" JSON:WRITE-RAW
   loop
   s" ]}" JSON:WRITE-RAW ;


\ A vector's dimension count past any one provider's; the embedding past it
\ counts up from 0.
4096 constant WIDE-DIMS
: WIDE-VECTOR ( ptr JSON:writer -- ptr JSON:writer )
   S\" {\qobject\q:\qlist\q,\qdata\q:[{\qindex\q:0,\qembedding\q:[0" JSON:WRITE-RAW
   WIDE-DIMS 1 ?do s" ," JSON:WRITE-RAW i JSON:WRITE-U loop
   s" ]}]}" JSON:WRITE-RAW ;


\ Two base64 vectors out of index order, 0.4 0.5 0.6 then 0.1 0.2 0.3.
: BASE64-VECTORS ( ptr JSON:writer -- ptr JSON:writer )
   S\" {\qobject\q:\qlist\q,\qdata\q:[" JSON:WRITE-RAW
   S\" {\qembedding\q:\qzczMPgAAAD+amRk/\q,\qindex\q:1}," JSON:WRITE-RAW
   S\" {\qindex\q:0,\qembedding\q:\qzczMPc3MTD6amZk+\q}]," JSON:WRITE-RAW
   S\" \qusage\q:{\qprompt_tokens\q:2,\qtotal_tokens\q:2}}" JSON:WRITE-RAW ;


\ A reply of one vector at index 0 whose embedding is the JSON text v.
: ONE-VECTOR ( ptr JSON:writer ptr u8 n -- ptr JSON:writer )
   {: v:ptr v-u:n :}
   S\" {\qobject\q:\qlist\q,\qdata\q:" JSON:WRITE-RAW
   S\" [{\qindex\q:0,\qembedding\q:" JSON:WRITE-RAW
   v v-u JSON:WRITE-RAW
   s" }]}" JSON:WRITE-RAW ;


\ Answers the base64 vector a reply case names, true, or false for none.
: BASE64-CASE? ( HTTP:response ptr u8 n -- bool )
   {: response:HTTP:response i:ptr i-u:n :}
   \ The first z escaped as \u007a: 0.1 0.2 0.3.
   i i-u s" escaped" STR= if
      response [: S\" \q\\u007aczMPc3MTD6amZk+\q" ONE-VECTOR ;] HTTP:JSON!
      true exit
   then
   \ Two values, 0.1 0.2.
   i i-u s" b64-short" STR= if
      response [: S\" \qzczMPc3MTD4=\q" ONE-VECTOR ;] HTTP:JSON! true exit
   then
   \ Four values, 0.1 0.2 0.3 0.4.
   i i-u s" b64-long" STR= if
      response [: S\" \qzczMPc3MTD6amZk+zczMPg==\q" ONE-VECTOR ;] HTTP:JSON!
      true exit
   then
   \ A character outside the base64 alphabet.
   i i-u s" b64-bad" STR= if
      response [: S\" \qzczMPc3MTD6amZk*\q" ONE-VECTOR ;] HTTP:JSON! true exit
   then
   \ 0.1, a quiet NaN ($7FC00000), 0.3.
   i i-u s" b64-nan" STR= if
      response [: S\" \qzczMPQAAwH+amZk+\q" ONE-VECTOR ;] HTTP:JSON! true exit
   then
   \ 0.1, positive infinity ($7F800000), 0.3.
   i i-u s" b64-inf" STR= if
      response [: S\" \qzczMPQAAgH+amZk+\q" ONE-VECTOR ;] HTTP:JSON! true exit
   then
   false ;


\ Responds by the request's inputs: a bound input is all x. The request asks
\ for base64; the replies that stay arrays stand for a provider ignoring it.
: EMBEDDINGS ( HTTP:request HTTP:response -- )
   {: r:HTTP:request response:HTTP:response :}
   1 CALLS atomic-add drop
   r KEEP-BODY
   r AUTHORIZED? 0= if response 401 HTTP:STATUS! exit then
   r response FAILED-NEXT? if exit then
   r s" encoding_format" BODY-MEMBER
   JSON:READ-TOKEN JSON:T-STR = ASSERT
   s" base64" JSON:READ-STR-EQ? ASSERT JSON:READ-CLOSE
   r INPUT-LINES RECEIVED$ {: i:ptr i-u:n :}
   i i-u 1 min s" x" STR= if
      response [: LENGTH-VECTOR ;] HTTP:JSON! exit
   then
   i i-u 1 min s" y" STR= if
      response [: INDEX-VECTORS ;] HTTP:JSON! exit
   then
   i i-u S\" one\ntwo" STR= if
      \ The provider may list them out of order; index decides each slot.
      response [: BASE64-VECTORS ;] HTTP:JSON! exit
   then
   response i i-u BASE64-CASE? if exit then
   i i-u s" wide" STR= if
      response [: WIDE-VECTOR ;] HTTP:JSON! exit
   then
   i i-u s" wrong-dims" STR= if
      response [: S\" {\qobject\q:\qlist\q,\qdata\q:[{\qembedding\q:[0.1,0.2],\qindex\q:0}]}" JSON:WRITE-RAW ;] HTTP:JSON!
      exit
   then
   i i-u s" bad-data" STR= if
      response [: S\" {\qobject\q:\qlist\q,\qdata\q:\qx\q,\qusage\q:{\qprompt_tokens\q:2,\qtotal_tokens\q:2}}" JSON:WRITE-RAW ;] HTTP:JSON!
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


\ Ollama's /api/embed: no Authorization header, truncation refused and no
\ encoding asked for.
: OLLAMA ( HTTP:request HTTP:response -- )
   {: r:HTTP:request response:HTTP:response :}
   1 CALLS atomic-add drop
   r s" Authorization" HTTP:HEADER-OF$ {: a:ptr u:n present:bool :}
   present if response 401 HTTP:STATUS! exit then
   r s" truncate" BODY-MEMBER JSON:READ-TOKEN JSON:T-FALSE = ASSERT JSON:READ-CLOSE
   r HTTP:BODY$ s" encoding_format" CONTAINS? 0= ASSERT
   r INPUT-LINES RECEIVED$ {: i:ptr i-u:n :}
   i i-u S\" one\ntwo" STR= if
      response [: S\" {\qembeddings\q:[[0.1,0.2,0.3],[0,1,2]]}" JSON:WRITE-RAW ;] HTTP:JSON!
      exit
   then
   i i-u s" bad-count" STR= if
      response [: S\" {\qembeddings\q:[]}" JSON:WRITE-RAW ;] HTTP:JSON! exit
   then
   \ No reply: the connection stays open until the client closes it.
   i i-u s" hold" STR= if r HTTP:TAKE-OVER 2drop HOLD-OPEN exit then
   i i-u s" bad-dims" STR= if
      response [: S\" {\qembeddings\q:[[0,1]]}" JSON:WRITE-RAW ;] HTTP:JSON! exit
   then
   i i-u s" bad-finite" STR= if
      response [: S\" {\qembeddings\q:[[1e309,0,1]]}" JSON:WRITE-RAW ;] HTTP:JSON! exit
   then
   \ Finite as a double, past binary32's range.
   i i-u s" bad-single" STR= if
      response [: S\" {\qembeddings\q:[[1e39,0,1]]}" JSON:WRITE-RAW ;]
      HTTP:JSON! exit
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


\ Runs the handler while no other handler runs.
: ALONE ( HTTP:request HTTP:response [ HTTP:request HTTP:response -- ] -- )
   TURN TASK:WAIT-SEM
   [: TURN TASK:SIGNAL-SEM ;] finally ;


public
EXPORT LONG-FINISH$
EXPORT REFUSAL$
EXPORT CUT$
EXPORT BAD-CONTENT$
EXPORT REASONING$
EXPORT FUNCTION-CALL$
EXPORT BIG$
EXPORT PLAIN$
EXPORT BAD-TYPE$
EXPORT FLAT-ERROR$
EXPORT NESTED-ERROR$
EXPORT FLEX-OUT$
EXPORT NO-ID$
EXPORT WIDE-DIMS
: BASE-URL$ ( -- ptr u8 n ) BASE-URL BASE-URL-U @ ;
: OLLAMA-URL$ ( -- ptr u8 n ) OLLAMA-URL OLLAMA-URL-U @ ;
\ Requests the peer received, refused or served.
: CALLS@ ( -- n ) CALLS atomic@ ;
\ The last chat, count, compaction or embedding request the peer received;
\ one past SENT-CAP fails the test.
: SENT$ ( -- ptr u8 n ) SENT-U @ dup 0< if FAIL then SENT-BODY swap ;
\ Answers the next chat, count, compaction or OpenAI embedding request as the
\ chat scenario named, whatever its text, as a provider outage does; the request
\ after it as its own text says.
: FAIL-NEXT ( ptr u8 n -- )
   {: a:ptr u:n :}
   u 0= u FAILURE-CAP > or if FAIL exit then
   a u FAILURE FAILURE-CAP SPAN:MAKE SPAN:COPY
   u FAILURE-U atomic! ;
\ Waits until the peer has sent the held response's in_progress event.
: AWAIT-HOLD ( -- ) HELD TAKE ;
\ Lets the reply flex-gate holds go.
: LET-GO ( -- ) GO TASK:SIGNAL-SEM ;


\ Four workers serve MANY-CALLS' four calls at once, one of them held.
: START ( -- )
   BIG-U MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop BIG !
   BIG-FILL
   0 HELD TASK:SEMAPHORE-INIT
   1 TURN TASK:SEMAPHORE-INIT
   0 GO TASK:SEMAPHORE-INIT
   0 CALLS atomic!
   STREAM-B 1 BUF:N>BLEN BUF:INIT
   HTTP:ROUTES-RESET
   s" POST" s" /responses" [: [: CHAT ;] ALONE ;] HTTP:ROUTE
   s" POST" s" /responses/input_tokens" [: [: INPUT-TOKENS ;] ALONE ;] HTTP:ROUTE
   s" POST" s" /responses/compact" [: [: COMPACTS ;] ALONE ;] HTTP:ROUTE
   s" POST" s" /embeddings" [: [: EMBEDDINGS ;] ALONE ;] HTTP:ROUTE
   s" POST" s" /api/embed" [: [: OLLAMA ;] ALONE ;] HTTP:ROUTE
   $7F000001 0 4 1000 HTTP:START
   BASE-URL s" " URL! BASE-URL-U !
   OLLAMA-URL s" /api/embed" URL! OLLAMA-URL-U ! ;


: STOP ( -- )
   HTTP:STOP
   HELD TASK:SEMAPHORE-DESTROY
   TURN TASK:SEMAPHORE-DESTROY
   GO TASK:SEMAPHORE-DESTROY
   STREAM-B BUF:DISPOSE
   BIG$ MEM:BYTES-ALLOC-LEN MEM:RELEASE-BYTES ;
;using
;package


package MODEL-TEST
using MODEL-FIXTURE
private
4 TYPED-BUFFER CALL MODEL:call
$10000 TASK:TASK WORK-TASK
TASK:SEMAPHORE STOP-READY
TASK:SEMAPHORE STOP-GO
TASK:SEMAPHORE LOCK-ENTERED
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
variable CHAT-NS    \ TIME:MONO-NS as the last CHAT-CODE began
: CHAT-CODE ( -- n ) TIME:MONO-NS CHAT-NS ! M [: dup MODEL:CHAT 2drop ;] catch {: code:n :} drop code ;
: EMBED-CODE ( -- n ) M [: dup MODEL:EMBED ;] catch {: code:n :} drop code ;
: VECTOR-CODE ( -- n ) 0 [: drop 0 M MODEL:VECTOR@ 2drop 0 ;] CODE-OF ;
: PREP ( ptr u8 n -- ) M MODEL:PARTS-RESET M MODEL:TEXT+ ;
: INPUTS ( ptr u8 n -- ) M MODEL:INPUT-RESET M MODEL:INPUT+ ;
: CHAT ( -- ) s" valid" PREP M MODEL:CHAT TRUE-CONTENT$ STR= ASSERT ;
: FAILS ( ptr u8 n n -- ) {: a:ptr u:n want:n :} a u PREP CHAT-CODE want = ASSERT ;
: INPUT-LEN ( n -- n ) {: u:n :} PAD u M MODEL:INPUT+ u ;
: DIMS ( n -- n ) {: d:n :} s" test-embedding" d 60000 M MODEL:EMBEDDING-MODEL! d ;


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


\ Whether a captured line, after its stamp, passes the test against the text.
: LOGGED-BY? ( ptr u8 n [ ptr u8 n ptr u8 n -- bool ] -- bool )
   {: want:ptr want-u:n pred :}
   LOG$ {: a:ptr u:n :}
   0 u 0 ?do
      a i + c@ LF = if
         i over - over a swap + swap want want-u pred execute if drop true unloop exit then
         drop i 1+
      then
   loop drop false ;


: LOGGED? ( ptr u8 n -- bool ) [: LINE? ;] LOGGED-BY? ;


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


\ Whether a captured line, after its stamp, is the text, a count of at most the
\ milliseconds since CHAT-CODE began and " ms".
: MS-LINE? ( ptr u8 n ptr u8 n -- bool )
   {: line:ptr line-u:n want:ptr want-u:n :}
   line-u DATE:TIME-LEN - want-u - 3 - {: digits:n :}
   digits 1 < if false exit then
   line DATE:TIME-LEN + {: text:ptr :}
   text want-u want want-u STR= 0= if false exit then
   text want-u + digits + 3 s"  ms" STR= 0= if false exit then
   0 digits 0 ?do
      text want-u + i + c@ dup STR-DIGIT? 0= if 2drop false unloop exit then
      [char] 0 - swap 10 * +
   loop
   TIME:MONO-NS CHAT-NS @ - 1000000 / <= ;


\ SB-LOGGED for a line that ends with SB, a count of milliseconds and " ms".
: SB-MS-LOGGED ( -- )
   SB$ [: MS-LINE? ;] LOGGED-BY? dup 0= if SB$ type cr LOG$ type then ASSERT ;


: CHAT-LOGGED ( ptr u8 n ptr u8 n -- )
   {: level:ptr level-u:n tail:ptr tail-u:n :}
   level level-u CHAT-LINE tail tail-u SB-APPEND SB-LOGGED ;


\ CHAT-LOGGED for a reply that named its response id.
: CHAT-ID-LOGGED ( ptr u8 n ptr u8 n -- )
   {: level:ptr level-u:n tail:ptr tail-u:n :}
   level level-u CHAT-ID-LINE tail tail-u SB-APPEND SB-LOGGED ;


\ The event counts of a stream PRELUDE wrote.
: PRELUDE-EVENTS$ ( -- ptr u8 n )
   s" ; events response.created=1 response.in_progress=1 response.output_text.delta=1 keepalive=1" ;


\ Appends to SB the events of a stream STREAMED wrote, which ends with an
\ event of the type, up to the count of milliseconds.
: STREAM-TAIL ( ptr u8 n -- )
   {: type:ptr type-u:n :}
   PRELUDE-EVENTS$ SB-APPEND s"  " SB-APPEND type type-u SB-APPEND
   s" =1; first output after " SB-APPEND ;


\ SB-MS-LOGGED for a stream STREAMED wrote, which ends with an event of the type.
: STREAMED-LOGGED ( ptr u8 n -- ) STREAM-TAIL SB-MS-LOGGED ;


\ The body logs no line.
: QUIET ( [ -- ] -- )
   LINES {: before:n :}
   execute
   LINES before = dup 0= if LOG$ type then ASSERT ;


: FIXTURE-KEY ( -- ) KEY-SERVICE$ KEY-ACCOUNT$ M MODEL:API-KEY! ;


\ Opens call n against the peer with the fixture key, system text and schema.
: SETUP-CALL ( n -- )
   CALL {: c:ptr :}
   c MODEL:OPEN
   MODEL-PEER:BASE-URL$ c MODEL:BASE-URL!
   s" test-generation" c MODEL:GENERATION-MODEL!
   s" test-embedding" 3 60000 c MODEL:EMBEDDING-MODEL!
   KEY-SERVICE$ KEY-ACCOUNT$ c MODEL:API-KEY!
   s" Read source evidence." c MODEL:SYSTEM!
   SCHEMA$ c MODEL:SCHEMA! ;


: SETUP ( -- ) 0 SETUP-CALL ;


: IMAGE ( -- )
   s" image" PREP
   s" image/png" s" aGVsbG8=" M MODEL:IMAGE+
   M MODEL:CHAT S\" {\qimages\q:[\qdata:image/png;base64,aGVsbG8=\q]}" STR= ASSERT ;


: SERVICE$ ( -- ptr u8 n ) KEY-SERVICE$ ;
\ The second key item's account.
: ACCOUNT$ ( -- ptr u8 n ) s" model-store" ;
: STORE ( ptr u8 n -- ) {: v:ptr vu:n :} SERVICE$ ACCOUNT$ v vu SECRET:WRITE ;
: STORED-FAILS ( ptr u8 n -- ) STORE s" valid" MODEL:E-KEY FAILS ;


\ A line for a failed key read: the code's name, then the key's service and
\ account and why.
: KEY-LOGGED ( ptr u8 n ptr u8 n -- )
   {: code:ptr code-u:n why:ptr why-u:n :}
   s" error" CHAT-LINE code code-u SB-APPEND
   s"  key service " SB-APPEND SERVICE$ SB-APPEND
   s" , account " SB-APPEND ACCOUNT$ SB-APPEND
   s" : " SB-APPEND why why-u SB-APPEND SB-LOGGED ;


\ The store refuses the key read with the code, which the call throws and its
\ line names.
: REFUSED-READ ( n ptr u8 n ptr u8 n -- )
   {: code:n name:ptr name-u:n why:ptr why-u:n :}
   code READ-FAULT!
   s" valid" code FAILS
   0 READ-FAULT!
   name name-u why why-u KEY-LOGGED ;


: STORE-REFUSALS ( -- )
   SECRET:E-ACCESS s" SECRET:E-ACCESS" s" access denied" REFUSED-READ
   SECRET:E-UNAVAILABLE s" SECRET:E-UNAVAILABLE" s" store unavailable"
   REFUSED-READ
   SECRET:E-CONFIG s" SECRET:E-CONFIG"
   s" store not set up, or service or account invalid" REFUSED-READ ;


\ A missing item and the store's refusals, then the peer's key, then an empty
\ and a non-printable value, each logged unsent with the key and why; then a
\ 4096-byte value that is accepted and sent: the peer's 401 refuses it, also
\ E-KEY.
: KEY-CASES ( -- )
   SERVICE$ ACCOUNT$ M MODEL:API-KEY!
   s" valid" MODEL:E-KEY FAILS
   s" MODEL:E-KEY" s" not found" KEY-LOGGED
   STORE-REFUSALS
   s" local-test-key" STORE
   CHAT
   s" " STORED-FAILS
   s" MODEL:E-KEY" s" empty" KEY-LOGGED
   s\" local\x01test-key" STORED-FAILS
   s" MODEL:E-KEY" s" not printable" KEY-LOGGED
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


: REPLIES ( -- )
   s" refusal" MODEL:E-REFUSED FAILS
   s" error" CHAT-ID-LINE s" MODEL:E-REFUSED status 200 body: " SB-APPEND
   MODEL-PEER:REFUSAL$ SB-APPEND
   s" response.completed" STREAMED-LOGGED
   s" length" MODEL:E-INCOMPLETE FAILS
   M MODEL:INCOMPLETE-REASON$ s" max_output_tokens" STR= ASSERT
   s" warn" CHAT-ID-LINE s" MODEL:E-INCOMPLETE incomplete max_output_tokens" SB-APPEND
   s" response.incomplete" STREAMED-LOGGED
   s" filtered" MODEL:E-INCOMPLETE FAILS
   M MODEL:INCOMPLETE-REASON$ s" content_filter" STR= ASSERT
   s" long-finish" MODEL:E-INCOMPLETE FAILS
   M MODEL:INCOMPLETE-REASON$ MODEL-PEER:LONG-FINISH$ STR= ASSERT
   s" bad-content" MODEL:E-RESPONSE FAILS
   M MODEL:INCOMPLETE-REASON$ nip 0= ASSERT
   s" error" CHAT-ID-LINE s" MODEL:E-RESPONSE status 200 body: " SB-APPEND
   MODEL-PEER:BAD-CONTENT$ SB-APPEND
   s" response.completed" STREAMED-LOGGED
   s" note-first" PREP M MODEL:CHAT TRUE-CONTENT$ STR= ASSERT
   s" note-last" PREP M MODEL:CHAT TRUE-CONTENT$ STR= ASSERT
   s" phase-null" PREP M MODEL:CHAT TRUE-CONTENT$ STR= ASSERT
   s" note-only" MODEL:E-RESPONSE FAILS
   s" answer-twice" MODEL:E-RESPONSE FAILS
   s" phase-other" MODEL:E-RESPONSE FAILS
   s" answer-other" MODEL:E-RESPONSE FAILS
   s" failed-server" MODEL:E-UNAVAILABLE FAILS
   s" error" CHAT-ID-LINE s" MODEL:E-UNAVAILABLE status 200: The server had an error." SB-APPEND
   s" response.failed" STREAMED-LOGGED
   s" failed-size" MODEL:E-CAPACITY FAILS
   s" failed-other" MODEL:E-HTTP FAILS
   M MODEL:ERROR$ s" bad prompt" STR= ASSERT
   s" error-event" MODEL:E-UNAVAILABLE FAILS
   M MODEL:ERROR$ s" The server had an error." STR= ASSERT
   s" error" CHAT-ID-LINE
   s" MODEL:E-UNAVAILABLE status 200: The server had an error. body: " SB-APPEND
   MODEL-PEER:FLAT-ERROR$ SB-APPEND
   s" ; events response.created=1 response.in_progress=1 error=1; no output" SB-APPEND
   SB-LOGGED
   s" error-nested" MODEL:E-UNAVAILABLE FAILS
   M MODEL:ERROR$ s" The server had an error." STR= ASSERT
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
   s" error" CHAT-LINE s" MODEL:E-KEY key service " SB-APPEND
   KEY-SERVICE$ SB-APPEND s" , account " SB-APPEND KEY-ACCOUNT$ SB-APPEND
   s" : refused by the provider status 403" SB-APPEND SB-LOGGED
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


\ A stream that ends before the response ends the call; one that drops after
\ it does not.
: EARLY-ENDS ( -- )
   s" done-drop" PREP M MODEL:CHAT TRUE-CONTENT$ STR= ASSERT
   M MODEL:HTTP-STATUS 200 = ASSERT
   s" early-drop" MODEL:E-TRANSPORT FAILS
   s" error" CHAT-LINE s" MODEL:E-TRANSPORT curl 18: " SB-APPEND
   18 CURL:>CODE CURL:ERROR$ SB-APPEND s" ; events keepalive=1; no output" SB-APPEND
   SB-LOGGED
   s" id-drop" MODEL:E-TRANSPORT FAILS
   s" error" CHAT-ID-LINE s" MODEL:E-TRANSPORT curl 18: " SB-APPEND
   18 CURL:>CODE CURL:ERROR$ SB-APPEND
   PRELUDE-EVENTS$ SB-APPEND s" ; first output after " SB-APPEND SB-MS-LOGGED
   s" twice-drop" MODEL:E-TRANSPORT FAILS
   s" error" CHAT-ID-LINE s" MODEL:E-TRANSPORT curl 18: " SB-APPEND
   18 CURL:>CODE CURL:ERROR$ SB-APPEND
   s" ; events keepalive=2 response.created=1 response.in_progress=1; no output" SB-APPEND
   SB-LOGGED
   s" no-progress" MODEL:E-TRANSPORT FAILS
   s" error" s" chat test-generation" MODEL-PEER:BASE-URL$ s" /responses resp_quiet" SB-LINE
   S\" MODEL:E-TRANSPORT status 200 body: {\qid\q:\qresp_quiet\q,\qstatus\q:\qqueued\q,\qoutput\q:[]}"
   SB-APPEND s" ; events response.created=1; no output" SB-APPEND SB-LOGGED ;


\ SB holds the log line's text up to its code; the line has the unusable
\ reply's status 200, the whole body and then the tail.
: BODY-LOGGED ( ptr u8 n ptr u8 n -- )
   {: a:ptr u:n tail:ptr tail-u:n :}
   s" MODEL:E-RESPONSE status 200 body: " SB-APPEND a u SB-APPEND
   tail tail-u SB-APPEND SB-LOGGED ;


\ A reply the call cannot read is logged whole, an event that is not UTF-8 as
\ its chunk with line feeds escaped.
: UNREADABLE ( -- )
   s" plain-json" MODEL:E-RESPONSE FAILS
   s" error" CHAT-LINE MODEL-PEER:PLAIN$ s" " BODY-LOGGED
   s" bad-type" MODEL:E-RESPONSE FAILS
   s" error" CHAT-ID-LINE MODEL-PEER:BAD-TYPE$
   s" ; events response.created=1 response.in_progress=1; no output" BODY-LOGGED
   s" no-id" MODEL:E-RESPONSE FAILS
   s" error" CHAT-LINE MODEL-PEER:NO-ID$ s" ; events response.created=1; no output" BODY-LOGGED
   s" bad-utf8" MODEL:E-RESPONSE FAILS
   s" error" CHAT-ID-LINE
   S\" event: response.created\\x0adata: {\qtype\q:\qresponse.created\q,\qsequence_number\q:0,\qresponse\q:{\qid\q:\qresp_1\q,\qstatus\q:\qqueued\q,\qoutput\q:[]}}\\x0a\\x0adata: \xff\\x0a\\x0a"
   s" ; events response.created=1; no output" BODY-LOGGED ;


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
\ sent its in_progress event.
: HOLD ( -- )
   s" hold" PREP
   ['] CHAT-WORK WORK-TASK TASK:ACTIVATE
   MODEL-PEER:AWAIT-HOLD ;


\ A stop ends the held stream's transfer at once.
1000000000 constant PROMPT-NS


\ The held call, stopped at mono-ns t0, ended cancelled at once.
: HOLD-ENDED ( n -- )
   {: t0:n :}
   CANCELLED
   mono-ns t0 - PROMPT-NS < ASSERT ;


: CANCELLATION ( -- )
   HOLD mono-ns
   M MODEL:CANCEL ASSERT
   HOLD-ENDED
   M MODEL:CANCEL 0= ASSERT ;


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


\ The vector is three binary32 values with these bits.
: VECTOR-IS ( ptr u8 n n n n -- )
   {: v:ptr v-u:n x:n y:n z:n :}
   v-u 12 = ASSERT
   v F32-BUF:LOAD x = ASSERT
   v 4 + F32-BUF:LOAD y = ASSERT
   v 8 + F32-BUF:LOAD z = ASSERT ;


: EMBEDDINGS ( -- )
   s" one" INPUTS
   s" two" M MODEL:INPUT+
   M MODEL:EMBED
   0 M MODEL:VECTOR@ $3DCCCCCD $3E4CCCCD $3E99999A VECTOR-IS
   1 M MODEL:VECTOR@ $3ECCCCCD $3F000000 $3F19999A VECTOR-IS ;


\ Finishes the request for `text` with JOURNAL-INPUT$.
: PREPARE ( ptr u8 n -- ) PREP M MODEL:JOURNAL-INPUT$ 2drop ;


\ With no journal, CHAT sends the request JOURNAL-INPUT$ finished once and
\ consumes it: a second CHAT has no request and is E-STATE. EMBED, PURPOSE! and
\ SYSTEM! discard a finished request, so the CHAT after them is E-STATE too.
: PREPARED-CHAT ( -- )
   MODEL-PEER:CALLS@ {: before:n :}
   s" valid" PREPARE
   M MODEL:CHAT TRUE-CONTENT$ STR= ASSERT
   MODEL-PEER:CALLS@ before 1+ = ASSERT
   CHAT-CODE MODEL:E-STATE = ASSERT
   s" valid" PREPARE
   s" x" INPUTS M MODEL:EMBED
   CHAT-CODE MODEL:E-STATE = ASSERT
   s" valid" PREPARE
   s" assemble" M MODEL:PURPOSE!
   CHAT-CODE MODEL:E-STATE = ASSERT
   s" valid" PREPARE
   s" Read source evidence." M MODEL:SYSTEM!
   CHAT-CODE MODEL:E-STATE = ASSERT ;


: START-CODE ( -- n ) 0 [: drop M MODEL:START 0 ;] CODE-OF ;
: AWAIT-CODE ( -- n ) 0 [: drop M MODEL:AWAIT 0 ;] CODE-OF ;
: CONTENT-CODE ( -- n ) 0 [: drop M MODEL:CONTENT$ 2drop 0 ;] CODE-OF ;
: STATE-IS ( n [ n -- n ] -- ) CODE-OF MODEL:E-STATE = ASSERT ;
: SERVE-CODE ( -- n ) 0 [: drop M MODEL:SERVE drop 0 ;] CODE-OF ;
\ The longest one wait between tries; a transfer's bytes or end wake the
\ task sooner.
100 constant SERVE-WAIT-MS
: SERVE-PAUSE ( -- ) SERVE-WAIT-MS >MS TASK:WAIT-MS drop ;


: SERVED ( ptr MODEL:call -- )
   {: c:ptr :}
   begin c MODEL:SERVE 0= while
      SERVE-PAUSE
   repeat ;


\ START sends the request JOURNAL-INPUT$ finished, else the one built since
\ PARTS-RESET; CONTENT$ reads only the chat AWAIT last ended.
: STARTED-CALLS ( -- )
   CHAT
   M MODEL:CONTENT$ TRUE-CONTENT$ STR= ASSERT
   AWAIT-CODE MODEL:E-STATE = ASSERT
   s" valid" PREPARE
   AWAIT-CODE MODEL:E-STATE = ASSERT
   CONTENT-CODE MODEL:E-STATE = ASSERT
   MODEL-PEER:CALLS@ {: before:n :}
   M MODEL:START M MODEL:AWAIT
   M MODEL:CONTENT$ TRUE-CONTENT$ STR= ASSERT
   MODEL-PEER:CALLS@ before 1+ = ASSERT
   s" valid" PREP
   M MODEL:START M MODEL:AWAIT
   M MODEL:CONTENT$ TRUE-CONTENT$ STR= ASSERT
   MODEL-PEER:CALLS@ before 2 + = ASSERT
   s" one" INPUTS
   s" two" M MODEL:INPUT+
   M MODEL:START-EMBED M MODEL:AWAIT
   0 M MODEL:VECTOR@ $3DCCCCCD $3E4CCCCD $3E99999A VECTOR-IS
   1 M MODEL:VECTOR@ $3ECCCCCD $3F000000 $3F19999A VECTOR-IS
   CONTENT-CODE MODEL:E-STATE = ASSERT ;


\ A started call refuses the words that send, prepare, set or close it, and
\ CONTENT$; CANCEL cuts it, so AWAIT throws E-CANCELLED at once.
: HELD-START ( -- )
   s" hold" PREP M MODEL:START MODEL-PEER:AWAIT-HOLD
   START-CODE MODEL:E-STATE = ASSERT
   0 [: drop M MODEL:START-EMBED 0 ;] STATE-IS
   CHAT-CODE MODEL:E-STATE = ASSERT
   EMBED-CODE MODEL:E-STATE = ASSERT
   0 [: drop M MODEL:JOURNAL-INPUT$ 2drop 0 ;] STATE-IS
   CONTENT-CODE MODEL:E-STATE = ASSERT
   0 [: drop s" Read source evidence." M MODEL:SYSTEM! 0 ;] STATE-IS
   0 [: drop M MODEL:CLOSE 0 ;] STATE-IS
   mono-ns {: t0:n :}
   M MODEL:CANCEL ASSERT
   AWAIT-CODE MODEL:E-CANCELLED = ASSERT
   mono-ns t0 - PROMPT-NS < ASSERT
   CONTENT-CODE MODEL:E-STATE = ASSERT
   AWAIT-CODE MODEL:E-STATE = ASSERT ;


\ A fault before the transfer lands the call in START, which still returns;
\ AWAIT throws it.
: LANDED-START ( -- )
   SERVICE$ s" missing" M MODEL:API-KEY!
   MODEL-PEER:CALLS@ {: calls:n :}
   s" valid" PREP M MODEL:START
   M MODEL:CANCEL 0= ASSERT
   START-CODE MODEL:E-STATE = ASSERT
   AWAIT-CODE MODEL:E-KEY = ASSERT
   MODEL-PEER:CALLS@ calls = ASSERT
   FIXTURE-KEY ;


\ A provider's refusal of a started chat or embed is AWAIT's fault, the code
\ CHAT or EMBED throws for it, and leaves no content of the chat before it.
: FAILED-START ( -- )
   s" too-large" PREP CHAT-CODE {: c:n :}
   c MODEL:E-CAPACITY = ASSERT
   CHAT
   s" too-large" PREP M MODEL:START
   AWAIT-CODE c = ASSERT
   CONTENT-CODE MODEL:E-STATE = ASSERT
   CHAT
   s" too-large" INPUTS M MODEL:START-EMBED
   AWAIT-CODE MODEL:E-CAPACITY = ASSERT
   CONTENT-CODE MODEL:E-STATE = ASSERT ;


create KEPT 65536 allot    \ the request body the peer received before a resend
variable KEPT-U
: KEPT$ ( -- ptr u8 n ) KEPT KEPT-U @ ;
\ Keeps the body of the last request the peer received.
: KEEP-SENT ( -- )
   MODEL-PEER:SENT$ {: a:ptr u:n :}
   a u KEPT 65536 SPAN:MAKE SPAN:COPY u KEPT-U ! ;
\ The peer received the body it received before the resend.
: SENT-AGAIN ( -- ) MODEL-PEER:SENT$ KEPT$ STR= ASSERT ;
: RETRY ( -- ) s" retry" M MODEL:PURPOSE! ;


\ The chat request built under RETRY, which the peer answers as `scenario`,
\ throws `code`; then RETRY and CHAT send the same body once more, and the
\ peer's reply to it is returned.
: RESENT ( ptr u8 n n -- )
   {: a:ptr u:n code:n :}
   MODEL-PEER:CALLS@ {: before:n :}
   a u MODEL-PEER:FAIL-NEXT
   CHAT-CODE code = ASSERT
   MODEL-PEER:CALLS@ before 1+ = ASSERT
   KEEP-SENT
   RETRY M MODEL:CHAT TRUE-CONTENT$ STR= ASSERT
   MODEL-PEER:CALLS@ before 2 + = ASSERT
   SENT-AGAIN ;


\ A chat that faults keeps its request, finished by JOURNAL-INPUT$ or built,
\ whether the provider answered with an error or cut the stream: PURPOSE! and
\ CHAT send it again byte for byte, and the chat that succeeds uses it up. A
\ key fault sends nothing, so its resend is the one request the peer receives.
: FAILED-RESENT ( -- )
   RETRY s" valid" PREPARE s" http-error" MODEL:E-UNAVAILABLE RESENT
   CHAT-CODE MODEL:E-STATE = ASSERT
   RETRY s" valid" PREP s" early-drop" MODEL:E-TRANSPORT RESENT
   CHAT-CODE MODEL:E-STATE = ASSERT
   SERVICE$ s" missing" M MODEL:API-KEY!
   MODEL-PEER:CALLS@ {: before:n :}
   RETRY s" valid" PREP CHAT-CODE MODEL:E-KEY = ASSERT
   MODEL-PEER:CALLS@ before = ASSERT
   FIXTURE-KEY
   RETRY M MODEL:CHAT TRUE-CONTENT$ STR= ASSERT
   MODEL-PEER:CALLS@ before 1+ = ASSERT ;


\ A failing send's line names the work by the call's note, before its code;
\ the note changes no request the peer receives.
: NOTED-FAILURE ( -- )
   CHAT KEEP-SENT
   s" job 7 extract step-3" M MODEL:LOG-NOTE!
   CHAT SENT-AGAIN
   s" http-bad" MODEL:E-HTTP FAILS
   s" error" s" job 7 extract step-3: MODEL:E-HTTP status 400: bad request"
   CHAT-LOGGED
   s" " M MODEL:LOG-NOTE! ;


\ SERVE refuses a call not started; a started call answers true once it lands
\ and again until AWAIT ends it, and a fault before the transfer lands it.
: SERVED-CALL ( -- )
   CHAT
   SERVE-CODE MODEL:E-STATE = ASSERT
   s" valid" PREPARE
   SERVE-CODE MODEL:E-STATE = ASSERT
   MODEL-PEER:CALLS@ {: before:n :}
   M MODEL:START M SERVED
   M MODEL:SERVE ASSERT
   M MODEL:AWAIT
   M MODEL:CONTENT$ TRUE-CONTENT$ STR= ASSERT
   MODEL-PEER:CALLS@ before 1+ = ASSERT
   SERVE-CODE MODEL:E-STATE = ASSERT
   SERVICE$ s" missing" M MODEL:API-KEY!
   s" valid" PREP M MODEL:START
   M MODEL:SERVE ASSERT
   AWAIT-CODE MODEL:E-KEY = ASSERT
   MODEL-PEER:CALLS@ before 1+ = ASSERT
   FIXTURE-KEY ;


: SERVE-WORK ( -- ) M MODEL:SERVE drop 0 TASK:RETURN ;
: AWAIT-WORK ( -- ) M MODEL:AWAIT 0 TASK:RETURN ;


\ The work task's run ended with E-STATE.
: STATE-WORK ( -- )
   WORK-TASK TASK:JOIN MATCH result
      ok OF drop FAIL ENDOF
      err OF MODEL:E-STATE = ASSERT ENDOF
   ;MATCH ;


\ SERVE and AWAIT of a started call are E-STATE on a task that did not start
\ it; its starter then lands, ends and closes it.
: STARTER-ONLY ( -- )
   MODEL-PEER:CALLS@ {: before:n :}
   s" valid" PREP M MODEL:START
   ['] SERVE-WORK WORK-TASK TASK:ACTIVATE STATE-WORK
   ['] AWAIT-WORK WORK-TASK TASK:ACTIVATE STATE-WORK
   M SERVED
   M MODEL:AWAIT
   M MODEL:CONTENT$ TRUE-CONTENT$ STR= ASSERT
   MODEL-PEER:CALLS@ before 1+ = ASSERT
   M MODEL:CLOSE ;


\ A halt of the starter while it waits lands the call with the transfer's
\ aborted fault before the task ends; another task then ends and closes it.
: HALTED-STARTER ( -- )
   HOLD
   WORK-TASK TASK:HALT
   WORK-TASK TASK:JOIN MATCH result
      ok OF drop FAIL ENDOF
      err OF drop ENDOF
   ;MATCH
   AWAIT-CODE MODEL:E-TRANSPORT = ASSERT
   M MODEL:CLOSE ;


\ SERVE never waits for a held stream; after CANCEL the call lands and AWAIT
\ throws E-CANCELLED.
: HELD-SERVE ( -- )
   s" hold" PREP M MODEL:START MODEL-PEER:AWAIT-HOLD
   M MODEL:SERVE 0= ASSERT
   M MODEL:SERVE 0= ASSERT
   M MODEL:CANCEL ASSERT
   M SERVED
   AWAIT-CODE MODEL:E-CANCELLED = ASSERT
   SERVE-CODE MODEL:E-STATE = ASSERT ;


: START-CHAT ( ptr u8 n ptr MODEL:call -- )
   {: a:ptr u:n c:ptr :}
   c MODEL:PARTS-RESET
   a u c MODEL:TEXT+
   c MODEL:START ;


\ Bit i is set once call i's SERVE has answered true.
variable LANDED
: SERVE-BIT ( n -- ) {: i:n :} i CALL MODEL:SERVE if LANDED @ 1 i lshift or LANDED ! then ;


\ Serves calls 0 to 3 once each. True once calls 1, 2 and 3 have each answered
\ true, whatever the held call 0 did; a landed call 0 FAILs at MANY-CALLS's cancel.
: ROUND ( -- bool )
   0 SERVE-BIT 1 SERVE-BIT 2 SERVE-BIT 3 SERVE-BIT
   LANDED @ 14 and 14 = ;


: ROUNDS ( -- )
   0 LANDED !
   begin ROUND 0= while
      SERVE-PAUSE
   repeat ;


\ One task serves four started calls in rounds: three land while the held one
\ stays out, and each ends with its own result.
: MANY-CALLS ( -- )
   1 SETUP-CALL 2 SETUP-CALL 3 SETUP-CALL
   MODEL-PEER:CALLS@ {: before:n :}
   s" hold" M START-CHAT MODEL-PEER:AWAIT-HOLD
   s" valid" 1 CALL START-CHAT
   2 CALL MODEL:INPUT-RESET
   s" one" 2 CALL MODEL:INPUT+
   s" two" 2 CALL MODEL:INPUT+
   2 CALL MODEL:START-EMBED
   s" valid" 3 CALL START-CHAT
   ROUNDS
   M MODEL:CANCEL ASSERT
   M SERVED
   AWAIT-CODE MODEL:E-CANCELLED = ASSERT
   1 CALL MODEL:AWAIT
   1 CALL MODEL:CONTENT$ TRUE-CONTENT$ STR= ASSERT
   2 CALL MODEL:AWAIT
   0 2 CALL MODEL:VECTOR@ $3DCCCCCD $3E4CCCCD $3E99999A VECTOR-IS
   1 2 CALL MODEL:VECTOR@ $3ECCCCCD $3F000000 $3F19999A VECTOR-IS
   3 CALL MODEL:AWAIT
   3 CALL MODEL:CONTENT$ TRUE-CONTENT$ STR= ASSERT
   MODEL-PEER:CALLS@ before 4 + = ASSERT
   1 CALL MODEL:CLOSE 2 CALL MODEL:CLOSE 3 CALL MODEL:CLOSE ;


\ A reply spoiled mid-stream is the served call's fault, the code CHAT throws.
: SPOILED-SERVE ( -- )
   s" bad-utf8" PREP CHAT-CODE {: c:n :}
   c MODEL:E-RESPONSE = ASSERT
   s" bad-utf8" PREP M MODEL:START M SERVED
   AWAIT-CODE c = ASSERT
   CONTENT-CODE MODEL:E-STATE = ASSERT ;


\ A malformed embeddings reply fails the call and leaves no vectors.
: EMBED-ERROR ( ptr u8 n -- )
   INPUTS
   EMBED-CODE MODEL:E-RESPONSE = ASSERT
   VECTOR-CODE MODEL:E-STATE = ASSERT ;


\ An escaped base64 vector reads as the plain one; a short, long, malformed,
\ NaN or infinite one fails the call.
: BASE64-CASES ( -- )
   s" escaped" INPUTS
   M MODEL:EMBED
   0 M MODEL:VECTOR@ $3DCCCCCD $3E4CCCCD $3E99999A VECTOR-IS
   s" b64-short" EMBED-ERROR
   s" b64-long" EMBED-ERROR
   s" b64-bad" EMBED-ERROR
   s" b64-nan" EMBED-ERROR
   s" b64-inf" EMBED-ERROR ;


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


\ An embed the provider fails keeps its inputs: EMBED sends the same body
\ again.
: EMBED-RESENT ( -- )
   s" x" INPUTS
   s" http-error" MODEL-PEER:FAIL-NEXT
   EMBED-CODE MODEL:E-UNAVAILABLE = ASSERT
   KEEP-SENT
   EMBED-CODE 0= ASSERT
   SENT-AGAIN ;


\ A 16 KiB input reaches the provider whole; a call holds MODEL:INPUT-MAX
\ inputs, none empty, and returns vectors only for its inputs.
: INPUT-BOUNDS ( -- )
   PAD PAD-U XS
   M MODEL:INPUT-RESET
   PAD-U INPUT-LEN drop
   M MODEL:EMBED
   0 M MODEL:VECTOR@ PAD-U s>f F32:NARROW 0 0 VECTOR-IS
   0 [: drop 1 M MODEL:VECTOR@ 2drop 0 ;] CODE-OF MODEL:E-CONFIG = ASSERT
   M MODEL:INPUT-RESET
   0 [: INPUT-LEN ;] CODE-OF MODEL:E-CONFIG = ASSERT
   MODEL:INPUT-MAX 0 ?do 1 INPUT-LEN drop loop
   1 [: INPUT-LEN ;] CODE-OF MODEL:E-CAPACITY = ASSERT ;


\ A call of MODEL:INPUT-MAX inputs reaches the provider in one request, and
\ each input's vector is read by its index.
: INPUT-MAX-EMBED ( -- )
   M MODEL:INPUT-RESET
   MODEL:INPUT-MAX 0 ?do s" y" M MODEL:INPUT+ loop
   MODEL-PEER:CALLS@ {: calls:n :}
   M MODEL:EMBED
   MODEL-PEER:CALLS@ calls 1+ = ASSERT
   MODEL:INPUT-MAX 0 ?do
      i M MODEL:VECTOR@ i s>f F32:NARROW 0 0 VECTOR-IS
   loop ;


\ The vector is the WIDE-VECTOR components, 0 up, as binary32 values.
: WIDE-IS ( ptr u8 n -- )
   {: v:ptr v-u:n :}
   v-u MODEL-PEER:WIDE-DIMS 4 * = ASSERT
   MODEL-PEER:WIDE-DIMS 0 ?do
      v i 4 * + F32-BUF:LOAD i s>f F32:NARROW = ASSERT
   loop ;


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


\ The call answers the embedding model it was last given.
: EMBEDDING-NAMED ( -- )
   s" named-embedding" 3 60000 M MODEL:EMBEDDING-MODEL!
   M MODEL:EMBEDDING-MODEL$ s" named-embedding" STR= ASSERT
   3 DIMS drop
   M MODEL:EMBEDDING-MODEL$ s" test-embedding" STR= ASSERT ;


\ Refused bases: another scheme, no host, a blank, a trailing slash, and
\ userinfo or a query that could carry a credential, in a base or an Ollama
\ endpoint. A refusal keeps the configured base.
: FTP-BASE ( n -- n ) s" ftp://x" M MODEL:BASE-URL! ;
: BARE-BASE ( n -- n ) s" http://" M MODEL:BASE-URL! ;
: BLANK-BASE ( n -- n ) s" http://x y" M MODEL:BASE-URL! ;
: SLASH-BASE ( n -- n ) s" http://x/" M MODEL:BASE-URL! ;
: USER-BASE ( n -- n ) s" https://user:token@x" M MODEL:BASE-URL! ;
: QUERY-BASE ( n -- n ) s" https://x/v1?key=k" M MODEL:BASE-URL! ;
: USER-OLLAMA ( n -- n ) s" http://user:token@x/api/embed" s" bge-m3" s" bge-m3" 3 60000 M MODEL:OLLAMA! ;
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
: TEST-RECORDER ( -- ) 0 [: TEST-RECORD ;] [: TEST-REJECT ;] M MODEL:RECORDER! ;
: ROW$ ( -- ptr u8 n ) ROW-TEXT ROW-U @ ;
\ A reader at the value of the last recorded row's member `key`.
: ROW ( ptr u8 n -- JSON:reader ) {: key:ptr key-u:n :} ROW$ key key-u MEMBER ;
: BLANK-PURPOSE ( n -- n ) s"  " M MODEL:PURPOSE! ;
: PURPOSE ( -- ) s" assemble" M MODEL:PURPOSE! ;
\ The first generation row's prefix hash, 64 hex digits.
create HASH 64 allot


\ How long a locked callback waits, as on a database lock, unless halted.
3000000000 constant LOCK-NS


\ Signals LOCK-ENTERED, then waits LOCK-NS, serving a halt at each turn.
: LOCK-WAIT ( -- )
   LOCK-ENTERED TASK:SIGNAL-SEM
   mono-ns {: t0:n :}
   begin
      TASK:PAUSE
      10 >MS TASK:WAIT-MS drop
      mono-ns t0 - LOCK-NS >
   until ;


: LOCKED-PUT ( n ptr u8 n ptr u8 n -- ) 2drop 2drop drop LOCK-WAIT ;
: LOCKED-RECORD ( n ptr u8 n -- n ) 2drop drop LOCK-WAIT 1 ;


\ A halt of the worker's chat while a locked callback waits ends it within
\ PROMPT-NS, the call landed E-CANCELLED; another task then ends and closes it.
: LOCK-HALTED ( -- )
   ['] CHAT-WORK WORK-TASK TASK:ACTIVATE
   LOCK-ENTERED TASK:WAIT-SEM
   mono-ns {: t0:n :}
   WORK-TASK TASK:HALT
   WORK-TASK TASK:JOIN MATCH result
      ok OF drop FAIL ENDOF
      err OF drop ENDOF
   ;MATCH
   mono-ns t0 - PROMPT-NS < ASSERT
   AWAIT-CODE MODEL:E-CANCELLED = ASSERT
   M MODEL:CLOSE ;


\ A halt while the journal put waits.
: PUT-HALT ( -- )
   0 LOCK-ENTERED TASK:SEMAPHORE-INIT
   0 [: CAPTURE-GET ;] [: LOCKED-PUT ;] M MODEL:JOURNAL!
   s" valid" PREP
   LOCK-HALTED
   LOCK-ENTERED TASK:SEMAPHORE-DESTROY ;


\ A halt while the recorder waits.
: RECORD-HALT ( -- )
   0 LOCK-ENTERED TASK:SEMAPHORE-INIT
   0 [: LOCKED-RECORD ;] [: TEST-REJECT ;] M MODEL:RECORDER!
   PURPOSE s" valid" PREP
   LOCK-HALTED
   LOCK-ENTERED TASK:SEMAPHORE-DESTROY ;


variable PAUSED-ROWS
: PAUSE-RECORD ( n ptr u8 n -- n ) 2drop drop TASK:PAUSE 1 PAUSED-ROWS +! 1 ;


\ A halt that came during the transfer ends the recorder at its first pause;
\ the call lands E-CANCELLED and unrecorded.
: PENDING-HALT ( -- )
   0 PAUSED-ROWS !
   0 [: PAUSE-RECORD ;] [: TEST-REJECT ;] M MODEL:RECORDER!
   PURPOSE
   HOLD
   mono-ns {: t0:n :}
   WORK-TASK TASK:HALT
   WORK-TASK TASK:JOIN MATCH result
      ok OF drop FAIL ENDOF
      err OF drop ENDOF
   ;MATCH
   mono-ns t0 - PROMPT-NS < ASSERT
   PAUSED-ROWS @ 0= ASSERT
   AWAIT-CODE MODEL:E-CANCELLED = ASSERT
   M MODEL:CLOSE ;


\ Every request dispatched to the backend is recorded once, served or failed,
\ under the purpose set for it; a call refused before dispatch is not. Its
\ journal record names the call's kind, configured model and the reply's usage
\ counts, null for each the reply omits, and no usage when the reply has none.
\ Generation rows share a prefix hash across user text; another system text
\ changes it. A recording fault fails a successful call after its reply is
\ journaled, so the same call then replays with no provider call, and it stays
\ behind the fault of a failed one.
: RECORDED-CALLS ( -- )
   CAPTURE
   TEST-RECORDER
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
   s" cache_write_tokens" ROW 3 INT-IS
   s" output_tokens" ROW 6 INT-IS
   s" reasoning_tokens" ROW 2 INT-IS
   s" prefix_hash" ROW HASH 64 JSON:READ-STR 64 = ASSERT JSON:READ-CLOSE
   RECORD$ s" call_id" MEMBER 1 INT-IS
   s" kind" USAGE s" generation" STR-IS
   s" model" USAGE s" test-generation" STR-IS
   s" input_tokens" USAGE 11 INT-IS
   s" cached_tokens" USAGE 4 INT-IS
   s" cache_write_tokens" USAGE 3 INT-IS
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
   s" cache_write_tokens" USAGE NULL-IS
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
   s" incomplete_reason" ROW s" max_output_tokens" STR-IS
   \ A reply whose vectors fail records its usage counts.
   s" embed" M MODEL:PURPOSE!
   s" bad-data" INPUTS
   EMBED-CODE MODEL:E-RESPONSE = ASSERT
   s" fault" ROW MODEL:E-RESPONSE INT-IS
   s" input_tokens" ROW 2 INT-IS ;


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
   TEST-RECORDER
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


\ Neither the last request nor its journal input names an output limit.
: NO-MAX-OUTPUT ( -- )
   MODEL-PEER:SENT$ READER s" max_output_tokens" JSON:READ-FIND-KEY 0= ASSERT JSON:READ-CLOSE
   INPUT$ s" configuration" MEMBER s" max_output_tokens" JSON:READ-FIND-KEY 0= ASSERT JSON:READ-CLOSE ;
\ The last request's output limit is the JSON number n.
: MAX-OUTPUT-IS ( n -- )
   {: n:n :}
   s" max_output_tokens" SENT JSON:READ-TOKEN JSON:T-INT = ASSERT
   JSON:READ-INT n = ASSERT JSON:READ-CLOSE ;
: NEGATIVE-MAX-OUTPUT ( n -- n ) -1 M MODEL:MAX-OUTPUT! ;


\ An output limit reaches every chat request and its journal input while set
\ and no embedding request. 0 sends none and a negative limit is refused.
: MAX-OUTPUTS ( -- )
   CAPTURE
   PURPOSE CHAT
   NO-MAX-OUTPUT
   9200 M MODEL:MAX-OUTPUT!
   PURPOSE CHAT
   9200 MAX-OUTPUT-IS
   s" max_output_tokens" CONFIG 9200 INT-IS
   PURPOSE CHAT
   9200 MAX-OUTPUT-IS
   s" embed" M MODEL:PURPOSE!
   s" one" INPUTS s" two" M MODEL:INPUT+ M MODEL:EMBED
   NO-MAX-OUTPUT
   0 M MODEL:MAX-OUTPUT!
   PURPOSE CHAT
   NO-MAX-OUTPUT
   0 [: NEGATIVE-MAX-OUTPUT ;] CODE-OF MODEL:E-CONFIG = ASSERT
   PURPOSE CHAT
   NO-MAX-OUTPUT ;


\ The last chat request names no service tier.
: NO-TIER ( -- )
   MODEL-PEER:SENT$ READER s" service_tier" JSON:READ-FIND-KEY 0= ASSERT JSON:READ-CLOSE ;


: NEGATIVE-TIER ( n -- n ) s" y" s" m" s" " s" " drop -1 M MODEL:PURPOSE-MODEL! ;


variable SENDS-AT    \ MODEL-PEER:CALLS@ at RESEND-MARK
variable ROWS-AT     \ ROWS-MADE at RESEND-MARK
: RESEND-MARK ( -- ) MODEL-PEER:CALLS@ SENDS-AT ! ROWS-MADE @ ROWS-AT ! ;
: SENDS-SINCE ( -- n ) MODEL-PEER:CALLS@ SENDS-AT @ - ;
: ROWS-SINCE ( -- n ) ROWS-MADE @ ROWS-AT @ - ;
\ A recorder that stops the call as it records.
: STOP-RECORD ( n ptr u8 n -- n ) TEST-RECORD M MODEL:REQUEST-STOP ;
\ A recorder that records one row and fails every one after it.
: ONCE-RECORD ( n ptr u8 n -- n ) TEST-RECORD true ROW-FAIL ! ;
variable LOOKUPS    \ COUNT-GET's lookups
\ A journal that never hits and counts its lookups.
: COUNT-GET ( n ptr u8 n ptr n -- bool ) CAPTURE-GET 1 LOOKUPS +! ;


\ Serves M, each SERVE answering false, until the peer has had n requests
\ since RESEND-MARK.
: SERVE-TO ( n -- )
   {: n:n :}
   begin SENDS-SINCE n < while
      M MODEL:SERVE 0= ASSERT
      SERVE-PAUSE
   repeat ;


\ With TIER-RESEND! a call that ends E-TIER at a tier it named is sent once
\ more naming none, each send recorded and looked up in the journal: CHAT and
\ AWAIT give the second send's reply, CALL-ID names its row, 0 when it has
\ none, and SERVE answers false while it is out, the tier the call's or a
\ purpose row's. A CANCEL then ends the call E-CANCELLED. A second E-TIER, a
\ failed recorder or a stopped call ends it; off, or with no tier named, E-TIER
\ ends the call after one request. It leaves the call open with the setting
\ off and its tier flex, recording as TIERS does.
: TIER-RESENDS ( -- )
   true M MODEL:TIER-RESEND!
   0 [: COUNT-GET ;] [: CAPTURE-PUT ;] M MODEL:JOURNAL!
   0 LOOKUPS !
   RESEND-MARK
   PURPOSE s" flex-then" PREP M MODEL:CHAT TRUE-CONTENT$ STR= ASSERT
   SENDS-SINCE 2 = ASSERT
   ROWS-SINCE 2 = ASSERT
   LOOKUPS @ 2 = ASSERT
   M MODEL:CALL-ID ROWS-MADE @ = ASSERT
   s" outcome" ROW s" ok" STR-IS
   s" requested_tier" ROW NULL-IS
   NO-TIER
   CAPTURE
   0 [: ONCE-RECORD ;] [: TEST-REJECT ;] M MODEL:RECORDER!
   RESEND-MARK
   PURPOSE s" flex-then" E-RECORDER FAILS
   SENDS-SINCE 2 = ASSERT
   ROWS-SINCE 1 = ASSERT
   M MODEL:CALL-ID 0= ASSERT
   M MODEL:RECORD-FAULT E-RECORDER = ASSERT
   false ROW-FAIL !
   TEST-RECORDER
   RESEND-MARK
   PURPOSE s" flex-gate" PREP M MODEL:START
   2 SERVE-TO
   MODEL-PEER:AWAIT-HOLD
   M MODEL:SERVE 0= ASSERT
   SENDS-SINCE 2 = ASSERT
   ROWS-SINCE 1 = ASSERT
   s" outcome" ROW s" tier_unavailable" STR-IS
   MODEL-PEER:LET-GO
   M MODEL:AWAIT
   M MODEL:CONTENT$ TRUE-CONTENT$ STR= ASSERT
   SENDS-SINCE 2 = ASSERT
   ROWS-SINCE 2 = ASSERT
   s" outcome" ROW s" ok" STR-IS
   RESEND-MARK
   PURPOSE s" flex-hold" PREP M MODEL:START
   2 SERVE-TO
   MODEL-PEER:AWAIT-HOLD
   ROWS-SINCE 1 = ASSERT
   s" outcome" ROW s" tier_unavailable" STR-IS
   M MODEL:CANCEL ASSERT
   AWAIT-CODE MODEL:E-CANCELLED = ASSERT
   SENDS-SINCE 2 = ASSERT
   ROWS-SINCE 2 = ASSERT
   s" outcome" ROW s" cancelled" STR-IS
   RESEND-MARK
   PURPOSE s" flex-out" MODEL:E-TIER FAILS
   SENDS-SINCE 2 = ASSERT
   s" outcome" ROW s" tier_unavailable" STR-IS
   s" requested_tier" ROW NULL-IS
   s" flexed" s" test-generation" s" " s" flex" M MODEL:PURPOSE-MODEL!
   s" " M MODEL:SERVICE-TIER!
   RESEND-MARK
   s" flexed" M MODEL:PURPOSE!
   s" flex-then" PREP M MODEL:CHAT TRUE-CONTENT$ STR= ASSERT
   SENDS-SINCE 2 = ASSERT
   ROWS-SINCE 2 = ASSERT
   s" outcome" ROW s" ok" STR-IS
   s" requested_tier" ROW NULL-IS
   s" purpose" ROW s" flexed" STR-IS
   NO-TIER
   RESEND-MARK
   PURPOSE s" flex-out" MODEL:E-TIER FAILS
   SENDS-SINCE 1 = ASSERT
   ROWS-SINCE 1 = ASSERT
   s" outcome" ROW s" tier_unavailable" STR-IS
   s" requested_tier" ROW NULL-IS
   s" flex" M MODEL:SERVICE-TIER!
   true ROW-FAIL !
   RESEND-MARK
   PURPOSE s" flex-then" MODEL:E-TIER FAILS
   SENDS-SINCE 1 = ASSERT
   M MODEL:RECORD-FAULT E-RECORDER = ASSERT
   false ROW-FAIL !
   false M MODEL:TIER-RESEND!
   RESEND-MARK
   PURPOSE s" flex-then" MODEL:E-TIER FAILS
   SENDS-SINCE 1 = ASSERT
   ROWS-SINCE 1 = ASSERT
   \ A stop stays with the call: CLOSE and SETUP give the rest of TIERS a new one.
   true M MODEL:TIER-RESEND!
   0 [: STOP-RECORD ;] [: TEST-REJECT ;] M MODEL:RECORDER!
   RESEND-MARK
   PURPOSE s" flex-then" MODEL:E-CANCELLED FAILS
   SENDS-SINCE 1 = ASSERT
   M MODEL:CLOSE
   SETUP
   CAPTURE
   TEST-RECORDER
   s" flex" M MODEL:SERVICE-TIER! ;


\ A service tier reaches a generation request only when set. The record
\ names the tier the reply names, NULL when it names none or is null, and the
\ tier a generation request sent, NULL when it sent none; an embedding sends
\ none whatever tier generation requests, and names none after a tiered reply. An
\ empty or non-string reply tier is refused. A tier with no room fails E-TIER
\ and records tier_unavailable. A purpose's row sets the tier of that
\ purpose's calls in place of the call's, an empty one sending none; a
\ negative tier length is refused.
: TIERS ( -- )
   CAPTURE
   TEST-RECORDER
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
   PURPOSE s" flex-out" MODEL:E-TIER FAILS
   M MODEL:ERROR$ s" Flex processing is temporarily unavailable. Please try again later or use standard processing." STR= ASSERT
   s" error" CHAT-ID-LINE
   s" MODEL:E-TIER status 200: Flex processing is temporarily unavailable. Please try again later or use standard processing. body: " SB-APPEND
   MODEL-PEER:FLEX-OUT$ SB-APPEND
   s" ; events response.created=1 response.in_progress=1 error=1; no output" SB-APPEND
   SB-LOGGED
   s" outcome" ROW s" tier_unavailable" STR-IS
   s" fault" ROW MODEL:E-TIER INT-IS
   s" http_status" ROW 200 INT-IS
   s" requested_tier" ROW s" flex" STR-IS
   s" service_tier" ROW NULL-IS
   TIER-RESENDS
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


\ Keeps only the journaled record's configuration and an incomplete refusal,
\ a record with no reply and no call id.
: BARE-REFUSAL ( -- )
   RECORD$ s" configuration" MEMBER
   JSON:READ-TOKEN JSON:T-OBJ = ASSERT
   JSON:READ-VALUE-SPAN$ {: a:ptr u:n :}
   SB-RESET S\" {\qconfiguration\q:" SB-APPEND a u SB-APPEND
   JSON:READ-CLOSE
   S\" ,\qrefusal\q:\qincomplete\q}" SB-APPEND
   SB$ JOURNAL-RECORD 65536 SPAN:MAKE SPAN:COPY
   SB$ nip JOURNAL-RECORD-U ! ;


\ A journal hit, here of a journal input past 16 KiB, returns the stored
\ content; it sends nothing and records nothing, and restores the call id its
\ live call recorded, so a refusal of the replayed reply marks that row. A
\ service tier set after the live call is not journal input, so the call
\ still replays.
: REPLAYED-CALLS ( -- )
   TEST-RECORDER
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
   \ START lands a journal hit at once: no request, no row, the live call's id.
   PURPOSE JOURNAL-TEXT JOURNAL-U PREP M MODEL:START
   M MODEL:CANCEL 0= ASSERT
   M MODEL:AWAIT
   MODEL-PEER:CALLS@ calls = ASSERT
   ROWS-MADE @ rows = ASSERT
   M MODEL:CONTENT$ TRUE-CONTENT$ STR= ASSERT
   M MODEL:CALL-ID id = ASSERT
   \ A refusal its live call journaled replays with no request and no line.
   false JOURNAL-READY !
   PURPOSE s" too-large" MODEL:E-CAPACITY FAILS
   MODEL-PEER:CALLS@ {: refused:n :}
   [: PURPOSE s" too-large" MODEL:E-CAPACITY FAILS ;] QUIET
   MODEL-PEER:CALLS@ refused = ASSERT
   \ A cut reply's record holds the provider's reply as received, the text
   \ streamed before the cut included, and the call's id. It replays as the
   \ cut, and so does a record holding neither.
   false JOURNAL-READY !
   PURPOSE s" length" MODEL:E-INCOMPLETE FAILS
   M MODEL:CALL-ID {: cut:n :}
   cut 0<> ASSERT
   RECORD$ s" refusal" MEMBER s" incomplete" STR-IS
   RECORD$ s" response" MEMBER MODEL-PEER:CUT$ STR-IS
   RECORD$ s" call_id" MEMBER cut INT-IS
   MODEL-PEER:CALLS@ {: sent:n :}
   [: PURPOSE s" length" MODEL:E-INCOMPLETE FAILS ;] QUIET
   BARE-REFUSAL
   [: PURPOSE s" length" MODEL:E-INCOMPLETE FAILS ;] QUIET
   MODEL-PEER:CALLS@ sent = ASSERT
   s" " M MODEL:SERVICE-TIER! ;


: FAILING-PUT ( n ptr u8 n ptr u8 n -- ) 2drop 2drop drop E-RECORDER throw ;


\ A journal put that throws while journaling a refusal is the call's fault.
: REFUSAL-PUT-FAULT ( -- )
   false JOURNAL-READY !
   0 [: JOURNAL-GET ;] [: FAILING-PUT ;] M MODEL:JOURNAL!
   PURPOSE s" too-large" E-RECORDER FAILS ;


create ASKED 32768 allot    \ the journal input JOURNAL-INPUT$ last answered
variable ASKED-U
: ASKED$ ( -- ptr u8 n ) ASKED ASKED-U @ ;
\ Finishes the request for `text` and keeps the journal input it answers.
: ASK ( ptr u8 n -- )
   PREP M MODEL:JOURNAL-INPUT$ {: a:ptr u:n :}
   a u ASKED 32768 SPAN:MAKE SPAN:COPY u ASKED-U ! ;
: REPLAYED ( ptr MODEL:call -- ptr MODEL:call ) {: c :} RECORD$ c MODEL:REPLAY 2drop c ;
: REPLAY-CODE ( -- n ) M [: REPLAYED ;] catch {: code:n :} drop code ;
: ADD-TEXT ( ptr MODEL:call -- ptr MODEL:call ) {: c :} s" x" c MODEL:TEXT+ c ;


\ A caller that looks its journal up itself reads the journal input of the
\ finished request with no request sent, then either sends exactly that
\ request with CHAT or restores a saved record with REPLAY, which sends and
\ records nothing and restores the call id the record was saved from. The
\ pending purpose's row reaches that input. REPLAY needs a request
\ JOURNAL-INPUT$ finished and not yet consumed or discarded by a builder.
: CALLER-JOURNAL ( -- )
   CAPTURE
   TEST-RECORDER
   MODEL-PEER:CALLS@ {: before:n :}
   PURPOSE s" valid" ASK
   MODEL-PEER:CALLS@ before = ASSERT
   M MODEL:JOURNAL-INPUT$ ASKED$ STR= ASSERT
   M MODEL:CHAT TRUE-CONTENT$ STR= ASSERT
   MODEL-PEER:CALLS@ before 1+ = ASSERT
   INPUT$ ASKED$ STR= ASSERT
   M MODEL:CALL-ID {: id:n :}
   ROWS-MADE @ {: rows:n :}
   MODEL-PEER:CALLS@ {: calls:n :}
   PURPOSE s" valid" ASK
   RECORD$ M MODEL:REPLAY TRUE-CONTENT$ STR= ASSERT
   MODEL-PEER:CALLS@ calls = ASSERT
   ROWS-MADE @ rows = ASSERT
   M MODEL:CALL-ID id = ASSERT
   M [: dup MODEL:JOURNAL-INPUT$ 2drop ;] catch {: code:n :} drop code MODEL:E-STATE = ASSERT
   PURPOSE s" valid" PREP
   REPLAY-CODE MODEL:E-STATE = ASSERT
   PURPOSE s" valid" ASK
   M [: ADD-TEXT ;] catch {: code:n :} drop code MODEL:E-STATE = ASSERT
   REPLAY-CODE MODEL:E-STATE = ASSERT
   s" reason" s" test-generation" s" low" s" " M MODEL:PURPOSE-MODEL!
   s" reason" M MODEL:PURPOSE! s" valid" ASK
   ASKED$ s" configuration" MEMBER s" effort" JSON:READ-FIND-KEY ASSERT s" low" STR-IS
   M MODEL:CHAT TRUE-CONTENT$ STR= ASSERT
   s" effort" ROW s" low" STR-IS
   \ A saved refusal replays as the refusal, with no request and no line, and
   \ keeps the request, which JOURNAL-INPUT$ answers again.
   PURPOSE s" too-large" MODEL:E-CAPACITY FAILS
   MODEL-PEER:CALLS@ {: refused:n :}
   PURPOSE s" too-large" ASK
   [: REPLAY-CODE MODEL:E-CAPACITY = ASSERT ;] QUIET
   MODEL-PEER:CALLS@ refused = ASSERT
   PURPOSE M MODEL:JOURNAL-INPUT$ ASKED$ STR= ASSERT ;


create GOT 32768 allot    \ the journal input KEEP-GET last received
variable GOT-U
variable GOT-NS           \ TIME:MONO-NS as KEEP-GET last ran
: GOT$ ( -- ptr u8 n ) GOT GOT-U @ ;
\ A journal getter that never hits and keeps the input it receives and when.
: KEEP-GET ( n ptr u8 n ptr n -- bool )
   {: ctx:n input:ptr input-u:n out:ptr :}
   input-u 32768 <= ASSERT
   input input-u GOT 32768 SPAN:MAKE SPAN:COPY input-u GOT-U !
   TIME:MONO-NS GOT-NS !
   false ;


\ A recorded, journaled chat that faults records a failed row; PURPOSE! and
\ CHAT then send the same body, which the journal looks up and puts under the
\ same input it looked the first try up under, and record an ok row.
: RECORDED-RESEND ( -- )
   TEST-RECORDER
   0 [: KEEP-GET ;] [: CAPTURE-PUT ;] M MODEL:JOURNAL!
   ROWS-MADE @ {: rows:n :}
   MODEL-PEER:CALLS@ {: calls:n :}
   PURPOSE s" valid" ASK
   s" http-error" MODEL-PEER:FAIL-NEXT
   CHAT-CODE MODEL:E-UNAVAILABLE = ASSERT
   MODEL-PEER:CALLS@ calls 1+ = ASSERT
   ROWS-MADE @ rows 1+ = ASSERT
   s" purpose" ROW s" assemble" STR-IS
   s" outcome" ROW s" failed" STR-IS
   GOT$ ASKED$ STR= ASSERT
   KEEP-SENT
   0 GOT-U ! 0 JOURNAL-INPUT-U !
   PURPOSE M MODEL:CHAT TRUE-CONTENT$ STR= ASSERT
   MODEL-PEER:CALLS@ calls 2 + = ASSERT
   ROWS-MADE @ rows 2 + = ASSERT
   s" purpose" ROW s" assemble" STR-IS
   s" outcome" ROW s" ok" STR-IS
   GOT$ ASKED$ STR= ASSERT
   INPUT$ ASKED$ STR= ASSERT
   SENT-AGAIN ;


\ Serves M, each SERVE answering false, until the call rests.
: SERVE-TO-REST ( -- )
   begin M MODEL:RESEND-DUE 0= while
      M MODEL:SERVE 0= ASSERT
      SERVE-PAUSE
   repeat ;


variable WAKE-MS    \ how long CANCEL-WORK and STOP-WORK-LATER sleep first
variable ACTED-NS   \ mono-ns as CANCEL-WORK or STOP-WORK-LATER acted
\ Cancels M from another task WAKE-MS after it starts.
: CANCEL-WORK ( -- )
   WAKE-MS @ >MS TASK:SLEEP
   mono-ns ACTED-NS !
   M MODEL:CANCEL bool>n TASK:RETURN ;


\ Takes every hint pending on this task's park.
: DRAIN-HINTS ( -- ) begin 0 >MS TASK:WAIT-MS 0= until ;


\ AWAITs M, which lands E-CANCELLED at once after the work task acts, and
\ joins the task, its answer passing the test. Answers whether the AWAIT's
\ park took the wake, which then leaves no hint pending on this task.
: AWAIT-WOKEN ( [ n -- bool ] -- bool )
   {: test :}
   AWAIT-CODE MODEL:E-CANCELLED = ASSERT
   mono-ns {: t1:n :}
   WORK-TASK TASK:JOIN MATCH result
      ok OF test execute ASSERT ENDOF
      err OF drop FAIL ENDOF
   ;MATCH
   t1 ACTED-NS @ - PROMPT-NS < ASSERT
   0 >MS TASK:WAIT-MS 0= ;


\ The delay of wake try k: 50, 200, then 800 ms.
: TRY-MS ( n -- n ) 50 swap 0 ?do 4 * loop ;


\ Runs a wake scenario at each try's delay until its AWAIT's park takes the
\ wake, and answers what that try answered; fails when no try's does.
: WAKE-TRIES ( [ n -- n n bool ] -- n n )
   {: try :}
   3 0 do
      i TRY-MS try execute if unloop exit then 2drop
   loop
   FAIL 0 0 ;


variable MATCHES   \ COUNT-BY's count
\ How many captured lines, after their stamps, pass the test against SB.
: COUNT-BY ( [ ptr u8 n ptr u8 n -- bool ] -- n )
   {: pred :}
   0 MATCHES !
   LOG$ {: a:ptr u:n :}
   0 u 0 ?do
      a i + c@ LF = if
         i over - over a swap + swap SB$ pred execute if 1 MATCHES +! then
         drop i 1+
      then
   loop drop MATCHES @ ;


\ How many captured lines are exactly SB's text.
: SB-COUNT ( -- n ) [: LINE? ;] COUNT-BY ;
\ How many are SB's text, a count of ms since CHAT-NS and " ms".
: MS-COUNT ( -- n ) [: MS-LINE? ;] COUNT-BY ;


\ A count now k more than `same`, and LINES k more than `before`.
: NEW-LINES ( n n n n -- )
   {: now:n same:n before:n k:n :}
   LINES before k + = now same k + = and
   dup 0= if SB$ type cr LOG$ type then ASSERT ;


\ SB-COUNT is one more than `same` and LINES one more than `before`.
: ONE-LINE ( n n -- )
   {: same:n before:n :}
   SB-COUNT same before 1 NEW-LINES ;


: NOTE$ ( -- ptr u8 n ) s" job 9 assemble" ;


\ SB holds a chat line at the level naming NOTE$, then the text.
: NOTED ( ptr u8 n ptr u8 n -- )
   {: level:ptr level-u:n tail:ptr tail-u:n :}
   level level-u CHAT-LINE NOTE$ SB-APPEND s" : " SB-APPEND
   tail tail-u SB-APPEND ;


\ NOTED for a reply that named its response id.
: ID-NOTED ( ptr u8 n ptr u8 n -- )
   {: level:ptr level-u:n tail:ptr tail-u:n :}
   level level-u CHAT-ID-LINE NOTE$ SB-APPEND s" : " SB-APPEND
   tail tail-u SB-APPEND ;


\ Starts M's chat, which the provider answers 503: it is recorded failed and
\ rests started, SERVE answering false and sending nothing, and the work
\ task CANCELs it after ms. Answers SB-COUNT and LINES from before the start
\ and whether the AWAIT's park took the CANCEL's wake.
: CANCELLED-REST ( n -- n n bool )
   WAKE-MS !
   SB-COUNT LINES
   DRAIN-HINTS
   RESEND-MARK
   s" http-busy" MODEL-PEER:FAIL-NEXT
   PURPOSE s" valid" PREP M MODEL:START
   SERVE-TO-REST
   SENDS-SINCE 1 = ASSERT
   ROWS-SINCE 1 = ASSERT
   s" outcome" ROW s" failed" STR-IS
   M MODEL:RESEND-DUE {: due:n :}
   M MODEL:SERVE 0= ASSERT
   M MODEL:RESEND-DUE due = ASSERT
   SENDS-SINCE 1 = ASSERT
   ['] CANCEL-WORK WORK-TASK TASK:ACTIVATE
   [: 0<> ;] AWAIT-WOKEN ;


\ With RESEND-WAIT! a chat the provider answers 503 is recorded failed, its
\ one line names the note and the wait, and it rests started: SERVE answers
\ false and sends nothing until it is due, and a CANCEL from another task
\ wakes the AWAIT parked on it, which ends at once with a cancelled record
\ and no line. A CANCEL before the park leaves its wake pending on the
\ starter, so the case cancels later until the park takes the wake.
: RESTS-CANCELLED ( -- )
   60000 M MODEL:RESEND-WAIT!
   s" warn" s" MODEL:E-UNAVAILABLE status 503: overloaded; sent again in 60 s"
   NOTED
   [: CANCELLED-REST ;] WAKE-TRIES {: same:n before:n :}
   SENDS-SINCE 1 = ASSERT
   ROWS-SINCE 2 = ASSERT
   s" outcome" ROW s" cancelled" STR-IS
   M MODEL:RESEND-DUE 0= ASSERT
   same before ONE-LINE ;


\ With a 1 s wait, whose line names 1 s, an AWAIT entered before the due
\ time waits for it: the resend's journal lookup, which comes before its
\ dispatch, runs at or after it. AWAIT gives the reply to the same body, its
\ flex tier included, looked up and put under the same journal input.
: RESTS-AWAITED ( -- )
   s" flex" M MODEL:SERVICE-TIER!
   1000 M MODEL:RESEND-WAIT!
   s" warn" s" MODEL:E-UNAVAILABLE status 503: overloaded; sent again in 1 s"
   NOTED
   SB-COUNT LINES {: same:n before:n :}
   RESEND-MARK
   s" http-busy" MODEL-PEER:FAIL-NEXT
   PURPOSE s" valid" ASK
   M MODEL:START
   SERVE-TO-REST
   same before ONE-LINE
   GOT$ ASKED$ STR= ASSERT
   KEEP-SENT
   0 GOT-U ! 0 GOT-NS ! 0 JOURNAL-INPUT-U !
   M MODEL:RESEND-DUE {: due:n :}
   mono-ns due < ASSERT
   M MODEL:AWAIT
   GOT-NS @ due >= ASSERT
   M MODEL:CONTENT$ TRUE-CONTENT$ STR= ASSERT
   SENDS-SINCE 2 = ASSERT
   ROWS-SINCE 2 = ASSERT
   s" outcome" ROW s" ok" STR-IS
   s" purpose" ROW s" assemble" STR-IS
   SENT-AGAIN
   GOT$ ASKED$ STR= ASSERT
   INPUT$ ASKED$ STR= ASSERT
   same before ONE-LINE
   s" " M MODEL:SERVICE-TIER! ;


\ With RESEND-WAIT! off the 503 ends the call, with its one error line.
: RESTS-OFF ( -- )
   0 M MODEL:RESEND-WAIT!
   s" error" s" MODEL:E-UNAVAILABLE status 503: overloaded" NOTED
   SB-COUNT LINES {: same:n before:n :}
   RESEND-MARK
   s" http-busy" MODEL-PEER:FAIL-NEXT
   PURPOSE s" valid" MODEL:E-UNAVAILABLE FAILS
   SENDS-SINCE 1 = ASSERT
   ROWS-SINCE 1 = ASSERT
   s" outcome" ROW s" failed" STR-IS
   same before ONE-LINE
   M MODEL:RESEND-DUE 0= ASSERT ;


: RESTS ( -- )
   TEST-RECORDER
   0 [: KEEP-GET ;] [: CAPTURE-PUT ;] M MODEL:JOURNAL!
   NOTE$ M MODEL:LOG-NOTE!
   RESTS-CANCELLED RESTS-AWAITED RESTS-OFF
   s" " M MODEL:LOG-NOTE! ;


variable LANDED-CANCELLED   \ CANCEL-ROW's CANCEL answer
\ A recorder that records the row, then CANCELs M after the first row since
\ RESEND-MARK: it runs in ACCOUNT, after the send has landed and before the
\ tier and resend machines sense it.
: CANCEL-ROW ( n ptr u8 n -- n )
   TEST-RECORD
   ROWS-SINCE 1 = if M MODEL:CANCEL LANDED-CANCELLED ! then ;


\ Starts the chat PREP built and serves it to AWAIT, M's recorder CANCEL-ROW;
\ the AWAIT's fault, at once.
: CANCELLED-LANDING ( -- n )
   0 [: CANCEL-ROW ;] [: TEST-REJECT ;] M MODEL:RECORDER!
   false LANDED-CANCELLED !
   M MODEL:START
   mono-ns {: t0:n :}
   AWAIT-CODE
   mono-ns t0 - PROMPT-NS < ASSERT
   TEST-RECORDER ;


\ A CANCEL of a send that landed with a 503, RESEND-WAIT! on, before the call
\ rests answers true and keeps it from resting: AWAIT throws E-CANCELLED at
\ once, the failed row followed by a cancelled one, nothing is sent again,
\ and the send's failure line, naming the note, is written once. With
\ RESEND-WAIT! off the send would not be sent again: the CANCEL answers false
\ and AWAIT throws the send's own fault, with the same one line.
: LANDED-CANCEL ( -- )
   NOTE$ M MODEL:LOG-NOTE!
   s" error" s" MODEL:E-UNAVAILABLE status 503: overloaded" NOTED
   60000 M MODEL:RESEND-WAIT!
   SB-COUNT LINES {: same:n before:n :}
   RESEND-MARK
   s" http-busy" MODEL-PEER:FAIL-NEXT
   PURPOSE s" valid" PREP CANCELLED-LANDING MODEL:E-CANCELLED = ASSERT
   LANDED-CANCELLED @ 0<> ASSERT
   SENDS-SINCE 1 = ASSERT
   ROWS-SINCE 2 = ASSERT
   s" outcome" ROW s" cancelled" STR-IS
   M MODEL:RESEND-DUE 0= ASSERT
   same before ONE-LINE
   0 M MODEL:RESEND-WAIT!
   SB-COUNT LINES {: again:n after:n :}
   RESEND-MARK
   s" http-busy" MODEL-PEER:FAIL-NEXT
   PURPOSE s" valid" PREP CANCELLED-LANDING MODEL:E-UNAVAILABLE = ASSERT
   LANDED-CANCELLED @ 0= ASSERT
   SENDS-SINCE 1 = ASSERT
   ROWS-SINCE 1 = ASSERT
   s" outcome" ROW s" failed" STR-IS
   again after ONE-LINE
   s" " M MODEL:LOG-NOTE! ;


\ With RESEND-WAIT! a send whose key read the store refuses sends nothing and
\ rests: its one line names the note, the key's service, account and reason
\ and the wait. Sent again when due and refused the same way, it writes one
\ more such line and no failure line; once the store answers, the resend is
\ served.
: KEY-RESTS ( -- )
   TEST-RECORDER
   1 M MODEL:RESEND-WAIT!
   NOTE$ M MODEL:LOG-NOTE!
   SERVICE$ ACCOUNT$ M MODEL:API-KEY!
   KEY$ STORE
   s" warn" s" SECRET:E-UNAVAILABLE key service " NOTED
   SERVICE$ SB-APPEND s" , account " SB-APPEND ACCOUNT$ SB-APPEND
   s" : store unavailable; sent again in 1 s" SB-APPEND
   SB-COUNT LINES {: same:n before:n :}
   SECRET:E-UNAVAILABLE READ-FAULT!
   RESEND-MARK
   PURPOSE s" valid" PREP M MODEL:START
   M MODEL:RESEND-DUE {: due:n :}
   due 0<> ASSERT
   same before ONE-LINE
   begin mono-ns due < while 1 >MS TASK:SLEEP repeat
   M MODEL:SERVE 0= ASSERT
   M MODEL:RESEND-DUE {: next:n :}
   next 0<> next due <> and ASSERT
   SB-COUNT same before 2 NEW-LINES
   SENDS-SINCE 0= ASSERT
   ROWS-SINCE 0= ASSERT
   0 READ-FAULT!
   M MODEL:AWAIT
   M MODEL:CONTENT$ TRUE-CONTENT$ STR= ASSERT
   SENDS-SINCE 1 = ASSERT
   ROWS-SINCE 1 = ASSERT
   LINES before 2 + = ASSERT
   0 M MODEL:RESEND-WAIT!
   s" " M MODEL:LOG-NOTE!
   FIXTURE-KEY ;


\ A note longer than the 64 KiB a pipe holds on macOS and Linux, so a line
\ that carries it is written into a pipe nobody drains only in part.
262144 constant LONG-U
create LONG-NOTE LONG-U allot
\ How many of the note's bytes in a row CUT-REST-WORK reads before it CANCELs.
1024 constant NOTE-RUN
create PIPE-BYTE 1 allot
create PIPE-CHUNK 4096 allot
variable PIPE-R      \ the read end of the pipe fd 2 writes to
variable SAVED-ERR   \ fd 2's capture file while the pipe holds fd 2

: PIPE-IN ( -- fd ) PIPE-R @ >FD ;


\ Sends fd 2 into a new pipe, keeping its capture file aside.
: ERR-TO-PIPE ( -- )
   FD:PIPE-PAIR {: r:fd w:fd :}
   r FD>N PIPE-R !
   FD:STDERR FD>N 0 10 fcntl {: saved:n :}
   saved 0 >= ASSERT
   saved SAVED-ERR !
   w FD>N FD:STDERR FD>N dup2 0 >= ASSERT
   w FD>N close-rc 0= ASSERT ;


\ Gives fd 2 its capture file back, which ends the pipe for its reader.
: ERR-BACK ( -- )
   SAVED-ERR @ FD:STDERR FD>N dup2 0 >= ASSERT
   SAVED-ERR @ close-rc 0= ASSERT ;


: PIPE-BYTE@ ( -- n )
   PIPE-IN PIPE-BYTE 1 SPAN:MAKE FD:READ-EXACT MATCH FD:fill
      full OF ENDOF
      eof OF drop FAIL ENDOF
   ;MATCH
   PIPE-BYTE c@ ;


\ Reads the pipe until NOTE-RUN of the note's bytes in a row: the line that
\ carries it is then being written, held until the pipe is drained.
: NOTE-REACHED ( -- )
   0 begin
      PIPE-BYTE@ [char] x = if 1+ else drop 0 then
      dup NOTE-RUN >=
   until drop ;


\ Reads the pipe to its end.
: PIPE-DRAIN ( -- )
   begin
      PIPE-IN PIPE-CHUNK 4096 SPAN:MAKE FD:READ-EXACT MATCH FD:fill
         full OF false ENDOF
         eof OF drop true ENDOF
      ;MATCH
   until ;


\ CANCELs M while its starter writes the rest line, then drains the pipe.
: CUT-REST-WORK ( -- )
   NOTE-REACHED
   M MODEL:CANCEL bool>n {: answer:n :}
   PIPE-DRAIN
   answer TASK:RETURN ;


\ Serves M until it lands or rests; answers whether it landed.
: SERVE-SETTLED ( -- bool )
   begin M MODEL:SERVE 0= while
      M MODEL:RESEND-DUE 0<> if false exit then
      SERVE-PAUSE
   repeat true ;


\ A CANCEL after the 503 send was sensed for a rest, and before the rest's
\ start was published, lands the call E-CANCELLED in that SERVE, so a caller
\ never parks on it. The CANCEL comes while the starter writes the rest line,
\ its note longer than the pipe fd 2 then goes to; MODEL runs no callback in
\ that window.
: CUT-REST ( -- )
   TEST-RECORDER
   LONG-NOTE LONG-U XS
   60000 M MODEL:RESEND-WAIT!
   LONG-NOTE LONG-U M MODEL:LOG-NOTE!
   RESEND-MARK
   s" http-busy" MODEL-PEER:FAIL-NEXT
   PURPOSE s" valid" PREP M MODEL:START
   ERR-TO-PIPE
   ['] CUT-REST-WORK WORK-TASK TASK:ACTIVATE
   SERVE-SETTLED {: landed:bool :}
   ERR-BACK
   WORK-TASK TASK:JOIN MATCH result
      ok OF 0<> ASSERT ENDOF
      err OF drop FAIL ENDOF
   ;MATCH
   PIPE-R @ close-rc 0= ASSERT
   landed ASSERT
   AWAIT-CODE MODEL:E-CANCELLED = ASSERT
   SENDS-SINCE 1 = ASSERT
   ROWS-SINCE 2 = ASSERT
   s" outcome" ROW s" cancelled" STR-IS
   M MODEL:RESEND-DUE 0= ASSERT
   s" " M MODEL:LOG-NOTE!
   0 M MODEL:RESEND-WAIT! ;


\ Starts M's chat, serves it until it rests, then waits for STOP-GO and
\ AWAITs it.
: REST-AWAIT-WORK ( -- )
   PURPOSE s" valid" PREP M MODEL:START
   SERVE-TO-REST
   STOP-READY TASK:SIGNAL-SEM
   STOP-GO TASK:WAIT-SEM
   M MODEL:AWAIT
   0 TASK:RETURN ;


\ CANCELs and AWAITs M on the task that started it; returns AWAIT's fault.
: CANCEL-AWAIT-WORK ( -- )
   M MODEL:CANCEL drop
   AWAIT-CODE TASK:RETURN ;


\ A halt of a resting call's starter, pending as it AWAITs the due rest, is
\ served before the resend begins: nothing is looked up or sent and the call
\ stays resting, until its starter CANCELs and AWAITs it E-CANCELLED.
: HALT-AT-DUE ( -- )
   TEST-RECORDER
   0 [: KEEP-GET ;] [: CAPTURE-PUT ;] M MODEL:JOURNAL!
   1 M MODEL:RESEND-WAIT!
   0 STOP-READY TASK:SEMAPHORE-INIT
   0 STOP-GO TASK:SEMAPHORE-INIT
   RESEND-MARK
   s" http-busy" MODEL-PEER:FAIL-NEXT
   ['] REST-AWAIT-WORK WORK-TASK TASK:ACTIVATE
   STOP-READY TASK:WAIT-SEM
   M MODEL:RESEND-DUE {: due:n :}
   due 0<> ASSERT
   0 GOT-U !
   begin mono-ns due < while 1 >MS TASK:SLEEP repeat
   WORK-TASK TASK:HALT
   STOP-GO TASK:SIGNAL-SEM
   WORK-TASK TASK:JOIN MATCH result
      ok OF drop FAIL ENDOF
      err OF drop ENDOF
   ;MATCH
   M MODEL:RESEND-DUE due = ASSERT
   GOT-U @ 0= ASSERT
   SENDS-SINCE 1 = ASSERT
   ROWS-SINCE 1 = ASSERT
   ['] CANCEL-AWAIT-WORK WORK-TASK TASK:ACTIVATE
   WORK-TASK TASK:JOIN MATCH result
      ok OF MODEL:E-CANCELLED = ASSERT ENDOF
      err OF drop FAIL ENDOF
   ;MATCH
   SENDS-SINCE 1 = ASSERT
   ROWS-SINCE 2 = ASSERT
   s" outcome" ROW s" cancelled" STR-IS
   0 M MODEL:RESEND-WAIT!
   STOP-READY TASK:SEMAPHORE-DESTROY
   STOP-GO TASK:SEMAPHORE-DESTROY ;


\ A CANCEL of a resting call answers true and SERVE lands it E-CANCELLED; a
\ second CANCEL then answers true too, and records nothing more.
: CANCELLED-TWICE ( -- )
   TEST-RECORDER
   60000 M MODEL:RESEND-WAIT!
   RESEND-MARK
   s" http-busy" MODEL-PEER:FAIL-NEXT
   PURPOSE s" valid" PREP M MODEL:START
   SERVE-TO-REST
   M MODEL:CANCEL ASSERT
   M MODEL:SERVE ASSERT
   M MODEL:CANCEL ASSERT
   AWAIT-CODE MODEL:E-CANCELLED = ASSERT
   SENDS-SINCE 1 = ASSERT
   ROWS-SINCE 2 = ASSERT
   s" outcome" ROW s" cancelled" STR-IS
   0 M MODEL:RESEND-WAIT! ;


: UNAVAILABLE-GET ( n ptr u8 n ptr n -- bool )
   2drop 2drop SECRET:E-UNAVAILABLE throw ;


\ With RESEND-WAIT! on, a journal lookup that throws SECRET:E-UNAVAILABLE
\ sends nothing, does not rest and writes no line: AWAIT throws the fault.
: LOOKUP-FAULT ( -- )
   0 [: UNAVAILABLE-GET ;] [: CAPTURE-PUT ;] M MODEL:JOURNAL!
   60000 M MODEL:RESEND-WAIT!
   NOTE$ M MODEL:LOG-NOTE!
   LINES {: before:n :}
   RESEND-MARK
   PURPOSE s" valid" PREP M MODEL:START
   M MODEL:RESEND-DUE 0= ASSERT
   M MODEL:SERVE ASSERT
   AWAIT-CODE SECRET:E-UNAVAILABLE = ASSERT
   SENDS-SINCE 0= ASSERT
   LINES before = ASSERT
   0 M MODEL:RESEND-WAIT!
   s" " M MODEL:LOG-NOTE!
   0 [: CAPTURE-GET ;] [: CAPTURE-PUT ;] M MODEL:JOURNAL! ;


: UNAVAILABLE-PUT ( n ptr u8 n ptr u8 n -- )
   2drop 2drop drop SECRET:E-UNAVAILABLE throw ;


\ With RESEND-WAIT! on, a served send whose journal put throws
\ SECRET:E-UNAVAILABLE does not rest and is not sent again: AWAIT throws the
\ put's fault.
: PUT-FAULT ( -- )
   0 [: CAPTURE-GET ;] [: UNAVAILABLE-PUT ;] M MODEL:JOURNAL!
   60000 M MODEL:RESEND-WAIT!
   RESEND-MARK
   PURPOSE s" valid" PREP M MODEL:START
   SERVE-SETTLED ASSERT
   M MODEL:RESEND-DUE 0= ASSERT
   AWAIT-CODE SECRET:E-UNAVAILABLE = ASSERT
   SENDS-SINCE 1 = ASSERT
   0 M MODEL:RESEND-WAIT!
   0 [: CAPTURE-GET ;] [: CAPTURE-PUT ;] M MODEL:JOURNAL! ;


variable GET-CANCELLED   \ CANCEL-GET's CANCEL answer
\ A journal getter that CANCELs M, started with nothing sent, and misses.
: CANCEL-GET ( n ptr u8 n ptr n -- bool )
   2drop 2drop M MODEL:CANCEL GET-CANCELLED ! false ;


\ A CANCEL before a send's failure lands keeps the failure's one line: the
\ journal lookup CANCELs the call, then its key read finds no key. AWAIT
\ throws E-CANCELLED, and the line names the note, the code and the key.
: CUT-KEY ( -- )
   0 [: CANCEL-GET ;] [: CAPTURE-PUT ;] M MODEL:JOURNAL!
   SERVICE$ s" missing" M MODEL:API-KEY!
   NOTE$ M MODEL:LOG-NOTE!
   s" error" CHAT-LINE NOTE$ SB-APPEND
   s" : MODEL:E-KEY key service " SB-APPEND SERVICE$ SB-APPEND
   s" , account missing: not found" SB-APPEND
   SB-COUNT LINES {: same:n before:n :}
   false GET-CANCELLED !
   RESEND-MARK
   PURPOSE s" valid" PREP CHAT-CODE MODEL:E-CANCELLED = ASSERT
   GET-CANCELLED @ 0<> ASSERT
   SENDS-SINCE 0= ASSERT
   same before ONE-LINE
   s" " M MODEL:LOG-NOTE!
   FIXTURE-KEY
   0 [: CAPTURE-GET ;] [: CAPTURE-PUT ;] M MODEL:JOURNAL! ;


\ A key the store reads and the provider refuses (401) is E-KEY, and its one
\ line names the key's service and account and that the provider refused it.
: PROVIDER-KEY ( -- )
   SERVICE$ ACCOUNT$ M MODEL:API-KEY!
   s" refused-key" STORE
   NOTE$ M MODEL:LOG-NOTE!
   s" error" CHAT-LINE NOTE$ SB-APPEND
   s" : MODEL:E-KEY key service " SB-APPEND SERVICE$ SB-APPEND
   s" , account " SB-APPEND ACCOUNT$ SB-APPEND
   s" : refused by the provider status 401" SB-APPEND
   SB-COUNT LINES {: same:n before:n :}
   PURPOSE s" valid" MODEL:E-KEY FAILS
   M MODEL:HTTP-STATUS 401 = ASSERT
   same before ONE-LINE
   s" " M MODEL:LOG-NOTE!
   KEY$ STORE
   FIXTURE-KEY ;


\ Starts M's chat with a 60 s RESEND-WAIT!. It rests on its first send, with
\ `sends` requests since RESEND-MARK and one line, SB's text then the wait;
\ a CANCEL then lands it E-CANCELLED with no more lines.
: RESTS-ON ( n -- )
   {: sends:n :}
   s" ; sent again in 60 s" SB-APPEND
   SB-COUNT LINES {: same:n before:n :}
   RESEND-MARK
   PURPOSE s" valid" PREP M MODEL:START
   SERVE-SETTLED 0= ASSERT
   M MODEL:RESEND-DUE 0<> ASSERT
   SENDS-SINCE sends = ASSERT
   same before ONE-LINE
   M MODEL:CANCEL ASSERT
   M MODEL:SERVE ASSERT
   AWAIT-CODE MODEL:E-CANCELLED = ASSERT
   same before ONE-LINE ;


\ Starts M's chat of the text with a 60 s RESEND-WAIT!. Its one send lands
\ with the code and does not rest; its one line passes the counter's test
\ against SB.
: SETTLES-ON ( ptr u8 n n [ -- n ] -- )
   {: a:ptr u:n code:n counted :}
   TIME:MONO-NS CHAT-NS !
   counted execute LINES {: same:n before:n :}
   RESEND-MARK
   PURPOSE a u PREP M MODEL:START
   SERVE-SETTLED ASSERT
   M MODEL:RESEND-DUE 0= ASSERT
   AWAIT-CODE code = ASSERT
   SENDS-SINCE 1 = ASSERT
   counted execute same before 1 NEW-LINES ;


\ NOTED, then the fixture key's service and ", account ": a key fault's line
\ up to the account.
: KEY-NOTED ( ptr u8 n ptr u8 n -- )
   NOTED KEY-SERVICE$ SB-APPEND s" , account " SB-APPEND ;


\ With RESEND-WAIT! on, a send that fails for want of the transport, the key
\ (missing, or refused by the provider), the model (404) or the key's store
\ rests, its one line a rest line; one that fails in its work (a refusal, a
\ bad request, an incomplete answer) lands with its one failure line.
: WHAT-RESTS ( -- )
   60000 M MODEL:RESEND-WAIT!
   NOTE$ M MODEL:LOG-NOTE!
   s" early-drop" MODEL-PEER:FAIL-NEXT
   s" warn" s" MODEL:E-TRANSPORT curl 18: " NOTED
   18 CURL:>CODE CURL:ERROR$ SB-APPEND
   s" ; events keepalive=1; no output" SB-APPEND
   1 RESTS-ON
   s" http-text" MODEL-PEER:FAIL-NEXT
   s" error" s" MODEL:E-CONFIG status 404: model not found" NOTED
   1 RESTS-ON
   s" forbidden" MODEL-PEER:FAIL-NEXT
   s" error" s" MODEL:E-KEY key service " KEY-NOTED
   KEY-ACCOUNT$ SB-APPEND s" : refused by the provider status 403" SB-APPEND
   1 RESTS-ON
   KEY-SERVICE$ s" missing" M MODEL:API-KEY!
   s" error" s" MODEL:E-KEY key service " KEY-NOTED
   s" missing: not found" SB-APPEND
   0 RESTS-ON
   FIXTURE-KEY
   SECRET:E-UNAVAILABLE READ-FAULT!
   s" warn" s" SECRET:E-UNAVAILABLE key service " KEY-NOTED
   KEY-ACCOUNT$ SB-APPEND s" : store unavailable" SB-APPEND
   0 RESTS-ON
   0 READ-FAULT!
   s" error" s" MODEL:E-REFUSED status 200 body: " ID-NOTED
   MODEL-PEER:REFUSAL$ SB-APPEND s" response.completed" STREAM-TAIL
   s" refusal" MODEL:E-REFUSED [: MS-COUNT ;] SETTLES-ON
   s" error" s" MODEL:E-HTTP status 400: bad request" NOTED
   s" http-bad" MODEL:E-HTTP [: SB-COUNT ;] SETTLES-ON
   s" warn" s" MODEL:E-INCOMPLETE incomplete max_output_tokens" ID-NOTED
   s" response.incomplete" STREAM-TAIL
   s" length" MODEL:E-INCOMPLETE [: MS-COUNT ;] SETTLES-ON
   0 M MODEL:RESEND-WAIT!
   s" " M MODEL:LOG-NOTE! ;


\ With RESEND-WAIT! on, a 503 send whose recorder fails does not rest: AWAIT
\ throws the send's fault, RECORD-FAULT keeps the recorder's, and the one
\ line is the send's failure line.
: UNRECORDED-REST ( -- )
   TEST-RECORDER
   60000 M MODEL:RESEND-WAIT!
   NOTE$ M MODEL:LOG-NOTE!
   s" error" s" MODEL:E-UNAVAILABLE status 503: overloaded" NOTED
   SB-COUNT LINES {: same:n before:n :}
   true ROW-FAIL !
   RESEND-MARK
   s" http-busy" MODEL-PEER:FAIL-NEXT
   PURPOSE s" valid" PREP M MODEL:START
   SERVE-SETTLED ASSERT
   M MODEL:RESEND-DUE 0= ASSERT
   AWAIT-CODE MODEL:E-UNAVAILABLE = ASSERT
   false ROW-FAIL !
   M MODEL:RECORD-FAULT E-RECORDER = ASSERT
   SENDS-SINCE 1 = ASSERT
   same before ONE-LINE
   0 M MODEL:RESEND-WAIT!
   s" " M MODEL:LOG-NOTE! ;


\ A recorder that records the row, then has the peer answer the next send
\ 503 after the first row since RESEND-MARK.
: BUSY-ROW ( n ptr u8 n -- n )
   TEST-RECORD
   ROWS-SINCE 1 = if s" http-busy" MODEL-PEER:FAIL-NEXT then ;


\ With RESEND-WAIT! a resend the provider answers 503 again rests again:
\ each failed send writes its one rest line and no failure line, and the
\ third send is served.
: RESTS-AGAIN ( -- )
   0 [: BUSY-ROW ;] [: TEST-REJECT ;] M MODEL:RECORDER!
   1 M MODEL:RESEND-WAIT!
   NOTE$ M MODEL:LOG-NOTE!
   s" warn" s" MODEL:E-UNAVAILABLE status 503: overloaded; sent again in 1 s"
   NOTED
   SB-COUNT LINES {: same:n before:n :}
   RESEND-MARK
   s" http-busy" MODEL-PEER:FAIL-NEXT
   PURPOSE s" valid" PREP M MODEL:START
   AWAIT-CODE 0= ASSERT
   M MODEL:CONTENT$ TRUE-CONTENT$ STR= ASSERT
   SENDS-SINCE 3 = ASSERT
   ROWS-SINCE 3 = ASSERT
   s" outcome" ROW s" ok" STR-IS
   SB-COUNT same before 2 NEW-LINES
   TEST-RECORDER
   0 M MODEL:RESEND-WAIT!
   s" " M MODEL:LOG-NOTE! ;


\ Stops M from another task WAKE-MS after it starts.
: STOP-WORK-LATER ( -- )
   WAKE-MS @ >MS TASK:SLEEP
   mono-ns ACTED-NS !
   M MODEL:REQUEST-STOP
   0 TASK:RETURN ;


\ Opens M again, as the stop stays with the call, and starts its chat, which
\ rests on a 503; the work task REQUEST-STOPs it after ms. Answers SB-COUNT
\ and LINES from before the start and whether the AWAIT's park took the
\ stop's wake.
: STOPPED-REST ( n -- n n bool )
   WAKE-MS !
   M MODEL:CLOSE SETUP
   TEST-RECORDER
   60000 M MODEL:RESEND-WAIT!
   NOTE$ M MODEL:LOG-NOTE!
   SB-COUNT LINES
   DRAIN-HINTS
   RESEND-MARK
   s" http-busy" MODEL-PEER:FAIL-NEXT
   PURPOSE s" valid" PREP M MODEL:START
   SERVE-TO-REST
   ['] STOP-WORK-LATER WORK-TASK TASK:ACTIVATE
   [: 0= ;] AWAIT-WOKEN ;


\ A REQUEST-STOP from another task during a rest wakes the AWAIT parked on
\ it, which lands the call E-CANCELLED at once with one cancelled record;
\ the send's rest line, naming the note, is the call's one line. A stop
\ before the park leaves its wake pending on the starter, so the case stops
\ later until the park takes the wake.
: STOP-REST ( -- )
   s" warn" s" MODEL:E-UNAVAILABLE status 503: overloaded; sent again in 60 s"
   NOTED
   [: STOPPED-REST ;] WAKE-TRIES {: same:n before:n :}
   SENDS-SINCE 1 = ASSERT
   ROWS-SINCE 2 = ASSERT
   s" outcome" ROW s" cancelled" STR-IS
   same before ONE-LINE ;


\ With TIER-RESEND! on, a CANCEL of a send that landed E-TIER at the flex
\ tier, before the tier machine senses it, answers true and keeps it from
\ being sent again: AWAIT throws E-CANCELLED, the tier_unavailable row
\ followed by a cancelled one, and the send's line, naming the note, is
\ written once.
: LANDED-TIER-CANCEL ( -- )
   true M MODEL:TIER-RESEND!
   s" flex" M MODEL:SERVICE-TIER!
   NOTE$ M MODEL:LOG-NOTE!
   s" error" s" MODEL:E-TIER status 200: Flex processing is temporarily unavailable. Please try again later or use standard processing. body: "
   ID-NOTED MODEL-PEER:FLEX-OUT$ SB-APPEND
   s" ; events response.created=1 response.in_progress=1 error=1; no output" SB-APPEND
   SB-COUNT LINES {: same:n before:n :}
   RESEND-MARK
   PURPOSE s" flex-then" PREP CANCELLED-LANDING MODEL:E-CANCELLED = ASSERT
   LANDED-CANCELLED @ 0<> ASSERT
   SENDS-SINCE 1 = ASSERT
   ROWS-SINCE 2 = ASSERT
   s" outcome" ROW s" cancelled" STR-IS
   same before ONE-LINE
   s" " M MODEL:LOG-NOTE!
   false M MODEL:TIER-RESEND! ;


\ With TIER-RESEND! and RESEND-WAIT! on, a send that ends E-TIER at the flex
\ tier is sent again at the standard tier; when that send fails 503 the call
\ rests, with one rest line, and is sent again at the standard tier, naming
\ none, and served.
: TIER-REST ( -- )
   true M MODEL:TIER-RESEND!
   s" flex" M MODEL:SERVICE-TIER!
   1 M MODEL:RESEND-WAIT!
   NOTE$ M MODEL:LOG-NOTE!
   0 [: BUSY-ROW ;] [: TEST-REJECT ;] M MODEL:RECORDER!
   s" warn" s" MODEL:E-UNAVAILABLE status 503: overloaded; sent again in 1 s"
   NOTED
   SB-COUNT LINES {: same:n before:n :}
   RESEND-MARK
   PURPOSE s" flex-then" PREP M MODEL:START
   AWAIT-CODE 0= ASSERT
   M MODEL:CONTENT$ TRUE-CONTENT$ STR= ASSERT
   SENDS-SINCE 3 = ASSERT
   ROWS-SINCE 3 = ASSERT
   s" outcome" ROW s" ok" STR-IS
   s" requested_tier" ROW NULL-IS
   NO-TIER
   LINES before 2 + = ASSERT
   SB-COUNT same 1+ = ASSERT
   TEST-RECORDER
   0 M MODEL:RESEND-WAIT!
   s" " M MODEL:LOG-NOTE!
   false M MODEL:TIER-RESEND! ;


\ With RESEND-WAIT! on, a send at the flex tier that fails 503 rests and is
\ sent again at the flex tier, its record naming it, and served.
: REST-TIER ( -- )
   TEST-RECORDER
   s" flex" M MODEL:SERVICE-TIER!
   1 M MODEL:RESEND-WAIT!
   RESEND-MARK
   s" http-busy" MODEL-PEER:FAIL-NEXT
   PURPOSE s" valid" PREP M MODEL:START
   AWAIT-CODE 0= ASSERT
   M MODEL:CONTENT$ TRUE-CONTENT$ STR= ASSERT
   SENDS-SINCE 2 = ASSERT
   ROWS-SINCE 2 = ASSERT
   s" outcome" ROW s" ok" STR-IS
   s" requested_tier" ROW s" flex" STR-IS
   s" service_tier" SENT s" flex" STR-IS
   0 M MODEL:RESEND-WAIT! ;


\ A recorder that records the row, then, at the first row since RESEND-MARK,
\ signals STOP-READY and waits for STOP-GO in the kernel, where a halt is not
\ served: the starter's halt is pending as ACCOUNT returns.
: HALT-ROW ( n ptr u8 n -- n )
   TEST-RECORD
   ROWS-SINCE 1 = if STOP-READY TASK:SIGNAL-SEM STOP-GO TASK:WAIT-SEM then ;


\ With TIER-RESEND! on, a halt of the starter pending once a send that landed
\ E-TIER at the flex tier is recorded keeps it from being sent again at the
\ standard tier: the starter unwinds, the call landed E-TIER, so a CANCEL
\ then answers false and AWAIT throws E-TIER.
: TIER-HALT ( -- )
   true M MODEL:TIER-RESEND!
   s" flex" M MODEL:SERVICE-TIER!
   0 [: HALT-ROW ;] [: TEST-REJECT ;] M MODEL:RECORDER!
   0 STOP-READY TASK:SEMAPHORE-INIT
   0 STOP-GO TASK:SEMAPHORE-INIT
   RESEND-MARK
   PURPOSE s" flex-then" PREP
   ['] CHAT-WORK WORK-TASK TASK:ACTIVATE
   STOP-READY TASK:WAIT-SEM
   WORK-TASK TASK:HALT
   STOP-GO TASK:SIGNAL-SEM
   WORK-TASK TASK:JOIN MATCH result
      ok OF drop FAIL ENDOF
      err OF drop ENDOF
   ;MATCH
   SENDS-SINCE 1 = ASSERT
   ROWS-SINCE 1 = ASSERT
   M MODEL:CANCEL 0= ASSERT
   AWAIT-CODE MODEL:E-TIER = ASSERT
   TEST-RECORDER
   STOP-READY TASK:SEMAPHORE-DESTROY
   STOP-GO TASK:SEMAPHORE-DESTROY
   false M MODEL:TIER-RESEND! ;


: TIER-PUT ( n ptr u8 n ptr u8 n -- )
   2drop 2drop drop MODEL:E-TIER throw ;


\ With TIER-RESEND! on and the flex tier named, a served send whose journal
\ put throws MODEL:E-TIER lands with that fault and is not sent again at the
\ standard tier.
: TIER-PUT-FAULT ( -- )
   true M MODEL:TIER-RESEND!
   s" flex" M MODEL:SERVICE-TIER!
   0 [: CAPTURE-GET ;] [: TIER-PUT ;] M MODEL:JOURNAL!
   RESEND-MARK
   PURPOSE s" valid" PREP M MODEL:START
   SERVE-SETTLED ASSERT
   AWAIT-CODE MODEL:E-TIER = ASSERT
   SENDS-SINCE 1 = ASSERT
   0 [: CAPTURE-GET ;] [: CAPTURE-PUT ;] M MODEL:JOURNAL!
   false M MODEL:TIER-RESEND! ;


\ The user message of the part "valid" as a chat request writes it.
: HEAD$ ( -- ptr u8 n )
   S\" {\qrole\q:\quser\q,\qcontent\q:[{\qtype\q:\qinput_text\q,\qtext\q:\qvalid\q}]}" ;


\ A journal input's members through its instructions and the key naming its
\ form, into SB.
: INPUT-HEAD ( ptr u8 n -- )
   {: key:ptr key-u:n :}
   SB-RESET
   S\" {\qkind\q:\qgeneration\q,\qconfiguration\q:{\qbackend\q:\qopenai-responses\q," SB-APPEND
   S\" \qmodel\q:\qtest-generation\q,\qendpoint\q:\q" SB-APPEND
   MODEL-PEER:BASE-URL$ SB-APPEND
   S\" /responses\q},\qinstructions\q:\qRead source evidence.\q,\q" SB-APPEND
   key key-u SB-APPEND S\" \q:" SB-APPEND ;


\ A request's members through its head message of the one text part, into SB.
: REQUEST-HEAD ( ptr u8 n -- )
   {: t:ptr t-u:n :}
   SB-RESET
   S\" {\qmodel\q:\qtest-generation\q,\qstore\q:false,\qstream\q:true," SB-APPEND
   S\" \qinstructions\q:\qRead source evidence.\q,\qinput\q:[" SB-APPEND
   S\" {\qrole\q:\quser\q,\qcontent\q:[{\qtype\q:\qinput_text\q,\qtext\q:\q" SB-APPEND
   t t-u SB-APPEND S\" \q}]}" SB-APPEND ;


\ The body of a schema call of the part "valid", into SB.
: SCHEMA-REQUEST ( -- )
   s" valid" REQUEST-HEAD
   S\" ],\qtext\q:{\qformat\q:{\qtype\q:\qjson_schema\q,\qname\q:\qanswer\q," SB-APPEND
   S\" \qstrict\q:true,\qschema\q:" SB-APPEND
   SCHEMA$ SB-APPEND
   s" }}}" SB-APPEND ;


\ A call with parts and no items: its journal input, by which a saved record
\ is found, and its sent body, byte for byte.
: PINNED ( -- )
   s" valid" ASK
   s" schema" INPUT-HEAD SCHEMA$ SB-APPEND
   S\" ,\qparts\q:[{\qtype\q:\qinput_text\q,\qtext\q:\qvalid\q}]}" SB-APPEND
   ASKED$ SB$ STR= ASSERT
   M MODEL:CHAT TRUE-CONTENT$ STR= ASSERT
   SCHEMA-REQUEST
   MODEL-PEER:SENT$ SB$ STR= ASSERT ;


\ History items in OpenAI's shapes; OUTPUT$ keeps a space between members.
: CALLED$ ( -- ptr u8 n )
   S\" {\qtype\q:\qfunction_call\q,\qcall_id\q:\qcall_1\q,\qname\q:\qlookup\q,\qarguments\q:\q{}\q}" ;
: OUTPUT$ ( -- ptr u8 n )
   S\" {\qtype\q:\qfunction_call_output\q, \qcall_id\q:\qcall_1\q,\qoutput\q:\qfound\q}" ;
: USER-ITEM$ ( -- ptr u8 n )
   S\" {\qtype\q:\qmessage\q,\qrole\q:\quser\q,\qcontent\q:[{\qtype\q:\qinput_text\q,\qtext\q:\qvalid\q}]}" ;
: ITEM ( ptr u8 n -- ) M MODEL:ITEM+ ;
: ADD-ITEM ( ptr u8 n ptr MODEL:call -- ptr u8 n ptr MODEL:call )
   {: a:ptr u:n c :}
   a u c MODEL:ITEM+ a u c ;
: ITEM-CODE ( ptr u8 n -- n )
   M [: ADD-ITEM ;] catch {: code:n :} drop 2drop code ;
\ The array or object the reader is on is exactly these source bytes.
: RAW-IS ( JSON:reader ptr u8 n -- )
   {: want:ptr want-u:n :}
   JSON:READ-VALUE-SPAN$ want want-u STR= ASSERT JSON:READ-CLOSE ;


\ ITEM+ before PARTS-RESET, after CHAT or after JOURNAL-INPUT$ is E-STATE, the
\ last dropping the request as a builder does; no part or item is E-CONFIG.
: ITEM-TURNS ( -- )
   CALLED$ ITEM-CODE MODEL:E-STATE = ASSERT
   CHAT
   CALLED$ ITEM-CODE MODEL:E-STATE = ASSERT
   s" valid" ASK
   CALLED$ ITEM-CODE MODEL:E-STATE = ASSERT
   REPLAY-CODE MODEL:E-STATE = ASSERT
   M MODEL:PARTS-RESET
   CALLED$ ITEM-CODE 0= ASSERT
   M MODEL:PARTS-RESET
   CHAT-CODE MODEL:E-CONFIG = ASSERT ;


\ Items follow the head message of the parts in the order added, verbatim in
\ the request and in the journal input's `items`, after `parts`.
: ITEM-ORDER ( -- )
   M MODEL:PARTS-RESET CALLED$ ITEM s" valid" M MODEL:TEXT+ OUTPUT$ ITEM
   M MODEL:JOURNAL-INPUT$ {: a:ptr u:n :}
   SB-RESET
   S\" ,\qparts\q:[{\qtype\q:\qinput_text\q,\qtext\q:\qvalid\q}],\qitems\q:[" SB-APPEND
   CALLED$ SB-APPEND s" ," SB-APPEND OUTPUT$ SB-APPEND s" ]}" SB-APPEND
   a u SB$ ENDS-WITH? ASSERT
   M MODEL:CHAT TRUE-CONTENT$ STR= ASSERT
   SB-RESET s" [" SB-APPEND HEAD$ SB-APPEND s" ," SB-APPEND
   CALLED$ SB-APPEND s" ," SB-APPEND OUTPUT$ SB-APPEND s" ]" SB-APPEND
   s" input" SENT SB$ RAW-IS ;


\ A call of items alone sends them as its whole input; its journal input names
\ no part.
: ITEMS-ONLY ( -- )
   M MODEL:PARTS-RESET USER-ITEM$ ITEM
   M MODEL:JOURNAL-INPUT$ {: a:ptr u:n :}
   SB-RESET S\" ,\qparts\q:[],\qitems\q:[" SB-APPEND
   USER-ITEM$ SB-APPEND s" ]}" SB-APPEND
   a u SB$ ENDS-WITH? ASSERT
   M MODEL:CHAT TRUE-CONTENT$ STR= ASSERT
   SB-RESET s" [" SB-APPEND USER-ITEM$ SB-APPEND s" ]" SB-APPEND
   s" input" SENT SB$ RAW-IS ;


\ An item that is not exactly one JSON object is E-CONFIG and adds nothing.
: ITEM-REFUSALS ( -- )
   s" valid" PREP CALLED$ ITEM
   s" " ITEM-CODE MODEL:E-CONFIG = ASSERT
   s" []" ITEM-CODE MODEL:E-CONFIG = ASSERT
   S\" \qx\q" ITEM-CODE MODEL:E-CONFIG = ASSERT
   s" {}{}" ITEM-CODE MODEL:E-CONFIG = ASSERT
   s" {" ITEM-CODE MODEL:E-CONFIG = ASSERT
   M MODEL:CHAT TRUE-CONTENT$ STR= ASSERT
   SB-RESET s" [" SB-APPEND HEAD$ SB-APPEND s" ," SB-APPEND
   CALLED$ SB-APPEND s" ]" SB-APPEND
   s" input" SENT SB$ RAW-IS ;


\ One strict function tool in OpenAI's shape.
: TOOLS$ ( -- ptr u8 n )
   S\" [{\qtype\q:\qfunction\q,\qname\q:\qlookup\q,\qdescription\q:\qLooks a term up.\q,\qparameters\q:{\qtype\q:\qobject\q,\qproperties\q:{\qq\q:{\qtype\q:\qstring\q}},\qrequired\q:[\qq\q],\qadditionalProperties\q:false},\qstrict\q:true}]" ;
: TOOLS ( -- ) TOOLS$ M MODEL:TOOLS! ;


\ The body of a tools call of the part "tool-call", into SB.
: TOOLS-REQUEST ( -- )
   s" tool-call" REQUEST-HEAD
   S\" ],\qtools\q:" SB-APPEND TOOLS$ SB-APPEND
   S\" ,\qtool_choice\q:\qrequired\q,\qparallel_tool_calls\q:false," SB-APPEND
   S\" \qinclude\q:[\qreasoning.encrypted_content\q]}" SB-APPEND ;


\ The output array of the peer's reply to "tool-call", into SB.
: TOOL-OUTPUT ( -- )
   SB-RESET s" [" SB-APPEND MODEL-PEER:REASONING$ SB-APPEND
   s" ," SB-APPEND MODEL-PEER:FUNCTION-CALL$ SB-APPEND s" ]" SB-APPEND ;


\ A tools call sends its tools verbatim, one required call, encrypted reasoning
\ and no text format; OUTPUT$ is the reply's output array, the content empty.
: TOOLS-BODY ( -- )
   TOOLS s" tool-call" PREP
   M MODEL:CHAT nip 0= ASSERT
   TOOLS-REQUEST MODEL-PEER:SENT$ SB$ STR= ASSERT
   TOOL-OUTPUT M MODEL:OUTPUT$ SB$ STR= ASSERT ;


\ The last of SCHEMA! and TOOLS! decides the call's form.
: FORM-LAST ( -- )
   TOOLS SCHEMA$ M MODEL:SCHEMA!
   CHAT
   SCHEMA-REQUEST MODEL-PEER:SENT$ SB$ STR= ASSERT ;


: TOOLS-CODE ( ptr u8 n -- n ) M MODEL:TOOLS! s" tool-call" PREP CHAT-CODE ;


\ Tools other than one non-empty JSON array of objects, and neither schema nor
\ tools, are E-CONFIG at CHAT with no request sent; that call has no output.
: TOOLS-REFUSALS ( -- )
   MODEL-PEER:CALLS@ {: before:n :}
   s" {}" TOOLS-CODE MODEL:E-CONFIG = ASSERT
   s" []" TOOLS-CODE MODEL:E-CONFIG = ASSERT
   s" [1]" TOOLS-CODE MODEL:E-CONFIG = ASSERT
   S\" \q\q" TOOLS-CODE MODEL:E-CONFIG = ASSERT
   s" [{},1]" TOOLS-CODE MODEL:E-CONFIG = ASSERT
   s" [{}][]" TOOLS-CODE MODEL:E-CONFIG = ASSERT
   s" [{}" TOOLS-CODE MODEL:E-CONFIG = ASSERT
   1 CALL {: c:ptr :}
   c MODEL:OPEN
   MODEL-PEER:BASE-URL$ c MODEL:BASE-URL!
   s" test-generation" c MODEL:GENERATION-MODEL!
   KEY-SERVICE$ KEY-ACCOUNT$ c MODEL:API-KEY!
   s" Read source evidence." c MODEL:SYSTEM!
   c MODEL:PARTS-RESET s" valid" c MODEL:TEXT+
   c [: dup MODEL:CHAT 2drop ;] catch {: chat-code:n :} drop chat-code MODEL:E-CONFIG = ASSERT
   c [: dup MODEL:OUTPUT$ 2drop ;] catch {: output-code:n :} drop output-code MODEL:E-STATE = ASSERT
   c MODEL:CLOSE
   MODEL-PEER:CALLS@ before = ASSERT ;


\ A tools reply of anything but one function call is E-RESPONSE, a refusal
\ E-REFUSED; a schema call's reply of one call has no answer: E-RESPONSE.
: TOOL-REPLIES ( -- )
   TOOLS
   s" tool-message" MODEL:E-RESPONSE FAILS
   s" tool-none" MODEL:E-RESPONSE FAILS
   s" tool-two" MODEL:E-RESPONSE FAILS
   s" tool-other" MODEL:E-RESPONSE FAILS
   s" tool-refusal" MODEL:E-REFUSED FAILS
   SCHEMA$ M MODEL:SCHEMA!
   s" tool-call" MODEL:E-RESPONSE FAILS ;


\ A tools call's journal input names its tools where a schema call's names its
\ schema, then its parts and items.
: TOOLS-JOURNAL ( -- )
   TOOLS
   M MODEL:PARTS-RESET s" valid" M MODEL:TEXT+ CALLED$ ITEM
   M MODEL:JOURNAL-INPUT$ {: a:ptr u:n :}
   s" tools" INPUT-HEAD TOOLS$ SB-APPEND
   S\" ,\qparts\q:[{\qtype\q:\qinput_text\q,\qtext\q:\qvalid\q}],\qitems\q:[" SB-APPEND
   CALLED$ SB-APPEND s" ]}" SB-APPEND
   a u SB$ STR= ASSERT ;


\ The record a tools call saved holds empty content and replays with no
\ request and the same output.
: TOOLS-REPLAY ( -- )
   CAPTURE
   TOOLS s" tool-call" PREP
   M MODEL:CHAT nip 0= ASSERT
   RECORD$ s" content" MEMBER s" " STR-IS
   MODEL-PEER:CALLS@ {: calls:n :}
   s" tool-call" ASK
   RECORD$ M MODEL:REPLAY nip 0= ASSERT
   MODEL-PEER:CALLS@ calls = ASSERT
   TOOL-OUTPUT M MODEL:OUTPUT$ SB$ STR= ASSERT ;


: COUNT-CODE ( ptr MODEL:call -- n )
   [: dup MODEL:COUNT-INPUT drop ;] catch {: code:n :} drop code ;


\ A count body's members through its instructions and the head message of the
\ one text part, the effort high, into SB.
: COUNT-HEAD ( ptr u8 n -- )
   {: t:ptr t-u:n :}
   SB-RESET
   S\" {\qmodel\q:\qtest-generation\q,\qreasoning\q:{\qeffort\q:\qhigh\q}," SB-APPEND
   S\" \qinstructions\q:\qRead source evidence.\q,\qinput\q:[" SB-APPEND
   S\" {\qrole\q:\quser\q,\qcontent\q:[{\qtype\q:\qinput_text\q,\qtext\q:\q" SB-APPEND
   t t-u SB-APPEND S\" \q}]}" SB-APPEND ;


\ A count sends the members the count endpoint accepts in the chat body's
\ order, with no store, stream, include, service_tier, prompt_cache_key or
\ max_output_tokens: a tools call its tools, one required call and no parallel
\ calls, a schema call its text format. It answers the peer's count, the body's
\ length.
: COUNT-BODY ( -- )
   TOOLS s" high" M MODEL:EFFORT!
   s" test:count" M MODEL:CACHE-KEY! s" flex" M MODEL:SERVICE-TIER!
   9200 M MODEL:MAX-OUTPUT!
   s" tool-call" PREP CALLED$ ITEM
   M MODEL:COUNT-INPUT {: tools-n:n :}
   s" tool-call" COUNT-HEAD s" ," SB-APPEND CALLED$ SB-APPEND
   S\" ],\qtools\q:" SB-APPEND TOOLS$ SB-APPEND
   S\" ,\qtool_choice\q:\qrequired\q,\qparallel_tool_calls\q:false}" SB-APPEND
   MODEL-PEER:SENT$ SB$ STR= ASSERT
   tools-n MODEL-PEER:SENT$ nip = ASSERT
   SCHEMA$ M MODEL:SCHEMA!
   s" valid" PREP
   M MODEL:COUNT-INPUT {: schema-n:n :}
   s" valid" COUNT-HEAD
   S\" ],\qtext\q:{\qformat\q:{\qtype\q:\qjson_schema\q,\qname\q:\qanswer\q," SB-APPEND
   S\" \qstrict\q:true,\qschema\q:" SB-APPEND SCHEMA$ SB-APPEND s" }}}" SB-APPEND
   MODEL-PEER:SENT$ SB$ STR= ASSERT
   schema-n MODEL-PEER:SENT$ nip = ASSERT
   s" " M MODEL:EFFORT! s" " M MODEL:CACHE-KEY! s" " M MODEL:SERVICE-TIER!
   0 M MODEL:MAX-OUTPUT! ;


create COUNTED 32768 allot    \ the input member of the last count body kept
variable COUNTED-U
: COUNTED$ ( -- ptr u8 n ) COUNTED COUNTED-U @ ;


\ A count leaves its request built: the CHAT after it sends the counted input
\ byte for byte and reads the tools reply as any tools call does.
: COUNT-THEN-CHAT ( -- )
   TOOLS s" tool-call" PREP CALLED$ ITEM
   M MODEL:COUNT-INPUT drop
   s" input" SENT JSON:READ-VALUE-SPAN$ {: a:ptr u:n :}
   u 32768 <= ASSERT
   a u COUNTED 32768 SPAN:MAKE SPAN:COPY u COUNTED-U !
   JSON:READ-CLOSE
   M MODEL:CHAT nip 0= ASSERT
   s" input" SENT COUNTED$ RAW-IS
   TOOL-OUTPUT M MODEL:OUTPUT$ SB$ STR= ASSERT
   SCHEMA$ M MODEL:SCHEMA! ;


\ A count's journal input is a generation input's with kind input_tokens and
\ the count endpoint; its record holds empty content and the recorder records
\ it as kind input_tokens. The same count again replays from the journal with
\ no request and no row and answers the same count. A journaled CHAT then sends
\ the counted request unrebuilt, under the journal input a fresh build gives.
\ The recorder stays until OPEN, so the call is opened again for the cases after.
: COUNT-JOURNAL ( -- )
   CAPTURE
   TEST-RECORDER
   0 ROWS-MADE ! false ROW-FAIL !
   PURPOSE s" valid" PREP
   M MODEL:COUNT-INPUT {: tokens:n :}
   tokens MODEL-PEER:SENT$ nip = ASSERT
   ROWS-MADE @ 1 = ASSERT
   s" kind" ROW s" input_tokens" STR-IS
   RECORD$ s" content" MEMBER s" " STR-IS
   SB-RESET
   S\" {\qkind\q:\qinput_tokens\q,\qconfiguration\q:{\qbackend\q:\qopenai-responses\q," SB-APPEND
   S\" \qmodel\q:\qtest-generation\q,\qendpoint\q:\q" SB-APPEND
   MODEL-PEER:BASE-URL$ SB-APPEND
   S\" /responses/input_tokens\q},\qinstructions\q:\qRead source evidence.\q,\qschema\q:" SB-APPEND
   SCHEMA$ SB-APPEND
   S\" ,\qparts\q:[{\qtype\q:\qinput_text\q,\qtext\q:\qvalid\q}]}" SB-APPEND
   INPUT$ SB$ STR= ASSERT
   MODEL-PEER:CALLS@ {: calls:n :}
   true JOURNAL-READY !
   0 [: JOURNAL-GET ;] [: JOURNAL-PUT ;] M MODEL:JOURNAL!
   PURPOSE s" valid" PREP
   M MODEL:COUNT-INPUT tokens = ASSERT
   MODEL-PEER:CALLS@ calls = ASSERT
   ROWS-MADE @ 1 = ASSERT
   CAPTURE
   PURPOSE M MODEL:CHAT TRUE-CONTENT$ STR= ASSERT
   PURPOSE s" valid" ASK
   ASKED$ INPUT$ STR= ASSERT
   M MODEL:CLOSE SETUP ;


\ A request JOURNAL-INPUT$ finished is not counted: COUNT-INPUT is E-STATE and
\ sends nothing, and the finished request still sends with CHAT or replays.
: COUNT-PREPARED ( -- )
   CAPTURE
   MODEL-PEER:CALLS@ {: before:n :}
   s" valid" ASK
   M COUNT-CODE MODEL:E-STATE = ASSERT
   MODEL-PEER:CALLS@ before = ASSERT
   M MODEL:CHAT TRUE-CONTENT$ STR= ASSERT
   MODEL-PEER:CALLS@ before 1+ = ASSERT
   s" valid" ASK
   M COUNT-CODE MODEL:E-STATE = ASSERT
   RECORD$ M MODEL:REPLAY TRUE-CONTENT$ STR= ASSERT
   MODEL-PEER:CALLS@ before 1+ = ASSERT ;


\ OpenAI's refusal of a count over the model's context is E-CAPACITY.
: COUNT-TOO-LARGE ( -- )
   s" too-large" PREP
   M COUNT-CODE MODEL:E-CAPACITY = ASSERT ;


\ A count the provider fails keeps its request: COUNT-INPUT sends the same
\ body again.
: COUNT-RESENT ( -- )
   s" valid" PREP
   s" http-error" MODEL-PEER:FAIL-NEXT
   M COUNT-CODE MODEL:E-UNAVAILABLE = ASSERT
   KEEP-SENT
   M COUNT-CODE 0= ASSERT
   SENT-AGAIN ;


\ A count has no content: CONTENT$ and OUTPUT$ after it are E-STATE. A count
\ with no request built, as after a CHAT, is E-STATE, and one of a call with
\ neither schema nor tools E-CONFIG, each sending nothing.
: COUNT-RESULT ( -- )
   s" valid" PREP M MODEL:COUNT-INPUT drop
   CONTENT-CODE MODEL:E-STATE = ASSERT
   M [: dup MODEL:OUTPUT$ 2drop ;] catch {: code:n :} drop code MODEL:E-STATE = ASSERT
   CHAT
   MODEL-PEER:CALLS@ {: before:n :}
   M COUNT-CODE MODEL:E-STATE = ASSERT
   1 CALL {: c:ptr :}
   c MODEL:OPEN
   MODEL-PEER:BASE-URL$ c MODEL:BASE-URL!
   s" test-generation" c MODEL:GENERATION-MODEL!
   KEY-SERVICE$ KEY-ACCOUNT$ c MODEL:API-KEY!
   s" Read source evidence." c MODEL:SYSTEM!
   c MODEL:PARTS-RESET s" valid" c MODEL:TEXT+
   c COUNT-CODE MODEL:E-CONFIG = ASSERT
   c MODEL:CLOSE
   MODEL-PEER:CALLS@ before = ASSERT ;


: COMPACT-CODE ( ptr MODEL:call -- n )
   [: dup MODEL:COMPACT ;] catch {: code:n :} drop code ;


\ The window the peer answers a compaction whose first text is t and whose
\ body is n bytes, into SB.
: WINDOW ( ptr u8 n n -- )
   {: t:ptr t-u:n body-u:n :}
   SB-RESET
   S\" [{\qtype\q:\qmessage\q,\qrole\q:\quser\q,\qcontent\q:[{\qtype\q:\qinput_text\q,\qtext\q:\q" SB-APPEND
   t t-u SB-APPEND
   S\" \q}]},{\qtype\q:\qcompaction\q,\qid\q:\qcmp_item\q,\qencrypted_content\q:\qlen-" SB-APPEND
   body-u FMT:SB-U S\" \q}]" SB-APPEND ;


\ A compaction sends exactly model, prompt_cache_key, service_tier,
\ instructions and input: no tools, reasoning, text, store, stream or include.
: COMPACT-BODY ( -- )
   TOOLS s" high" M MODEL:EFFORT!
   s" test:compact" M MODEL:CACHE-KEY! s" flex" M MODEL:SERVICE-TIER!
   s" valid" PREP CALLED$ ITEM
   M MODEL:COMPACT
   SB-RESET
   S\" {\qmodel\q:\qtest-generation\q,\qprompt_cache_key\q:\qtest:compact\q," SB-APPEND
   S\" \qservice_tier\q:\qflex\q,\qinstructions\q:\qRead source evidence.\q,\qinput\q:[" SB-APPEND
   S\" {\qrole\q:\quser\q,\qcontent\q:[{\qtype\q:\qinput_text\q,\qtext\q:\qvalid\q}]}," SB-APPEND
   CALLED$ SB-APPEND s" ]}" SB-APPEND
   MODEL-PEER:SENT$ SB$ STR= ASSERT
   s" " M MODEL:EFFORT! s" " M MODEL:CACHE-KEY! s" " M MODEL:SERVICE-TIER!
   SCHEMA$ M MODEL:SCHEMA! ;


\ A compaction leaves its window as the call's finished input: a TOOLS! after
\ it keeps it, the count and the CHAT after it send the window byte for byte
\ with the tools set, and the CHAT reads the tools reply as any tools call.
: COMPACT-THEN-CHAT ( -- )
   TOOLS s" tool-call" PREP CALLED$ ITEM OUTPUT$ ITEM
   M MODEL:COUNT-INPUT drop
   M MODEL:COMPACT
   MODEL-PEER:SENT$ nip {: body-u:n :}
   TOOLS
   M MODEL:COUNT-INPUT drop
   s" tool-call" body-u WINDOW s" input" SENT SB$ RAW-IS
   M MODEL:CHAT nip 0= ASSERT
   s" tool-call" body-u WINDOW s" input" SENT SB$ RAW-IS
   s" tools" SENT TOOLS$ RAW-IS
   TOOL-OUTPUT M MODEL:OUTPUT$ SB$ STR= ASSERT
   SCHEMA$ M MODEL:SCHEMA! ;


\ A compaction's journal input is kind compaction with the compact endpoint,
\ the parts and items as built and no effort, schema or tools; its record holds
\ empty content and a configuration with no effort, and the recorder records it
\ as kind compaction with the reply's usage and no effort. The same compaction
\ again replays from the journal with no request, after which
\ GENERATION-CONFIG$ still carries the effort, and leaves the live window,
\ which the count after it sends. The recorder stays until OPEN, so the call
\ is opened again for the cases after.
: COMPACT-JOURNAL ( -- )
   CAPTURE
   TEST-RECORDER
   0 ROWS-MADE ! false ROW-FAIL !
   s" high" M MODEL:EFFORT!
   PURPOSE s" valid" PREP CALLED$ ITEM
   M MODEL:COMPACT
   MODEL-PEER:SENT$ nip {: body-u:n :}
   ROWS-MADE @ 1 = ASSERT
   s" kind" ROW s" compaction" STR-IS
   s" effort" ROW NULL-IS
   RECORD$ s" configuration" MEMBER s" effort" JSON:READ-FIND-KEY 0= ASSERT JSON:READ-CLOSE
   s" input_tokens" ROW 11 INT-IS
   s" kind" USAGE s" compaction" STR-IS
   s" input_tokens" USAGE 11 INT-IS
   RECORD$ s" content" MEMBER s" " STR-IS
   SB-RESET
   S\" {\qkind\q:\qcompaction\q,\qconfiguration\q:{\qbackend\q:\qopenai-responses\q," SB-APPEND
   S\" \qmodel\q:\qtest-generation\q,\qendpoint\q:\q" SB-APPEND
   MODEL-PEER:BASE-URL$ SB-APPEND
   S\" /responses/compact\q},\qinstructions\q:\qRead source evidence.\q," SB-APPEND
   S\" \qparts\q:[{\qtype\q:\qinput_text\q,\qtext\q:\qvalid\q}],\qitems\q:[" SB-APPEND
   CALLED$ SB-APPEND s" ]}" SB-APPEND
   INPUT$ SB$ STR= ASSERT
   MODEL-PEER:CALLS@ {: calls:n :}
   true JOURNAL-READY !
   0 [: JOURNAL-GET ;] [: JOURNAL-PUT ;] M MODEL:JOURNAL!
   PURPOSE s" valid" PREP CALLED$ ITEM
   M MODEL:COMPACT
   MODEL-PEER:CALLS@ calls = ASSERT
   M MODEL:GENERATION-CONFIG$ s" effort" MEMBER s" high" STR-IS
   CAPTURE
   PURPOSE M MODEL:COUNT-INPUT drop
   s" valid" body-u WINDOW s" input" SENT SB$ RAW-IS
   M MODEL:CLOSE SETUP ;


\ A request JOURNAL-INPUT$ finished is not compacted: COMPACT is E-STATE and
\ sends nothing, and the finished request still sends with CHAT.
: COMPACT-PREPARED ( -- )
   CAPTURE
   MODEL-PEER:CALLS@ {: before:n :}
   s" valid" ASK
   M COMPACT-CODE MODEL:E-STATE = ASSERT
   MODEL-PEER:CALLS@ before = ASSERT
   M MODEL:CHAT TRUE-CONTENT$ STR= ASSERT
   MODEL-PEER:CALLS@ before 1+ = ASSERT ;


\ OpenAI's refusal of a compaction over the model's context is E-CAPACITY.
: COMPACT-TOO-LARGE ( -- )
   s" too-large" PREP
   M COMPACT-CODE MODEL:E-CAPACITY = ASSERT ;


\ A compaction the provider fails keeps its request: COMPACT sends the same
\ body again.
: COMPACT-RESENT ( -- )
   s" valid" PREP
   s" http-error" MODEL-PEER:FAIL-NEXT
   M COMPACT-CODE MODEL:E-UNAVAILABLE = ASSERT
   KEEP-SENT
   M COMPACT-CODE 0= ASSERT
   SENT-AGAIN ;


\ A compaction has no content: CONTENT$ and OUTPUT$ after it are E-STATE, and
\ its usage is the reply's. A call with neither schema nor tools compacts. A
\ compaction with no request built, as after a CHAT, is E-STATE and sends
\ nothing.
: COMPACT-RESULT ( -- )
   s" valid" PREP M MODEL:COMPACT
   CONTENT-CODE MODEL:E-STATE = ASSERT
   M [: dup MODEL:OUTPUT$ 2drop ;] catch {: code:n :} drop code MODEL:E-STATE = ASSERT
   M MODEL:INPUT-TOKENS 11 = ASSERT
   M MODEL:CACHED-TOKENS 4 = ASSERT
   1 CALL {: c:ptr :}
   c MODEL:OPEN
   MODEL-PEER:BASE-URL$ c MODEL:BASE-URL!
   s" test-generation" c MODEL:GENERATION-MODEL!
   KEY-SERVICE$ KEY-ACCOUNT$ c MODEL:API-KEY!
   s" Read source evidence." c MODEL:SYSTEM!
   c MODEL:PARTS-RESET s" valid" c MODEL:TEXT+
   c COMPACT-CODE 0= ASSERT
   c MODEL:CLOSE
   CHAT
   MODEL-PEER:CALLS@ {: before:n :}
   M COMPACT-CODE MODEL:E-STATE = ASSERT
   MODEL-PEER:CALLS@ before = ASSERT ;


\ The count after a compaction that failed E-RESPONSE sends the input as built:
\ one user message of the text.
: UNTOUCHED ( ptr u8 n -- )
   {: t:ptr t-u:n :}
   M MODEL:COUNT-INPUT drop
   SB-RESET
   S\" [{\qrole\q:\quser\q,\qcontent\q:[{\qtype\q:\qinput_text\q,\qtext\q:\q" SB-APPEND
   t t-u SB-APPEND S\" \q}]}]" SB-APPEND
   s" input" SENT SB$ RAW-IS ;


\ A compaction of the text whose reply is E-RESPONSE leaves the input as built.
: BAD-WINDOW ( ptr u8 n -- )
   {: t:ptr t-u:n :}
   t t-u PREP
   M COMPACT-CODE MODEL:E-RESPONSE = ASSERT
   t t-u UNTOUCHED ;


\ A compaction reply whose output ends in no compaction item is E-RESPONSE,
\ leaves the input as built, and is recorded with the reply's usage, the
\ requested tier and the tier the reply names. The recorder stays until OPEN,
\ so the call is opened again for the cases after.
: COMPACT-REPLY ( -- )
   CAPTURE
   TEST-RECORDER
   0 ROWS-MADE ! false ROW-FAIL !
   s" auto" M MODEL:SERVICE-TIER!
   PURPOSE s" bad-compact" PREP
   M COMPACT-CODE MODEL:E-RESPONSE = ASSERT
   ROWS-MADE @ 1 = ASSERT
   s" input_tokens" ROW 11 INT-IS
   s" requested_tier" ROW s" auto" STR-IS
   s" service_tier" ROW s" flex" STR-IS
   PURPOSE s" bad-compact" UNTOUCHED
   M MODEL:CLOSE SETUP ;


\ A compaction reply whose output is empty, holds a non-object or has an item
\ after its compaction item is E-RESPONSE and leaves the input as built.
: COMPACT-WINDOWS ( -- )
   s" empty-window" BAD-WINDOW
   s" number-window" BAD-WINDOW
   s" late-window" BAD-WINDOW ;


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
\ with no provider call. The record it returns is a live call's to the peer,
\ captured first.
: OPENAI-BASE ( -- )
   MODEL-PEER:BASE-URL$ BASE-IS
   s" valid" JOURNAL-TEXT 5 SPAN:MAKE SPAN:COPY
   JOURNAL-TEXT 5 + JOURNAL-U 5 - XS
   CAPTURE
   JOURNAL-TEXT JOURNAL-U PREP
   M MODEL:CHAT TRUE-CONTENT$ STR= ASSERT
   s" " M MODEL:BASE-URL!
   s" https://api.openai.com/v1" BASE-IS
   0 [: OPENAI-GET ;] [: JOURNAL-PUT ;] M MODEL:JOURNAL!
   MODEL-PEER:CALLS@ {: calls:n :}
   JOURNAL-TEXT JOURNAL-U PREP
   M MODEL:CHAT TRUE-CONTENT$ STR= ASSERT
   MODEL-PEER:CALLS@ calls = ASSERT ;


: IDENTITY$ ( -- ptr u8 n ) s" 7907646426070047a77226ac3e684fbbe8410524f7b4a74d02837e43f2146bab" ;


: OLLAMA-AT ( n n -- )
   {: d:n deadline:n :}
   MODEL-PEER:OLLAMA-URL$ s" bge-m3:567m" IDENTITY$ d deadline M MODEL:OLLAMA! ;

: OLLAMA-DIMS ( n -- n ) {: d:n :} d 60000 OLLAMA-AT d ;


\ A held embedding reply ends the call at its 300 ms deadline, within 2 s.
2000000000 constant DEADLINE-NS
: DEADLINE-ENDS ( -- )
   3 300 OLLAMA-AT
   s" hold" INPUTS
   mono-ns {: t0:n :}
   EMBED-CODE MODEL:E-TRANSPORT = ASSERT
   mono-ns t0 - DEADLINE-NS < ASSERT
   MODEL-PEER:AWAIT-HOLD ;


: OLLAMA-CASES ( -- )
   3 OLLAMA-DIMS drop
   M MODEL:EMBEDDING-CONFIG$ s" identity" MEMBER IDENTITY$ STR-IS
   s" one" INPUTS
   s" two" M MODEL:INPUT+
   M MODEL:EMBED
   0 M MODEL:VECTOR@ $3DCCCCCD $3E4CCCCD $3E99999A VECTOR-IS
   1 M MODEL:VECTOR@ 0 $3F800000 $40000000 VECTOR-IS
   s" bad-count" EMBED-ERROR
   s" bad-dims" EMBED-ERROR
   s" bad-finite" EMBED-ERROR
   s" bad-single" EMBED-ERROR
   s" too-large" INPUTS
   EMBED-CODE MODEL:E-CAPACITY = ASSERT
   M MODEL:HTTP-STATUS 400 = ASSERT
   DEADLINE-ENDS
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


\ While a limit is set, a count and a compaction send and journal none, a
\ negative limit keeps the earlier one, and setting one drops a prepared request.
: MAX-OUTPUT-KINDS ( -- )
   CAPTURE
   9200 M MODEL:MAX-OUTPUT!
   0 [: NEGATIVE-MAX-OUTPUT ;] CODE-OF MODEL:E-CONFIG = ASSERT
   PURPOSE CHAT
   9200 MAX-OUTPUT-IS
   PURPOSE s" valid" PREP M MODEL:COUNT-INPUT drop
   NO-MAX-OUTPUT
   PURPOSE M MODEL:CHAT TRUE-CONTENT$ STR= ASSERT
   9200 MAX-OUTPUT-IS
   s" max_output_tokens" CONFIG 9200 INT-IS
   PURPOSE s" valid" PREP CALLED$ ITEM M MODEL:COMPACT
   NO-MAX-OUTPUT
   0 M MODEL:MAX-OUTPUT!
   PURPOSE s" valid" PREPARE
   9200 M MODEL:MAX-OUTPUT!
   CHAT-CODE MODEL:E-STATE = ASSERT
   0 M MODEL:MAX-OUTPUT! ;


: CASES ( -- )
   SETUP
   M MODEL:CANCEL 0= ASSERT
   BASES CHAT-REQUEST IMAGE STORE-KEY NEGATIVE-KEY INVALID-IMAGE INVALID-SCHEMA
   REPLIES EARLY-ENDS UNREADABLE REFUSED [: CANCELLATION ;] QUIET
   EMBEDDINGS BAD-VECTORS BASE64-CASES EMBED-TOO-LARGE EMBED-RESENT INPUT-BOUNDS
   INPUT-MAX-EMBED
   DIMS-PROPERTY EMBEDDING-NAMED
   STOP-BETWEEN
   M MODEL:CLOSE
   SETUP
   CHAT EMBEDDINGS PREPARED-CHAT
   [: STARTED-CALLS HELD-START ;] QUIET LANDED-START FAILED-START
   FAILED-RESENT NOTED-FAILURE
   SERVED-CALL STARTER-ONLY SETUP HALTED-STARTER
   SETUP HELD-SERVE MANY-CALLS SPOILED-SERVE
   STOP-ACTIVE
   M MODEL:CLOSE
   SETUP PUT-HALT SETUP RECORD-HALT SETUP PENDING-HALT
   SETUP RECORDED-CALLS REPLAYED-CALLS REFUSAL-PUT-FAULT CALLER-JOURNAL
   RECORDED-RESEND RESTS LANDED-CANCEL KEY-RESTS
   CUT-REST HALT-AT-DUE CANCELLED-TWICE
   LOOKUP-FAULT PUT-FAULT CUT-KEY PROVIDER-KEY
   WHAT-RESTS UNRECORDED-REST RESTS-AGAIN STOP-REST
   M MODEL:CLOSE
   SETUP ITEM-TURNS ITEM-ORDER ITEMS-ONLY ITEM-REFUSALS PINNED
   M MODEL:CLOSE
   SETUP TOOLS-BODY FORM-LAST TOOLS-REFUSALS TOOL-REPLIES TOOLS-JOURNAL TOOLS-REPLAY
   M MODEL:CLOSE
   SETUP COUNT-BODY COUNT-THEN-CHAT COUNT-JOURNAL COUNT-PREPARED COUNT-TOO-LARGE COUNT-RESENT COUNT-RESULT
   M MODEL:CLOSE
   SETUP COMPACT-BODY COMPACT-THEN-CHAT COMPACT-JOURNAL COMPACT-PREPARED COMPACT-TOO-LARGE COMPACT-RESENT COMPACT-RESULT COMPACT-REPLY COMPACT-WINDOWS
   M MODEL:CLOSE
   SETUP EFFORTS CACHE-KEYS MAX-OUTPUTS MAX-OUTPUT-KINDS
   M MODEL:CLOSE
   SETUP TIERS LANDED-TIER-CANCEL TIER-REST REST-TIER TIER-HALT TIER-PUT-FAULT
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
