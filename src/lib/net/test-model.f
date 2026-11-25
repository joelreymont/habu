\ test-model.f - a model server's replies for tests, never a production one:
\ each public word answers one request on Habu's HTTP server as OpenAI's
\ Responses API does. COMPLETED!, INCOMPLETE!, REFUSAL! and OUTPUT! answer a
\ POST /responses as it streams it, an event stream of response resp_1's
\ created and in_progress events and then the terminal event carrying the
\ reply; CUT! sends those two events alone. Each terminal reply carries
\ fixed usage, input 11 tokens (4 cached), output 6 (2 reasoning), total 17,
\ so a test may assert them. The host routes the request to its own handler
\ and calls one of them there.
\ STORAGE CLASS. PROCESS-WIDE: rows indexed by HTTP:WORKER-SLOT, each a reply
\ body and its writer, whose vector a slot's first reply makes and the process
\ keeps; a body stays in its row, as HTTP:BODY! borrows it, until the slot's
\ next reply.
\ https://platform.openai.com/docs/api-reference/responses-streaming

require src/lib/string.f
require src/lib/heap.f
require src/lib/vector.f
require src/lib/json.f
require src/lib/net/http.f
require src/lib/net/http-arena.f
require src/lib/net/http-response.f
require src/lib/json/write.f

package TEST-MODEL
private

HTTP:MAX-WORKERS TYPED-BUFFER BODIES ^VEC:vector<u8>
HTTP:MAX-WORKERS TYPED-BUFFER WRITER JSON:writer

: SLOT ( -- n ) HTTP:WORKER-SLOT ;
: ROW ( -- ^VEC:vector<u8> ) SLOT BODIES @ ;
: ROW$ ( -- str ) ROW VEC:BYTES ;


: READY ( -- )
   ROW NULL-PTR <> if exit then
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW SLOT BODIES ! ;


\ Opens the data of an event of the type numbered n, after its type and number.
: EVENT-OPEN ( ^JSON:writer str n -- ^JSON:writer )
   {: type:str seq:n :}
   s" event: " JSON:WRITE-RAW type JSON:WRITE-RAW
   S\" \ndata: {" JSON:WRITE-RAW
   s" type" type JSON:WRITE-FIELD-S
   S\" ,\qsequence_number\q:" JSON:WRITE-RAW seq JSON:WRITE-U ;


: EVENT-CLOSE ( ^JSON:writer -- ^JSON:writer )
   S\" }\n\n" JSON:WRITE-RAW ;


\ EVENT-OPEN for an event that carries a response, which comes next.
: RESPONSE-OPEN ( ^JSON:writer str n -- ^JSON:writer )
   EVENT-OPEN S\" ,\qresponse\q:" JSON:WRITE-RAW ;


: PENDING ( ^JSON:writer str -- ^JSON:writer )
   {: status:str :}
   S\" {\qid\q:\qresp_1\q,\qobject\q:\qresponse\q," JSON:WRITE-RAW
   s" status" status JSON:WRITE-FIELD-S
   S\" ,\qoutput\q:[]}" JSON:WRITE-RAW ;


\ A reply object's members through its first output item, a reasoning item.
: REPLY-HEAD ( ^JSON:writer str -- ^JSON:writer )
   {: status:str :}
   S\" {\qid\q:\qresp_1\q,\qobject\q:\qresponse\q," JSON:WRITE-RAW
   s" status" status JSON:WRITE-FIELD-S
   S\" ,\qoutput\q:[{\qtype\q:\qreasoning\q,\qid\q:\qrs_1\q,\qsummary\q:[]}" JSON:WRITE-RAW ;


: USAGE ( ^JSON:writer -- ^JSON:writer )
   S\" ,\qusage\q:{\qinput_tokens\q:11,\qinput_tokens_details\q:{\qcached_tokens\q:4},\qoutput_tokens\q:6,\qoutput_tokens_details\q:{\qreasoning_tokens\q:2},\qtotal_tokens\q:17}" JSON:WRITE-RAW ;


\ A reply's message item, up to its one content part.
: MESSAGE-OPEN ( ^JSON:writer -- ^JSON:writer )
   S\" ,{\qtype\q:\qmessage\q,\qid\q:\qmsg_1\q,\qrole\q:\qassistant\q,\qcontent\q:[{" JSON:WRITE-RAW ;


: MESSAGE-CLOSE ( ^JSON:writer -- ^JSON:writer )
   S\" }]}],\qincomplete_details\q:null,\qerror\q:null" JSON:WRITE-RAW USAGE
   s" }" JSON:WRITE-RAW ;


: COMPLETED-REPLY ( ^JSON:writer str -- ^JSON:writer )
   {: text:str :}
   s" completed" REPLY-HEAD MESSAGE-OPEN
   S\" \qtype\q:\qoutput_text\q," JSON:WRITE-RAW
   s" text" text JSON:WRITE-FIELD-S
   S\" ,\qannotations\q:[]" JSON:WRITE-RAW
   MESSAGE-CLOSE ;


: INCOMPLETE-REPLY ( ^JSON:writer str -- ^JSON:writer )
   {: reason:str :}
   s" incomplete" REPLY-HEAD
   S\" ],\qincomplete_details\q:{" JSON:WRITE-RAW
   s" reason" reason JSON:WRITE-FIELD-S
   S\" },\qerror\q:null" JSON:WRITE-RAW USAGE
   s" }" JSON:WRITE-RAW ;


: REFUSAL-REPLY ( ^JSON:writer str -- ^JSON:writer )
   {: text:str :}
   s" completed" REPLY-HEAD MESSAGE-OPEN
   S\" \qtype\q:\qrefusal\q," JSON:WRITE-RAW
   s" refusal" text JSON:WRITE-FIELD-S
   MESSAGE-CLOSE ;


: OUTPUT-REPLY ( ^JSON:writer str -- ^JSON:writer )
   {: items:str :}
   S\" {\qid\q:\qresp_1\q,\qobject\q:\qresponse\q,\qstatus\q:\qcompleted\q,\qoutput\q:" JSON:WRITE-RAW
   items JSON:WRITE-RAW
   S\" ,\qincomplete_details\q:null,\qerror\q:null" JSON:WRITE-RAW USAGE
   s" }" JSON:WRITE-RAW ;


: OPENED ( -- ^JSON:writer )
   READY
   SLOT WRITER ROW JSON:OPEN
   s" response.created" 0 RESPONSE-OPEN s" queued" PENDING EVENT-CLOSE
   s" response.in_progress" 1 RESPONSE-OPEN s" in_progress" PENDING EVENT-CLOSE ;


: SENT ( ^JSON:writer HTTP:response -- )
   {: answer:HTTP:response :}
   JSON:CLOSE
   answer s" Content-Type" s" text/event-stream" HTTP:HEADER!
   answer ROW$ HTTP:BODY! ;


: STREAMED ( HTTP:response str [ ^JSON:writer str -- ^JSON:writer ] str -- )
   {: answer:HTTP:response text:str body type:str :}
   OPENED
   type 2 RESPONSE-OPEN text body execute EVENT-CLOSE
   answer SENT ;

public

\ A stream ending in a completed reply whose message holds the text.
: COMPLETED! ( HTTP:response str -- )
   [: COMPLETED-REPLY ;] s" response.completed" STREAMED ;


\ A stream ending in an incomplete reply with the reason.
: INCOMPLETE! ( HTTP:response str -- )
   [: INCOMPLETE-REPLY ;] s" response.incomplete" STREAMED ;


\ A stream cut before its reply: its body ends cleanly with no terminal event.
: CUT! ( HTTP:response -- )
   {: answer:HTTP:response :}
   OPENED answer SENT ;


\ A stream ending in a completed reply whose message part is a refusal with
\ the text.
: REFUSAL! ( HTTP:response str -- )
   [: REFUSAL-REPLY ;] s" response.completed" STREAMED ;


\ A stream ending in a completed reply whose output is exactly the JSON array.
: OUTPUT! ( HTTP:response str -- )
   [: OUTPUT-REPLY ;] s" response.completed" STREAMED ;


\ A count of the request's input tokens, the reply OpenAI gives at
\ /responses/input_tokens: one JSON object, no event stream and no usage.
: INPUT-TOKENS! ( HTTP:response n -- )
   {: answer:HTTP:response tokens:n :}
   READY
   SLOT WRITER ROW JSON:OPEN
   S\" {\qobject\q:\qresponse.input_tokens\q,\qinput_tokens\q:" JSON:WRITE-RAW
   tokens JSON:WRITE-U s" }" JSON:WRITE-RAW
   JSON:CLOSE
   answer s" Content-Type" s" application/json" HTTP:HEADER!
   answer ROW$ HTTP:BODY! ;


\ A compaction of the request's input, the reply OpenAI gives at
\ /responses/compact: one JSON object, no event stream, whose output is
\ exactly the JSON array and whose usage is the terminal replies' own.
: COMPACTION! ( HTTP:response str -- )
   {: answer:HTTP:response items:str :}
   READY
   SLOT WRITER ROW JSON:OPEN
   S\" {\qid\q:\qcmp_1\q,\qobject\q:\qresponse.compaction\q,\qcreated_at\q:0,\qoutput\q:" JSON:WRITE-RAW
   items JSON:WRITE-RAW USAGE s" }" JSON:WRITE-RAW
   JSON:CLOSE
   answer s" Content-Type" s" application/json" HTTP:HEADER!
   answer ROW$ HTTP:BODY! ;

;package
