\ test-model-test.f - MODEL:CHAT against a loopback HTTP server whose
\ /responses routes answer through TEST-MODEL: a completed reply gives its text,
\ JSON escapes included; an incomplete one MODEL:E-INCOMPLETE with its reason;
\ a refusal MODEL:E-REFUSED; a tool reply answers its output array whole; a
\ count answers the input tokens the server counted; a compaction's usage
\ reaches the call; a halt in the recorder leaves the call free to close. Every
\ key item reads as one key from a store the test binds with SECRET:STORE!, so
\ none reaches the desktop store.
\ Run: bin/hb --load src/lib/net/test-model-test.f

require src/lib/test.f
require src/lib/string.f
require src/lib/span.f
require src/lib/fmt.f
require src/lib/aio.f
require src/lib/secret.f
require src/lib/task.f
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

$10000 TASK:TASK WORK-TASK
TASK:SEMAPHORE IN-REC
TASK:SEMAPHORE GO

: KEY$ ( -- ptr u8 n ) s" test-model-key" ;
: TEXT$ ( -- ptr u8 n ) S\" {\qanswer\q:\qsay \\\qhi\\\q\q}" ;
: REASON$ ( -- ptr u8 n ) s" max_output_tokens" ;

: SCHEMA$ ( -- ptr u8 n )
   S\" {\qtype\q:\qobject\q,\qproperties\q:{\qanswer\q:{\qtype\q:\qstring\q}},\qrequired\q:[\qanswer\q]}" ;
: TOOLS$ ( -- ptr u8 n )
   S\" [{\qtype\q:\qfunction\q,\qname\q:\qlookup\q,\qparameters\q:{\qtype\q:\qobject\q,\qproperties\q:{}}}]" ;
: TOOL-OUTPUT$ ( -- ptr u8 n )
   S\" [{\qtype\q:\qreasoning\q,\qid\q:\qrs_1\q,\qsummary\q:[]},{\qtype\q:\qfunction_call\q,\qid\q:\qfc_1\q,\qcall_id\q:\qcall_1\q,\qname\q:\qlookup\q,\qarguments\q:\q{}\q,\qstatus\q:\qcompleted\q}]" ;


: WINDOW$ ( -- ptr u8 n )
   S\" [{\qtype\q:\qmessage\q,\qrole\q:\quser\q,\qcontent\q:[{\qtype\q:\qinput_text\q,\qtext\q:\qkept\q}]},{\qtype\q:\qcompaction\q,\qid\q:\qcmp_item\q,\qencrypted_content\q:\qsealed\q}]" ;


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
   s" POST" s" /refusal/responses" [: nip s" cannot comply" TEST-MODEL:REFUSAL! ;] HTTP:ROUTE
   s" POST" s" /tools/responses" [: nip TOOL-OUTPUT$ TEST-MODEL:OUTPUT! ;] HTTP:ROUTE
   s" POST" s" /count/responses/input_tokens" [: nip 11 TEST-MODEL:INPUT-TOKENS! ;] HTTP:ROUTE
   s" POST" s" /compact/responses/compact" [: nip WINDOW$ TEST-MODEL:COMPACTION! ;] HTTP:ROUTE ;


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
   M MODEL:CHAT TEXT$ T$=
   s" a completed reply's usage reaches the call" T-LABEL
   M MODEL:INPUT-TOKENS 11 T=
   M MODEL:CACHED-TOKENS 4 T= ;


: INCOMPLETE ( -- )
   s" an incomplete reply is E-INCOMPLETE with its reason" T-LABEL
   s" /incomplete" AT
   [: M MODEL:CHAT 2drop ;] MODEL:E-INCOMPLETE TTHROWSQ
   M MODEL:INCOMPLETE-REASON$ REASON$ T$= ;


: REFUSAL ( -- )
   s" a refusal is E-REFUSED" T-LABEL
   s" /refusal" AT
   [: M MODEL:CHAT 2drop ;] MODEL:E-REFUSED TTHROWSQ ;


: TOOLS ( -- )
   s" a tool reply answers its output array whole" T-LABEL
   TOOLS$ M MODEL:TOOLS!
   s" /tools" AT
   M MODEL:CHAT nip 0 T=
   M MODEL:OUTPUT$ TOOL-OUTPUT$ T$=
   SCHEMA$ M MODEL:SCHEMA! ;


\ The call is a schema call, as SETUP and TOOLS leave it.
: COUNTED ( -- )
   s" a count answers the input tokens the server counted" T-LABEL
   s" /count" AT
   M MODEL:COUNT-INPUT 11 T= ;


\ A one-part schema call, as COUNTED leaves it.
: COMPACTED ( -- )
   s" a compaction's usage reaches the call" T-LABEL
   s" /compact" AT
   M MODEL:COMPACT
   M MODEL:INPUT-TOKENS 11 T= ;


: RECORD ( n ptr u8 n -- n )
   2drop drop
   IN-REC TASK:SIGNAL-SEM
   GO TASK:WAIT-SEM
   TASK:PAUSE
   7 ;


: WORK ( -- )
   [: M MODEL:CHAT 2drop ;] catch
   {: code:n :}
   IN-REC TASK:SIGNAL-SEM
   code 0<> if code throw then
   0 TASK:RETURN ;


: HALTED ( -- )
   s" a halt in the recorder leaves the call free to close" T-LABEL
   s" /completed" AT
   0 [: RECORD ;] [: 2drop ;] M MODEL:RECORDER!
   s" probe" M MODEL:PURPOSE!
   0 IN-REC TASK:SEMAPHORE-INIT
   0 GO TASK:SEMAPHORE-INIT
   ['] WORK WORK-TASK TASK:ACTIVATE
   IN-REC TASK:WAIT-SEM
   WORK-TASK TASK:HALT
   GO TASK:SIGNAL-SEM
   WORK-TASK TASK:JOIN MATCH result
      ok OF drop T-FAIL ENDOF
      err OF E-TASK-NO-RESULT T= ENDOF
   ;MATCH
   [: M MODEL:CLOSE ;] catch 0 T=
   [: M MODEL:CLOSE ;] MODEL:E-STATE TTHROWSQ
   IN-REC TASK:SEMAPHORE-DESTROY
   GO TASK:SEMAPHORE-DESTROY ;


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
   COMPLETED INCOMPLETE REFUSAL TOOLS COUNTED COMPACTED HALTED
   HTTP:STOP
   CURL:LOOP-STOP
   AIO:STOP ;

T-RESET
CASES
T-REPORT

;package
