\ json-rpc-test.f - the JSON-RPC 2.0 envelope, through src/lib/json-rpc.f.
\ Run: bin/hb --load src/lib/json-rpc-test.f

require src/lib/error.f
require src/lib/string.f
require src/lib/test.f
require src/lib/fmt.f
require src/lib/heap.f
require src/lib/vector.f
require src/lib/json.f
require src/lib/json-rpc.f

package JSON-RPC-TEST
using JSON-RPC
using JSON

TYPED-VARIABLE OUT ^VEC:vector<u8>   \ TEST-MAIN makes and deletes it
TYPED-VARIABLE W JSON:writer

: SB-SP ( -- )  s"  " SB-APPEND ;

: VALUE+ ( JSON:value -- )  JSON:TEXT$ SB-APPEND ;

\ Params as their text, nothing for none.
: PARAMS+ ( option<JSON:value> -- )
   MATCH option
      none OF ENDOF
      some OF VALUE+ ENDOF
   ;MATCH ;

: SHOW$ ( JSON-RPC:message -- str )
   SB-RESET
   MATCH JSON-RPC:message
      request OF {: i m p :}
         s" request" SB-APPEND SB-SP i VALUE+ SB-SP
         m VALUE+ SB-SP p PARAMS+ ENDOF
      notification OF {: m p :}
         s" notification" SB-APPEND SB-SP m VALUE+ SB-SP p PARAMS+ ENDOF
      success OF {: i v :}
         s" success" SB-APPEND SB-SP i VALUE+ SB-SP v VALUE+ ENDOF
      failure OF {: i v :}
         s" failure" SB-APPEND SB-SP i VALUE+ SB-SP v VALUE+ ENDOF
      invalid OF {: c:n w:^ wu:n i :}
         s" invalid" SB-APPEND SB-SP c FMT:SB-INT SB-SP i VALUE+ SB-SP
         w wu >str SB-APPEND ENDOF
   ;MATCH
   SB$ ;

: SORTS ( str str -- )
   {: body:str want:str :}
   body >MESSAGE SHOW$ want T$= ;

\ Members in any order and with blanks between; params an object, an array,
\ absent or null; ids as their exact text: an integer, a negative fraction with
\ an exponent, a string with escapes, null.
: TEST-REQUEST ( -- )
   s\" {\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{\"a\":[1]}}"
   s\" request 1 \"initialize\" {\"a\":[1]}" SORTS
   s\" { \"params\" : [1, \"x\"] , \"method\" : \"m\" ,\n\"id\" : -1.5e2 , \"jsonrpc\" : \"2.0\" }"
   s\" request -1.5e2 \"m\" [1, \"x\"]" SORTS
   s\" {\"jsonrpc\":\"2.0\",\"id\":\"a\\\"b\\u0041\",\"method\":\"m\"}"
   s\" request \"a\\\"b\\u0041\" \"m\" " SORTS
   s\" {\"jsonrpc\":\"2.0\",\"id\":null,\"method\":\"shutdown\"}"
   s\" request null \"shutdown\" " SORTS
   s\" {\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"shutdown\",\"params\":null}"
   s\" request 2 \"shutdown\" " SORTS ;

\ Params present, absent or null; member names and the jsonrpc value escaped; an
\ unknown member, even one holding a `method`, passed over.
: TEST-NOTIFICATION ( -- )
   s\" {\"jsonrpc\":\"2.0\",\"method\":\"initialized\",\"params\":{}}"
   s\" notification \"initialized\" {}" SORTS
   s\" {\"jsonrpc\":\"2.0\",\"method\":\"exit\"}"
   s\" notification \"exit\" " SORTS
   s\" {\"jsonrpc\":\"2.0\",\"method\":\"exit\",\"params\":null}"
   s\" notification \"exit\" " SORTS
   s\" {\"j\\u0073onrpc\":\"2\\u002e0\",\"m\\u0065thod\":\"x\\u0079\"}"
   s\" notification \"x\\u0079\" " SORTS
   s\" {\"extra\":{\"method\":\"y\",\"id\":3},\"jsonrpc\":\"2.0\",\"method\":\"x\"}"
   s\" notification \"x\" " SORTS ;

\ Results null, a quoted string and an object; errors with members in any order,
\ data holding a `code`, and a whole code written `1.0`.
: TEST-RESPONSE ( -- )
   s\" {\"jsonrpc\":\"2.0\",\"id\":7,\"result\":null}"
   s\" success 7 null" SORTS
   s\" {\"result\":\"ok\",\"jsonrpc\":\"2.0\",\"id\":\"q\"}"
   s\" success \"q\" \"ok\"" SORTS
   s\" {\"jsonrpc\":\"2.0\",\"id\":8,\"result\":{\"a\":{}}}"
   s\" success 8 {\"a\":{}}" SORTS
   s\" {\"jsonrpc\":\"2.0\",\"id\":9,\"error\":{\"code\":-32601,\"message\":\"m\"}}"
   s\" failure 9 {\"code\":-32601,\"message\":\"m\"}" SORTS
   s\" {\"jsonrpc\":\"2.0\",\"id\":10,\"error\":{\"message\":\"m\",\"data\":{\"code\":\"x\"},\"code\":1}}"
   s\" failure 10 {\"message\":\"m\",\"data\":{\"code\":\"x\"},\"code\":1}" SORTS
   s\" {\"jsonrpc\":\"2.0\",\"id\":11,\"error\":{\"code\":1.0,\"message\":\"m\"}}"
   s\" failure 11 {\"code\":1.0,\"message\":\"m\"}" SORTS ;

: PARSE-HEAD$ ( -- str )  s" invalid -32700 null " ;

\ The body sorts as PARSE-ERROR with the null id.
: NOT-JSON ( str -- )
   >MESSAGE SHOW$ {: s:str :}
   PARSE-HEAD$ STR:LEN s STR:LEN min {: hu:n :}
   s STR:DATA hu >str PARSE-HEAD$ T$= ;

\ A string holding one byte that cannot start UTF-8.
create BAD-UTF8 123 c, 34 c, 109 c, 34 c, 58 c, 34 c, $FF c, 34 c, 125 c,

\ Text that is not JSON is PARSE-ERROR with id null; the first case also checks
\ the reader's text as its why.
: TEST-PARSE ( -- )
   s" "
   s" invalid -32700 null 1:1: expected a value, found the end of the input; write the rest of the document, closing each open array with `]` and object with `}`"
   SORTS
   s" {" NOT-JSON
   s" nul" NOT-JSON
   s\" {\"jsonrpc\":\"2.0\",\"method\":\"x\"} {}" NOT-JSON
   s\" {\"jsonrpc\":\"2.0\",\"method\":\"\\x\"}" NOT-JSON
   s\" {\"jsonrpc\":\"2.0\",\"method\":\"\\ud800\"}" NOT-JSON
   s\" {\"jsonrpc\":\"2.0\",\"method\":\"x\",}" NOT-JSON
   BAD-UTF8 9 >str NOT-JSON ;

\ An array, empty or not; a number, string or null root: INVALID-REQUEST, id null.
: TEST-ROOT ( -- )
   s\" [{\"jsonrpc\":\"2.0\",\"method\":\"x\"}]"
   s" invalid -32600 null an array, a batch this reader does not take" SORTS
   s" []" s" invalid -32600 null an array, a batch this reader does not take" SORTS
   s" 1" s" invalid -32600 null a body that is not an object" SORTS
   s\" \"x\"" s" invalid -32600 null a body that is not an object" SORTS
   s" null" s" invalid -32600 null a body that is not an object" SORTS ;

\ No jsonrpc, "1.0", the number 2.0; a method that is not a string; params a
\ number or string: INVALID-REQUEST with the message's id, or null in a
\ notification.
: TEST-REQUEST-FAULTS ( -- )
   s\" {\"id\":1,\"method\":\"x\"}"
   s\" invalid -32600 1 a jsonrpc member that is not \"2.0\"" SORTS
   s\" {\"jsonrpc\":\"1.0\",\"id\":\"r\",\"method\":\"x\"}"
   s\" invalid -32600 \"r\" a jsonrpc member that is not \"2.0\"" SORTS
   s\" {\"jsonrpc\":2.0,\"method\":\"x\"}"
   s\" invalid -32600 null a jsonrpc member that is not \"2.0\"" SORTS
   s\" {\"jsonrpc\":\"2.0 \",\"id\":2,\"method\":\"x\"}"
   s\" invalid -32600 2 a jsonrpc member that is not \"2.0\"" SORTS
   s\" {\"jsonrpc\":\"2.0\",\"id\":3,\"method\":1}"
   s" invalid -32600 3 a method that is not a string" SORTS
   s\" {\"jsonrpc\":\"2.0\",\"method\":null}"
   s" invalid -32600 null a method that is not a string" SORTS
   s\" {\"jsonrpc\":\"2.0\",\"id\":4,\"method\":\"x\",\"params\":1}"
   s" invalid -32600 4 params that are not an object or array" SORTS
   s\" {\"jsonrpc\":\"2.0\",\"method\":\"x\",\"params\":\"p\"}"
   s" invalid -32600 null params that are not an object or array" SORTS ;

: BAD-ID ( str -- )
   s" invalid -32600 null an id that is not a number, string or null" SORTS ;

\ An id that is an object, array or boolean: INVALID-REQUEST, id null.
: TEST-ENVELOPE-FAULTS ( -- )
   s\" {\"jsonrpc\":\"2.0\",\"id\":{},\"method\":\"x\"}" BAD-ID
   s\" {\"jsonrpc\":\"2.0\",\"id\":[1],\"method\":\"x\"}" BAD-ID
   s\" {\"jsonrpc\":\"2.0\",\"id\":true,\"result\":1}" BAD-ID ;

\ A member named twice in any object, decoded: in the envelope, in params or
\ in an error. INVALID-REQUEST with the reader's text and the null id.
: TEST-REPEAT ( -- )
   s\" {\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"x\",\"method\":\"y\"}"
   s\" invalid -32600 null 1:38: expected a member name not used before in this object, found `\"method\"`; remove the repeated member" SORTS
   s\" {\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"x\",\"params\":{\"a\":1,\"a\":2}}"
   s\" invalid -32600 null 1:54: expected a member name not used before in this object, found `\"a\"`; remove the repeated member" SORTS
   s\" {\"jsonrpc\":\"2.0\",\"id\":3,\"method\":\"x\",\"m\\u0065thod\":\"x\"}"
   s\" invalid -32600 null 1:38: expected a member name not used before in this object, found `\"m\\u0065thod\"`; remove the repeated member" SORTS
   s\" {\"jsonrpc\":\"2.0\",\"id\":4,\"method\":\"x\",\"params\":{},\"params\":{}}"
   s\" invalid -32600 null 1:50: expected a member name not used before in this object, found `\"params\"`; remove the repeated member" SORTS
   s\" {\"jsonrpc\":\"2.0\",\"id\":1,\"id\":2,\"method\":\"x\"}"
   s\" invalid -32600 null 1:25: expected a member name not used before in this object, found `\"id\"`; remove the repeated member" SORTS
   s\" {\"jsonrpc\":\"2.0\",\"id\":5,\"error\":{\"code\":1,\"code\":2,\"message\":\"m\"}}"
   s\" invalid -32600 null 1:43: expected a member name not used before in this object, found `\"code\"`; remove the repeated member" SORTS ;

: NOT-ERROR ( str -- )
   s" invalid -32600 null an error that is not an error object" SORTS ;

\ INVALID-REQUEST, id null: no method or id, no id, both or neither of result and
\ error, and an error not an object, empty, or with a bad code or message.
: TEST-RESPONSE-FAULTS ( -- )
   s\" {\"jsonrpc\":\"2.0\"}"
   s" invalid -32600 null a message with neither a method nor an id" SORTS
   s\" {\"jsonrpc\":\"2.0\",\"result\":1}"
   s" invalid -32600 null a message with neither a method nor an id" SORTS
   s\" {\"jsonrpc\":\"2.0\",\"id\":5,\"result\":1,\"error\":{}}"
   s" invalid -32600 null a response without exactly one of result and error" SORTS
   s\" {\"jsonrpc\":\"2.0\",\"id\":5}"
   s" invalid -32600 null a response without exactly one of result and error" SORTS
   s\" {\"jsonrpc\":\"2.0\",\"id\":5,\"error\":\"e\"}" NOT-ERROR
   s\" {\"jsonrpc\":\"2.0\",\"id\":5,\"error\":{}}" NOT-ERROR
   s\" {\"jsonrpc\":\"2.0\",\"id\":5,\"error\":{\"code\":1.5,\"message\":false}}" NOT-ERROR
   s\" {\"jsonrpc\":\"2.0\",\"id\":5,\"error\":{\"code\":1.5,\"message\":\"m\"}}" NOT-ERROR
   s\" {\"jsonrpc\":\"2.0\",\"id\":5,\"error\":{\"code\":1,\"message\":false}}" NOT-ERROR
   s\" {\"jsonrpc\":\"2.0\",\"id\":5,\"error\":{\"code\":9223372036854775808,\"message\":\"m\"}}" NOT-ERROR ;

: WRITER ( -- ^JSON:writer )
   W OUT @ JSON:OPEN ;

: CLOSED$ ( ^JSON:writer -- str )
   {: w :}
   w WRITTEN$ {: s:str :}
   w JSON:CLOSE
   s ;

: ID ( str -- JSON:value )  JSON:PARSE ;

\ RESULT, ERROR and NOTIFY: exact bytes, a message escaped, each read back by
\ >MESSAGE.
: TEST-WRITE ( -- )
   WRITER s" 2" ID RESULT WRITE-NULL END CLOSED$
   dup s\" {\"jsonrpc\":\"2.0\",\"id\":2,\"result\":null}" T$=
   s" success 2 null" SORTS
   WRITER s\" \"a\\\"b\"" ID RESULT WRITE-OBJECT-START WRITE-OBJECT-END
   END CLOSED$
   dup s\" {\"jsonrpc\":\"2.0\",\"id\":\"a\\\"b\",\"result\":{}}" T$=
   s\" success \"a\\\"b\" {}" SORTS
   WRITER NULL-ID METHOD-NOT-FOUND s\" no \"x\"" ERROR CLOSED$
   dup s\" {\"jsonrpc\":\"2.0\",\"id\":null,\"error\":{\"code\":-32601,\"message\":\"no \\\"x\\\"\"}}" T$=
   s\" failure null {\"code\":-32601,\"message\":\"no \\\"x\\\"\"}" SORTS
   WRITER s" textDocument/publishDiagnostics" NOTIFY
   WRITE-ARRAY-START WRITE-ARRAY-END END CLOSED$
   dup s\" {\"jsonrpc\":\"2.0\",\"method\":\"textDocument/publishDiagnostics\",\"params\":[]}" T$=
   s\" notification \"textDocument/publishDiagnostics\" []" SORTS ;

: TEST-MAIN ( -- )
   T-RESET
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW OUT !
   TEST-REQUEST
   TEST-NOTIFICATION
   TEST-RESPONSE
   TEST-PARSE
   TEST-ROOT
   TEST-REQUEST-FAULTS
   TEST-ENVELOPE-FAULTS
   TEST-REPEAT
   TEST-RESPONSE-FAULTS
   TEST-WRITE
   OUT @ VEC:DELETE
   T-REPORT
   s" json-rpc-test: ok" type cr ;

TEST-MAIN

;using
;using
;package
