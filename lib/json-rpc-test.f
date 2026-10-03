\ json-rpc-test.f - the JSON-RPC 2.0 envelope, through lib/json-rpc.f.
\ Run: bin/hb --load lib/json-rpc-test.f
\
\ The failure list the module was written against, each line with its test:
\ - a request, members in any order and with blanks between, params an object,
\   an array, absent or null; ids as their exact text: an integer, a negative
\   fraction with an exponent, a string with escapes, null ... TEST-REQUEST
\ - a notification, params present, absent or null; member names and the
\   jsonrpc value escaped; an unknown member, even one holding a `method`,
\   passed over ............................................. TEST-NOTIFICATION
\ - a result (null, a string with its quotes, an object) and an error answering
\   an id, the error's members in any order with data holding a `code`
\   ......................................................... TEST-RESPONSE
\ - text that is not JSON: nothing, an open object, a bare word, a value then
\   more, a bad escape, a lone surrogate, a byte that is not UTF-8:
\   PARSE-ERROR, id null .................................... TEST-PARSE
\ - an array, empty or not; a number, string or null for a root: INVALID-REQUEST,
\   id null ................................................. TEST-ROOT
\ - no jsonrpc, "1.0", the number 2.0; a method that is not a string; params a
\   number or string: INVALID-REQUEST with the message's id, or null in a
\   notification ............................................ TEST-REQUEST-FAULTS
\ - an id that is an object, array or boolean: INVALID-REQUEST, id null; an id
\   named twice: the last counts ............................ TEST-ENVELOPE-FAULTS
\ - no method and no id; a result without an id; both result and error;
\   neither; an error that is not an object, an empty one, one whose code is a
\   fraction or whose message is not a string: INVALID-REQUEST, id null
\   ......................................................... TEST-RESPONSE-FAULTS
\ - METHOD? decoded: equal, escaped, a prefix, longer, another name ... TEST-METHOD
\ - >MESSAGE with storage under JR:STORAGE-BYTES: JR:E-CAPACITY  TEST-STORAGE
\ - RESULT, ERROR and NOTIFY: exact bytes, a message escaped, and each read
\   back by >MESSAGE; an empty id: E-JSON-RPC-ID with nothing written
\   ......................................................... TEST-WRITE

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/fmt.f
require lib/json-read.f
require lib/json-write.f
require lib/json-rpc.f

package JSON-RPC-TEST
using JSON-RPC
using JSON-WRITE
using JR

create ST STORAGE-BYTES allot
256 BUFFER: OUT
TYPED-VARIABLE W JSON-WRITE:writer

: SORTED ( ptr u8 n -- JSON-RPC:message )
   {: a:ptr u:n :}
   ST STORAGE-BYTES a u >MESSAGE ;

: SB-SP ( -- )  s"  " SB-APPEND ;

\ The message as one line: its kind, then its parts as text, space-separated.
: SHOW$ ( JSON-RPC:message -- ptr u8 n )
   SB-RESET
   MATCH JSON-RPC:message
      request OF {: i m:ptr mu:n p:ptr pu:n :}
         s" request" SB-APPEND SB-SP i ID$ SB-APPEND SB-SP
         m mu SB-APPEND SB-SP p pu SB-APPEND ENDOF
      notification OF {: m:ptr mu:n p:ptr pu:n :}
         s" notification" SB-APPEND SB-SP m mu SB-APPEND SB-SP p pu SB-APPEND ENDOF
      success OF {: i v:ptr vu:n :}
         s" success" SB-APPEND SB-SP i ID$ SB-APPEND SB-SP v vu SB-APPEND ENDOF
      failure OF {: i v:ptr vu:n :}
         s" failure" SB-APPEND SB-SP i ID$ SB-APPEND SB-SP v vu SB-APPEND ENDOF
      invalid OF {: c:n w:ptr wu:n i :}
         s" invalid" SB-APPEND SB-SP c FMT:SB-INT SB-SP i ID$ SB-APPEND SB-SP
         w wu SB-APPEND ENDOF
   ;MATCH
   SB$ ;

\ Reads a body and expects the message shown as this line.
: SORTS ( ptr u8 n ptr u8 n -- )
   {: a:ptr u:n want:ptr wu:n :}
   a u SORTED SHOW$ want wu T$= ;

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
   s\" failure 10 {\"message\":\"m\",\"data\":{\"code\":\"x\"},\"code\":1}" SORTS ;

: NOT-JSON ( ptr u8 n -- )
   s" invalid -32700 null a body that is not JSON" SORTS ;

\ A string holding one byte that cannot start UTF-8.
create BAD-UTF8 123 c, 34 c, 109 c, 34 c, 58 c, 34 c, $FF c, 34 c, 125 c,

: TEST-PARSE ( -- )
   s" " NOT-JSON
   s" {" NOT-JSON
   s" nul" NOT-JSON
   s\" {\"jsonrpc\":\"2.0\",\"method\":\"x\"} {}" NOT-JSON
   s\" {\"jsonrpc\":\"2.0\",\"method\":\"\\x\"}" NOT-JSON
   s\" {\"jsonrpc\":\"2.0\",\"method\":\"\\ud800\"}" NOT-JSON
   s\" {\"jsonrpc\":\"2.0\",\"method\":\"x\",}" NOT-JSON
   BAD-UTF8 9 NOT-JSON ;

: TEST-ROOT ( -- )
   s\" [{\"jsonrpc\":\"2.0\",\"method\":\"x\"}]"
   s" invalid -32600 null an array, a batch this reader does not take" SORTS
   s" []" s" invalid -32600 null an array, a batch this reader does not take" SORTS
   s" 1" s" invalid -32600 null a body that is not an object" SORTS
   s\" \"x\"" s" invalid -32600 null a body that is not an object" SORTS
   s" null" s" invalid -32600 null a body that is not an object" SORTS ;

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

: BAD-ID ( ptr u8 n -- )
   s" invalid -32600 null an id that is not a number, string or null" SORTS ;

: TEST-ENVELOPE-FAULTS ( -- )
   s\" {\"jsonrpc\":\"2.0\",\"id\":{},\"method\":\"x\"}" BAD-ID
   s\" {\"jsonrpc\":\"2.0\",\"id\":[1],\"method\":\"x\"}" BAD-ID
   s\" {\"jsonrpc\":\"2.0\",\"id\":true,\"result\":1}" BAD-ID
   s\" {\"jsonrpc\":\"2.0\",\"id\":1,\"id\":2,\"method\":\"x\"}"
   s\" request 2 \"x\" " SORTS ;

: NOT-ERROR ( ptr u8 n -- )
   s" invalid -32600 null an error that is not an error object" SORTS ;

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
   s\" {\"jsonrpc\":\"2.0\",\"id\":5,\"error\":{\"code\":1,\"message\":false}}" NOT-ERROR ;

: NAMES? ( ptr u8 n ptr u8 n -- bool )
   {: m:ptr mu:n name:ptr nu:n :}
   ST STORAGE-BYTES m mu name nu METHOD? ;

: TEST-METHOD ( -- )
   s\" \"initialize\"" s" initialize" NAMES? TTRUE
   s\" \"\\u0069nitiali\\u007ae\"" s" initialize" NAMES? TTRUE
   s\" \"initialize\"" s" init" NAMES? TFALSE
   s\" \"init\"" s" initialize" NAMES? TFALSE
   s\" \"initialized\"" s" initialize" NAMES? TFALSE
   s\" \"\"" s" " NAMES? TTRUE ;

: SHORT-READ ( -- )
   ST STORAGE-BYTES 1- s" {}" >MESSAGE SHOW$ 2drop ;

: TEST-STORAGE ( -- )
   [: SHORT-READ ;] E-CAPACITY TTHROWSQ ;

: WRITER ( -- ptr JSON-WRITE:writer )
   W OUT 256 JSON-WRITE:OPEN ;

\ The bytes written, then the writer closed.
: WRITTEN$ ( ptr JSON-WRITE:writer -- ptr u8 n )
   {: w :}
   w $ {: a:ptr u:n :}
   w JSON-WRITE:CLOSE
   a u ;

: ID ( ptr u8 n -- JSON-RPC:id )  JSON--RPC-ID:MAKE ;

: EMPTY-RESULT ( -- )
   WRITER s" " ID RESULT WRITTEN$ 2drop ;

: EMPTY-ERROR ( -- )
   WRITER s" " ID INTERNAL-ERROR s" m" ERROR WRITTEN$ 2drop ;

: TEST-WRITE ( -- )
   WRITER s" 2" ID RESULT NULL END WRITTEN$
   2dup s\" {\"jsonrpc\":\"2.0\",\"id\":2,\"result\":null}" T$=
   s" success 2 null" SORTS
   WRITER s\" \"a\\\"b\"" ID RESULT OBJECT-START OBJECT-END
   END WRITTEN$
   2dup s\" {\"jsonrpc\":\"2.0\",\"id\":\"a\\\"b\",\"result\":{}}" T$=
   s\" success \"a\\\"b\" {}" SORTS
   WRITER NULL-ID METHOD-NOT-FOUND s\" no \"x\"" ERROR WRITTEN$
   2dup s\" {\"jsonrpc\":\"2.0\",\"id\":null,\"error\":{\"code\":-32601,\"message\":\"no \\\"x\\\"\"}}" T$=
   s\" failure null {\"code\":-32601,\"message\":\"no \\\"x\\\"\"}" SORTS
   WRITER s" textDocument/publishDiagnostics" NOTIFY
   ARRAY-START ARRAY-END END WRITTEN$
   2dup s\" {\"jsonrpc\":\"2.0\",\"method\":\"textDocument/publishDiagnostics\",\"params\":[]}" T$=
   s\" notification \"textDocument/publishDiagnostics\" []" SORTS
   [: EMPTY-RESULT ;] E-JSON-RPC-ID TTHROWSQ
   W WRITTEN$ nip 0 T=
   [: EMPTY-ERROR ;] E-JSON-RPC-ID TTHROWSQ
   W WRITTEN$ nip 0 T= ;

: TEST-MAIN ( -- )
   T-RESET
   TEST-REQUEST
   TEST-NOTIFICATION
   TEST-RESPONSE
   TEST-PARSE
   TEST-ROOT
   TEST-REQUEST-FAULTS
   TEST-ENVELOPE-FAULTS
   TEST-RESPONSE-FAULTS
   TEST-METHOD
   TEST-STORAGE
   TEST-WRITE
   T-REPORT
   s" json-rpc-test: ok" type cr ;

TEST-MAIN

;using
;using
;using
;package
