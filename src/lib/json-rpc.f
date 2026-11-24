split-str
\ json-rpc.f - the JSON-RPC 2.0 envelope: >MESSAGE sorts one body into a
\ message and the writers build replies on a caller's JSON writer. Each part
\ of a message is a JSON:value, so an id, a method or a string result keeps
\ its quotes and escapes in its TEXT$. Params absent or null are none. An
\ invalid message's why is program text, or the reader's REASON text for a
\ body that is not JSON (PARSE-ERROR) or that names a member twice in any
\ object (INVALID-REQUEST), valid until the task's next REASON:THROW.
\ INVALID-REQUEST is also an array (a batch, not taken), a root that is not an
\ object, `jsonrpc` other than "2.0", an id not a number, string or null, a
\ method not a string, params not an object, array or null, and a message with
\ no method unless it has an id and exactly one of result and error, an error
\ being an object with a string message and a whole-number code that fits a
\ cell. Member names compare decoded. A body that is not JSON or names a
\ member twice carries the null id; any other invalid message carries its own
\ id when that id is valid and it has a method, else the null id, so a reply
\ never answers a response. STORAGE CLASS. CALLER-OWNED: the message's values
\ borrow the caller's body, the null id program text; no cursor, no module
\ state.

\ Framing is src/lib/content-length.f's.

require src/lib/adt/option.f
require src/lib/json.f
require src/lib/reason.f

package JSON-RPC
using JSON
public

-32700 constant PARSE-ERROR
-32600 constant INVALID-REQUEST
-32601 constant METHOD-NOT-FOUND
-32602 constant INVALID-PARAMS
-32603 constant INTERNAL-ERROR

ENUM message
   VARIANT request
      FIELD id JSON:value FIELD method JSON:value
      FIELD params option<JSON:value>
   ;VARIANT
   VARIANT notification
      FIELD method JSON:value FIELD params option<JSON:value>
   ;VARIANT
   VARIANT success FIELD id JSON:value FIELD value JSON:value ;VARIANT
   VARIANT failure FIELD id JSON:value FIELD value JSON:value ;VARIANT
   VARIANT invalid
      FIELD code n FIELD why ^u8 FIELD why-len n FIELD id JSON:value
   ;VARIANT
;ENUM

: NULL-ID ( -- JSON:value )  s" null" PARSE ;

private

: INVALID ( n str JSON:value -- message )  JSON--RPC-MESSAGE:INVALID ;

: FAULT ( str JSON:value -- message )
   {: why:^ wu:n id :}
   INVALID-REQUEST why wu id INVALID ;

: PRESENT? ( option<JSON:value> -- bool )
   MATCH option
      none OF false ENDOF
      some OF drop true ENDOF
   ;MATCH ;

\ Whether the member is there and q holds of its value.
: HOLDS? ( option<JSON:value> [ JSON:value -- bool ] -- bool )
   {: o q :}
   o MATCH option
      none OF false ENDOF
      some OF q execute ENDOF
   ;MATCH ;

: ID-KIND? ( JSON:value -- bool )
   {: v :}
   v NUMBER? v STRING? or v NULL? or ;

: V2? ( JSON:value -- bool )
   s" jsonrpc" FIND [: s" 2.0" STRING= ;] HOLDS? ;

\ An object with an integer code and a string message.
: ERROR-OBJECT? ( JSON:value -- bool )
   {: e :}
   e OBJECT? 0= if false exit then
   e s" code" FIND [: INTEGER? ;] HOLDS?
   e s" message" FIND [: STRING? ;] HOLDS? and ;

\ The id a fault answers: the message's own when it has a method, else null.
: REPLY-ID ( option<JSON:value> option<JSON:value> -- JSON:value )
   {: i m :}
   m PRESENT? 0= if NULL-ID exit then
   i MATCH option
      none OF NULL-ID ENDOF
      some OF ENDOF
   ;MATCH ;

\ Params that are there as a message carries them, none for null, and
\ whether they are null, an object or an array.
: GIVEN-PARAMS ( JSON:value -- option<JSON:value> bool )
   {: p :}
   p NULL? if OPTION:NONE true exit then
   p OPTION:SOME  p OBJECT? p ARRAY? or ;

\ The params as a message carries them, and whether they are allowed.
: PARAMS ( option<JSON:value> -- option<JSON:value> bool )
   MATCH option
      none OF OPTION:NONE true ENDOF
      some OF GIVEN-PARAMS ENDOF
   ;MATCH ;

: CALLED ( option<JSON:value> JSON:value option<JSON:value> -- message )
   {: i m p :}
   i MATCH option
      none OF m p JSON--RPC-MESSAGE:NOTIFICATION ENDOF
      some OF m p JSON--RPC-MESSAGE:REQUEST ENDOF
   ;MATCH ;

\ A message with a method: a request with an id, else a notification.
: CALL ( option<JSON:value> JSON:value JSON:value JSON:value -- message )
   {: i m root reply :}
   m STRING? 0= if
      s" a method that is not a string" reply FAULT exit
   then
   root s" params" FIND PARAMS {: p ok:bool :}
   ok 0= if
      s" params that are not an object or array" reply FAULT exit
   then
   i m p CALLED ;

: NOT-ONE ( -- message )
   s" a response without exactly one of result and error" NULL-ID FAULT ;

: FAILED ( JSON:value option<JSON:value> -- message )
   {: id x :}
   x MATCH option
      none OF NOT-ONE ENDOF
      some OF {: e :}
         e ERROR-OBJECT? if id e JSON--RPC-MESSAGE:FAILURE else
            s" an error that is not an error object" NULL-ID FAULT
         then
      ENDOF
   ;MATCH ;

\ A response answering this id.
: RESPONSE ( JSON:value JSON:value -- message )
   {: id root :}
   root s" error" FIND {: x :}
   root s" result" FIND MATCH option
      none OF id x FAILED ENDOF
      some OF {: r :}
         x PRESENT? if NOT-ONE else
            id r JSON--RPC-MESSAGE:SUCCESS
         then
      ENDOF
   ;MATCH ;

\ A message with no method: a result or an error answering its id.
: ANSWER ( option<JSON:value> JSON:value -- message )
   {: i root :}
   i MATCH option
      none OF
         s" a message with neither a method nor an id" NULL-ID FAULT
      ENDOF
      some OF root RESPONSE ENDOF
   ;MATCH ;

\ A root object whose id, if any, is a number, string or null.
: ENVELOPE ( option<JSON:value> option<JSON:value> JSON:value -- message )
   {: i m root :}
   i m REPLY-ID {: reply :}
   root V2? 0= if
      s\" a jsonrpc member that is not \"2.0\"" reply FAULT exit
   then
   m MATCH option
      none OF i root ANSWER ENDOF
      some OF {: mv :} i mv root reply CALL ENDOF
   ;MATCH ;

: SORT-OBJECT ( JSON:value -- message )
   {: root :}
   root s" id" FIND {: i :}
   i [: ID-KIND? 0= ;] HOLDS? if
      s" an id that is not a number, string or null" NULL-ID FAULT exit
   then
   i root s" method" FIND root ENVELOPE ;

: SORT-ROOT ( JSON:value -- message )
   {: root :}
   root ARRAY? if
      s" an array, a batch this reader does not take" NULL-ID FAULT exit
   then
   root OBJECT? 0= if
      s" a body that is not an object" NULL-ID FAULT exit
   then
   root SORT-OBJECT ;

\ The JSON reader's codes for text that is not JSON: E-JSON-READ-MALFORMED
\ through E-JSON-READ-COMMA.
: SYNTAX? ( n -- bool )
   {: code:n :}
   code E-JSON-READ-COMMA >= code E-JSON-READ-MALFORMED <= and ;

\ E-MEMBER, a member named twice, is INVALID-REQUEST and a syntax code
\ PARSE-ERROR; any other code, such as JSON:E-READ-SOURCE or a failed growth
\ of the reader's storage, is misuse and is thrown.
: NOT-PARSED ( n -- message )
   {: code:n :}
   code E-MEMBER = if
      INVALID-REQUEST code REASON:TEXT$ NULL-ID INVALID exit
   then
   code SYNTAX? 0= if code throw then
   PARSE-ERROR code REASON:TEXT$ NULL-ID INVALID ;

\ PARSE's value replaces the slot under the body, so the quotation under
\ catch leaves the types it was given.
: PARSE-SLOT ( JSON:value str -- JSON:value str )
   {: v body:^ len:n :}
   body len PARSE  body len ;

public

: >MESSAGE ( str -- message )
   {: body:^ len:n :}
   NULL-ID body len [: PARSE-SLOT ;] catch {: code:n :}
   code 0<> if 2drop drop code NOT-PARSED exit then
   2drop SORT-ROOT ;

private

: HEAD ( ^JSON:writer -- ^JSON:writer )
   WRITE-OBJECT-START  s" jsonrpc" s" 2.0" WRITE-FIELD-S ;

: ID-FIELD ( ^JSON:writer JSON:value -- ^JSON:writer )
   {: id :}
   WRITE-COMMA  s" id" id TEXT$ WRITE-FIELD-RAW ;

public

\ `{"jsonrpc":"2.0","id":ID,"result":`; the caller writes the value, then END.
: RESULT ( ^JSON:writer JSON:value -- ^JSON:writer )
   {: id :}
   HEAD id ID-FIELD
   WRITE-COMMA  s" result" WRITE-KEY ;

\ A whole error reply: `{"jsonrpc":"2.0","id":ID,"error":{"code":N,"message":M}}`.
: ERROR ( ^JSON:writer JSON:value n str -- ^JSON:writer )
   {: id code:n msg:^ mu:n :}
   HEAD id ID-FIELD
   WRITE-COMMA  s" error" WRITE-KEY
   WRITE-OBJECT-START
   s" code" code WRITE-FIELD-INT
   WRITE-COMMA  s" message" msg mu WRITE-FIELD-S
   WRITE-OBJECT-END
   WRITE-OBJECT-END ;

\ `{"jsonrpc":"2.0","method":M,"params":`; the caller writes params, then END.
: NOTIFY ( ^JSON:writer str -- ^JSON:writer )
   {: m:^ mu:n :}
   HEAD
   WRITE-COMMA  s" method" m mu WRITE-FIELD-S
   WRITE-COMMA  s" params" WRITE-KEY ;

: END ( ^JSON:writer -- ^JSON:writer )
   WRITE-OBJECT-END ;

;using
;package
