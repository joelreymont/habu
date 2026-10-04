\ json-rpc.f - the JSON-RPC 2.0 envelope: what one message is, and its replies.
\
\ >MESSAGE sorts one message body into a JSON-RPC:message: a request (id,
\ method, params), a notification (method, params), a result or an error
\ answering an id, or an invalid message carrying the code and id its reply
\ takes. Every part is the body's own JSON text, borrowed: an id, a method or a
\ string result keeps its quotes and escapes, and params or a result is the
\ whole value. Params absent or null are empty. METHOD? compares a method's
\ text with a name after decoding it, so `"initialize"` names initialize.
\
\ >MESSAGE reads the body once, recording each member of a root object that
\ the envelope names as it meets it. A body that is not JSON (RFC 8259, as
\ lib/json-read.f reads it) is invalid with PARSE-ERROR. INVALID-REQUEST covers
\ the rest: an array (a batch, which this reader does not take), a root that is
\ not an object, `jsonrpc` other than the string "2.0", an id that is not a
\ number, string or null, a method that is not a string, params that are not an
\ object, an array or null, and a message with no method unless it has an id
\ and exactly one of result and error, an error being an error object: an
\ object whose code is an integer and whose message is a string. Member names
\ are compared decoded, the last of a repeated name counts, and other members
\ are passed over. An invalid message carries its own id when that id is valid
\ and the message has a method, else the null id, so a reply never answers a
\ response.
\
\ The writers build replies on a caller's JSON-WRITE writer, member by member:
\ RESULT writes the envelope up to `"result":`, the caller writes the value and
\ END closes it; ERROR writes a whole error reply; NOTIFY writes a notification
\ up to `"params":`, closed by END after the caller's params. A reply needs an
\ id: an empty one is E-JSON-RPC-ID before any byte is written. The error codes
\ of the specification are constants; framing is lib/content-length.f's.
\
\ STORAGE CLASS. CALLER-OWNED. >MESSAGE parses in the caller's JR:STORAGE-BYTES
\ of storage (a smaller one is JR:E-CAPACITY) and the message borrows the
\ caller's body; the module keeps no state.

require lib/errors.f
require lib/string.f
require lib/json-read.f
require lib/json-write.f

package JSON-RPC
using JR
using JSON-WRITE
public

-32700 constant PARSE-ERROR
-32600 constant INVALID-REQUEST
-32601 constant METHOD-NOT-FOUND
-32602 constant INVALID-PARAMS
-32603 constant INTERNAL-ERROR

\ An id as the JSON text it came in: a number's digits, a string with its
\ quotes, or `null`.
STRUCTURE id 0
   FIELD json ptr u8
   FIELD len n
;STRUCTURE

ENUM message 0
   VARIANT request FIELD id id FIELD method ptr u8 FIELD method-len n FIELD params ptr u8 FIELD params-len n ;VARIANT
   VARIANT notification FIELD method ptr u8 FIELD method-len n FIELD params ptr u8 FIELD params-len n ;VARIANT
   VARIANT success FIELD id id FIELD value ptr u8 FIELD value-len n ;VARIANT
   VARIANT failure FIELD id id FIELD value ptr u8 FIELD value-len n ;VARIANT
   VARIANT invalid FIELD code n FIELD why ptr u8 FIELD why-len n FIELD id id ;VARIANT
;ENUM

: ID$ ( id -- ptr u8 n )  JSON--RPC-ID:UNMAKE ;
: NULL-ID ( -- id )  s" null" JSON--RPC-ID:MAKE ;

\ Whether a method's JSON text, as >MESSAGE gives it, names this method.
: METHOD? ( ptr n n ptr u8 n ptr u8 n -- bool )
   {: st:ptr cap:n m:ptr mu:n name:ptr nu:n :}
   st cap m mu INIT
   NEXT drop
   name nu STR-EQ? {: same:bool :}
   JR:CLOSE
   same ;

private

-1 constant ABSENT              \ the kind of a member the envelope lacks

\ One envelope member: its token kind, ABSENT when the root has none, and its
\ JSON text.
STRUCTURE member 0
   FIELD kind n
   FIELD json ptr u8
   FIELD len n
;STRUCTURE

: KIND ( member -- n )  MEMBER-UNMAKE 2drop ;
: TEXT$ ( member -- ptr u8 n )  MEMBER-UNMAKE rot drop ;
: PRESENT? ( member -- bool )  KIND ABSENT <> ;
: >ID ( member -- id )  TEXT$ JSON--RPC-ID:MAKE ;
: NO-MEMBER ( -- member )  ABSENT s" " MEMBER-MAKE ;

\ What the walk over a root object met: whether jsonrpc is the string "2.0";
\ the id, method, params, result and error; whether that error is an error
\ object.
STRUCTURE envelope 0
   FIELD v2 bool
   FIELD id member
   FIELD method member
   FIELD params member
   FIELD result member
   FIELD error member
   FIELD error-ok bool
;STRUCTURE

: NO-ENVELOPE ( -- envelope )
   false NO-MEMBER NO-MEMBER NO-MEMBER NO-MEMBER NO-MEMBER false ENVELOPE-MAKE ;

: INVALID ( n ptr u8 n id -- message )  JSON--RPC-MESSAGE:invalid ;

: NOT-JSON ( -- message )
   PARSE-ERROR s" a body that is not JSON" NULL-ID INVALID ;

\ JR's codes for text that is not JSON; the others are misuse.
: SYNTAX? ( n -- bool )
   {: code:n :}
   code E-JR-COMMA >= code E-JR-MALFORMED <= and ;

: ID-KIND? ( n -- bool )
   {: k:n :}
   k T-INT = k T-FLOAT = or k T-STR = or k T-NULL = or ;

: CONTAINER? ( n -- bool )
   {: k:n :}
   k T-OBJ = k T-ARR = or ;

\ The current value as a member: a container whole, a string with its quotes.
: >MEMBER ( JR:reader -- JR:reader member )
   TOKEN {: kind:n :}
   kind CONTAINER? if VALUE-SPAN$ else SPAN$ then {: a:ptr u:n :}
   kind T-STR = if kind a 1- u 2 + MEMBER-MAKE exit then
   kind a u MEMBER-MAKE ;

\ Whether the current value is the string "2.0".
: V2? ( JR:reader -- JR:reader bool )
   TOKEN T-STR = if s" 2.0" STR-EQ? exit then
   SKIP-VALUE false ;

\ The next member of an error object, given whether its code is an integer and
\ its message a string so far; false after the last.
: ERROR-MEMBER ( JR:reader bool bool -- JR:reader bool bool bool )
   {: code:bool text:bool :}
   NEXT T-KEY <> if code text false exit then
   s" code" STR-EQ? {: at-code:bool :}
   s" message" STR-EQ? {: at-text:bool :}
   NEXT {: kind:n :}
   SKIP-VALUE
   at-code if kind T-INT = else code then
   at-text if kind T-STR = else text then
   true ;

\ The current value as the error member, and whether it is an error object.
: ERROR-VALUE ( JR:reader -- JR:reader member bool )
   TOKEN T-OBJ <> if >MEMBER false exit then
   SPAN$ drop {: open:ptr :}
   false false begin ERROR-MEMBER 0= until and {: ok:bool :}
   SPAN$ drop {: close:ptr :}
   T-OBJ open close 1+ open - MEMBER-MAKE ok ;

\ The member at the current key: recorded when the envelope names it, over any
\ earlier one of that name, else passed over.
: RECORD ( JR:reader envelope -- JR:reader envelope )
   {: e :}
   e ENVELOPE-UNMAKE {: v:bool i m p r x ok:bool :}
   s" jsonrpc" STR-EQ? if NEXT drop V2? i m p r x ok ENVELOPE-MAKE exit then
   s" id" STR-EQ? if NEXT drop >MEMBER {: n :} v n m p r x ok ENVELOPE-MAKE exit then
   s" method" STR-EQ? if NEXT drop >MEMBER {: n :} v i n p r x ok ENVELOPE-MAKE exit then
   s" params" STR-EQ? if NEXT drop >MEMBER {: n :} v i m n r x ok ENVELOPE-MAKE exit then
   s" result" STR-EQ? if NEXT drop >MEMBER {: n :} v i m p n x ok ENVELOPE-MAKE exit then
   s" error" STR-EQ? if
      NEXT drop ERROR-VALUE {: n good:bool :} v i m p r n good ENVELOPE-MAKE exit
   then
   NEXT drop SKIP-VALUE e ;

\ The next member of the root object, recorded; false after the last.
: ROOT-MEMBER ( JR:reader envelope -- JR:reader envelope bool )
   {: e :}
   NEXT T-KEY <> if e false exit then
   e RECORD true ;

\ A message with a method: a request with an id, else a notification.
: CALL ( member member member id -- message )
   {: i m p reply :}
   m KIND T-STR <> if
      INVALID-REQUEST s" a method that is not a string" reply INVALID exit
   then
   p KIND {: pk:n :}
   pk CONTAINER? pk ABSENT = or pk T-NULL = or 0= if
      INVALID-REQUEST s" params that are not an object or array" reply INVALID exit
   then
   pk CONTAINER? if p TEXT$ else s" " then {: pa:ptr pu:n :}
   i PRESENT? if reply m TEXT$ pa pu JSON--RPC-MESSAGE:request exit then
   m TEXT$ pa pu JSON--RPC-MESSAGE:notification ;

\ A message with no method: a result or an error answering its id.
: ANSWER ( member member member bool -- message )
   {: i r x ok:bool :}
   i PRESENT? 0= if
      INVALID-REQUEST s" a message with neither a method nor an id" NULL-ID INVALID exit
   then
   r PRESENT? x PRESENT? xor 0= if
      INVALID-REQUEST s" a response without exactly one of result and error" NULL-ID INVALID exit
   then
   r PRESENT? if i >ID r TEXT$ JSON--RPC-MESSAGE:success exit then
   ok 0= if
      INVALID-REQUEST s" an error that is not an error object" NULL-ID INVALID exit
   then
   i >ID x TEXT$ JSON--RPC-MESSAGE:failure ;

\ The message a root object's envelope makes.
: SORT ( envelope -- message )
   ENVELOPE-UNMAKE {: v:bool i m p r x ok:bool :}
   i PRESENT? if i KIND ID-KIND? 0= if
      INVALID-REQUEST s" an id that is not a number, string or null" NULL-ID INVALID exit
   then then
   m PRESENT? i PRESENT? and if i >ID else NULL-ID then {: reply :}
   v 0= if
      INVALID-REQUEST s\" a jsonrpc member that is not \"2.0\"" reply INVALID exit
   then
   m PRESENT? if i m p reply CALL exit then
   i r x ok ANSWER ;

\ The message a root of this kind makes, with the envelope its members gave.
: SORT-ROOT ( n envelope -- message )
   {: root:n e :}
   root T-ARR = if
      INVALID-REQUEST s" an array, a batch this reader does not take" NULL-ID INVALID exit
   then
   root T-OBJ <> if
      INVALID-REQUEST s" a body that is not an object" NULL-ID INVALID exit
   then
   e SORT ;

\ Reads the body once - its root, every member of a root object, then its end,
\ which NEXT answers or refuses with E-JR-TRAILING - and sorts it in place of
\ the message given. Text that is not JSON throws JR's code first.
: READ-BODY ( message ptr n n ptr u8 n -- message ptr n n ptr u8 n )
   {: prior st:ptr cap:n body:ptr len:n :}
   st cap body len INIT
   NEXT {: root:n :}
   root T-OBJ = if
      NO-ENVELOPE begin ROOT-MEMBER 0= until
   else
      SKIP-VALUE NO-ENVELOPE
   then {: e :}
   NEXT drop
   JR:CLOSE
   root e SORT-ROOT  st cap body len ;

public

\ One message body, sorted. The storage is JR's, at least JR:STORAGE-BYTES.
: >MESSAGE ( ptr n n ptr u8 n -- message )
   {: st:ptr cap:n body:ptr len:n :}
   NOT-JSON st cap body len [: READ-BODY ;] catch {: code:n :}
   2drop 2drop {: m :}
   code 0= if m exit then
   code SYNTAX? 0= if code throw then
   NOT-JSON ;

private

: ID-TEXT$ ( id -- ptr u8 n )
   ID$ {: a:ptr u:n :}
   u 0 <= if E-JSON-RPC-ID throw then
   a u ;

: HEAD ( ptr JSON-WRITE:writer -- ptr JSON-WRITE:writer )
   OBJECT-START  s" jsonrpc" s" 2.0" FIELD-S ;

: ID-FIELD ( ptr JSON-WRITE:writer ptr u8 n -- ptr JSON-WRITE:writer )
   {: a:ptr u:n :}
   COMMA  s" id" a u FIELD-RAW ;

public

\ `{"jsonrpc":"2.0","id":ID,"result":`; the caller writes the value, then END.
: RESULT ( ptr JSON-WRITE:writer id -- ptr JSON-WRITE:writer )
   ID-TEXT$ {: a:ptr u:n :}
   HEAD a u ID-FIELD
   COMMA  s" result" KEY ;

\ A whole error reply: `{"jsonrpc":"2.0","id":ID,"error":{"code":N,"message":M}}`.
: ERROR ( ptr JSON-WRITE:writer id n ptr u8 n -- ptr JSON-WRITE:writer )
   {: code:n msg:ptr mu:n :}
   ID-TEXT$ {: a:ptr u:n :}
   HEAD a u ID-FIELD
   COMMA  s" error" KEY
   OBJECT-START
   s" code" code FIELD-INT
   COMMA  s" message" msg mu FIELD-S
   OBJECT-END
   OBJECT-END ;

\ `{"jsonrpc":"2.0","method":M,"params":`; the caller writes params, then END.
: NOTIFY ( ptr JSON-WRITE:writer ptr u8 n -- ptr JSON-WRITE:writer )
   {: m:ptr mu:n :}
   HEAD
   COMMA  s" method" m mu FIELD-S
   COMMA  s" params" KEY ;

: END ( ptr JSON-WRITE:writer -- ptr JSON-WRITE:writer )
   OBJECT-END ;

;using
;using
;package
