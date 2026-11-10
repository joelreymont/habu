\ json-rpc.f - the JSON-RPC 2.0 envelope: >MESSAGE sorts one body into a message
\ and the writers build replies on a caller's JSON writer. Every part of a
\ message is the body's own JSON text, borrowed: an id, a method or a string
\ result keeps its quotes and escapes. STORAGE CLASS. CALLER-OWNED. >MESSAGE and
\ METHOD? parse in the caller's JSON:cursor record and the message borrows the
\ caller's body; the module keeps no state.

\ Framing is src/lib/content-length.f's.

require src/lib/error.f
require src/lib/string.f
require src/lib/json.f

package JSON-RPC
using JSON
public

-32700 constant PARSE-ERROR
-32600 constant INVALID-REQUEST
-32601 constant METHOD-NOT-FOUND
-32602 constant INVALID-PARAMS
-32603 constant INTERNAL-ERROR

\ An id as the JSON text it came in: a number's digits, a string with its
\ quotes, or `null`.
STRUCTURE id
   FIELD json ptr u8
   FIELD len n
;STRUCTURE

ENUM message
   VARIANT request FIELD id id FIELD method ptr u8 FIELD method-len n FIELD params ptr u8 FIELD params-len n ;VARIANT
   VARIANT notification FIELD method ptr u8 FIELD method-len n FIELD params ptr u8 FIELD params-len n ;VARIANT
   VARIANT success FIELD id id FIELD value ptr u8 FIELD value-len n ;VARIANT
   VARIANT failure FIELD id id FIELD value ptr u8 FIELD value-len n ;VARIANT
   VARIANT invalid FIELD code n FIELD why ptr u8 FIELD why-len n FIELD id id ;VARIANT
;ENUM

: ID$ ( id -- ptr u8 n )  JSON--RPC-ID:UNMAKE ;
: NULL-ID ( -- id )  s" null" JSON--RPC-ID:MAKE ;

\ Whether a method's JSON text, as >MESSAGE gives it, names this method once
\ decoded: `"initialize"` names initialize.
: METHOD? ( ptr JSON:cursor ptr u8 n ptr u8 n -- bool )
   {: st:ptr m:ptr mu:n name:ptr nu:n :}
   st m mu READ-INIT
   READ-NEXT drop
   name nu READ-STR-EQ? {: same:bool :}
   READ-CLOSE
   same ;

private

-1 constant ABSENT              \ the kind of a member the envelope lacks

\ One envelope member: its token kind, ABSENT when the root has none, and its
\ JSON text.
STRUCTURE member
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
STRUCTURE envelope
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

\ The JSON reader's codes for text that is not JSON, E-JSON-READ-MALFORMED through E-JSON-READ-COMMA, are
\ PARSE-ERROR; any other code, such as JSON:E-READ-SOURCE or a failed growth of the
\ reader's nesting stack, is misuse and is thrown.
: SYNTAX? ( n -- bool )
   {: code:n :}
   code E-JSON-READ-COMMA >= code E-JSON-READ-MALFORMED <= and ;

: ID-KIND? ( n -- bool )
   {: k:n :}
   k T-INT = k T-FLOAT = or k T-STR = or k T-NULL = or ;

: CONTAINER? ( n -- bool )
   {: k:n :}
   k T-OBJ = k T-ARR = or ;

\ The current value as a member, its text from READ-ANY-SPAN$; the kind is read
\ first, as that word leaves a container's reader on its close.
: >MEMBER ( JSON:reader -- JSON:reader member )
   READ-TOKEN {: kind:n :}
   READ-ANY-SPAN$ {: a:ptr u:n :}
   kind a u MEMBER-MAKE ;

: V2? ( JSON:reader -- JSON:reader bool )
   READ-TOKEN T-STR = if s" 2.0" READ-STR-EQ? exit then
   READ-SKIP-VALUE false ;

\ The next member of an error object, given whether its code is an integer and
\ its message a string so far; false after the last.
: ERROR-MEMBER ( JSON:reader bool bool -- JSON:reader bool bool bool )
   {: code:bool text:bool :}
   READ-NEXT T-KEY <> if code text false exit then
   s" code" READ-STR-EQ? {: at-code:bool :}
   s" message" READ-STR-EQ? {: at-text:bool :}
   READ-NEXT {: kind:n :}
   READ-SKIP-VALUE
   at-code if kind T-INT = else code then
   at-text if kind T-STR = else text then
   true ;

\ The current value as the error member, and whether it is an error object.
: ERROR-VALUE ( JSON:reader -- JSON:reader member bool )
   READ-TOKEN T-OBJ <> if >MEMBER false exit then
   READ-SPAN$ drop {: open:ptr :}
   false false begin ERROR-MEMBER 0= until and {: ok:bool :}
   READ-SPAN$ drop {: close:ptr :}
   T-OBJ open close 1+ open - MEMBER-MAKE ok ;

\ The member at the current key: recorded when the envelope names it, over any
\ earlier one of that name, else passed over.
: RECORD ( JSON:reader envelope -- JSON:reader envelope )
   {: e :}
   e ENVELOPE-UNMAKE {: v:bool i m p r x ok:bool :}
   s" jsonrpc" READ-STR-EQ? if READ-NEXT drop V2? i m p r x ok ENVELOPE-MAKE exit then
   s" id" READ-STR-EQ? if READ-NEXT drop >MEMBER {: n :} v n m p r x ok ENVELOPE-MAKE exit then
   s" method" READ-STR-EQ? if READ-NEXT drop >MEMBER {: n :} v i n p r x ok ENVELOPE-MAKE exit then
   s" params" READ-STR-EQ? if READ-NEXT drop >MEMBER {: n :} v i m n r x ok ENVELOPE-MAKE exit then
   s" result" READ-STR-EQ? if READ-NEXT drop >MEMBER {: n :} v i m p n x ok ENVELOPE-MAKE exit then
   s" error" READ-STR-EQ? if
      READ-NEXT drop ERROR-VALUE {: n good:bool :} v i m p r n good ENVELOPE-MAKE exit
   then
   READ-NEXT drop READ-SKIP-VALUE e ;

: ROOT-MEMBER ( JSON:reader envelope -- JSON:reader envelope bool )
   {: e :}
   READ-NEXT T-KEY <> if e false exit then
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

: SORT-ROOT ( n envelope -- message )
   {: root:n e :}
   root T-ARR = if
      INVALID-REQUEST s" an array, a batch this reader does not take" NULL-ID INVALID exit
   then
   root T-OBJ <> if
      INVALID-REQUEST s" a body that is not an object" NULL-ID INVALID exit
   then
   e SORT ;

\ Reads the body once; READ-NEXT answers its end or refuses it with E-JSON-READ-TRAILING.
\ Text that is not JSON throws the JSON reader's code first.
: READ-BODY ( message ptr JSON:cursor ptr u8 n -- message ptr JSON:cursor ptr u8 n )
   {: prior st:ptr body:ptr len:n :}
   st body len READ-INIT
   READ-NEXT {: root:n :}
   root T-OBJ = if
      NO-ENVELOPE begin ROOT-MEMBER 0= until
   else
      READ-SKIP-VALUE NO-ENVELOPE
   then {: e :}
   READ-NEXT drop
   READ-CLOSE
   root e SORT-ROOT  st body len ;

public

\ Params absent or null are empty. A body that is not JSON (RFC 8259, as
\ src/lib/json/read.f reads it) is PARSE-ERROR. INVALID-REQUEST is an array (a
\ batch, not taken), a root that is not an object, `jsonrpc` other than "2.0", an
\ id not a number, string or null, a method not a string, params not an object,
\ array or null, and a message with no method unless it has an id and exactly one
\ of result and error, an error being an object with an integer code and a string
\ message. Member names compare decoded, the last repeat counts, other members are
\ passed over. An invalid message carries its own id when that id is valid and it
\ has a method, else the null id, so a reply never answers a response.
: >MESSAGE ( ptr JSON:cursor ptr u8 n -- message )
   {: st:ptr body:ptr len:n :}
   NOT-JSON st body len [: READ-BODY ;] catch {: code:n :}
   2drop drop {: m :}
   code 0= if m exit then
   code SYNTAX? 0= if code throw then
   NOT-JSON ;

private

: ID-TEXT$ ( id -- ptr u8 n )
   ID$ {: a:ptr u:n :}
   u 0 <= if E-JSON-RPC-ID throw then
   a u ;

: HEAD ( ptr JSON:writer -- ptr JSON:writer )
   WRITE-OBJECT-START  s" jsonrpc" s" 2.0" WRITE-FIELD-S ;

: ID-FIELD ( ptr JSON:writer ptr u8 n -- ptr JSON:writer )
   {: a:ptr u:n :}
   WRITE-COMMA  s" id" a u WRITE-FIELD-RAW ;

public

\ A reply needs an id: an empty one is E-JSON-RPC-ID before any byte is written.
\ `{"jsonrpc":"2.0","id":ID,"result":`; the caller writes the value, then END.
: RESULT ( ptr JSON:writer id -- ptr JSON:writer )
   ID-TEXT$ {: a:ptr u:n :}
   HEAD a u ID-FIELD
   WRITE-COMMA  s" result" WRITE-KEY ;

\ A whole error reply: `{"jsonrpc":"2.0","id":ID,"error":{"code":N,"message":M}}`.
: ERROR ( ptr JSON:writer id n ptr u8 n -- ptr JSON:writer )
   {: code:n msg:ptr mu:n :}
   ID-TEXT$ {: a:ptr u:n :}
   HEAD a u ID-FIELD
   WRITE-COMMA  s" error" WRITE-KEY
   WRITE-OBJECT-START
   s" code" code WRITE-FIELD-INT
   WRITE-COMMA  s" message" msg mu WRITE-FIELD-S
   WRITE-OBJECT-END
   WRITE-OBJECT-END ;

\ `{"jsonrpc":"2.0","method":M,"params":`; the caller writes params, then END.
: NOTIFY ( ptr JSON:writer ptr u8 n -- ptr JSON:writer )
   {: m:ptr mu:n :}
   HEAD
   WRITE-COMMA  s" method" m mu WRITE-FIELD-S
   WRITE-COMMA  s" params" WRITE-KEY ;

: END ( ptr JSON:writer -- ptr JSON:writer )
   WRITE-OBJECT-END ;

;using
;package
