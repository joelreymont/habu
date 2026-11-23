split-str
\ json-rpc.f - the JSON-RPC 2.0 envelope: >MESSAGE sorts one body into a message
\ and the writers build replies on a caller's JSON writer. Every part of a
\ message is the body's own JSON text, borrowed: an id, a method or a string
\ result keeps its quotes and escapes. An invalid message's why is program text,
\ or for PARSE-ERROR the reader's REASON text, valid until the task's next
\ REASON:THROW. INVALID-REQUEST is an array (a batch, not taken), a root that is
\ not an object, a member the envelope reads named twice: jsonrpc, id, method,
\ params, result or error, or an error's code or message; `jsonrpc` other than
\ "2.0", an id not a number, string or null, a method not a string, params not
\ an object, array or null, and a message with no method unless it has an id
\ and exactly one of result and error, an error being an object with an
\ integer code and a string message. Member names compare decoded, so no
\ reader here picks a copy; other members, and the text of params, result and
\ an error, are passed over for their reader. An
\ invalid message carries its own id when that id is valid, named once, and it
\ has a method, else the null id, so a reply never answers a response. STORAGE CLASS. CALLER-OWNED. >MESSAGE and
\ METHOD? each parse in a JSON:cursor record of their own, released on return or
\ throw; the message borrows the caller's body; the module keeps no state.

\ Framing is src/lib/content-length.f's.

require src/lib/error.f
require src/lib/string.f
require src/lib/memory.f
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

\ An id as the JSON text it came in: a number's digits, a string with its
\ quotes, or `null`.
STRUCTURE id
   FIELD json ^u8
   FIELD len n
;STRUCTURE

ENUM message
   VARIANT request FIELD id id FIELD method ^u8 FIELD method-len n FIELD params ^u8 FIELD params-len n ;VARIANT
   VARIANT notification FIELD method ^u8 FIELD method-len n FIELD params ^u8 FIELD params-len n ;VARIANT
   VARIANT success FIELD id id FIELD value ^u8 FIELD value-len n ;VARIANT
   VARIANT failure FIELD id id FIELD value ^u8 FIELD value-len n ;VARIANT
   VARIANT invalid FIELD code n FIELD why ^u8 FIELD why-len n FIELD id id ;VARIANT
;ENUM

: ID$ ( id -- str )  JSON--RPC-ID:UNMAKE ;
: NULL-ID ( -- id )  s" null" JSON--RPC-ID:MAKE ;

private

: METHOD-READ ( ^JSON:cursor str str -- bool )
   {: st:^ m:^ mu:n name:^ nu:n :}
   st m mu READ-INIT
   READ-NEXT drop
   name nu READ-STR-EQ? {: same:bool :}
   READ-CLOSE
   same ;

: METHOD-IN ( str str ^JSON:cursor -- bool )
   {: m:^ mu:n name:^ nu:n st:^ :}
   [: st m mu name nu METHOD-READ ;] [: st READ-DISPOSE ;] finally ;

public

\ Whether a method's JSON text, as >MESSAGE gives it, names this method once
\ decoded: `"initialize"` names initialize.
: METHOD? ( str str -- bool )
   [type] JSON:cursor [: METHOD-IN ;] MEM:WITH-RECORD ;

private

-1 constant ABSENT              \ the kind of a member the envelope lacks
-2 constant TWICE               \ the kind of a member named twice

\ One envelope member: its token kind, ABSENT when the root has none, and its
\ JSON text.
STRUCTURE member
   FIELD kind n
   FIELD json ^u8
   FIELD len n
;STRUCTURE

: KIND ( member -- n )  MEMBER-UNMAKE 2drop ;
: TEXT$ ( member -- str )  MEMBER-UNMAKE rot drop ;
: PRESENT? ( member -- bool )  KIND ABSENT <> ;
: >ID ( member -- id )  TEXT$ JSON--RPC-ID:MAKE ;
: NO-MEMBER ( -- member )  ABSENT s" " MEMBER-MAKE ;

\ What the walk over a root object met: whether jsonrpc is the string "2.0";
\ the jsonrpc, id, method, params, result and error; whether that error is an
\ error object.
STRUCTURE envelope
   FIELD v2 bool
   FIELD jsonrpc member
   FIELD id member
   FIELD method member
   FIELD params member
   FIELD result member
   FIELD error member
   FIELD error-ok bool
;STRUCTURE

: NO-ENVELOPE ( -- envelope )
   false NO-MEMBER NO-MEMBER NO-MEMBER NO-MEMBER NO-MEMBER NO-MEMBER false
   ENVELOPE-MAKE ;

: INVALID ( n str id -- message )  JSON--RPC-MESSAGE:INVALID ;

: NOT-JSON ( n -- message )
   {: code:n :}
   PARSE-ERROR code REASON:TEXT$ NULL-ID INVALID ;

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
   READ-ANY-SPAN$ {: a:^ u:n :}
   kind a u MEMBER-MAKE ;

: V2? ( JSON:reader -- JSON:reader bool )
   READ-TOKEN T-STR = if s" 2.0" READ-STR-EQ? exit then
   READ-SKIP-VALUE false ;

\ The jsonrpc value as a member of its kind, and whether it is "2.0".
: >V2 ( JSON:reader -- JSON:reader member bool )
   READ-TOKEN {: kind:n :}
   V2? {: good:bool :}
   kind s" " MEMBER-MAKE good ;

: >PLAIN ( JSON:reader -- JSON:reader member bool )  >MEMBER true ;

\ Whether this key names an error member already met, of kind K.
: AGAIN? ( bool n -- bool )
   {: at:bool k:n :}
   at k ABSENT <> and ;

\ The next member of an error object, given its code's and message's kinds so
\ far, ABSENT before either, and whether one came twice; false after the last.
: ERROR-MEMBER ( JSON:reader n n bool -- JSON:reader n n bool bool )
   {: code:n text:n twice:bool :}
   READ-NEXT T-KEY <> if code text twice false exit then
   s" code" READ-STR-EQ? {: at-code:bool :}
   s" message" READ-STR-EQ? {: at-text:bool :}
   READ-NEXT {: kind:n :}
   READ-SKIP-VALUE
   at-code if kind else code then
   at-text if kind else text then
   at-code code AGAIN? at-text text AGAIN? or twice or
   true ;

\ The current value as the error member, TWICE when it names its code or
\ message twice, and whether it is an error object.
: ERROR-VALUE ( JSON:reader -- JSON:reader member bool )
   READ-TOKEN T-OBJ <> if >MEMBER false exit then
   READ-SPAN$ drop {: open:^ :}
   ABSENT ABSENT false begin ERROR-MEMBER 0= until
   {: code:n text:n twice:bool :}
   READ-SPAN$ drop {: close:^ :}
   twice if TWICE else T-OBJ then open close 1+ open - MEMBER-MAKE
   code T-INT = text T-STR = and ;

\ The member at the current value and Q's reading of it, given the one before
\ it: when the root named it already, TWICE and false, its value skipped.
: >ONCE-MEMBER
   ( JSON:reader member [ JSON:reader -- JSON:reader member bool ] --
     JSON:reader member bool )
   {: prior q :}
   prior PRESENT? if
      READ-SKIP-VALUE TWICE s" " MEMBER-MAKE false exit
   then
   q execute ;

\ The member at the current key: recorded when the envelope names it, else
\ passed over.
: RECORD ( JSON:reader envelope -- JSON:reader envelope )
   {: e :}
   e ENVELOPE-UNMAKE {: v:bool j i m p r x ok:bool :}
   s" jsonrpc" READ-STR-EQ? if
      READ-NEXT drop j [: >V2 ;] >ONCE-MEMBER {: n good:bool :}
      good n i m p r x ok ENVELOPE-MAKE exit
   then
   s" id" READ-STR-EQ? if
      READ-NEXT drop i [: >PLAIN ;] >ONCE-MEMBER drop {: n :}
      v j n m p r x ok ENVELOPE-MAKE exit
   then
   s" method" READ-STR-EQ? if
      READ-NEXT drop m [: >PLAIN ;] >ONCE-MEMBER drop {: n :}
      v j i n p r x ok ENVELOPE-MAKE exit
   then
   s" params" READ-STR-EQ? if
      READ-NEXT drop p [: >PLAIN ;] >ONCE-MEMBER drop {: n :}
      v j i m n r x ok ENVELOPE-MAKE exit
   then
   s" result" READ-STR-EQ? if
      READ-NEXT drop r [: >PLAIN ;] >ONCE-MEMBER drop {: n :}
      v j i m p n x ok ENVELOPE-MAKE exit
   then
   s" error" READ-STR-EQ? if
      READ-NEXT drop x [: ERROR-VALUE ;] >ONCE-MEMBER {: n good:bool :}
      v j i m p r n good ENVELOPE-MAKE exit
   then
   READ-NEXT drop READ-SKIP-VALUE e ;

: ROOT-MEMBER ( JSON:reader envelope -- JSON:reader envelope bool )
   {: e :}
   READ-NEXT T-KEY <> if e false exit then
   e RECORD true ;

: REPLY ( member member -- id )
   {: i m :}
   m PRESENT? i KIND ID-KIND? and if i >ID exit then
   NULL-ID ;

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
   pk CONTAINER? if p TEXT$ else s" " then {: pa:^ pu:n :}
   i PRESENT? if reply m TEXT$ pa pu JSON--RPC-MESSAGE:REQUEST exit then
   m TEXT$ pa pu JSON--RPC-MESSAGE:NOTIFICATION ;

\ A message with no method: a result or an error answering its id.
: ANSWER ( member member member bool -- message )
   {: i r x ok:bool :}
   i PRESENT? 0= if
      INVALID-REQUEST s" a message with neither a method nor an id" NULL-ID INVALID exit
   then
   r PRESENT? x PRESENT? xor 0= if
      INVALID-REQUEST s" a response without exactly one of result and error" NULL-ID INVALID exit
   then
   r PRESENT? if i >ID r TEXT$ JSON--RPC-MESSAGE:SUCCESS exit then
   ok 0= if
      INVALID-REQUEST s" an error that is not an error object" NULL-ID INVALID exit
   then
   i >ID x TEXT$ JSON--RPC-MESSAGE:FAILURE ;

: SORT ( envelope -- message )
   ENVELOPE-UNMAKE {: v:bool j i m p r x ok:bool :}
   i PRESENT? if i KIND ID-KIND? 0= if
      INVALID-REQUEST s" an id that is not a number, string or null" NULL-ID INVALID exit
   then then
   i m REPLY {: reply :}
   v 0= if
      INVALID-REQUEST s\" a jsonrpc member that is not \"2.0\"" reply INVALID exit
   then
   m PRESENT? if i m p reply CALL exit then
   i r x ok ANSWER ;

: TWICE? ( member -- bool )  KIND TWICE = ;

: NAMED-TWICE? ( envelope -- bool )
   ENVELOPE-UNMAKE {: v:bool j i m p r x ok:bool :}
   j TWICE? i TWICE? or m TWICE? or
   p TWICE? or r TWICE? or x TWICE? or ;

: REPEATED ( envelope -- message )
   ENVELOPE-UNMAKE {: v:bool j i m p r x ok:bool :}
   INVALID-REQUEST s" an envelope member named twice"
   i m REPLY INVALID ;

: SORT-ROOT ( n envelope -- message )
   {: root:n e :}
   root T-ARR = if
      INVALID-REQUEST s" an array, a batch this reader does not take" NULL-ID INVALID exit
   then
   root T-OBJ <> if
      INVALID-REQUEST s" a body that is not an object" NULL-ID INVALID exit
   then
   e NAMED-TWICE? if e REPEATED exit then
   e SORT ;

: READ-BODY ( message ^JSON:cursor str -- message ^JSON:cursor str )
   {: prior st:^ body:^ len:n :}
   st body len READ-INIT
   READ-NEXT {: root:n :}
   root T-OBJ = if
      NO-ENVELOPE begin ROOT-MEMBER 0= until
   else
      READ-SKIP-VALUE NO-ENVELOPE
   then {: e :}
   READ-FINISH
   root e SORT-ROOT  st body len ;

: READ-IN ( str ^JSON:cursor -- message )
   {: body:^ len:n st:^ :}
   0 NOT-JSON st body len [: READ-BODY ;] catch {: code:n :}
   2drop drop {: m :}
   st READ-DISPOSE
   code 0= if m exit then
   code SYNTAX? 0= if code throw then
   code NOT-JSON ;

public

\ Params absent or null are empty; a body that is not JSON is PARSE-ERROR.
: >MESSAGE ( str -- message )
   [type] JSON:cursor [: READ-IN ;] MEM:WITH-RECORD ;

private

: ID-TEXT$ ( id -- str )
   ID$ {: a:^ u:n :}
   u 0 <= if E-JSON-RPC-ID throw then
   a u ;

: HEAD ( ^JSON:writer -- ^JSON:writer )
   WRITE-OBJECT-START  s" jsonrpc" s" 2.0" WRITE-FIELD-S ;

: ID-FIELD ( ^JSON:writer str -- ^JSON:writer )
   {: a:^ u:n :}
   WRITE-COMMA  s" id" a u WRITE-FIELD-RAW ;

public

\ A reply needs an id: an empty one is E-JSON-RPC-ID before any byte is written.
\ `{"jsonrpc":"2.0","id":ID,"result":`; the caller writes the value, then END.
: RESULT ( ^JSON:writer id -- ^JSON:writer )
   ID-TEXT$ {: a:^ u:n :}
   HEAD a u ID-FIELD
   WRITE-COMMA  s" result" WRITE-KEY ;

\ A whole error reply: `{"jsonrpc":"2.0","id":ID,"error":{"code":N,"message":M}}`.
: ERROR ( ^JSON:writer id n str -- ^JSON:writer )
   {: code:n msg:^ mu:n :}
   ID-TEXT$ {: a:^ u:n :}
   HEAD a u ID-FIELD
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
