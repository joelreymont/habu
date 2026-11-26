\ value.f - JSON:value, one JSON value inside a text: PARSE makes one, and
\ the other words look values up, walk them and read them. A file of package
\ JSON (src/lib/json.f).
\
\ A value is its document's address, its offset there and its length, so a
\ cursor opened on it counts places from the document's start.
\ PARSE reads the text once and keeps the decoded keys of the objects open at
\ the cursor in key storage that grows with them. At an object's `{` it pushes
\ k, the keys held, as the object's first index lo; at its `}` it pops lo, finds
\ the first of keys lo..k-1 that repeats one before it, and drops those keys.
\ The lo stack lies in the order's low cells: every enclosing object holds a key
\ below an inner object's lo, so the stack stays below the range a check groups.
\ The repeat earliest in the text is rejected once the whole text has read well
\ formed.
\ Every value a word answers lies in text PARSE checked, so GET, FIND, EACH,
\ EACH-MEMBER and the -WHILE forms step over a value by its brackets and
\ strings alone, where a backslash escapes the byte after it and a quote
\ closes the string; they open a cursor only to reject, or to decode a name
\ with an escape. WALK runs its quotation while its cursor is open. A value
\ a walk gives borrows the text, so it stays valid after the walk. WALK keeps
\ each open container's start on a stack that grows with its depth, so a
\ walk of any depth takes constant machine stack.
\ STORAGE CLASS. CALLER-OWNED: the caller owns the text a value borrows and
\ the vector APPEND-STRING grows. The cursor record lives in a MEM scope,
\ the cursor's depth mapping in a mapping of its own, and the key storage,
\ WALK's stack of starts, WITH-STRING's decoded text and AT's decoded token on
\ the heap, all released on return or throw. No module state.

require src/lib/memory.f
require src/lib/vector.f
require src/lib/heap.f
require src/lib/adt/option.f
require src/lib/string-option.f
require src/lib/reason.f
require src/lib/json/read.f
require src/lib/json/equal.f
require src/lib/json/pointer.f
require src/lib/utf8-scalar.f

package JSON

public

STRUCTURE value OPAQUE
   FIELD doc ^u8
   FIELD offset n
   FIELD len n
;STRUCTURE

private

\ The record reads to the value's end, so after the value it answers T-END.
: OPEN-AT ( ^cursor value -- ) {: state:^ v :}
   v VALUE-UNMAKE {: doc:^ at:n len:n :}
   state doc at len + >str INIT-STATE
   at state CURSOR-POS ! ;

: RUN-BODY ( R ^cursor [ R ^cursor -- S ] -- S )
   execute ;

\ WITH-RECORD zero-fills the record, so it owns no mapping until it grows one.
: CURSOR-SCOPE ( R ^cursor value [ R ^cursor -- S ] -- S )
   {: state:^ v body :}
   state v OPEN-AT
   state body
   [: RUN-BODY ;] [: state MORE-RELEASE ;] finally ;

\ Runs body on a cursor at the value's first byte, released on return or
\ throw.
: WITH-CURSOR ( R value [ R ^cursor -- S ] -- S )
   {: v body :}
   [type] cursor [: v body CURSOR-SCOPE ;] MEM:WITH-RECORD ;

\ Pushes k as the lo of an object opened inside m others.
: OPEN-OBJECT ( ^keys n -- n ) {: keys:^ m:n :}
   keys KEYS-INDICES @ m 1+ VEC:ENSURE
   keys TABLE @  keys ORDER m cells + !
   m 1+ ;

\ The earlier of first and the place of the key found, if any.
: EARLIER ( option<n> ^keys n -- n ) {: found keys:^ first:n :}
   found MATCH option
     none OF first ENDOF
     some OF cells keys PLACES + @ first min ENDOF
   ;MATCH ;

\ Pops the innermost object's lo, checks its keys and drops them.
: CLOSE-OBJECT ( ^keys n n -- n n ) {: keys:^ m:n first:n :}
   keys ORDER m 1- cells + @ {: lo:n :}
   keys lo FIRST-REPEAT keys first EARLIER
   lo keys TABLE !
   m 1- swap ;

\ Takes one token, given m open objects and the earliest repeat's place,
\ the text's length for none; true at the text's end.
: WALK-STEP ( n n ^cursor ^keys -- n n bool )
   {: m:n first:n state:^ keys:^ :}
   state NEXT-INNER {: tok:n :}
   tok T-OBJ = if keys m OPEN-OBJECT first false exit then
   tok T-KEY = if state keys RECORD-KEY m first false exit then
   tok T-OBJ-END = if keys m first CLOSE-OBJECT false exit then
   m first tok T-END = ;

: CHECK-KEYS ( ^cursor ^keys -- ) {: state:^ keys:^ :}
   state CURSOR-SRC-LEN @ {: none:n :}
   0 none begin state keys WALK-STEP until
   nip {: first:n :}
   first none = if exit then
   state first s" a member name not used before in this object"
   s" remove the repeated member" E-MEMBER REJECT-AT ;

\ The value of a text read as one: only white space is around it.
: TRIMMED ( str -- value ) {: text:str :}
   0 begin text STR:DATA over + c@ WS? while 1+ repeat {: at:n :}
   text STR:LEN begin text STR:DATA over + 1- c@ WS? while 1- repeat {: end:n :}
   text STR:DATA at end at - VALUE-MAKE ;

public

\ A text that is not one JSON value throws the reader's code; a repeated
\ member name, decoded, throws E-MEMBER at the repeat earliest in the text.
: PARSE ( str -- value ) {: text:str :}
   text REQUIRE-SOURCE
   text STR:DATA 0 text STR:LEN VALUE-MAKE
   [: [: CHECK-KEYS ;] WITH-KEYS ;] WITH-CURSOR
   text TRIMMED ;

: TEXT$ ( value -- str )
   VALUE-UNMAKE {: doc:^ at:n len:n :}
   doc at + len >str ;

private

: FIRST-BYTE ( value -- n )
   TEXT$ STR:DATA c@ ;

public

: STRING? ( value -- bool )  FIRST-BYTE DQ = ;
: OBJECT? ( value -- bool )  FIRST-BYTE LBRACE = ;
: ARRAY? ( value -- bool )  FIRST-BYTE LBRACK = ;
: NULL? ( value -- bool )  FIRST-BYTE CH-N = ;

: BOOL? ( value -- bool )
   FIRST-BYTE dup CH-T = swap CH-F = or ;

: NUMBER? ( value -- bool )
   FIRST-BYTE dup MINUS = swap STR:DIGIT? or ;

private

\ The negated value of count significant digits from offset f, stepping over
\ the point.
: NEGATED-DIGITS ( ^u8 n n -- n )
   {: p:^ f:n count:n :}
   0 f  count 0 ?do
      p over + c@ ZERO -  rot swap DIGIT-ONTO swap
      i count 1- < if p swap NEXT-DIGIT then
   loop drop ;

\ The negated magnitude of 0.d1..dn * 10^x for x >= n. Each zero appended
\ grows it tenfold, so a large x ends in E-JSON-READ-NUMBER.
: NEGATED-WHOLE ( ^u8 n n n -- n )
   {: p:^ x:n f:n count:n :}
   p f count NEGATED-DIGITS
   x count - 0 ?do 0 DIGIT-ONTO loop ;

\ The lexeme's value from its negated magnitude m.
: SIGNED ( ^u8 n -- n )
   {: p:^ m:n :}
   p NEGATIVE? if m exit then
   m LEAST-N = if E-JSON-READ-NUMBER throw then
   m negate ;

\ Whether a number lexeme has no significant digit: its value is zero.
: ZERO-TEXT? ( str -- bool )
   MANTISSA drop drop drop 0< ;

\ Whether count significant digits with point exponent x leave no fraction.
: WHOLE-DIGITS? ( n n -- bool )
   {: x:n count:n :}
   count x <= ;

\ A number lexeme's value, E-JSON-READ-NUMBER when no signed cell holds
\ it: a fraction, or past a cell.
: WHOLE-OF ( str -- n )
   {: s:str :}
   s ZERO-TEXT? if 0 exit then
   s STR:DATA s STR:LEN DIGITS {: x:n f:n count:n :}
   x count WHOLE-DIGITS? 0= if E-JSON-READ-NUMBER throw then
   s STR:DATA  s STR:DATA x f count NEGATED-WHOLE  SIGNED ;

\ WHOLE-OF's answer replaces the slot under the lexeme, so the quotation
\ under catch leaves the types it was given.
: WHOLE-SLOT ( n str -- n str )
   {: s:str :}
   drop  s WHOLE-OF  s ;

\ Some for a number whose value is whole and fits a signed cell, whatever
\ its spelling (1.0, 1e2, -0).
: WHOLE-VALUE ( value -- option<n> )
   {: v :}
   v NUMBER? 0= if OPTION:NONE exit then
   0 v TEXT$ [: WHOLE-SLOT ;] catch {: code:n :} drop {: n :}
   code 0= if n OPTION:SOME exit then
   code E-JSON-READ-NUMBER = if OPTION:NONE exit then
   code throw ;

\ DIGITS's three results replace the slots under the lexeme, so the
\ quotation under catch leaves the types it was given.
: TEXT-DIGITS ( n n n str -- n n n str )
   {: s:str :}
   drop drop drop  s STR:DATA s STR:LEN DIGITS  s ;

\ Whether the exponent a lexeme states after its mantissa's end m is
\ negative; the lexeme has one.
: NEGATIVE-EXP? ( str n -- bool )
   {: s:str m:n :}
   s STR:DATA m + 1+ c@ MINUS = ;

\ Whether a number lexeme's value has no fractional part. A point exponent no
\ cell holds outweighs every digit, so the stated exponent's sign alone decides.
: WHOLE-TEXT? ( str -- bool )
   {: s:str :}
   s ZERO-TEXT? if true exit then
   0 0 0 s [: TEXT-DIGITS ;] catch {: code:n :}
   drop {: x f count :}
   code 0= if x count WHOLE-DIGITS? exit then
   code E-JSON-READ-NUMBER <> if code throw then
   s  s MANTISSA nip nip nip  NEGATIVE-EXP? 0= ;

public

\ A whole number that also fits a signed cell.
: INTEGER? ( value -- bool )
   WHOLE-VALUE MATCH option
     none OF false ENDOF
     some OF drop true ENDOF
   ;MATCH ;

\ A number whose value has no fractional part, whatever its size.
: INTEGRAL? ( value -- bool )
   {: v :}
   v NUMBER? 0= if false exit then
   v TEXT$ WHOLE-TEXT? ;

private

\ Throws E-MEMBER at the current token, the found part its kind.
: WRONG-KIND ( ^cursor str str -- )
   {: state:^ es:str fs:str :}
   state state TOKEN-START REJECT-HEAD
   es REASON:APPEND
   REASON:FOUND state CURSOR-KIND @ KIND$ REASON:APPEND
   REASON:FIX fs REASON:APPEND
   E-MEMBER REASON:STOP ;

\ The value at the current token; the cursor moves past it.
: TOKEN-VALUE ( ^cursor -- value ) {: state:^ :}
   state TOKEN-START {: at:n :}
   state ANY-SPAN-INNER STR:LEN {: u:n :}
   state CURSOR-SRC @ at u VALUE-MAKE ;

\ The current key as a string value, its quotes included.
: NAME-VALUE ( ^cursor -- value ) {: state:^ :}
   state CURSOR-SRC @ state TOKEN-START
   state CURSOR-TOK-LEN @ 2 + VALUE-MAKE ;

\ Rejects the value, read as a token, for its kind: es is the kind expected.
: NOT-KIND ( value str str -- ) {: v es:str fs:str :}
   v [: dup NEXT-INNER drop es fs WRONG-KIND ;] WITH-CURSOR ;

\ NEST-END through MEMBER-ENDS scan a container's text from an index with
\ no bound: the container's close follows every value inside it.

\ The index after the string or container at i. Or-ing $20 reads `[` as `{`,
\ `]` as `}`, and no byte checked text holds outside a string as `"`.
: NEST-END ( str n -- n ) {: s:str i:n :}
   s STR:DATA {: p:^ :}
   0 i begin
      p over + c@ $20 or
      dup DQ = if drop 1+ begin p over + c@ dup DQ <> while
            BACKSLASH = if 2 + else 1+ then repeat drop 1+ else
         dup LBRACE = if drop swap 1+ swap else
         RBRACE = if swap 1- swap then then 1+
      then
      over 0=
   until nip ;

\ The index after the number or literal at i.
: SCALAR-END ( str n -- n ) {: s:str i:n :}
   s STR:DATA {: p:^ :}
   p i + c@ {: b:n :}
   b CH-T = b CH-N = or if i 4 + exit then
   b CH-F = if i 5 + exit then
   i begin p over + c@ NUM-CHAR? while 1+ repeat ;

: VALUE-END ( str n -- n ) {: s:str i:n :}
   s STR:DATA i + c@ {: b:n :}
   b DQ = b LBRACE = or b LBRACK = or if s i NEST-END exit then
   s i SCALAR-END ;

: WS-END ( str n -- n ) {: s:str i:n :}
   s STR:DATA {: p:^ :}
   i begin p over + c@ WS? while 1+ repeat ;

\ The index of the item after a value that ends at e, or of the close.
: NEXT-ITEM ( str n -- n ) {: s:str e:n :}
   s e WS-END {: j:n :}
   s STR:DATA j + c@ COMMA = if s j 1+ WS-END exit then
   j ;

\ From a member's name at k: the name's end, and its value's start and end.
: MEMBER-ENDS ( str n -- n n n ) {: s:str k:n :}
   s k NEST-END {: ke:n :}
   s s ke WS-END 1+ WS-END {: i:n :}
   ke i s i VALUE-END ;

\ The value of the bytes from i to e of the value's text.
: PART ( value n n -- value ) {: v i:n e:n :}
   v VALUE-UNMAKE {: doc:^ at:n len:n :}
   doc at i + e i - VALUE-MAKE ;

\ The str is the fix a wrong kind's rejection gives; it names the caller's
\ walk.
: ELEMENTS ( R value str [ R value -- R bool ] -- R )
   {: v fs:str body :}
   v ARRAY? 0= if v s" an array" fs NOT-KIND then
   v TEXT$ {: s:str :}
   s 1 WS-END begin s STR:DATA over + c@ RBRACK <> while
      {: i:n :} s i VALUE-END {: e:n :}
      v i e PART body execute 0= if exit then
      s e NEXT-ITEM
   repeat drop ;

\ The str is the fix, as in ELEMENTS.
: MEMBERS ( R value str [ R value value -- R bool ] -- R )
   {: v fs:str body :}
   v OBJECT? 0= if v s" an object" fs NOT-KIND then
   v TEXT$ {: s:str :}
   s 1 WS-END begin s STR:DATA over + c@ RBRACE <> while
      {: k:n :} s k MEMBER-ENDS {: ke:n i:n e:n :}
      v k ke PART v i e PART body execute 0= if exit then
      s e NEXT-ITEM
   repeat drop ;

public

: EACH ( R value [ R value -- R ] -- R ) {: v body :}
   v s" walk only an array with JSON:EACH"
   [: body execute true ;] ELEMENTS ;

\ The name is the key as written, a string value.
: EACH-MEMBER ( R value [ R value value -- R ] -- R )
   {: v body :}
   v s" walk only an object with JSON:EACH-MEMBER"
   [: body execute true ;] MEMBERS ;

: EACH-WHILE ( R value [ R value -- R bool ] -- R )
   {: v body :}
   v s" walk only an array with JSON:EACH-WHILE" body ELEMENTS ;

: EACH-MEMBER-WHILE ( R value [ R value value -- R bool ] -- R )
   {: v body :}
   v s" walk only an object with JSON:EACH-MEMBER-WHILE" body MEMBERS ;

\ An event WALK gives. A name is the member's name as written, a string value;
\ a close carries the whole container that just ended.
ENUM event
   VARIANT open-object ;VARIANT
   VARIANT open-array ;VARIANT
   VARIANT name FIELD value value ;VARIANT
   VARIANT scalar FIELD value value ;VARIANT
   VARIANT close FIELD value value ;VARIANT
;ENUM

private

: PUSH-START ( ^cursor ^VEC:vector<n> -- ) {: state:^ starts:^ :}
   state TOKEN-START starts VEC:PUSH ! ;

\ The container the cursor just closed; its start is popped.
: CLOSED ( ^cursor ^VEC:vector<n> -- value ) {: state:^ starts:^ :}
   starts VEC:COUNT 1- {: top:n :}
   starts top VEC:AT @ {: at:n :}
   starts top VEC:RESIZE
   state CURSOR-SRC @ at  state CURSOR-TOK-AT @ 1+ at -  VALUE-MAKE ;

: NEXT-EVENT ( ^cursor ^VEC:vector<n> -- event ) {: state:^ starts:^ :}
   state NEXT-INNER {: tok:n :}
   tok OPENER? if state starts PUSH-START then
   tok T-OBJ = if JSON-EVENT:OPEN-OBJECT exit then
   tok T-ARR = if JSON-EVENT:OPEN-ARRAY exit then
   tok CLOSER? if state starts CLOSED JSON-EVENT:CLOSE exit then
   tok T-KEY = if state NAME-VALUE JSON-EVENT:NAME exit then
   state TOKEN-VALUE JSON-EVENT:SCALAR ;

\ The walk ends after the root's close, or a scalar root's one event, when no
\ container is open.
: EVENTS ( R ^cursor ^VEC:vector<n> [ R event -- R bool ] -- R )
   {: state:^ starts:^ body :}
   begin
      state starts NEXT-EVENT body execute 0= if exit then
      starts VEC:COUNT 0<>
   while repeat ;

public

\ Reads the value once and runs the body on each event in text order: an open
\ before its container's names and values, a name before its member's value, a
\ close after them. It stops after the event the body answers false.
: WALK ( R value [ R event -- R bool ] -- R ) {: v body :}
   HEAP:ALLOCATOR 0 [type] n VEC:NEW {: starts:^ :}
   [: v [: starts body EVENTS ;] WITH-CURSOR ;]
   [: starts VEC:DELETE ;] finally ;

private

\ Opens the value the cursor reads next and answers its `{`; any other
\ value is rejected with its kind.
: ENTER-OBJECT ( ^cursor -- n ) {: state:^ :}
   state NEXT-INNER T-OBJ = if state TOKEN-START exit then
   state s" an object" s" write an object here" WRONG-KIND ;

\ Quotes the span, cut at REASON:QUOTE-MOST on a character's first byte.
: QUOTE-CUT ( str -- ) {: s:str :}
   s REASON:QUOTE-MOST UTF8:FIT {: k:n :}
   s STR:DATA k >str  k s STR:LEN <  REASON:QUOTE ;

: MISSING ( ^cursor n str -- ) {: state:^ at:n name:str :}
   state at REJECT-HEAD
   s" a member " REASON:APPEND  name QUOTE-CUT
   REASON:FOUND s" none" REASON:APPEND
   REASON:FIX s" add the member to the object" REASON:APPEND
   E-MEMBER REASON:STOP ;

: OFFSET-OF ( value -- n )
   VALUE-UNMAKE {: doc:^ at:n len:n :} at ;

\ Throws E-MEMBER at the object's `{`: it has no member of that name.
: ABSENT ( value str -- value ) {: v name:str :}
   v [: v OFFSET-OF name MISSING ;] WITH-CURSOR ;

\ Rejects, as a cursor reading members does, a value that is not an object,
\ then a name no text can be.
: LOOKUP-REJECT ( value str -- ) {: v name:str :}
   v [: dup ENTER-OBJECT drop name REQUIRE-KEY-SPAN ;] WITH-CURSOR ;

: MATCH-AT ( ^cursor str -- bool ) {: state:^ want:str :}
   state NEXT-INNER drop
   state want MATCH-INNER ;

\ A string's text inside its quotes.
: INSIDE$ ( value -- str )
   VALUE-UNMAKE {: doc:^ at:n len:n :}
   doc at + 1+ len 2 - >str ;

: ESCAPED? ( str -- bool )
   BACKSLASH STR:INDEX-OF MATCH option
     none OF false ENDOF
     some OF drop true ENDOF
   ;MATCH ;

\ Whether a string value decodes to want. Decoding shortens a string with an
\ escape and keeps any other; it streams, so no buffer bounds it.
: QUOTED= ( value str -- bool ) {: v want:str :}
   v INSIDE$ {: raw:str :}
   raw STR:LEN want STR:LEN - {: more:n :}
   more 0< if false exit then
   more 0= if
      raw want STR:= 0= if false exit then  raw ESCAPED? 0= exit
   then
   raw ESCAPED? 0= if false exit then
   v [: want MATCH-AT ;] WITH-CURSOR ;

: POINTED ( ^cursor str ^VEC:vector<u8> -- option<value> )
   {: state:^ s:str out:^ :}
   state s out SEEK {: at:n len:n found:bool :}
   found 0= if OPTION:NONE exit then
   state CURSOR-SRC @ at len VALUE-MAKE OPTION:SOME ;

public

\ None for an object without the member; a value that is not an object
\ throws E-MEMBER.
: FIND ( value str -- option<value> ) {: v name:str :}
   v OBJECT? 0= name BAD-KEY? or if v name LOOKUP-REJECT then
   v TEXT$ {: s:str :}
   s 1 WS-END begin s STR:DATA over + c@ RBRACE <> while
      {: k:n :} s k MEMBER-ENDS {: ke:n i:n e:n :}
      v k ke PART name QUOTED= if v i e PART OPTION:SOME exit then
      s e NEXT-ITEM
   repeat drop OPTION:NONE ;

\ The member of that decoded name. A value that is not an object, or an
\ object without the member, throws E-MEMBER.
: GET ( value str -- value ) {: v name:str :}
   v name FIND MATCH option
     none OF v name ABSENT ENDOF
     some OF ENDOF
   ;MATCH ;

\ The value the RFC 6901 pointer names from this one, none for no such place.
\ A pointer POINTER? rejects throws E-JSON-POINTER before anything is read.
: AT ( value str -- option<value> ) {: v s:str :}
   s POINTER? 0= if E-JSON-POINTER throw then
   HEAP:ALLOCATOR s STR:LEN [type] u8 VEC:NEW {: out:^ :}
   [: v [: s out POINTED ;] WITH-CURSOR ;]
   [: out VEC:DELETE ;] finally ;

private

: NEXT-STRING ( ^cursor -- ) {: state:^ :}
   state NEXT-INNER T-STR <> if
      state s" a string" s" write a string" WRONG-KIND
   then ;

\ Lends body the next string, decoded into the vector.
: DECODE-RUN ( R ^cursor ^VEC:vector<u8> [ R str -- S ] -- S )
   {: state:^ text:^ body :}
   state NEXT-STRING
   state text DECODED$ body execute ;

\ Throws E-MEMBER at the current token, the found part the token.
: WHOLE-REJECT ( ^cursor -- )
   s" a whole number from -9223372036854775808 to 9223372036854775807"
   s" write a whole number in that range" E-MEMBER REJECT-TOKEN ;

\ Reads the value's token to reject it at its place.
: NOT-WHOLE ( ^cursor -- )
   {: state:^ :}
   state NEXT-INNER drop  state WHOLE-REJECT ;

: BOOL-AT ( ^cursor -- bool ) {: state:^ :}
   state NEXT-INNER {: tok:n :}
   tok T-TRUE = if true exit then
   tok T-FALSE <> if
      state s" true or false" s" write true or false" E-MEMBER
      REJECT-TOKEN
   then
   false ;

public

: STRING= ( value str -- bool ) {: v want:str :}
   v STRING? 0= if false exit then
   v want QUOTED= ;

\ The text is lent for the call only: the bytes inside the quotes when the
\ string holds no escape, else its decoding in a vector of its own.
: WITH-STRING ( R value [ R str -- S ] -- S ) {: v body :}
   v STRING? if
      v INSIDE$ dup ESCAPED? 0= if body execute exit then
      drop
   then
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW {: text:^ :}
   [: v [: text body DECODE-RUN ;] WITH-CURSOR ;]
   [: text VEC:DELETE ;] finally ;

\ Appends the string's decoded bytes after the vector's own.
: APPEND-STRING ( value ^VEC:vector<u8> -- ) {: v out:^ :}
   v STRING? if
      v INSIDE$ dup ESCAPED? 0= if out VEC:APPEND exit then
      drop
   then
   v [: dup NEXT-STRING out DECODE-APPEND ;] WITH-CURSOR ;

\ A number whose value is whole and fits a signed cell.
: >N ( value -- n )
   {: v :}
   v WHOLE-VALUE MATCH option
     none OF v [: NOT-WHOLE ;] WITH-CURSOR ENDOF
     some OF ENDOF
   ;MATCH ;

\ A value is parsed text, so `t` begins true and `f` false; the cursor
\ rejects any other value at its place.
: >BOOL ( value -- bool ) {: v :}
   v FIRST-BYTE {: b:n :}
   b CH-T = if true exit then
   b CH-F = if false exit then
   v [: BOOL-AT ;] WITH-CURSOR ;

private

: VALUE-REJECT ( ^cursor str str -- ) {: state:^ es:str fs:str :}
   state state CURSOR-POS @ es fs E-MEMBER REJECT-AT ;

public

\ Throws E-MEMBER at the value's first byte with this expected text and fix.
: REJECT ( value str str -- ) {: v es:str fs:str :}
   v [: es fs VALUE-REJECT ;] WITH-CURSOR ;

;package
