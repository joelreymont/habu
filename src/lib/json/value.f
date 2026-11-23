split-str
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
\ The walks EACH, EACH-MEMBER and their -WHILE forms run their quotation
\ while the walk's cursor is open; a value it is given borrows the text, not
\ the cursor, so it stays valid after the walk.
\ STORAGE CLASS. CALLER-OWNED: the caller owns the text a value borrows and
\ the vector APPEND-STRING grows. The cursor record, its depth mapping, AT's
\ decoded token and WITH-STRING's decoded text live in a MEM:WITH-BYTES scope,
\ and the key storage on the heap, all released on return or throw. No module
\ state.

require src/lib/memory.f
require src/lib/vector.f
require src/lib/adt/option.f
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
   state doc at len + INIT-STATE
   at state CURSOR-POS ! ;

: RUN-BODY ( R ^cursor ^u8 [ R ^cursor ^u8 -- S ] -- S )
   execute ;

\ WITH-RECORD zero-fills the record, so it owns no mapping until it grows one.
: CURSOR-SCOPE ( R ^cursor ^u8 value [ R ^cursor ^u8 -- S ] -- S )
   {: state:^ base:^ v body :}
   state v OPEN-AT
   state base body
   [: RUN-BODY ;] [: state MORE-RELEASE ;] finally ;

\ Runs body on a cursor at the value's first byte and n bytes of
\ zero-filled scratch, both released on return or throw.
: WITH-CURSOR ( R value n [ R ^cursor ^u8 -- S ] -- S )
   {: v extra:n body :}
   [type] cursor
   [: extra MEM:BYTES-ALLOC-LEN [: drop v body CURSOR-SCOPE ;] MEM:WITH-BYTES ;]
   MEM:WITH-RECORD ;

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
: TRIMMED ( str -- value ) {: text:^ u:n :}
   0 begin text over + c@ WS? while 1+ repeat {: at:n :}
   u begin text over + 1- c@ WS? while 1- repeat {: end:n :}
   text at end at - VALUE-MAKE ;

public

\ A text that is not one JSON value throws the reader's code; a repeated
\ member name, decoded, throws E-MEMBER at the repeat earliest in the text.
: PARSE ( str -- value ) {: text:^ u:n :}
   text u REQUIRE-SOURCE
   text 0 u VALUE-MAKE 0
   [: drop [: CHECK-KEYS ;] WITH-KEYS ;] WITH-CURSOR
   text u TRIMMED ;

: TEXT$ ( value -- str )
   VALUE-UNMAKE {: doc:^ at:n len:n :}
   doc at + len ;

private

: FIRST-BYTE ( value -- n )
   TEXT$ drop c@ ;

public

: STRING? ( value -- bool )  FIRST-BYTE DQ = ;
: OBJECT? ( value -- bool )  FIRST-BYTE LBRACE = ;
: ARRAY? ( value -- bool )  FIRST-BYTE LBRACK = ;
: NULL? ( value -- bool )  FIRST-BYTE CH-N = ;

: BOOL? ( value -- bool )
   FIRST-BYTE dup CH-T = swap CH-F = or ;

: NUMBER? ( value -- bool )
   FIRST-BYTE dup MINUS = swap STR-DIGIT? or ;

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

\ A number lexeme's value, E-JSON-READ-NUMBER when no signed cell holds
\ it: a fraction, or past a cell.
: WHOLE-OF ( str -- n )
   {: p:^ u:n :}
   p u MANTISSA drop drop drop 0< if 0 exit then
   p u DIGITS {: x:n f:n count:n :}
   count x > if E-JSON-READ-NUMBER throw then
   p  p x f count NEGATED-WHOLE  SIGNED ;

\ WHOLE-OF's answer replaces the slot under the lexeme, so the quotation
\ under catch leaves the types it was given.
: WHOLE-SLOT ( n str -- n str )
   {: p:^ u:n :}
   drop  p u WHOLE-OF  p u ;

\ Some for a number whose value is whole and fits a signed cell, whatever
\ its spelling (1.0, 1e2, -0).
: WHOLE-VALUE ( value -- option<n> )
   {: v :}
   v NUMBER? 0= if OPTION:NONE exit then
   0 v TEXT$ [: WHOLE-SLOT ;] catch {: n p u code:n :}
   code 0= if n OPTION:SOME exit then
   code E-JSON-READ-NUMBER = if OPTION:NONE exit then
   code throw ;

public

: INTEGER? ( value -- bool )
   WHOLE-VALUE MATCH option
     none OF false ENDOF
     some OF drop true ENDOF
   ;MATCH ;

private

\ Throws E-MEMBER at the current token, the found part its kind.
: WRONG-KIND ( ^cursor str str -- )
   {: state:^ ea:^ eu:n fa:^ fu:n :}
   state state TOKEN-START REJECT-HEAD
   ea eu REASON:APPEND
   REASON:FOUND state CURSOR-KIND @ KIND$ REASON:APPEND
   REASON:FIX fa fu REASON:APPEND
   E-MEMBER REASON:STOP ;

\ The value at the current token; the cursor moves past it.
: TOKEN-VALUE ( ^cursor -- value ) {: state:^ :}
   state TOKEN-START {: at:n :}
   state ANY-SPAN-INNER nip {: u:n :}
   state CURSOR-SRC @ at u VALUE-MAKE ;

\ The current key as a string value, its quotes included.
: NAME-VALUE ( ^cursor -- value ) {: state:^ :}
   state CURSOR-SRC @ state TOKEN-START
   state CURSOR-TOK-LEN @ 2 + VALUE-MAKE ;

\ The str is the fix a wrong kind's rejection gives; it names the caller's walk.
: ELEMENTS ( R ^cursor str [ R value -- R bool ] -- R )
   {: state:^ fa:^ fu:n body :}
   state NEXT-INNER T-ARR <> if state s" an array" fa fu WRONG-KIND then
   begin state NEXT-INNER T-ARR-END <> while
      state TOKEN-VALUE body execute 0= if exit then
   repeat ;

\ The str is the fix, as in ELEMENTS.
: MEMBERS ( R ^cursor str [ R value value -- R bool ] -- R )
   {: state:^ fa:^ fu:n body :}
   state NEXT-INNER T-OBJ <> if state s" an object" fa fu WRONG-KIND then
   begin state NEXT-INNER T-OBJ-END <> while
      state NAME-VALUE
      state NEXT-INNER drop state TOKEN-VALUE body execute 0= if exit then
   repeat ;

public

: EACH ( R value [ R value -- R ] -- R ) {: v body :}
   v 0 [: drop s" walk only an array with JSON:EACH"
      [: body execute true ;] ELEMENTS ;] WITH-CURSOR ;

\ The name is the key as written, a string value.
: EACH-MEMBER ( R value [ R value value -- R ] -- R )
   {: v body :}
   v 0 [: drop s" walk only an object with JSON:EACH-MEMBER"
      [: body execute true ;] MEMBERS ;] WITH-CURSOR ;

: EACH-WHILE ( R value [ R value -- R bool ] -- R )
   {: v body :}
   v 0 [: drop s" walk only an array with JSON:EACH-WHILE" body ELEMENTS ;]
   WITH-CURSOR ;

: EACH-MEMBER-WHILE ( R value [ R value value -- R bool ] -- R )
   {: v body :}
   v 0 [: drop s" walk only an object with JSON:EACH-MEMBER-WHILE" body MEMBERS ;]
   WITH-CURSOR ;

private

\ Opens the value the cursor reads next and answers its `{`; any other
\ value is rejected with its kind.
: ENTER-OBJECT ( ^cursor -- n ) {: state:^ :}
   state NEXT-INNER T-OBJ = if state TOKEN-START exit then
   state s" an object" s" read a member only from an object" WRONG-KIND ;

\ Quotes the span, cut at REASON:QUOTE-MOST on a character's first byte.
: QUOTE-CUT ( str -- ) {: a:^ u:n :}
   a u REASON:QUOTE-MOST UTF8:FIT {: k:n :}
   a k  k u <  REASON:QUOTE ;

: MISSING ( ^cursor n str -- ) {: state:^ at:n name:^ u:n :}
   state at REJECT-HEAD
   s" a member " REASON:APPEND  name u QUOTE-CUT
   REASON:FOUND s" none" REASON:APPEND
   REASON:FIX s" add the member or read it with JSON:FIND" REASON:APPEND
   E-MEMBER REASON:STOP ;

: MEMBER-VALUE ( ^cursor str -- value ) {: state:^ name:^ u:n :}
   state ENTER-OBJECT {: at:n :}
   state name u FIND-INNER if state TOKEN-VALUE exit then
   state at name u MISSING ;

: MEMBER-OPTION ( ^cursor str -- option<value> )
   {: state:^ name:^ u:n :}
   state ENTER-OBJECT drop
   state name u FIND-INNER 0= if OPTION:NONE exit then
   state TOKEN-VALUE OPTION:SOME ;

: POINTED ( ^cursor str ^u8 -- option<value> )
   {: state:^ p:^ u:n room:^ :}
   state p u room SEEK {: at:n len:n found:bool :}
   found 0= if OPTION:NONE exit then
   state CURSOR-SRC @ at len VALUE-MAKE OPTION:SOME ;

public

\ The member of that decoded name. A value that is not an object, or an
\ object without the member, throws E-MEMBER.
: GET ( value str -- value ) {: v name:^ u:n :}
   v 0 [: drop name u MEMBER-VALUE ;] WITH-CURSOR ;

\ None for an object without the member; a value that is not an object
\ throws E-MEMBER.
: FIND ( value str -- option<value> ) {: v name:^ u:n :}
   v 0 [: drop name u MEMBER-OPTION ;] WITH-CURSOR ;

\ The value the RFC 6901 pointer names from this one, none for no such place.
\ A pointer POINTER? rejects throws E-JSON-POINTER before anything is read.
: AT ( value str -- option<value> ) {: v p:^ u:n :}
   p u POINTER? 0= if E-JSON-POINTER throw then
   v u [: p u rot POINTED ;] WITH-CURSOR ;

private

: MATCH-AT ( ^cursor str -- bool ) {: state:^ want:^ len:n :}
   state NEXT-INNER drop
   state want len MATCH-INNER ;

\ A string's text inside its quotes.
: INSIDE$ ( value -- str )
   VALUE-UNMAKE {: doc:^ at:n len:n :}
   doc at + 1+ len 2 - ;

: ESCAPED? ( str -- bool )
   BACKSLASH INDEX-OF MATCH option
     none OF false ENDOF
     some OF drop true ENDOF
   ;MATCH ;

: NEXT-STRING ( ^cursor -- ) {: state:^ :}
   state NEXT-INNER T-STR <> if
      state s" a string" s" write a string" WRONG-KIND
   then ;

\ The scratch holds the value, so its decoded text fits.
: DECODE-RUN ( R ^cursor ^u8 [ R str -- S ] -- S )
   {: state:^ buf:^ body :}
   state NEXT-STRING
   state buf state CURSOR-TOK-LEN @ STR-INNER {: u:n :}
   buf u body execute ;

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

\ Streams the decoding, so no buffer bounds the string.
: STRING= ( value str -- bool ) {: v want:^ len:n :}
   v STRING? 0= if false exit then
   v 0 [: drop want len MATCH-AT ;] WITH-CURSOR ;

\ The text is lent for the call only: the bytes inside the quotes when the
\ string holds no escape, else its decoding in the cursor's scratch.
: WITH-STRING ( R value [ R str -- S ] -- S ) {: v body :}
   v STRING? if
      v INSIDE$ 2dup ESCAPED? 0= if body execute exit then
      2drop
   then
   v v TEXT$ nip [: body DECODE-RUN ;] WITH-CURSOR ;

\ Appends the string's decoded bytes after the vector's own.
: APPEND-STRING ( value ^VEC:vector<u8> -- ) {: v out:^ :}
   v STRING? if
      v INSIDE$ 2dup ESCAPED? 0= if out VEC:APPEND exit then
      2drop
   then
   v 0 [: drop dup NEXT-STRING out DECODE-APPEND ;] WITH-CURSOR ;

\ A number whose value is whole and fits a signed cell.
: >N ( value -- n )
   {: v :}
   v WHOLE-VALUE MATCH option
     none OF v 0 [: drop NOT-WHOLE ;] WITH-CURSOR ENDOF
     some OF ENDOF
   ;MATCH ;

: >BOOL ( value -- bool )
   0 [: drop BOOL-AT ;] WITH-CURSOR ;

private

: VALUE-REJECT ( ^cursor str str -- ) {: state:^ ea:^ eu:n fa:^ fu:n :}
   state state CURSOR-POS @ ea eu fa fu E-MEMBER REJECT-AT ;

public

\ Throws E-MEMBER at the value's first byte with this expected text and fix.
: REJECT ( value str str -- ) {: v ea:^ eu:n fa:^ fu:n :}
   v 0 [: drop ea eu fa fu VALUE-REJECT ;] WITH-CURSOR ;

;package
