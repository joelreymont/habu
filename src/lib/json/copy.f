split-str
\ copy.f - COPY-VALUE, one JSON:value into a writer.
\
\ COPY-VALUE walks the value's tokens with a cursor in one loop, counting
\ containers opened minus closed, and stops after the token that brings the
\ count back to 0: a value of any depth copies in constant stack. A comma is
\ due before a key, a value or a start exactly when the token written last was
\ a value or an end.

require src/lib/vector.f
require src/lib/heap.f
require src/lib/json/read.f
require src/lib/json/write.f
require src/lib/json/value.f

package JSON

private

: COPY-COMMA ( ^writer bool -- ^writer )
   if WRITE-COMMA then ;

: COPY-KEY ( ^writer ^cursor ^VEC:vector<u8> -- ^writer )
   {: state:^ text:^ :}
   state text DECODED$ WRITE-KEY ;

: COPY-SCALAR ( ^writer ^cursor ^VEC:vector<u8> n -- ^writer )
   {: state:^ text:^ kind:n :}
   kind T-STR = if state text DECODED$ WRITE-STRING exit then
   kind T-INT = kind T-FLOAT = or if
      state RAW-SPAN$ WRITE-RAW exit
   then
   kind T-TRUE = if true WRITE-BOOL exit then
   kind T-FALSE = if false WRITE-BOOL exit then
   WRITE-NULL ;

\ Open a container: the depth one deeper, no comma due.
: COPY-OPEN ( ^writer n n bool -- n bool )
   {: w kind:n depth:n sep:bool :}
   w sep COPY-COMMA
   kind T-OBJ = if WRITE-OBJECT-START else WRITE-ARRAY-START then
   drop depth 1 + false ;

\ Close a container: the depth one shallower, a comma due.
: COPY-CLOSE ( ^writer n n -- n bool )
   {: w kind:n depth:n :}
   w kind T-OBJ-END = if WRITE-OBJECT-END else WRITE-ARRAY-END then
   drop depth 1 - true ;

\ Write the cursor's current token, of kind kind, and answer the new depth
\ and whether a comma is due before the next token.
: COPY-STEP ( n n bool ^writer ^cursor ^VEC:vector<u8> -- n bool )
   {: kind:n depth:n sep:bool w state:^ text:^ :}
   kind T-OBJ = kind T-ARR = or if
      w kind depth sep COPY-OPEN exit
   then
   kind T-OBJ-END = kind T-ARR-END = or if
      w kind depth COPY-CLOSE exit
   then
   kind T-KEY = if
      w sep COPY-COMMA state text COPY-KEY drop depth false exit
   then
   w sep COPY-COMMA state text kind COPY-SCALAR drop depth true ;

\ Each key or string decodes into the vector, in place of the one before.
: COPY-WALK ( ^writer ^cursor ^VEC:vector<u8> -- ^writer )
   {: w state:^ text:^ :}
   state NEXT-INNER 0 false w state text COPY-STEP
   begin over 0<> while
      state NEXT-INNER -rot w state text COPY-STEP
   repeat drop drop w ;

public

: COPY-VALUE ( JSON:value ^JSON:writer -- ^JSON:writer )
   {: v w :}
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW {: text:^ :}
   [: w v [: text COPY-WALK ;] WITH-CURSOR ;]
   [: text VEC:DELETE ;] finally ;

;package
