\ copy.f - COPY-VALUE, one value from a reader into a writer.
\
\ COPY-VALUE walks the reader's tokens in one loop, counting containers opened
\ minus closed, and stops after the token that brings the count back to 0: a
\ value of any depth copies in constant stack. A comma is due before a key, a
\ value or a start exactly when the token written last was a value or an end.

require src/lib/json/read.f
require src/lib/json/write.f

package JSON

private

\ T-STR through T-NULL are the scalar kinds.
: COPY-SCALAR? ( n -- bool )
   {: kind:n :}
   kind T-STR >= kind T-NULL <= and ;

: COPY-COMMA ( ^writer bool -- ^writer )
   if WRITE-COMMA then ;

: COPY-KEY ( JSON:reader ^writer ^u8 n -- JSON:reader ^writer )
   {: dst:^ cap:n :}
   swap dst cap READ-STR {: len:n :} swap
   dst len WRITE-KEY ;

: COPY-SCALAR ( JSON:reader ^writer ^u8 n n -- JSON:reader ^writer )
   {: dst:^ cap:n kind:n :}
   kind T-STR = if
      swap dst cap READ-STR {: len:n :} swap
      dst len WRITE-STRING exit
   then
   kind T-INT = kind T-FLOAT = or if
      swap READ-SPAN$ {: a:^ u:n :} swap
      a u WRITE-RAW exit
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
   depth 0= if E-JSON-READ-STATE throw then
   w kind T-OBJ-END = if WRITE-OBJECT-END else WRITE-ARRAY-END then
   drop depth 1 - true ;

\ Write the reader's current token, of kind kind, and answer the new depth
\ and whether a comma is due before the next token.
: COPY-STEP ( JSON:reader n n bool ^writer ^u8 n -- JSON:reader n bool )
   {: kind:n depth:n sep:bool w dst:^ cap:n :}
   kind T-OBJ = kind T-ARR = or if
      w kind depth sep COPY-OPEN exit
   then
   kind T-OBJ-END = kind T-ARR-END = or if
      w kind depth COPY-CLOSE exit
   then
   kind T-KEY = if
      depth 0= if E-JSON-READ-STATE throw then
      w sep COPY-COMMA dst cap COPY-KEY drop depth false exit
   then
   kind COPY-SCALAR? 0= if E-JSON-READ-STATE throw then
   w sep COPY-COMMA dst cap kind COPY-SCALAR drop depth true ;

: COPY-ADVANCE ( JSON:reader n bool -- JSON:reader n n bool )
   {: depth:n sep:bool :}
   READ-NEXT depth sep ;

public

: COPY-VALUE ( JSON:reader ^JSON:writer ^u8 n -- JSON:reader ^JSON:writer )
   {: w dst:^ cap:n :}
   READ-TOKEN 0 false w dst cap COPY-STEP
   begin over 0<> while
      COPY-ADVANCE w dst cap COPY-STEP
   repeat drop drop w ;

;package
