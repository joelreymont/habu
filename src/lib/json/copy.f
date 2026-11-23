split-str
\ copy.f - COPY-VALUE, one JSON:value into a writer.
\
\ COPY-VALUE walks the value's tokens with a cursor in one loop, counting
\ containers opened minus closed, and stops after the token that brings the
\ count back to 0: a value of any depth copies in constant stack. A comma is
\ due before a key, a value or a start exactly when the token written last was
\ a value or an end.

require src/lib/json/read.f
require src/lib/json/write.f
require src/lib/json/value.f

package JSON

private

: COPY-COMMA ( ^writer bool -- ^writer )
   if WRITE-COMMA then ;

: COPY-KEY ( ^writer ^cursor ^u8 n -- ^writer )
   {: state:^ dst:^ cap:n :}
   state dst cap STR-INNER {: len:n :}
   dst len WRITE-KEY ;

: COPY-SCALAR ( ^writer ^cursor ^u8 n n -- ^writer )
   {: state:^ dst:^ cap:n kind:n :}
   kind T-STR = if
      state dst cap STR-INNER {: len:n :}
      dst len WRITE-STRING exit
   then
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
: COPY-STEP ( n n bool ^writer ^cursor ^u8 n -- n bool )
   {: kind:n depth:n sep:bool w state:^ dst:^ cap:n :}
   kind T-OBJ = kind T-ARR = or if
      w kind depth sep COPY-OPEN exit
   then
   kind T-OBJ-END = kind T-ARR-END = or if
      w kind depth COPY-CLOSE exit
   then
   kind T-KEY = if
      w sep COPY-COMMA state dst cap COPY-KEY drop depth false exit
   then
   w sep COPY-COMMA state dst cap kind COPY-SCALAR drop depth true ;

\ The scratch holds the value, so a decoded key or string fits.
: COPY-WALK ( ^writer ^cursor ^u8 n -- ^writer )
   {: w state:^ dst:^ cap:n :}
   state NEXT-INNER 0 false w state dst cap COPY-STEP
   begin over 0<> while
      state NEXT-INNER -rot w state dst cap COPY-STEP
   repeat drop drop w ;

public

: COPY-VALUE ( JSON:value ^JSON:writer -- ^JSON:writer )
   {: v w :}
   v TEXT$ nip {: len:n :}
   w v len [: len COPY-WALK ;] WITH-CURSOR ;

;package
