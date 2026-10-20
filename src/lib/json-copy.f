\ Copy one JSON value from the reader's current token into a compact writer.
\ The caller owns the reader, writer, and a decode buffer at least as large as
\ the source value; escaped strings can never decode to more source bytes.

require src/lib/json-read.f
require src/lib/json-write.f

package JSON-COPY

public

: VALUE ( JR:reader ptr JSON-WRITE:writer ptr u8 n -- JR:reader ptr JSON-WRITE:writer )
   {: dst:ptr cap:n :}
   swap JR:TOKEN {: kind:n :} swap
   kind JR:T-STR = if
      swap dst cap JR:STR {: len:n :} swap
      dst len JSON-WRITE:STRING exit
   then
   kind JR:T-INT = kind JR:T-FLOAT = or if
      swap JR:SPAN$ {: a:ptr u:n :} swap
      a u JSON-WRITE:RAW exit
   then
   kind JR:T-TRUE = if true JSON-WRITE:BOOL exit then
   kind JR:T-FALSE = if false JSON-WRITE:BOOL exit then
   kind JR:T-NULL = if JSON-WRITE:NULL exit then
   kind JR:T-OBJ = if
      JSON-WRITE:OBJECT-START
      swap JR:NEXT rot swap
      begin dup JR:T-OBJ-END <> while
         JR:T-KEY <> if E-JR-STATE throw then
         swap dst cap JR:STR {: len:n :} swap
         dst len JSON-WRITE:KEY
         swap JR:NEXT drop swap
         dst cap RECURSE
         swap JR:NEXT rot swap
         dup JR:T-OBJ-END <> if >r JSON-WRITE:COMMA r> then
      repeat drop
      JSON-WRITE:OBJECT-END exit
   then
   kind JR:T-ARR = if
      JSON-WRITE:ARRAY-START
      swap JR:NEXT rot swap
      begin dup JR:T-ARR-END <> while
         drop
         dst cap RECURSE
         swap JR:NEXT rot swap
         dup JR:T-ARR-END <> if >r JSON-WRITE:COMMA r> then
      repeat drop
      JSON-WRITE:ARRAY-END exit
   then
   E-JR-STATE throw ;

;package
