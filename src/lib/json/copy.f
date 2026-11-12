\ copy.f - COPY-VALUE, one value from a reader into a writer.

require src/lib/json/read.f
require src/lib/json/write.f

package JSON

public

: COPY-VALUE ( JSON:reader ptr JSON:writer ptr u8 n -- JSON:reader ptr JSON:writer )
   {: dst:ptr cap:n :}
   swap READ-TOKEN {: kind:n :} swap
   kind T-STR = if
      swap dst cap READ-STR {: len:n :} swap
      dst len WRITE-STRING exit
   then
   kind T-INT = kind T-FLOAT = or if
      swap READ-SPAN$ {: a:ptr u:n :} swap
      a u WRITE-RAW exit
   then
   kind T-TRUE = if true WRITE-BOOL exit then
   kind T-FALSE = if false WRITE-BOOL exit then
   kind T-NULL = if WRITE-NULL exit then
   kind T-OBJ = if
      WRITE-OBJECT-START
      swap READ-NEXT rot swap
      begin dup T-OBJ-END <> while
         T-KEY <> if E-JSON-READ-STATE throw then
         swap dst cap READ-STR {: len:n :} swap
         dst len WRITE-KEY
         swap READ-NEXT drop swap
         dst cap RECURSE
         swap READ-NEXT rot swap
         dup T-OBJ-END <> if >r WRITE-COMMA r> then
      repeat drop
      WRITE-OBJECT-END exit
   then
   kind T-ARR = if
      WRITE-ARRAY-START
      swap READ-NEXT rot swap
      begin dup T-ARR-END <> while
         drop
         dst cap RECURSE
         swap READ-NEXT rot swap
         dup T-ARR-END <> if >r WRITE-COMMA r> then
      repeat drop
      WRITE-ARRAY-END exit
   then
   E-JSON-READ-STATE throw ;

;package
