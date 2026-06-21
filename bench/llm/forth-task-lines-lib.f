\ forth-task-lines-lib.f - checked harness=forth task-row emitter.
\
\ Load after lib/errors.f, lib/string.f, lib/memory.f, lib/fs.f, and
\ bench/llm/manifest.f.

10 constant FTL-LF

variable FTL-TASK-P
variable FTL-TASK-CAP-U
variable FTL-OUT-P
variable FTL-OUT-CAP-U
variable FTL-TASK-U
variable FTL-OUT-U
variable FTL-NEXT

TRUSTED: FTL-TASK-BUF ( -- ptr u8 )
   FTL-TASK-P @ ;

TRUSTED: FTL-OUT-BUF ( -- ptr u8 )
   FTL-OUT-P @ ;

: FTL-TASK-CAP ( -- n )
   FTL-TASK-CAP-U @ ;

: FTL-OUT-CAP ( -- n )
   FTL-OUT-CAP-U @ ;

: FTL-MIN-ONE ( n -- n )
   dup 1 < if drop 1 then ;

: FTL-STORE-TASK-SPAN ( ptr u8 n -- )
   FTL-TASK-CAP-U ! FTL-TASK-P ! ;

: FTL-STORE-OUT-SPAN ( ptr u8 n -- )
   FTL-OUT-CAP-U ! FTL-OUT-P ! ;

: FTL-ENSURE-TASK-CAP ( n -- ) {: need :}
   need FTL-MIN-ONE FTL-TASK-CAP <= if exit then
   need FTL-MIN-ONE MEM-ALLOC-64K-SPAN FTL-STORE-TASK-SPAN ;

: FTL-ENSURE-OUT-CAP ( n -- ) {: need :}
   need FTL-MIN-ONE FTL-OUT-CAP <= if exit then
   need FTL-MIN-ONE MEM-ALLOC-64K-SPAN FTL-STORE-OUT-SPAN ;

: FTL-OUT-NEED ( n -- n ) {: u :}
   u 0 < if E-BM-SCHEMA throw then
   u MEM-MAX-N >= if E-BM-SCHEMA throw then
   u 1 + ;

: FTL$ ( -- ptr u8 n )
   FTL-OUT-BUF FTL-OUT-U @ ;

: FTL-RESET ( -- )
   0 FTL-OUT-U !
   0 FTL-NEXT ! ;

: FTL-ROOM ( n -- ) {: add :}
   add 0 < if E-BM-SCHEMA throw then
   add FTL-OUT-CAP FTL-OUT-U @ - > if E-BM-SCHEMA throw then ;

: FTL-APPEND ( ptr u8 n -- ) {: a:ptr u :}
   u FTL-ROOM
   a FTL-OUT-BUF FTL-OUT-U @ + u BYTE-COPY
   FTL-OUT-U @ u + FTL-OUT-U ! ;

: FTL-NL ( -- )
   1 FTL-ROOM
   FTL-LF FTL-OUT-BUF FTL-OUT-U @ + c!
   FTL-OUT-U @ 1+ FTL-OUT-U ! ;

: FTL-FORTH? ( ptr u8 n -- bool )
   BM-T-HARNESS BM-TASK-FIELD$ s" forth" STR= ;

: FTL-APPEND-ROW ( ptr u8 n -- )
   FTL-APPEND
   FTL-NL ;

: FTL-EMIT-ROW? ( ptr u8 n -- ) {: a:ptr u :}
   a u BM-BLANK-OR-COMMENT? if exit then
   a u BM-TASK-FIELDS BM-REQUIRE-FIELDS
   a u FTL-FORTH? if a u FTL-APPEND-ROW then ;

: FTL-SCAN-DATA ( ptr u8 n -- ) {: a:ptr u :}
   FTL-RESET
   a u 0 BM-LINE-NEXT if
      FTL-NEXT !
      BM-REQUIRE-TASK-HEADER
   else
      drop 2drop E-BM-SCHEMA throw
   then
   begin a u FTL-NEXT @ BM-LINE-NEXT while
      FTL-NEXT !
      FTL-EMIT-ROW?
   repeat
   drop 2drop ;

: FTL-EMIT-DATA ( ptr u8 n -- ptr u8 n )
   dup FTL-OUT-NEED FTL-ENSURE-OUT-CAP
   FTL-SCAN-DATA
   FTL$ ;

: FTL-LOAD ( ptr u8 n -- )
   2dup FILE-SIZE FTL-ENSURE-TASK-CAP
   FTL-TASK-BUF FTL-TASK-CAP READ-ALL FTL-TASK-U ! ;

: FTL-WRITE-DATA ( ptr u8 n ptr u8 n -- ) {: data:ptr datau out:ptr outu :}
   data datau FTL-EMIT-DATA out outu 2swap WRITE-ALL ;

: FTL-WRITE-FILE ( ptr u8 n ptr u8 n -- ) {: path:ptr pathu out:ptr outu :}
   path pathu FTL-LOAD
   FTL-TASK-BUF FTL-TASK-U @ out outu FTL-WRITE-DATA ;

: FTL-FILE$ ( ptr u8 n -- ptr u8 n )
   FTL-LOAD
   FTL-TASK-BUF FTL-TASK-U @ FTL-EMIT-DATA ;
