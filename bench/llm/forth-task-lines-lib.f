\ forth-task-lines-lib.f - checked harness=forth task-row emitter.
\
\ Load after lib/errors.f, lib/string.f, lib/fs.f, and bench/llm/manifest.f.

65536 constant FTL-TASK-CAP
65536 constant FTL-OUT-CAP
10 constant FTL-LF

create FTL-TASK-BUF FTL-TASK-CAP allot
create FTL-OUT-BUF FTL-OUT-CAP allot

variable FTL-TASK-U
variable FTL-OUT-U
variable FTL-NEXT

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
   FTL-SCAN-DATA
   FTL$ ;

: FTL-LOAD ( ptr u8 n -- )
   FTL-TASK-BUF FTL-TASK-CAP READ-ALL FTL-TASK-U ! ;

: FTL-WRITE-DATA ( ptr u8 n ptr u8 n -- ) {: data:ptr datau out:ptr outu :}
   data datau FTL-EMIT-DATA out outu 2swap WRITE-ALL ;

: FTL-WRITE-FILE ( ptr u8 n ptr u8 n -- ) {: path:ptr pathu out:ptr outu :}
   path pathu FTL-LOAD
   FTL-TASK-BUF FTL-TASK-U @ out outu FTL-WRITE-DATA ;
