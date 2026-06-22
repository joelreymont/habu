\ attempt-solutions-lib.f - checked reference extractor for attempt runners.
\
\ Load after lib/errors.f, lib/string.f, lib/memory.f, lib/vector.f, lib/fs.f, lib/fs-mutate.f,
\ bench/llm/manifest.f, lib/memory.f, tools/lint/lib.f, and
\ tools/lint/source-lex.f.

128 constant AS-MAX
128 constant AS-FIELD-CAP
46 constant AS-DOT
102 constant AS-F
10 constant AS-LF

-3220 constant E-AS-CAPACITY
-3221 constant E-AS-MISSING
-3222 constant E-AS-EXTRA
-3223 constant E-AS-DUPLICATE
-3224 constant E-AS-SYNTAX

variable AS-TASK-P
variable AS-TASK-CAP-U
variable AS-SOL-P
variable AS-SOL-CAP-U
create AS-ID-BUF AS-MAX AS-FIELD-CAP * allot
create AS-NAME-BUF AS-MAX AS-FIELD-CAP * allot
create AS-ID-U AS-MAX cells allot
create AS-NAME-U AS-MAX cells allot
create AS-SEEN AS-MAX cells allot
create AS-OUT-DIR FS-PATH-CAP allot
create AS-FILE-NAME FS-PATH-CAP allot
create AS-PATH FS-PATH-CAP allot
create AS-LF-BUF 1 allot

AS-LF AS-LF-BUF c!

variable AS-TASK-LEN
variable AS-SOL-LEN
variable AS-TASK-NEXT
variable AS-LINE-A
variable AS-LINE-U
variable AS-COUNT
variable AS-OUT-U
variable AS-I
variable AS-J
variable AS-K
variable AS-SEMI

TRUSTED: AS-LINE! ( ptr u8 n -- )
   AS-LINE-U ! AS-LINE-A ! ;

TRUSTED: AS-LINE$ ( -- ptr u8 n )
   AS-LINE-A @ AS-LINE-U @ ;

TRUSTED: AS-TASK-BUF ( -- ptr u8 )
   AS-TASK-P @ ;

TRUSTED: AS-SOL-BUF ( -- ptr u8 )
   AS-SOL-P @ ;

: AS-TASK-CAP ( -- n )
   AS-TASK-CAP-U @ ;

: AS-SOL-CAP ( -- n )
   AS-SOL-CAP-U @ ;

: AS-MIN-ONE ( n -- n )
   dup 1 < if drop 1 then ;

: AS-STORE-TASK-SPAN ( ptr u8 n -- )
   AS-TASK-CAP-U ! AS-TASK-P ! ;

: AS-STORE-SOL-SPAN ( ptr u8 n -- )
   AS-SOL-CAP-U ! AS-SOL-P ! ;

: AS-ENSURE-TASK-CAP ( n -- ) {: need :}
   need AS-MIN-ONE AS-TASK-CAP <= if exit then
   need AS-MIN-ONE MEM-ALLOC-64K-SPAN AS-STORE-TASK-SPAN ;

: AS-ENSURE-SOL-CAP ( n -- ) {: need :}
   need AS-MIN-ONE AS-SOL-CAP <= if exit then
   need AS-MIN-ONE MEM-ALLOC-64K-SPAN AS-STORE-SOL-SPAN ;

: AS-TRUE ( -- bool )
   0 0= ;

: AS-FALSE ( -- bool )
   AS-TRUE 0= ;

: AS-CHECK-INDEX ( n -- ) {: idx :}
   idx 0 < if E-AS-CAPACITY throw then
   idx AS-MAX >= if E-AS-CAPACITY throw then ;

: AS-ID-SLOT ( n -- ptr u8 ) {: idx :}
   idx AS-CHECK-INDEX
   AS-ID-BUF idx AS-FIELD-CAP * + ;

: AS-NAME-SLOT ( n -- ptr u8 ) {: idx :}
   idx AS-CHECK-INDEX
   AS-NAME-BUF idx AS-FIELD-CAP * + ;

: AS-ID-LEN-PTR ( n -- ptr n ) {: idx :}
   idx AS-CHECK-INDEX
   AS-ID-U idx cells + ;

: AS-NAME-LEN-PTR ( n -- ptr n ) {: idx :}
   idx AS-CHECK-INDEX
   AS-NAME-U idx cells + ;

: AS-SEEN-PTR ( n -- ptr n ) {: idx :}
   idx AS-CHECK-INDEX
   AS-SEEN idx cells + ;

: AS-ID$ ( n -- ptr u8 n ) {: idx :}
   idx AS-ID-SLOT idx AS-ID-LEN-PTR @ ;

: AS-NAME$ ( n -- ptr u8 n ) {: idx :}
   idx AS-NAME-SLOT idx AS-NAME-LEN-PTR @ ;

: AS-SEEN? ( n -- bool )
   AS-SEEN-PTR @ 0 <> ;

: AS-SEEN! ( n -- )
   -1 swap AS-SEEN-PTR ! ;

: AS-COPY-FIELD ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u dst:ptr lenp:ptr :}
   u 0 <= if E-BM-FIELD throw then
   u AS-FIELD-CAP > if E-AS-CAPACITY throw then
   a dst u BYTE-COPY
   u lenp ! ;

: AS-RESET ( -- )
   0 AS-COUNT !
   0 AS-OUT-U !
   0 begin dup AS-MAX < while
      0 over AS-ID-LEN-PTR !
      0 over AS-NAME-LEN-PTR !
      0 over AS-SEEN-PTR !
      1+
   repeat drop ;

: AS-TASKS! ( ptr u8 n -- ) {: a:ptr u :}
   u 0 < if E-BM-SCHEMA throw then
   u AS-ENSURE-TASK-CAP
   a AS-TASK-BUF u BYTE-COPY
   u AS-TASK-LEN ! ;

: AS-SOLUTIONS! ( ptr u8 n -- ) {: a:ptr u :}
   u 0 < if E-AS-SYNTAX throw then
   u AS-ENSURE-SOL-CAP
   a AS-SOL-BUF u BYTE-COPY
   u AS-SOL-LEN ! ;

: AS-LOAD-TASKS ( ptr u8 n -- )
   2dup FILE-SIZE AS-ENSURE-TASK-CAP
   AS-TASK-BUF AS-TASK-CAP READ-ALL AS-TASK-LEN ! ;

: AS-LOAD-SOLUTIONS ( ptr u8 n -- )
   2dup FILE-SIZE AS-ENSURE-SOL-CAP
   AS-SOL-BUF AS-SOL-CAP READ-ALL AS-SOL-LEN ! ;

: AS-OUT-DIR! ( ptr u8 n -- ) {: a:ptr u :}
   u 0 <= if E-AS-CAPACITY throw then
   u FS-PATH-CAP > if E-AS-CAPACITY throw then
   a AS-OUT-DIR u BYTE-COPY
   u AS-OUT-U ! ;

: AS-READ-TASK-LINE ( -- bool )
   AS-TASK-BUF AS-TASK-LEN @ AS-TASK-NEXT @ BM-LINE-NEXT if
      AS-TASK-NEXT !
      AS-LINE!
      AS-TRUE exit
   then
   drop 2drop AS-FALSE ;

: AS-FIND-ID ( ptr u8 n -- n bool ) {: a:ptr u :}
   0 begin dup AS-COUNT @ < while
      dup AS-ID$ a u STR= if AS-TRUE exit then
      1+
   repeat drop 0 AS-FALSE ;

: AS-FIND-NAME ( ptr u8 n -- n bool ) {: a:ptr u :}
   0 begin dup AS-COUNT @ < while
      dup AS-NAME$ a u STR= if AS-TRUE exit then
      1+
   repeat drop 0 AS-FALSE ;

: AS-REQUIRE-NEW-ID ( ptr u8 n -- )
   2dup AS-FIND-ID if
      E-AS-DUPLICATE throw
   then
   drop 2drop ;

: AS-REQUIRE-NEW-NAME ( ptr u8 n -- )
   2dup AS-FIND-NAME if
      E-AS-DUPLICATE throw
   then
   drop 2drop ;

: AS-ADD-TASK ( -- )
   AS-COUNT @ AS-MAX >= if E-AS-CAPACITY throw then
   AS-LINE$ BM-T-ID BM-TASK-FIELD$ AS-REQUIRE-NEW-ID
   AS-LINE$ BM-T-NAME BM-TASK-FIELD$ AS-REQUIRE-NEW-NAME
   AS-COUNT @ {: idx :}
   AS-LINE$ BM-T-ID BM-TASK-FIELD$ idx AS-ID-SLOT idx AS-ID-LEN-PTR AS-COPY-FIELD
   AS-LINE$ BM-T-NAME BM-TASK-FIELD$ idx AS-NAME-SLOT idx AS-NAME-LEN-PTR AS-COPY-FIELD
   0 idx AS-SEEN-PTR !
   AS-COUNT @ 1+ AS-COUNT ! ;

: AS-HARNESS-FORTH? ( -- bool )
   AS-LINE$ BM-T-HARNESS BM-TASK-FIELD$ s" forth" STR= ;

: AS-BUILD-TASKS ( -- )
   0 AS-TASK-NEXT !
   AS-READ-TASK-LINE 0= if E-BM-SCHEMA throw then
   AS-LINE$ BM-REQUIRE-TASK-HEADER
   begin AS-READ-TASK-LINE while
      AS-LINE$ BM-BLANK-OR-COMMENT? 0= if
         AS-LINE$ BM-TASK-FIELDS BM-REQUIRE-FIELDS
         AS-HARNESS-FORTH? if AS-ADD-TASK then
      then
   repeat ;

: AS-TOK= ( n ptr u8 n -- bool ) {: k a:ptr u :}
   k LK@ L-WORD <> if AS-FALSE exit then
   k LTOK a u STR= ;

: AS-FIND-DEF-END ( n -- n bool ) {: start :}
   start AS-J !
   begin AS-J @ L# @ < while
      AS-J @ s" ;" AS-TOK= if AS-J @ AS-TRUE exit then
      AS-J @ 1+ AS-J !
   repeat
   start AS-FALSE ;

: AS-DEF-END ( n -- n )
   AS-FIND-DEF-END if exit then
   E-AS-SYNTAX throw ;

: AS-TOK-END-BYTE ( n -- n ) {: k :}
   k LB@ k LTOK nip + ;

: AS-FILE-NAME$ ( n -- ptr u8 n ) {: idx :}
   idx AS-ID$ {: a:ptr u :}
   u 2 + FS-PATH-CAP > if E-AS-CAPACITY throw then
   a AS-FILE-NAME u BYTE-COPY
   AS-DOT AS-FILE-NAME u + c!
   AS-F AS-FILE-NAME u 1 + + c!
   AS-FILE-NAME u 2 + ;

: AS-OUT-PATH$ ( n -- ptr u8 n ) {: idx :}
   AS-OUT-U @ 0 <= if E-AS-CAPACITY throw then
   AS-OUT-DIR AS-OUT-U @ idx AS-FILE-NAME$ AS-PATH JOIN-PATH {: u :}
   AS-PATH u ;

: AS-WRITE-SOLUTION ( n ptr u8 n -- ) {: idx a:ptr u :}
   idx AS-OUT-PATH$ a u WRITE-ALL
   idx AS-OUT-PATH$ AS-LF-BUF 1 APPEND-FILE ;

: AS-DEF-SOURCE$ ( n n -- ptr u8 n ) {: start semi :}
   AS-SOL-BUF start LB@ +
   semi AS-TOK-END-BYTE start LB@ - ;

: AS-REQUIRE-DEF-HEAD ( n -- ) {: k :}
   k 1+ L# @ >= if E-AS-SYNTAX throw then
   k 1+ LK@ L-WORD <> if E-AS-SYNTAX throw then ;

: AS-LOAD-DEF-NAME ( n -- ) {: k :}
   k 1+ LTOK AS-FIND-NAME if
      AS-I !
      exit
   then
   drop
   E-AS-EXTRA throw ;

: AS-REQUIRE-UNSEEN ( -- )
   AS-I @ AS-SEEN? if E-AS-DUPLICATE throw then ;

: AS-HANDLE-DEF ( n -- n ) {: k :}
   k AS-REQUIRE-DEF-HEAD
   k AS-LOAD-DEF-NAME
   AS-REQUIRE-UNSEEN
   k 2 + AS-DEF-END AS-SEMI !
   AS-I @ AS-SEEN!
   AS-OUT-U @ 0 > if AS-I @ k AS-SEMI @ AS-DEF-SOURCE$ AS-WRITE-SOLUTION then
   AS-SEMI @ 1+ ;

: AS-SCAN-SOLUTIONS ( -- )
   AS-SOL-BUF AS-SOL-LEN @ LEX-SOURCE
   0 AS-K !
   begin AS-K @ L# @ < while
      AS-K @ s" :" AS-TOK= if
         AS-K @ AS-HANDLE-DEF AS-K !
      else
         AS-K @ 1+ AS-K !
      then
   repeat ;

: AS-VERIFY-SEEN ( -- )
   0 begin dup AS-COUNT @ < while
      dup AS-SEEN? 0= if E-AS-MISSING throw then
      1+
   repeat drop ;

: AS-EXTRACT-DATA ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: tasks:ptr tasksu sol:ptr solu out:ptr outu :}
   AS-RESET
   tasks tasksu AS-TASKS!
   sol solu AS-SOLUTIONS!
   out outu AS-OUT-DIR!
   out outu MAKE-DIRS
   AS-BUILD-TASKS
   AS-SCAN-SOLUTIONS
   AS-VERIFY-SEEN ;

: AS-EXTRACT-FILES ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: tasks:ptr tasksu sol:ptr solu out:ptr outu :}
   AS-RESET
   tasks tasksu AS-LOAD-TASKS
   sol solu AS-LOAD-SOLUTIONS
   out outu AS-OUT-DIR!
   out outu MAKE-DIRS
   AS-BUILD-TASKS
   AS-SCAN-SOLUTIONS
   AS-VERIFY-SEEN ;
