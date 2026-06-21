\ forth-bundle.f - checked Forth benchmark bundle builder.
\
\ Load after lib/errors.f, lib/string.f, lib/fs.f, and bench/llm/manifest.f.

10 constant FB-LF
46 constant FB-DOT
102 constant FB-F

-3270 constant E-FB-CAPACITY
-3271 constant E-FB-MISSING
-3272 constant E-FB-DUPLICATE

create FB-ID-FILE FS-PATH-CAP allot
create FB-REF-PATH FS-PATH-CAP allot

variable FB-OUT-U
variable FB-READ-U
variable FB-NEXT
variable FB-DUP-NEXT
variable FB-LINE-START
variable FB-TARGET-SEEN
variable FB-REF-U

: FB-TRUE ( -- bool )
   0 0= ;

: FB-FALSE ( -- bool )
   FB-TRUE 0= ;

: FB-RESET ( -- )
   0 FB-OUT-U !
   0 FB-NEXT !
   0 FB-TARGET-SEEN ! ;

: FB-BUF-ROOM ( n n n -- ) {: add cap used :}
   add 0 < if E-FB-CAPACITY throw then
   add cap used - > if E-FB-CAPACITY throw then ;

: FB-BUF+ ( ptr u8 n ptr u8 n -- ) {: a:ptr u out:ptr cap :}
   u cap FB-OUT-U @ FB-BUF-ROOM
   a out FB-OUT-U @ + u BYTE-COPY
   FB-OUT-U @ u + FB-OUT-U ! ;

: FB-BUF-C ( n ptr u8 n -- ) {: c out:ptr cap :}
   1 cap FB-OUT-U @ FB-BUF-ROOM
   c out FB-OUT-U @ + c!
   FB-OUT-U @ 1+ FB-OUT-U ! ;

: FB-ID-FILE$ ( ptr u8 n -- ptr u8 n ) {: id:ptr idu :}
   idu 2 + FS-PATH-CAP > if E-FB-CAPACITY throw then
   id FB-ID-FILE idu BYTE-COPY
   FB-DOT FB-ID-FILE idu + c!
   FB-F FB-ID-FILE idu 1 + + c!
   FB-ID-FILE idu 2 + ;

: FB-REF-PATH! ( ptr u8 n ptr u8 n -- ) {: ref:ptr refu id:ptr idu :}
   ref refu id idu FB-ID-FILE$ FB-REF-PATH JOIN-PATH FB-REF-U ! ;

: FB-REF-PATH$ ( -- ptr u8 n )
   FB-REF-PATH FB-REF-U @ ;

: FB-APPEND-FILE ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: path:ptr pathu out:ptr outcap scratch:ptr scratchcap :}
   path pathu FILE? 0= if E-FB-MISSING throw then
   path pathu scratch scratchcap READ-ALL FB-READ-U !
   scratch FB-READ-U @ out outcap FB-BUF+ ;

: FB-APPEND-FILE-LN ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: path:ptr pathu out:ptr outcap scratch:ptr scratchcap :}
   path pathu out outcap scratch scratchcap FB-APPEND-FILE
   FB-LF out outcap FB-BUF-C ;

: FB-APPEND-REF-LN ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   {: ref:ptr refu id:ptr idu out:ptr outcap scratch:ptr scratchcap :}
   ref refu id idu FB-REF-PATH!
   FB-REF-PATH$ out outcap scratch scratchcap FB-APPEND-FILE-LN ;

: FB-LINE-ID$ ( ptr u8 n -- ptr u8 n )
   BM-T-ID BM-TASK-FIELD$ ;

: FB-VALID-TASK-LINE? ( ptr u8 n -- bool )
   BM-BLANK-OR-COMMENT? 0= ;

: FB-ID-DUP-BEFORE? ( ptr u8 n n ptr u8 n -- bool )
   {: tasks:ptr tasksu limit id:ptr idu :}
   0 FB-DUP-NEXT !
   begin FB-DUP-NEXT @ limit < while
      tasks tasksu FB-DUP-NEXT @ BM-LINE-NEXT if
         FB-DUP-NEXT !
         2dup FB-VALID-TASK-LINE? if
            2dup BM-TASK-FIELDS BM-REQUIRE-FIELDS
            2dup FB-LINE-ID$ id idu STR= if 2drop FB-TRUE exit then
         then
         2drop
      else
         drop 2drop FB-FALSE exit
      then
   repeat
   FB-FALSE ;

: FB-APPEND-TASK-SOURCE ( ptr u8 n ptr u8 n n ptr u8 n ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   {: line:ptr lineu tasks:ptr tasksu start ref:ptr refu target:ptr targetu cand:ptr candu out:ptr outcap scratch:ptr scratchcap :}
   line lineu FB-VALID-TASK-LINE? 0= if exit then
   line lineu BM-TASK-FIELDS BM-REQUIRE-FIELDS
   tasks tasksu start line lineu FB-LINE-ID$ FB-ID-DUP-BEFORE? if E-FB-DUPLICATE throw then
   line lineu FB-LINE-ID$ target targetu STR= if
      -1 FB-TARGET-SEEN !
      cand candu out outcap scratch scratchcap FB-APPEND-FILE-LN
      exit
   then
   ref refu line lineu FB-LINE-ID$ out outcap scratch scratchcap FB-APPEND-REF-LN ;

: FB-REQUIRE-TARGET ( -- )
   FB-TARGET-SEEN @ 0= if E-FB-MISSING throw then ;

: FB-BUILD-BUNDLE-INTO ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- n )
   {: tasks:ptr tasksu ref:ptr refu target:ptr targetu cand:ptr candu tests:ptr testsu out:ptr outcap scratch:ptr scratchcap :}
   FB-RESET
   begin
      FB-NEXT @ FB-LINE-START !
      tasks tasksu FB-NEXT @ BM-LINE-NEXT
   while
      FB-NEXT !
      tasks tasksu FB-LINE-START @ ref refu target targetu cand candu out outcap scratch scratchcap FB-APPEND-TASK-SOURCE
   repeat drop 2drop
   FB-REQUIRE-TARGET
   tests testsu out outcap scratch scratchcap FB-APPEND-FILE
   FB-OUT-U @ ;
