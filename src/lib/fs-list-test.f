\ fs-list-test.f - focused tests for src/lib/fs-list.f.
\ Run: bin/hb --load src/lib/errors.f src/lib/string.f src/lib/test.f src/lib/fs.f src/lib/fs-mutate.f src/lib/task.f src/lib/fs-list.f src/lib/fs-list-test.f

require src/lib/errors.f
require src/lib/string.f
require src/lib/test.f
require src/lib/test/wait.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/task.f
require src/lib/fs-list.f

create FLT-ROOT-BUF PATH-CAP allot
create FLT-PATH-BUF PATH-CAP allot
create FLT-NAMES 64 allot
variable FLT-ROOT-U
variable FLT-SEEN
variable FLT-SEEN-BYTES
variable FLT-TYPED-SEEN
variable FLT-TYPED-A
variable FLT-TYPED-B
variable FLT-TYPED-DIR
variable FLT-TYPED-LINK
variable FLT-TYPED-MANY
variable FLT-TYPED-LONG
create FLT-MANY-SEEN 160 allot
create FLT-MANY-NAME 4 allot
create FLT-LONG-NAME 240 allot
variable FLT-LONG-SEEN
create FLT-DIRENT-TYPE-FIXTURE 24 allot
create FLT-CTX-A FS-LIST:STORAGE-BYTES allot
create FLT-CTX-B FS-LIST:STORAGE-BYTES allot
create FLT-CTX-BAD FS-LIST:STORAGE-BYTES 1+ allot
TYPED-VARIABLE FLT-A FS-LIST:context
TYPED-VARIABLE FLT-B FS-LIST:context

: FLT-ROOT$ ( -- ptr u8 n )
   FLT-ROOT-BUF FLT-ROOT-U @ ;

: FLT-ROOT! ( -- )
   s" habu-fs-list" FS:MKTEMP-DIR {: a:ptr u:n :}
   a FLT-ROOT-BUF u BYTE-COPY
   u FLT-ROOT-U ! ;

: FLT-PATH ( ptr u8 n -- ptr u8 n )
   FLT-ROOT$ 2swap FLT-PATH-BUF FS:JOIN-PATH FLT-PATH-BUF swap ;

: FLT-COUNT ( ptr u8 n -- )
   nip FLT-SEEN-BYTES +! 1 FLT-SEEN +! ;

: FLT-EACH ( -- )
   0 FLT-SEEN ! 0 FLT-SEEN-BYTES !
   FLT-ROOT$ [: FLT-COUNT ;] FS-LIST:EACH ;

: FLT-TEST-EMPTY ( -- )
   FLT-EACH
   FLT-SEEN @ 0 T=
   FLT-ROOT$ FLT-NAMES 64 FS-LIST:NAMES 0 T= ;

\ Two files and a directory, created out of byte order.
: FLT-TEST-ENTRIES ( -- )
   s" b.txt" FLT-PATH s" second" FS:WRITE-ALL
   s" sub" FLT-PATH FS:MAKE-DIR
   s" a.txt" FLT-PATH s" first" FS:WRITE-ALL
   FLT-EACH
   FLT-SEEN @ 3 T=
   FLT-SEEN-BYTES @ 13 T=
   FLT-ROOT$ FLT-NAMES 64 FS-LIST:NAMES {: n:n :}
   n 15 T=
   FLT-NAMES 5 s" a.txt" T$=
   FLT-NAMES 5 + c@ 10 T=
   FLT-NAMES 6 + 5 s" b.txt" T$=
   FLT-NAMES 11 + c@ 10 T=
   FLT-NAMES 12 + 3 s" sub" T$= ;

: FLT-TYPE-KIND ( u8 u8 -- ) {: got:u8 want:u8 :}
   got FS-LIST:DT-UNKNOWN = got want = or TTRUE ;

: FLT-MANY-TYPE ( ptr u8 n u8 -- ) {: name:ptr size:n kind:u8 :}
   size 3 <> if E-FS-LIST-ENTRY throw then
   name c@ [char] m <> if E-FS-LIST-ENTRY throw then
   name 1 + c@ [char] A - {: hi:n :}
   name 2 + c@ [char] A - {: lo:n :}
   hi 0 >= hi 10 < and lo 0 >= lo 16 < and and
      0= if E-FS-LIST-ENTRY throw then
   hi 16 * lo + {: idx:n :}
   FLT-MANY-SEEN idx + c@ 0 T=
   1 FLT-MANY-SEEN idx + c!
   1 FLT-TYPED-MANY +!
   kind FS-LIST:DT-REG FLT-TYPE-KIND ;

: FLT-TYPE ( ptr u8 n u8 -- ) {: name:ptr size:n kind:u8 :}
   1 FLT-TYPED-SEEN +!
   name size s" a.txt" STR= if
      1 FLT-TYPED-A +!
      kind FS-LIST:DT-REG FLT-TYPE-KIND exit
   then
   name size s" b.txt" STR= if
      1 FLT-TYPED-B +!
      kind FS-LIST:DT-REG FLT-TYPE-KIND exit
   then
   name size s" sub" STR= if
      1 FLT-TYPED-DIR +!
      kind FS-LIST:DT-DIR FLT-TYPE-KIND exit
   then
   name size s" dangling" STR= if
      1 FLT-TYPED-LINK +!
      kind FS-LIST:DT-LNK FLT-TYPE-KIND exit
   then
   name size FLT-LONG-NAME 240 STR= if
      1 FLT-TYPED-LONG +!
      kind FS-LIST:DT-REG FLT-TYPE-KIND exit
   then
   name size kind FLT-MANY-TYPE ;

: FLT-TYPED-RESET ( -- )
   0 FLT-TYPED-SEEN ! 0 FLT-TYPED-A ! 0 FLT-TYPED-B !
   0 FLT-TYPED-DIR ! 0 FLT-TYPED-LINK !
   0 FLT-TYPED-MANY ! 0 FLT-TYPED-LONG !
   160 0 do 0 FLT-MANY-SEEN i + c! loop ;

: FLT-CHECK-TYPED-BASE ( -- )
   FLT-TYPED-A @ 1 T=
   FLT-TYPED-B @ 1 T=
   FLT-TYPED-DIR @ 1 T=
   FLT-TYPED-LINK @ 1 T= ;

: FLT-CHECK-TYPED-REFILL ( -- )
   FLT-TYPED-SEEN @ 165 T=
   FLT-CHECK-TYPED-BASE
   FLT-TYPED-MANY @ 160 T=
   FLT-TYPED-LONG @ 1 T= ;

: FLT-TYPED ( -- )
   FLT-TYPED-RESET
   FLT-ROOT$ [: FLT-TYPE ;] FS-LIST:EACH-TYPED ;

: FLT-TEST-TYPES ( -- )
   s" no-target" s" dangling" FLT-PATH FS:MAKE-SYMLINK
   FLT-TYPED
   FLT-TYPED-SEEN @ 4 T=
   FLT-CHECK-TYPED-BASE
   FLT-TYPED-MANY @ 0 T=
   FLT-TYPED-LONG @ 0 T=
   \ A separate fixture reaches the decoder's fallback even when this host's
   \ filesystem always supplies a useful d_type.
   0 FLT-DIRENT-TYPE-FIXTURE 18 + c!
   0 FLT-DIRENT-TYPE-FIXTURE 20 + c!
   FLT-DIRENT-TYPE-FIXTURE FS:DIRENT-TYPE FS-LIST:DT-UNKNOWN T=
   99 FLT-DIRENT-TYPE-FIXTURE 18 + c!
   99 FLT-DIRENT-TYPE-FIXTURE 20 + c!
   FLT-DIRENT-TYPE-FIXTURE FS:DIRENT-TYPE 99 T= ;

: FLT-MANY-PATH ( n -- ptr u8 n ) {: idx:n :}
   [char] m FLT-MANY-NAME c!
   idx 16 / 65 + FLT-MANY-NAME 1 + c!
   idx 16 mod 65 + FLT-MANY-NAME 2 + c!
   FLT-MANY-NAME 3 FLT-PATH ;

: FLT-TEST-REFILL ( -- )
   160 0 do i FLT-MANY-PATH s" x" FS:WRITE-ALL loop
   240 0 do [char] q FLT-LONG-NAME i + c! loop
   FLT-LONG-NAME 240 FLT-PATH s" x" FS:WRITE-ALL
   FLT-TYPED
   FLT-CHECK-TYPED-REFILL
   FLT-TYPED-RESET
   FLT-A @ FLT-ROOT$ [: FLT-TYPE ;] FS-LIST:EACH-TYPED-IN
   FLT-CHECK-TYPED-REFILL ;

: FLT-CHECK-LONG ( ptr u8 n -- )
   2dup FLT-COUNT
   dup 240 = if
      FLT-LONG-NAME 240 T$=
      1 FLT-LONG-SEEN +!
   else
      2drop
   then ;

: FLT-TEST-LONG-REFILL ( -- )
   0 FLT-LONG-SEEN ! 0 FLT-SEEN ! 0 FLT-SEEN-BYTES !
   FLT-A @ FLT-ROOT$ [: FLT-CHECK-LONG ;] FS-LIST:EACH-IN
   FLT-SEEN @ 165 T=
   FLT-LONG-SEEN @ 1 T= ;

: FLT-TOO-SMALL ( -- )
   FLT-ROOT$ FLT-NAMES 8 FS-LIST:NAMES drop ;

: FLT-MISSING ( -- )
   s" missing" FLT-PATH [: FLT-COUNT ;] FS-LIST:EACH ;

\ ---- a listing is the call's own -----------------------------------------------
\ A listing begun while another is open, in another task or inside the open
\ one's quotation, leaves the first one's descriptor and block alone. Each case
\ opens the second listing at the first one's first name, so the two overlap on
\ every run.

create FLT-INNER 64 allot                \ sub's names, listed while the root's listing is open
variable FLT-INNER-U
variable FLT-HANDED                      \ 1 once the lister is inside its listing
variable FLT-RESUMED                     \ 1 once this thread has listed sub
5000000000 constant FLT-WAIT-NS

TASK:MIN-STACK TASK:TASK FLT-LISTER

: FLT-SUB! ( -- )
   s" sub/one" FLT-PATH s" 1" FS:WRITE-ALL
   s" sub/two" FLT-PATH s" 2" FS:WRITE-ALL ;

: FLT-SUB-NAMES ( -- )
   s" sub" FLT-PATH FLT-INNER 64 FS-LIST:NAMES FLT-INNER-U ! ;

: FLT-COUNTS-RESET ( -- )
   0 FLT-SEEN !  0 FLT-SEEN-BYTES !  0 FLT-INNER-U ! ;

: FLT-BOTH-WHOLE ( -- )
   s" the open listing saw every name" T-LABEL
   FLT-SEEN @ 3 T=
   FLT-SEEN-BYTES @ 13 T=
   s" the listing inside it saw every name" T-LABEL
   FLT-INNER FLT-INNER-U @ s\" one\ntwo" T$= ;

: FLT-NEST ( ptr u8 n -- )
   FLT-COUNT
   FLT-SEEN @ 1 = if FLT-SUB-NAMES then ;

: FLT-NESTED ( -- )
   FLT-ROOT$ [: FLT-NEST ;] FS-LIST:EACH ;

: FLT-TEST-NESTED ( -- )
   FLT-COUNTS-RESET
   s" a listing inside another's quotation throws nothing" T-LABEL
   [: FLT-NESTED ;] 0 TTHROWSQ
   FLT-BOTH-WHOLE ;

\ A wait the other side ends; past FLT-WAIT-NS it throws E-PROC-TIMEOUT rather
\ than hang the suite. It prints nothing, because the lister waits here too and
\ a task asserts nothing: main names the deadline where it reads it.
: FLT-WAIT ( ptr n n -- ) {: flag want :}
   mono-ns FLT-WAIT-NS + {: until:n :}
   begin flag atomic@ want < while
      mono-ns until >= if E-PROC-TIMEOUT throw then
      1 >MS TASK:SLEEP
   repeat ;

\ A FLT-WAIT deadline main reads, its own or the lister's from the join, is the
\ row's own deadline: it is named and thrown, never asserted. Any other code is
\ answered.
: FLT-EXPIRED ( n -- n )
   {: code:n :}
   code E-PROC-TIMEOUT = if s" fs-list-test FLT-WAIT" TEST-WAIT:EXPIRED then
   code ;

: FLT-HANDOFF ( ptr u8 n -- )
   FLT-COUNT
   FLT-SEEN @ 1 = if
      1 FLT-HANDED atomic!
      FLT-RESUMED 1 FLT-WAIT
   then ;

\ Runs INSIDE the lister, so it asserts nothing: the join answers its throw.
: FLT-LISTER-BODY ( -- )
   FLT-ROOT$ [: FLT-HANDOFF ;] FS-LIST:EACH
   0 TASK:RETURN ;

: FLT-JOIN-LISTER ( -- )
   FLT-LISTER TASK:JOIN MATCH result
      ok OF 0 T= ENDOF
      err OF FLT-EXPIRED 0 T= ENDOF
   ;MATCH ;

: FLT-TEST-TWO-TASKS ( -- )
   FLT-COUNTS-RESET
   0 FLT-HANDED !  0 FLT-RESUMED !
   ['] FLT-LISTER-BODY FLT-LISTER TASK:ACTIVATE
   [: FLT-HANDED 1 FLT-WAIT ;] catch FLT-EXPIRED {: code:n :}
   code 0<> if code throw then
   FLT-SUB-NAMES
   1 FLT-RESUMED atomic!
   s" the lister's listing throws nothing" T-LABEL
   FLT-JOIN-LISTER
   FLT-BOTH-WHOLE ;

\ A quotation that throws ends its listing with the descriptor closed, so the
\ next open is handed the number the listing held.
: FLT-FD-NEXT ( -- n )
   FLT-ROOT$ FS:>PATHZ open-rd {: fd:n :}
   fd close
   fd ;

: FLT-THROW ( ptr u8 n -- )
   2drop E-A-BOUNDS throw ;              \ no listing path raises the quotation's code

: FLT-THROWING ( -- )
   FLT-ROOT$ [: FLT-THROW ;] FS-LIST:EACH ;

: FLT-TYPED-THROW ( ptr u8 n u8 -- )
   drop FLT-THROW ;

: FLT-TYPED-THROWING ( -- )
   FLT-ROOT$ [: FLT-TYPED-THROW ;] FS-LIST:EACH-TYPED ;

: FLT-TEST-THROW-CLOSES ( -- )
   FLT-FD-NEXT {: before:n :}
   [: FLT-THROWING ;] E-A-BOUNDS TTHROWSQ
   s" a listing its quotation threw out of closed its descriptor" T-LABEL
   FLT-FD-NEXT before T=
   [: FLT-TYPED-THROWING ;] E-A-BOUNDS TTHROWSQ
   FLT-FD-NEXT before T= ;

: FLT-CONTEXTS! ( -- )
   FLT-CTX-A FS-LIST:STORAGE-BYTES SPAN:MAKE FS-LIST:INIT FLT-A !
   FLT-CTX-B FS-LIST:STORAGE-BYTES SPAN:MAKE FS-LIST:INIT FLT-B ! ;

: FLT-A-COUNT ( -- )
   0 FLT-SEEN ! 0 FLT-SEEN-BYTES !
   FLT-A @ FLT-ROOT$ [: FLT-COUNT ;] FS-LIST:EACH-IN ;

: FLT-NAMES-IN-SMALL ( -- )
   FLT-A @ FLT-ROOT$ FLT-NAMES 8 FS-LIST:NAMES-IN drop ;

: FLT-READ-FAIL-IN ( -- )
   FLT-A @ s" a.txt" FLT-PATH [: FLT-COUNT ;] FS-LIST:EACH-IN ;

: FLT-TEST-CONTEXT-REUSE ( -- )
   FLT-A-COUNT
   FLT-SEEN @ 3 T=
   [: FLT-NAMES-IN-SMALL ;] E-FS-LIST-CAPACITY TTHROWSQ
   FLT-A @ FLT-ROOT$ FLT-NAMES 64 FS-LIST:NAMES-IN 15 T=
   [: FLT-READ-FAIL-IN ;] E-FS-LIST-READ TTHROWSQ
   FLT-A-COUNT
   FLT-SEEN @ 3 T=
   [: FLT-A @ s" missing" FLT-PATH [: FLT-COUNT ;] FS-LIST:EACH-IN ;]
      E-FS-LIST-OPEN TTHROWSQ
   FLT-A-COUNT
   FLT-SEEN @ 3 T=
   [: FLT-A @ FLT-ROOT$ [: FLT-THROW ;] FS-LIST:EACH-IN ;]
      E-A-BOUNDS TTHROWSQ
   FLT-A-COUNT
   FLT-SEEN @ 3 T= ;

: FLT-B-SUB-NAMES ( -- )
   FLT-B @ s" sub" FLT-PATH FLT-INNER 64 FS-LIST:NAMES-IN FLT-INNER-U ! ;

: FLT-A-NEST ( ptr u8 n -- )
   FLT-COUNT
   FLT-SEEN @ 1 = if FLT-B-SUB-NAMES then ;

: FLT-TEST-CONTEXT-NEST ( -- )
   FLT-COUNTS-RESET
   FLT-A @ FLT-ROOT$ [: FLT-A-NEST ;] FS-LIST:EACH-IN
   FLT-BOTH-WHOLE ;

: FLT-A-AGAIN ( ptr u8 n -- )
   FLT-COUNT
   FLT-SEEN @ 1 = if
      [: FLT-A @ FLT-ROOT$ [: 2drop ;] FS-LIST:EACH-IN ;]
         FS-LIST:E-ACTIVE TTHROWSQ
      FS:ERRNO 0 T=
   then ;

: FLT-TEST-CONTEXT-AGAIN ( -- )
   0 FLT-SEEN ! 0 FLT-SEEN-BYTES !
   FLT-A @ FLT-ROOT$ [: FLT-A-AGAIN ;] FS-LIST:EACH-IN
   FLT-SEEN @ 3 T=
   FLT-A-COUNT FLT-SEEN @ 3 T= ;

: FLT-BUSY ( -- )
   FLT-A @ FLT-ROOT$ [: 2drop ;] FS-LIST:EACH-IN ;

: FLT-CONTEXT-LISTER-BODY ( -- )
   FLT-A @ FLT-ROOT$ [: FLT-HANDOFF ;] FS-LIST:EACH-IN
   0 TASK:RETURN ;

: FLT-TEST-CONTEXT-TASKS ( -- )
   FLT-COUNTS-RESET
   0 FLT-HANDED ! 0 FLT-RESUMED !
   ['] FLT-CONTEXT-LISTER-BODY FLT-LISTER TASK:ACTIVATE
   FLT-HANDED 1 FLT-WAIT
   [: FLT-BUSY ;] FS-LIST:E-ACTIVE TTHROWSQ
   FS:ERRNO 0 T=
   FLT-B-SUB-NAMES
   1 FLT-RESUMED atomic!
   FLT-JOIN-LISTER
   FLT-BOTH-WHOLE
   FLT-A-COUNT FLT-SEEN @ 3 T= ;

: FLT-SMALL-CTX ( -- )
   FLT-CTX-A FS-LIST:STORAGE-BYTES 1- SPAN:MAKE FS-LIST:INIT drop ;

: FLT-UNALIGNED-CTX ( -- )
   FLT-CTX-BAD 1+ FS-LIST:STORAGE-BYTES SPAN:MAKE FS-LIST:INIT drop ;


: FS-LIST-TEST-MAIN ( -- )
   FLT-ROOT!
   FLT-CONTEXTS!
   FLT-TEST-EMPTY
   FLT-A-COUNT FLT-SEEN @ 0 T=
   FLT-TEST-ENTRIES
   [: FLT-SMALL-CTX ;] E-FS-LIST-CAPACITY TTHROWSQ
   [: FLT-UNALIGNED-CTX ;] E-FS-LIST-CAPACITY TTHROWSQ
   FLT-TEST-CONTEXT-REUSE
   FLT-TEST-CONTEXT-AGAIN
   [: FLT-TOO-SMALL ;] E-FS-LIST-CAPACITY TTHROWSQ
   [: FLT-MISSING ;] E-FS-LIST-OPEN TTHROWSQ
   FLT-ROOT$ FLT-NAMES 64 FS-LIST:NAMES 15 T=      \ a refused listing leaves the next one intact
   FLT-SUB!
   FLT-TEST-CONTEXT-NEST
   FLT-TEST-CONTEXT-TASKS
   FLT-TEST-NESTED
   FLT-TEST-TWO-TASKS
   FLT-TEST-THROW-CLOSES
   FLT-TEST-TYPES
   FLT-TEST-REFILL
   FLT-TEST-LONG-REFILL
   FLT-ROOT$ FS:REMOVE-TREE
   T-REPORT
   s" fs-list-test: ok" type cr ;

FS-LIST-TEST-MAIN
