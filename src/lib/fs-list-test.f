\ fs-list-test.f - focused tests for src/lib/fs-list.f.
\ Run: bin/hb --load src/lib/error.f src/lib/string.f src/lib/test.f src/lib/fs.f src/lib/fs-mutate.f src/lib/task.f src/lib/fs-list.f src/lib/fs-list-test.f

require src/lib/error.f
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
variable FLT-AT-DIR                  \ the descriptor the AT cases list
variable FLT-OUTER-SEEN
variable FLT-INNER-SEEN
create FLT-MOVED-BUF PATH-CAP allot
create FLT-DIRENT-TYPE-FIXTURE 24 allot

: FLT-ROOT$ ( -- str )
   FLT-ROOT-BUF FLT-ROOT-U @ >str ;

: FLT-ROOT! ( -- )
   s" habu-fs-list" FS:MKTEMP-DIR {: made:str :}
   made STR:DATA FLT-ROOT-BUF made STR:LEN BYTE-COPY
   made STR:LEN FLT-ROOT-U ! ;

: FLT-PATH ( str -- str )
   FLT-ROOT$ swap FLT-PATH-BUF FS:JOIN-PATH FLT-PATH-BUF swap >str ;

: FLT-COUNT ( str -- )
   STR:LEN FLT-SEEN-BYTES +! 1 FLT-SEEN +! ;

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
   FLT-NAMES 5 >str s" a.txt" T$=
   FLT-NAMES 5 + c@ 10 T=
   FLT-NAMES 6 + 5 >str s" b.txt" T$=
   FLT-NAMES 11 + c@ 10 T=
   FLT-NAMES 12 + 3 >str s" sub" T$= ;

: FLT-TYPE-KIND ( u8 u8 -- ) {: got:u8 want:u8 :}
   got FS-LIST:DT-UNKNOWN = got want = or TTRUE ;

: FLT-MANY-TYPE ( str u8 -- ) {: name:str kind:u8 :}
   name STR:LEN 3 <> if E-FS-LIST-ENTRY throw then
   name STR:DATA c@ [char] m <> if E-FS-LIST-ENTRY throw then
   name STR:DATA 1 + c@ [char] A - {: hi:n :}
   name STR:DATA 2 + c@ [char] A - {: lo:n :}
   hi 0 >= hi 10 < and lo 0 >= lo 16 < and and
      0= if E-FS-LIST-ENTRY throw then
   hi 16 * lo + {: idx:n :}
   FLT-MANY-SEEN idx + c@ 0 T=
   1 FLT-MANY-SEEN idx + c!
   1 FLT-TYPED-MANY +!
   kind FS-LIST:DT-REG FLT-TYPE-KIND ;

: FLT-TYPE ( str u8 -- ) {: name:str kind:u8 :}
   1 FLT-TYPED-SEEN +!
   name s" a.txt" STR:= if
      1 FLT-TYPED-A +!
      kind FS-LIST:DT-REG FLT-TYPE-KIND exit
   then
   name s" b.txt" STR:= if
      1 FLT-TYPED-B +!
      kind FS-LIST:DT-REG FLT-TYPE-KIND exit
   then
   name s" sub" STR:= if
      1 FLT-TYPED-DIR +!
      kind FS-LIST:DT-DIR FLT-TYPE-KIND exit
   then
   name s" dangling" STR:= if
      1 FLT-TYPED-LINK +!
      kind FS-LIST:DT-LNK FLT-TYPE-KIND exit
   then
   name FLT-LONG-NAME 240 >str STR:= if
      1 FLT-TYPED-LONG +!
      kind FS-LIST:DT-REG FLT-TYPE-KIND exit
   then
   name kind FLT-MANY-TYPE ;

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

: FLT-MANY-PATH ( n -- str ) {: idx:n :}
   [char] m FLT-MANY-NAME c!
   idx 16 / 65 + FLT-MANY-NAME 1 + c!
   idx 16 mod 65 + FLT-MANY-NAME 2 + c!
   FLT-MANY-NAME 3 >str FLT-PATH ;

: FLT-TEST-REFILL ( -- )
   160 0 do i FLT-MANY-PATH s" x" FS:WRITE-ALL loop
   240 0 do [char] q FLT-LONG-NAME i + c! loop
   FLT-LONG-NAME 240 >str FLT-PATH s" x" FS:WRITE-ALL
   FLT-TYPED
   FLT-CHECK-TYPED-REFILL ;

\ The refill fixture through a descriptor the test opened.
: FLT-TYPED-AT ( fd -- ) {: dir:fd :}
   FLT-TYPED-RESET
   dir [: FLT-TYPE ;] FS-LIST:EACH-TYPED-AT
   FLT-CHECK-TYPED-REFILL ;

: FLT-INNER-COUNT ( str u8 -- )
   drop drop 1 FLT-INNER-SEEN +! ;

\ The outer listing's first entry lists the whole directory again through
\ the same descriptor, before the outer one reads its second block.
: FLT-OUTER ( str u8 -- )
   drop drop 1 FLT-OUTER-SEEN +!
   FLT-OUTER-SEEN @ 1 <> if exit then
   FLT-AT-DIR @ >FD [: FLT-INNER-COUNT ;] FS-LIST:EACH-TYPED-AT ;

: FLT-NESTED-AT ( fd -- ) {: dir:fd :}
   dir FD>N FLT-AT-DIR !
   0 FLT-OUTER-SEEN ! 0 FLT-INNER-SEEN !
   dir [: FLT-OUTER ;] FS-LIST:EACH-TYPED-AT
   FLT-INNER-SEEN @ 165 T=
   FLT-OUTER-SEEN @ 165 T= ;

: FLT-MOVED$ ( -- str )
   FLT-ROOT-U @ {: u:n :}
   FLT-ROOT-BUF FLT-MOVED-BUF u BYTE-COPY
   s" -moved" {: tail:str :}
   tail STR:DATA FLT-MOVED-BUF u + tail STR:LEN BYTE-COPY
   FLT-MOVED-BUF u tail STR:LEN + >str ;

\ The root, renamed after the test opened it, lists whole through the
\ descriptor; its old path is gone until it is renamed back.
: FLT-MOVED-AT ( fd -- ) {: dir:fd :}
   FLT-ROOT$ FLT-MOVED$ FS:RENAME-FILE
   FLT-ROOT$ FS:TRY-STAT TFALSE
   dir FLT-TYPED-AT
   FLT-MOVED$ FLT-ROOT$ FS:RENAME-FILE ;

: FLT-AT-CLOSED ( -- )
   FLT-AT-DIR @ >FD [: FLT-INNER-COUNT ;] FS-LIST:EACH-TYPED-AT ;

\ Two listings, a nested one and a moved one leave the descriptor the test's;
\ once closed, it is E-FS-LIST-OPEN with EBADF.
: FLT-TEST-AT ( -- )
   FLT-ROOT$ FS:OPEN-DIR-READ >FD {: dir:fd :}
   dir FLT-TYPED-AT
   dir FLT-TYPED-AT
   dir FLT-NESTED-AT
   dir FLT-MOVED-AT
   dir FD>N FS:CLOSE-FD 0 T=
   [: FLT-AT-CLOSED ;] E-FS-LIST-OPEN TTHROWSQ
   FS:ERRNO FS:EBADF T= ;

: FLT-CHECK-LONG ( str -- )
   dup FLT-COUNT
   dup STR:LEN 240 = if
      FLT-LONG-NAME 240 >str T$=
      1 FLT-LONG-SEEN +!
   else
      drop
   then ;

: FLT-TEST-LONG-REFILL ( -- )
   0 FLT-LONG-SEEN ! 0 FLT-SEEN ! 0 FLT-SEEN-BYTES !
   FLT-ROOT$ [: FLT-CHECK-LONG ;] FS-LIST:EACH
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
   FLT-INNER FLT-INNER-U @ >str s\" one\ntwo" T$= ;

: FLT-NEST ( str -- )
   FLT-COUNT
   FLT-SEEN @ 1 = if FLT-SUB-NAMES then ;

: FLT-NESTED ( -- )
   FLT-ROOT$ [: FLT-NEST ;] FS-LIST:EACH ;

: FLT-TEST-NESTED ( -- )
   FLT-COUNTS-RESET
   s" a listing inside another's quotation throws nothing" T-LABEL
   [: FLT-NESTED ;] 0 TTHROWSQ
   FLT-BOTH-WHOLE ;

\ A main thread that ends before it resumes the lister ends the lister's wait.
: FLT-HANDOFF ( str -- )
   FLT-COUNT
   FLT-SEEN @ 1 = if
      1 FLT-HANDED atomic!
      begin FLT-RESUMED atomic@ 1 < while TASK:PAUSE repeat
   then ;

\ Runs INSIDE the lister, so it asserts nothing: the join answers its throw.
: FLT-LISTER-BODY ( -- )
   FLT-ROOT$ [: FLT-HANDOFF ;] FS-LIST:EACH
   0 TASK:RETURN ;

: FLT-JOIN-LISTER ( -- )
   FLT-LISTER TASK:JOIN MATCH result
      ok OF 0 T= ENDOF
      err OF 0 T= ENDOF
   ;MATCH ;

: FLT-TEST-TWO-TASKS ( -- )
   FLT-COUNTS-RESET
   0 FLT-HANDED !  0 FLT-RESUMED !
   ['] FLT-LISTER-BODY FLT-LISTER TASK:ACTIVATE
   FLT-HANDED 1 FLT-LISTER TEST-WAIT:FOR-CELL
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

: FLT-THROW ( str -- )
   drop E-A-BOUNDS throw ;              \ no listing path raises the quotation's code

: FLT-THROWING ( -- )
   FLT-ROOT$ [: FLT-THROW ;] FS-LIST:EACH ;

: FLT-TYPED-THROW ( str u8 -- )
   drop FLT-THROW ;

: FLT-TYPED-THROWING ( -- )
   FLT-ROOT$ [: FLT-TYPED-THROW ;] FS-LIST:EACH-TYPED ;

: FLT-AT-THROWING ( -- )
   FLT-AT-DIR @ >FD [: FLT-TYPED-THROW ;] FS-LIST:EACH-TYPED-AT ;

: FLT-TEST-THROW-CLOSES ( -- )
   FLT-FD-NEXT {: before:n :}
   [: FLT-THROWING ;] E-A-BOUNDS TTHROWSQ
   s" a listing its quotation threw out of closed its descriptor" T-LABEL
   FLT-FD-NEXT before T=
   [: FLT-TYPED-THROWING ;] E-A-BOUNDS TTHROWSQ
   FLT-FD-NEXT before T=
   FLT-ROOT$ FS:OPEN-DIR-READ FLT-AT-DIR !
   FLT-FD-NEXT {: at-next:n :}
   [: FLT-AT-THROWING ;] E-A-BOUNDS TTHROWSQ
   FLT-FD-NEXT at-next T=
   FLT-AT-DIR @ FS:CLOSE-FD 0 T= ;

: FS-LIST-TEST-MAIN ( -- )
   FLT-ROOT!
   FLT-TEST-EMPTY
   FLT-TEST-ENTRIES
   [: FLT-TOO-SMALL ;] E-FS-LIST-CAPACITY TTHROWSQ
   [: FLT-MISSING ;] E-FS-LIST-OPEN TTHROWSQ
   FLT-ROOT$ FLT-NAMES 64 FS-LIST:NAMES 15 T=      \ a refused listing leaves the next one intact
   FLT-SUB!
   FLT-TEST-NESTED
   FLT-TEST-TWO-TASKS
   FLT-TEST-THROW-CLOSES
   FLT-TEST-TYPES
   FLT-TEST-REFILL
   FLT-TEST-AT
   FLT-TEST-LONG-REFILL
   FLT-ROOT$ FS:REMOVE-TREE
   T-REPORT
   s" fs-list-test: ok" type cr ;

FS-LIST-TEST-MAIN
