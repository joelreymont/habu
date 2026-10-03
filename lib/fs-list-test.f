\ fs-list-test.f - focused tests for lib/fs-list.f.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f lib/fs-mutate.f lib/task.f lib/fs-list.f lib/fs-list-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/fs.f
require lib/fs-mutate.f
require lib/task.f
require lib/fs-list.f

create FLT-ROOT-BUF FS-PATH-CAP allot
create FLT-PATH-BUF FS-PATH-CAP allot
create FLT-NAMES 64 allot
variable FLT-ROOT-U
variable FLT-SEEN
variable FLT-SEEN-BYTES

: FLT-ROOT$ ( -- ptr u8 n )
   FLT-ROOT-BUF FLT-ROOT-U @ ;

: FLT-ROOT! ( -- )
   s" habu-fs-list" HB-TMP-MKDIR {: a:ptr u:n :}
   a FLT-ROOT-BUF u BYTE-COPY
   u FLT-ROOT-U ! ;

: FLT-PATH ( ptr u8 n -- ptr u8 n )
   FLT-ROOT$ 2swap FLT-PATH-BUF JOIN-PATH FLT-PATH-BUF swap ;

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
   s" b.txt" FLT-PATH s" second" WRITE-ALL
   s" sub" FLT-PATH MAKE-DIR
   s" a.txt" FLT-PATH s" first" WRITE-ALL
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

: FLT-TOO-SMALL ( -- )
   FLT-ROOT$ FLT-NAMES 8 FS-LIST:NAMES drop ;

: FLT-MISSING ( -- )
   s" missing" FLT-PATH [: FLT-COUNT ;] FS-LIST:EACH ;

\ ---- a listing is the call's own -----------------------------------------------
\ The descriptor, the cursor and the dirent block were one set for the image:
\ a listing begun while another was open - in another task, or in the open
\ one's own quotation - closed the first one's descriptor and wrote over its
\ block, and the first then threw E-FS-LIST-READ. Each case opens the second
\ listing at the first one's first name, so the two overlap on every run.

create FLT-INNER 64 allot                \ sub's names, listed while the root's listing is open
variable FLT-INNER-U
variable FLT-HANDED                      \ 1 once the lister is inside its listing
variable FLT-RESUMED                     \ 1 once this thread has listed sub
5000000000 constant FLT-WAIT-NS          \ one handshake wait: 5 s

TASK:MIN-STACK TASK:TASK FLT-LISTER

: FLT-SUB! ( -- )
   s" sub/one" FLT-PATH s" 1" WRITE-ALL
   s" sub/two" FLT-PATH s" 2" WRITE-ALL ;

: FLT-SUB-NAMES ( -- )
   s" sub" FLT-PATH FLT-INNER 64 FS-LIST:NAMES FLT-INNER-U ! ;

: FLT-COUNTS-RESET ( -- )
   0 FLT-SEEN !  0 FLT-SEEN-BYTES !  0 FLT-INNER-U ! ;

\ Both listings came back whole: the root's three names in 13 bytes, and sub's two.
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
\ than hang the suite.
: FLT-WAIT ( ptr n n -- ) {: flag want :}
   mono-ns FLT-WAIT-NS + {: until:n :}
   begin flag atomic@ want < while
      mono-ns until >= if E-PROC-TIMEOUT throw then
      1 >MS TASK:SLEEP
   repeat ;

\ The lister's quotation: at its first name it hands over to this thread and
\ waits until sub is listed.
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
      err OF 0 T= ENDOF
   ;MATCH ;

: FLT-TEST-TWO-TASKS ( -- )
   FLT-COUNTS-RESET
   0 FLT-HANDED !  0 FLT-RESUMED !
   ['] FLT-LISTER-BODY FLT-LISTER TASK:ACTIVATE
   FLT-HANDED 1 FLT-WAIT
   FLT-SUB-NAMES
   1 FLT-RESUMED atomic!
   s" the lister's listing throws nothing" T-LABEL
   FLT-JOIN-LISTER
   FLT-BOTH-WHOLE ;

\ A quotation that throws ends its listing with the descriptor closed, so the
\ next open is handed the number the listing held.
: FLT-FD-NEXT ( -- n )
   FLT-ROOT$ FS-PATHZ open-rd {: fd:n :}
   fd close
   fd ;

: FLT-THROW ( ptr u8 n -- )
   2drop E-A-BOUNDS throw ;              \ no listing path raises the quotation's code

: FLT-THROWING ( -- )
   FLT-ROOT$ [: FLT-THROW ;] FS-LIST:EACH ;

: FLT-TEST-THROW-CLOSES ( -- )
   FLT-FD-NEXT {: before:n :}
   [: FLT-THROWING ;] E-A-BOUNDS TTHROWSQ
   s" a listing its quotation threw out of closed its descriptor" T-LABEL
   FLT-FD-NEXT before T= ;

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
   FLT-ROOT$ REMOVE-TREE
   T-REPORT
   s" fs-list-test: ok" type cr ;

FS-LIST-TEST-MAIN
