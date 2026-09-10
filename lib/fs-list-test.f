\ fs-list-test.f - focused tests for lib/fs-list.f.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f lib/fs-mutate.f lib/fs-list.f lib/fs-list-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/fs.f
require lib/fs-mutate.f
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
   s" habu-fs-list" TMPDIR-MKDIR {: a:ptr u:n :}
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

: FS-LIST-TEST-MAIN ( -- )
   FLT-ROOT!
   FLT-TEST-EMPTY
   FLT-TEST-ENTRIES
   [: FLT-TOO-SMALL ;] E-FS-LIST-CAPACITY TTHROWSQ
   [: FLT-MISSING ;] E-FS-LIST-OPEN TTHROWSQ
   FLT-ROOT$ FLT-NAMES 64 FS-LIST:NAMES 15 T=      \ a refused listing leaves the next one intact
   FLT-ROOT$ REMOVE-TREE
   T-REPORT
   s" fs-list-test: ok" type cr ;

FS-LIST-TEST-MAIN
