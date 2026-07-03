\ filemap-lint-test.f - fixture tests for the derived filemap-lint policy.
\ Run: bin/hb --load tools/filemap-lint-test.f
\ Requiring tools/filemap-lint.f also runs the real repo lint (must be green).

require lib/errors.f
require lib/string.f
require lib/fs.f
require lib/fs-mutate.f
require lib/test.f
require tools/filemap-lint.f

create FLT-ROOT FS-PATH-CAP allot
create FLT-SRC FS-PATH-CAP allot
create FLT-LISTED FS-PATH-CAP allot
create FLT-UNLISTED FS-PATH-CAP allot
create FLT-EXCLUDED FS-PATH-CAP allot
create FLT-NOTES FS-PATH-CAP allot
create FLT-GHOST FS-PATH-CAP allot
create FLT-MAP1 FS-PATH-CAP allot
create FLT-MAP2 FS-PATH-CAP allot
create FLT-MAP3 FS-PATH-CAP allot

variable FLT-ROOT-U
variable FLT-SRC-U
variable FLT-LISTED-U
variable FLT-UNLISTED-U
variable FLT-EXCLUDED-U
variable FLT-NOTES-U
variable FLT-GHOST-U
variable FLT-MAP1-U
variable FLT-MAP2-U
variable FLT-MAP3-U

: FLT-ROOT$ ( -- ptr u8 n ) FLT-ROOT FLT-ROOT-U @ ;
: FLT-SRC$ ( -- ptr u8 n ) FLT-SRC FLT-SRC-U @ ;
: FLT-LISTED$ ( -- ptr u8 n ) FLT-LISTED FLT-LISTED-U @ ;
: FLT-UNLISTED$ ( -- ptr u8 n ) FLT-UNLISTED FLT-UNLISTED-U @ ;
: FLT-EXCLUDED$ ( -- ptr u8 n ) FLT-EXCLUDED FLT-EXCLUDED-U @ ;
: FLT-NOTES$ ( -- ptr u8 n ) FLT-NOTES FLT-NOTES-U @ ;
: FLT-GHOST$ ( -- ptr u8 n ) FLT-GHOST FLT-GHOST-U @ ;
: FLT-MAP1$ ( -- ptr u8 n ) FLT-MAP1 FLT-MAP1-U @ ;
: FLT-MAP2$ ( -- ptr u8 n ) FLT-MAP2 FLT-MAP2-U @ ;
: FLT-MAP3$ ( -- ptr u8 n ) FLT-MAP3 FLT-MAP3-U @ ;

: FLT-ROOT! ( -- )
   s" habu-filemap-lint" TMPDIR-MKDIR
   {: a:ptr u:n :}
   a FLT-ROOT u BYTE-COPY
   u FLT-ROOT-U ! ;

: FLT-SRC! ( -- )
   FLT-ROOT$ s" fsrc" FLT-SRC JOIN-PATH FLT-SRC-U !
   FLT-SRC$ MAKE-DIRS ;

: FLT-PATHS! ( -- )
   FLT-SRC$ s" listed.f" FLT-LISTED JOIN-PATH FLT-LISTED-U !
   FLT-SRC$ s" unlisted.f" FLT-UNLISTED JOIN-PATH FLT-UNLISTED-U !
   FLT-SRC$ s" excluded.f" FLT-EXCLUDED JOIN-PATH FLT-EXCLUDED-U !
   FLT-SRC$ s" notes.txt" FLT-NOTES JOIN-PATH FLT-NOTES-U !
   FLT-SRC$ s" ghost.f" FLT-GHOST JOIN-PATH FLT-GHOST-U !
   FLT-ROOT$ s" map1.md" FLT-MAP1 JOIN-PATH FLT-MAP1-U !
   FLT-ROOT$ s" map2.md" FLT-MAP2 JOIN-PATH FLT-MAP2-U !
   FLT-ROOT$ s" map3.md" FLT-MAP3 JOIN-PATH FLT-MAP3-U ! ;

: FLT-ROW+ ( ptr u8 n -- )
   s" - " SB-APPEND
   96 SB-APPEND-C
   SB-APPEND
   96 SB-APPEND-C
   s"  fixture row" SB-APPEND
   10 SB-APPEND-C ;

: FLT-MAP1-SRC$ ( -- ptr u8 n )
   SB-RESET
   FLT-LISTED$ FLT-ROW+
   FLT-UNLISTED$ FLT-ROW+
   SB$ ;

: FLT-MAP2-SRC$ ( -- ptr u8 n )
   SB-RESET
   FLT-LISTED$ FLT-ROW+
   SB$ ;

: FLT-MAP3-SRC$ ( -- ptr u8 n )
   SB-RESET
   FLT-LISTED$ FLT-ROW+
   FLT-UNLISTED$ FLT-ROW+
   FLT-GHOST$ FLT-ROW+
   SB$ ;

: FLT-FILES! ( -- )
   FLT-LISTED$ s" \ fixture" WRITE-ALL
   FLT-UNLISTED$ s" \ fixture" WRITE-ALL
   FLT-EXCLUDED$ s" \ fixture" WRITE-ALL
   FLT-NOTES$ s" fixture notes" WRITE-ALL
   FLT-MAP1$ FLT-MAP1-SRC$ WRITE-ALL
   FLT-MAP2$ FLT-MAP2-SRC$ WRITE-ALL
   FLT-MAP3$ FLT-MAP3-SRC$ WRITE-ALL ;

: FLT-PREPARE ( -- )
   FLT-ROOT!
   FLT-SRC!
   FLT-PATHS!
   FLT-FILES! ;

: FLT-RUN ( ptr u8 n -- ) {: mapa:ptr mapu:n :}
   mapa mapu FM-FILEMAP!
   FLT-SRC$ FM-ROOT+
   FM-SCAN-PATHS
   FM-CHECK-PATHS
   FM-CHECK-EXCLUSIONS
   FM-CHECK-TREE ;

: FLT-TEST-CLEAN ( -- )
   FM-LINT-RESET
   FLT-EXCLUDED$ FM-EXC
   FLT-MAP1$ FLT-RUN
   FM-BAD @ 0 T= ;

: FLT-TEST-UNLISTED ( -- )
   FM-LINT-RESET
   FLT-EXCLUDED$ FM-EXC
   FLT-MAP2$ FLT-RUN
   FM-BAD @ 1 T= ;

: FLT-TEST-STALE-EXCLUSION ( -- )
   FM-LINT-RESET
   FLT-EXCLUDED$ FM-EXC
   FLT-GHOST$ FM-EXC
   FLT-MAP2$ FLT-RUN
   FM-BAD @ 2 T= ;

: FLT-TEST-STALE-PATH ( -- )
   FM-LINT-RESET
   FLT-EXCLUDED$ FM-EXC
   FLT-MAP3$ FLT-RUN
   FM-BAD @ 1 T= ;

: FLT-TEST-MISSING-DOC ( -- )
   FM-LINT-RESET
   FLT-EXCLUDED$ FM-EXC
   FLT-MAP1$ FLT-RUN
   s" missing-doc.md" FM-REQ
   FM-BAD @ 1 T= ;

: FLT-TEST-UNEXCLUDED-WITHOUT-ROW ( -- )
   FM-LINT-RESET
   FLT-MAP2$ FLT-RUN
   FM-BAD @ 2 T= ;

: FLT-CLEANUP ( -- )
   FLT-ROOT$ REMOVE-TREE ;

: FLT-MAIN ( -- )
   T-RESET
   FLT-PREPARE
   FLT-TEST-CLEAN
   FLT-TEST-UNLISTED
   FLT-TEST-STALE-EXCLUSION
   FLT-TEST-STALE-PATH
   FLT-TEST-MISSING-DOC
   FLT-TEST-UNEXCLUDED-WITHOUT-ROW
   FLT-CLEANUP
   FLT-ROOT$ EXISTS? TFALSE
   T-REPORT ;

FLT-MAIN
