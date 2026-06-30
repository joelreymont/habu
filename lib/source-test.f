\ source-test.f - focused tests for checked source materialization helpers.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f lib/fs-mutate.f lib/source.f lib/source-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/source.f

2048 constant ST-CAP
16 constant ST-LINE-CAP
4 constant ST-SMALL-LINE-CAP
92 constant ST-BACKSLASH

variable ST-ROOT-U
variable ST-A-U
variable ST-B-U
variable ST-OUT-U
variable ST-SRC-U
variable ST-INS-U
variable ST-WANT-U
variable ST-LINE-COUNT
variable ST-APPEND-LEN

create ST-BUF ST-CAP allot
create ST-SRC-BUF ST-CAP allot
create ST-INS-BUF ST-CAP allot
create ST-WANT-BUF ST-CAP allot
create ST-LINE-BUF ST-LINE-CAP allot
create ST-SMALL-LINE-BUF ST-SMALL-LINE-CAP allot
create ST-ROOT-BUF FS-PATH-CAP allot
create ST-A-BUF FS-PATH-CAP allot
create ST-B-BUF FS-PATH-CAP allot
create ST-OUT-PATH-BUF FS-PATH-CAP allot
create ST-PATHS 2 cells allot
create ST-LENS 2 cells allot

: ST-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: src:ptr u dst:ptr lenp:ptr :}
   u FS-PATH-CAP > if E-FS-CAPACITY throw then
   src dst u BYTE-COPY
   u lenp ! ;

: ST-PATH! ( ptr u8 n ptr u8 n ptr u8 ptr n -- ) {: base:ptr baseu name:ptr nameu dst:ptr lenp:ptr :}
   base baseu name nameu dst JOIN-PATH lenp ! ;

: ST-BYTES! ( ptr u8 n ptr u8 ptr n -- ) {: src:ptr u dst:ptr lenp:ptr :}
   u ST-CAP > if E-FS-CAPACITY throw then
   src dst u BYTE-COPY
   u lenp ! ;

: ST-ROOT ( -- ptr u8 n )
   ST-ROOT-BUF ST-ROOT-U @ ;

: ST-A ( -- ptr u8 n )
   ST-A-BUF ST-A-U @ ;

: ST-B ( -- ptr u8 n )
   ST-B-BUF ST-B-U @ ;

: ST-OUT-PATH ( -- ptr u8 n )
   ST-OUT-PATH-BUF ST-OUT-U @ ;

: ST-SRC ( -- ptr u8 n )
   ST-SRC-BUF ST-SRC-U @ ;

: ST-INS ( -- ptr u8 n )
   ST-INS-BUF ST-INS-U @ ;

: ST-WANT ( -- ptr u8 n )
   ST-WANT-BUF ST-WANT-U @ ;

: ST-PATH-SLOT! ( ptr u8 n n -- ) {: a:ptr u idx :}
   a ST-PATHS idx cells + !
   u ST-LENS idx cells + ! ;

: ST-READ-STDIN-ALL ( ptr u8 n -- n )
   >LEN READ-STDIN-ALL LEN>N ;

: ST-CONCAT-FILES ( ptr a ptr a n ptr u8 n -- n )
   {: paths:ptr lens:ptr count dst:ptr cap :}
   paths lens count >COUNT dst cap >LEN CONCAT-FILES LEN>N ;

: ST-WRITE-SOURCE-LIST ( ptr a ptr a n ptr u8 n -- )
   {: paths:ptr lens:ptr count out:ptr outu :}
   paths lens count >COUNT out outu >LEN WRITE-SOURCE-LIST ;

: ST-INSERT-BEFORE-FINAL-LINE ( ptr u8 n ptr u8 n ptr u8 n -- n )
   {: src:ptr srcu ins:ptr insu dst:ptr cap :}
   src srcu >LEN ins insu >LEN dst cap >LEN INSERT-BEFORE-FINAL-LINE LEN>N ;

: ST-COMMENT-EXPORTS ( ptr u8 n ptr u8 n -- n )
   {: src:ptr srcu dst:ptr cap :}
   src srcu >LEN dst cap >LEN COMMENT-EXPORTS LEN>N ;

: ST-FILE-LINES ( ptr u8 n [ ptr u8 n n -- ] -- )
   {: path:ptr pathu q :}
   path pathu ST-LINE-BUF ST-LINE-CAP q SOURCE-FILE-LINES ;

: ST-SMALL-FILE-LINES ( ptr u8 n [ ptr u8 n n -- ] -- )
   {: path:ptr pathu q :}
   path pathu ST-SMALL-LINE-BUF ST-SMALL-LINE-CAP q SOURCE-FILE-LINES ;

: ST-LF ( -- )
   10 SB-APPEND-C ;

: ST-CR ( -- )
   13 SB-APPEND-C ;

: ST-PROVIDED ( ptr u8 n -- ) {: path:ptr pathu:n :}
   s" s" SB-APPEND
   34 SB-APPEND-C
   32 SB-APPEND-C
   path pathu SB-APPEND
   34 SB-APPEND-C
   32 SB-APPEND-C
   s" provided" SB-APPEND
   ST-LF ;

: ST-SOURCE-LIST-WANT$ ( -- ptr u8 n )
   SB-RESET
   ST-A ST-PROVIDED
   s" alpha" SB-APPEND
   ST-LF
   ST-B ST-PROVIDED
   s" beta" SB-APPEND
   ST-LF
   SB$ ;

: ST-SB>SRC ( -- )
   SB$ ST-SRC-BUF ST-SRC-U ST-BYTES! ;

: ST-SB>INS ( -- )
   SB$ ST-INS-BUF ST-INS-U ST-BYTES! ;

: ST-SB>WANT ( -- )
   SB$ ST-WANT-BUF ST-WANT-U ST-BYTES! ;

: ST-INSERT-CASE! ( -- )
   SB-RESET s" one" SB-APPEND ST-LF s" two" SB-APPEND ST-LF ST-SB>SRC
   SB-RESET s" X" SB-APPEND ST-LF ST-SB>INS
   SB-RESET s" one" SB-APPEND ST-LF s" X" SB-APPEND ST-LF s" two" SB-APPEND ST-LF ST-SB>WANT ;

: ST-FINAL-CASE! ( -- )
   SB-RESET s" final" SB-APPEND ST-SB>SRC
   SB-RESET s" pre" SB-APPEND ST-LF ST-SB>INS
   SB-RESET s" pre" SB-APPEND ST-LF s" final" SB-APPEND ST-SB>WANT ;

: ST-EXPORT-CASE! ( -- )
   SB-RESET
   s" : A ;" SB-APPEND ST-LF
   STR-SPACE SB-APPEND-C STR-SPACE SB-APPEND-C s" EXPORT A" SB-APPEND ST-LF
   s" EXPORT B" SB-APPEND ST-LF
   ST-SB>SRC
   SB-RESET
   s" : A ;" SB-APPEND ST-LF
   ST-BACKSLASH SB-APPEND-C STR-SPACE SB-APPEND-C s" EXPORT A" SB-APPEND ST-LF
   ST-BACKSLASH SB-APPEND-C STR-SPACE SB-APPEND-C s" EXPORT B" SB-APPEND ST-LF
   ST-SB>WANT ;

: ST-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-source" TMPDIR-MKDIR {: a:ptr u :}
   a u ST-ROOT-BUF ST-ROOT-U ST-COPY!
   ST-ROOT CLEANUP-TREE+
   ST-ROOT s" a.f" ST-A-BUF ST-A-U ST-PATH!
   ST-ROOT s" b.f" ST-B-BUF ST-B-U ST-PATH!
   ST-ROOT s" out.f" ST-OUT-PATH-BUF ST-OUT-U ST-PATH!
   ST-A s" alpha" WRITE-ALL
   ST-B s" beta" WRITE-ALL
   ST-A 0 ST-PATH-SLOT!
   ST-B 1 ST-PATH-SLOT! ;

: TEST-READ-STDIN-ALL ( -- )
   ST-BUF ST-CAP ST-READ-STDIN-ALL 0 T= ;

: TEST-READ-STDIN-DATA ( -- )
   ST-BUF ST-CAP ST-READ-STDIN-ALL 4 T=
   ST-BUF 4 s" DATA" T$= ;

: TEST-CONCAT-FILES ( -- )
   ST-PATHS ST-LENS 2 ST-BUF ST-CAP ST-CONCAT-FILES 9 T=
   ST-BUF 9 s" alphabeta" T$= ;

: TEST-WRITE-SOURCE-LIST ( -- )
   ST-PATHS ST-LENS 2 ST-OUT-PATH ST-WRITE-SOURCE-LIST
   ST-OUT-PATH ST-BUF ST-CAP READ-ALL {: u:n :}
   ST-BUF u ST-SOURCE-LIST-WANT$ T$= ;

: TEST-INSERT-BEFORE-FINAL-LINE ( -- )
   ST-INSERT-CASE!
   ST-SRC ST-INS ST-BUF ST-CAP ST-INSERT-BEFORE-FINAL-LINE ST-WANT-U @ T=
   ST-BUF ST-WANT-U @ ST-WANT T$=
   ST-FINAL-CASE!
   ST-SRC ST-INS ST-BUF ST-CAP ST-INSERT-BEFORE-FINAL-LINE ST-WANT-U @ T=
   ST-BUF ST-WANT-U @ ST-WANT T$= ;

: TEST-COMMENT-EXPORTS ( -- )
   ST-EXPORT-CASE!
   ST-SRC ST-BUF ST-CAP ST-COMMENT-EXPORTS ST-WANT-U @ T=
   ST-BUF ST-WANT-U @ ST-WANT T$= ;

: ST-LINES-PARTIAL-CB ( ptr u8 n n -- ) {: a:ptr u line :}
   line 1 = if a u s" alpha" T$= then
   line 2 = if a u s" beta" T$= then
   line 2 > if STR-FALSE T-ASSERT then
   ST-LINE-COUNT @ 1+ ST-LINE-COUNT ! ;

: ST-LINES-CRLF-CB ( ptr u8 n n -- ) {: a:ptr u line :}
   line 1 = if a u s" alpha" T$= then
   line 2 = if a u s" beta" T$= then
   ST-LINE-COUNT @ 1+ ST-LINE-COUNT ! ;

: ST-LINES-NOOP ( ptr u8 n n -- )
   drop 2drop ;

: TEST-SOURCE-FILE-LINES-EMPTY ( -- )
   ST-A s" " WRITE-ALL
   0 ST-LINE-COUNT !
   ST-A [: ST-LINES-NOOP ;] ST-FILE-LINES
   ST-LINE-COUNT @ 0 T= ;

: TEST-SOURCE-FILE-LINES-PARTIAL ( -- )
   SB-RESET s" alpha" SB-APPEND ST-LF s" beta" SB-APPEND
   ST-A SB$ WRITE-ALL
   0 ST-LINE-COUNT !
   ST-A [: ST-LINES-PARTIAL-CB ;] ST-FILE-LINES
   ST-LINE-COUNT @ 2 T= ;

: TEST-SOURCE-FILE-LINES-CRLF ( -- )
   SB-RESET s" alpha" SB-APPEND ST-CR ST-LF s" beta" SB-APPEND ST-CR ST-LF
   ST-A SB$ WRITE-ALL
   0 ST-LINE-COUNT !
   ST-A [: ST-LINES-CRLF-CB ;] ST-FILE-LINES
   ST-LINE-COUNT @ 2 T= ;

: TEST-SOURCE-FILE-LINES-LONG ( -- )
   ST-A s" abcde" WRITE-ALL
   ST-A [: ST-LINES-NOOP ;] ST-SMALL-FILE-LINES ;

: TEST-SOURCE-FILE-LINES-MISSING ( -- )
   ST-ROOT s" missing-lines.txt" ST-BUF JOIN-PATH {: u :}
   ST-BUF u [: ST-LINES-NOOP ;] ST-FILE-LINES ;

: TEST-SOURCE-ERRORS ( -- )
   s" abc" s" xyz" ST-BUF 5 ST-INSERT-BEFORE-FINAL-LINE drop ;

: TEST-SOURCE-APPEND-NEG-LEN ( -- )
   -1 ST-APPEND-LEN !
   s" x" >LEN ST-BUF 4 >LEN ST-APPEND-LEN SOURCE-APPEND-BYTES ;

: TEST-SOURCE-APPEND-HIGH-LEN ( -- )
   5 ST-APPEND-LEN !
   s" x" >LEN ST-BUF 4 >LEN ST-APPEND-LEN SOURCE-APPEND-BYTES ;

: SOURCE-TEST-MAIN ( -- )
   T-RESET
   ST-PREPARE
   TEST-READ-STDIN-ALL
   TEST-CONCAT-FILES
   TEST-WRITE-SOURCE-LIST
   TEST-INSERT-BEFORE-FINAL-LINE
   TEST-COMMENT-EXPORTS
   TEST-SOURCE-FILE-LINES-EMPTY
   TEST-SOURCE-FILE-LINES-PARTIAL
   TEST-SOURCE-FILE-LINES-CRLF
   [: TEST-SOURCE-FILE-LINES-LONG ;] E-FS-CAPACITY TTHROWSQ
   [: TEST-SOURCE-FILE-LINES-MISSING ;] E-FS-OPEN TTHROWSQ
   [: TEST-SOURCE-ERRORS ;] E-FS-CAPACITY TTHROWSQ
   [: TEST-SOURCE-APPEND-NEG-LEN ;] E-FS-CAPACITY TTHROWSQ
   [: TEST-SOURCE-APPEND-HIGH-LEN ;] E-FS-CAPACITY TTHROWSQ
   CLEANUP-RUN
   ST-ROOT EXISTS? TFALSE
   T-REPORT
   s" source-test: ok" type cr ;

: SOURCE-TEST-STDIN-MAIN ( -- )
   T-RESET
   TEST-READ-STDIN-DATA
   T-REPORT
   s" source-test stdin: ok" type cr ;

: SOURCE-TEST-USAGE ( -- )
   s" source-test: usage: [stdin]" 64 die ;

: SOURCE-TEST-ENTRY ( -- )
   SCRIPT-ARGC 0= if SOURCE-TEST-MAIN exit then
   SCRIPT-ARGC 1 <> if SOURCE-TEST-USAGE then
   0 SCRIPT-ARGV$ s" stdin" STR= if SOURCE-TEST-STDIN-MAIN exit then
   SOURCE-TEST-USAGE ;

SOURCE-TEST-ENTRY
