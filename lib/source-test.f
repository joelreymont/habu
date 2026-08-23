\ source-test.f - focused tests for checked source materialization helpers.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/memory.f lib/fs.f lib/fs-mutate.f lib/source.f lib/source-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/source.f
require lib/test/vmsize.f

\ Reopens the package rather than importing it: the strip rule (COMMENT-EXPORTS)
\ and its destination bound (COMMENT-NEED) are package privates, and they are
\ exactly what these cases exist to hold. Only COMMENT-EXPORTS$ is public, and a
\ test that could reach nothing but the public word could not tell a correct
\ bound from a lucky one.
package SOURCE

2048 constant ST-CAP
92 constant ST-BACKSLASH

\ The large case: every line is the worst one for the strip, `EXPORT A` (9 bytes
\ in, 11 bytes out), repeated past the $20000 destination constant this dot
\ deleted. 20000 of them is 180000 bytes in and 220000 out - both over the old
\ cap, so a destination sized by any constant this file used to name throws
\ E-FS-CAPACITY here instead of answering.
20000 constant ST-BIG-LINES

variable ST-SRC-U
variable ST-WANT-U
variable ST-APPEND-LEN
variable ST-QP-U
variable ST-BIG-A
variable ST-BIG-U

create ST-BUF ST-CAP allot
create ST-SRC-BUF ST-CAP allot
create ST-WANT-BUF ST-CAP allot
create ST-QP-BUF ST-CAP allot

: ST-BYTES! ( ptr u8 n ptr u8 ptr n -- ) {: src:ptr u dst:ptr lenp:ptr :}
   u ST-CAP > if E-FS-CAPACITY throw then
   src dst u BYTE-COPY
   u lenp ! ;

: ST-SRC ( -- ptr u8 n )
   ST-SRC-BUF ST-SRC-U @ ;

: ST-WANT ( -- ptr u8 n )
   ST-WANT-BUF ST-WANT-U @ ;

: ST-READ-STDIN-ALL ( ptr u8 n -- n )
   >LEN READ-STDIN-ALL LEN>N ;

: ST-COMMENT-EXPORTS ( ptr u8 n ptr u8 n -- n )
   {: src:ptr srcu dst:ptr cap :}
   src srcu >LEN dst cap >LEN COMMENT-EXPORTS LEN>N ;

: ST-LF ( -- )
   10 SB-APPEND-C ;

: ST-SB>SRC ( -- )
   SB$ ST-SRC-BUF ST-SRC-U ST-BYTES! ;

: ST-SB>WANT ( -- )
   SB$ ST-WANT-BUF ST-WANT-U ST-BYTES! ;

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

: TEST-READ-STDIN-ALL ( -- )
   ST-BUF ST-CAP ST-READ-STDIN-ALL 0 T= ;

: TEST-READ-STDIN-DATA ( -- )
   ST-BUF ST-CAP ST-READ-STDIN-ALL 4 T=
   ST-BUF 4 s" DATA" T$= ;

: TEST-COMMENT-EXPORTS ( -- )
   ST-EXPORT-CASE!
   ST-SRC ST-BUF ST-CAP ST-COMMENT-EXPORTS ST-WANT-U @ T=
   ST-BUF ST-WANT-U @ ST-WANT T$= ;

\ Package context splits EXPORT's roles (dot habu-compiler-pkg-re-688212c1):
\ an in-package `EXPORT NAME` is the re-export declaration and passes through
\ untouched; only top-level directive lines are commented, including after
\ the package block closes with `;package`.
: ST-EXPORT-PKG-CASE! ( -- )
   SB-RESET
   s" EXPORT TOP1" SB-APPEND ST-LF
   s" package XP" SB-APPEND ST-LF
   s" public" SB-APPEND ST-LF
   s" EXPORT XA:W" SB-APPEND ST-LF
   s" ;package" SB-APPEND ST-LF
   s" EXPORT TOP2" SB-APPEND ST-LF
   ST-SB>SRC
   SB-RESET
   ST-BACKSLASH SB-APPEND-C STR-SPACE SB-APPEND-C s" EXPORT TOP1" SB-APPEND ST-LF
   s" package XP" SB-APPEND ST-LF
   s" public" SB-APPEND ST-LF
   s" EXPORT XA:W" SB-APPEND ST-LF
   s" ;package" SB-APPEND ST-LF
   ST-BACKSLASH SB-APPEND-C STR-SPACE SB-APPEND-C s" EXPORT TOP2" SB-APPEND ST-LF
   ST-SB>WANT ;

: TEST-COMMENT-EXPORTS-PKG ( -- )
   ST-EXPORT-PKG-CASE!
   ST-SRC ST-BUF ST-CAP ST-COMMENT-EXPORTS ST-WANT-U @ T=
   ST-BUF ST-WANT-U @ ST-WANT T$= ;

: ST-BIG-LINE$ ( -- ptr u8 n )
   s" EXPORT A" ;

: ST-BIG$ ( -- ptr u8 n )
   ST-BIG-A SOURCE-PTR-U8@ ST-BIG-U @ ;

: ST-BIG-C! ( n n -- ) {: c:n off:n :}
   c ST-BIG-A SOURCE-PTR-U8@ off + c! ;

: ST-BIG-LINE! ( n -- ) {: off:n :}
   ST-BIG-LINE$ {: a:ptr u:n :}
   a ST-BIG-A SOURCE-PTR-U8@ off + u BYTE-COPY
   SOURCE-LF off u + ST-BIG-C! ;

: ST-BIG-BUILD ( -- )
   ST-BIG-LINE$ nip 1 + {: lineu:n :}
   ST-BIG-LINES lineu * {: need:n :}
   need SOURCE-ALLOC-BUF ST-BIG-A SOURCE-PTR-U8!
   0 begin dup ST-BIG-LINES < while
      dup lineu * ST-BIG-LINE!
      1+
   repeat drop
   need ST-BIG-U ! ;

\ The property the fixed destination could not have: a source over every
\ constant this module used to name is commented WHOLE, and the answer is the
\ exact byte count the rule produces - the comment prefix on every line.
: TEST-COMMENT-EXPORTS-LARGE ( -- )
   ST-BIG-BUILD
   ST-BIG$ nip {: u:n :}
   u ST-BIG-LINES ST-BIG-LINE$ nip 1 + * T=
   ST-BIG$ >LEN COMMENT-EXPORTS$ {: out:ptr outu:len :}
   outu LEN>N  u ST-BIG-LINES 2 * +  T=
   out 11 s\" \\ EXPORT A\n" T$=
   out outu LEN>N 11 - + 11 s\" \\ EXPORT A\n" T$= ;

\ The bound is not a guess: it is what the rule needs, so the strip fills it to
\ the byte on the worst input and never exceeds it.
: TEST-COMMENT-NEED-IS-TIGHT ( -- )
   ST-BIG$ >LEN COMMENT-NEED {: need:n :}
   need  ST-BIG$ nip ST-BIG-LINES 2 * +  T=
   ST-EXPORT-CASE!
   ST-SRC >LEN COMMENT-NEED ST-WANT-U @ >= TTRUE ;

\ An empty source is a source: the strip answers an empty span, it does not die
\ in the sizing.
: TEST-COMMENT-EXPORTS-EMPTY ( -- )
   SB-RESET ST-SB>SRC
   ST-SRC >LEN COMMENT-EXPORTS$ {: out:ptr outu:len :}
   outu LEN>N 0 T= ;

\ Growing must not accumulate mappings. One measured growth of S pages is the
\ unit; growing S -> 2S -> 4S -> 8S afterwards costs 8 units live, and 15 if
\ every superseded span is kept. The threshold sits between them, and the whole
\ assertion is a RATIO of measured pages, so it never assumes a page size.
1024 1024 * constant ST-GROW-STEP

: TEST-BUF-GROWTH-RELEASES ( -- )
   VMSIZE:PAGES {: m0:n :}
   ST-GROW-STEP BUF-ENSURE
   VMSIZE:PAGES {: m1:n :}
   ST-GROW-STEP 2 * BUF-ENSURE
   ST-GROW-STEP 4 * BUF-ENSURE
   ST-GROW-STEP 8 * BUF-ENSURE
   VMSIZE:PAGES {: m2:n :}
   m1 m0 - {: unit:n :}
   unit 0 > TTRUE
   m2 m0 - unit 11 * <= TTRUE ;

: TEST-SOURCE-APPEND-NEG-LEN ( -- )
   -1 ST-APPEND-LEN !
   s" x" >LEN ST-BUF 4 >LEN ST-APPEND-LEN SOURCE-APPEND-BYTES ;

: TEST-SOURCE-APPEND-HIGH-LEN ( -- )
   5 ST-APPEND-LEN !
   s" x" >LEN ST-BUF 4 >LEN ST-APPEND-LEN SOURCE-APPEND-BYTES ;

: ST-QP$ ( -- ptr u8 n )
   ST-QP-BUF ST-QP-U @ LEN>N ;

: ST-QPATH ( ptr u8 n -- ) {: a:ptr u:n :}
   0 >LEN ST-QP-U !
   a u >LEN ST-QP-BUF ST-CAP >LEN ST-QP-U SOURCE-APPEND-QPATH ;

: ST-QPATH-WANT$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   SB-RESET
   s" s" SB-APPEND
   34 SB-APPEND-C
   32 SB-APPEND-C
   a u SB-APPEND
   34 SB-APPEND-C
   SB$ ;

: ST-QP-UNSAFE! ( n -- ) {: c:n :}
   SB-RESET
   s" a" SB-APPEND
   c SB-APPEND-C
   s" b.f" SB-APPEND
   SB$ ST-QPATH ;

: TEST-QPATH-SAFE ( -- )
   s" lib/foo.f" ST-QPATH
   ST-QP$ s" lib/foo.f" ST-QPATH-WANT$ T$= ;

: TEST-QPATH-SPACE ( -- )
   s" a dir/b.f" ST-QPATH
   ST-QP$ s" a dir/b.f" ST-QPATH-WANT$ T$= ;

: TEST-QPATH-DQ ( -- )
   [: 34 ST-QP-UNSAFE! ;] E-FS-PATH-UNSAFE TTHROWSQ ;

: TEST-QPATH-BS ( -- )
   [: ST-BACKSLASH ST-QP-UNSAFE! ;] E-FS-PATH-UNSAFE TTHROWSQ ;

: TEST-QPATH-LF ( -- )
   [: 10 ST-QP-UNSAFE! ;] E-FS-PATH-UNSAFE TTHROWSQ ;

: TEST-QPATH-CR ( -- )
   [: 13 ST-QP-UNSAFE! ;] E-FS-PATH-UNSAFE TTHROWSQ ;

: ST-MAIN ( -- )
   T-RESET
   TEST-READ-STDIN-ALL
   TEST-QPATH-SAFE
   TEST-QPATH-SPACE
   TEST-QPATH-DQ
   TEST-QPATH-BS
   TEST-QPATH-LF
   TEST-QPATH-CR
   TEST-COMMENT-EXPORTS
   TEST-COMMENT-EXPORTS-PKG
   TEST-COMMENT-EXPORTS-LARGE
   TEST-COMMENT-EXPORTS-EMPTY
   TEST-BUF-GROWTH-RELEASES
   TEST-COMMENT-NEED-IS-TIGHT
   [: TEST-SOURCE-APPEND-NEG-LEN ;] E-FS-CAPACITY TTHROWSQ
   [: TEST-SOURCE-APPEND-HIGH-LEN ;] E-FS-CAPACITY TTHROWSQ
   T-REPORT
   s" source-test: ok" type cr ;

: ST-STDIN-MAIN ( -- )
   T-RESET
   TEST-READ-STDIN-DATA
   T-REPORT
   s" source-test stdin: ok" type cr ;

: ST-USAGE ( -- )
   s" source-test: usage: [stdin]" 64 die ;

: ST-ENTRY ( -- )
   SCRIPT-ARGC 0= if ST-MAIN exit then
   SCRIPT-ARGC 1 <> if ST-USAGE then
   0 SCRIPT-ARGV$ s" stdin" STR= if ST-STDIN-MAIN exit then
   ST-USAGE ;

ST-ENTRY

;package
