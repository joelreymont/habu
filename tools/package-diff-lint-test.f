\ package-diff-lint-test.f - hostile exact-diff package ownership fixtures.
\ Run: bin/hb --load tools/package-diff-lint-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/vector.f
require lib/fs.f
require lib/fs-mutate.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f
require tools/lint/source-lex.f
require tools/package-diff-lint-core.f

package PACKAGE-DIFF
private

$4000 constant TEST-SOURCE-CAP
$8000 constant TEST-DIFF-CAP
$4000 constant TEST-OUT-CAP
64 constant TEST-NAME-CAP
10 constant TEST-LF-C
32 constant TEST-SPACE-C
34 constant TEST-DQUOTE-C
43 constant TEST-PLUS-C
45 constant TEST-MINUS-C
92 constant TEST-BACKSLASH-C
96 constant TEST-TICK-C   \ backtick: the report's finding-subject delimiter
$0B constant TEST-VT-C
9 constant TEST-PATH#     \ rows in the grammar-fixture path table
14 constant TEST-OPENER#  \ declaration openers the category knows
5 constant TEST-PREFIX-U  \ length of the `test/` every listed path begins with
32 constant TEST-CASE-DELTA   \ lower-case byte minus its upper-case twin

create TEST-NAME-BUF TEST-NAME-CAP allot
create TEST-ROOT-BUF FS-PATH-CAP allot
create TEST-PATH-BUF FS-PATH-CAP allot
\ A path built for a hostile variant cannot live in TEST-PATH-BUF: TEST-FULL-PATH
\ joins into that same buffer, so passing it back in would read and write one
\ buffer at once.
create TEST-ALT-BUF FS-PATH-CAP allot
create TEST-SOURCE-BUF TEST-SOURCE-CAP allot
create TEST-DIFF-BUF TEST-DIFF-CAP allot
create TEST-OUT-BUF TEST-OUT-CAP allot

variable TEST-ROOT-U
variable TEST-PATH-U
variable TEST-SOURCE-U
variable TEST-SOURCE-START
variable TEST-DIFF-U
variable TEST-CELL-BAD    \ path-and-opener cells that behaved wrongly
variable TEST-CELL-OK     \ path-and-opener cells that were admitted
variable TEST-ROW-BAD     \ per-path rejection checks that behaved wrongly

: TEST-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr up:ptr :}
   a dst u BYTE-COPY
   u up ! ;

: TEST-ROOT$ ( -- ptr u8 n )
   TEST-ROOT-BUF TEST-ROOT-U @ ;

: TEST-SOURCE$ ( -- ptr u8 n )
   TEST-SOURCE-BUF TEST-SOURCE-U @ ;

: TEST-SOURCE-RESET ( -- )
   0 TEST-SOURCE-U ! ;

: TEST-SOURCE+ ( ptr u8 n -- ) {: a:ptr u:n :}
   TEST-SOURCE-U @ u + TEST-SOURCE-CAP > if E-FS-CAPACITY throw then
   a TEST-SOURCE-BUF TEST-SOURCE-U @ + u BYTE-COPY
   TEST-SOURCE-U @ u + TEST-SOURCE-U ! ;

: TEST-SOURCE-C ( n -- ) {: c:n :}
   TEST-SOURCE-U @ TEST-SOURCE-CAP >= if E-FS-CAPACITY throw then
   c TEST-SOURCE-BUF TEST-SOURCE-U @ + c!
   TEST-SOURCE-U @ 1+ TEST-SOURCE-U ! ;

: TEST-SOURCE-LINE ( ptr u8 n -- )
   TEST-SOURCE+ TEST-LF-C TEST-SOURCE-C ;

: TEST-SOURCE-STRING-LINE ( -- )
   s" : TEXT ( -- ) s" TEST-SOURCE+
   TEST-DQUOTE-C TEST-SOURCE-C
   s" : FORGED package FORGED ;package" TEST-SOURCE+
   TEST-DQUOTE-C TEST-SOURCE-C
   s"  drop ;" TEST-SOURCE-LINE ;

: TEST-DIFF$ ( -- ptr u8 n )
   TEST-DIFF-BUF TEST-DIFF-U @ ;

: TEST-DIFF-RESET ( -- )
   0 TEST-DIFF-U ! ;

: TEST-DIFF+ ( ptr u8 n -- ) {: a:ptr u:n :}
   TEST-DIFF-U @ u + TEST-DIFF-CAP > if E-FS-CAPACITY throw then
   a TEST-DIFF-BUF TEST-DIFF-U @ + u BYTE-COPY
   TEST-DIFF-U @ u + TEST-DIFF-U ! ;

: TEST-DIFF-C ( n -- ) {: c:n :}
   TEST-DIFF-U @ TEST-DIFF-CAP >= if E-FS-CAPACITY throw then
   c TEST-DIFF-BUF TEST-DIFF-U @ + c!
   TEST-DIFF-U @ 1+ TEST-DIFF-U ! ;

: TEST-FULL-PATH ( ptr u8 n -- ptr u8 n ) {: path:ptr pathu:n :}
   TEST-ROOT$ path pathu TEST-PATH-BUF JOIN-PATH TEST-PATH-U !
   TEST-PATH-BUF TEST-PATH-U @ ;

: TEST-WRITE-SOURCE ( ptr u8 n -- )
   TEST-FULL-PATH TEST-SOURCE$ WRITE-ALL ;

: TEST-LF ( -- )
   TEST-LF-C TEST-DIFF-C ;

: TEST-U+ ( n -- ) {: u:n :}
   u 9 > if u 10 / RECURSE then
   u 10 mod 48 + TEST-DIFF-C ;

: TEST-DIFF-HEAD ( ptr u8 n -- ) {: path:ptr pathu:n :}
   s" diff --git a/" TEST-DIFF+ path pathu TEST-DIFF+
   s"  b/" TEST-DIFF+ path pathu TEST-DIFF+ TEST-LF ;

: TEST-ADD-HEAD ( ptr u8 n -- ) {: path:ptr pathu:n :}
   path pathu TEST-DIFF-HEAD
   s" new file mode 100644" TEST-DIFF+ TEST-LF
   s" index 0000000..abcdef0" TEST-DIFF+ TEST-LF
   s" --- /dev/null" TEST-DIFF+ TEST-LF
   s" +++ b/" TEST-DIFF+ path pathu TEST-DIFF+ TEST-LF ;

: TEST-MODIFY-HEAD ( ptr u8 n -- ) {: path:ptr pathu:n :}
   path pathu TEST-DIFF-HEAD
   s" index 1234567..abcdef0 100644" TEST-DIFF+ TEST-LF
   s" --- a/" TEST-DIFF+ path pathu TEST-DIFF+ TEST-LF
   s" +++ b/" TEST-DIFF+ path pathu TEST-DIFF+ TEST-LF ;

: TEST-ADDED-SOURCE-LINES ( -- )
   0 TEST-SOURCE-START !
   0 begin dup TEST-SOURCE-U @ < while
      dup TEST-SOURCE-BUF + c@ TEST-LF-C = if
         TEST-PLUS-C TEST-DIFF-C
         TEST-SOURCE-BUF TEST-SOURCE-START @ +
         over TEST-SOURCE-START @ - TEST-DIFF+ TEST-LF
         dup 1+ TEST-SOURCE-START !
      then
      1+
   repeat drop ;

: TEST-ADD-SOURCE-SECTION ( ptr u8 n -- ) {: path:ptr pathu:n :}
   path pathu TEST-WRITE-SOURCE
   path pathu TEST-ADD-HEAD
   s" @@ -0,0 +1," TEST-DIFF+
   TEST-SOURCE$ TEST-LF-C LINT-COUNT-CHAR TEST-U+
   s"  @@" TEST-DIFF+ TEST-LF
   TEST-ADDED-SOURCE-LINES ;

: TEST-RUN-BUILT ( -- n n )
   PACKAGE-DIFF:RESET
   TEST-ROOT$ PACKAGE-DIFF:ROOT!
   TEST-OUT-BUF TEST-OUT-CAP LINT-OUT-BUFFER!
   [: TEST-DIFF$ PACKAGE-DIFF:SOURCE PACKAGE-DIFF:FINISH ;] catch {: rc:n :}
   PACKAGE-DIFF:FINDINGS {: bad:n :}
   LINT-OUT-BUFFER-OFF
   rc bad ;

: TEST-EXPECT-CLEAN ( -- )
   TEST-RUN-BUILT swap 0 T= 0 T= ;

: TEST-EXPECT-FINDINGS ( n -- ) {: want:n :}
   TEST-RUN-BUILT {: rc:n bad:n :}
   rc 1 T=
   bad want T= ;

: TEST-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-package-diff-lint" TMPDIR-MKDIR
   TEST-ROOT-BUF TEST-ROOT-U TEST-COPY!
   TEST-ROOT$ CLEANUP-TREE+
   TEST-ROOT$ s" lib" TEST-PATH-BUF JOIN-PATH TEST-PATH-U !
   TEST-PATH-BUF TEST-PATH-U @ MAKE-DIRS
   TEST-ROOT$ s" lib/type" TEST-PATH-BUF JOIN-PATH TEST-PATH-U !
   TEST-PATH-BUF TEST-PATH-U @ MAKE-DIRS
   TEST-ROOT$ s" lib/adt" TEST-PATH-BUF JOIN-PATH TEST-PATH-U !
   TEST-PATH-BUF TEST-PATH-U @ MAKE-DIRS
   TEST-ROOT$ s" tools" TEST-PATH-BUF JOIN-PATH TEST-PATH-U !
   TEST-PATH-BUF TEST-PATH-U @ MAKE-DIRS
   \ holds the stage0 hostile that ends in a listed path from outside test/,
   \ tools/test/<name>
   TEST-ROOT$ s" tools/test" TEST-PATH-BUF JOIN-PATH TEST-PATH-U !
   TEST-PATH-BUF TEST-PATH-U @ MAKE-DIRS
   TEST-ROOT$ s" test" TEST-PATH-BUF JOIN-PATH TEST-PATH-U !
   TEST-PATH-BUF TEST-PATH-U @ MAKE-DIRS
   TEST-ROOT$ s" test/lib" TEST-PATH-BUF JOIN-PATH TEST-PATH-U !
   TEST-PATH-BUF TEST-PATH-U @ MAKE-DIRS
   \ holds the nested-duplicate hostile, test/test/<name>
   TEST-ROOT$ s" test/test" TEST-PATH-BUF JOIN-PATH TEST-PATH-U !
   TEST-PATH-BUF TEST-PATH-U @ MAKE-DIRS
   TEST-ROOT$ s" src/core" TEST-PATH-BUF JOIN-PATH TEST-PATH-U !
   TEST-PATH-BUF TEST-PATH-U @ MAKE-DIRS
   TEST-ROOT$ s" src/habu" TEST-PATH-BUF JOIN-PATH TEST-PATH-U !
   TEST-PATH-BUF TEST-PATH-U @ MAKE-DIRS
   \ a copy of the engine-trunk directory one level down, so a fixture can sit at
   \ a path that ENDS WITH a listed one - which is what a suffix comparison at the
   \ shared row site would wrongly admit
   TEST-ROOT$ s" test/src/habu" TEST-PATH-BUF JOIN-PATH TEST-PATH-U !
   TEST-PATH-BUF TEST-PATH-U @ MAKE-DIRS
   \ the ARM64 encoder prefix, which the entry names file by file rather than by
   \ directory, so the disassembler beside them is a negative
   TEST-ROOT$ s" src/arch/arm64" TEST-PATH-BUF JOIN-PATH TEST-PATH-U !
   TEST-PATH-BUF TEST-PATH-U @ MAKE-DIRS
   \ the image writer, whose entry likewise names three files rather than the
   \ src/os tree, so the signer beside macho.f is a negative
   TEST-ROOT$ s" src/os/linux" TEST-PATH-BUF JOIN-PATH TEST-PATH-U !
   TEST-PATH-BUF TEST-PATH-U @ MAKE-DIRS
   TEST-ROOT$ s" src/os/macos" TEST-PATH-BUF JOIN-PATH TEST-PATH-U !
   TEST-PATH-BUF TEST-PATH-U @ MAKE-DIRS
   \ the Gforth recovery mirror's own directory, plus a same-basename neighbour
   \ one level up and a copy of the whole path under test/, which is what a
   \ suffix comparison would wrongly admit
   TEST-ROOT$ s" bootstrap/cg" TEST-PATH-BUF JOIN-PATH TEST-PATH-U !
   TEST-PATH-BUF TEST-PATH-U @ MAKE-DIRS
   TEST-ROOT$ s" test/bootstrap/cg" TEST-PATH-BUF JOIN-PATH TEST-PATH-U !
   TEST-PATH-BUF TEST-PATH-U @ MAKE-DIRS ;

: TEST-GLOBAL-SOURCE ( ptr u8 n -- )
   TEST-SOURCE-RESET
   s" : " TEST-SOURCE+ TEST-SOURCE+ s"  ( -- n ) 1 ;" TEST-SOURCE-LINE ;

: TEST-GLOBAL-OWNERS ( -- )
   TEST-DIFF-RESET
   s" LIB-LEAK" TEST-GLOBAL-SOURCE s" lib/global.f" TEST-ADD-SOURCE-SECTION
   s" TOOL-LEAK" TEST-GLOBAL-SOURCE s" tools/global.f" TEST-ADD-SOURCE-SECTION
   s" TEST-LEAK" TEST-GLOBAL-SOURCE s" test/global.f" TEST-ADD-SOURCE-SECTION
   3 TEST-EXPECT-FINDINGS ;

: TEST-CASE-CLOSE-REOPEN ( -- )
   TEST-SOURCE-RESET
   s" PaCkAgE MOD" TEST-SOURCE-LINE
   s" : OK ( -- n ) 1 ;" TEST-SOURCE-LINE
   s" ;PaCkAgE" TEST-SOURCE-LINE
   s" cHeCkEd: LEAK ( -- n ) 2 ;" TEST-SOURCE-LINE
   s" package MOD" TEST-SOURCE-LINE
   s" : OK2 ( -- n ) 3 ;" TEST-SOURCE-LINE
   s" ;package" TEST-SOURCE-LINE
   TEST-DIFF-RESET s" lib/case.f" TEST-ADD-SOURCE-SECTION
   1 TEST-EXPECT-FINDINGS ;

: TEST-REDUNDANT-PREFIXES ( -- )
   TEST-DIFF-RESET
   TEST-SOURCE-RESET
   s" package LRD" TEST-SOURCE-LINE
   s" : LRD-OPEN ( -- n ) 1 ;" TEST-SOURCE-LINE
   s" ;package" TEST-SOURCE-LINE
   s" lib/lrd.f" TEST-ADD-SOURCE-SECTION
   TEST-SOURCE-RESET
   s" package STORE" TEST-SOURCE-LINE
   s" : CACHE-OPEN ( -- n ) 1 ;" TEST-SOURCE-LINE
   s" ;package" TEST-SOURCE-LINE
   s" lib/cache.f" TEST-ADD-SOURCE-SECTION
   2 TEST-EXPECT-FINDINGS ;

: TEST-ADD-COLON-DEFINERS ( -- )
   s" : COLON-WORD ( -- ) ;" TEST-SOURCE-LINE
   s" +: PLUS-COLON-WORD ( -- ) ;" TEST-SOURCE-LINE
   s" CHECKED: CHECKED-WORD ( -- ) ;" TEST-SOURCE-LINE
   s" TRUSTED: TRUSTED-WORD ( -- ) ;" TEST-SOURCE-LINE
   s" KERNEL: KERNEL-WORD ( -- ) ;" TEST-SOURCE-LINE
   s" CAST: CAST-WORD ( n -- n ) ;" TEST-SOURCE-LINE
   s" MODEL: MODEL-WORD ( -- ) ;" TEST-SOURCE-LINE ;

: TEST-ADD-BLOCK-DEFINERS ( -- )
   s" SUMTYPE sum-word 0 ;SUMTYPE" TEST-SOURCE-LINE
   s" PRODUCT product-word 0 ;PRODUCT" TEST-SOURCE-LINE
   s" ENUM enum-word item ;ENUM" TEST-SOURCE-LINE
   s" STRUCTURE structure-word 0 ;STRUCTURE" TEST-SOURCE-LINE
   s" VALUE-RECORD RECORD-WORD END-VALUE-RECORD" TEST-SOURCE-LINE
   s" BEGIN-STRUCTURE LOW-WORD END-STRUCTURE" TEST-SOURCE-LINE ;

: TEST-ADD-NATIVE-DEFINERS ( -- )
   s" constant CONSTANT-WORD" TEST-SOURCE-LINE
   s" 2constant TWO-CONSTANT-WORD" TEST-SOURCE-LINE
   s" fconstant FLOAT-CONSTANT-WORD" TEST-SOURCE-LINE
   s" variable VARIABLE-WORD" TEST-SOURCE-LINE
   s" 2variable TWO-VARIABLE-WORD" TEST-SOURCE-LINE
   s" fvariable FLOAT-VARIABLE-WORD" TEST-SOURCE-LINE
   s" create CREATE-WORD" TEST-SOURCE-LINE
   s" value VALUE-WORD" TEST-SOURCE-LINE
   s" defer DEFER-WORD" TEST-SOURCE-LINE ;

: TEST-ADD-STORAGE-DEFINERS ( -- )
   s" LAYOUT-BUFFER LAYOUT-WORD" TEST-SOURCE-LINE
   s" DEFER-LAYOUT-BUFFER DEFER-LAYOUT-WORD" TEST-SOURCE-LINE
   s" TYPED-BUFFER TYPED-BUFFER-WORD" TEST-SOURCE-LINE
   s" TYPED-VARIABLE TYPED-VARIABLE-WORD" TEST-SOURCE-LINE
   s" PTR-VARIABLE PTR-VARIABLE-WORD" TEST-SOURCE-LINE
   s" PTR-FIELD: PTR-FIELD-WORD" TEST-SOURCE-LINE
   s" CFIELD: CFIELD-WORD" TEST-SOURCE-LINE
   s" +FIELD FIELD-WORD" TEST-SOURCE-LINE ;

: TEST-ADD-TYPE-DEFINERS ( -- )
   s" NEWTYPE family-word 0" TEST-SOURCE-LINE
   s" DEFTYPE DEFTYPE-WORD" TEST-SOURCE-LINE
   s" DEFLINEAR LINEAR-WORD" TEST-SOURCE-LINE
   s" ENUM+ ENUM-WORD" TEST-SOURCE-LINE
   s" ENUM4+ ENUM4-WORD" TEST-SOURCE-LINE ;

: TEST-ADD-PROJECT-DEFINERS ( -- )
   s" BUFFER: BYTE-BUFFER-WORD" TEST-SOURCE-LINE
   s" BUFFER CODEGEN-BUFFER-WORD" TEST-SOURCE-LINE
   s" BUFFER-E CODEGEN-BUFFER-E-WORD" TEST-SOURCE-LINE
   s" CODEGEN:BUFFER QUALIFIED-BUFFER-WORD" TEST-SOURCE-LINE
   s" CODEGEN:BUFFER-E QUALIFIED-BUFFER-E-WORD" TEST-SOURCE-LINE
   s" TASK TASK-WORD" TEST-SOURCE-LINE
   s" +USER USER-WORD" TEST-SOURCE-LINE
   s" FACILITY FACILITY-WORD" TEST-SOURCE-LINE
   s" TASK:TASK QUALIFIED-TASK-WORD" TEST-SOURCE-LINE
   s" TASK:+USER QUALIFIED-USER-WORD" TEST-SOURCE-LINE
   s" TASK:FACILITY QUALIFIED-FACILITY-WORD" TEST-SOURCE-LINE
   s" TR-FILES: TEST-RUN-FILES-WORD" TEST-SOURCE-LINE
   s" GE-FILES: GATE-ENGINE-FILES-WORD" TEST-SOURCE-LINE
   s" IOP: ICODE-OP-WORD" TEST-SOURCE-LINE
   s" CONST TEST-CONSTANT-WORD" TEST-SOURCE-LINE
   s" ARR TEST-ARRAY-WORD" TEST-SOURCE-LINE ;

: TEST-ADD-MAKI-DEFINERS ( -- )
   s" EXTENT: EXTENT-WORD" TEST-SOURCE-LINE
   s" FREE-EXTENT: FREE-EXTENT-WORD" TEST-SOURCE-LINE
   s" EXTPROD: EXTENT-PRODUCT-WORD" TEST-SOURCE-LINE
   s" TENSOR: TENSOR-WORD" TEST-SOURCE-LINE
   s" ITENSOR: INDEX-TENSOR-WORD" TEST-SOURCE-LINE
   s" SPEC: SPEC-WORD ;" TEST-SOURCE-LINE ;

: TEST-ADD-DEFINER-INVENTORY ( -- )
   TEST-ADD-COLON-DEFINERS
   TEST-ADD-BLOCK-DEFINERS
   TEST-ADD-NATIVE-DEFINERS
   TEST-ADD-STORAGE-DEFINERS
   TEST-ADD-TYPE-DEFINERS
   TEST-ADD-PROJECT-DEFINERS
   TEST-ADD-MAKI-DEFINERS ;

: TEST-DEFINER-INVENTORY ( -- )
   TEST-SOURCE-RESET TEST-ADD-DEFINER-INVENTORY
   TEST-DIFF-RESET s" tools/forms.f" TEST-ADD-SOURCE-SECTION
   57 TEST-EXPECT-FINDINGS ;

: TEST-ADD-REGISTRY-LANGUAGE ( -- )
   s" using TEST" TEST-SOURCE-LINE
   s" SUITE package-ownership" TEST-SOURCE-LINE
   s" tools/package-diff-lint-test.f" TEST-SOURCE-LINE
   s" ;SUITE" TEST-SOURCE-LINE
   s" SUITE-STDIN package-ownership-stdin" TEST-SOURCE-LINE
   s" stdin-payload" TEST-SOURCE-LINE
   s" ;SUITE" TEST-SOURCE-LINE
   s" GROUP lint PARA" TEST-SOURCE-LINE
   s" ;GROUP" TEST-SOURCE-LINE
   s" PRIM: checker-word PRIM;" TEST-SOURCE-LINE
   s" PPRIM: checker-package checker-word PPRIM;" TEST-SOURCE-LINE
   s" VJP: +. 0 DUP ;" TEST-SOURCE-LINE
   s" GRID: extent-x" TEST-SOURCE-LINE
   s" WHERE extent-x <= block-32" TEST-SOURCE-LINE ;

: TEST-REGISTRY-LANGUAGE ( -- )
   TEST-SOURCE-RESET TEST-ADD-REGISTRY-LANGUAGE
   TEST-DIFF-RESET s" test/registry.f" TEST-ADD-SOURCE-SECTION
   TEST-EXPECT-CLEAN ;

\ ---- primitive-axiom registry rows -------------------------------------------
\ tools/lint/source-lex.f delivers a complete `PRIM: ... PRIM;` or
\ `PPRIM: pkg ... PPRIM;` row as ONE opaque REGISTRY token.  These fixtures hold
\ that contract from both directions.  A well-formed row is invisible to the
\ ownership rules: its body bytes can neither forge a package or a definition nor
\ close a real package that surrounds it.  An incomplete row stops the scan with
\ the registry code, because a token table truncated at the defect must never be
\ analysed as if it were the whole file.

: TEST-RUN-DIRECT ( -- )
   PACKAGE-DIFF:RESET
   TEST-ROOT$ PACKAGE-DIFF:ROOT!
   TEST-DIFF$ PACKAGE-DIFF:SOURCE
   PACKAGE-DIFF:FINISH ;

\ A finding count alone cannot tell a report about the real global apart from a
\ report about a word forged out of row bytes: both are "one finding".  The
\ report writes its subject between backticks, so these read that field.
: TEST-QUOTED-NAME$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   u 2 + TEST-NAME-CAP > if E-FS-CAPACITY throw then
   TEST-TICK-C TEST-NAME-BUF c!
   a TEST-NAME-BUF 1+ u BYTE-COPY
   TEST-TICK-C TEST-NAME-BUF 1+ u + c!
   TEST-NAME-BUF u 2 + ;

: TEST-REPORTED? ( ptr u8 n -- bool )
   TEST-QUOTED-NAME$ {: a:ptr u:n :}
   LINT-OUT$ a u LINT-CONTAINS? ;

: TEST-NAMES ( ptr u8 n -- ) {: a:ptr u:n :}
   s" the report names the changed global itself" T-LABEL
   a u TEST-REPORTED? TTRUE ;

: TEST-NOT-NAMES ( ptr u8 n -- ) {: a:ptr u:n :}
   s" the report never names a word forged from row bytes" T-LABEL
   a u TEST-REPORTED? TFALSE ;

: TEST-ROW-FORGED-PACKAGE ( -- )
   TEST-SOURCE-RESET
   s" PRIM: forge-pkg package FORGED PRIM;" TEST-SOURCE-LINE
   s" : ROW-LEAK-PKG ( -- n ) 1 ;" TEST-SOURCE-LINE
   TEST-DIFF-RESET s" test/row-package.f" TEST-ADD-SOURCE-SECTION
   s" a row body cannot open a package around the next definition" T-LABEL
   1 TEST-EXPECT-FINDINGS
   s" ROW-LEAK-PKG" TEST-NAMES ;

: TEST-ROW-FORGED-COLON ( -- )
   TEST-SOURCE-RESET
   s" PRIM: forge-colon : PRIM;" TEST-SOURCE-LINE
   s" : ROW-LEAK-COLON ( -- n ) 1 ;" TEST-SOURCE-LINE
   TEST-DIFF-RESET s" test/row-colon.f" TEST-ADD-SOURCE-SECTION
   s" a row body cannot open a definition that swallows the next global" T-LABEL
   1 TEST-EXPECT-FINDINGS
   s" ROW-LEAK-COLON" TEST-NAMES
   s" PRIM;" TEST-NOT-NAMES ;

: TEST-ROW-FORGED-DATA ( -- )
   TEST-SOURCE-RESET
   s" PRIM: forge-data create FORGED-CELL PRIM;" TEST-SOURCE-LINE
   s" : ROW-LEAK-DATA ( -- n ) 1 ;" TEST-SOURCE-LINE
   TEST-DIFF-RESET s" test/row-data.f" TEST-ADD-SOURCE-SECTION
   s" a row body cannot publish a data definition" T-LABEL
   1 TEST-EXPECT-FINDINGS
   s" ROW-LEAK-DATA" TEST-NAMES
   s" FORGED-CELL" TEST-NOT-NAMES ;

: TEST-ROW-KEEPS-PACKAGE ( -- )
   TEST-SOURCE-RESET
   s" package ROWPKG" TEST-SOURCE-LINE
   s" PPRIM: forged-pkg forged-word ;package PPRIM;" TEST-SOURCE-LINE
   s" : ROW-STAYS-OWNED ( -- n ) 1 ;" TEST-SOURCE-LINE
   s" ;package" TEST-SOURCE-LINE
   TEST-DIFF-RESET s" test/row-keep.f" TEST-ADD-SOURCE-SECTION
   s" a row body cannot close the package that surrounds it" T-LABEL
   TEST-EXPECT-CLEAN ;

\ Same token text, two roles: `CLOSE-PRIVATE` closes a `PPRIM:` row (it ends that
\ row's private wordlist) and is an ordinary effect field inside a bare `PRIM:`
\ row, which has no package wordlist to close.  A scanner that matched the text
\ without the family would cut one of these rows in the wrong place.
: TEST-ROW-CLOSER-ROLES ( -- )
   TEST-SOURCE-RESET
   s" PPRIM: pkg-row pkg-word CLOSE-PRIVATE" TEST-SOURCE-LINE
   s" PRIM: bare-word CLOSE-PRIVATE PRIM;" TEST-SOURCE-LINE
   s" : ROW-AFTER-CLOSERS ( -- n ) 1 ;" TEST-SOURCE-LINE
   TEST-DIFF-RESET s" test/row-closers.f" TEST-ADD-SOURCE-SECTION
   s" CLOSE-PRIVATE closes a package row and is a field in a bare row" T-LABEL
   1 TEST-EXPECT-FINDINGS
   s" ROW-AFTER-CLOSERS" TEST-NAMES ;

: TEST-ROW-REJECT ( ptr u8 n -- ) {: a:ptr u:n :}
   TEST-SOURCE-RESET
   a u TEST-SOURCE-LINE
   TEST-DIFF-RESET s" test/row-bad.f" TEST-ADD-SOURCE-SECTION
   [: TEST-RUN-DIRECT ;] E-PKGDIFF-ROW TTHROWSQ ;

: TEST-ROW-MALFORMED-SHAPES ( -- )
   s" bare row: the closer stands where the primitive name belongs" T-LABEL
   s" PRIM: PRIM;" TEST-ROW-REJECT
   s" package row: one header field then a bare-row closer" T-LABEL
   s" PPRIM: checker-package-word PRIM;" TEST-ROW-REJECT
   s" package row closed by the bare-row closer" T-LABEL
   s" PPRIM: pkg-row pkg-word PRIM;" TEST-ROW-REJECT
   s" bare row closed by the package-row closer" T-LABEL
   s" PRIM: bare-word PPRIM;" TEST-ROW-REJECT
   s" a second opener nested inside a row body" T-LABEL
   s" PRIM: outer-word PRIM: inner-word PRIM;" TEST-ROW-REJECT
   s" row body runs to end of input with no closer" T-LABEL
   s" PRIM: unclosed-word" TEST-ROW-REJECT
   s" row header runs to end of input" T-LABEL
   s" PRIM:" TEST-ROW-REJECT
   s" every rejected row released its mappings" T-LABEL
   LIVE-MAPPING# 0 T= ;

: TEST-WRITE-OLD-ROW-SOURCE ( -- )
   TEST-SOURCE-RESET
   s" package OLDROW" TEST-SOURCE-LINE
   s" : OLD-ROW-OK ( -- n ) 1 ;" TEST-SOURCE-LINE
   s" ;package" TEST-SOURCE-LINE
   s" test/row-old.f" TEST-WRITE-SOURCE ;

\ The deleted line exists only in the reconstructed old source, so this reaches
\ SCAN-OLD-BOUNDARIES and never SCAN-DEFINITIONS.
: TEST-OLD-ROW-DIFF ( -- )
   s" test/row-old.f" TEST-MODIFY-HEAD
   s" @@ -1,4 +1,3 @@" TEST-DIFF+ TEST-LF
   TEST-MINUS-C TEST-DIFF-C s" PRIM: old-word PPRIM;" TEST-DIFF+ TEST-LF
   TEST-SPACE-C TEST-DIFF-C s" package OLDROW" TEST-DIFF+ TEST-LF
   TEST-SPACE-C TEST-DIFF-C s" : OLD-ROW-OK ( -- n ) 1 ;" TEST-DIFF+ TEST-LF
   TEST-SPACE-C TEST-DIFF-C s" ;package" TEST-DIFF+ TEST-LF ;

: TEST-OLD-SIDE-ROW ( -- )
   TEST-WRITE-OLD-ROW-SOURCE
   TEST-DIFF-RESET TEST-OLD-ROW-DIFF
   s" a malformed row in the reconstructed old source rejects too" T-LABEL
   [: TEST-RUN-DIRECT ;] E-PKGDIFF-ROW TTHROWSQ ;

\ The other lexer defect must keep its own code.  One shared code would mean the
\ kind dispatch had no arms, and every source defect would send the reader to the
\ wrong place.
: TEST-ROW-QUOTE-DEFECT ( -- )
   TEST-SOURCE-RESET
   s" : ROW-OPEN-STRING ( -- ) s" TEST-SOURCE+
   TEST-DQUOTE-C TEST-SOURCE-C
   TEST-SPACE-C TEST-SOURCE-C
   s" never closed" TEST-SOURCE-LINE
   TEST-DIFF-RESET s" test/row-quote.f" TEST-ADD-SOURCE-SECTION
   s" an open string literal keeps its own source-defect code" T-LABEL
   [: TEST-RUN-DIRECT ;] E-PKGDIFF-QUOTE TTHROWSQ ;

: TEST-REGISTRY-ROWS ( -- )
   TEST-ROW-FORGED-PACKAGE
   TEST-ROW-FORGED-COLON
   TEST-ROW-FORGED-DATA
   TEST-ROW-KEEPS-PACKAGE
   TEST-ROW-CLOSER-ROLES
   TEST-ROW-MALFORMED-SHAPES
   TEST-OLD-SIDE-ROW
   TEST-ROW-QUOTE-DEFECT ;

\ ---- paren-named definitions and nameless definers ---------------------------
\ A word may legitimately be NAMED with a leading paren - src/habu/habu1.f
\ defines `: (CMP) ( n -- ) ... ;` - so the lexer hands `(CMP)` over as the
\ definition name and a packaged paren-named definition lints clean. Before the
\ standalone-`(` rule the name lexed as a comment, the definer had no name, and
\ every diff touching that file was refused as a broken artifact.
: TEST-PAREN-NAMED-DEFINITION ( -- )
   TEST-SOURCE-RESET
   s" package PARENPKG" TEST-SOURCE-LINE
   s" : (CMP) ( n -- ) drop ;" TEST-SOURCE-LINE
   s" ;package" TEST-SOURCE-LINE
   TEST-DIFF-RESET s" test/paren-name.f" TEST-ADD-SOURCE-SECTION
   s" a definition named with a leading paren keeps its package owner" T-LABEL
   TEST-EXPECT-CLEAN ;

\ A definer whose name never arrives - the definer stands at the end of the
\ scan, or a comment follows it - is a defect of the FILE, not of the diff
\ artifact, so it must carry its own source-defect code and never the artifact
\ code E-DIFF-SYNTAX.
: TEST-NONAME-AT-END ( -- )
   TEST-SOURCE-RESET
   s" package NONAME" TEST-SOURCE-LINE
   s" : OK ( -- n ) 1 ;" TEST-SOURCE-LINE
   s" ;package" TEST-SOURCE-LINE
   s" :" TEST-SOURCE-LINE
   TEST-DIFF-RESET s" test/noname-end.f" TEST-ADD-SOURCE-SECTION
   s" a definer at the end of the scan is a named source defect" T-LABEL
   [: TEST-RUN-DIRECT ;] E-PKGDIFF-NONAME TTHROWSQ ;

: TEST-NONAME-BEFORE-COMMENT ( -- )
   TEST-SOURCE-RESET
   s" : ( n -- ) drop ;" TEST-SOURCE-LINE
   TEST-DIFF-RESET s" test/noname-comment.f" TEST-ADD-SOURCE-SECTION
   s" a definer followed by a comment is a named source defect" T-LABEL
   [: TEST-RUN-DIRECT ;] E-PKGDIFF-NONAME TTHROWSQ ;

: TEST-WRITE-NONAME-OLD-SOURCE ( -- )
   TEST-SOURCE-RESET
   s" package NONAME" TEST-SOURCE-LINE
   s" : OK ( -- n ) 1 ;" TEST-SOURCE-LINE
   s" ;package" TEST-SOURCE-LINE
   s" test/noname-old.f" TEST-WRITE-SOURCE ;

\ The deleted nameless definer exists only in the reconstructed old source, so
\ this reaches OLD-START-DEFINITION and never START-DEFINITION.
: TEST-NONAME-OLD-DIFF ( -- )
   s" test/noname-old.f" TEST-MODIFY-HEAD
   s" @@ -1,4 +1,3 @@" TEST-DIFF+ TEST-LF
   TEST-SPACE-C TEST-DIFF-C s" package NONAME" TEST-DIFF+ TEST-LF
   TEST-SPACE-C TEST-DIFF-C s" : OK ( -- n ) 1 ;" TEST-DIFF+ TEST-LF
   TEST-SPACE-C TEST-DIFF-C s" ;package" TEST-DIFF+ TEST-LF
   TEST-MINUS-C TEST-DIFF-C s" : ( -- n )" TEST-DIFF+ TEST-LF ;

: TEST-OLD-SIDE-NONAME ( -- )
   TEST-WRITE-NONAME-OLD-SOURCE
   TEST-DIFF-RESET TEST-NONAME-OLD-DIFF
   s" a nameless definer in the reconstructed old source rejects too" T-LABEL
   [: TEST-RUN-DIRECT ;] E-PKGDIFF-NONAME TTHROWSQ ;

: TEST-PAREN-NAMES ( -- )
   TEST-PAREN-NAMED-DEFINITION
   TEST-NONAME-AT-END
   TEST-NONAME-BEFORE-COMMENT
   TEST-OLD-SIDE-NONAME ;

: TEST-ADD-WHOLE-CORE-EXEMPTION ( ptr u8 n ptr u8 n -- ) {: name:ptr nameu:n path:ptr pathu:n :}
   name nameu TEST-GLOBAL-SOURCE
   path pathu TEST-ADD-SOURCE-SECTION ;

: TEST-CORE-EXEMPTIONS ( -- )
   TEST-DIFF-RESET
   s" PRELUDE-GLOBAL" s" lib/prelude.f" TEST-ADD-WHOLE-CORE-EXEMPTION
   s" UTIL-GLOBAL" s" src/core/util.f" TEST-ADD-WHOLE-CORE-EXEMPTION
   s" SHA256-GLOBAL" s" src/core/sha256.f" TEST-ADD-WHOLE-CORE-EXEMPTION
   s" SUMTYPE-GLOBAL" s" src/core/sumtype.f" TEST-ADD-WHOLE-CORE-EXEMPTION
   s" LBUF-GLOBAL" s" src/core/layout-buffer.f" TEST-ADD-WHOLE-CORE-EXEMPTION
   s" ROLE-GLOBAL" s" src/core/roles.f" TEST-ADD-WHOLE-CORE-EXEMPTION
   s" STRUCTURE-GLOBAL" s" src/core/structures.f" TEST-ADD-WHOLE-CORE-EXEMPTION
   s" ENUM-GLOBAL" s" src/core/enums.f" TEST-ADD-WHOLE-CORE-EXEMPTION
   TEST-EXPECT-CLEAN
   \ The layout-buffer row is an EXACT path, like every other row here: a global
   \ in a file that merely ends with the same name is still reported.
   TEST-SOURCE-RESET
   s" LBUF-GLOBAL" TEST-GLOBAL-SOURCE
   TEST-DIFF-RESET s" test/layout-buffer.f" TEST-ADD-SOURCE-SECTION
   s" a global in test/layout-buffer.f is still reported" T-LABEL
   1 TEST-EXPECT-FINDINGS
   TEST-SOURCE-RESET
   s" : DEFTYPE ( -- ) ;" TEST-SOURCE-LINE
   s" : UNRELATED ( -- ) ;" TEST-SOURCE-LINE
   TEST-DIFF-RESET s" lib/type/deftype.f" TEST-ADD-SOURCE-SECTION
   1 TEST-EXPECT-FINDINGS
   \ The extent-index converter surface carries a TWO-name exception in its own
   \ file: both converters are exempt there, a third global beside them is not,
   \ and neither name is exempt anywhere else.
   TEST-SOURCE-RESET
   s" : IX>N ( -- ) ;" TEST-SOURCE-LINE
   s" : >RED ( -- ) ;" TEST-SOURCE-LINE
   TEST-DIFF-RESET s" lib/type/extent-role.f" TEST-ADD-SOURCE-SECTION
   s" the extent-index converters are admitted at their own path" T-LABEL
   TEST-EXPECT-CLEAN
   TEST-SOURCE-RESET
   s" : IX>N ( -- ) ;" TEST-SOURCE-LINE
   s" : UNRELATED ( -- ) ;" TEST-SOURCE-LINE
   TEST-DIFF-RESET s" lib/type/extent-role.f" TEST-ADD-SOURCE-SECTION
   1 TEST-EXPECT-FINDINGS
   TEST-SOURCE-RESET
   s" : >RED ( -- ) ;" TEST-SOURCE-LINE
   TEST-DIFF-RESET s" lib/type/deftype.f" TEST-ADD-SOURCE-SECTION
   1 TEST-EXPECT-FINDINGS
   TEST-SOURCE-RESET
   s" : STRUCTURE ( -- ) ;" TEST-SOURCE-LINE
   s" : UNRELATED ( -- ) ;" TEST-SOURCE-LINE
   TEST-DIFF-RESET s" src/core/structure-decl.f" TEST-ADD-SOURCE-SECTION
   1 TEST-EXPECT-FINDINGS
   \ ENUM carries the same single-name exception in its own front-end file: the
   \ keyword itself is exempt, a second global beside it is not, and the name is
   \ exempt only in that file.
   TEST-SOURCE-RESET
   s" : ENUM ( -- ) ;" TEST-SOURCE-LINE
   s" : UNRELATED ( -- ) ;" TEST-SOURCE-LINE
   TEST-DIFF-RESET s" src/core/enum-decl.f" TEST-ADD-SOURCE-SECTION
   1 TEST-EXPECT-FINDINGS
   TEST-SOURCE-RESET
   s" : ENUM ( -- ) ;" TEST-SOURCE-LINE
   TEST-DIFF-RESET s" src/core/structure-decl.f" TEST-ADD-SOURCE-SECTION
   1 TEST-EXPECT-FINDINGS
   s" NEARBY-GLOBAL" TEST-GLOBAL-SOURCE
   TEST-DIFF-RESET s" src/core/enum-decl.f" TEST-ADD-SOURCE-SECTION
   1 TEST-EXPECT-FINDINGS ;

: TEST-OPTION-SOURCE ( ptr u8 n -- )
   TEST-SOURCE-RESET
   s" ENUM " TEST-SOURCE+ TEST-SOURCE+ s"  1" TEST-SOURCE-LINE
   s"   VARIANT none ;VARIANT" TEST-SOURCE-LINE
   s"   VARIANT some FIELD value a ;VARIANT" TEST-SOURCE-LINE
   s" ;ENUM" TEST-SOURCE-LINE ;

: TEST-OPTION-GLOBAL ( -- )
   s" option" TEST-OPTION-SOURCE
   TEST-DIFF-RESET s" lib/adt/option.f" TEST-ADD-SOURCE-SECTION
   s" OPTION is admitted only at its standard-library path" T-LABEL
   TEST-EXPECT-CLEAN
   s" optional" TEST-OPTION-SOURCE
   TEST-DIFF-RESET s" lib/adt/option.f" TEST-ADD-SOURCE-SECTION
   s" the OPTION path does not admit another ENUM" T-LABEL
   1 TEST-EXPECT-FINDINGS
   s" option" TEST-OPTION-SOURCE
   TEST-DIFF-RESET s" lib/adt/option-neighbor.f" TEST-ADD-SOURCE-SECTION
   s" a neighboring path does not inherit OPTION admission" T-LABEL
   1 TEST-EXPECT-FINDINGS
   TEST-SOURCE-RESET
   s" SUMTYPE option 1" TEST-SOURCE-LINE
   s"   VARIANT none ;VARIANT" TEST-SOURCE-LINE
   s"   VARIANT some a ;VARIANT" TEST-SOURCE-LINE
   s" ;SUMTYPE" TEST-SOURCE-LINE
   TEST-DIFF-RESET s" lib/adt/option.f" TEST-ADD-SOURCE-SECTION
   s" only the unified OPTION declaration is admitted" T-LABEL
   1 TEST-EXPECT-FINDINGS ;

: TEST-RESULT+ ( ptr u8 n -- )
   s" ENUM " TEST-SOURCE+ TEST-SOURCE+ s"  2" TEST-SOURCE-LINE
   s"   VARIANT ok  FIELD value a ;VARIANT" TEST-SOURCE-LINE
   s"   VARIANT err FIELD error b ;VARIANT" TEST-SOURCE-LINE
   s" ;ENUM" TEST-SOURCE-LINE ;

: TEST-RESULT-LEGACY+ ( -- )
   s" SUMTYPE result 2" TEST-SOURCE-LINE
   s"   VARIANT ok a ;VARIANT" TEST-SOURCE-LINE
   s"   VARIANT err b ;VARIANT" TEST-SOURCE-LINE
   s" ;SUMTYPE" TEST-SOURCE-LINE ;

: TEST-RESULT-SOURCE ( ptr u8 n -- )
   TEST-SOURCE-RESET TEST-RESULT+ ;

: TEST-RESULT-WRAPPED ( -- )
   TEST-SOURCE-RESET
   s" package BOX" TEST-SOURCE-LINE
   s" result" TEST-RESULT+
   s" ;package" TEST-SOURCE-LINE
   TEST-DIFF-RESET s" lib/adt/result.f" TEST-ADD-SOURCE-SECTION
   s" RESULT cannot move behind a package owner" T-LABEL
   1 TEST-EXPECT-FINDINGS
   TEST-SOURCE-RESET
   s" package BOX" TEST-SOURCE-LINE
   s" outcome" TEST-RESULT+
   s" ;package" TEST-SOURCE-LINE
   TEST-DIFF-RESET s" lib/adt/result.f" TEST-ADD-SOURCE-SECTION
   s" another package-owned family is outside the RESULT exception" T-LABEL
   TEST-EXPECT-CLEAN ;

: TEST-RESULT-GLOBAL ( -- )
   TEST-RESULT-WRAPPED
   TEST-SOURCE-RESET
   TEST-RESULT-LEGACY+
   TEST-DIFF-RESET s" lib/adt/result.f" TEST-ADD-SOURCE-SECTION
   s" legacy RESULT is not the canonical global declaration" T-LABEL
   1 TEST-EXPECT-FINDINGS
   s" outcome" TEST-RESULT-SOURCE
   TEST-DIFF-RESET s" lib/adt/result.f" TEST-ADD-SOURCE-SECTION
   s" the RESULT path does not admit another family" T-LABEL
   1 TEST-EXPECT-FINDINGS
   s" result" TEST-RESULT-SOURCE
   TEST-DIFF-RESET s" lib/adt/result-neighbor.f" TEST-ADD-SOURCE-SECTION
   s" a neighboring path does not inherit RESULT admission" T-LABEL
   1 TEST-EXPECT-FINDINGS
   s" result" TEST-RESULT-SOURCE
   TEST-DIFF-RESET s" lib/adt/result.f" TEST-ADD-SOURCE-SECTION
   s" RESULT is admitted only at its standard-library path" T-LABEL
   TEST-EXPECT-CLEAN ;

\ ---- declaration-grammar fixture suites -------------------------------------
\ The second principled category.  These files test the global declaration
\ grammar, so a global DECLARATION is the thing under test and packaging it
\ would delete the proof rather than satisfy the rule.  Admission needs both
\ halves: the path must be on the exact list, and the definition's own definer
\ token must be one of the declaration openers measured from those suites.
\ The pins below fix the list, fix the opener set, and prove the category
\ cannot spread in either direction - a test file that is not listed still
\ reports, no path outside test/ can ever be admitted, and an ordinary colon
\ word, variable, constant or create standing beside the fixtures in a LISTED
\ file reports like any other unpackaged global.

: TEST-FIXTURE-FAMILY-SOURCE ( ptr u8 n -- )   \ a top-level family declaration
   TEST-SOURCE-RESET
   s" NEWTYPE " TEST-SOURCE+ TEST-SOURCE+ s"  0" TEST-SOURCE-LINE ;

: TEST-FIXTURE-AT ( ptr u8 n -- ) {: path:ptr pathu:n :}
   s" pdlfam" TEST-FIXTURE-FAMILY-SOURCE
   TEST-DIFF-RESET path pathu TEST-ADD-SOURCE-SECTION ;

: TEST-GRAMMAR-FIXTURE-LIST ( -- )   \ every listed path is admitted
   s" every listed grammar-fixture suite admits a declaration" T-LABEL
   TEST-DIFF-RESET
   s" pdlfam" TEST-FIXTURE-FAMILY-SOURCE
   s" test/type-decl-suite.f" TEST-ADD-SOURCE-SECTION
   s" test/extent-substrate-probe.f" TEST-ADD-SOURCE-SECTION
   s" test/extent-product-test.f" TEST-ADD-SOURCE-SECTION
   s" test/typed-storage-test.f" TEST-ADD-SOURCE-SECTION
   s" test/cast-suite.f" TEST-ADD-SOURCE-SECTION
   s" test/cast-negative-suite.f" TEST-ADD-SOURCE-SECTION
   s" test/layout-buffer.f" TEST-ADD-SOURCE-SECTION
   s" test/layout-defer.f" TEST-ADD-SOURCE-SECTION
   s" test/engine-suite.f" TEST-ADD-SOURCE-SECTION
   TEST-EXPECT-CLEAN ;

\ ---- the path-and-opener matrix ----------------------------------------------
\ The admission rule is per path, so the evidence has to be per path too.  The
\ three tables below are the TEST's own statement of the measurement, written in
\ the test's own opener order and as literal masks rather than by reusing the
\ lint's O-* bits, so a wrong edit to the lint's table cannot quietly agree with
\ itself.  Every one of the 9 x 13 pairs is then run through the real lint.

: TEST-PATH$ ( n -- ptr u8 n ) {: p:n :}
   p 0 = if s" test/type-decl-suite.f" exit then
   p 1 = if s" test/extent-substrate-probe.f" exit then
   p 2 = if s" test/extent-product-test.f" exit then
   p 3 = if s" test/typed-storage-test.f" exit then
   p 4 = if s" test/cast-suite.f" exit then
   p 5 = if s" test/cast-negative-suite.f" exit then
   p 6 = if s" test/layout-buffer.f" exit then
   p 7 = if s" test/layout-defer.f" exit then
   s" test/engine-suite.f" ;

: TEST-OPENER$ ( n -- ptr u8 n ) {: o:n :}
   o 0 = if s" SUMTYPE" exit then
   o 1 = if s" PRODUCT" exit then
   o 2 = if s" ENUM" exit then
   o 3 = if s" VALUE-RECORD" exit then
   o 4 = if s" NEWTYPE" exit then
   o 5 = if s" DEFTYPE" exit then
   o 6 = if s" DEFLINEAR" exit then
   o 7 = if s" CAST:" exit then
   o 8 = if s" LAYOUT-BUFFER" exit then
   o 9 = if s" DEFER-LAYOUT-BUFFER" exit then
   o 10 = if s" TYPED-VARIABLE" exit then
   o 11 = if s" TYPED-BUFFER" exit then
   o 12 = if s" PTR-VARIABLE" exit then
   s" TRUSTED:" ;

\ One complete top-level declaration per opener, closer included where the form
\ needs one, so every cell presents a real definition rather than a fragment.
: TEST-OPENER-LINE ( n -- ) {: o:n :}
   o 0 = if s" SUMTYPE pdlx 0 ;SUMTYPE" TEST-SOURCE-LINE exit then
   o 1 = if s" PRODUCT pdlx 0 ;PRODUCT" TEST-SOURCE-LINE exit then
   o 2 = if s" ENUM pdlx item ;ENUM" TEST-SOURCE-LINE exit then
   o 3 = if s" VALUE-RECORD pdlx q n END-VALUE-RECORD" TEST-SOURCE-LINE exit then
   o 4 = if s" NEWTYPE pdlx 0" TEST-SOURCE-LINE exit then
   o 5 = if s" DEFTYPE pdlx" TEST-SOURCE-LINE exit then
   o 6 = if s" DEFLINEAR pdlx" TEST-SOURCE-LINE exit then
   o 7 = if s" CAST: >PDLX ( n -- pdlx )" TEST-SOURCE-LINE exit then
   o 8 = if s" LAYOUT-BUFFER pdlx" TEST-SOURCE-LINE exit then
   o 9 = if s" DEFER-LAYOUT-BUFFER pdlx" TEST-SOURCE-LINE exit then
   o 10 = if s" TYPED-VARIABLE pdlx" TEST-SOURCE-LINE exit then
   o 11 = if s" TYPED-BUFFER pdlx" TEST-SOURCE-LINE exit then
   o 12 = if s" PTR-VARIABLE pdlx" TEST-SOURCE-LINE exit then
   s" TRUSTED: PDLX ( -- n ) 1 ;" TEST-SOURCE-LINE ;

\ Bit i of a row means opener i above.  Each comment spells the row out, so the
\ mask and the prose have to be edited together to move a pin.
: TEST-PATH-MASK ( n -- n ) {: p:n :}
   \ SUMTYPE PRODUCT ENUM VALUE-RECORD NEWTYPE DEFLINEAR LAYOUT-BUFFER
   p 0 = if $015F exit then
   \ NEWTYPE
   p 1 = if $0010 exit then
   \ NEWTYPE
   p 2 = if $0010 exit then
   \ SUMTYPE NEWTYPE DEFLINEAR LAYOUT-BUFFER TYPED-VARIABLE TYPED-BUFFER
   \ PTR-VARIABLE
   p 3 = if $1D51 exit then
   \ NEWTYPE CAST:
   p 4 = if $0090 exit then
   \ NEWTYPE
   p 5 = if $0010 exit then
   \ SUMTYPE PRODUCT ENUM NEWTYPE DEFLINEAR LAYOUT-BUFFER PTR-VARIABLE
   p 6 = if $1157 exit then
   \ SUMTYPE NEWTYPE DEFLINEAR DEFER-LAYOUT-BUFFER
   p 7 = if $0251 exit then
   \ VALUE-RECORD NEWTYPE DEFTYPE DEFLINEAR LAYOUT-BUFFER TRUSTED:
   $2178 ;

: TEST-ALLOWED? ( n n -- bool ) {: p:n o:n :}
   p TEST-PATH-MASK  1 o lshift and  0<> ;

\ One lint run against an expected finding count.  Zero means the run must come
\ back clean; anything else means it must come back rejected with that count.
: TEST-RUN-EXPECT? ( n -- bool ) {: want:n :}
   TEST-RUN-BUILT {: rc:n bad:n :}
   want 0 = if rc 0 = bad 0 = and exit then
   rc 1 = bad want = and ;

: TEST-CELL-NOTE ( n n -- ) {: p:n o:n :}
   TEST-CELL-BAD @ 1+ TEST-CELL-BAD !
   s" matrix cell wrong: " type p TEST-PATH$ type
   s"  declared with " type o TEST-OPENER$ type cr ;

: TEST-CELL ( n n -- ) {: p:n o:n :}
   TEST-SOURCE-RESET o TEST-OPENER-LINE
   TEST-DIFF-RESET p TEST-PATH$ TEST-ADD-SOURCE-SECTION
   p o TEST-ALLOWED? if
      0 TEST-RUN-EXPECT? if TEST-CELL-OK @ 1+ TEST-CELL-OK ! exit then
      p o TEST-CELL-NOTE exit
   then
   1 TEST-RUN-EXPECT? if exit then
   p o TEST-CELL-NOTE ;

: TEST-CELL-ROW ( n -- ) {: p:n :}
   0 begin dup TEST-OPENER# < while
      p over TEST-CELL
      1+
   repeat drop ;

: TEST-OPENER-MATRIX ( -- )
   0 TEST-CELL-BAD ! 0 TEST-CELL-OK !
   0 begin dup TEST-PATH# < while
      dup TEST-CELL-ROW
      1+
   repeat drop
   s" each path admits exactly the openers its own fixtures declare with" T-LABEL
   TEST-CELL-BAD @ 0 T=
   s" the nine rows admit 36 of the 126 path-and-opener pairs" T-LABEL
   TEST-CELL-OK @ 36 T= ;

\ The narrowing itself.  Each of these four stands in a listed file beside real
\ fixtures, and every one of them reports in every row.  Under the file-wide
\ admission this replaced, all four passed.
\
\ TRUSTED: IS NOT ONE OF THEM ANY MORE, and belongs in the matrix above instead,
\ exactly where CAST: sits: it now carries an opener bit, so "is it admitted" is
\ a per-path question and a blanket "every row rejects it" would be false by
\ construction.  The matrix answers it at all nine paths - admitted at
\ test/engine-suite.f alone, reported at the other eight.  The plain `:` stays
\ here, and its presence at every path is what keeps the engine-suite row from
\ reading as a file-wide exemption.
: TEST-ADD-ORDINARY-GLOBALS ( -- )
   s" : PDL-HELPER ( -- n ) 1 ;" TEST-SOURCE-LINE
   s" variable PDL-STATE" TEST-SOURCE-LINE
   s" constant PDL-LIMIT" TEST-SOURCE-LINE
   s" create PDL-CELL" TEST-SOURCE-LINE ;

\ The effect declarer, kept separate because it is admitted in ONE grammar-fixture
\ row and in no stage0 row at all.  A caller that wants it says so.
: TEST-ADD-TRUSTED-GLOBAL ( -- )
   s" TRUSTED: PDL-WRAPPER ( -- n ) 2 ;" TEST-SOURCE-LINE ;

: TEST-OPENER-CASE-AT ( ptr u8 n -- ) {: opener:ptr openeru:n :}
   TEST-SOURCE-RESET
   opener openeru TEST-SOURCE+ s"  pdlcase 0" TEST-SOURCE-LINE
   TEST-DIFF-RESET s" test/type-decl-suite.f" TEST-ADD-SOURCE-SECTION ;

\ The definer match is case-insensitive, like every other definer test in this
\ lint: three spellings of one Forth word must behave alike.  The last case
\ holds the other direction - case-folding must not turn a non-opener into one.
: TEST-GRAMMAR-FIXTURE-CASE ( -- )
   s" an upper-case opener is admitted in a listed file" T-LABEL
   s" NEWTYPE" TEST-OPENER-CASE-AT
   TEST-EXPECT-CLEAN
   s" the lower-case spelling is the same word and is admitted" T-LABEL
   s" newtype" TEST-OPENER-CASE-AT
   TEST-EXPECT-CLEAN
   s" the mixed-case spelling is the same word and is admitted" T-LABEL
   s" NeWtYpE" TEST-OPENER-CASE-AT
   TEST-EXPECT-CLEAN
   s" a case-varied ordinary definer is still not an opener" T-LABEL
   TEST-SOURCE-RESET
   s" VaRiAbLe PDL-MIXED" TEST-SOURCE-LINE
   TEST-DIFF-RESET s" test/type-decl-suite.f" TEST-ADD-SOURCE-SECTION
   1 TEST-EXPECT-FINDINGS
   s" PDL-MIXED" TEST-NAMES ;

\ Declaration keywords that are only text.  A `( )` comment body, a backslash
\ line comment and a string body each carry an admitted opener AND an ordinary
\ definer.  None of the six is a real definer, so the count is exactly one: the
\ colon word at the end.  The three `variable` lines are what makes a leak
\ visible - if comment or string bytes were scanned as words they would publish
\ globals with a NON-admitted definer and raise the count, whereas a leaked
\ `NEWTYPE` would be admitted and leave no trace.
: TEST-ADD-FORGED-OPENER-TEXT ( -- )
   s" ( NEWTYPE pdlfa 0 )" TEST-SOURCE-LINE
   s" ( variable pdlfb )" TEST-SOURCE-LINE
   TEST-BACKSLASH-C TEST-SOURCE-C s"  NEWTYPE pdlfc 0" TEST-SOURCE-LINE
   TEST-BACKSLASH-C TEST-SOURCE-C s"  variable pdlfd" TEST-SOURCE-LINE
   s" s" TEST-SOURCE+ TEST-DQUOTE-C TEST-SOURCE-C
   s" NEWTYPE pdlfe 0" TEST-SOURCE+ TEST-DQUOTE-C TEST-SOURCE-C
   s"  drop" TEST-SOURCE-LINE
   s" s" TEST-SOURCE+ TEST-DQUOTE-C TEST-SOURCE-C
   s" variable pdlff" TEST-SOURCE+ TEST-DQUOTE-C TEST-SOURCE-C
   s"  drop" TEST-SOURCE-LINE
   s" : PDL-AFTER-FORGERY ( -- n ) 1 ;" TEST-SOURCE-LINE ;

\ ---- the per-path rejection sweep --------------------------------------------
\ These eight hold for every row, so they run on every row.  A representative
\ path would leave eight rows unproven, and looping the table costs a second.
\
\ The four path variants loop too, and that is not belt-and-braces.  Each row
\ calls the comparator itself, so the comparator is a per-row decision: weaken
\ ONE row from LINT-STR= to LINT-ENDS-WITH? and only that row starts admitting
\ test/lib/<name>, while a hostile written against one other path stays green
\ and reports nothing.  A per-row weakening needs a per-row kill, so every row
\ gets the subdirectory, nested, case-varied and outside-test/ shapes.

\ NEWTYPE is on all nine rows, so the deleted-boundary case can use it
\ everywhere and stay a genuine "admitted opener, boundary removed" case.
: TEST-LOSS-SRC ( ptr u8 n -- ) {: path:ptr pathu:n :}
   TEST-SOURCE-RESET
   s" NEWTYPE pdlfam 0" TEST-SOURCE-LINE
   path pathu TEST-WRITE-SOURCE ;

: TEST-LOSS-DIFF ( ptr u8 n -- ) {: path:ptr pathu:n :}
   path pathu TEST-MODIFY-HEAD
   s" @@ -1,3 +1 @@" TEST-DIFF+ TEST-LF
   s" -package PDLFIX" TEST-DIFF+ TEST-LF
   s"  NEWTYPE pdlfam 0" TEST-DIFF+ TEST-LF
   s" -;package" TEST-DIFF+ TEST-LF ;

\ Every listed path starts `test/`, so the file name is what follows it.
: TEST-TAIL$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   u TEST-PREFIX-U <= if E-FS-CAPACITY throw then
   a TEST-PREFIX-U + u TEST-PREFIX-U - ;

\ The same file name under a different directory.
: TEST-REDIR$ ( ptr u8 n ptr u8 n -- ptr u8 n ) {: dir:ptr diru:n a:ptr u:n :}
   a u TEST-TAIL$ {: tail:ptr tailu:n :}
   diru tailu + FS-PATH-CAP > if E-FS-CAPACITY throw then
   dir TEST-ALT-BUF diru BYTE-COPY
   tail TEST-ALT-BUF diru + tailu BYTE-COPY
   TEST-ALT-BUF diru tailu + ;

\ The same path with the first letter of the file name in the other case.  A row
\ that compared case-insensitively would still admit this one.
: TEST-RECASE$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   u FS-PATH-CAP > if E-FS-CAPACITY throw then
   a TEST-ALT-BUF u BYTE-COPY
   TEST-ALT-BUF TEST-PREFIX-U + c@ TEST-CASE-DELTA -
   TEST-ALT-BUF TEST-PREFIX-U + c!
   TEST-ALT-BUF u ;

\ One family declaration at a path that must not be the listed one.
: TEST-VARIANT-ROW? ( ptr u8 n -- bool ) {: path:ptr pathu:n :}
   s" pdlfam" TEST-FIXTURE-FAMILY-SOURCE
   TEST-DIFF-RESET path pathu TEST-ADD-SOURCE-SECTION
   1 TEST-RUN-EXPECT? ;

: TEST-ORDINARY-ROW? ( n -- bool ) {: p:n :}
   TEST-SOURCE-RESET TEST-ADD-ORDINARY-GLOBALS
   TEST-DIFF-RESET p TEST-PATH$ TEST-ADD-SOURCE-SECTION
   4 TEST-RUN-EXPECT? ;

: TEST-STRUCTURE-ROW? ( n -- bool ) {: p:n :}
   TEST-SOURCE-RESET
   s" STRUCTURE pdlstruct 0 ;STRUCTURE" TEST-SOURCE-LINE
   TEST-DIFF-RESET p TEST-PATH$ TEST-ADD-SOURCE-SECTION
   1 TEST-RUN-EXPECT? ;

: TEST-FORGED-ROW? ( n -- bool ) {: p:n :}
   TEST-SOURCE-RESET TEST-ADD-FORGED-OPENER-TEXT
   TEST-DIFF-RESET p TEST-PATH$ TEST-ADD-SOURCE-SECTION
   1 TEST-RUN-EXPECT? ;

\ The listed file name moved into a subdirectory: what a comparison narrowed to
\ the file name alone, or to a path-segment suffix, would admit.
: TEST-SUBDIR-ROW? ( n -- bool ) {: p:n :}
   s" test/lib/" p TEST-PATH$ TEST-REDIR$ TEST-VARIANT-ROW? ;

\ The listed path nested under a second `test/`.  This one both starts with
\ `test/` and carries the WHOLE listed path as a trailing byte run, so it is the
\ shape that the comparator weakened from equality to LINT-ENDS-WITH? admits.
\ test/lib/<name> does not do that job: `lib/<name>` is not `test/<name>`, so a
\ byte-suffix comparison rejects it and the weakening would survive.
: TEST-NESTED-ROW? ( n -- bool ) {: p:n :}
   s" test/test/" p TEST-PATH$ TEST-REDIR$ TEST-VARIANT-ROW? ;

: TEST-RECASE-ROW? ( n -- bool ) {: p:n :}
   p TEST-PATH$ TEST-RECASE$ TEST-VARIANT-ROW? ;

: TEST-MOVED-ROW? ( n -- bool ) {: p:n :}
   s" tools/" p TEST-PATH$ TEST-REDIR$ TEST-VARIANT-ROW? ;

: TEST-BOUNDARY-ROW? ( n -- bool ) {: p:n :}
   p TEST-PATH$ TEST-LOSS-SRC
   TEST-DIFF-RESET p TEST-PATH$ TEST-LOSS-DIFF
   1 TEST-RUN-EXPECT? ;

: TEST-ROW-NOTE ( n ptr u8 n -- ) {: p:n what:ptr whatu:n :}
   s" row check wrong: " type p TEST-PATH$ type
   s"  on " type what whatu type cr ;

: TEST-ROW-CHECKS ( n -- n ) {: p:n :}
   0
   p TEST-ORDINARY-ROW? 0= if p s" ordinary globals" TEST-ROW-NOTE 1+ then
   p TEST-STRUCTURE-ROW? 0= if p s" a STRUCTURE declaration" TEST-ROW-NOTE 1+ then
   p TEST-FORGED-ROW? 0= if p s" forged comment and string text" TEST-ROW-NOTE 1+ then
   p TEST-SUBDIR-ROW? 0= if p s" the test/lib/ subdirectory variant" TEST-ROW-NOTE 1+ then
   p TEST-NESTED-ROW? 0= if p s" the nested test/test/ variant" TEST-ROW-NOTE 1+ then
   p TEST-RECASE-ROW? 0= if p s" the case-varied path variant" TEST-ROW-NOTE 1+ then
   p TEST-MOVED-ROW? 0= if p s" the tools/ directory variant" TEST-ROW-NOTE 1+ then
   p TEST-BOUNDARY-ROW? 0= if p s" a deleted package boundary" TEST-ROW-NOTE 1+ then ;

: TEST-PATH-ROWS ( -- )
   0 TEST-ROW-BAD !
   0 begin dup TEST-PATH# < while
      dup TEST-ROW-CHECKS TEST-ROW-BAD @ + TEST-ROW-BAD !
      1+
   repeat drop
   s" every row rejects ordinary globals, STRUCTURE, forged text, a changed path and a lost boundary" T-LABEL
   TEST-ROW-BAD @ 0 T= ;

\ ---- the production table itself ---------------------------------------------
\ Everything above tests BEHAVIOUR at nine paths this suite names.  That can
\ never see a row this suite does not name: append one more ROW+ line after the
\ production block and a tenth exemption exists which no behavioural pin visits.
\ So the table is pinned directly - its length against the expected row count,
\ and every row's path and opener set against this suite's own literals.  The
\ test-side masks are deliberately written in the same bit order as the O-*
\ constants so they can be compared as numbers, and they are written as literals
\ so a change on either side has to be mirrored on the other.
: TEST-TABLE-ROW? ( n -- bool ) {: p:n :}
   p ROW-PATH$ p TEST-PATH$ LINT-STR= 0= if false exit then
   p ROW-MASK p TEST-PATH-MASK = ;

: TEST-TABLE ( -- )
   s" the production row table holds exactly the rows this suite pins" T-LABEL
   FIXTURE-ROW# @ TEST-PATH# T=
   0 TEST-ROW-BAD !
   0 begin dup TEST-PATH# < while
      dup TEST-TABLE-ROW? 0= if
         dup s" its production path or opener set" TEST-ROW-NOTE
         TEST-ROW-BAD @ 1+ TEST-ROW-BAD !
      then
      1+
   repeat drop
   s" every production row carries this suite's own path and opener set" T-LABEL
   TEST-ROW-BAD @ 0 T= ;

\ The sweeps above count findings; they cannot say WHICH word a finding named.
\ This spot-check does, on one row, for the two fixtures where naming the wrong
\ subject would be the interesting failure.
: TEST-FIX-NAMES ( -- )
   s" the ordinary-global report names each ordinary global" T-LABEL
   TEST-SOURCE-RESET TEST-ADD-ORDINARY-GLOBALS
   TEST-DIFF-RESET s" test/type-decl-suite.f" TEST-ADD-SOURCE-SECTION
   4 TEST-EXPECT-FINDINGS
   s" PDL-HELPER" TEST-NAMES
   s" PDL-STATE" TEST-NAMES
   s" PDL-CELL" TEST-NAMES
   s" the forged-text report names the colon word, never the forged names" T-LABEL
   TEST-SOURCE-RESET TEST-ADD-FORGED-OPENER-TEXT
   TEST-DIFF-RESET s" test/type-decl-suite.f" TEST-ADD-SOURCE-SECTION
   1 TEST-EXPECT-FINDINGS
   s" PDL-AFTER-FORGERY" TEST-NAMES
   s" pdlfb" TEST-NOT-NAMES
   s" pdlfd" TEST-NOT-NAMES
   s" pdlff" TEST-NOT-NAMES ;

: TEST-GRAMMAR-FIXTURE-HOSTILE ( -- )
   s" an unlisted test file still reports its global declaration" T-LABEL
   s" test/not-a-grammar-fixture.f" TEST-FIXTURE-AT
   1 TEST-EXPECT-FINDINGS
   s" a listed basename under another directory is not the listed path" T-LABEL
   s" tools/type-decl-suite.f" TEST-FIXTURE-AT
   1 TEST-EXPECT-FINDINGS
   s" a listed path as a suffix of a longer path is not the listed path" T-LABEL
   s" test/lib/type-decl-suite.f" TEST-FIXTURE-AT
   1 TEST-EXPECT-FINDINGS
   \ the path list matches case-sensitively while the definer list does not.
   \ That asymmetry is deliberate - a path is a filename, a definer is a Forth
   \ word - so it needs its own pin rather than a reader's assumption.
   s" a listed path spelled in another case is not the listed path" T-LABEL
   s" test/Type-Decl-Suite.f" TEST-FIXTURE-AT
   1 TEST-EXPECT-FINDINGS
   s" the category never admits a library source, however it is spelled" T-LABEL
   s" lib/process-pty-handle.f" TEST-FIXTURE-AT
   1 TEST-EXPECT-FINDINGS
   s" a global colon word in an unlisted test file still reports" T-LABEL
   s" TEST-LEAK" TEST-GLOBAL-SOURCE
   TEST-DIFF-RESET s" test/other-suite.f" TEST-ADD-SOURCE-SECTION
   1 TEST-EXPECT-FINDINGS ;

\ test/internal-word-gate.f was on the path list and was removed: measured
\ against the shape rule it declares nothing through the grammar - its fixtures
\ are declaration SOURCE built as strings for child processes - so the entry
\ only ever admitted its raw-stem IWG- helpers, which are ordinary packaging
\ debt (dot habu-pkg-internal-word-da4149d9).  Pin the removal, or a future
\ reader restores the entry from the old comment.
: TEST-GRAMMAR-FIXTURE-REMOVED ( -- )
   s" test/internal-word-gate.f is no longer listed and now reports" T-LABEL
   s" test/internal-word-gate.f" TEST-FIXTURE-AT
   1 TEST-EXPECT-FINDINGS
   s" pdlfam" TEST-NAMES ;

\ ---- stage0 recovery fixtures -------------------------------------------------
\ The fourth principled category, and the narrowest path list in the lint.  A
\ stage0 recovery fixture is a source tools/bootstrap.sh hands to Gforth, which
\ compiles it with the recovery emitter into a standalone binary the script then
\ runs and compares whole-stream against exact expected output.  Two of those
\ fixtures need a word at genuine global top level - one so the emitter's bare
\ 13-byte `checker-using` lookup can find the stand-in hook, one because
\ top-level bare visibility around `using` is the property under test - so a
\ package there would delete the proof instead of satisfying the rule.
\
\ The pins below fix the row list, fix the admitted shape, and prove the category
\ cannot spread in either direction.  Admission needs three things: the exact
\ path, a path under test/ ending in -src.f, and the plain lower-case `:`
\ definer.  The last two constrain a FUTURE row rather than today's two paths, so
\ they are pinned by hostile paths that a weakened comparison would let through.
2 constant TEST-STAGE0-PATH#     \ rows in the stage0 fixture path table

: TEST-STAGE0-PATH$ ( n -- ptr u8 n ) {: p:n :}
   p 0 = if s" test/bootstrap-using-src.f" exit then
   s" test/bootstrap-using-checker-hook-src.f" ;

: TEST-STAGE0-COLON-AT ( ptr u8 n -- ) {: path:ptr pathu:n :}
   s" PDL-STAGE0" TEST-GLOBAL-SOURCE
   TEST-DIFF-RESET path pathu TEST-ADD-SOURCE-SECTION ;

\ The production row table is pinned directly, its length against the expected
\ count and every row's path against this suite's own literal, so a wrong edit on
\ either side cannot quietly agree with itself.
: TEST-STAGE0-ROW? ( n -- bool ) {: p:n :}
   p STAGE0-PATH$ p TEST-STAGE0-PATH$ LINT-STR= ;

: TEST-STAGE0-TABLE ( -- )
   s" the stage0 row table holds exactly the rows this suite pins" T-LABEL
   STAGE0-ROW# @ TEST-STAGE0-PATH# T=
   0 TEST-ROW-BAD !
   0 begin dup TEST-STAGE0-PATH# < while
      dup TEST-STAGE0-ROW? 0= if TEST-ROW-BAD @ 1+ TEST-ROW-BAD ! then
      1+
   repeat drop
   s" every stage0 row carries this suite's own path" T-LABEL
   TEST-ROW-BAD @ 0 T= ;

: TEST-STAGE0-LIST ( -- )
   s" both listed stage0 fixtures admit a global colon word" T-LABEL
   TEST-DIFF-RESET
   s" PDL-STAGE0" TEST-GLOBAL-SOURCE
   0 TEST-STAGE0-PATH$ TEST-ADD-SOURCE-SECTION
   1 TEST-STAGE0-PATH$ TEST-ADD-SOURCE-SECTION
   TEST-EXPECT-CLEAN ;

\ The narrowing.  Five globals stand in a LISTED stage0 fixture and exactly one
\ of them - the plain colon word - is admitted; the other four report by name.
\ Under a file-wide admission all five would pass.  TRUSTED: is spelled out here
\ rather than taken from the ordinary set because this is the row that has to
\ prove a stage0 fixture never inherits it: it is an admitted opener in one
\ grammar-fixture row, and this category admits the plain `:` and nothing else.
: TEST-STAGE0-NARROW ( -- )
   s" only the plain colon word is admitted in a listed fixture" T-LABEL
   TEST-SOURCE-RESET TEST-ADD-ORDINARY-GLOBALS TEST-ADD-TRUSTED-GLOBAL
   TEST-DIFF-RESET 0 TEST-STAGE0-PATH$ TEST-ADD-SOURCE-SECTION
   4 TEST-EXPECT-FINDINGS
   s" PDL-WRAPPER" TEST-NAMES
   s" PDL-STATE" TEST-NAMES
   s" PDL-LIMIT" TEST-NAMES
   s" PDL-CELL" TEST-NAMES
   s" PDL-HELPER" TEST-NOT-NAMES
   s" a CHECKED: global in a listed fixture is not a plain colon word" T-LABEL
   TEST-SOURCE-RESET
   s" CHECKED: PDL-CHECKED ( -- n ) 1 ;" TEST-SOURCE-LINE
   TEST-DIFF-RESET 1 TEST-STAGE0-PATH$ TEST-ADD-SOURCE-SECTION
   1 TEST-EXPECT-FINDINGS
   s" PDL-CHECKED" TEST-NAMES
   s" a stage0 row does not inherit the grammar category's openers" T-LABEL
   s" pdlfam" TEST-FIXTURE-FAMILY-SOURCE
   TEST-DIFF-RESET 0 TEST-STAGE0-PATH$ TEST-ADD-SOURCE-SECTION
   1 TEST-EXPECT-FINDINGS
   s" pdlfam" TEST-NAMES ;

: TEST-STAGE0-HOSTILE ( -- )
   s" an unlisted fixture of the same family still reports its global" T-LABEL
   s" test/bootstrap-using-scope-src.f" TEST-STAGE0-COLON-AT
   1 TEST-EXPECT-FINDINGS
   s" PDL-STAGE0" TEST-NAMES
   s" a listed basename under another directory is not the listed path" T-LABEL
   s" tools/bootstrap-using-src.f" TEST-STAGE0-COLON-AT
   1 TEST-EXPECT-FINDINGS
   s" a listed basename in a deeper directory is not the listed path" T-LABEL
   s" test/lib/bootstrap-using-src.f" TEST-STAGE0-COLON-AT
   1 TEST-EXPECT-FINDINGS
   \ These two carry the listed path as a whole trailing SUFFIX, which is what a
   \ suffix or basename comparison would accept.  The nested one satisfies both
   \ narrowing guards, so only the exact comparison keeps it reported; the second
   \ is what the `test/` guard alone refuses once that comparison is weakened.
   s" a listed path nested under a repeated test/ is not the listed path" T-LABEL
   s" test/test/bootstrap-using-src.f" TEST-STAGE0-COLON-AT
   1 TEST-EXPECT-FINDINGS
   s" a path ending in the listed path from outside test/ is not admitted" T-LABEL
   s" tools/test/bootstrap-using-src.f" TEST-STAGE0-COLON-AT
   1 TEST-EXPECT-FINDINGS
   \ The path list matches case-sensitively while the definer list does not: a
   \ path is a filename, a definer is a Forth word.  This hostile varies case only
   \ BETWEEN the guarded `test/` head and the guarded `-src.f` tail, because those
   \ two guards compare case-sensitively themselves and would otherwise refuse the
   \ path for the wrong reason and hide a case-folded row comparison.
   s" a listed path spelled in another case is not the listed path" T-LABEL
   s" test/Bootstrap-Using-src.f" TEST-STAGE0-COLON-AT
   1 TEST-EXPECT-FINDINGS
   s" a listed path extended past its -src.f tail is not the listed path" T-LABEL
   s" test/bootstrap-using-src.fs" TEST-STAGE0-COLON-AT
   1 TEST-EXPECT-FINDINGS
   \ The other direction: a path that BEGINS with a listed path and still satisfies
   \ both narrowing guards, so only the exact comparison keeps it reported.
   s" a listed path extended into a longer -src.f name is not admitted" T-LABEL
   s" test/bootstrap-using-src.f-more-src.f" TEST-STAGE0-COLON-AT
   1 TEST-EXPECT-FINDINGS
   s" the category never admits a library source, however it is spelled" T-LABEL
   s" lib/bootstrap-using-src.f" TEST-STAGE0-COLON-AT
   1 TEST-EXPECT-FINDINGS
   s" a global colon word in an ordinary test file still reports" T-LABEL
   s" test/bootstrap-plain.f" TEST-STAGE0-COLON-AT
   1 TEST-EXPECT-FINDINGS ;

: TEST-STAGE0-FIXTURES ( -- )
   TEST-STAGE0-TABLE
   TEST-STAGE0-LIST
   TEST-STAGE0-NARROW
   TEST-STAGE0-HOSTILE ;

: TEST-GRAMMAR-FIXTURES ( -- )
   TEST-TABLE
   TEST-GRAMMAR-FIXTURE-LIST
   TEST-OPENER-MATRIX
   TEST-PATH-ROWS
   TEST-FIX-NAMES
   TEST-GRAMMAR-FIXTURE-CASE
   TEST-GRAMMAR-FIXTURE-HOSTILE
   TEST-GRAMMAR-FIXTURE-REMOVED ;

: TEST-WRITE-TYPE-FAMILY-BODY ( ptr u8 n -- ) {: path:ptr pathu:n :}
   TEST-SOURCE-RESET
   s" : TF-STR-CAP ( -- n )" TEST-SOURCE-LINE
   s"    TF-STR-CAP-V @" TEST-SOURCE-LINE
   s" ;" TEST-SOURCE-LINE
   path pathu TEST-WRITE-SOURCE ;

: TEST-TYPE-FAMILY-BODY-DIFF ( ptr u8 n -- ) {: path:ptr pathu:n :}
   path pathu TEST-MODIFY-HEAD
   s" @@ -1,2 +1,3 @@" TEST-DIFF+ TEST-LF
   s"  : TF-STR-CAP ( -- n )" TEST-DIFF+ TEST-LF
   s" +   TF-STR-CAP-V @" TEST-DIFF+ TEST-LF
   s"  ;" TEST-DIFF+ TEST-LF ;

: TEST-TYPE-FAMILY-CASE ( ptr u8 n -- ) {: path:ptr pathu:n :}
   path pathu TEST-WRITE-TYPE-FAMILY-BODY
   TEST-DIFF-RESET path pathu TEST-TYPE-FAMILY-BODY-DIFF ;

: TEST-WRITE-TYPE-FAMILY-OWNER-LOSS ( -- )
   TEST-SOURCE-RESET
   s" : TFAM-N@ ( -- n ) 0 ;" TEST-SOURCE-LINE
   s" src/core/type-family.f" TEST-WRITE-SOURCE ;

: TEST-TYPE-FAMILY-OWNER-LOSS-DIFF ( -- )
   s" src/core/type-family.f" TEST-MODIFY-HEAD
   s" @@ -1,3 +1 @@" TEST-DIFF+ TEST-LF
   s" -package TYPE-NAME" TEST-DIFF+ TEST-LF
   s"  : TFAM-N@ ( -- n ) 0 ;" TEST-DIFF+ TEST-LF
   s" -;package" TEST-DIFF+ TEST-LF ;

: TEST-TYPE-FAMILY-EXEMPTION ( -- )
   \ Positive: a changed body line of a global word in the documented core
   \ type-family surface is exempt from package ownership.
   s" src/core/type-family.f" TEST-TYPE-FAMILY-CASE
   s" type-family core surface exempts a changed global body" T-LABEL
   TEST-EXPECT-CLEAN
   \ Negative: a sibling name in the same directory shares the allowlist path as
   \ a prefix but is not exact, so it must still fail (not a startswith match).
   s" src/core/type-family-extra.f" TEST-TYPE-FAMILY-CASE
   s" sibling src/core/type-family-extra.f still fails ownership" T-LABEL
   1 TEST-EXPECT-FINDINGS
   \ Negative: the same basename in another directory shares the allowlist path
   \ as a suffix but is not exact, so it must still fail (full path, not suffix).
   s" lib/type-family.f" TEST-TYPE-FAMILY-CASE
   s" lib/type-family.f basename collision still fails ownership" T-LABEL
   1 TEST-EXPECT-FINDINGS
   \ Negative: an ordinary non-allowlisted lib file still fails: the exemption
   \ was added for exactly one path and must not widen.
   s" lib/string.f" TEST-TYPE-FAMILY-CASE
   s" non-allowlisted lib/string.f still fails ownership" T-LABEL
   1 TEST-EXPECT-FINDINGS
   \ Negative (structural): deleting a package/;package boundary inside the
   \ allowlisted src/core/type-family.f still reports lost ownership.  The
   \ exemption suppresses a plain global body change, never a scope change.
   TEST-WRITE-TYPE-FAMILY-OWNER-LOSS
   TEST-DIFF-RESET TEST-TYPE-FAMILY-OWNER-LOSS-DIFF
   s" deleted package boundary in type-family.f still fails ownership" T-LABEL
   1 TEST-EXPECT-FINDINGS ;

: TEST-WRITE-CHECKER-COMMENT-BODY ( ptr u8 n -- ) {: path:ptr pathu:n :}
   TEST-SOURCE-RESET
   s" : CHECK-RESET ( -- n )" TEST-SOURCE-LINE
   s"    TOKBUF-ENSURE   \ gate probe comment" TEST-SOURCE-LINE
   s" ;" TEST-SOURCE-LINE
   path pathu TEST-WRITE-SOURCE ;

: TEST-CHECKER-COMMENT-DIFF ( ptr u8 n -- ) {: path:ptr pathu:n :}
   path pathu TEST-MODIFY-HEAD
   s" @@ -1,3 +1,3 @@" TEST-DIFF+ TEST-LF
   s"  : CHECK-RESET ( -- n )" TEST-DIFF+ TEST-LF
   s" -   TOKBUF-ENSURE" TEST-DIFF+ TEST-LF
   s" +   TOKBUF-ENSURE   \ gate probe comment" TEST-DIFF+ TEST-LF
   s"  ;" TEST-DIFF+ TEST-LF ;

: TEST-CHECKER-COMMENT-CASE ( ptr u8 n -- ) {: path:ptr pathu:n :}
   path pathu TEST-WRITE-CHECKER-COMMENT-BODY
   TEST-DIFF-RESET path pathu TEST-CHECKER-COMMENT-DIFF ;

: TEST-CHECKER-NEW-GLOBAL-CASE ( ptr u8 n -- ) {: path:ptr pathu:n :}
   TEST-DIFF-RESET
   s" CHECKER-NEW-GLOBAL" TEST-GLOBAL-SOURCE
   path pathu TEST-ADD-SOURCE-SECTION ;

: TEST-WRITE-CHECKER-OWNER-LOSS ( -- )
   TEST-SOURCE-RESET
   s" : RBF-POP ( -- ) ;" TEST-SOURCE-LINE
   s" src/core/checker.f" TEST-WRITE-SOURCE ;

: TEST-CHECKER-OWNER-LOSS-DIFF ( -- )
   s" src/core/checker.f" TEST-MODIFY-HEAD
   s" @@ -1,3 +1 @@" TEST-DIFF+ TEST-LF
   s" -package CHECKER-FRAME" TEST-DIFF+ TEST-LF
   s"  : RBF-POP ( -- ) ;" TEST-DIFF+ TEST-LF
   s" -;package" TEST-DIFF+ TEST-LF ;

: TEST-CHECKER-EXEMPTION ( -- )
   \ Positive: the control probe that proved the gate used to reject every
   \ possible checker change -- one trailing comment on an existing global body,
   \ defining no new word and changing no behavior -- is now admitted.
   s" src/core/checker.f" TEST-CHECKER-COMMENT-CASE
   s" checker core surface exempts a comment-only global body change" T-LABEL
   TEST-EXPECT-CLEAN
   \ Positive: a new global word in the checker is admitted too; the pre-hook
   \ axiom and rollback-frame surface grows by adding global words.
   s" src/core/checker.f" TEST-CHECKER-NEW-GLOBAL-CASE
   s" checker core surface exempts a new global definition" T-LABEL
   TEST-EXPECT-CLEAN
   \ Negative: the very same new-global diff on a neighbouring core file still
   \ fails.  The exemption is one exact path, not a hole and not a directory.
   s" src/core/check-hook.f" TEST-CHECKER-NEW-GLOBAL-CASE
   s" nearby core src/core/check-hook.f still fails ownership" T-LABEL
   1 TEST-EXPECT-FINDINGS
   \ Negative: a sibling whose name carries the allowlist path as a prefix is not
   \ an exact match, so it must still fail (not a startswith match).
   s" src/core/checker-extra.f" TEST-CHECKER-NEW-GLOBAL-CASE
   s" sibling src/core/checker-extra.f still fails ownership" T-LABEL
   1 TEST-EXPECT-FINDINGS
   \ Negative: the same basename in another directory carries the allowlist path
   \ as a suffix but is not exact, so it must still fail (full path, not suffix).
   s" lib/checker.f" TEST-CHECKER-NEW-GLOBAL-CASE
   s" lib/checker.f basename collision still fails ownership" T-LABEL
   1 TEST-EXPECT-FINDINGS
   \ Negative (structural): deleting a package/;package boundary inside the
   \ allowlisted src/core/checker.f still reports lost ownership, exactly as it
   \ does for the other exempt files.  The exemption suppresses a plain global
   \ body or definition change, never a scope change.
   TEST-WRITE-CHECKER-OWNER-LOSS
   TEST-DIFF-RESET TEST-CHECKER-OWNER-LOSS-DIFF
   s" deleted package boundary in checker.f still fails ownership" T-LABEL
   1 TEST-EXPECT-FINDINGS ;

: TEST-WRITE-RENDER-OWNER-LOSS ( -- )
   TEST-SOURCE-RESET
   s" : FAM-QNAME-REND ( n -- ) drop ;" TEST-SOURCE-LINE
   s" src/core/render.f" TEST-WRITE-SOURCE ;

: TEST-RENDER-OWNER-LOSS-DIFF ( -- )
   s" src/core/render.f" TEST-MODIFY-HEAD
   s" @@ -1,3 +1 @@" TEST-DIFF+ TEST-LF
   s" -package RENDER-DIAG" TEST-DIFF+ TEST-LF
   s"  : FAM-QNAME-REND ( n -- ) drop ;" TEST-DIFF+ TEST-LF
   s" -;package" TEST-DIFF+ TEST-LF ;

: TEST-RENDER-EXEMPTION ( -- )
   \ Positive: the renderer is the checker's other half and is admitted the same
   \ way.  Before this entry a comment-only change to an existing global body in
   \ render.f red the gate, so no change to the diagnostic renderer could land.
   s" src/core/render.f" TEST-CHECKER-COMMENT-CASE
   s" render core surface exempts a comment-only global body change" T-LABEL
   TEST-EXPECT-CLEAN
   \ Positive: a new global word in the renderer is admitted too (RFOLD, the
   \ case-folding emitter, is exactly this shape).
   s" src/core/render.f" TEST-CHECKER-NEW-GLOBAL-CASE
   s" render core surface exempts a new global definition" T-LABEL
   TEST-EXPECT-CLEAN
   \ Negative: a sibling carrying the allowlist path as a prefix is not an exact
   \ match and must still fail (not a startswith match).
   s" src/core/render-extra.f" TEST-CHECKER-NEW-GLOBAL-CASE
   s" sibling src/core/render-extra.f still fails ownership" T-LABEL
   1 TEST-EXPECT-FINDINGS
   \ Negative: the same basename elsewhere carries the allowlist path as a suffix
   \ but is not exact, so it must still fail (full path, not suffix).
   s" lib/render.f" TEST-CHECKER-NEW-GLOBAL-CASE
   s" lib/render.f basename collision still fails ownership" T-LABEL
   1 TEST-EXPECT-FINDINGS
   \ Negative (structural): deleting a package/;package boundary inside the
   \ allowlisted render.f still reports lost ownership.  The exemption suppresses
   \ a plain global body or definition change, never a scope change.
   TEST-WRITE-RENDER-OWNER-LOSS
   TEST-DIFF-RESET TEST-RENDER-OWNER-LOSS-DIFF
   s" deleted package boundary in render.f still fails ownership" T-LABEL
   1 TEST-EXPECT-FINDINGS ;

\ ---- src/arch/arm64/{icode,mnem}.f: the ARM64 encoder prefix ----
\ Same category and same fixtures as the checker and render entries: the two
\ files carry no package at all, they load ahead of the compiler that defines
\ packages, and their names are resolved bare by every later engine source. The
\ positive is the shape that measured the problem - adding the operand bounds to
\ the encoders - and the negatives pin the match as an exact path and keep every
\ scope change reported. src/arch/arm64/asm.f is the entry's RETIRED third
\ member: dot habu-pkg-the-arm64-ce972795 made it package A64ASM, so it appears
\ here only as a negative, and the two cases that used to be its positives are
\ the ones that would fail if the row came back.

: TEST-WRITE-ARM64-OWNER-LOSS ( -- )
   TEST-SOURCE-RESET
   s" : ENC-RET ( -- n ) 3596550080 ;" TEST-SOURCE-LINE
   s" src/arch/arm64/asm.f" TEST-WRITE-SOURCE ;

: TEST-ARM64-OWNER-LOSS-DIFF ( -- )
   s" src/arch/arm64/asm.f" TEST-MODIFY-HEAD
   s" @@ -1,3 +1 @@" TEST-DIFF+ TEST-LF
   s" -package A64ASM" TEST-DIFF+ TEST-LF
   s"  : ENC-RET ( -- n ) 3596550080 ;" TEST-DIFF+ TEST-LF
   s" -;package" TEST-DIFF+ TEST-LF ;

: TEST-ARM64-EXEMPTION ( -- )
   \ Positive: a body change to an existing global encoder is admitted, for both
   \ files still in the entry, because the whole surface of each is global by
   \ construction.
   s" src/arch/arm64/icode.f" TEST-CHECKER-COMMENT-CASE
   s" the ARM64 encoder prefix exempts a body change in icode.f" T-LABEL
   TEST-EXPECT-CLEAN
   s" src/arch/arm64/mnem.f" TEST-CHECKER-COMMENT-CASE
   s" the ARM64 encoder prefix exempts a body change in mnem.f" T-LABEL
   TEST-EXPECT-CLEAN
   \ Positive: a new global word is admitted too. This is the exact shape of the
   \ operand bounds - ?REG, ?IMM12, SCALE/ - added beside the encoders.
   s" src/arch/arm64/icode.f" TEST-CHECKER-NEW-GLOBAL-CASE
   s" the ARM64 encoder prefix exempts a new global bound in icode.f" T-LABEL
   TEST-EXPECT-CLEAN
   \ Negative: src/arch/arm64/asm.f is package A64ASM now and left the entry, so
   \ the two shapes that used to be its positives report again. This is what
   \ makes the retirement enforced rather than merely announced.
   s" src/arch/arm64/asm.f" TEST-CHECKER-COMMENT-CASE
   s" packaged src/arch/arm64/asm.f no longer exempts a global body change" T-LABEL
   1 TEST-EXPECT-FINDINGS
   s" src/arch/arm64/asm.f" TEST-CHECKER-NEW-GLOBAL-CASE
   s" packaged src/arch/arm64/asm.f no longer exempts a new global" T-LABEL
   1 TEST-EXPECT-FINDINGS
   \ Negative: a sibling carrying the allowlist path as a prefix is not an exact
   \ match and must still fail.
   s" src/arch/arm64/icode-extra.f" TEST-CHECKER-NEW-GLOBAL-CASE
   s" sibling src/arch/arm64/icode-extra.f still fails ownership" T-LABEL
   1 TEST-EXPECT-FINDINGS
   \ Negative: the same basename in another directory carries the allowlist path
   \ as a suffix but is not exact, so it must still fail.
   s" lib/icode.f" TEST-CHECKER-NEW-GLOBAL-CASE
   s" lib/icode.f basename collision still fails ownership" T-LABEL
   1 TEST-EXPECT-FINDINGS
   \ Negative: the disassembler shares the directory and is NOT in the entry, so
   \ a new global there is still reported - the category is two named files, not
   \ the src/arch/arm64 tree.
   s" src/arch/arm64/disasm.f" TEST-CHECKER-NEW-GLOBAL-CASE
   s" src/arch/arm64/disasm.f still fails ownership" T-LABEL
   1 TEST-EXPECT-FINDINGS
   \ Negative (structural): deleting a package boundary inside an allowlisted
   \ file still reports lost ownership. The entry suppresses a plain global body
   \ or definition change, never a scope change.
   TEST-WRITE-ARM64-OWNER-LOSS
   TEST-DIFF-RESET TEST-ARM64-OWNER-LOSS-DIFF
   s" deleted package boundary in asm.f still fails ownership" T-LABEL
   1 TEST-EXPECT-FINDINGS ;

\ ---- src/core/internal-mark.f: the self-sealing marking pass ----
\ The positive is the exact probe that measured the problem on pristine master:
\ one trailing comment on an existing global body reported
\ `E-PACKAGE-OWNERSHIP src/core/internal-mark.f:98:3`, so no change to the
\ seal-time marking pass could land at all -- not even the repair of the rc 134
\ abort that found it. The second positive is the new global word, which the
\ entry must admit because closing the qualified-name escape added six.
\ The negatives are the whole point of the pairing: the row must admit this one
\ path and nothing that merely looks like it. A neighbour in the same directory,
\ a sibling carrying the path as a prefix, the same basename elsewhere, and any
\ scope change inside the admitted file itself all still report.

: TEST-WRITE-INTMARK-OWNER-LOSS ( -- )
   TEST-SOURCE-RESET
   s" : IMK-PASS ( -- ) ;" TEST-SOURCE-LINE
   s" src/core/internal-mark.f" TEST-WRITE-SOURCE ;

: TEST-INTMARK-OWNER-LOSS-DIFF ( -- )
   s" src/core/internal-mark.f" TEST-MODIFY-HEAD
   s" @@ -1,3 +1 @@" TEST-DIFF+ TEST-LF
   s" -package INTERNAL-MARK" TEST-DIFF+ TEST-LF
   s"  : IMK-PASS ( -- ) ;" TEST-DIFF+ TEST-LF
   s" -;package" TEST-DIFF+ TEST-LF ;

: TEST-INTMARK-EXEMPTION ( -- )
   \ Positive: the control probe that proved the gate rejected every possible
   \ change to the marking pass -- one trailing comment on an existing global
   \ body, defining no new word and changing no behaviour -- is now admitted.
   s" src/core/internal-mark.f" TEST-CHECKER-COMMENT-CASE
   s" the marking pass exempts a comment-only global body change" T-LABEL
   TEST-EXPECT-CLEAN
   \ Positive: a new global word is admitted too. This is the exact shape the
   \ qualified-name walk needed -- IMK-QUAL, IMK-PKG-PUBLICS, IMK-WALK-PACKAGES.
   s" src/core/internal-mark.f" TEST-CHECKER-NEW-GLOBAL-CASE
   s" the marking pass exempts a new global definition" T-LABEL
   TEST-EXPECT-CLEAN
   \ Negative: a sibling carrying the allowlist path as a prefix is not an exact
   \ match and must still fail (not a startswith match).
   s" src/core/internal-mark-extra.f" TEST-CHECKER-NEW-GLOBAL-CASE
   s" sibling src/core/internal-mark-extra.f still fails ownership" T-LABEL
   1 TEST-EXPECT-FINDINGS
   \ Negative: the same basename in another directory carries the allowlist path
   \ as a suffix but is not exact, so it must still fail (full path, not suffix).
   s" lib/internal-mark.f" TEST-CHECKER-NEW-GLOBAL-CASE
   s" lib/internal-mark.f basename collision still fails ownership" T-LABEL
   1 TEST-EXPECT-FINDINGS
   \ Negative: the marking pass's own regression neighbour is NOT in the entry,
   \ so a new global there is still reported - the category is one named file,
   \ not the src/core tree and not everything the dot touched.
   s" src/core/top-row.f" TEST-CHECKER-NEW-GLOBAL-CASE
   s" src/core/top-row.f still fails ownership" T-LABEL
   1 TEST-EXPECT-FINDINGS
   \ Negative (structural): once the file DOES open a package - the retirement
   \ this entry is waiting on - deleting that boundary still reports lost
   \ ownership. The entry suppresses a plain global body or definition change,
   \ never a scope change.
   TEST-WRITE-INTMARK-OWNER-LOSS
   TEST-DIFF-RESET TEST-INTMARK-OWNER-LOSS-DIFF
   s" deleted package boundary in internal-mark.f still fails ownership" T-LABEL
   1 TEST-EXPECT-FINDINGS ;

\ ---- src/os/{image-bytes,linux/elf,macos/macho}.f: the image writer ----
\ Same category and same fixtures as the ARM64 encoder prefix: none of the three
\ carries a package, and every later prefix source resolves their names bare. The
\ positive is the shape that measured the problem - changing the number in
\ `$220000 constant MSIZE` - and the negatives pin the match as an exact path,
\ keep the sibling that shares each directory reported, and keep every scope
\ change reported.

: TEST-WRITE-IMAGE-OWNER-LOSS ( -- )
   TEST-SOURCE-RESET
   s" : MLEN@ ( -- n ) 0 ;" TEST-SOURCE-LINE
   s" src/os/image-bytes.f" TEST-WRITE-SOURCE ;

: TEST-IMAGE-OWNER-LOSS-DIFF ( -- )
   s" src/os/image-bytes.f" TEST-MODIFY-HEAD
   s" @@ -1,3 +1 @@" TEST-DIFF+ TEST-LF
   s" -package IMAGE-BYTES" TEST-DIFF+ TEST-LF
   s"  : MLEN@ ( -- n ) 0 ;" TEST-DIFF+ TEST-LF
   s" -;package" TEST-DIFF+ TEST-LF ;

: TEST-IMAGE-EXEMPTION ( -- )
   \ Positive: a body change to an existing global is admitted in all three,
   \ because the whole surface of each is global by construction.
   s" src/os/image-bytes.f" TEST-CHECKER-COMMENT-CASE
   s" the image writer exempts a body change in image-bytes.f" T-LABEL
   TEST-EXPECT-CLEAN
   s" src/os/linux/elf.f" TEST-CHECKER-COMMENT-CASE
   s" the image writer exempts a body change in elf.f" T-LABEL
   TEST-EXPECT-CLEAN
   s" src/os/macos/macho.f" TEST-CHECKER-COMMENT-CASE
   s" the image writer exempts a body change in macho.f" T-LABEL
   TEST-EXPECT-CLEAN
   \ Positive: a new global is admitted too, the way it is for the encoder prefix.
   s" src/os/macos/macho.f" TEST-CHECKER-NEW-GLOBAL-CASE
   s" the image writer exempts a new global in macho.f" T-LABEL
   TEST-EXPECT-CLEAN
   \ Negative: a sibling carrying an allowlist path as a prefix is not an exact
   \ match and must still fail.
   s" src/os/image-bytes-extra.f" TEST-CHECKER-NEW-GLOBAL-CASE
   s" sibling src/os/image-bytes-extra.f still fails ownership" T-LABEL
   1 TEST-EXPECT-FINDINGS
   \ Negative: the same basename elsewhere carries the allowlist path as a suffix
   \ but is not exact, so it must still fail.
   s" lib/macho.f" TEST-CHECKER-NEW-GLOBAL-CASE
   s" lib/macho.f basename collision still fails ownership" T-LABEL
   1 TEST-EXPECT-FINDINGS
   \ Negative: the signer shares macho.f's directory and is NOT in the entry, so a
   \ new global there is still reported - the category is three named files, not
   \ the src/os tree.
   s" src/os/macos/sign2.f" TEST-CHECKER-NEW-GLOBAL-CASE
   s" src/os/macos/sign2.f still fails ownership" T-LABEL
   1 TEST-EXPECT-FINDINGS
   \ Negative (structural): deleting a package boundary inside an allowlisted file
   \ still reports lost ownership. The entry suppresses a plain global body or
   \ definition change, never a scope change.
   TEST-WRITE-IMAGE-OWNER-LOSS
   TEST-DIFF-RESET TEST-IMAGE-OWNER-LOSS-DIFF
   s" deleted package boundary in image-bytes.f still fails ownership" T-LABEL
   1 TEST-EXPECT-FINDINGS ;

\ ---- src/habu/habu2.f: the engine emitter's body-edit admission ----
\ This is the narrowest of the three principled categories, so its fixtures pin
\ BOTH halves: the one shape it admits, and the shape it must keep reporting.
\ The positive is the exact probe that measured the problem -- one trailing
\ comment on an existing global body -- and the first negative is the new global
\ word, which the checker and render entries DO admit and this one must not.

: TEST-WRITE-ENGINE-BODY ( ptr u8 n -- ) {: path:ptr pathu:n :}
   TEST-SOURCE-RESET
   s" : EM-SNAPSHOT-RX-FLUSH ( -- )" TEST-SOURCE-LINE
   s"    LPROT-RX   \ gate probe comment" TEST-SOURCE-LINE
   s" ;" TEST-SOURCE-LINE
   path pathu TEST-WRITE-SOURCE ;

: TEST-ENGINE-BODY-DIFF ( ptr u8 n -- ) {: path:ptr pathu:n :}
   path pathu TEST-MODIFY-HEAD
   s" @@ -1,3 +1,3 @@" TEST-DIFF+ TEST-LF
   s"  : EM-SNAPSHOT-RX-FLUSH ( -- )" TEST-DIFF+ TEST-LF
   s" -   LPROT-RX" TEST-DIFF+ TEST-LF
   s" +   LPROT-RX   \ gate probe comment" TEST-DIFF+ TEST-LF
   s"  ;" TEST-DIFF+ TEST-LF ;

: TEST-ENGINE-BODY-CASE ( ptr u8 n -- ) {: path:ptr pathu:n :}
   path pathu TEST-WRITE-ENGINE-BODY
   TEST-DIFF-RESET path pathu TEST-ENGINE-BODY-DIFF ;

: TEST-WRITE-ENGINE-NEW-GLOBAL ( ptr u8 n -- ) {: path:ptr pathu:n :}
   TEST-SOURCE-RESET
   s" : EM-SNAPSHOT-RX-FLUSH ( -- ) LPROT-RX ;" TEST-SOURCE-LINE
   s" : EM-SMUGGLED ( -- n ) 1 ;" TEST-SOURCE-LINE
   path pathu TEST-WRITE-SOURCE ;

: TEST-ENGINE-NEW-GLOBAL-DIFF ( ptr u8 n -- ) {: path:ptr pathu:n :}
   path pathu TEST-MODIFY-HEAD
   s" @@ -1 +1,2 @@" TEST-DIFF+ TEST-LF
   s"  : EM-SNAPSHOT-RX-FLUSH ( -- ) LPROT-RX ;" TEST-DIFF+ TEST-LF
   s" +: EM-SMUGGLED ( -- n ) 1 ;" TEST-DIFF+ TEST-LF ;

: TEST-ENGINE-NEW-GLOBAL-CASE ( ptr u8 n -- ) {: path:ptr pathu:n :}
   path pathu TEST-WRITE-ENGINE-NEW-GLOBAL
   TEST-DIFF-RESET path pathu TEST-ENGINE-NEW-GLOBAL-DIFF ;

: TEST-WRITE-ENGINE-OWNER-LOSS ( -- )
   TEST-SOURCE-RESET
   s" : LOOP-EMIT-TAIL ( -- ) ;" TEST-SOURCE-LINE
   s" src/habu/habu2.f" TEST-WRITE-SOURCE ;

: TEST-ENGINE-OWNER-LOSS-DIFF ( -- )
   s" src/habu/habu2.f" TEST-MODIFY-HEAD
   s" @@ -1,3 +1 @@" TEST-DIFF+ TEST-LF
   s" -package LOOP-EMIT" TEST-DIFF+ TEST-LF
   s"  : LOOP-EMIT-TAIL ( -- ) ;" TEST-DIFF+ TEST-LF
   s" -;package" TEST-DIFF+ TEST-LF ;

: TEST-WRITE-ENGINE-ARRIVAL ( -- )
   TEST-SOURCE-RESET
   s" : EM-ARRIVED ( -- n ) 1 ;" TEST-SOURCE-LINE
   s" src/habu/habu2.f" TEST-WRITE-SOURCE ;

: TEST-ENGINE-ARRIVAL-DIFF ( -- )
   s" diff --git a/src/habu/engine-old.f b/src/habu/habu2.f" TEST-DIFF+ TEST-LF
   s" similarity index 100%" TEST-DIFF+ TEST-LF
   s" rename from src/habu/engine-old.f" TEST-DIFF+ TEST-LF
   s" rename to src/habu/habu2.f" TEST-DIFF+ TEST-LF ;

: TEST-ENGINE-EXEMPTION ( -- )
   \ Positive: the measured control probe.  Before this entry, one trailing
   \ comment on an existing global body in src/habu/habu2.f reported
   \ E-PACKAGE-OWNERSHIP, so no change at all to the native engine emitter could
   \ pass the commit gate.
   s" src/habu/habu2.f" TEST-ENGINE-BODY-CASE
   s" engine emitter exempts a comment-only global body change" T-LABEL
   TEST-EXPECT-CLEAN
   \ Negative: a genuinely new global word added beside an existing one is still
   \ reported.  This is where the engine entry is narrower than the checker and
   \ render entries, which admit new globals: habu2.f already carries real
   \ packages, so a new engine word has an owner to join.
   s" src/habu/habu2.f" TEST-ENGINE-NEW-GLOBAL-CASE
   s" new global in the engine emitter still fails ownership" T-LABEL
   1 TEST-EXPECT-FINDINGS
   \ Negative: a sibling whose name carries the allowlist path as a prefix is not
   \ an exact match, so its body edit must still fail (not a startswith match).
   s" src/habu/habu2-extra.f" TEST-ENGINE-BODY-CASE
   s" sibling src/habu/habu2-extra.f still fails ownership" T-LABEL
   1 TEST-EXPECT-FINDINGS
   \ Negative: the same basename in another directory carries the allowlist path
   \ as a suffix but is not exact, so it must still fail (full path, not suffix).
   s" lib/habu2.f" TEST-ENGINE-BODY-CASE
   s" lib/habu2.f basename collision still fails ownership" T-LABEL
   1 TEST-EXPECT-FINDINGS
   \ Negative (structural): deleting a package/;package boundary inside the
   \ allowlisted engine file still reports lost ownership.  FINISH-DEFINITION
   \ checks SCOPE-DELTA before it consults the admission, so an engine word
   \ pushed out of LOOP-EMIT is reported even though its own body line is
   \ unchanged.
   TEST-WRITE-ENGINE-OWNER-LOSS
   TEST-DIFF-RESET TEST-ENGINE-OWNER-LOSS-DIFF
   s" deleted package boundary in habu2.f still fails ownership" T-LABEL
   1 TEST-EXPECT-FINDINGS
   \ Negative (hostile): a pure rename that makes some OTHER file arrive at
   \ src/habu/habu2.f marks no line as added, so the added-head test alone would
   \ admit every global in it.  A whole-file change is not a body edit of an
   \ existing engine word and is reported.
   TEST-WRITE-ENGINE-ARRIVAL
   TEST-DIFF-RESET TEST-ENGINE-ARRIVAL-DIFF
   s" file renamed onto habu2.f still fails ownership" T-LABEL
   1 TEST-EXPECT-FINDINGS
   \ The second engine-trunk row, src/habu/layout.f, is pinned in both directions
   \ by the same fixtures, because it is admitted by the same rule and must be
   \ narrow in the same way.  The positive is the measured probe: changing the
   \ value of an existing layout constant reported E-PACKAGE-OWNERSHIP before
   \ this row, so the snapshot format version could not be bumped at all.
   s" src/habu/layout.f" TEST-ENGINE-BODY-CASE
   s" layout exempts a comment-only global body change" T-LABEL
   TEST-EXPECT-CLEAN
   \ Negative: a new unpackaged constant in layout.f is still reported by name,
   \ which is what keeps the row from becoming a licence to grow the file's
   \ global surface -- new layout bands have to open a package.
   s" src/habu/layout.f" TEST-ENGINE-NEW-GLOBAL-CASE
   s" new global in layout still fails ownership" T-LABEL
   1 TEST-EXPECT-FINDINGS
   \ Negative: a sibling carrying the row's path as a prefix is not an exact
   \ match, so its body edit still fails.
   s" src/habu/layout-extra.f" TEST-ENGINE-BODY-CASE
   s" sibling src/habu/layout-extra.f still fails ownership" T-LABEL
   1 TEST-EXPECT-FINDINGS
   \ Negative: the same basename in another directory carries the row's path as
   \ a suffix but is not exact, so it still fails.  The per-target layout files
   \ src/os/macos/layout.f and src/os/linux/layout.f are the real words this
   \ guards: they are NOT engine trunk and must keep reporting.
   s" lib/layout.f" TEST-ENGINE-BODY-CASE
   s" lib/layout.f basename collision still fails ownership" T-LABEL
   1 TEST-EXPECT-FINDINGS
   \ The third engine-trunk row, src/habu/xref.f, is pinned by the same fixtures
   \ in both directions.  The positive is its measured probe: adding the
   \ code-reclamation notice to the last line of the existing global
   \ FORGET-DEFS-FROM reported E-PACKAGE-OWNERSHIP before this row, so the
   \ dictionary-truncation path could not be changed at all.
   s" src/habu/xref.f" TEST-ENGINE-BODY-CASE
   s" xref exempts a comment-only global body change" T-LABEL
   TEST-EXPECT-CLEAN
   \ Negative: a new unpackaged word in xref.f is still reported.  The file
   \ already opens PKG-AUTH, GENERATED-DECL-NAME-PREFLIGHT and CODE-RECLAIM, so a
   \ new dictionary word has an owner it can join -- which is what keeps this row
   \ from becoming a licence to grow the file's global surface.
   s" src/habu/xref.f" TEST-ENGINE-NEW-GLOBAL-CASE
   s" new global in xref still fails ownership" T-LABEL
   1 TEST-EXPECT-FINDINGS
   \ Negative: a sibling carrying the row's path as a prefix is not an exact
   \ match, so its body edit still fails.
   s" src/habu/xref-extra.f" TEST-ENGINE-BODY-CASE
   s" sibling src/habu/xref-extra.f still fails ownership" T-LABEL
   1 TEST-EXPECT-FINDINGS
   \ Negative: the same basename in another directory carries the row's path as a
   \ suffix but is not exact, so it still fails.
   s" lib/xref.f" TEST-ENGINE-BODY-CASE
   s" lib/xref.f basename collision still fails ownership" T-LABEL
   1 TEST-EXPECT-FINDINGS
   \ The fourth engine-trunk row, src/habu/habu1.f, is pinned by the same
   \ fixtures in both directions.  The positive is its measured probe: adding a
   \ trailing comment to the body of the existing global EMIT-FLUSH reported
   \ E-PACKAGE-OWNERSHIP before this row, so the engine's primitive emitters
   \ could not be changed at all.
   s" src/habu/habu1.f" TEST-ENGINE-BODY-CASE
   s" habu1 exempts a comment-only global body change" T-LABEL
   TEST-EXPECT-CLEAN
   \ Negative: a new unpackaged word in habu1.f is still reported.  The file
   \ already opens ENGINE-BUILD, ENGINE-HELPER, GUARD, PROT-GUARD and
   \ ENGINE-EMIT, so a new emitter has an owner it can join.
   s" src/habu/habu1.f" TEST-ENGINE-NEW-GLOBAL-CASE
   s" new global in habu1 still fails ownership" T-LABEL
   1 TEST-EXPECT-FINDINGS
   \ Negative: a sibling carrying the row's path as a prefix is not an exact
   \ match, so its body edit still fails.
   s" src/habu/habu1-extra.f" TEST-ENGINE-BODY-CASE
   s" sibling src/habu/habu1-extra.f still fails ownership" T-LABEL
   1 TEST-EXPECT-FINDINGS
   \ Negative: the same basename in another directory carries the row's path as a
   \ suffix but is not exact, so it still fails.
   s" lib/habu1.f" TEST-ENGINE-BODY-CASE
   s" lib/habu1.f basename collision still fails ownership" T-LABEL
   1 TEST-EXPECT-FINDINGS
   \ The fifth engine-trunk row, src/habu/driver-io.f, is pinned by the same
   \ fixtures in both directions.  The positive is its measured probe: repointing
   \ the one body line of the existing global DRV-WRITE-IMAGE-PATH at the
   \ extracted FDIO:WALL reported E-PACKAGE-OWNERSHIP before this row, so the
   \ build drivers' shared I/O could not be changed at all -- and a booted engine
   \ cannot load the file to reach its write loop, which is what forced the
   \ extraction.
   s" src/habu/driver-io.f" TEST-ENGINE-BODY-CASE
   s" driver-io exempts a comment-only global body change" T-LABEL
   TEST-EXPECT-CLEAN
   \ Negative: a new unpackaged word in driver-io.f is still reported.  The file's
   \ successor package DRIVER-IO is already specified on dot
   \ habu-pkg-build-driver-acbd02b7, so a new driver word has an owner it can join
   \ -- which is what keeps this row from becoming a licence to grow the DRV-*
   \ global set while that dot is open.
   s" src/habu/driver-io.f" TEST-ENGINE-NEW-GLOBAL-CASE
   s" new global in driver-io still fails ownership" T-LABEL
   1 TEST-EXPECT-FINDINGS
   \ Negative: a sibling carrying the row's path as a prefix is not an exact
   \ match, so its body edit still fails.
   s" src/habu/driver-io-extra.f" TEST-ENGINE-BODY-CASE
   s" sibling src/habu/driver-io-extra.f still fails ownership" T-LABEL
   1 TEST-EXPECT-FINDINGS
   \ Negative: the same basename in another directory carries the row's path as a
   \ suffix but is not exact, so it still fails.
   s" lib/driver-io.f" TEST-ENGINE-BODY-CASE
   s" lib/driver-io.f basename collision still fails ownership" T-LABEL
   1 TEST-EXPECT-FINDINGS
   \ Negative (hostile), and it guards all five rows at once because they share
   \ one comparison site: a path that ENDS WITH a listed one.  The prose above
   \ claims a suffix weakening dies on these fixtures, and until this case existed
   \ it did not - swapping the row comparison for LINT-ENDS-WITH? left the whole
   \ suite green (measured 2026-08-15), because no other fixture path has a full
   \ row path as its tail.  test/src/habu/driver-io.f is not the build drivers'
   \ I/O and must keep reporting.
   s" test/src/habu/driver-io.f" TEST-ENGINE-BODY-CASE
   s" test/src/habu/driver-io.f suffix path still fails ownership" T-LABEL
   1 TEST-EXPECT-FINDINGS ;

\ ---- bootstrap/cg/forth.fs: the Gforth recovery mirror ------------------------
\ This is the one category where the ownership rule cannot be satisfied at all.
\ The file is compiled by Gforth, which has no `package` word, so writing one
\ would abort the no-binary recovery.  Both definition directions are therefore
\ admitted, and every negative below is about the PATH rather than about the shape
\ of the definition -- which is the exact opposite of the engine-trunk fixtures
\ above, where the shape is what decides.
\
\ The positive is the measured probe: adding one trailing comment to the existing
\ global BCOUNT reported `E-PACKAGE-OWNERSHIP bootstrap/cg/forth.fs:811:3` before
\ this row existed, so no change at all to the recovery emitter could pass the
\ commit gate.  The fixtures use the mirror's own idiom (bare register words and
\ an emitter body) so the source they lex is the kind of text the real file holds.

: TEST-WRITE-MIRROR-BODY ( ptr u8 n -- ) {: path:ptr pathu:n :}
   TEST-SOURCE-RESET
   s" : BCOUNT ( -- )" TEST-SOURCE-LINE
   s"    A G-POP  B A 0 LDRB   \ gate probe comment" TEST-SOURCE-LINE
   s" ;" TEST-SOURCE-LINE
   path pathu TEST-WRITE-SOURCE ;

: TEST-MIRROR-BODY-DIFF ( ptr u8 n -- ) {: path:ptr pathu:n :}
   path pathu TEST-MODIFY-HEAD
   s" @@ -1,3 +1,3 @@" TEST-DIFF+ TEST-LF
   s"  : BCOUNT ( -- )" TEST-DIFF+ TEST-LF
   s" -   A G-POP  B A 0 LDRB" TEST-DIFF+ TEST-LF
   s" +   A G-POP  B A 0 LDRB   \ gate probe comment" TEST-DIFF+ TEST-LF
   s"  ;" TEST-DIFF+ TEST-LF ;

: TEST-MIRROR-BODY-CASE ( ptr u8 n -- ) {: path:ptr pathu:n :}
   path pathu TEST-WRITE-MIRROR-BODY
   TEST-DIFF-RESET path pathu TEST-MIRROR-BODY-DIFF ;

\ The direction the engine trunk reports and this category admits: a brand-new
\ global word beside an existing one.  `BUSING` is the real shape the stage-0
\ `using` work adds -- a new keyword handler in the emitter.
: TEST-WRITE-MIRROR-NEW ( ptr u8 n -- ) {: path:ptr pathu:n :}
   TEST-SOURCE-RESET
   s" : BCOUNT ( -- ) A G-POP ;" TEST-SOURCE-LINE
   s" : BUSING ( -- ) A G-POP ;" TEST-SOURCE-LINE
   path pathu TEST-WRITE-SOURCE ;

: TEST-MIRROR-NEW-DIFF ( ptr u8 n -- ) {: path:ptr pathu:n :}
   path pathu TEST-MODIFY-HEAD
   s" @@ -1 +1,2 @@" TEST-DIFF+ TEST-LF
   s"  : BCOUNT ( -- ) A G-POP ;" TEST-DIFF+ TEST-LF
   s" +: BUSING ( -- ) A G-POP ;" TEST-DIFF+ TEST-LF ;

: TEST-MIRROR-NEW-CASE ( ptr u8 n -- ) {: path:ptr pathu:n :}
   path pathu TEST-WRITE-MIRROR-NEW
   TEST-DIFF-RESET path pathu TEST-MIRROR-NEW-DIFF ;

: TEST-WRITE-MIRROR-ARRIVAL ( -- )
   TEST-SOURCE-RESET
   s" : BARRIVED ( -- ) A G-POP ;" TEST-SOURCE-LINE
   s" bootstrap/cg/forth.fs" TEST-WRITE-SOURCE ;

: TEST-MIRROR-ARRIVAL-DIFF ( -- )
   s" diff --git a/bootstrap/cg/mirror-old.fs b/bootstrap/cg/forth.fs"
   TEST-DIFF+ TEST-LF
   s" similarity index 100%" TEST-DIFF+ TEST-LF
   s" rename from bootstrap/cg/mirror-old.fs" TEST-DIFF+ TEST-LF
   s" rename to bootstrap/cg/forth.fs" TEST-DIFF+ TEST-LF ;

\ Gforth would refuse this file, so the shape cannot occur in the real mirror.
\ It is pinned anyway: the admission must sit behind FINISH-DEFINITION's
\ SCOPE-DELTA test like every other category, so that a boundary appearing or
\ disappearing in the mirror is reported instead of quietly admitted.
: TEST-WRITE-MIRROR-OWNER-LOSS ( -- )
   TEST-SOURCE-RESET
   s" : BCOUNT ( -- ) A G-POP ;" TEST-SOURCE-LINE
   s" bootstrap/cg/forth.fs" TEST-WRITE-SOURCE ;

: TEST-MIRROR-OWNER-LOSS-DIFF ( -- )
   s" bootstrap/cg/forth.fs" TEST-MODIFY-HEAD
   s" @@ -1,3 +1 @@" TEST-DIFF+ TEST-LF
   s" -package BCG" TEST-DIFF+ TEST-LF
   s"  : BCOUNT ( -- ) A G-POP ;" TEST-DIFF+ TEST-LF
   s" -;package" TEST-DIFF+ TEST-LF ;

: TEST-MIRROR-ADMITTED ( -- )
   \ Positive: the measured control probe, a comment-only change to an existing
   \ global emitter body.
   s" bootstrap/cg/forth.fs" TEST-MIRROR-BODY-CASE
   s" the Gforth mirror exempts a comment-only global body change" T-LABEL
   TEST-EXPECT-CLEAN
   \ Positive, and the direction that separates this category from the engine
   \ trunk: a genuinely new global word is admitted too.  The trunk reports it
   \ because habu2.f and layout.f are compiled by `bin/hb` and already open real
   \ packages, so a new engine word has an owner to join.  The mirror has none and
   \ can have none while Gforth compiles it, so reporting here would report a
   \ fault whose only repair breaks the recovery path.
   s" bootstrap/cg/forth.fs" TEST-MIRROR-NEW-CASE
   s" the Gforth mirror exempts a new global definition too" T-LABEL
   TEST-EXPECT-CLEAN ;

: TEST-MIRROR-OTHER-PATHS ( -- )
   \ Negative: a sibling in the same directory whose stem carries the row's stem
   \ as a prefix is not an exact match and still fails.
   s" bootstrap/cg/forth-extra.fs" TEST-MIRROR-BODY-CASE
   s" sibling bootstrap/cg/forth-extra.fs still fails ownership" T-LABEL
   1 TEST-EXPECT-FINDINGS
   s" BCOUNT" TEST-NAMES
   \ Negative: the same basename one directory up shares only the last component
   \ and still fails.
   s" bootstrap/forth.fs" TEST-MIRROR-BODY-CASE
   s" bootstrap/forth.fs basename collision still fails ownership" T-LABEL
   1 TEST-EXPECT-FINDINGS
   s" BCOUNT" TEST-NAMES
   \ Negative, and the case a suffix comparison would wrongly admit: the whole
   \ row path repeated under test/.  Nothing but a whole-path comparison rejects
   \ this one, so it is the fixture that dies first if the comparison is loosened.
   s" test/bootstrap/cg/forth.fs" TEST-MIRROR-BODY-CASE
   s" test/bootstrap/cg/forth.fs path suffix still fails ownership" T-LABEL
   1 TEST-EXPECT-FINDINGS
   s" BCOUNT" TEST-NAMES
   \ Negative: the same stem with the habu extension.  The key is the exact path,
   \ never the `.fs` extension: a `.f` twin is compiled by `bin/hb`, can open a
   \ package, and must.
   s" bootstrap/cg/forth.f" TEST-MIRROR-BODY-CASE
   s" the .f twin bootstrap/cg/forth.f still fails ownership" T-LABEL
   1 TEST-EXPECT-FINDINGS
   s" BCOUNT" TEST-NAMES
   \ Negative: a differently-cased spelling of the row path.  A case-insensitive
   \ path comparison would admit it; file paths are bytes and this is a different
   \ file.
   s" bootstrap/cg/FORTH.fs" TEST-MIRROR-BODY-CASE
   s" bootstrap/cg/FORTH.fs case variant still fails ownership" T-LABEL
   1 TEST-EXPECT-FINDINGS
   s" BCOUNT" TEST-NAMES
   \ Negative: a sibling Gforth mirror source.  Every one of the 63 other
   \ Gforth-hosted `.fs` files in the tree was measured to report, and they are
   \ left reporting on purpose: a row is added only when a real change is blocked
   \ and that file's own compensating parity authority can be named.
   s" bootstrap/cg/jit.fs" TEST-MIRROR-NEW-CASE
   s" sibling mirror bootstrap/cg/jit.fs still fails ownership" T-LABEL
   1 TEST-EXPECT-FINDINGS
   s" BUSING" TEST-NAMES ;

: TEST-MIRROR-STRUCTURAL ( -- )
   \ Negative (hostile): a pure rename that makes some OTHER file arrive at the
   \ mirror path marks no line as added, so without WHOLE-CHANGED every global in
   \ the arriving file would ride in unread.  The mirror is one committed file
   \ whose content the parity gates know, and a wholesale replacement of it is
   \ exactly where that authority has not looked yet, so it is reported -- the
   \ same decision the engine trunk makes.
   TEST-WRITE-MIRROR-ARRIVAL
   TEST-DIFF-RESET TEST-MIRROR-ARRIVAL-DIFF
   s" a file renamed onto the mirror path still fails ownership" T-LABEL
   1 TEST-EXPECT-FINDINGS
   s" BARRIVED" TEST-NAMES
   \ Negative (structural): the admission sits behind SCOPE-DELTA, so a deleted
   \ package boundary is reported in the mirror like anywhere else.
   TEST-WRITE-MIRROR-OWNER-LOSS
   TEST-DIFF-RESET TEST-MIRROR-OWNER-LOSS-DIFF
   s" a deleted package boundary in the mirror still fails ownership" T-LABEL
   1 TEST-EXPECT-FINDINGS
   s" BCOUNT" TEST-NAMES ;

: TEST-MIRROR-EXEMPTION ( -- )
   TEST-MIRROR-ADMITTED
   TEST-MIRROR-OTHER-PATHS
   TEST-MIRROR-STRUCTURAL ;

\ ---- one-line definitions, where line position decides nothing ----------------
\ Every fixture above changes a colon word, whose head and body sit on different
\ lines.  A `constant` puts the definer, the name and the value on ONE line, so
\ every edit to it touches the head line and a value change is byte for byte the
\ same shape as a brand-new constant.  layout.f is nothing but constants, which
\ is why these are the cases the entry was minted for and the cases no rule about
\ WHICH LINES CHANGED can decide.  What decides them is whether the file already
\ defined the name, so each fixture below varies exactly that: same shape, same
\ line, one name that the pre-image had and one it did not.  The subject is read
\ back by name every time a finding is expected, because a count alone cannot
\ tell a report about the intended constant from one about its neighbour.

: TEST-WRITE-CONST-VALUE ( ptr u8 n -- ) {: path:ptr pathu:n :}
   TEST-SOURCE-RESET
   s" 5 constant SNAP-FORMAT-VERSION" TEST-SOURCE-LINE
   s" 0 constant DATA-START" TEST-SOURCE-LINE
   path pathu TEST-WRITE-SOURCE ;

: TEST-CONST-VALUE-DIFF ( ptr u8 n -- ) {: path:ptr pathu:n :}
   path pathu TEST-MODIFY-HEAD
   s" @@ -1,2 +1,2 @@" TEST-DIFF+ TEST-LF
   s" -4 constant SNAP-FORMAT-VERSION" TEST-DIFF+ TEST-LF
   s" +5 constant SNAP-FORMAT-VERSION" TEST-DIFF+ TEST-LF
   s"  0 constant DATA-START" TEST-DIFF+ TEST-LF ;

: TEST-CONST-VALUE-CASE ( ptr u8 n -- ) {: path:ptr pathu:n :}
   path pathu TEST-WRITE-CONST-VALUE
   TEST-DIFF-RESET path pathu TEST-CONST-VALUE-DIFF ;

: TEST-WRITE-CONST-NEW ( ptr u8 n -- ) {: path:ptr pathu:n :}
   TEST-SOURCE-RESET
   s" 4 constant SNAP-FORMAT-VERSION" TEST-SOURCE-LINE
   s" 64 constant SNAP-SMUGGLED-BAND" TEST-SOURCE-LINE
   path pathu TEST-WRITE-SOURCE ;

: TEST-CONST-NEW-DIFF ( ptr u8 n -- ) {: path:ptr pathu:n :}
   path pathu TEST-MODIFY-HEAD
   s" @@ -1 +1,2 @@" TEST-DIFF+ TEST-LF
   s"  4 constant SNAP-FORMAT-VERSION" TEST-DIFF+ TEST-LF
   s" +64 constant SNAP-SMUGGLED-BAND" TEST-DIFF+ TEST-LF ;

: TEST-CONST-NEW-CASE ( ptr u8 n -- ) {: path:ptr pathu:n :}
   path pathu TEST-WRITE-CONST-NEW
   TEST-DIFF-RESET path pathu TEST-CONST-NEW-DIFF ;

\ A rename in place: one constant leaves under its old name and arrives under a
\ new one, so the file's definition COUNT is unchanged and nothing but the name
\ itself distinguishes this from the value change above.
: TEST-WRITE-CONST-RENAME ( ptr u8 n -- ) {: path:ptr pathu:n :}
   TEST-SOURCE-RESET
   s" 4 constant SNAP-VERSION" TEST-SOURCE-LINE
   path pathu TEST-WRITE-SOURCE ;

: TEST-CONST-RENAME-DIFF ( ptr u8 n -- ) {: path:ptr pathu:n :}
   path pathu TEST-MODIFY-HEAD
   s" @@ -1 +1 @@" TEST-DIFF+ TEST-LF
   s" -4 constant SNAP-FORMAT-VERSION" TEST-DIFF+ TEST-LF
   s" +4 constant SNAP-VERSION" TEST-DIFF+ TEST-LF ;

: TEST-CONST-RENAME-CASE ( ptr u8 n -- ) {: path:ptr pathu:n :}
   path pathu TEST-WRITE-CONST-RENAME
   TEST-DIFF-RESET path pathu TEST-CONST-RENAME-DIFF ;

\ The name existed in the old file, but inside a package.  Moving it out to top
\ level grows the file's GLOBAL surface, which is the surface this entry guards,
\ so it must be reported even though the spelling is not new to the file.
: TEST-WRITE-CONST-PROMOTED ( -- )
   TEST-SOURCE-RESET
   s" 96 constant XTCELL-END" TEST-SOURCE-LINE
   s" package SNAP-RELOC" TEST-SOURCE-LINE
   s" 4 constant XTCELL-N" TEST-SOURCE-LINE
   s" ;package" TEST-SOURCE-LINE
   s" src/habu/layout.f" TEST-WRITE-SOURCE ;

: TEST-CONST-PROMOTED-DIFF ( -- )
   s" src/habu/layout.f" TEST-MODIFY-HEAD
   s" @@ -1,4 +1,4 @@" TEST-DIFF+ TEST-LF
   s" +96 constant XTCELL-END" TEST-DIFF+ TEST-LF
   s"  package SNAP-RELOC" TEST-DIFF+ TEST-LF
   s"  4 constant XTCELL-N" TEST-DIFF+ TEST-LF
   s" -96 constant XTCELL-END" TEST-DIFF+ TEST-LF
   s"  ;package" TEST-DIFF+ TEST-LF ;

: TEST-CONST-VALUE-DIRECTIONS ( -- )
   \ Positive: the measured case.  Bumping an existing layout constant's value
   \ reported E-PACKAGE-OWNERSHIP on SNAP-FORMAT-VERSION before the name key
   \ existed, so the snapshot format could not be versioned at all.
   s" src/habu/layout.f" TEST-CONST-VALUE-CASE
   s" layout exempts a value change to an existing constant" T-LABEL
   TEST-EXPECT-CLEAN
   \ The same shape on the other trunk row, because both rows are admitted by the
   \ one predicate and must move together.
   s" src/habu/habu2.f" TEST-CONST-VALUE-CASE
   s" the engine emitter exempts the same value change" T-LABEL
   TEST-EXPECT-CLEAN
   \ Negative: a sibling carrying the row's path as a prefix is not an exact
   \ match, so the identical value change still fails there.
   s" src/habu/layout-extra.f" TEST-CONST-VALUE-CASE
   s" sibling layout-extra.f still fails a constant value change" T-LABEL
   1 TEST-EXPECT-FINDINGS
   s" SNAP-FORMAT-VERSION" TEST-NAMES
   \ Negative: the same basename elsewhere carries the row's path as a suffix and
   \ is not exact, so it still fails.
   s" lib/layout.f" TEST-CONST-VALUE-CASE
   s" lib/layout.f still fails a constant value change" T-LABEL
   1 TEST-EXPECT-FINDINGS
   s" SNAP-FORMAT-VERSION" TEST-NAMES ;

: TEST-CONST-NEW-DIRECTIONS ( -- )
   \ Negative: a brand-new constant beside an existing one is reported by name in
   \ both trunk rows.  This is what keeps the entry from becoming a licence to
   \ grow the file's global surface: new layout bands open a package.
   s" src/habu/layout.f" TEST-CONST-NEW-CASE
   s" a new constant in layout still fails ownership" T-LABEL
   1 TEST-EXPECT-FINDINGS
   s" SNAP-SMUGGLED-BAND" TEST-NAMES
   s" src/habu/habu2.f" TEST-CONST-NEW-CASE
   s" a new constant in the engine emitter still fails ownership" T-LABEL
   1 TEST-EXPECT-FINDINGS
   s" SNAP-SMUGGLED-BAND" TEST-NAMES ;

: TEST-CONST-RENAME-DIRECTIONS ( -- )
   \ Negative: a rename keeps the definition count and touches one line, exactly
   \ like the admitted value change, and differs only in that the arriving name
   \ is one this file never defined.  It is a new global word and is reported.
   s" src/habu/layout.f" TEST-CONST-RENAME-CASE
   s" renaming a layout constant in place still fails ownership" T-LABEL
   1 TEST-EXPECT-FINDINGS
   s" SNAP-VERSION" TEST-NAMES
   s" src/habu/habu2.f" TEST-CONST-RENAME-CASE
   s" renaming an engine constant in place still fails ownership" T-LABEL
   1 TEST-EXPECT-FINDINGS
   s" SNAP-VERSION" TEST-NAMES ;

: TEST-CONST-PROMOTED-DIRECTION ( -- )
   TEST-WRITE-CONST-PROMOTED
   TEST-DIFF-RESET TEST-CONST-PROMOTED-DIFF
   s" a package-local constant moved to top level fails ownership" T-LABEL
   1 TEST-EXPECT-FINDINGS
   s" XTCELL-END" TEST-NAMES ;

\ Forth word identity, not byte identity: `bin/hb` resolves `foo` to `FOO`, so
\ respelling an existing global in another case publishes no name the file did
\ not already have and is a body edit like any other.  An exact-byte lookup would
\ report this as a new global word, which is a claim about the dictionary that
\ the dictionary does not make.
: TEST-WRITE-CONST-SPELLING ( -- )
   TEST-SOURCE-RESET
   s" 4 constant snap-format-version" TEST-SOURCE-LINE
   s" src/habu/layout.f" TEST-WRITE-SOURCE ;

: TEST-CONST-SPELLING-DIFF ( -- )
   s" src/habu/layout.f" TEST-MODIFY-HEAD
   s" @@ -1 +1 @@" TEST-DIFF+ TEST-LF
   s" -4 constant SNAP-FORMAT-VERSION" TEST-DIFF+ TEST-LF
   s" +4 constant snap-format-version" TEST-DIFF+ TEST-LF ;

: TEST-CONST-SPELLING-DIRECTION ( -- )
   TEST-WRITE-CONST-SPELLING
   TEST-DIFF-RESET TEST-CONST-SPELLING-DIFF
   s" respelling a layout constant's case defines no new word" T-LABEL
   TEST-EXPECT-CLEAN ;

\ The hostile that decides the key.  In the old file this definition sits inside
\ a `( ... )` comment, so the file never defined the word; the diff uncomments it
\ and edits one line of its body.  The definition therefore has a changed line
\ and is judged, but its own opener and name line are untouched, so a key that
\ asked "was the head line touched?" reads it as a body edit and admits a global
\ word this file has never had.  Nothing here is a body edit of an existing
\ engine word, and the report must say so by name.
: TEST-WRITE-UNCOMMENTED ( -- )
   TEST-SOURCE-RESET
   s" \ disabled block" TEST-SOURCE-LINE
   s" : EM-UNCOMMENTED ( -- n )" TEST-SOURCE-LINE
   s"    1 ;" TEST-SOURCE-LINE
   s" \ )" TEST-SOURCE-LINE
   s" : EM-SNAPSHOT-RX-FLUSH ( -- ) LPROT-RX ;" TEST-SOURCE-LINE
   s" src/habu/habu2.f" TEST-WRITE-SOURCE ;

: TEST-UNCOMMENTED-DIFF ( -- )
   s" src/habu/habu2.f" TEST-MODIFY-HEAD
   s" @@ -1,5 +1,5 @@" TEST-DIFF+ TEST-LF
   s" -( disabled block" TEST-DIFF+ TEST-LF
   s" +\ disabled block" TEST-DIFF+ TEST-LF
   s"  : EM-UNCOMMENTED ( -- n )" TEST-DIFF+ TEST-LF
   s" -   0 ;" TEST-DIFF+ TEST-LF
   s" +   1 ;" TEST-DIFF+ TEST-LF
   s" -)" TEST-DIFF+ TEST-LF
   s" +\ )" TEST-DIFF+ TEST-LF
   s"  : EM-SNAPSHOT-RX-FLUSH ( -- ) LPROT-RX ;" TEST-DIFF+ TEST-LF ;

: TEST-UNCOMMENTED-DIRECTION ( -- )
   TEST-WRITE-UNCOMMENTED
   TEST-DIFF-RESET TEST-UNCOMMENTED-DIFF
   s" a commented-out engine word arriving as a global fails ownership" T-LABEL
   1 TEST-EXPECT-FINDINGS
   s" EM-UNCOMMENTED" TEST-NAMES ;

: TEST-CONST-DEFINITIONS ( -- )
   TEST-CONST-VALUE-DIRECTIONS
   TEST-CONST-NEW-DIRECTIONS
   TEST-CONST-RENAME-DIRECTIONS
   TEST-CONST-PROMOTED-DIRECTION
   TEST-CONST-SPELLING-DIRECTION
   TEST-UNCOMMENTED-DIRECTION ;

: TEST-WRITE-OUTSIDE-HUNK-SOURCE ( -- )
   TEST-SOURCE-RESET
   s" package SHARED" TEST-SOURCE-LINE
   s" : OLD ( -- n ) 1 ;" TEST-SOURCE-LINE
   s" : NEW ( -- n ) OTHER:WORD ;" TEST-SOURCE-LINE
   s" ;package" TEST-SOURCE-LINE
   s" lib/shared-a.f" TEST-WRITE-SOURCE ;

: TEST-OUTSIDE-HUNK-DIFF ( -- )
   s" lib/shared-a.f" TEST-MODIFY-HEAD
   s" @@ -2 +2,2 @@" TEST-DIFF+ TEST-LF
   s"  : OLD ( -- n ) 1 ;" TEST-DIFF+ TEST-LF
   s" +: NEW ( -- n ) OTHER:WORD ;" TEST-DIFF+ TEST-LF ;

: TEST-ADD-REOPENED-SOURCE ( -- )
   TEST-SOURCE-RESET
   s" package SHARED" TEST-SOURCE-LINE
   s" : SECOND ( -- n ) 2 ;" TEST-SOURCE-LINE
   s" ;package" TEST-SOURCE-LINE
   s" lib/shared-b.f" TEST-ADD-SOURCE-SECTION ;

: TEST-ADD-FORGED-SOURCE ( -- )
   TEST-SOURCE-RESET
   s" package SAFE" TEST-SOURCE-LINE
   s" \ : COMMENT-FORGED ( -- ) ;" TEST-SOURCE-LINE
   TEST-SOURCE-STRING-LINE
   s" : HEADER-TEXT ( -- ) ; \ +++ b/forged.f" TEST-SOURCE-LINE
   s" ;package" TEST-SOURCE-LINE
   s" tools/forged.f" TEST-ADD-SOURCE-SECTION ;

: TEST-ADD-PRELUDE-SOURCE ( -- )
   TEST-SOURCE-RESET
   s" : true ( -- bool ) 0 0= ;" TEST-SOURCE-LINE
   s" lib/prelude.f" TEST-ADD-SOURCE-SECTION ;

: TEST-WRITE-BALANCED-SOURCE ( -- )
   TEST-SOURCE-RESET
   s" : LEGACY ( -- n ) 1 ;" TEST-SOURCE-LINE
   s" package SIDE" TEST-SOURCE-LINE
   s" : LOCAL ( -- n ) 2 ;" TEST-SOURCE-LINE
   s" ;package" TEST-SOURCE-LINE
   s" : LATER ( -- n ) 3 ;" TEST-SOURCE-LINE
   s" lib/balanced.f" TEST-WRITE-SOURCE ;

: TEST-BALANCED-DIFF ( -- )
   s" lib/balanced.f" TEST-MODIFY-HEAD
   s" @@ -1,2 +1,5 @@" TEST-DIFF+ TEST-LF
   s"  : LEGACY ( -- n ) 1 ;" TEST-DIFF+ TEST-LF
   s" +package SIDE" TEST-DIFF+ TEST-LF
   s" +: LOCAL ( -- n ) 2 ;" TEST-DIFF+ TEST-LF
   s" +;package" TEST-DIFF+ TEST-LF
   s"  : LATER ( -- n ) 3 ;" TEST-DIFF+ TEST-LF ;

: TEST-WRITE-PAREN-NAME-SOURCE ( -- )
   TEST-SOURCE-RESET
   s" package PAREN-NAME" TEST-SOURCE-LINE
   s" : (CMP) ( n -- ) ;" TEST-SOURCE-LINE
   TEST-BACKSLASH-C TEST-SOURCE-C
   s"  new comment" TEST-SOURCE-LINE
   s" ;package" TEST-SOURCE-LINE
   s" lib/paren-name.f" TEST-WRITE-SOURCE ;

: TEST-PAREN-NAME-DIFF ( -- )
   s" lib/paren-name.f" TEST-MODIFY-HEAD
   s" @@ -1,4 +1,4 @@" TEST-DIFF+ TEST-LF
   s"  package PAREN-NAME" TEST-DIFF+ TEST-LF
   s"  : (CMP) ( n -- ) ;" TEST-DIFF+ TEST-LF
   s" -" TEST-DIFF+  TEST-BACKSLASH-C TEST-DIFF-C
   s"  old comment" TEST-DIFF+ TEST-LF
   s" +" TEST-DIFF+  TEST-BACKSLASH-C TEST-DIFF-C
   s"  new comment" TEST-DIFF+ TEST-LF
   s"  ;package" TEST-DIFF+ TEST-LF ;

: TEST-PAREN-NAME-REPLAY ( -- )
   TEST-WRITE-PAREN-NAME-SOURCE
   TEST-DIFF-RESET
   TEST-PAREN-NAME-DIFF
   s" comment-only diff preserves parenthesized word name" T-LABEL
   TEST-EXPECT-CLEAN ;

: TEST-WRITE-CONTROL-COMMENT-SOURCE ( -- )
   TEST-SOURCE-RESET
   s" PRIM: FOO (" TEST-SOURCE+
   TEST-VT-C TEST-SOURCE-C
   s" PRIM; package FAKE ) PE-N PRIM;" TEST-SOURCE-LINE
   s" : CONTROL-LEAK ( -- n ) 1 ;" TEST-SOURCE-LINE
   s" PRIM: BAR (" TEST-SOURCE+
   TEST-VT-C TEST-SOURCE-C
   s" PRIM; ;package ) PE-N PRIM;" TEST-SOURCE-LINE ;

\ Both fake package transitions sit inside control-whitespace comments in real
\ registry rows.  They cannot surround and hide the changed global definition.
: TEST-CONTROL-COMMENT-REPLAY ( -- )
   TEST-WRITE-CONTROL-COMMENT-SOURCE
   TEST-DIFF-RESET
   s" lib/control-comment.f" TEST-ADD-SOURCE-SECTION
   s" control-whitespace row comments cannot forge package scope" T-LABEL
   1 TEST-EXPECT-FINDINGS ;

: TEST-POSITIVES ( -- )
   TEST-WRITE-OUTSIDE-HUNK-SOURCE
   TEST-DIFF-RESET
   TEST-OUTSIDE-HUNK-DIFF
   TEST-ADD-REOPENED-SOURCE
   s" package opener outside hunk and reopened package" T-LABEL
   TEST-EXPECT-CLEAN
   TEST-DIFF-RESET
   TEST-ADD-FORGED-SOURCE
   s" comments strings and header text cannot forge scope" T-LABEL
   TEST-EXPECT-CLEAN
   TEST-DIFF-RESET
   TEST-ADD-PRELUDE-SOURCE
   s" documented prelude global is exempt" T-LABEL
   TEST-EXPECT-CLEAN
   TEST-WRITE-BALANCED-SOURCE
   TEST-DIFF-RESET
   TEST-BALANCED-DIFF
   s" balanced package insertion does not taint legacy globals" T-LABEL
   TEST-EXPECT-CLEAN ;

: TEST-WRITE-DELETED-OWNER-SOURCE ( -- )
   TEST-SOURCE-RESET
   s" : LEAK ( -- n ) 1 ;" TEST-SOURCE-LINE
   s" lib/deleted-owner.f" TEST-WRITE-SOURCE ;

: TEST-DELETED-OWNER-DIFF ( -- )
   s" lib/deleted-owner.f" TEST-MODIFY-HEAD
   s" @@ -1,3 +1 @@" TEST-DIFF+ TEST-LF
   s" -package OLD" TEST-DIFF+ TEST-LF
   s"  : LEAK ( -- n ) 1 ;" TEST-DIFF+ TEST-LF
   s" -;package" TEST-DIFF+ TEST-LF ;

: TEST-DELETED-OWNER ( -- )
   TEST-WRITE-DELETED-OWNER-SOURCE
   TEST-DIFF-RESET TEST-DELETED-OWNER-DIFF
   1 TEST-EXPECT-FINDINGS ;

: TEST-ZERO-COUNT-OWNER-DIFF ( -- )
   s" lib/deleted-owner.f" TEST-MODIFY-HEAD
   s" @@ -1 +0,0 @@" TEST-DIFF+ TEST-LF
   s" -package OLD" TEST-DIFF+ TEST-LF
   s" @@ -3 +1,0 @@" TEST-DIFF+ TEST-LF
   s" -;package" TEST-DIFF+ TEST-LF ;

: TEST-ZERO-COUNT-OWNER-DELETION ( -- )
   TEST-WRITE-DELETED-OWNER-SOURCE
   TEST-DIFF-RESET TEST-ZERO-COUNT-OWNER-DIFF
   s" zero-count deletion at start preserves owner alignment" T-LABEL
   1 TEST-EXPECT-FINDINGS ;

: TEST-WRITE-EMPTY-SOURCE ( -- )
   TEST-SOURCE-RESET
   s" lib/emptied.f" TEST-WRITE-SOURCE ;

: TEST-DELETE-TO-EMPTY-DIFF ( -- )
   s" lib/emptied.f" TEST-MODIFY-HEAD
   s" @@ -1,3 +0,0 @@" TEST-DIFF+ TEST-LF
   s" -package OLD" TEST-DIFF+ TEST-LF
   s" -: GONE ( -- ) ;" TEST-DIFF+ TEST-LF
   s" -;package" TEST-DIFF+ TEST-LF ;

: TEST-DELETION-TO-EMPTY ( -- )
   TEST-WRITE-EMPTY-SOURCE
   TEST-DIFF-RESET TEST-DELETE-TO-EMPTY-DIFF
   s" deletion to an empty Forth file stays canonical" T-LABEL
   TEST-EXPECT-CLEAN ;

: TEST-WRITE-GLOBAL-BODY-SOURCE ( ptr u8 n -- ) {: path:ptr pathu:n :}
   TEST-SOURCE-RESET
   s" : LEGACY ( -- n )" TEST-SOURCE-LINE
   s"    1" TEST-SOURCE-LINE
   s"    2 +" TEST-SOURCE-LINE
   s" ;" TEST-SOURCE-LINE
   path pathu TEST-WRITE-SOURCE ;

: TEST-GLOBAL-BODY-DIFF ( ptr u8 n -- )
   TEST-MODIFY-HEAD
   s" @@ -1,3 +1,4 @@" TEST-DIFF+ TEST-LF
   s"  : LEGACY ( -- n )" TEST-DIFF+ TEST-LF
   s"     1" TEST-DIFF+ TEST-LF
   s" +   2 +" TEST-DIFF+ TEST-LF
   s"  ;" TEST-DIFF+ TEST-LF ;

: TEST-CHANGED-GLOBAL-BODY ( -- )
   s" lib/global-body.f" TEST-WRITE-GLOBAL-BODY-SOURCE
   TEST-DIFF-RESET s" lib/global-body.f" TEST-GLOBAL-BODY-DIFF
   1 TEST-EXPECT-FINDINGS ;

\ A Gforth-hosted .fs source outside the one admitted mirror row is scanned like
\ any other file and its unowned global reports. The extension is deliberately
\ NOT the key (see package-diff-lint-core.f, the mirror category): admitting
\ every .fs would admit any future file that happened to be named that way, and
\ would silently retire the mirror row and all of its hostile fixtures.
: TEST-FS-GLOBAL ( -- )
   s" tools/recovery.fs" TEST-WRITE-GLOBAL-BODY-SOURCE
   TEST-DIFF-RESET s" tools/recovery.fs" TEST-GLOBAL-BODY-DIFF
   s" an ordinary changed .fs global outside the mirror row still reports" T-LABEL
   1 TEST-EXPECT-FINDINGS ;

: TEST-WRITE-COMMENT-CANCEL-SOURCE ( -- )
   TEST-SOURCE-RESET
   s" (" TEST-SOURCE-LINE
   s" )" TEST-SOURCE-LINE
   s" : LEAK ( -- n ) 1 ;" TEST-SOURCE-LINE
   s" lib/comment-cancel.f" TEST-WRITE-SOURCE ;

: TEST-COMMENT-CANCEL-DIFF ( -- )
   s" lib/comment-cancel.f" TEST-MODIFY-HEAD
   s" @@ -1,7 +1,3 @@" TEST-DIFF+ TEST-LF
   s"  (" TEST-DIFF+ TEST-LF
   s" -;package" TEST-DIFF+ TEST-LF
   s" -package COMMENT-FORGERY" TEST-DIFF+ TEST-LF
   s"  )" TEST-DIFF+ TEST-LF
   s" -package OLD" TEST-DIFF+ TEST-LF
   s"  : LEAK ( -- n ) 1 ;" TEST-DIFF+ TEST-LF
   s" -;package" TEST-DIFF+ TEST-LF ;

: TEST-MULTILINE-COMMENT-CANCELLATION ( -- )
   TEST-WRITE-COMMENT-CANCEL-SOURCE
   TEST-DIFF-RESET TEST-COMMENT-CANCEL-DIFF
   s" deleted multiline-comment tokens cannot cancel owner loss" T-LABEL
   1 TEST-EXPECT-FINDINGS ;

: TEST-WRITE-DEFINITION-DELETE-SOURCE ( -- )
   TEST-SOURCE-RESET
   s" package SAFE" TEST-SOURCE-LINE
   s" : HOLDER ( -- )" TEST-SOURCE-LINE
   s" ;" TEST-SOURCE-LINE
   s" ;package" TEST-SOURCE-LINE
   s" : LEGACY ( -- n ) 1 ;" TEST-SOURCE-LINE
   s" lib/definition-delete.f" TEST-WRITE-SOURCE ;

: TEST-DEFINITION-DELETE-DIFF ( -- )
   s" lib/definition-delete.f" TEST-MODIFY-HEAD
   s" @@ -1,6 +1,5 @@" TEST-DIFF+ TEST-LF
   s"  package SAFE" TEST-DIFF+ TEST-LF
   s"  : HOLDER ( -- )" TEST-DIFF+ TEST-LF
   s" -   package NOT-A-BOUNDARY" TEST-DIFF+ TEST-LF
   s"  ;" TEST-DIFF+ TEST-LF
   s"  ;package" TEST-DIFF+ TEST-LF
   s"  : LEGACY ( -- n ) 1 ;" TEST-DIFF+ TEST-LF ;

: TEST-DELETED-TOKEN-IN-DEFINITION ( -- )
   TEST-WRITE-DEFINITION-DELETE-SOURCE
   TEST-DIFF-RESET TEST-DEFINITION-DELETE-DIFF
   s" deleted package token inside definition is not structural" T-LABEL
   TEST-EXPECT-CLEAN ;

: TEST-WRITE-RENAMED-GLOBAL-SOURCE ( -- )
   s" RENAMED-GLOBAL" TEST-GLOBAL-SOURCE
   s" lib/renamed-global.f" TEST-WRITE-SOURCE ;

: TEST-RENAMED-GLOBAL-DIFF ( -- )
   s" diff --git a/lib/old-global.f b/lib/renamed-global.f" TEST-DIFF+ TEST-LF
   s" similarity index 100%" TEST-DIFF+ TEST-LF
   s" rename from lib/old-global.f" TEST-DIFF+ TEST-LF
   s" rename to lib/renamed-global.f" TEST-DIFF+ TEST-LF ;

: TEST-BODYLESS-RENAME ( -- )
   TEST-WRITE-RENAMED-GLOBAL-SOURCE
   TEST-DIFF-RESET TEST-RENAMED-GLOBAL-DIFF
   s" bodyless rename still changes module ownership" T-LABEL
   1 TEST-EXPECT-FINDINGS ;

: TEST-WRITE-STALE-SOURCE ( -- )
   TEST-SOURCE-RESET
   s" package STALE" TEST-SOURCE-LINE
   s" : GOOD ( -- n ) 1 ;" TEST-SOURCE-LINE
   s" ;package" TEST-SOURCE-LINE
   s" lib/stale.f" TEST-WRITE-SOURCE ;

: TEST-STALE-DIFF ( -- )
   s" lib/stale.f" TEST-MODIFY-HEAD
   s" @@ -2 +2 @@" TEST-DIFF+ TEST-LF
   s" -: OLD ( -- n ) 0 ;" TEST-DIFF+ TEST-LF
   s" +: BAD ( -- n ) 1 ;" TEST-DIFF+ TEST-LF ;

: TEST-REUSE-AFTER-ERROR ( -- )
   TEST-SOURCE-RESET
   s" package REUSE" TEST-SOURCE-LINE
   s" : OK ( -- n ) 1 ;" TEST-SOURCE-LINE
   s" ;package" TEST-SOURCE-LINE
   TEST-DIFF-RESET s" lib/reuse.f" TEST-ADD-SOURCE-SECTION
   TEST-DIFF$ PACKAGE-DIFF:SOURCE
   PACKAGE-DIFF:FINISH
   PACKAGE-DIFF:FINDINGS 0 T=
   LIVE-MAPPING# 0 T= ;

: TEST-STALE-FAILS-CLOSED ( -- )
   TEST-WRITE-STALE-SOURCE
   TEST-DIFF-RESET TEST-STALE-DIFF
   [: TEST-RUN-DIRECT ;] E-DIFF-SYNTAX TTHROWSQ
   LIVE-MAPPING# 0 T=
   \ new source, mark table, reconstructed old source, old-side name table
   MAPPING-PEAK @ 4 T=
   TEST-REUSE-AFTER-ERROR ;

: TEST-MALFORMED-REUSE ( -- )
   TEST-DIFF-RESET
   s" this is not a unified diff" TEST-DIFF+ TEST-LF
   [: TEST-RUN-DIRECT ;] E-DIFF-SYNTAX TTHROWSQ
   LIVE-MAPPING# 0 T=
   MAPPING-PEAK @ 0 T=
   TEST-REUSE-AFTER-ERROR ;

: TEST-RUN-ONE-ALLOCATION-FAULT ( -- )
   PACKAGE-DIFF:RESET
   TEST-ROOT$ PACKAGE-DIFF:ROOT!
   true FAIL-NEXT-MARK-ALLOC !
   TEST-DIFF$ PACKAGE-DIFF:SOURCE
   PACKAGE-DIFF:FINISH ;

: TEST-ONE-ALLOCATION-REUSE ( -- )
   TEST-SOURCE-RESET
   s" package NEVER" TEST-SOURCE-LINE
   s" : WORD ( -- ) ;" TEST-SOURCE-LINE
   s" ;package" TEST-SOURCE-LINE
   TEST-DIFF-RESET s" tools/allocation-fault.f" TEST-ADD-SOURCE-SECTION
   [: TEST-RUN-ONE-ALLOCATION-FAULT ;] E-MEM-SIZE TTHROWSQ
   LIVE-MAPPING# 0 T=
   MAPPING-PEAK @ 1 T=
   TEST-REUSE-AFTER-ERROR ;

: TEST-RUN-TWO-ALLOCATION-FAULT ( -- )
   PACKAGE-DIFF:RESET
   TEST-ROOT$ PACKAGE-DIFF:ROOT!
   true FAIL-NEXT-OLD-ALLOC !
   TEST-DIFF$ PACKAGE-DIFF:SOURCE
   PACKAGE-DIFF:FINISH ;

: TEST-TWO-ALLOCATION-REUSE ( -- )
   TEST-SOURCE-RESET
   s" package NEVER" TEST-SOURCE-LINE
   s" : WORD ( -- ) ;" TEST-SOURCE-LINE
   s" ;package" TEST-SOURCE-LINE
   TEST-DIFF-RESET s" tools/old-allocation-fault.f" TEST-ADD-SOURCE-SECTION
   [: TEST-RUN-TWO-ALLOCATION-FAULT ;] E-MEM-SIZE TTHROWSQ
   LIVE-MAPPING# 0 T=
   MAPPING-PEAK @ 2 T=
   TEST-REUSE-AFTER-ERROR ;

: TEST-RUN-THREE-ALLOCATION-FAULT ( -- )
   PACKAGE-DIFF:RESET
   TEST-ROOT$ PACKAGE-DIFF:ROOT!
   true FAIL-NEXT-NAME-ALLOC !
   TEST-DIFF$ PACKAGE-DIFF:SOURCE
   PACKAGE-DIFF:FINISH ;

: TEST-THREE-ALLOCATION-REUSE ( -- )
   TEST-SOURCE-RESET
   s" package NEVER" TEST-SOURCE-LINE
   s" : WORD ( -- ) ;" TEST-SOURCE-LINE
   s" ;package" TEST-SOURCE-LINE
   TEST-DIFF-RESET s" tools/name-allocation-fault.f" TEST-ADD-SOURCE-SECTION
   [: TEST-RUN-THREE-ALLOCATION-FAULT ;] E-MEM-SIZE TTHROWSQ
   LIVE-MAPPING# 0 T=
   MAPPING-PEAK @ 3 T=
   TEST-REUSE-AFTER-ERROR ;

\ The name table is sized so that a well-formed source cannot fill it, so this
\ fixture lowers the bound to make the full table happen on purpose.  What is
\ under test is the refusal: the run must stop with the table's own code, and it
\ must stop even though the file it is reading is a perfectly ordinary global
\ body edit that the entry would otherwise admit.  A table that truncated instead
\ would answer "this name is new" and turn the admission into a report -- or, on
\ the next diff, hide a global behind a name it failed to record.
: TEST-RUN-NAME-OVERFLOW ( -- )
   PACKAGE-DIFF:RESET
   TEST-ROOT$ PACKAGE-DIFF:ROOT!
   1 FORCE-NAME-LIMIT !
   TEST-DIFF$ PACKAGE-DIFF:SOURCE
   PACKAGE-DIFF:FINISH ;

: TEST-NAME-OVERFLOW ( -- )
   s" src/habu/layout.f" TEST-CONST-VALUE-CASE
   s" a full old-side name table stops the scan by name" T-LABEL
   [: TEST-RUN-NAME-OVERFLOW ;] E-PKGDIFF-NAMETAB TTHROWSQ
   LIVE-MAPPING# 0 T=
   TEST-REUSE-AFTER-ERROR ;

\ ---- lib/errors.f error-vocabulary admission ------------------------------
\ lib/errors.f may hold global error-code constants and nothing else.  Every
\ fixture below runs the real lint entry on a constructed diff artifact.
\
\ The declaration lines are assembled from parts instead of written out.  This
\ file must never spell `-NNNN constant E-NAME` literally: tools/error-code-lint.f
\ scans repository sources for that token sequence and would record these
\ fixtures as real claims on the shared error-code ledger.

: TEST-ERR-DECL+ ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: code:ptr codeu:n def:ptr defu:n name:ptr nameu:n :}
   TEST-MINUS-C TEST-SOURCE-C
   code codeu TEST-SOURCE+
   TEST-SPACE-C TEST-SOURCE-C
   def defu TEST-SOURCE+
   TEST-SPACE-C TEST-SOURCE-C
   name nameu TEST-SOURCE+ ;

: TEST-ERR-DECL-LINE ( ptr u8 n ptr u8 n ptr u8 n -- )
   TEST-ERR-DECL+ TEST-LF-C TEST-SOURCE-C ;

: TEST-ERR-CODE-LINE ( ptr u8 n ptr u8 n -- ) {: code:ptr codeu:n name:ptr nameu:n :}
   code codeu s" constant" name nameu TEST-ERR-DECL-LINE ;

: TEST-ERR-DIFF-CODE ( n ptr u8 n ptr u8 n -- ) {: mark:n code:ptr codeu:n name:ptr nameu:n :}
   mark TEST-DIFF-C
   TEST-MINUS-C TEST-DIFF-C
   code codeu TEST-DIFF+
   s"  constant " TEST-DIFF+
   name nameu TEST-DIFF+ TEST-LF ;

: TEST-ADD-ERR-CODES ( -- )
   s" 4400" s" E-TEST-FIRST" TEST-ERR-CODE-LINE
   s" 4401" s" E-TEST-STATE" TEST-ERR-CODE-LINE ;

: TEST-ERR-CODES-ADMITTED ( -- )
   TEST-SOURCE-RESET TEST-ADD-ERR-CODES
   TEST-DIFF-RESET s" lib/errors.f" TEST-ADD-SOURCE-SECTION
   s" global error codes are admitted in lib/errors.f" T-LABEL
   TEST-EXPECT-CLEAN ;

: TEST-ADD-ERR-PACKAGED ( -- )
   s" package JR" TEST-SOURCE-LINE
   s" public" TEST-SOURCE-LINE
   s" 3912" s" E-CAPACITY" TEST-ERR-CODE-LINE
   s" ;package" TEST-SOURCE-LINE ;

: TEST-ERR-PACKAGED-BLOCK ( -- )
   TEST-SOURCE-RESET TEST-ADD-ERR-CODES TEST-ADD-ERR-PACKAGED
   TEST-DIFF-RESET s" lib/errors.f" TEST-ADD-SOURCE-SECTION
   s" a packaged code block in lib/errors.f stays clean" T-LABEL
   TEST-EXPECT-CLEAN ;

: TEST-ADD-ERR-BAD-NAMES ( -- )
   s" 4401" s" TEST-STATE" TEST-ERR-CODE-LINE
   s" 4402" s" ETEST-STATE" TEST-ERR-CODE-LINE
   s" 4403" s" E_TEST-STATE" TEST-ERR-CODE-LINE
   s" 4404" s" e-test-state" TEST-ERR-CODE-LINE
   s" 4405" s" E-Test-State" TEST-ERR-CODE-LINE
   s" 4406" s" E-" TEST-ERR-CODE-LINE ;

: TEST-ERR-BAD-NAMES ( -- )
   TEST-SOURCE-RESET TEST-ADD-ERR-BAD-NAMES
   TEST-DIFF-RESET s" lib/errors.f" TEST-ADD-SOURCE-SECTION
   s" only capitalised E-prefixed names are admitted" T-LABEL
   6 TEST-EXPECT-FINDINGS ;

: TEST-ADD-ERR-BAD-DEFINERS ( -- )
   s" : E-TEST-COLON ( -- n ) 0 ;" TEST-SOURCE-LINE
   s" CHECKED: E-TEST-CHECKED ( -- n ) 0 ;" TEST-SOURCE-LINE
   s" variable E-TEST-VARIABLE" TEST-SOURCE-LINE
   s" create E-TEST-CREATE" TEST-SOURCE-LINE
   s" value E-TEST-VALUE" TEST-SOURCE-LINE
   s" defer E-TEST-DEFER" TEST-SOURCE-LINE
   s" 2constant E-TEST-PAIR" TEST-SOURCE-LINE
   s" 4403" s" CONSTANT" s" E-TEST-CAPITAL" TEST-ERR-DECL-LINE
   s" 4404" s" Constant" s" E-TEST-MIXED" TEST-ERR-DECL-LINE ;

: TEST-ERR-BAD-DEFINERS ( -- )
   TEST-SOURCE-RESET TEST-ADD-ERR-BAD-DEFINERS
   TEST-DIFF-RESET s" lib/errors.f" TEST-ADD-SOURCE-SECTION
   s" only the lower-case constant definer is admitted" T-LABEL
   9 TEST-EXPECT-FINDINGS ;

: TEST-ERR-CODES-AT ( ptr u8 n ptr u8 n -- ) {: why:ptr whyu:n path:ptr pathu:n :}
   TEST-SOURCE-RESET TEST-ADD-ERR-CODES
   TEST-DIFF-RESET path pathu TEST-ADD-SOURCE-SECTION
   why whyu T-LABEL
   2 TEST-EXPECT-FINDINGS ;

: TEST-ERR-OTHER-PATHS ( -- )
   s" lib/errors-extra.f keeps the path as a prefix and still fails"
   s" lib/errors-extra.f" TEST-ERR-CODES-AT
   s" test/lib/errors.f keeps the path as a suffix and still fails"
   s" test/lib/errors.f" TEST-ERR-CODES-AT
   s" tools/errors.f shares only the basename and still fails"
   s" tools/errors.f" TEST-ERR-CODES-AT ;

: TEST-WRITE-ERR-RENAME-SOURCE ( -- )
   TEST-SOURCE-RESET
   s" 4400" s" E-TEST-FIRST" TEST-ERR-CODE-LINE
   s" 4401" s" E-TEST-MODE" TEST-ERR-CODE-LINE
   s" lib/errors.f" TEST-WRITE-SOURCE ;

: TEST-ERR-RENAME-DIFF ( -- )
   s" lib/errors.f" TEST-MODIFY-HEAD
   s" @@ -1,2 +1,2 @@" TEST-DIFF+ TEST-LF
   TEST-SPACE-C s" 4400" s" E-TEST-FIRST" TEST-ERR-DIFF-CODE
   TEST-MINUS-C s" 4401" s" E-TEST-STATE" TEST-ERR-DIFF-CODE
   TEST-PLUS-C  s" 4401" s" E-TEST-MODE" TEST-ERR-DIFF-CODE ;

: TEST-ERR-RENAMED-CODE ( -- )
   TEST-WRITE-ERR-RENAME-SOURCE
   TEST-DIFF-RESET TEST-ERR-RENAME-DIFF
   s" renaming an existing error code stays admitted" T-LABEL
   TEST-EXPECT-CLEAN ;

: TEST-WRITE-ERR-LOST-OWNER-SOURCE ( -- )
   TEST-SOURCE-RESET
   s" 4401" s" E-TEST-STATE" TEST-ERR-CODE-LINE
   s" lib/errors.f" TEST-WRITE-SOURCE ;

: TEST-ERR-LOST-OWNER-DIFF ( -- )
   s" lib/errors.f" TEST-MODIFY-HEAD
   s" @@ -1,3 +1 @@" TEST-DIFF+ TEST-LF
   s" -package ERRV" TEST-DIFF+ TEST-LF
   TEST-SPACE-C s" 4401" s" E-TEST-STATE" TEST-ERR-DIFF-CODE
   s" -;package" TEST-DIFF+ TEST-LF ;

: TEST-ERR-DELETED-BOUNDARY ( -- )
   TEST-WRITE-ERR-LOST-OWNER-SOURCE
   TEST-DIFF-RESET TEST-ERR-LOST-OWNER-DIFF
   s" deleting a package boundary in lib/errors.f still fails" T-LABEL
   1 TEST-EXPECT-FINDINGS ;

: TEST-WRITE-ERR-MOVED-OWNER-SOURCE ( -- )
   TEST-SOURCE-RESET
   s" package ERRV" TEST-SOURCE-LINE
   s" 4400" s" E-TEST-FIRST" TEST-ERR-CODE-LINE
   s" ;package" TEST-SOURCE-LINE
   s" 4401" s" E-TEST-STATE" TEST-ERR-CODE-LINE
   s" lib/errors.f" TEST-WRITE-SOURCE ;

: TEST-ERR-MOVED-OWNER-DIFF ( -- )
   s" lib/errors.f" TEST-MODIFY-HEAD
   s" @@ -1,4 +1,4 @@" TEST-DIFF+ TEST-LF
   s"  package ERRV" TEST-DIFF+ TEST-LF
   TEST-SPACE-C s" 4400" s" E-TEST-FIRST" TEST-ERR-DIFF-CODE
   s" +;package" TEST-DIFF+ TEST-LF
   TEST-SPACE-C s" 4401" s" E-TEST-STATE" TEST-ERR-DIFF-CODE
   s" -;package" TEST-DIFF+ TEST-LF ;

: TEST-ERR-ADDED-BOUNDARY ( -- )
   TEST-WRITE-ERR-MOVED-OWNER-SOURCE
   TEST-DIFF-RESET TEST-ERR-MOVED-OWNER-DIFF
   s" adding a package boundary in lib/errors.f still fails" T-LABEL
   1 TEST-EXPECT-FINDINGS ;

: TEST-ERR-COMMENT-CODE ( ptr u8 n ptr u8 n -- )
   TEST-BACKSLASH-C TEST-SOURCE-C
   TEST-SPACE-C TEST-SOURCE-C
   TEST-ERR-CODE-LINE ;

: TEST-ERR-QUOTED-LINE ( -- )
   s" s" TEST-SOURCE+
   TEST-DQUOTE-C TEST-SOURCE-C
   TEST-SPACE-C TEST-SOURCE-C
   s" 4402" s" constant" s" E-TEST-QUOTED" TEST-ERR-DECL+
   TEST-DQUOTE-C TEST-SOURCE-C
   s"  drop" TEST-SOURCE-LINE ;

: TEST-ERR-COMMENT-LINE ( ptr u8 n -- ) {: a:ptr u:n :}
   TEST-BACKSLASH-C TEST-SOURCE-C
   TEST-SPACE-C TEST-SOURCE-C
   a u TEST-SOURCE-LINE ;

\ Two commented legs: an admitted shape, so a leaked comment cannot satisfy the
\ admission, and a rejected shape, so a lexer that fed comment text into the
\ token stream would raise the finding count and turn this fixture red.
: TEST-ADD-ERR-SMUGGLED ( -- )
   s" 4401" s" E-TEST-COMMENTED" TEST-ERR-COMMENT-CODE
   s" variable E-TEST-COMMENTED" TEST-ERR-COMMENT-LINE
   TEST-ERR-QUOTED-LINE
   s" variable E-TEST-STATE" TEST-SOURCE-LINE ;

: TEST-ERR-SMUGGLED-TEXT ( -- )
   TEST-SOURCE-RESET TEST-ADD-ERR-SMUGGLED
   TEST-DIFF-RESET s" lib/errors.f" TEST-ADD-SOURCE-SECTION
   s" comment and string text cannot admit a bad declaration" T-LABEL
   1 TEST-EXPECT-FINDINGS ;

\ Recorded decision: this lint reads the path, the definer, and the name shape,
\ never the declared value, so a positive value is admitted here.  Error codes
\ are negative by convention, but that ledger belongs to tools/error-code-lint.f;
\ enforcing negativity in the package lint would be a separate change.
: TEST-ERR-POSITIVE-VALUE ( -- )
   TEST-SOURCE-RESET
   s" 76 constant E-TEST-EXIT" TEST-SOURCE-LINE
   TEST-DIFF-RESET s" lib/errors.f" TEST-ADD-SOURCE-SECTION
   s" a positive value is admitted: shape is checked, value is not" T-LABEL
   TEST-EXPECT-CLEAN ;

: TEST-ERROR-VOCABULARY ( -- )
   TEST-ERR-CODES-ADMITTED
   TEST-ERR-POSITIVE-VALUE
   TEST-ERR-PACKAGED-BLOCK
   TEST-ERR-BAD-NAMES
   TEST-ERR-BAD-DEFINERS
   TEST-ERR-OTHER-PATHS
   TEST-ERR-RENAMED-CODE
   TEST-ERR-DELETED-BOUNDARY
   TEST-ERR-ADDED-BOUNDARY
   TEST-ERR-SMUGGLED-TEXT ;

: TEST-MAIN ( -- )
   T-RESET
   TEST-PREPARE
   TEST-GLOBAL-OWNERS
   TEST-CASE-CLOSE-REOPEN
   TEST-REDUNDANT-PREFIXES
   TEST-DEFINER-INVENTORY
   TEST-REGISTRY-LANGUAGE
   TEST-REGISTRY-ROWS
   TEST-PAREN-NAMES
   TEST-CORE-EXEMPTIONS
   TEST-OPTION-GLOBAL
   TEST-RESULT-GLOBAL
   TEST-GRAMMAR-FIXTURES
   TEST-STAGE0-FIXTURES
   TEST-TYPE-FAMILY-EXEMPTION
   TEST-CHECKER-EXEMPTION
   TEST-RENDER-EXEMPTION
   TEST-INTMARK-EXEMPTION
   TEST-ARM64-EXEMPTION
   TEST-IMAGE-EXEMPTION
   TEST-ENGINE-EXEMPTION
   TEST-MIRROR-EXEMPTION
   TEST-CONST-DEFINITIONS
   TEST-ERROR-VOCABULARY
   TEST-POSITIVES
   TEST-PAREN-NAME-REPLAY
   TEST-CONTROL-COMMENT-REPLAY
   TEST-DELETED-OWNER
   TEST-ZERO-COUNT-OWNER-DELETION
   TEST-DELETION-TO-EMPTY
   TEST-FS-GLOBAL
   TEST-CHANGED-GLOBAL-BODY
   TEST-MULTILINE-COMMENT-CANCELLATION
   TEST-DELETED-TOKEN-IN-DEFINITION
   TEST-BODYLESS-RENAME
   TEST-STALE-FAILS-CLOSED
   TEST-MALFORMED-REUSE
   TEST-ONE-ALLOCATION-REUSE
   TEST-TWO-ALLOCATION-REUSE
   TEST-THREE-ALLOCATION-REUSE
   TEST-NAME-OVERFLOW
   CLEANUP-RUN
   TEST-ROOT$ EXISTS? TFALSE
   T-REPORT
   s" package-diff-lint-test: ok" type cr ;

TEST-MAIN

;package
