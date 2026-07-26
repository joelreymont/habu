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

create TEST-NAME-BUF TEST-NAME-CAP allot
create TEST-ROOT-BUF FS-PATH-CAP allot
create TEST-PATH-BUF FS-PATH-CAP allot
create TEST-SOURCE-BUF TEST-SOURCE-CAP allot
create TEST-DIFF-BUF TEST-DIFF-CAP allot
create TEST-OUT-BUF TEST-OUT-CAP allot

variable TEST-ROOT-U
variable TEST-PATH-U
variable TEST-SOURCE-U
variable TEST-SOURCE-START
variable TEST-DIFF-U

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
   TEST-ROOT$ s" tools" TEST-PATH-BUF JOIN-PATH TEST-PATH-U !
   TEST-PATH-BUF TEST-PATH-U @ MAKE-DIRS
   TEST-ROOT$ s" test" TEST-PATH-BUF JOIN-PATH TEST-PATH-U !
   TEST-PATH-BUF TEST-PATH-U @ MAKE-DIRS
   TEST-ROOT$ s" test/lib" TEST-PATH-BUF JOIN-PATH TEST-PATH-U !
   TEST-PATH-BUF TEST-PATH-U @ MAKE-DIRS
   TEST-ROOT$ s" src/core" TEST-PATH-BUF JOIN-PATH TEST-PATH-U !
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

: TEST-ADD-WHOLE-CORE-EXEMPTION ( ptr u8 n ptr u8 n -- ) {: name:ptr nameu:n path:ptr pathu:n :}
   name nameu TEST-GLOBAL-SOURCE
   path pathu TEST-ADD-SOURCE-SECTION ;

: TEST-CORE-EXEMPTIONS ( -- )
   TEST-DIFF-RESET
   s" PRELUDE-GLOBAL" s" lib/prelude.f" TEST-ADD-WHOLE-CORE-EXEMPTION
   s" UTIL-GLOBAL" s" src/core/util.f" TEST-ADD-WHOLE-CORE-EXEMPTION
   s" SUMTYPE-GLOBAL" s" src/core/sumtype.f" TEST-ADD-WHOLE-CORE-EXEMPTION
   s" ROLE-GLOBAL" s" src/core/roles.f" TEST-ADD-WHOLE-CORE-EXEMPTION
   s" STRUCTURE-GLOBAL" s" src/core/structures.f" TEST-ADD-WHOLE-CORE-EXEMPTION
   s" ENUM-GLOBAL" s" src/core/enums.f" TEST-ADD-WHOLE-CORE-EXEMPTION
   TEST-EXPECT-CLEAN
   TEST-SOURCE-RESET
   s" : DEFTYPE ( -- ) ;" TEST-SOURCE-LINE
   s" : UNRELATED ( -- ) ;" TEST-SOURCE-LINE
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

\ ---- declaration-grammar fixture suites -------------------------------------
\ The second principled category.  These files test the global declaration
\ grammar, so a global declaration is the thing under test and packaging it
\ would delete the proof rather than satisfy the rule.  The pins below fix the
\ exact list, and the hostile cases prove the category cannot spread: a test
\ file that is not listed still reports, and the category refuses any path
\ outside test/ so a library or engine source can never be admitted through it.

: TEST-FIXTURE-FAMILY-SOURCE ( ptr u8 n -- )   \ a top-level family declaration
   TEST-SOURCE-RESET
   s" NEWTYPE " TEST-SOURCE+ TEST-SOURCE+ s"  0" TEST-SOURCE-LINE ;

: TEST-FIXTURE-AT ( ptr u8 n -- ) {: path:ptr pathu:n :}
   s" pdlfam" TEST-FIXTURE-FAMILY-SOURCE
   TEST-DIFF-RESET path pathu TEST-ADD-SOURCE-SECTION ;

: TEST-GRAMMAR-FIXTURE-LIST ( -- )   \ every listed path is admitted
   TEST-DIFF-RESET
   s" pdlfam" TEST-FIXTURE-FAMILY-SOURCE
   s" test/type-decl-suite.f" TEST-ADD-SOURCE-SECTION
   s" test/internal-word-gate.f" TEST-ADD-SOURCE-SECTION
   s" test/extent-substrate-probe.f" TEST-ADD-SOURCE-SECTION
   s" test/extent-product-test.f" TEST-ADD-SOURCE-SECTION
   s" test/typed-storage-test.f" TEST-ADD-SOURCE-SECTION
   s" test/cast-suite.f" TEST-ADD-SOURCE-SECTION
   s" test/cast-negative-suite.f" TEST-ADD-SOURCE-SECTION
   s" test/layout-buffer.f" TEST-ADD-SOURCE-SECTION
   s" test/layout-defer.f" TEST-ADD-SOURCE-SECTION
   s" test/engine-suite.f" TEST-ADD-SOURCE-SECTION
   TEST-EXPECT-CLEAN ;

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
   s" the category never admits a library source, however it is spelled" T-LABEL
   s" lib/process-pty-handle.f" TEST-FIXTURE-AT
   1 TEST-EXPECT-FINDINGS
   s" a global colon word in an unlisted test file still reports" T-LABEL
   s" TEST-LEAK" TEST-GLOBAL-SOURCE
   TEST-DIFF-RESET s" test/other-suite.f" TEST-ADD-SOURCE-SECTION
   1 TEST-EXPECT-FINDINGS ;

: TEST-WRITE-FIXTURE-OWNER-LOSS ( -- )
   TEST-SOURCE-RESET
   s" NEWTYPE pdlfam 0" TEST-SOURCE-LINE
   s" test/type-decl-suite.f" TEST-WRITE-SOURCE ;

: TEST-FIXTURE-OWNER-LOSS-DIFF ( -- )
   s" test/type-decl-suite.f" TEST-MODIFY-HEAD
   s" @@ -1,3 +1 @@" TEST-DIFF+ TEST-LF
   s" -package PDLFIX" TEST-DIFF+ TEST-LF
   s"  NEWTYPE pdlfam 0" TEST-DIFF+ TEST-LF
   s" -;package" TEST-DIFF+ TEST-LF ;

: TEST-GRAMMAR-FIXTURE-SCOPE-DELTA ( -- )
   \ FINISH-DEFINITION checks SCOPE-DELTA before the category, so a deleted
   \ package boundary around a fixture is reported even in a listed file: the
   \ category admits a plain global declaration, never a scope change.
   s" a deleted package boundary in a listed file is still reported" T-LABEL
   TEST-WRITE-FIXTURE-OWNER-LOSS
   TEST-DIFF-RESET TEST-FIXTURE-OWNER-LOSS-DIFF
   1 TEST-EXPECT-FINDINGS ;

: TEST-GRAMMAR-FIXTURES ( -- )
   s" every listed grammar-fixture suite is admitted" T-LABEL
   TEST-GRAMMAR-FIXTURE-LIST
   TEST-GRAMMAR-FIXTURE-HOSTILE
   TEST-GRAMMAR-FIXTURE-SCOPE-DELTA ;

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

: TEST-WRITE-GLOBAL-BODY-SOURCE ( -- )
   TEST-SOURCE-RESET
   s" : LEGACY ( -- n )" TEST-SOURCE-LINE
   s"    1" TEST-SOURCE-LINE
   s"    2 +" TEST-SOURCE-LINE
   s" ;" TEST-SOURCE-LINE
   s" lib/global-body.f" TEST-WRITE-SOURCE ;

: TEST-GLOBAL-BODY-DIFF ( -- )
   s" lib/global-body.f" TEST-MODIFY-HEAD
   s" @@ -1,3 +1,4 @@" TEST-DIFF+ TEST-LF
   s"  : LEGACY ( -- n )" TEST-DIFF+ TEST-LF
   s"     1" TEST-DIFF+ TEST-LF
   s" +   2 +" TEST-DIFF+ TEST-LF
   s"  ;" TEST-DIFF+ TEST-LF ;

: TEST-CHANGED-GLOBAL-BODY ( -- )
   TEST-WRITE-GLOBAL-BODY-SOURCE
   TEST-DIFF-RESET TEST-GLOBAL-BODY-DIFF
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
   MAPPING-PEAK @ 3 T=
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
   TEST-CORE-EXEMPTIONS
   TEST-GRAMMAR-FIXTURES
   TEST-TYPE-FAMILY-EXEMPTION
   TEST-CHECKER-EXEMPTION
   TEST-ERROR-VOCABULARY
   TEST-POSITIVES
   TEST-DELETED-OWNER
   TEST-ZERO-COUNT-OWNER-DELETION
   TEST-DELETION-TO-EMPTY
   TEST-CHANGED-GLOBAL-BODY
   TEST-MULTILINE-COMMENT-CANCELLATION
   TEST-DELETED-TOKEN-IN-DEFINITION
   TEST-BODYLESS-RENAME
   TEST-STALE-FAILS-CLOSED
   TEST-MALFORMED-REUSE
   TEST-ONE-ALLOCATION-REUSE
   TEST-TWO-ALLOCATION-REUSE
   CLEANUP-RUN
   TEST-ROOT$ EXISTS? TFALSE
   T-REPORT
   s" package-diff-lint-test: ok" type cr ;

TEST-MAIN

;package
