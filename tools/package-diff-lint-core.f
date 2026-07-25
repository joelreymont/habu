\ package-diff-lint-core.f - enforce package ownership on changed Forth definitions.
\
\ The unified-diff grammar belongs to tools/lint/diff.f.  Each canonical section
\ is parsed once to verify the new side and reconstruct the complete old source,
\ then replayed to align genuine old-side package transitions with new lines.
\ Both complete sources are lexed: hunk context and isolated deleted lines cannot
\ prove package scope across multiline comments, strings, or definitions.
\
\ Load after the lint lexer, memory, filesystem, and DIFF packages.

require lib/adt/option.f
require lib/memory.f
require lib/vector.f
require lib/fs.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f
require tools/lint/source-lex.f
require tools/lint/diff.f

package PACKAGE-DIFF
private

32 constant NUM-CAP
1 constant COLON-BLOCK
2 constant SUMTYPE-BLOCK
3 constant PRODUCT-BLOCK
4 constant ENUM-BLOCK
5 constant STRUCTURE-BLOCK
6 constant VALUE-RECORD-BLOCK
7 constant LOW-STRUCTURE-BLOCK
8 constant DATA-DEFINITION
10 constant LF-C
45 constant DASH-C
47 constant SLASH-C
48 constant ZERO-C
57 constant NINE-C
58 constant COLON-C
65 constant UPPER-A-C
69 constant UPPER-E-C
90 constant UPPER-Z-C
2 constant ERR-PREFIX-U   \ length of the mandatory `E-` error-name prefix

create NUM NUM-CAP allot
create ONE 1 allot
create ROOT-BUF FS-PATH-CAP allot
create FILE-BUF FS-PATH-CAP allot
create FULL-BUF FS-PATH-CAP allot
create PACKAGE-BUF FS-PATH-CAP allot

variable ROOT-U
variable FILE-U
variable FULL-U
variable PACKAGE-U
variable SOURCE-A
variable SOURCE-U
variable SOURCE-CAP
variable OLD-A
variable OLD-U
variable OLD-CAP
variable MARK-A
variable MARK-CAP
variable MARK-U
variable NEW-SLOTS
variable OLD-SLOTS
variable MAPPING-PEAK
variable FAIL-NEXT-MARK-ALLOC
variable FAIL-NEXT-OLD-ALLOC
variable BAD
variable SECTION-ACTIVE
variable SECTION-SEEN
variable WHOLE-CHANGED
variable SOURCE-LINE
variable SOURCE-OFF
variable NEW-LINE
variable OLD-LINE
variable LEX-I
variable PACKAGE-OPEN
variable SCOPE-DELTA
variable SCAN-LINE
variable DEF-OPEN
variable DEF-KIND
variable DEF-DEFINER-I
variable DEF-NAME-I
variable DEF-START-LINE
variable DEF-PACKAGED
variable DEF-TAIL-ADDED
variable NUM-I
variable SCAN-START
variable STEM-START
variable STEM-END
variable INPUT-A
variable INPUT-U
variable SECTION-START
variable ARTIFACT-LINE-START
variable ARTIFACT-I
variable REPLAY-SECTION-SEEN
variable FILE-USED

: PTR-SLOT ( ptr a -- ptr ptr u8 )
   0 ptr-field ;

: SOURCE-PTR ( -- ptr ptr u8 )
   SOURCE-A PTR-SLOT ;

: MARK-PTR ( -- ptr ptr u8 )
   MARK-A PTR-SLOT ;

: OLD-PTR ( -- ptr ptr u8 )
   OLD-A PTR-SLOT ;

: INPUT-PTR ( -- ptr ptr u8 )
   INPUT-A PTR-SLOT ;

: SOURCE-A@ ( -- ptr u8 )
   SOURCE-PTR @ ;

: MARK-A@ ( -- ptr u8 )
   MARK-PTR @ ;

: OLD-A@ ( -- ptr u8 )
   OLD-PTR @ ;

: ROOT$ ( -- ptr u8 n )
   ROOT-BUF ROOT-U @ ;

: FILE$ ( -- ptr u8 n )
   FILE-BUF FILE-U @ ;

: FULL$ ( -- ptr u8 n )
   FULL-BUF FULL-U @ ;

: PACKAGE$ ( -- ptr u8 n )
   PACKAGE-BUF PACKAGE-U @ ;

: SOURCE$ ( -- ptr u8 n )
   SOURCE-A@ SOURCE-U @ ;

: OLD$ ( -- ptr u8 n )
   OLD-A@ OLD-U @ ;

: INPUT$ ( -- ptr u8 n )
   INPUT-PTR @ INPUT-U @ ;

: LIVE-MAPPING# ( -- n )
   0
   SOURCE-CAP @ 0<> if 1+ then
   MARK-CAP @ 0<> if 1+ then
   OLD-CAP @ 0<> if 1+ then ;

: NOTE-MAPPING-PEAK ( -- )
   LIVE-MAPPING# MAPPING-PEAK @ max MAPPING-PEAK ! ;

: COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr up:ptr :}
   u 0 < if E-FS-CAPACITY throw then
   u FS-PATH-CAP > if E-FS-CAPACITY throw then
   a dst u BYTE-COPY
   u up ! ;

: U$ ( n -- ptr u8 n ) {: u:n :}
   NUM-CAP NUM-I !
   u 0= if
      NUM-I @ 1- NUM-I !
      48 NUM NUM-I @ + c!
      NUM NUM-I @ + 1 exit
   then
   u begin dup 0 > while
      dup 10 mod 48 +
      NUM-I @ 1- NUM-I !
      NUM NUM-I @ + c!
      10 /
   repeat drop
   NUM NUM-I @ + NUM-CAP NUM-I @ - ;

: OUT ( ptr u8 n -- )
   1 -rot LINT-OUT-WRITE ;

: OUT-C ( n -- ) {: c:n :}
   c ONE c!
   ONE 1 OUT ;

: BAD+ ( -- )
   BAD @ 1+ BAD ! ;

: FORTH? ( -- bool )
   FILE$ s" .f" LINT-ENDS-WITH? if true exit then
   FILE$ s" .fs" LINT-ENDS-WITH? ;

\ Exact files whose global (unpackaged) definitions implement the core
\ language/prelude, so a changed global there is not an ownership fault.  All are
\ entirely global except src/core/type-family.f, which also opens inner packages:
\ it is exempt only for its global surface, and only until the TFAM sealing work
\ (dot habu-tfam-2b-sealed-1b77662c) seals that surface into packages, when this
\ entry must be removed.  Package-boundary changes are still reported for every
\ file here (FINISH-DEFINITION checks SCOPE-DELTA before this allowlist).  Files
\ with only one global declarer are handled by GLOBAL-SURFACE? below, so an
\ unrelated global added beside DEFTYPE or STRUCTURE is still rejected.
: GLOBAL-IMPLEMENTATION? ( -- bool )
   FILE$ s" lib/prelude.f" LINT-STR= if true exit then
   FILE$ s" src/core/sumtype.f" LINT-STR= if true exit then
   FILE$ s" src/core/roles.f" LINT-STR= if true exit then
   FILE$ s" src/core/structures.f" LINT-STR= if true exit then
   FILE$ s" src/core/type-family.f" LINT-STR= if true exit then  \ core surface, interim; see header
   FILE$ s" src/core/enums.f" LINT-STR= ;

: SOURCE-ALLOC-NEED ( n -- n )
   dup 0 <= if drop 1 then ;

: SOURCE-ALLOC ( n -- )
   SOURCE-ALLOC-NEED MEM-ALLOC-64K-SPAN SOURCE-CAP ! SOURCE-PTR !
   NOTE-MAPPING-PEAK ;

: MARK-ALLOC ( n -- )
   FAIL-NEXT-MARK-ALLOC @ if
      false FAIL-NEXT-MARK-ALLOC !
      drop E-MEM-SIZE throw
   then
   SOURCE-ALLOC-NEED MEM-ALLOC-64K-SPAN MARK-CAP ! MARK-PTR !
   NOTE-MAPPING-PEAK ;

: OLD-ALLOC ( n -- )
   FAIL-NEXT-OLD-ALLOC @ if
      false FAIL-NEXT-OLD-ALLOC !
      drop E-MEM-SIZE throw
   then
   SOURCE-ALLOC-NEED MEM-ALLOC-64K-SPAN OLD-CAP ! OLD-PTR !
   NOTE-MAPPING-PEAK ;

: SOURCE-RELEASE ( -- )
   SOURCE-CAP @ 0= if exit then
   SOURCE-A@ {: a:ptr :}
   SOURCE-CAP @ {: cap:n :}
   0 SOURCE-CAP !
   a cap MEM:BYTES-ALLOC-LEN MEM:RELEASE-BYTES ;

: MARK-RELEASE ( -- )
   MARK-CAP @ 0= if exit then
   MARK-A@ {: a:ptr :}
   MARK-CAP @ {: cap:n :}
   0 MARK-CAP !
   a cap MEM:BYTES-ALLOC-LEN MEM:RELEASE-BYTES ;

: OLD-RELEASE ( -- )
   OLD-CAP @ 0= if exit then
   OLD-A@ {: a:ptr :}
   OLD-CAP @ {: cap:n :}
   0 OLD-CAP !
   a cap MEM:BYTES-ALLOC-LEN MEM:RELEASE-BYTES ;

: CLEANUP-COMBINE ( n n -- n )
   over 0 <> if drop exit then nip ;

\ Release every mapping even if one munmap fails; an existing primary error wins.
: RELEASE-BUFFERS ( n -- )
   [: SOURCE-RELEASE ;] catch CLEANUP-COMBINE
   [: MARK-RELEASE ;] catch CLEANUP-COMBINE
   [: OLD-RELEASE ;] catch CLEANUP-COMBINE
   dup 0 <> if throw then drop ;

: MARK-CLEAR ( n -- ) {: need:n :}
   0 begin dup need < while
      0 MARK-A@ over + c!
      1+
   repeat drop ;

: ADD-SIZE ( n n -- n ) {: a:n b:n :}
   a 0 < b 0 < or if E-MEM-SIZE throw then
   a MEM-MAX-N b - > if E-MEM-SIZE throw then
   a b + ;

: DOUBLE-SIZE ( n -- n )
   dup 0 < if drop E-MEM-SIZE throw then
   dup MEM-MAX-N 2 / > if drop E-MEM-SIZE throw then
   2 * ;

: BUILD-FULL-PATH ( -- )
   ROOT$ FILE$ FULL-BUF JOIN-PATH FULL-U ! ;

: LOAD-SOURCE ( -- )
   BUILD-FULL-PATH
   FULL$ FILE-SIZE {: size:n :}
   size 0 < if E-MEM-SIZE throw then
   size 2 ADD-SIZE NEW-SLOTS !
   size INPUT-U @ ADD-SIZE 2 ADD-SIZE OLD-SLOTS !
   NEW-SLOTS @ DOUBLE-SIZE OLD-SLOTS @ ADD-SIZE MARK-U !
   size SOURCE-ALLOC
   FULL$ SOURCE-A@ SOURCE-CAP @ READ-ALL SOURCE-U !
   SOURCE-U @ size <> if E-DIFF-SYNTAX throw then
   MARK-U @ dup MARK-ALLOC MARK-CLEAR
   OLD-SLOTS @ 2 - OLD-ALLOC
   0 OLD-U ! ;

: LINE-OFF ( n -- n ) {: line:n :}
   line 0 <= if E-DIFF-SYNTAX throw then
   line NEW-SLOTS @ >= if E-DIFF-SYNTAX throw then
   line 2 * ;

: OLD-LINE-OFF ( n -- n ) {: line:n :}
   line 0 <= if E-DIFF-SYNTAX throw then
   line OLD-SLOTS @ >= if E-DIFF-SYNTAX throw then
   NEW-SLOTS @ 2 * line + ;

: MARK-LINE ( n -- ) {: line:n :}
   1 MARK-A@ line LINE-OFF + c! ;

: ADDED? ( n -- bool )
   dup 0 <= if drop false exit then
   LINE-OFF MARK-A@ + c@ 0<> ;

: DELTA@ ( n -- n )
   LINE-OFF 1+ MARK-A@ + c@
   dup 127 > if 256 - then ;

: DELTA+ ( n n -- ) {: line:n amount:n :}
   line DELTA@ amount + {: next:n :}
   next -127 < next 127 > or if E-DIFF-SYNTAX throw then
   next 0 < if next 256 + else next then
   MARK-A@ line LINE-OFF + 1+ c! ;

: OLD-DELTA@ ( n -- n )
   OLD-LINE-OFF MARK-A@ + c@
   dup 127 > if 256 - then ;

: OLD-DELTA+ ( n n -- ) {: line:n amount:n :}
   line OLD-DELTA@ amount + {: next:n :}
   next -1 < next 1 > or if E-DIFF-SYNTAX throw then
   next 0 < if next 256 + else next then
   MARK-A@ line OLD-LINE-OFF + c! ;

: OLD+ ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 < if E-MEM-SIZE throw then
   OLD-U @ u ADD-SIZE OLD-SLOTS @ 2 - > if E-MEM-SIZE throw then
   a OLD-A@ OLD-U @ + u BYTE-COPY
   OLD-U @ u + OLD-U ! ;

: OLD-LINE+ ( ptr u8 n -- )
   OLD+
   LF-C ONE c!
   ONE 1 OLD+ ;

: SOURCE-END? ( -- bool )
   SOURCE-OFF @ SOURCE-U @ >= ;

: SOURCE-LINE$ ( -- ptr u8 n )
   SOURCE-END? if E-DIFF-SYNTAX throw then
   SOURCE-OFF @ {: start:n :}
   start begin dup SOURCE-U @ < while
      SOURCE-A@ over + c@ LF-C = if
         SOURCE-A@ start + swap start - exit
      then
      1+
   repeat drop
   SOURCE-A@ start + SOURCE-U @ start - ;

: SOURCE-LINE+ ( -- )
   SOURCE-END? if E-DIFF-SYNTAX throw then
   begin SOURCE-OFF @ SOURCE-U @ < while
      SOURCE-A@ SOURCE-OFF @ + c@ {: c:n :}
      SOURCE-OFF @ 1+ SOURCE-OFF !
      c LF-C = if
         SOURCE-LINE @ 1+ SOURCE-LINE ! exit
      then
   repeat
   SOURCE-LINE @ 1+ SOURCE-LINE ! ;

: SOURCE-SEEK ( n -- ) {: target:n :}
   target SOURCE-LINE @ < if E-DIFF-SYNTAX throw then
   begin SOURCE-LINE @ target < while
      SOURCE-OFF @ {: start:n :}
      SOURCE-LINE+
      SOURCE-A@ start + SOURCE-OFF @ start - OLD+
   repeat ;

: LINE-CHECK ( ptr u8 n -- ) {: a:ptr u:n :}
   SOURCE-LINE$ a u LINT-STR= 0= if E-DIFF-SYNTAX throw then ;

: CONTENT-LINE ( bool -- ) {: added:bool :}
   DIFF:CONTENT$ LINE-CHECK
   added if
      NEW-LINE @ MARK-LINE
      SOURCE-LINE+
   else
      SOURCE-OFF @ {: start:n :}
      SOURCE-LINE+
      SOURCE-A@ start + SOURCE-OFF @ start - OLD+
   then
   NEW-LINE @ 1+ NEW-LINE ! ;

: COPY-SOURCE-REST ( -- )
   SOURCE-OFF @ SOURCE-U @ > if E-DIFF-SYNTAX throw then
   SOURCE-A@ SOURCE-OFF @ + SOURCE-U @ SOURCE-OFF @ - OLD+
   SOURCE-U @ SOURCE-OFF ! ;

: CI-PREFIX? ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u:n p:ptr pu:n :}
   u pu < if false exit then
   a pu p pu LINT-STR=CI ;

: OWNER-PREFIX? ( ptr u8 n ptr u8 n -- bool ) {: name:ptr nameu:n owner:ptr owneru:n :}
   owneru 0= if false exit then
   nameu owneru <= if false exit then
   name owneru owner owneru CI-PREFIX? 0= if false exit then
   name owneru + c@ DASH-C = ;

: STEM$ ( -- ptr u8 n )
   0 STEM-START !
   FILE-U @ STEM-END !
   FILE$ s" .fs" LINT-ENDS-WITH? if
      STEM-END @ 3 - STEM-END !
   else
      FILE$ s" .f" LINT-ENDS-WITH? if STEM-END @ 2 - STEM-END ! then
   then
   0 begin dup STEM-END @ < while
      FILE-BUF over + c@ SLASH-C = if dup 1+ STEM-START ! then
      1+
   repeat drop
   FILE-BUF STEM-START @ + STEM-END @ STEM-START @ - ;

: WORD? ( n -- bool )
   dup 0 < if drop false exit then
   dup LINT-LEX:COUNT >= if drop false exit then
   LINT-LEX:KIND@ LINT-LEX:WORD = ;

: TOK=CI ( n ptr u8 n -- bool ) {: k:n a:ptr u:n :}
   k WORD? 0= if false exit then
   k LINT-LEX:TOKEN a u LINT-STR=CI ;

\ Case-sensitive token match, for the few rules that pin an exact spelling
\ rather than a Forth word identity.
: TOK= ( n ptr u8 n -- bool ) {: k:n a:ptr u:n :}
   k WORD? 0= if false exit then
   k LINT-LEX:TOKEN a u LINT-STR= ;

\ Audited publication inventory.  The native forms come from the engine's
\ dictionary definers and docs/typed-top-level.md.  The type/storage forms are
\ the complete UNSAFE-TOK?/top-level declaration set in checker.f.  The
\ remaining project forms are the transitive defining words found by auditing
\ every executable `create` and generated-definition `evaluate` owner in the
\ Forth tree.  Registry grammars that do not publish dictionary words (PRIM:,
\ PPRIM:, SUITE/GROUP, VJP:, GRID:/WHERE) are intentionally absent.
: COLON-DEFINER? ( n -- bool ) {: k:n :}
   k s" :" TOK=CI if true exit then
   k s" +:" TOK=CI if true exit then
   k s" CHECKED:" TOK=CI if true exit then
   k s" TRUSTED:" TOK=CI if true exit then
   k s" KERNEL:" TOK=CI if true exit then
   k s" CAST:" TOK=CI if true exit then
   k s" MODEL:" TOK=CI ;

: BLOCK-DEFINER-KIND ( n -- n ) {: k:n :}
   k s" SUMTYPE" TOK=CI if SUMTYPE-BLOCK exit then
   k s" PRODUCT" TOK=CI if PRODUCT-BLOCK exit then
   k s" ENUM" TOK=CI if ENUM-BLOCK exit then
   k s" STRUCTURE" TOK=CI if STRUCTURE-BLOCK exit then
   k s" VALUE-RECORD" TOK=CI if VALUE-RECORD-BLOCK exit then
   k s" BEGIN-STRUCTURE" TOK=CI if LOW-STRUCTURE-BLOCK exit then
   0 ;

: NATIVE-DATA-DEFINER? ( n -- bool ) {: k:n :}
   k s" constant" TOK=CI if true exit then
   k s" 2constant" TOK=CI if true exit then
   k s" fconstant" TOK=CI if true exit then
   k s" variable" TOK=CI if true exit then
   k s" 2variable" TOK=CI if true exit then
   k s" fvariable" TOK=CI if true exit then
   k s" create" TOK=CI if true exit then
   k s" value" TOK=CI if true exit then
   k s" defer" TOK=CI ;

: STORAGE-DEFINER? ( n -- bool ) {: k:n :}
   k s" LAYOUT-BUFFER" TOK=CI if true exit then
   k s" DEFER-LAYOUT-BUFFER" TOK=CI if true exit then
   k s" TYPED-BUFFER" TOK=CI if true exit then
   k s" TYPED-VARIABLE" TOK=CI if true exit then
   k s" PTR-VARIABLE" TOK=CI if true exit then
   k s" PTR-FIELD:" TOK=CI if true exit then
   k s" CFIELD:" TOK=CI if true exit then
   k s" +FIELD" TOK=CI ;

: TYPE-DEFINER? ( n -- bool ) {: k:n :}
   k s" TYPEFAMILY" TOK=CI if true exit then
   k s" DEFTYPE" TOK=CI if true exit then
   k s" DEFLINEAR" TOK=CI if true exit then
   k s" ENUM+" TOK=CI if true exit then
   k s" ENUM4+" TOK=CI ;

: PROJECT-DATA-DEFINER? ( n -- bool ) {: k:n :}
   k s" BUFFER:" TOK=CI if true exit then
   k s" BUFFER" TOK=CI if true exit then
   k s" BUFFER-E" TOK=CI if true exit then
   k s" CODEGEN:BUFFER" TOK=CI if true exit then
   k s" CODEGEN:BUFFER-E" TOK=CI if true exit then
   k s" TASK" TOK=CI if true exit then
   k s" +USER" TOK=CI if true exit then
   k s" FACILITY" TOK=CI if true exit then
   k s" TASK:TASK" TOK=CI if true exit then
   k s" TASK:+USER" TOK=CI if true exit then
   k s" TASK:FACILITY" TOK=CI if true exit then
   k s" TR-FILES:" TOK=CI if true exit then
   k s" GE-FILES:" TOK=CI if true exit then
   k s" IOP:" TOK=CI if true exit then
   k s" CONST" TOK=CI if true exit then
   k s" ARR" TOK=CI ;

: MAKI-DEFINER? ( n -- bool ) {: k:n :}
   k s" EXTENT:" TOK=CI if true exit then
   k s" FREE-EXTENT:" TOK=CI if true exit then
   k s" EXTPROD:" TOK=CI if true exit then
   k s" TENSOR:" TOK=CI if true exit then
   k s" ITENSOR:" TOK=CI if true exit then
   k s" SPEC:" TOK=CI ;

: DATA-DEFINER? ( n -- bool ) {: k:n :}
   k NATIVE-DATA-DEFINER? if true exit then
   k STORAGE-DEFINER? if true exit then
   k TYPE-DEFINER? if true exit then
   k PROJECT-DATA-DEFINER? if true exit then
   k MAKI-DEFINER? ;

: DEFINER-KIND ( n -- n ) {: k:n :}
   k COLON-DEFINER? if COLON-BLOCK exit then
   k BLOCK-DEFINER-KIND dup 0<> if exit then drop
   k DATA-DEFINER? if DATA-DEFINITION exit then
   0 ;

: CLOSE? ( n n -- bool ) {: k:n kind:n :}
   kind COLON-BLOCK = if k s" ;" TOK=CI exit then
   kind SUMTYPE-BLOCK = if k s" ;SUMTYPE" TOK=CI exit then
   kind PRODUCT-BLOCK = if k s" ;PRODUCT" TOK=CI exit then
   kind ENUM-BLOCK = if k s" ;ENUM" TOK=CI exit then
   kind STRUCTURE-BLOCK = if k s" ;STRUCTURE" TOK=CI exit then
   kind VALUE-RECORD-BLOCK = if k s" END-VALUE-RECORD" TOK=CI exit then
   kind LOW-STRUCTURE-BLOCK = if k s" END-STRUCTURE" TOK=CI exit then
   false ;

: ADDED-RANGE? ( n n -- bool ) {: first:n last:n :}
   first begin dup last <= while
      dup ADDED? if drop true exit then
      1+
   repeat drop false ;

: REPORT-HEAD ( n ptr u8 n -- ) {: k:n code:ptr codeu:n :}
   BAD+
   code codeu OUT
   32 OUT-C FILE$ OUT COLON-C OUT-C
   k LINT-LEX:LINE@ U$ OUT COLON-C OUT-C k LINT-LEX:COL@ U$ OUT
   s" : `" OUT k LINT-LEX:TOKEN OUT s" ` " OUT ;

: REPORT-GLOBAL ( -- )
   DEF-NAME-I @ s" E-PACKAGE-OWNERSHIP" REPORT-HEAD
   s" defines a changed module word outside a package" OUT LF-C OUT-C ;

: REPORT-OWNER-PREFIX ( -- )
   DEF-NAME-I @ s" E-REDUNDANT-PACKAGE-PREFIX" REPORT-HEAD
   s" repeats its package owner `" OUT PACKAGE$ OUT s" `" OUT LF-C OUT-C ;

: REPORT-STEM-PREFIX ( -- )
   DEF-NAME-I @ s" E-REDUNDANT-FILE-PREFIX" REPORT-HEAD
   s" repeats its file owner `" OUT STEM$ OUT s" `" OUT LF-C OUT-C ;

: CHECK-PREFIX ( -- )
   DEF-TAIL-ADDED @ 0= if exit then
   DEF-NAME-I @ LINT-LEX:TOKEN {: name:ptr nameu:n :}
   name nameu PACKAGE$ OWNER-PREFIX? if REPORT-OWNER-PREFIX exit then
   name nameu STEM$ OWNER-PREFIX? if REPORT-STEM-PREFIX then ;

: ERR-CHAR? ( n -- bool ) {: c:n :}
   c UPPER-A-C >= c UPPER-Z-C <= and if true exit then
   c ZERO-C >= c NINE-C <= and if true exit then
   c DASH-C = ;

\ An error-code name is `E-` followed by at least one more character, written
\ only in capitals, digits, and hyphens.  A lower-case letter anywhere, a
\ different prefix, or the bare prefix alone is not an error-code name.
: ERR-NAME? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u ERR-PREFIX-U <= if false exit then
   a 0 BYTE@ UPPER-E-C <> if false exit then
   a 1 BYTE@ DASH-C <> if false exit then
   0 begin dup u < while
      dup a + c@ ERR-CHAR? 0= if drop false exit then
      1+
   repeat drop true ;

\ lib/errors.f holds the repository's shared error vocabulary: every package
\ throws and catches these codes, so the codes themselves stay global words.
\ That file is not globally exempt, though.  It admits exactly one declaration
\ shape -- an error-code name defined by the lower-case `constant` definer --
\ and reports everything else: another name shape, another defining word,
\ `CONSTANT` in capitals, and the same declaration in any other file.
\ FINISH-DEFINITION checks SCOPE-DELTA before it consults this admission, so a
\ deleted or moved `package`/`;package` boundary that leaves a constant
\ unpackaged at its scan point is still reported.  A complete block whose
\ opener and closer both change nets zero delta and is not a scope change.
: ERR-VOCAB? ( -- bool )
   FILE$ s" lib/errors.f" LINT-STR= 0= if false exit then
   DEF-DEFINER-I @ s" constant" TOK= 0= if false exit then
   DEF-NAME-I @ LINT-LEX:TOKEN ERR-NAME? ;

: GLOBAL-SURFACE? ( -- bool )
   GLOBAL-IMPLEMENTATION? if true exit then
   ERR-VOCAB? if true exit then
   FILE$ s" lib/type/deftype.f" LINT-STR= if
      DEF-NAME-I @ s" DEFTYPE" TOK=CI exit
   then
   FILE$ s" src/core/structure-decl.f" LINT-STR= if
      DEF-NAME-I @ s" STRUCTURE" TOK=CI exit
   then
   false ;

\ A changed unpackaged definition normally loses its package owner.  A non-zero
\ SCOPE-DELTA means a `package`/`;package` boundary around this definition was
\ added or deleted in this diff, so its ownership genuinely changed: that is
\ reported for every file, including the allowlisted core surface (type-family.f
\ still opens inner packages).  Only a plain body change or whole-file change of
\ an already-global definition is exempt, and only on that surface.
: FINISH-DEFINITION ( n -- ) {: last-line:n :}
   DEF-PACKAGED @ if
      CHECK-PREFIX
   else
      SCOPE-DELTA @ 0<> if
         REPORT-GLOBAL
      else
         DEF-START-LINE @ last-line ADDED-RANGE? WHOLE-CHANGED @ or
         GLOBAL-SURFACE? 0= and if REPORT-GLOBAL then
      then
   then
   false DEF-OPEN ! ;

: START-DEFINITION ( n n -- ) {: k:n kind:n :}
   k 1+ dup WORD? 0= if drop E-DIFF-SYNTAX throw then {: namei:n :}
   kind DEF-KIND !
   k DEF-DEFINER-I !
   namei DEF-NAME-I !
   k LINT-LEX:LINE@ DEF-START-LINE !
   PACKAGE-OPEN @ DEF-PACKAGED !
   k LINT-LEX:LINE@ ADDED? namei LINT-LEX:LINE@ ADDED? or DEF-TAIL-ADDED !
   kind DATA-DEFINITION = if
      namei LINT-LEX:LINE@ FINISH-DEFINITION
   else
      true DEF-OPEN !
   then
   namei 1+ LEX-I ! ;

: PACKAGE-SET ( n -- ) {: namei:n :}
   namei WORD? 0= if E-DIFF-SYNTAX throw then
   PACKAGE-OPEN @ if E-DIFF-SYNTAX throw then
   namei LINT-LEX:TOKEN PACKAGE-BUF PACKAGE-U COPY!
   true PACKAGE-OPEN ! ;

: PACKAGE-CLEAR ( -- )
   PACKAGE-OPEN @ 0= if E-DIFF-SYNTAX throw then
   false PACKAGE-OPEN !
   0 PACKAGE-U ! ;

: PACKAGE-TOKEN ( n -- bool ) {: k:n :}
   k s" package" TOK=CI if
      k LINT-LEX:LINE@ ADDED? if SCOPE-DELTA @ 1+ SCOPE-DELTA ! then
      k 1+ PACKAGE-SET
      k 2 + LEX-I !
      true exit
   then
   k s" ;package" TOK=CI if
      k LINT-LEX:LINE@ ADDED? if SCOPE-DELTA @ 1- SCOPE-DELTA ! then
      PACKAGE-CLEAR
      k 1+ LEX-I !
      true exit
   then
   false ;

: SCAN-TOKEN ( n -- ) {: k:n :}
   DEF-OPEN @ if
      k DEF-KIND @ CLOSE? if
         k LINT-LEX:LINE@ FINISH-DEFINITION
      then
      k 1+ LEX-I ! exit
   then
   k PACKAGE-TOKEN if exit then
   k DEFINER-KIND dup 0= if drop k 1+ LEX-I ! exit then
   k swap START-DEFINITION ;

: APPLY-DELETED-DELTA ( n -- ) {: line:n :}
   begin SCAN-LINE @ line <= while
      SCOPE-DELTA @ SCAN-LINE @ DELTA@ + SCOPE-DELTA !
      SCAN-LINE @ 1+ SCAN-LINE !
   repeat ;

: SCAN-DEFINITIONS ( -- )
   SOURCE$ LINT-LEX:SOURCE
   LINT-LEX:ERROR? if E-DIFF-SYNTAX throw then
   0 LEX-I !
   false PACKAGE-OPEN !
   0 SCOPE-DELTA !
   1 SCAN-LINE !
   false DEF-OPEN !
   0 PACKAGE-U !
   begin LEX-I @ LINT-LEX:COUNT < while
      LEX-I @ LINT-LEX:LINE@ APPLY-DELETED-DELTA
      LEX-I @ SCAN-TOKEN
   repeat
   DEF-OPEN @ 0<> PACKAGE-OPEN @ 0<> or if E-DIFF-SYNTAX throw then ;

: OLD-PACKAGE-TOKEN ( n -- bool ) {: k:n :}
   k s" package" TOK=CI if
      k 1+ WORD? 0= if E-DIFF-SYNTAX throw then
      PACKAGE-OPEN @ if E-DIFF-SYNTAX throw then
      k LINT-LEX:LINE@ 1 OLD-DELTA+
      true PACKAGE-OPEN !
      k 2 + LEX-I !
      true exit
   then
   k s" ;package" TOK=CI if
      PACKAGE-OPEN @ 0= if E-DIFF-SYNTAX throw then
      k LINT-LEX:LINE@ -1 OLD-DELTA+
      false PACKAGE-OPEN !
      k 1+ LEX-I !
      true exit
   then
   false ;

: OLD-START-DEFINITION ( n n -- ) {: k:n kind:n :}
   k 1+ dup WORD? 0= if drop E-DIFF-SYNTAX throw then {: namei:n :}
   kind DATA-DEFINITION = if
      namei 1+ LEX-I ! exit
   then
   kind DEF-KIND !
   true DEF-OPEN !
   namei 1+ LEX-I ! ;

: OLD-SCAN-TOKEN ( n -- ) {: k:n :}
   DEF-OPEN @ if
      k DEF-KIND @ CLOSE? if false DEF-OPEN ! then
      k 1+ LEX-I ! exit
   then
   k OLD-PACKAGE-TOKEN if exit then
   k DEFINER-KIND dup 0= if drop k 1+ LEX-I ! exit then
   k swap OLD-START-DEFINITION ;

: SCAN-OLD-BOUNDARIES ( -- )
   OLD$ LINT-LEX:SOURCE
   LINT-LEX:ERROR? if E-DIFF-SYNTAX throw then
   0 LEX-I !
   false PACKAGE-OPEN !
   false DEF-OPEN !
   begin LEX-I @ LINT-LEX:COUNT < while
      LEX-I @ OLD-SCAN-TOKEN
   repeat
   DEF-OPEN @ 0<> PACKAGE-OPEN @ 0<> or if E-DIFF-SYNTAX throw then ;

: WHOLE-CHANGE? ( DIFF:form -- bool )
   MATCH DIFF:form
      modify OF false ENDOF
      add-file OF false ENDOF
      delete-file OF false ENDOF
      rename OF true ENDOF
      copy OF true ENDOF
      mode OF false ENDOF
      binary OF false ENDOF
   ;MATCH ;

: BEGIN-SECTION ( ptr u8 n ptr u8 n DIFF:form bool -- )
   {: old:ptr oldu:n new:ptr newu:n kind:DIFF:form body:bool :}
   old oldu 2drop
   SECTION-SEEN @ if E-DIFF-SYNTAX throw then
   true SECTION-SEEN !
   new newu FILE-BUF FILE-U COPY!
   false SECTION-ACTIVE !
   kind WHOLE-CHANGE? WHOLE-CHANGED !
   newu 0= if exit then
   FORTH? 0= if exit then
   body 0= WHOLE-CHANGED @ 0= and if exit then
   LOAD-SOURCE
   1 SOURCE-LINE !
   0 SOURCE-OFF !
   1 NEW-LINE !
   true SECTION-ACTIVE ! ;

: HUNK ( n -- ) {: line:n :}
   line NEW-LINE !
   SECTION-ACTIVE @ if line SOURCE-SEEK then ;

: HUNK-NEW-LINE ( -- n )
   DIFF:HUNK-NEW-START
   DIFF:HUNK-NEW-COUNT 0= if 1 ADD-SIZE then ;

: DELETE-CONTENT ( -- )
   DIFF:CONTENT$ OLD-LINE+ ;

: EVENT ( DIFF:event -- )
   MATCH DIFF:event
      none OF ENDOF
      section OF
         DIFF:SECTION-OLD$ DIFF:SECTION-NEW$
         DIFF:SECTION-FORM DIFF:SECTION-BODY? BEGIN-SECTION
      ENDOF
      hunk OF HUNK-NEW-LINE HUNK ENDOF
      add OF SECTION-ACTIVE @ if true CONTENT-LINE then ENDOF
      context OF SECTION-ACTIVE @ if false CONTENT-LINE then ENDOF
      delete OF SECTION-ACTIVE @ if DELETE-CONTENT then ENDOF
   ;MATCH ;

: LINE ( ptr u8 n -- )
   DIFF:LINE EVENT ;

: SOURCE-LINES ( ptr u8 n -- ) {: a:ptr u:n :}
   a u DIFF:SOURCE-VALIDATE
   0 SCAN-START !
   0 begin dup u < while
      dup a + c@ LF-C = if
         a SCAN-START @ + over SCAN-START @ - LINE
         dup 1+ SCAN-START !
      then
      1+
   repeat drop ;

: REPLAY-BEGIN ( -- )
   REPLAY-SECTION-SEEN @ if E-DIFF-SYNTAX throw then
   true REPLAY-SECTION-SEEN !
   1 OLD-LINE !
   1 NEW-LINE ! ;

: REPLAY-HUNK ( n -- ) {: line:n :}
   line NEW-LINE @ < if E-DIFF-SYNTAX throw then
   line NEW-LINE @ - {: gap:n :}
   OLD-LINE @ gap ADD-SIZE OLD-LINE !
   line NEW-LINE ! ;

: REPLAY-DELETE ( -- )
   OLD-LINE @ OLD-DELTA@ negate {: amount:n :}
   amount 0<> if NEW-LINE @ amount DELTA+ then
   OLD-LINE @ 1 ADD-SIZE OLD-LINE ! ;

: REPLAY-EVENT ( DIFF:event -- )
   MATCH DIFF:event
      none OF ENDOF
      section OF REPLAY-BEGIN ENDOF
      hunk OF HUNK-NEW-LINE REPLAY-HUNK ENDOF
      add OF NEW-LINE @ 1 ADD-SIZE NEW-LINE ! ENDOF
      context OF
         OLD-LINE @ 1 ADD-SIZE OLD-LINE !
         NEW-LINE @ 1 ADD-SIZE NEW-LINE !
      ENDOF
      delete OF REPLAY-DELETE ENDOF
   ;MATCH ;

: REPLAY-LINE ( ptr u8 n -- )
   DIFF:LINE REPLAY-EVENT ;

: REPLAY-SOURCE-LINES ( ptr u8 n -- ) {: a:ptr u:n :}
   0 SCAN-START !
   0 begin dup u < while
      dup a + c@ LF-C = if
         a SCAN-START @ + over SCAN-START @ - REPLAY-LINE
         dup 1+ SCAN-START !
      then
      1+
   repeat drop ;

: REPLAY-DELETED-BOUNDARIES ( -- )
   DIFF:RESET
   false REPLAY-SECTION-SEEN !
   INPUT$ REPLAY-SOURCE-LINES
   DIFF:FINISH REPLAY-EVENT
   REPLAY-SECTION-SEEN @ 0= if E-DIFF-SYNTAX throw then ;

: FINISH-SECTION-WORK ( -- )
   DIFF:FINISH EVENT
   SECTION-SEEN @ 0= if E-DIFF-SYNTAX throw then
   SECTION-ACTIVE @ if
      COPY-SOURCE-REST
      SCAN-OLD-BOUNDARIES
      REPLAY-DELETED-BOUNDARIES
      SCAN-DEFINITIONS
   then
   DIFF:RESET ;

: PROCESS-SECTION-WORK ( -- )
   DIFF:RESET
   false SECTION-SEEN !
   false SECTION-ACTIVE !
   INPUT$ SOURCE-LINES
   FINISH-SECTION-WORK ;

: PROCESS-SECTION ( ptr u8 n -- )
   INPUT-U ! INPUT-PTR !
   [: PROCESS-SECTION-WORK ;] catch {: rc:n :}
   false SECTION-ACTIVE !
   rc RELEASE-BUFFERS ;

: ABORT-PARSE ( n -- ) {: rc:n :}
   false SECTION-ACTIVE !
   false FAIL-NEXT-MARK-ALLOC !
   false FAIL-NEXT-OLD-ALLOC !
   DIFF:RESET
   rc RELEASE-BUFFERS ;

: SECTION-HEAD? ( ptr u8 n -- bool )
   s" diff --git " LINT-PREFIX? ;

: PROCESS-PRIOR-SECTION ( ptr u8 n -- ) {: a:ptr end:n :}
   SECTION-START @ -1 = if
      ARTIFACT-LINE-START @ 0<> if E-DIFF-SYNTAX throw then
      ARTIFACT-LINE-START @ SECTION-START ! exit
   then
   a SECTION-START @ + end SECTION-START @ - PROCESS-SECTION
   ARTIFACT-LINE-START @ SECTION-START ! ;

: PROCESS-ARTIFACT ( ptr u8 n -- ) {: a:ptr u:n :}
   a u DIFF:SOURCE-VALIDATE
   -1 SECTION-START !
   0 ARTIFACT-LINE-START !
   0 ARTIFACT-I !
   begin ARTIFACT-I @ u < while
      a ARTIFACT-I @ + c@ LF-C = if
         a ARTIFACT-LINE-START @ +
         ARTIFACT-I @ ARTIFACT-LINE-START @ - SECTION-HEAD? if
            a ARTIFACT-LINE-START @ PROCESS-PRIOR-SECTION
         then
         ARTIFACT-I @ 1+ ARTIFACT-LINE-START !
      then
      ARTIFACT-I @ 1+ ARTIFACT-I !
   repeat
   SECTION-START @ -1 = if E-DIFF-SYNTAX throw then
   a SECTION-START @ + u SECTION-START @ - PROCESS-SECTION ;

: SOURCE-WORK ( -- )
   INPUT$ PROCESS-ARTIFACT ;

public

: ROOT! ( ptr u8 n -- )
   ROOT-BUF ROOT-U COPY! ;

: RESET ( -- )
   0 RELEASE-BUFFERS
   DIFF:RESET
   s" ." ROOT!
   0 BAD !
   0 MAPPING-PEAK !
   false FAIL-NEXT-MARK-ALLOC !
   false FAIL-NEXT-OLD-ALLOC !
   0 FILE-U !
   false FILE-USED !
   false SECTION-ACTIVE ! ;

: SOURCE ( ptr u8 n -- )
   INPUT-U ! INPUT-PTR !
   [: SOURCE-WORK ;] catch {: rc:n :}
   rc 0<> if rc ABORT-PARSE then ;

: FILE ( ptr u8 n -- )
   FILE-USED @ if 2drop E-DIFF-SYNTAX throw then
   true FILE-USED !
   LINT-SOURCE:LOAD
   LINT-SOURCE:TEXT SOURCE ;

\ Embedders use FINDINGS to distinguish policy findings from parser failures
\ while routing diagnostics through their own LINT-OUT sink.
: FINDINGS ( -- n )
   BAD @ ;

: FINISH ( -- )
   BAD @ 0 > if 1 throw then ;

;package
