\ enum-census-test.f - hostile fixtures for the plain-ENUM census scanner.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/memory.f
\ lib/vector.f lib/fs.f lib/fs-mutate.f tools/lint/text.f tools/lint/token.f
\ tools/lint/lib.f tools/lint/source-lex.f tools/enum-census-core.f
\ tools/enum-census-test.f
\
\ What has to be true for the census to mean anything: it must see every real
\ declaration and no imitation of one. A count alone would pass on a scanner
\ that matched the four letters anywhere, so each fixture below writes a file
\ that LOOKS like it holds declarations and pins how many the scanner actually
\ finds, which ones, and in what order.
\
\ The scan-only mode is used throughout: re-declaring a fixture would register
\ families in this process, and what is under test here is site DETECTION. The
\ replay half is exercised by the production run over the real tree
\ (tools/enum-census.f verify), which is where a mis-replayed site shows up as a
\ baseline divergence.

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
require tools/enum-census-core.f

package ENUM-CENSUS-TEST
private

$4000 constant SRC-CAP
create SRC-BUF SRC-CAP allot
variable SRC-U
variable PATH-U
create PATH-BUF FS-PATH-CAP allot

TRUSTED: SRC$ ( -- ptr u8 n ) SRC-BUF SRC-U @ ;
TRUSTED: SRC-C ( n -- ) {: c:n :}
   SRC-U @ SRC-CAP >= IF s" enum-census-test: fixture buffer overflow" 1 die THEN
   c SRC-BUF SRC-U @ + c!  SRC-U @ 1 + SRC-U ! ;
TRUSTED: PATH$ ( -- ptr u8 n ) PATH-BUF PATH-U @ ;
TRUSTED: PATH! ( ptr u8 n -- ) {: a:ptr u:n :}
   a PATH-BUF u BYTE-COPY  u PATH-U ! ;

variable TI
: PUT ( ptr u8 n -- ) {: a:ptr u:n :}
   0 TI !
   BEGIN TI @ u < WHILE  a TI @ + c@ SRC-C  TI @ 1 + TI !  REPEAT ;
: LINE ( ptr u8 n -- ) PUT 10 SRC-C ;
: DQ ( -- ) 34 SRC-C ;
: RESET-SRC ( -- ) 0 SRC-U ! ;

\ The fixture is written to a real file because the census reads files; the
\ scanner is the subject, so the path is a scratch one and the label it reports
\ is that same path.
: WRITE-FIXTURE ( -- )
   PATH$ SRC$ WRITE-ALL ;

: SCAN-FIXTURE ( -- )
   ENUM-CENSUS:RESET
   ENUM-CENSUS:SCAN-ONLY!
   WRITE-FIXTURE
   PATH$ ENUM-CENSUS:FILE ;

: REPORT-HAS? ( ptr u8 n -- bool )
   ENUM-CENSUS:REPORT$ 2swap CONTAINS? ;

\ ---------------------------------------------------------------------------
\ 1. Imitations. Comments, a string, direct definition names, and four escaped
\    references all spell ENUM; none is a declaration. One real declaration
\    sits at the end, so a scanner that saw nothing at all would fail too.
\ ---------------------------------------------------------------------------
: FIXTURE-IMITATIONS ( -- )
   RESET-SRC
   s" \ ENUM commented red green ;ENUM" LINE
   s" ( ENUM parenthesised red green ;ENUM )" LINE
   s" : QUOTED ( -- ptr u8 n ) s" PUT DQ s"  ENUM instring red ;ENUM" PUT DQ s"  ;" LINE
   s" : ENUM ( -- ) ;" LINE
   s" 3 constant ENUM" LINE
   s" : REFS ( -- ) ' ENUM drop ['] ENUM drop ;" LINE
   s" : MORE-REFS ( -- ) postpone ENUM ; immediate" LINE
   s" : CHARS ( -- ) char ENUM drop [char] ENUM drop ;" LINE
   s" ENUM real-one alpha beta ;ENUM" LINE ;

: TEST-IMITATIONS ( -- )
   FIXTURE-IMITATIONS
   SCAN-FIXTURE
   s" imitations: exactly one site" T-LABEL
   ENUM-CENSUS:SITES 1 T=
   s" imitations: no findings" T-LABEL
   ENUM-CENSUS:FINDINGS 0 T=
   s" imitations: the site is the real declaration" T-LABEL
   s" name=real-one" REPORT-HAS? TTRUE
   s" imitations: the commented family is not a site" T-LABEL
   s" name=commented" REPORT-HAS? TFALSE
   s" imitations: the parenthesised family is not a site" T-LABEL
   s" name=parenthesised" REPORT-HAS? TFALSE
   s" imitations: the in-string family is not a site" T-LABEL
   s" name=instring" REPORT-HAS? TFALSE ;

\ ---------------------------------------------------------------------------
\ 2. Order and duplicate tails. Two declarations in one file may share a tail
\    when they sit in different packages; the locator is the site's ordinal in
\    the file, so the two records are distinguishable and stay in source order.
\ ---------------------------------------------------------------------------
: FIXTURE-DUPLICATES ( -- )
   RESET-SRC
   s" package alpha-pkg" LINE
   s" public" LINE
   s" ENUM colour red green ;ENUM" LINE
   s" ;package" LINE
   s" package beta-pkg" LINE
   s" private" LINE
   s" ENUM colour blue ;ENUM" LINE
   s" ;package" LINE ;

: TEST-DUPLICATES ( -- )
   FIXTURE-DUPLICATES
   SCAN-FIXTURE
   s" duplicates: two sites" T-LABEL
   ENUM-CENSUS:SITES 2 T=
   s" duplicates: first is ordinal 0" T-LABEL
   s" #0 name=colour site=public" REPORT-HAS? TTRUE
   s" duplicates: second is ordinal 1 and private" T-LABEL
   s" #1 name=colour site=private" REPORT-HAS? TTRUE ;

\ ---------------------------------------------------------------------------
\ 3. Visibility follows the site. A declaration before any `public` in a package
\    is private; one after it is public; one outside any package is public
\    (top level). Getting this wrong would silently change whether constructors
\    are generated for the replayed site.
\ ---------------------------------------------------------------------------
: FIXTURE-VISIBILITY ( -- )
   RESET-SRC
   s" ENUM at-top red ;ENUM" LINE
   s" package vis-pkg" LINE
   s" ENUM before-public red ;ENUM" LINE
   s" public" LINE
   s" ENUM after-public red ;ENUM" LINE
   s" private" LINE
   s" ENUM after-private red ;ENUM" LINE
   s" ;package" LINE
   s" ENUM after-package red ;ENUM" LINE ;

: TEST-VISIBILITY ( -- )
   FIXTURE-VISIBILITY
   SCAN-FIXTURE
   s" visibility: five sites" T-LABEL
   ENUM-CENSUS:SITES 5 T=
   s" visibility: top level is public" T-LABEL
   s" name=at-top site=public" REPORT-HAS? TTRUE
   s" visibility: package default is private" T-LABEL
   s" name=before-public site=private" REPORT-HAS? TTRUE
   s" visibility: after public" T-LABEL
   s" name=after-public site=public" REPORT-HAS? TTRUE
   s" visibility: after private again" T-LABEL
   s" name=after-private site=private" REPORT-HAS? TTRUE
   s" visibility: after ;package is top level again" T-LABEL
   s" name=after-package site=public" REPORT-HAS? TTRUE ;

\ ---------------------------------------------------------------------------
\ 4. Shapes that are not a plain declaration. The full named-variant form is
\    counted apart and never replayed (its payload types belong to its own file);
\    an unterminated declaration and one whose body carries a comment token are
\    reported as malformed rather than silently half-read.
\ ---------------------------------------------------------------------------
: FIXTURE-SHAPES ( -- )
   RESET-SRC
   s" ENUM full-form 0 VARIANT one ;VARIANT ;ENUM" LINE
   s" ENUM plain-one red ;ENUM" LINE
   s" ENUM body-comment red ( stray ) green ;ENUM" LINE
   s" ENUM unterminated red green" LINE ;

: TEST-SHAPES ( -- )
   FIXTURE-SHAPES
   SCAN-FIXTURE
   s" shapes: one full-form site counted apart" T-LABEL
   ENUM-CENSUS:FULL-SITES 1 T=
   s" shapes: the full form is not in the plain report" T-LABEL
   s" name=full-form" REPORT-HAS? TFALSE
   s" shapes: the plain one is" T-LABEL
   s" name=plain-one" REPORT-HAS? TTRUE
   s" shapes: a comment inside the body is malformed" T-LABEL
   s" MALFORMED-BODY" REPORT-HAS? TTRUE
   s" shapes: an unterminated declaration is malformed" T-LABEL
   s" MALFORMED-UNTERMINATED" REPORT-HAS? TTRUE
   s" shapes: both malformed shapes are findings" T-LABEL
   ENUM-CENSUS:FINDINGS 2 T= ;

\ ---------------------------------------------------------------------------
\ 5. The second-parser tripwire. Re-defining one of the retired legacy words, or
\    re-adding an ENUM primitive-axiom row, is reported; naming them in a comment
\    or a string is not, and an unrelated definition or axiom row is not.
\ ---------------------------------------------------------------------------
: FIXTURE-TRIPWIRE ( -- )
   RESET-SRC
   s" \ CHECKER-DEFENUM in a comment is prose, not a definition" LINE
   s" : MENTIONS ( -- ptr u8 n ) s" PUT DQ s" ENUM-COLLECT" PUT DQ s"  ;" LINE
   s" : UNRELATED ( -- ) ;" LINE
   s" PRIM: CHECKER-DEFSUM PE-PTR-U8 PE-IN PRIM;" LINE
   s" : CHECKER-DEFENUM ( -- ) ;" LINE
   s" PRIM: ENUM PRIM;" LINE ;

: TEST-TRIPWIRE ( -- )
   FIXTURE-TRIPWIRE
   SCAN-FIXTURE
   s" tripwire: the definition and the axiom row are both reported" T-LABEL
   ENUM-CENSUS:FINDINGS 2 T=
   s" tripwire: the retired definition is named" T-LABEL
   s" SECOND-PARSER definition CHECKER-DEFENUM" REPORT-HAS? TTRUE
   s" tripwire: the retired axiom row is named" T-LABEL
   s" SECOND-PARSER axiom-row" REPORT-HAS? TTRUE
   s" tripwire: no site was invented" T-LABEL
   ENUM-CENSUS:SITES 0 T= ;

create ROOT-BUF FS-PATH-CAP allot
variable ROOT-U
TRUSTED: ROOT$ ( -- ptr u8 n ) ROOT-BUF ROOT-U @ ;
TRUSTED: ROOT! ( ptr u8 n -- ) {: a:ptr u:n :}
   a ROOT-BUF u BYTE-COPY  u ROOT-U ! ;

: PREPARE ( -- )
   s" habu-enum-census" TMPDIR-MKDIR ROOT!
   ROOT$ s" fixture.f" PATH-BUF JOIN-PATH PATH-U ! ;

public

: RUN ( -- )
   T-RESET
   PREPARE
   TEST-IMITATIONS
   TEST-DUPLICATES
   TEST-VISIBILITY
   TEST-SHAPES
   TEST-TRIPWIRE
   PATH$ REMOVE-FILE
   ROOT$ REMOVE-DIR
   T-REPORT
   s" enum-census-test: ok" type cr ;

;package

ENUM-CENSUS-TEST:RUN
