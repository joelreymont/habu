\ package-diff-lint-core.f - enforce package ownership on changed Forth definitions.
\
\ The unified-diff grammar belongs to tools/lint/diff.f.  Each canonical section
\ is parsed once to verify the new side and reconstruct the complete old source,
\ then replayed to align genuine old-side package transitions with new lines.
\ Both complete sources are lexed: hunk context and isolated deleted lines cannot
\ prove package scope across multiline comments, strings, or definitions.
\
\ The scan consumes the lint lexer's events rather than re-reading source text.
\ Three kinds arrive.  A WORD drives the ownership rules.  A `( ... )` comment is
\ inert.  A `PRIM:`/`PPRIM:` primitive-axiom row arrives whole, as one REGISTRY
\ token: it declares an engine primitive's stack effect, publishes no dictionary
\ word, opens no package, and its fields never appear as separate tokens, so the
\ scan steps over the entire span.  Any other kind, and any lexer diagnostic,
\ stops the scan with a named source-defect code.  There is deliberately no
\ second row grammar here: a private re-parse of row text is exactly how a body
\ carrying `package`, `:`, or `create` bytes could forge a scope the engine never
\ enters and hide the global definition this lint exists to report.
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
require tools/lint/def.f
require tools/lint/diff.f

package PACKAGE-DIFF
private

32 constant NUM-CAP
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

\ ---- source-defect codes -----------------------------------------------------
\ A lexer diagnostic describes the FILE being linted, not the diff artifact, so
\ it must not be rethrown as tools/lint/diff-error.f's E-DIFF-SYNTAX: that code
\ means "this patch is not a unified diff" and sends the reader to the artifact
\ instead of to the broken source line.  The lexer names more than one defect and
\ each one stops its scan, so each defect gets its own name here, the same way
\ tools/lint/shadow-lint.f splits E-SHADOW-UNTERM from E-SHADOW-REGISTRY.
\ Negative, so tools/error-code-lint.f keeps them globally unique.  -4803..-4805
\ continue the unclaimed lint-tool gap that already holds E-SHADOW-UNTERM
\ (-4800), E-CLOBBER-WRAP-UNRESOLVED (-4801), and E-SHADOW-REGISTRY (-4802); the
\ gap ends before lib/errors.f's reserved E-REPORT block at -4900.
-4803 constant E-PKGDIFF-ROW     \ a `PRIM:`/`PPRIM:` axiom row lacked a header or its closer
-4804 constant E-PKGDIFF-QUOTE   \ a string literal ran past end of input
\ The residual arm: a diagnostic kind added to LINT-LEX after this consumer was
\ written.  It must reach a named refusal rather than be reported as one of the
\ two defects this lint does understand, and it must never pass silently.
-4805 constant E-PKGDIFF-LEX     \ a lexer diagnostic or token kind this lint was never taught
\ The grammar-fixture row table is built once at load time from fixed rows, so
\ overflowing it means a row was added without raising its capacity.  That must
\ stop the load rather than silently drop the row and quietly stop admitting the
\ suite it belongs to.  The engine-trunk and Gforth-mirror tables further down
\ are read by index too, and an index past the end of either one would answer
\ with some other row's path, so they raise this same code.
-4806 constant E-PKGDIFF-ROWTAB  \ a fixture row, or a row index, fell outside the table
\ A definer whose name never arrives - the definer token stands at the end of
\ the scan, or the next token is not a WORD (a comment, say) - is a defect of
\ the FILE being linted, not of the diff artifact, so it must not be reported
\ as E-DIFF-SYNTAX either.
-4807 constant E-PKGDIFF-NONAME  \ a definition-opening token has no name word after it
\ The old-side name table (see "the old file's global definition names" below)
\ is sized from the reconstructed pre-image by a bound that holds for every
\ well-formed source, so it should never fill.  If it ever does, or if a record
\ turns up without its terminator, the table no longer describes the old file,
\ and a table that quietly stopped recording would start answering "this name is
\ new" for words the file already had -- which is the one answer that admits a
\ new global.  So it stops instead.
\ -4808..-4810 went to tools/error-code-lint-core.f while this block was being
\ written, so this code continues after them rather than beside its neighbours.
-4811 constant E-PKGDIFF-NAMETAB \ the old-side name table overflowed or lost a record

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
variable NAME-A
variable NAME-U
variable NAME-CAP                  \ the mapped span, which only the release uses
variable NAME-LIMIT                \ the proven bound the recorder may fill
variable MARK-A
variable MARK-CAP
variable MARK-U
variable NEW-SLOTS
variable OLD-SLOTS
variable MAPPING-PEAK
variable FAIL-NEXT-MARK-ALLOC
variable FAIL-NEXT-OLD-ALLOC
variable FAIL-NEXT-NAME-ALLOC
variable FORCE-NAME-LIMIT          \ lowers the next name-table bound, for the overflow fixture
variable BAD
variable SECTION-ACTIVE
variable SECTION-SEEN
variable WHOLE-CHANGED
variable TRUNK-HIT                 \ engine-trunk path match, accumulated over the row table
variable MIRROR-HIT                \ gforth-mirror path match, accumulated the same way
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

: NAME-PTR ( -- ptr ptr u8 )
   NAME-A PTR-SLOT ;

: INPUT-PTR ( -- ptr ptr u8 )
   INPUT-A PTR-SLOT ;

: SOURCE-A@ ( -- ptr u8 )
   SOURCE-PTR @ ;

: MARK-A@ ( -- ptr u8 )
   MARK-PTR @ ;

: OLD-A@ ( -- ptr u8 )
   OLD-PTR @ ;

: NAME-A@ ( -- ptr u8 )
   NAME-PTR @ ;

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
   OLD-CAP @ 0<> if 1+ then
   NAME-CAP @ 0<> if 1+ then ;

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

: HABU? ( -- bool )
   FILE$ s" .f" LINT-ENDS-WITH? if true exit then
   FILE$ s" .fs" LINT-ENDS-WITH? ;

\ Exact files whose global (unpackaged) definitions implement the core
\ language/prelude, so a changed global there is not an ownership fault.
\ src/core/util.f is the strongest case in the list rather than an exception to
\ it: it is the FIRST core prefix source, it contains no `package` at all, it
\ loads before the check hook exists, and the checker registers the words it
\ defines (CORE-STR=, PATHZ, PATH0) as global primitive axioms that every later
\ prefix file and the makers resolve bare -- giving them a package owner would
\ break those axiom rows and every core caller.  Every entry here is entirely
\ global; src/core/type-family.f was the one that was not, and it left this list
\ when the TFAM sealing work (dot habu-tfam-2b-sealed-1b77662c) put its 566
\ remaining definitions inside `package TFAM`.  The six names that could not
\ follow are admitted by TFAM-BRIDGE? below, by exact path, name and definer.
\ src/core/checker.f is the first interim entry, on the
\ same terms: the checker is a global pre-hook language surface by current
\ construction -- its PRIM:/PPRIM: primitive-axiom machinery and its RBF
\ rollback-frame surface are global by design and load before any package
\ exists -- so it is admitted the way sumtype.f, roles.f, structures.f and
\ enums.f are, and it must be removed once the checker sealing work (dot
\ habu-seal-the-checker-5314c0ab) gives those seams real package owners.
\ src/core/render.f is the third interim entry, admitted on exactly the same
\ terms and for the same subsystem: by its own header it is "the render half of
\ the native sigparse/checker", split out only to keep one concern per file. It
\ contains no `package` at all, it loads as part of the compiler prefix before
\ any package exists, and checker.f resolves the words it defines (EMIT1, RSTR,
\ DIAG-BUFFER!, FAM-QNAME-REND) bare. Measured: with no entry here, changing one
\ line of an existing global body in render.f reds the gate, so the gate
\ rejected every possible change to the diagnostic renderer. Packaging one new
\ word while its ~100 neighbours stay global would split the surface rather than
\ seal it, so this entry is interim and is retired by the render sealing dot
\ alongside the checker sealing work.
\ src/arch/arm64/icode.f and src/arch/arm64/mnem.f are the fourth entry, on the
\ same terms and for one subsystem: the ARM64 encoder prefix.  Neither contains a
\ `package` at all.  They are the fourth and fifth files tools/srclist.f puts in
\ the engine source prefix, ahead of the compiler that defines packages in the
\ first place, and every later prefix file resolves their names bare --
\ src/habu/jit.f, src/habu/habu1.f and src/habu/habu2.f call `LDR,` and `CBZ,`
\ with no qualifier, as do tools/asm-src-test.f and test/compiler/insn-schema.f.
\ Their own headers say why: they are written in the STANDALONE's Forth so the
\ Gforth recovery compiler can read them before the native checker exists.
\ Measured on 2026-07-30: with no entry here, adding the operand bounds to the
\ encoders reported ten ownership faults for words they already published, so the
\ gate rejected every possible change to the ARM64 assembler.  Interim, exactly
\ like the checker and render entries, and retired by dot
\ habu-pkg-the-arm64-ffabc063, which gives the two remaining files real package
\ owners and migrates their callers.
\ src/arch/arm64/asm.f is NO LONGER here: dot habu-pkg-the-arm64-ce972795 made it
\ package A64ASM, so the encoders own their own names and every caller reaches
\ them through `using A64ASM` or a qualifier.  The Gforth recovery host compiles
\ a package declaration in a prefix file (bootstrap/cg/forth.fs implements
\ package/public/private/;package and `using`), which is what the entry's removal
\ had to establish first.  A global added to asm.f now reports, and its fixtures
\ below pin that.
\ src/os/image-bytes.f, src/os/linux/elf.f and src/os/macos/macho.f are the fifth
\ entry, on the icode.f/mnem.f terms and for one subsystem: the executable image
\ writer.  None of the three contains a `package` at all, and every later prefix
\ source resolves their names bare -- src/habu/driver-io.f calls BUILD-IMAGE,
\ MLEN@ and TEXTSZ, src/habu/snap-lib.f calls CODE-OFF and MBUF, and
\ src/habu/habu2.f, tools/object-image.f and tools/image-bytes-test.f do the
\ same.  Measured 2026-08-14: on master, changing the single line
\ `$220000 constant MSIZE` in src/os/image-bytes.f to another number -- defining
\ no new word, moving no boundary -- reported
\ `E-PACKAGE-OWNERSHIP src/os/image-bytes.f:12:18`, so as configured the gate
\ rejected every possible change to the image buffer's size, including the code
\ window lift that found it (dot habu-lift-the-image-42b2b9f4, whose whole
\ subject is those numbers).  Interim, exactly like the checker, render and ARM64
\ entries.  Its retirement dot is NOT minted yet: the three files need real
\ package owners and their bare callers migrated, which is a caller cascade this
\ lane did not take, and the leaf for it is owed.
\ src/core/layout-buffer.f is the fourth file of the sumtype.f class: a pre-hook
\ core surface whose reason for being global is that it OWNS a global declaring
\ word -- LAYOUT-BUFFER, the way sumtype.f owns NEWTYPE/SUMTYPE/ENUM/PRODUCT,
\ roles.f owns its role declarers and structures.f owns the structure grammar.
\ Its absence here was an asymmetry rather than a decision: the identical edit --
\ retargeting a duplicate-definition guard from the retired CHECKER-DEFINED? to
\ CHECKER-DEFINED-HERE? (dot habu-checker-defined-answers-1504bbde) -- passed
\ silently in sumtype.f and was refused in layout-buffer.f, for two files with
\ the same standing.  Giving it a package is a real change and a large one (71
\ files reference LAYOUT-BUFFER), which is why it is not the price of a one-word
\ edit; when someone does it, this row goes with it, exactly as asm.f's did.
\
\ src/core/internal-mark.f is the LAST prefix source and the seal-time
\ internal-word marking pass, and it is global for a reason its own header
\ writes down rather than by drift: the pass reads raw dictionary-record cells,
\ and every word in it must stay checker-unknown so the pass marks THEM
\ DNAME-INT as it finishes -- self-sealing, nothing in the file remains callable
\ from user source once IMK-PASS has run.  Measured 2026-08-20 on pristine
\ master: adding a single trailing comment to the body of IMK-WALK -- defining
\ no new word and changing no behaviour -- reported
\ `E-PACKAGE-OWNERSHIP src/core/internal-mark.f:98:3`, so as configured the gate
\ rejected every possible change to the marking pass, including the repair of the
\ live SIGABRT that found this (dot habu-pkg-publics-escape-41532ee7: a qualified
\ `PRIM-LINK:COUNT` read six cells below the interpret base and aborted rc 134).
\ It is a full entry rather than a habu2.f-style body-only one because the pass
\ grows by adding words: closing the qualified-name escape needed six.
\
\ ITS RETIREMENT CONDITION IS NAMED AND IS NOT "SOMEONE PACKAGES IT".  The end
\ state is `package INTERNAL-MARK` with every word private and BOTH wordlists
\ sealed by `get-current prot-wid-add`, the shape src/habu/xref.f already uses for
\ PKG-AUTH, so C-PACKAGE-SEAL-GUARD refuses to reopen it and the privates are
\ unreachable by every route.  That is strictly stronger than DNAME-INT and this
\ entry goes with it.  It cannot land FIRST, and the ordering is measured, not
\ cautious: a package private is reachable today by reopening its package --
\ `package PRIM-LINK / private / KEY-SYM / ;package` aborts rc 134 on master --
\ so packaging the pass before that hole closes would make IMK-CLASSIFY callable
\ with an arbitrary record index, where the marked global is callable from
\ nowhere at all.  Sealing removes that objection; until the reopen defect is
\ closed, this file is more sealed as an allowlisted global than as a package.
\
\ lib/test/runner.f and lib/test/runner-test.f are the one entry here that does
\ NOT fit the list's contract, and saying so is the point of the row rather than
\ an apology for it.  Every other member is a core language or prelude surface:
\ it loads before packages exist, or it owns a global declaring word.  The test
\ runner is neither.  It loads long after the check hook, it declares nothing,
\ and its globals are raw GT-/GTT- stems -- ordinary packaging debt, the shape
\ this lint's own history packages rather than admits (test/internal-word-gate.f
\ left the sibling category and became `package INTERNAL-WORD-GATE` with its 85
\ IWG- helpers renamed; lib/process-pty-handle.f is named in that category's
\ header as a file that must NOT be added).  This row is a bridge across a
\ measured campaign, not a claim that the file belongs.
\
\ What it cost to avoid, measured 2026-08-21 rather than estimated.  The freeze
\ is total and it is not about new words: on pristine master, editing one body
\ line of GT-RC-NONZERO -- a word the change never touches -- reported
\ `E-PACKAGE-OWNERSHIP lib/test/runner.f:312:3`, so no change to the runner
\ could land at all, including the repair of the blind failure that found this
\ (dot habu-gt-rc-throws-395eb72a: GT-RC= reduced the observed exit code to a
\ bool, and two agents diagnosed spawn failures with the rc never printed).
\ Packaging was then BUILT, not guessed: `package GT` with all 84 definitions
\ stripped to bare names, `package GTT` beside it, 24 consumers rewritten to
\ `GT:RUN` form, 558 token rewrites, loading clean and green.  It reported 64
\ NEW ownership findings in seven more unpackaged files -- gate-common-lib.f,
\ gate-pool.f, gate-engine-lib.f, gate-debug-lib.f, gate-build-common.f,
\ gate-stdlib-inline-lib.f, gate-build-hbb.f -- because a qualifier edit lands
\ inside their definitions and touching a definition in an unpackaged file IS
\ the finding.  A static ring-3 census (scope-aware, comments and strings
\ excluded) proved the cluster closes there: every global caller of those seven
\ is another of the seven.  Bounded, then -- but two facts the census could not
\ see decide it.  A transform over those 782 names is only safe driven by
\ tools/lint/def.f's 57 defining forms, not a hand list: `defer RUN` at
\ gate-engine-lib.f:899 and the three `defer` hooks in gate-pool.f are global
\ definitions a six-definer scan never sees.  And nineteen of the names live
\ inside string literals in three load-bearing classes, each a separate
\ judgement: `GE-FILES:` is itself one of def.f's 57 forms, so the rename edits
\ the classifier this lint consumes and its fixtures; `GSI-INCLUDE`,
\ `GSI-FORK-INCLUDE` and `GSI-REQUIRE` are matched AS TEXT by
\ tools/lint/schedule-lint.f:583-585, the lint that proves every test file is
\ registered in a suite, so a rename that misses those strings stops it seeing
\ registrations and still exits 0; and gate-engine-lib.f:779-791 builds child
\ program source calling GE-HB$, GE-RUN-STDIN-FILE and GE-RUN-ENV by name while
\ GE-SHAPE-FIND-AFTER searches source TEXT for the literal `GE-RUN-STDIN`.  A
\ 782-name sweep whose failure mode is silent green is not a lane's tail work.
\
\ ITS RETIREMENT IS THE CAMPAIGN, dot habu-pkg-lib-test-06230d06, which owns the
\ gate-cluster packaging with that first leg, the census and its static method,
\ the def.f-driven requirement, the three string classes and a schedule-lint
\ re-verification as a closing gate.  When it lands, this row's two positives
\ below become negatives in the fixture, exactly as asm.f's did.
\
\ Package-boundary changes are still reported for every file here
\ (FINISH-DEFINITION checks SCOPE-DELTA before this allowlist).  Files with only
\ one global declarer are handled by GLOBAL-SURFACE? below, so an unrelated
\ global added beside DEFTYPE or STRUCTURE is still rejected.
: GLOBAL-IMPLEMENTATION? ( -- bool )
   FILE$ s" lib/prelude.f" LINT-STR= if true exit then
   FILE$ s" src/core/util.f" LINT-STR= if true exit then          \ first prefix source; see header
   FILE$ s" src/core/sha256.f" LINT-STR= if true exit then       \ standalone stage-0 prefix; tools/bootstrap.sh:81
   FILE$ s" src/core/sumtype.f" LINT-STR= if true exit then
   FILE$ s" src/core/layout-buffer.f" LINT-STR= if true exit then
   FILE$ s" src/core/roles.f" LINT-STR= if true exit then
   FILE$ s" src/core/structures.f" LINT-STR= if true exit then
   FILE$ s" src/core/checker.f" LINT-STR= if true exit then      \ core surface, interim; see header
   FILE$ s" src/core/render.f" LINT-STR= if true exit then       \ checker's render half, interim; see header
   \ Retires with `package INTERNAL-MARK` + prot-wid-add sealing, AFTER the
   \ package-reopen defect closes; see header.
   FILE$ s" src/core/internal-mark.f" LINT-STR= if true exit then \ self-sealing marking pass, interim; see header
   FILE$ s" src/arch/arm64/icode.f" LINT-STR= if true exit then  \ ARM64 encoder prefix, interim; see header
   FILE$ s" src/arch/arm64/mnem.f" LINT-STR= if true exit then   \ ARM64 encoder prefix, interim; see header
   FILE$ s" src/os/image-bytes.f" LINT-STR= if true exit then    \ image writer, interim; see header
   FILE$ s" src/os/linux/elf.f" LINT-STR= if true exit then      \ image writer, interim; see header
   FILE$ s" src/os/macos/macho.f" LINT-STR= if true exit then    \ image writer, interim; see header
   \ Retires with the gate-cluster packaging campaign, habu-pkg-lib-test-06230d06;
   \ does not fit this list's contract and says so in the header.
   FILE$ s" lib/test/runner.f" LINT-STR= if true exit then       \ test runner, campaign bridge; see header
   FILE$ s" lib/test/runner-test.f" LINT-STR= if true exit then  \ test runner, campaign bridge; see header
   FILE$ s" src/core/enums.f" LINT-STR= ;

\ Declaration-grammar fixture suites.  The second principled category, built on
\ the same terms as GLOBAL-IMPLEMENTATION? above: a named, reasoned, exact-path
\ list pinned by this lint's own unit test, not an ad hoc waiver.
\
\ Why it is a category and not an exception.  These suites test the global
\ declaration grammar itself, so a global declaration IS the thing under test.
\ test/type-decl-suite.f says so in its own header: everything it declares is
\ "USER source arriving after the engine sealed the TFAM/TYPE/MATCH system
\ packages", and each accepting declaration "is also the post-seal proof: user
\ declarations register families through the baked grammar words WITHOUT opening
\ any reserved package".  Wrapping those fixtures in a package would not satisfy
\ the ownership rule, it would delete the proof - the suite would stop testing
\ the position a real user declares from.  The same obligation covers the other
\ entries, each of which must declare at genuine top level.
\
\ The category is deliberately narrow in three structural ways.  Entries are
\ exact paths, so a global added to any OTHER test file still reports.  The path
\ must live under test/, so the list can never admit a library or engine source:
\ lib/process-pty-handle.f is NOT here and must not be added - its six role
\ families were real debt and were packaged (dot habu-pkg-process-pty-ad38b5da).
\ And admission is by declaration SHAPE, per path: each row below pairs an exact
\ path with the exact set of declaration openers that path's own fixtures use,
\ so a suite whose fixtures only ever declare with NEWTYPE does not admit
\ SUMTYPE, ENUM or anything else, and an ordinary colon word, variable, create,
\ constant or TRUSTED: wrapper is reported in every row like any other
\ unpackaged global.  The opener token comes from the index the scan already
\ recorded, so the decision needs no second parser and never looks at the
\ declared family name.  The token-to-bit half sits beside GLOBAL-SURFACE?
\ below rather than here, because it needs the token comparator, exactly as
\ ERR-VOCAB? does.  FINISH-DEFINITION still checks SCOPE-DELTA first, so adding
\ or deleting a package boundary around a fixture is reported even here.
\
\ test/internal-word-gate.f was listed here and was removed once the shape rule
\ was measured: it declares nothing through the grammar - its fixtures are
\ declaration SOURCE built as strings and handed to child processes - so its
\ entry admitted only its 85 raw-stem IWG- test helpers, which are ordinary
\ packaging debt tracked by dot habu-pkg-internal-word-da4149d9.
\ One bit per declaration opener any listed suite declares with.  These are the
\ only openers the category knows; a definer outside them is an ordinary global
\ everywhere, in every row.  STRUCTURE has no bit because no listed suite
\ declares with it; the leaf that lands the first suite which does adds the bit
\ here and the bit to that suite's row, and to no other row.
$0001 constant O-SUMTYPE
$0002 constant O-PRODUCT
$0004 constant O-ENUM
$0008 constant O-VALUE-RECORD
$0010 constant O-NEWTYPE
$0020 constant O-DEFTYPE
$0040 constant O-DEFLINEAR
$0080 constant O-CAST
$0100 constant O-LAYOUT-BUFFER
$0200 constant O-DEFER-LAYOUT
$0400 constant O-TYPED-VARIABLE
$0800 constant O-TYPED-BUFFER
$1000 constant O-PTR-VARIABLE
\ TRUSTED: is the one opener here that declares an EFFECT rather than a type.
\ It earns a bit for the same reason the others have one: a listed suite has to
\ declare with it at genuine top level, and no suite that does not gets the bit
\ in its row.
$2000 constant O-TRUSTED

\ Each set is the EXACT set of openers that path's own fixtures declare with,
\ measured by replaying that file through this lint with the category off and
\ reading every reported definition back at its own line and column.  The counts
\ in each comment are that measurement; they are what a reviewer re-derives to
\ check a row, and they are why no row carries an opener its suite never uses.
\ SUMTYPE 28, PRODUCT 15, ENUM 8, NEWTYPE 5, LAYOUT-BUFFER 4, VALUE-RECORD 1,
\ DEFLINEAR 1 - the declaration grammar's own behaviour suite: positives,
\ negatives, rollback and multi-error, all from real top-level user position.
O-SUMTYPE O-PRODUCT or O-ENUM or O-VALUE-RECORD or O-NEWTYPE or
O-DEFLINEAR or O-LAYOUT-BUFFER or constant TYPE-DECL-SET
\ NEWTYPE 5 - decision-record probe for the extent substrate: the stand-in
\ families must be declared the way the candidate design would declare them.
O-NEWTYPE constant EXTENT-PROBE-SET
\ NEWTYPE 5 - BTC-7 product/factorization regression over extent-role families.
O-NEWTYPE constant EXTENT-PRODUCT-SET
\ TYPED-VARIABLE 3, TYPED-BUFFER 2, SUMTYPE 2, NEWTYPE 2, PTR-VARIABLE 1,
\ LAYOUT-BUFFER 1, DEFLINEAR 1 - the TYPED-VARIABLE / TYPED-BUFFER contract: the
\ nominal scalars must be ordinary user families, since the point is that a raw
\ `variable` cannot mint one.
O-TYPED-VARIABLE O-TYPED-BUFFER or O-SUMTYPE or O-NEWTYPE or
O-PTR-VARIABLE or O-LAYOUT-BUFFER or O-DEFLINEAR or constant TYPED-STORAGE-SET
\ CAST: 6, NEWTYPE 3 - the CAST: accept contract: the retype declarer under test
\ plus the arity-0 and parametric families a real caller would write.
O-NEWTYPE O-CAST or constant CAST-SET
\ NEWTYPE 1 - the CAST: reject contract.  Its illegal casts are handed to the
\ checker as strings rather than written at top level, so the file itself
\ declares only the family they name, and this row admits only that.
O-NEWTYPE constant CAST-NEG-SET
\ SUMTYPE 3, LAYOUT-BUFFER 3, NEWTYPE 2, PTR-VARIABLE 1, PRODUCT 1, ENUM 1,
\ DEFLINEAR 1 - LAYOUT-BUFFER is the introduction form for a top-level nominal
\ scalar, so the family it introduces has to be declared at top level.
O-SUMTYPE O-PRODUCT or O-ENUM or O-NEWTYPE or
O-DEFLINEAR or O-LAYOUT-BUFFER or O-PTR-VARIABLE or constant LAYOUT-BUFFER-SET
\ DEFER-LAYOUT-BUFFER 2, SUMTYPE 1, NEWTYPE 1, DEFLINEAR 1 - the deferred form
\ over a top-level arity-0 nominal scalar, same obligation.
O-DEFER-LAYOUT O-SUMTYPE or O-NEWTYPE or O-DEFLINEAR or constant LAYOUT-DEFER-SET
\ VALUE-RECORD 8, DEFTYPE 3, NEWTYPE 1, LAYOUT-BUFFER 1, DEFLINEAR 1,
\ TRUSTED: 42 - the engine's own behaviour suite: its nominal scalar backs the
\ only checked source of a pointee accessor, declared where the engine sees user
\ source.
\
\ WHY THIS ROW ALONE CARRIES O-TRUSTED.  The suite's subject is what the engine
\ and the checker do at genuine GLOBAL top level: which scope claims a bare tail,
\ what a package private shadows, what survives a checker scope, what a `using`
\ collides with.  Its abstract boundary words used to be bare `trust` rows, which
\ declare an effect without defining anything; since a row must now name a word
\ the engine resolves (src/core/checker.f TRUST-RESOLVES?, dot
\ habu-make-trust-refuse-cc8e19de) each of them is a `TRUSTED:` definition, and
\ 41 of the 42 could sit in the suite's own ENGINE-SUITE package.  T-PRESO cannot:
\ its case asserts that a package private shadows the GLOBAL binding and that the
\ global one comes back after `;package`, and measured, a package-owned outer word
\ does not come back - the candidate answers 1 (uncheckable) where the case
\ requires -1.  Packaging that fixture would delete the property under test, which
\ is the same argument that admits the two stage0 recovery fixtures below, and the
\ global leg is precisely the leg E-USING-SHADOW-GLOBAL exists to guard.
\
\ The row admits the FIXTURE, not the pattern.  It is one path and one opener: a
\ `TRUSTED:` at global scope in any other file is reported, a plain `:` in this
\ file is reported, and a neighbouring path is not implicitly exempt.
\ tools/package-diff-lint-test.f pins all three.
O-VALUE-RECORD O-NEWTYPE or O-DEFTYPE or O-TRUSTED or
O-DEFLINEAR or O-LAYOUT-BUFFER or constant ENGINE-SET

\ ---- the row table -----------------------------------------------------------
\ The rows are DATA, not nine repeated comparisons.  That distinction is the
\ whole design: when each row carried its own `FILE$ s" ..." LINT-STR=` test,
\ each row could be weakened on its own - swap one of them to a suffix or
\ case-insensitive comparison and only that path starts admitting
\ test/lib/<name>, which no hostile written against a different path can see.
\ Here every row is a (path, opener set) pair in one table, reached through one
\ accessor, and compared by the single LINT-STR= site in FIXTURE-ROW-AT.  A
\ weakening now has exactly one place to live and changes every row at once, so
\ the generated hostile battery in the unit test kills it on all nine.
16 constant FIXTURE-ROW-CAP        \ headroom for the cutover's new fixture files
512 constant FIXTURE-TEXT-CAP      \ arena holding the row paths
create FIXTURE-TEXT FIXTURE-TEXT-CAP allot
create FIXTURE-PATH-A FIXTURE-ROW-CAP cells allot
create FIXTURE-PATH-U FIXTURE-ROW-CAP cells allot
create FIXTURE-MASK FIXTURE-ROW-CAP cells allot
variable FIXTURE-ROW#
variable FIXTURE-TEXT-U

: ROW-PATH$ ( n -- ptr u8 n ) {: i:n :}
   i 0 < i FIXTURE-ROW# @ >= or if E-PKGDIFF-ROWTAB throw then
   i cells FIXTURE-PATH-A + @   i cells FIXTURE-PATH-U + @ ;

: ROW-MASK ( n -- n ) {: i:n :}
   i 0 < i FIXTURE-ROW# @ >= or if E-PKGDIFF-ROWTAB throw then
   i cells FIXTURE-MASK + @ ;

\ Rows are appended once at load time.  The path bytes are copied into the arena
\ because a source string literal is transient; storing its address would leave
\ every row pointing at whatever text was parsed last.
: ROW+ ( ptr u8 n n -- ) {: a:ptr u:n mask:n :}
   FIXTURE-ROW# @ FIXTURE-ROW-CAP >= if E-PKGDIFF-ROWTAB throw then
   FIXTURE-TEXT-U @ u + FIXTURE-TEXT-CAP > if E-PKGDIFF-ROWTAB throw then
   FIXTURE-TEXT FIXTURE-TEXT-U @ + {: dst:ptr :}
   a dst u BYTE-COPY
   dst FIXTURE-ROW# @ cells FIXTURE-PATH-A + !
   u FIXTURE-ROW# @ cells FIXTURE-PATH-U + !
   mask FIXTURE-ROW# @ cells FIXTURE-MASK + !
   FIXTURE-TEXT-U @ u + FIXTURE-TEXT-U !
   FIXTURE-ROW# @ 1+ FIXTURE-ROW# ! ;

\ THE path comparison.  Every row is admitted or refused by this one word, so
\ the whole category has a single place where "is this the listed file?" is
\ decided: whole path, exact bytes, no suffix and no case folding.
: ROW-PATH= ( n ptr u8 n -- bool ) {: i:n a:ptr u:n :}
   a u i ROW-PATH$ LINT-STR= ;

: FIXTURE-ROW-AT ( ptr u8 n -- n ) {: a:ptr u:n :}
   0 begin dup FIXTURE-ROW# @ < while
      dup a u ROW-PATH= if exit then
      1+
   repeat drop -1 ;

\ One line per row, carrying the exact path and that path's exact opener set.
s" test/type-decl-suite.f" TYPE-DECL-SET ROW+
s" test/extent-substrate-probe.f" EXTENT-PROBE-SET ROW+
s" test/extent-product-test.f" EXTENT-PRODUCT-SET ROW+
s" test/typed-storage-test.f" TYPED-STORAGE-SET ROW+
s" test/cast-suite.f" CAST-SET ROW+
s" test/cast-negative-suite.f" CAST-NEG-SET ROW+
s" test/layout-buffer.f" LAYOUT-BUFFER-SET ROW+
s" test/layout-defer.f" LAYOUT-DEFER-SET ROW+
s" test/engine-suite.f" ENGINE-SET ROW+

\ No row means the empty opener set, so an unlisted path and a listed path that
\ cannot declare with this opener fail through the same arithmetic rather than
\ through two different rules.
: FIXTURE-OPENER-SET ( -- n )
   FILE$ FIXTURE-ROW-AT dup 0 < if drop 0 exit then
   ROW-MASK ;

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

\ The recorder is bounded by the requested size, not by the span the allocator
\ rounded it up to: the request is the bound the table's correctness argument is
\ written against, and the rounding is an accident of the allocator.  Keeping the
\ two apart also lets the overflow fixture lower the bound without ever putting a
\ write outside the mapping, and leaves the release with the real span.
: NAME-ALLOC ( n -- ) {: need:n :}
   FAIL-NEXT-NAME-ALLOC @ if
      false FAIL-NEXT-NAME-ALLOC !
      E-MEM-SIZE throw
   then
   need SOURCE-ALLOC-NEED {: want:n :}
   want MEM-ALLOC-64K-SPAN NAME-CAP ! NAME-PTR !
   want NAME-LIMIT !
   FORCE-NAME-LIMIT @ {: forced:n :}
   forced 0 > if
      0 FORCE-NAME-LIMIT !
      forced want < if forced NAME-LIMIT ! then
   then
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

: NAME-RELEASE ( -- )
   NAME-CAP @ 0= if exit then
   NAME-A@ {: a:ptr :}
   NAME-CAP @ {: cap:n :}
   0 NAME-CAP !
   0 NAME-LIMIT !
   a cap MEM:BYTES-ALLOC-LEN MEM:RELEASE-BYTES ;

: CLEANUP-COMBINE ( n n -- n )
   over 0 <> if drop exit then nip ;

\ Release every mapping even if one munmap fails; an existing primary error wins.
: RELEASE-BUFFERS ( n -- )
   [: SOURCE-RELEASE ;] catch CLEANUP-COMBINE
   [: MARK-RELEASE ;] catch CLEANUP-COMBINE
   [: OLD-RELEASE ;] catch CLEANUP-COMBINE
   [: NAME-RELEASE ;] catch CLEANUP-COMBINE
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

\ ---- the old file's global definition names ----------------------------------
\ ENGINE-BODY-EDIT? below has to answer "did this file already define this name
\ at global scope?", and the only place that fact exists is the pre-image the
\ lint reconstructs into OLD$.  SCAN-OLD-BOUNDARIES walks that pre-image before
\ SCAN-DEFINITIONS starts and the two scans share one lexer, so by the time a
\ definition is judged the old token table is gone and the old text cannot be
\ re-lexed.  The old-side scan therefore copies each global definition's name
\ into this table as it passes it.
\
\ A record is the name bytes followed by one LF.  A definition name is a WORD
\ token and nothing else (tools/lint/def.f NAME-I admits no other kind), and a
\ WORD token is whitespace-delimited, so an LF can never occur inside a record:
\ the delimiter needs no escape and no length field.
\
\ The bound is proven, not guessed.  In the old text each definition contributes
\ its own name token plus at least the one delimiter byte in front of it; those
\ spans are disjoint across definitions, because two distinct tokens cannot begin
\ at the same offset and a name holds no whitespace.  So the sum of (name + 1)
\ over all definitions is at most the old text's length, which is what the table
\ is sized to.  The capacity test still stands and it throws, because a table
\ that silently stopped recording would report every later definition as new.
\
\ Only definitions at global scope are recorded.  The question is about the
\ file's GLOBAL surface, so a name that used to sit inside a package and now
\ stands at top level is new to that surface and must be reported.
: OLD-NAME+ ( n -- ) {: namei:n :}
   namei LINT-LEX:TOKEN {: a:ptr u:n :}
   NAME-U @ u ADD-SIZE 1 ADD-SIZE NAME-LIMIT @ > if E-PKGDIFF-NAMETAB throw then
   a NAME-A@ NAME-U @ + u BYTE-COPY
   LF-C NAME-A@ NAME-U @ + u + c!
   NAME-U @ u + 1+ NAME-U ! ;

\ Offset of the LF that ends the record starting here.  Every record is written
\ with its terminator, so a record without one means the table is not the thing
\ this scan wrote and no answer read out of it can be trusted.
: NAME-REC-END ( n -- n ) {: start:n :}
   start begin dup NAME-U @ < while
      NAME-A@ over + c@ LF-C = if exit then
      1+
   repeat drop E-PKGDIFF-NAMETAB throw ;

\ Whole name, and case-insensitively, because that is what "the same word" means
\ to the dictionary being described: `bin/hb` resolves `foo` to `FOO`, so
\ respelling an existing global in another case publishes no name the file did
\ not already have.  This is NOT the case fold the row table below warns about --
\ that one would loosen an exact FILE PATH, which is not an identity relation the
\ engine has any opinion about.  A name that differs by anything other than case
\ is a different word and is not found here.
: NAME-REC= ( n n ptr u8 n -- bool ) {: start:n end:n a:ptr u:n :}
   end start - u <> if false exit then
   NAME-A@ start + u a u LINT-STR=CI ;

: OLD-GLOBAL? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   0 begin dup NAME-U @ < while
      dup NAME-REC-END
      2dup a u NAME-REC= if 2drop true exit then
      nip 1+
   repeat drop false ;

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
   0 OLD-U !
   OLD-SLOTS @ 2 - NAME-ALLOC
   0 NAME-U ! ;

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

\ Classify one scanned token into exactly one arm.  A WORD drives the ownership
\ rules below.  A `( ... )` comment is inert, and a complete `PRIM: ... PRIM;` or
\ `PPRIM: pkg ... PPRIM;` primitive-axiom row arrives as a single REGISTRY token
\ spanning the whole row (tools/lint/source-lex.f): a row publishes no dictionary
\ word, opens no package, and its fields are never separate tokens, so the scan
\ steps over the entire span.  That opacity is the rule, not an omission -- a row
\ body carrying the bytes `package`, `:`, or `create` would otherwise forge a
\ scope the engine never enters and hide the very global this lint reports.
\ Any other kind is one this lint was never taught, and skipping it in silence is
\ how a scanner goes blind: the token spans source the analysis never sees while
\ the diff still reports zero findings.
: OPAQUE? ( n -- bool ) {: k:n :}
   k LINT-LEX:KIND@ {: kind:n :}
   kind LINT-LEX:REGISTRY = if true exit then
   kind LINT-LEX:COMMENT = if true exit then
   kind LINT-LEX:WORD <> if E-PKGDIFF-LEX throw then
   false ;

\ The lexer stopped on a defect in the linted source.  Name the one it hit: a
\ malformed axiom row sends the reader to the row opener, an open string sends
\ them to a missing quote, and an unknown kind fails closed instead of borrowing
\ either label.
: LEX-DEFECT ( -- )
   LINT-LEX:ERROR-KIND@ {: kind:n :}
   kind LINT-LEX:MALFORMED-REGISTRY = if E-PKGDIFF-ROW throw then
   kind LINT-LEX:UNTERMINATED-QUOTE = if E-PKGDIFF-QUOTE throw then
   E-PKGDIFF-LEX throw ;

\ Every scan of a reconstructed or on-disk source runs through here first: a
\ diagnostic truncates the token table at the defect, so continuing would analyse
\ a source that stops before the definitions the diff actually changed.
: LEX-CHECK ( -- )
   LINT-LEX:ERROR? if LEX-DEFECT then ;

: TOK=CI ( n ptr u8 n -- bool ) {: k:n a:ptr u:n :}
   k WORD? 0= if false exit then
   k LINT-LEX:TOKEN a u LINT-STR=CI ;

\ Case-sensitive token match, for the few rules that pin an exact spelling
\ rather than a Forth word identity.
: TOK= ( n ptr u8 n -- bool ) {: k:n a:ptr u:n :}
   k WORD? 0= if false exit then
   k LINT-LEX:TOKEN a u LINT-STR= ;

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

: REPORT-RESULT-OWNER ( -- )
   DEF-NAME-I @ s" E-PACKAGE-OWNERSHIP" REPORT-HEAD
   s" must remain the global result declaration" OUT LF-C OUT-C ;

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

\ Which opener a definer token is, as the bit the per-path rows are written in.
\ Zero means the token is not a declaration opener at all, in any row.
: COMPOSITE-OPENER-BIT ( n -- n ) {: k:n :}
   k s" SUMTYPE" TOK=CI if O-SUMTYPE exit then
   k s" PRODUCT" TOK=CI if O-PRODUCT exit then
   k s" ENUM" TOK=CI if O-ENUM exit then
   k s" VALUE-RECORD" TOK=CI if O-VALUE-RECORD exit then
   0 ;

: FAMILY-OPENER-BIT ( n -- n ) {: k:n :}
   \ the live family-introduction word (src/core/sumtype.f) and the only opener
   \ every one of the nine rows carries: 25 declarations across all of them
   k s" NEWTYPE" TOK=CI if O-NEWTYPE exit then
   k s" DEFTYPE" TOK=CI if O-DEFTYPE exit then
   k s" DEFLINEAR" TOK=CI if O-DEFLINEAR exit then
   \ cast-suite.f's subject: the checked retype declarer its whole contract is
   \ about, declared from real top-level user position.  It gets a bit on purpose
   \ even though DEFINER-KIND lexes CAST: as a colon block - these bits name
   \ declaration ROLE, which is a different question from block shape.
   k s" CAST:" TOK=CI if O-CAST exit then
   0 ;

: STORAGE-OPENER-BIT ( n -- n ) {: k:n :}
   k s" LAYOUT-BUFFER" TOK=CI if O-LAYOUT-BUFFER exit then
   k s" DEFER-LAYOUT-BUFFER" TOK=CI if O-DEFER-LAYOUT exit then
   k s" TYPED-VARIABLE" TOK=CI if O-TYPED-VARIABLE exit then
   k s" TYPED-BUFFER" TOK=CI if O-TYPED-BUFFER exit then
   k s" PTR-VARIABLE" TOK=CI if O-PTR-VARIABLE exit then
   0 ;

\ The effect declarer.  Like CAST: above it gets a bit even though DEFINER-KIND
\ lexes it as a colon block, because these bits name declaration ROLE.  A plain
\ `:` deliberately has no bit and never will: it is the ordinary definer, and a
\ bit for it would turn any listed row into a blanket exemption for its file.
: EFFECT-OPENER-BIT ( n -- n ) {: k:n :}
   k s" TRUSTED:" TOK=CI if O-TRUSTED exit then
   0 ;

: OPENER-BIT ( n -- n ) {: k:n :}
   k COMPOSITE-OPENER-BIT dup 0<> if exit then drop
   k FAMILY-OPENER-BIT dup 0<> if exit then drop
   k EFFECT-OPENER-BIT dup 0<> if exit then drop
   k STORAGE-OPENER-BIT ;

\ A listed path is necessary but not sufficient, and neither is a declaration in
\ the abstract: the definition must be a declaration THIS path's fixtures make.
\ The definer token is the one the scan already recorded when the definition
\ opened, so a declaration keyword written inside a comment or a string body is
\ not a definer and cannot admit anything.
: GRAMMAR-FIXTURE? ( -- bool )
   FILE$ s" test/" LINT-STARTS-WITH? 0= if false exit then
   FIXTURE-OPENER-SET {: allowed:n :}
   allowed 0= if false exit then
   DEF-DEFINER-I @ OPENER-BIT {: bit:n :}
   bit 0= if false exit then
   allowed bit and 0<> ;

\ The engine trunk -- habu2.f, layout.f, xref.f, habu1.f and the build drivers'
\ shared I/O, driver-io.f -- is the third principled category and the narrowest
\ of the three.  It is admitted for exactly one shape: a change to the BODY of a
\ global word that already exists.  Adding a new global word to any of them is
\ still reported, which is what separates this entry from the interim entries in
\ GLOBAL-IMPLEMENTATION? above.
\ the whole of stdout and the first stderr line against exact expected text.  The
\ fixture is the gate's own input and its correctness authority is that
\ whole-stream comparison, not a load through a package boundary.
\
\ Why packaging these two would delete the proof instead of satisfying the rule.
\ Both reasons are engine contracts and both were measured before this entry was
\ written.
\
\ test/bootstrap-using-checker-hook-src.f defines a stand-in CHECKER-USING so the
\ engine's mirror call is observable.  The emitter's C-USING-CHECK-CALL looks that
\ hook up by the bare 13-byte name `checker-using`, the same way the real global
\ CHECKER-USING in src/core/checker.f is found, and calls it with the package-name
\ token.  A package would put the tail in that package's wordlist, where a bare
\ lookup cannot see it: the mirror call would find nothing, the fixture's
\ `checker-using: BUS-A` line would disappear, and the case would stop testing the
\ mirror.
\
\ test/bootstrap-using-src.f defines BUS-SHADOW and BUS-CALLER at top level
\ because top-level bare visibility is the property under test.  BUS-SHADOW is the
\ name that must already resolve before `using BUS-A` opens, so the fixture can
\ prove an import never shadows a name that already resolves; from inside a
\ package nothing would resolve bare at the use site and the check would be
\ vacuous.  BUS-CALLER is compiled while the import is open, from the real
\ top-level position a recovery build's own source occupies; wrapping it in a
\ package would quietly change the subject to whether an outer import survives
\ into a newly opened package, which is a different question this fixture does not
\ answer.
\
\ The category is narrow in three structural ways.  Entries are exact whole paths,
\ so a global in any other fixture still reports.  Admission is also by shape:
\ only the plain lower-case `:` definer, because what these fixtures need is a
\ word the stage0 engine resolves by bare lookup.  A global `variable`, `create`,
\ `constant`, `CHECKED:`, `TRUSTED:` or type declaration in a listed fixture
\ reports like any other unpackaged global.  FINISH-DEFINITION checks SCOPE-DELTA
\ before this admission, so adding or deleting a package boundary around one of
\ these words is still reported.
\
\ The third narrowing is the pair of guards below: the path must start with `test/`
\ and end with `-src.f`, which is exactly how tools/bootstrap.sh composes the path
\ it builds (`test/$name-src.f`).  Be honest about what they do.  Against the exact
\ comparison and today's two rows they are redundant - removing either one alone
\ changes no verdict.  They exist for the edits that come later: they keep a future
\ row from admitting a library, tool or engine source, and they are what refuses a
\ hostile sibling if the path comparison itself is ever weakened.  Measured on
\ 2026-07-30: weaken STAGE0-PATH= to a suffix, prefix or case-folded comparison and
\ each of those weakenings is refused by one of these guards or reported by a named
\ hostile in tools/package-diff-lint-test.f; weaken the comparison AND drop the
\ matching guard and the hostile flips, which is how the unit test proves each
\ guard carries real weight rather than decoration.
\
\ There is no retirement condition.  Unlike the interim core-surface entries above
\ this is not debt waiting for a sealing pass: the recovery gate has to keep
\ proving what a real top-level user program sees, so while `using` exists in the
\ stage0 engine these fixtures have to define at top level.
4 constant STAGE0-ROW-CAP        \ headroom for the next fixture that needs a top-level word
256 constant STAGE0-TEXT-CAP     \ arena holding the row paths
create STAGE0-TEXT STAGE0-TEXT-CAP allot
create STAGE0-PATH-A STAGE0-ROW-CAP cells allot
create STAGE0-PATH-U STAGE0-ROW-CAP cells allot
variable STAGE0-ROW#
variable STAGE0-TEXT-U

: STAGE0-PATH$ ( n -- ptr u8 n ) {: i:n :}
   i 0 < i STAGE0-ROW# @ >= or if E-PKGDIFF-ROWTAB throw then
   i cells STAGE0-PATH-A + @   i cells STAGE0-PATH-U + @ ;

\ Rows are appended once at load time.  The path bytes are copied into the arena
\ because a source string literal is transient; storing its address would leave
\ every row pointing at whatever text was parsed last.
: STAGE0-ROW+ ( ptr u8 n -- ) {: a:ptr u:n :}
   STAGE0-ROW# @ STAGE0-ROW-CAP >= if E-PKGDIFF-ROWTAB throw then
   STAGE0-TEXT-U @ u + STAGE0-TEXT-CAP > if E-PKGDIFF-ROWTAB throw then
   STAGE0-TEXT STAGE0-TEXT-U @ + {: dst:ptr :}
   a dst u BYTE-COPY
   dst STAGE0-ROW# @ cells STAGE0-PATH-A + !
   u STAGE0-ROW# @ cells STAGE0-PATH-U + !
   STAGE0-TEXT-U @ u + STAGE0-TEXT-U !
   STAGE0-ROW# @ 1+ STAGE0-ROW# ! ;

\ THE path comparison for this category.  Every row is admitted or refused by
\ this one word, so "is this the listed fixture?" is decided in a single place:
\ whole path, exact bytes, no suffix match and no case folding.
: STAGE0-PATH= ( n ptr u8 n -- bool ) {: i:n a:ptr u:n :}
   a u i STAGE0-PATH$ LINT-STR= ;

: STAGE0-LISTED? ( -- bool )
   0 begin dup STAGE0-ROW# @ < while
      dup FILE$ STAGE0-PATH= if drop true exit then
      1+
   repeat drop false ;

\ One line per fixture that needs a word at global top level.
s" test/bootstrap-using-src.f" STAGE0-ROW+
s" test/bootstrap-using-checker-hook-src.f" STAGE0-ROW+

: STAGE0-FIXTURE? ( -- bool )
   FILE$ s" test/" LINT-STARTS-WITH? 0= if false exit then
   FILE$ s" -src.f" LINT-ENDS-WITH? 0= if false exit then
   STAGE0-LISTED? 0= if false exit then
   DEF-DEFINER-I @ s" :" TOK= ;

\ The native engine emitter, src/habu/habu2.f, is the third principled category
\ and the narrowest of the three.  It is admitted for exactly one shape: a change
\ to the BODY of a global word that already exists.  Adding a new global word to
\ that file is still reported, which is what separates this entry from the
\ interim entries in GLOBAL-IMPLEMENTATION? above.
\
\ Why the file needs an entry at all.  habu2.f is about 7,300 lines of code that
\ emits the machine code of the engine itself.  Almost all of it is global by
\ construction: it runs while the engine is being built, before any package
\ exists, and the assembler words, register names, label helpers and emit passes
\ resolve each other bare in one flat namespace.  Measured on 2026-07-29: adding
\ a single trailing comment to the body of EM-SNAPSHOT-RX-FLUSH -- defining no
\ new word and changing no behaviour -- reported
\ `E-PACKAGE-OWNERSHIP src/habu/habu2.f:4134:3`, so as configured the gate
\ rejected every possible change to the engine emitter, including the repair of
\ the snapshot relocation regression that found this.
\
\ Why it is narrower than the core-surface entries.  Unlike checker.f and
\ render.f, habu2.f is not a growing language surface; it is a body of existing
\ code being packaged file-region by file-region.  The packaging seams are real
\ and already present -- SNAP-RELOC, KWDATA, LOOP-EMIT, LASTC-TRUST,
\ DOESPATCH, INTERP-EMIT, COMPILE-EMIT, LABELS, ENGINE-BUILD, LOWER-TXN and the
\ rest all open in this same file -- so a genuinely new engine word has a package
\ to join and must join one.  The distinction is structural, not a value guess:
\ what separates the two shapes is whether the file already defined this NAME at
\ global scope, and that is a fact read out of the diff's own pre-image.
\
\ Why src/habu/layout.f is the second member.  It is the same kind of file for
\ the same reason: about 240 constants that name the image, dictionary and DATA
\ layout, all global by construction because every engine source reads them bare
\ while the engine is being built, and its packaging is blocked on the stage0
\ recovery compiler learning the `using` keyword (dot habu-add-using-to-d815f0ab).
\ Without this entry the gate rejects any change to an existing layout constant's
\ value -- measured 2026-07-29: bumping SNAP-FORMAT-VERSION from 4 to 5 and
\ deriving DATA-START from the new relocation bands reported E-PACKAGE-OWNERSHIP
\ on both, so the snapshot format could not be versioned at all.  New layout
\ constants still have to join a package, which is what the relocation bands did.
\
\ The key, and the one it replaced.  This entry first asked whether the diff had
\ touched the line the definition's opener or name sits on (DEF-TAIL-ADDED, which
\ CHECK-PREFIX still uses).  For a colon word that separates a body edit from a
\ new definition exactly, because the body is on other lines.  For a `constant`
\ it cannot: the definer, the name and the value share one line, so changing the
\ VALUE necessarily marks the head as added, and `4 constant SNAP-FORMAT-VERSION`
\ becoming `5 constant SNAP-FORMAT-VERSION` looks byte for byte like a brand-new
\ constant.  layout.f is nothing but constants, so keyed on that this entry
\ admitted nothing it was minted for: both real findings above survived it.
\
\ The key is now the fact that question was standing in for -- was this NAME
\ already defined at global scope in this file's pre-image (OLD-GLOBAL? above).
\ It is one structural fact rather than a line-position proxy, and it is strictly
\ narrower than "the head line is unchanged OR the name is old".  Measured: the
\ head-line test admits nothing the name test rejects EXCEPT one shape, and it
\ decides that shape wrongly.  A definition the old file kept inside a `( ... )`
\ comment becomes a real global word the moment the diff changes the comment's
\ opening line, without one byte of the definition's own line changing: the
\ head-line test sees an untouched head and admits a word the file never had,
\ and the name test reports it.  A disjunct whose only distinguishing case is one
\ it gets wrong is not a second opinion, so it is gone rather than kept for
\ safety.  tools/package-diff-lint-test.f pins that shape.
\
\ Three consequences are deliberate.  A definition of an EXISTING global name
\ whose head line was deleted and rewritten -- moved within the file, resplit, or
\ retyped from `:` to `CHECKED:` -- is admitted, because afterwards the file's
\ global surface is the same set of names, and that surface is precisely what
\ this entry guards.  A SECOND definition of a name the file already defines is
\ admitted for the same reason: it publishes no name the file did not already
\ publish.  Whether a trunk file should carry two definitions of one word is a
\ real question, but it is a question about redefinition, not about who owns the
\ name.  And a rename is admitted in neither direction: the arriving spelling is
\ a name this file never had, so it is reported, while the departing one's
\ deletion is the packaging dot's business rather than this entry's.
\
\ Why src/habu/xref.f is the third member.  Same kind of file and same rule: it
\ is the live-dictionary surface of the cold prefix - the record readers, the
\ lookup helpers, the retirement and truncation entries - and every one of those
\ names is resolved bare by later prefix sources and by the REPL.  Its packaging
\ census is already written down (dot habu-pkg-dictionary-cross-73f449d9: 88
\ unowned definitions, to be split into a read package and a lifecycle package),
\ so like habu2.f and layout.f it is a body of existing code waiting to be
\ packaged rather than a growing language surface.  Measured 2026-08-03: adding
\ the code-reclamation notice to the last line of the existing global
\ FORGET-DEFS-FROM - one call, publishing no new name - reported
\ `E-PACKAGE-OWNERSHIP src/habu/xref.f:504:3`, so as configured the gate rejected
\ every possible change to the dictionary-truncation path, including the repair
\ of the stale-row miscompiles that found this.  The asymmetry is earned here for
\ the same structural reason it is earned for habu2.f: xref.f already opens real
\ packages (PKG-AUTH, GENERATED-DECL-NAME-PREFLIGHT, CODE-RECLAIM), so a
\ genuinely new word in it has an owner it can join and must join one - and the
\ notice's own package is exactly that shape.
\
\ Why src/habu/habu1.f is the fourth member.  It is habu2.f's other half and the
\ same kind of file: the arm64 assembler vocabulary, the register aliases, the
\ label helpers and the primitive emitters that habu2.f's passes call bare while
\ the engine is being built.  Measured 2026-08-04: adding a single trailing
\ comment to the body of the existing global EMIT-FLUSH -- defining no new word
\ and changing no behaviour -- reported
\ `E-PACKAGE-OWNERSHIP src/habu/habu1.f:2714:3`, so as configured the gate
\ rejected every possible change to the engine's primitive emitters, including
\ the dictionary-lookup repair that found this (dot
\ habu-compile-shaped-cost-4e74a181, which had to change the bodies of the
\ existing global BNDSET and EMIT-FIND and nothing else).  The asymmetry is
\ earned on the same structural fact as the other three: habu1.f already opens
\ ENGINE-BUILD, ENGINE-HELPER, GUARD, PROT-GUARD and ENGINE-EMIT, so a
\ genuinely new word in it has an owner it can join and must join one -- which
\ is what the repair above did with its five new labels, keeping them lexical
\ inside the word that emits them rather than adding five globals.
\
\ Why src/habu/driver-io.f is the fifth member.  Same kind of file and the same
\ rule, one layer out from the engine: it is the shared fail-closed I/O every
\ internal build driver runs on - the image-emission tail, the size-map
\ reconciliation and the uncaught-throw reporter - and it is CONCATENATED into
\ every build source (tools/bootstrap.sh SRC_COMMON, tools/build-fixpoint.f
\ BF-APPEND-DRIVER-IO) rather than required, so stage2.f, build.f, maker.f,
\ stdin.f, snap-lib.f and aot-lib.f all resolve its 35 DRV-* words bare in one
\ flat namespace.  Its packaging census is already written down and scoped as its
\ own P1 task (dot habu-pkg-build-driver-acbd02b7: wrap it in package DRIVER-IO,
\ export only the emit-image, success/failure, size-map and reload-retirement
\ entries, with byte-golden fixtures on every command path and before/after
\ size and latency measurements), so like the four above it is a body of existing
\ code waiting on a packaging dot rather than a growing language surface.
\ Measured 2026-08-15: extracting the whole-span descriptor write into
\ src/habu/fdio.f and repointing its one caller - replacing the single body line
\ `DRV-WFD @ MBUF MLEN@ DRV-WALL` with `... FDIO:WALL`, publishing no new name in
\ this file and deleting one - reported
\ `E-PACKAGE-OWNERSHIP src/habu/driver-io.f:17:3` on DRV-WRITE-IMAGE-PATH, and
\ restoring driver-io.f to its pre-image with the rest of that diff in place
\ returned the gate to rc 0, which is how the finding was pinned to that one
\ line.  As configured the gate therefore rejected every possible change to the
\ build drivers' I/O, including the deduplication of the short-write loop that
\ found this: a booted engine cannot load driver-io.f at all (E-UNDEFINED: MBUF,
\ with six files including the target's Mach-O writer between), so a capture
\ writing an artifact inside bin/hb had no way to reach the tree's one write-all
\ loop.  The asymmetry is earned on the same structural fact as the other four:
\ the file's own successor package DRIVER-IO is already specified on that dot, so
\ a genuinely new word here has an owner it can join and must join one - which is
\ what fdio.f did, opening package FDIO for the extracted concern rather than
\ adding a fifth global beside the DRV-* set.
\
\ Retirement condition.  The habu2.f half is removed when the continuing habu2
\ packaging work (dot habu-cont-habu2-emitter-493363e7) extends those seams over
\ the remaining global surface, the layout.f half when dot
\ habu-give-layout-f-315df2ca finishes packaging that file, the xref.f half
\ when dot habu-pkg-dictionary-cross-73f449d9 splits that file into its read and
\ lifecycle packages, the habu1.f half when dot
\ habu-pkg-the-engine-842acca7 packages the primitive emitters, and the
\ driver-io.f half when dot habu-pkg-build-driver-acbd02b7 wraps that file in
\ package DRIVER-IO - exactly as the checker and render entries are removed by
\ their own sealing dots.  That dot carries the matching obligation in its own
\ leaf (RETIREMENT CLAUSE, 2026-08-15): removing this row is part of its
\ acceptance, so the two point at each other and neither can drift alone.
\ FINISH-DEFINITION
\ checks SCOPE-DELTA before it consults this admission, so adding or deleting a
\ package boundary around an engine word is still reported here like anywhere
\ else.  A rename or copy that makes some other file arrive AT one of these paths
\ is not a body edit of an existing engine word -- every definition in it is new
\ here even though no line of it is marked added -- so WHOLE-CHANGED closes that
\ hole and reports the whole file.
\
\ The five paths are rows reached through ONE comparison site, for the reason the
\ fixture row table below gives: a weakening -- a suffix match, a case fold, a
\ prefix test -- then has exactly one place to live and changes every row at once,
\ so the hostile fixtures kill it on all of them.
5 constant ENGINE-TRUNK-N

: ENGINE-TRUNK-AT ( n -- ptr u8 n ) {: row:n :}
   row 0= if s" src/habu/habu2.f" exit then
   row 1 = if s" src/habu/layout.f" exit then
   row 2 = if s" src/habu/xref.f" exit then
   row 3 = if s" src/habu/habu1.f" exit then
   s" src/habu/driver-io.f" ;

: ENGINE-TRUNK-PATH? ( -- bool )
   0 TRUNK-HIT !
   ENGINE-TRUNK-N 0 ?do
      FILE$ i ENGINE-TRUNK-AT LINT-STR= if 1 TRUNK-HIT ! then
   loop
   TRUNK-HIT @ 0 <> ;

: ENGINE-BODY-EDIT? ( -- bool )
   ENGINE-TRUNK-PATH? 0= if false exit then
   WHOLE-CHANGED @ if false exit then
   DEF-NAME-I @ LINT-LEX:TOKEN OLD-GLOBAL? ;

\ The Gforth recovery mirror -- bootstrap/cg/forth.fs -- is the fourth principled
\ category, and the only one where satisfying the ownership rule is not merely
\ inconvenient but impossible.
\
\ What the file is.  It is the stage-0 code generator that emits a standalone
\ native Forth without any habu binary, and it is compiled by GFORTH, not by
\ `bin/hb`.  `package`, `public`, `private` and `;package` are habu engine words:
\ Gforth does not have them, so a `package NAME` line in this file would abort the
\ no-binary recovery documented in docs/bootstrap.md before it emitted anything.
\ Every definition in it is therefore global by necessity.  The one occurrence of
\ the bytes `;package` in the file is a string literal in the keyword table this
\ emitter WRITES for the engine it builds, which is the opposite of the file
\ opening a scope of its own.
\
\ Why the file needs an entry at all.  Measured 2026-07-30: adding a single
\ trailing comment to the body of the existing global BCOUNT -- defining no new
\ word and changing no behaviour -- reported
\ `E-PACKAGE-OWNERSHIP bootstrap/cg/forth.fs:811:3`, so as configured the gate
\ rejected every possible change to the recovery emitter.  The stage-0 `using`
\ work (dot habu-add-using-to-d815f0ab) is the live consumer: 40 of the 44
\ findings on that commit are this one gap.
\
\ Why BOTH directions are admitted, unlike the engine trunk.  ENGINE-BODY-EDIT?
\ above admits a body edit and still reports a NEW global, and that asymmetry is
\ earned by a fact about the trunk files: habu2.f and layout.f are compiled by
\ `bin/hb` and already open real packages, so a genuinely new engine word has an
\ owner it CAN join and must join.  Here there is no such owner and there never
\ will be while the file is Gforth-hosted, so reporting a new global would be
\ reporting a fault whose only repair breaks the recovery path.  The category
\ therefore admits changed and new definitions alike.
\
\ What owns the mirror's correctness instead.  Not package scope -- the parity
\ gates.  tools/bootstrap-mirror-lint.f names it as the file whose absent
\ width-aware pass makes the src/ declaration boundary a red gate, and the
\ recovery fixtures driven by tools/bootstrap.sh run it end to end.  A
\ definition added here is reviewed by those gates, which read what the emitter
\ produces, and that is a stronger statement about this file than a wordlist name
\ would be.
\
\ Why this exact path and not its neighbours.  There are 63 other Gforth-hosted
\ `.fs` sources in the tree (bootstrap/cg, bootstrap/src, the bootstrap load
\ drivers, and the two Gforth test harnesses test/nf.fs and
\ test/bootstrap-wide-memory.fs), and all 64 were measured on 2026-07-30 by
\ appending one global definition to each and running this lint: every one of them
\ reports E-PACKAGE-OWNERSHIP, so the gate does scan them and none of them is
\ admitted by accident.  They are left reporting on purpose.  The argument above
\ has two halves -- packaging is impossible, AND named parity gates own the file's
\ correctness instead -- and only bootstrap/cg/forth.fs has the second half today.
\ A row is added for a sibling when a real change to it is blocked and its own
\ compensating authority can be named, one measurement per row, which is what the
\ exact-path rule in docs/forth.md is for.  The extension is deliberately NOT the
\ key: `.fs` alone would admit any future file that happened to be named that way.
\
\ A rename or copy that makes some other file arrive AT this path is reported, the
\ same way the trunk reports it.  The mirror is one committed file whose content
\ the parity gates know; a wholesale replacement arriving at its path is exactly
\ the event where that authority has not yet looked, and WHOLE-CHANGED marks no
\ line as added, so without this the whole arriving file would ride in unread.
\ FINISH-DEFINITION also checks SCOPE-DELTA before it consults this admission, so
\ the impossible-but-attempted case of a `package` boundary appearing in the
\ mirror is still reported.
\
\ Retirement condition.  Unlike the interim entries above this one has no
\ packaging dot to wait for, because there is nothing to package: it retires when
\ the recovery path stops being Gforth-hosted, that is, when bootstrap/cg/forth.fs
\ is replaced by checked habu source that `bin/hb` compiles.
\
\ The path is a row table reached through ONE comparison site, for the reason the
\ fixture row table gives: a weakening -- a suffix match, a case fold, a prefix
\ test -- then has exactly one place to live and moves every row at once, so the
\ hostile fixtures in tools/package-diff-lint-test.f kill it.
1 constant MIRROR-N

: MIRROR-AT ( n -- ptr u8 n ) {: row:n :}
   row 0 < row MIRROR-N >= or if E-PKGDIFF-ROWTAB throw then
   s" bootstrap/cg/forth.fs" ;

: MIRROR-PATH? ( -- bool )
   0 MIRROR-HIT !
   MIRROR-N 0 ?do
      FILE$ i MIRROR-AT LINT-STR= if 1 MIRROR-HIT ! then
   loop
   MIRROR-HIT @ 0 <> ;

: MIRROR-EDIT? ( -- bool )
   MIRROR-PATH? 0= if false exit then
   WHOLE-CHANGED @ 0= ;

: EXACT-ENUM? ( ptr u8 n ptr u8 n -- bool )
   {: path:ptr pathu:n name:ptr nameu:n :}
   FILE$ path pathu LINT-STR= 0= if false exit then
   DEF-DEFINER-I @ s" ENUM" TOK=CI 0= if false exit then
   DEF-NAME-I @ name nameu TOK=CI ;

\ src/core/type-family.f is `package TFAM` now (dot habu-tfam-2b-sealed-1b77662c):
\ 221 publics and 345 privates under one owner, with nothing renamed, so its
\ whole-file GLOBAL-IMPLEMENTATION? entry is gone.  SIX names could not go with
\ them, and the reason is the ENGINE's, not a preference: it resolves each one by
\ a bare global spelling that no package spelling can reach.
\
\ Four are the `construct`/`match`/`;match` keyword bridge.  habu2.f
\ EM-ADT-MATCH-FAM and EM-MATCH-SEMI call C-FIND-GLOBAL, which ZEROES the open
\ package's wordlist cells before the lookup so that the answer cannot depend on
\ where the user's `match` is written; the four spellings are baked byte for byte
\ into KWDATA (habu2.f:1955) and mirrored in the Gforth recovery emitter
\ (bootstrap/cg/forth.fs:2612).  Measured on the candidate: moving TFL-MATCH-FAM?
\ into the package made every engine boot write `tfl-match-fam?` to fd 2 and exit
\ 70, because that lookup is what compiles the keyword.
\
\ Two more are named by AOT-captured call sites.  The boot seed re-resolves each
\ baked callee by (name, scope) and admits a global scope or an explicitly
\ qualified pooled name; a `using`-imported bare name is neither, and a qualified
\ one is not resolvable at seed time.  Measured: `using TFAM` in src/habu/xref.f
\ and `package TFAM` in src/core/type-family-sha.f each produced `hb: AOT call
\ site unresolved`, exit 82, on every run of the built engine.
\
\ The row is exact path + exact name + exact definer, so a seventh global added
\ beside them still reports, and so does any of the six re-declared by another
\ definer or in another file.  Retirement: it goes when the engine learns to
\ resolve these six under their owner - the keyword bridge through a qualified
\ C-FIND, the AOT pool through a scope the seed can rebase - not before.
: TFAM-BRIDGE-COLON? ( -- bool )
   DEF-NAME-I @ s" TFAM-NAME$" TOK=CI if true exit then
   DEF-NAME-I @ s" TFL-CON-FAM?" TOK=CI if true exit then
   DEF-NAME-I @ s" TFL-CVAR?" TOK=CI if true exit then
   DEF-NAME-I @ s" TFL-MATCH-FAM?" TOK=CI if true exit then
   DEF-NAME-I @ s" TFAM-CTOR-WORD?" TOK=CI ;

: TFAM-BRIDGE? ( -- bool )
   FILE$ s" src/core/type-family.f" LINT-STR= 0= if false exit then
   DEF-DEFINER-I @ s" :" TOK= if TFAM-BRIDGE-COLON? exit then
   DEF-DEFINER-I @ s" defer" TOK= 0= if false exit then
   DEF-NAME-I @ s" TF-SHA16-XT" TOK=CI ;

: GLOBAL-SURFACE? ( -- bool )
   GLOBAL-IMPLEMENTATION? if true exit then
   GRAMMAR-FIXTURE? if true exit then
   STAGE0-FIXTURE? if true exit then
   ERR-VOCAB? if true exit then
   ENGINE-BODY-EDIT? if true exit then
   MIRROR-EDIT? if true exit then
   TFAM-BRIDGE? if true exit then
   s" lib/adt/option.f" s" option" EXACT-ENUM? if true exit then
   s" lib/adt/result.f" s" result" EXACT-ENUM? if true exit then
   FILE$ s" lib/type/deftype.f" LINT-STR= if
      DEF-NAME-I @ s" DEFTYPE" TOK=CI exit
   then
   \ Same shape and same reason as DEFTYPE above, forced by the checker rather
   \ than chosen: `ix`, `extprod` and `redx` are engine-registered cell families
   \ (src/core/type-family.f) declared in the global, empty package, and a CHECKED
   \ cast that INTRODUCES a value into a cell family is authorized only from that
   \ family's declaring package (src/core/checker.f CAST-OWNER?). The converter
   \ pair for those families therefore cannot live in a package at all. These are
   \ the only two names the file defines; a third global added beside them is
   \ still reported.
   FILE$ s" lib/type/extent-role.f" LINT-STR= if
      DEF-NAME-I @ s" IX>N" TOK=CI
      DEF-NAME-I @ s" >RED" TOK=CI or exit
   then
   FILE$ s" src/core/structure-decl.f" LINT-STR= if
      DEF-NAME-I @ s" STRUCTURE" TOK=CI exit
   then
   \ Same shape and same reason as STRUCTURE above: ENUM is one of the executable
   \ composite-declaration keywords, a documented global language surface, and it
   \ is the ONE name in this file that may stay global. Everything else the file
   \ defines lives in package ENUM-DECL and is still reported.
   FILE$ s" src/core/enum-decl.f" LINT-STR= if
      DEF-NAME-I @ s" ENUM" TOK=CI exit
   then
   false ;

\ A changed unpackaged definition normally loses its package owner.  A non-zero
\ SCOPE-DELTA means a `package`/`;package` boundary around this definition was
\ added or deleted in this diff, so its ownership genuinely changed: that is
\ reported for every file, including the allowlisted core surface (checker.f and
\ render.f still stand beside real packages).  Only a plain body change or
\ whole-file change of an already-global definition is exempt, and only on that
\ surface.
: FINISH-DEFINITION ( n -- ) {: last-line:n :}
   DEF-PACKAGED @ if
      s" lib/adt/result.f" s" result" EXACT-ENUM? if
         REPORT-RESULT-OWNER
      else
         CHECK-PREFIX
      then
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
   k LINT-DEF:NAME-I
   MATCH option
      none OF E-PKGDIFF-NONAME throw ENDOF
      some OF {: namei:n :}
         kind DEF-KIND !
         k DEF-DEFINER-I !
         namei DEF-NAME-I !
         k LINT-LEX:LINE@ DEF-START-LINE !
         PACKAGE-OPEN @ DEF-PACKAGED !
         k LINT-LEX:LINE@ ADDED? namei LINT-LEX:LINE@ ADDED? or DEF-TAIL-ADDED !
         kind LINT-DEF:DATA = if
            namei LINT-LEX:LINE@ FINISH-DEFINITION
         else
            true DEF-OPEN !
         then
         namei 1+ LEX-I !
      ENDOF
   ;MATCH ;

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
   k OPAQUE? if k 1+ LEX-I ! exit then
   DEF-OPEN @ if
      k DEF-KIND @ LINT-DEF:CLOSE? if
         k LINT-LEX:LINE@ FINISH-DEFINITION
      then
      k 1+ LEX-I ! exit
   then
   k PACKAGE-TOKEN if exit then
   k LINT-DEF:DIRECT-KIND dup LINT-DEF:NONE = if drop k 1+ LEX-I ! exit then
   k swap START-DEFINITION ;

: APPLY-DELETED-DELTA ( n -- ) {: line:n :}
   begin SCAN-LINE @ line <= while
      SCOPE-DELTA @ SCAN-LINE @ DELTA@ + SCOPE-DELTA !
      SCAN-LINE @ 1+ SCAN-LINE !
   repeat ;

: SCAN-DEFINITION-PASS ( -- )
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

: SCAN-DEFINITIONS ( -- )
   SOURCE$ LINT-LEX:SOURCE
   LEX-CHECK
   SCAN-DEFINITION-PASS ;

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

\ The old scan's second job: every definition it passes at global scope leaves its
\ name in the table ENGINE-BODY-EDIT? reads.  PACKAGE-OPEN is the old side's own
\ scope, tracked by OLD-PACKAGE-TOKEN, so a definition that was owned then does
\ not count as part of the old global surface now.
: OLD-START-DEFINITION ( n n -- ) {: k:n kind:n :}
   k LINT-DEF:NAME-I
   MATCH option
      none OF E-PKGDIFF-NONAME throw ENDOF
      some OF {: namei:n :}
         PACKAGE-OPEN @ 0= if namei OLD-NAME+ then
         kind LINT-DEF:DATA = if
            namei 1+ LEX-I !
         else
            kind DEF-KIND !
            true DEF-OPEN !
            namei 1+ LEX-I !
         then
      ENDOF
   ;MATCH ;

: OLD-SCAN-TOKEN ( n -- ) {: k:n :}
   k OPAQUE? if k 1+ LEX-I ! exit then
   DEF-OPEN @ if
      k DEF-KIND @ LINT-DEF:CLOSE? if false DEF-OPEN ! then
      k 1+ LEX-I ! exit
   then
   k OLD-PACKAGE-TOKEN if exit then
   k LINT-DEF:DIRECT-KIND dup LINT-DEF:NONE = if drop k 1+ LEX-I ! exit then
   k swap OLD-START-DEFINITION ;

: SCAN-OLD-BOUNDARIES ( -- )
   OLD$ LINT-LEX:SOURCE
   LEX-CHECK
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
   HABU? 0= if exit then
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
   false FAIL-NEXT-NAME-ALLOC !
   0 FORCE-NAME-LIMIT !
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
   false FAIL-NEXT-NAME-ALLOC !
   0 FORCE-NAME-LIMIT !
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
