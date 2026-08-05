\ run-closure-lint-test.f - focused fixtures for the phase-set closure lint.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/memory.f
\   lib/fs.f lib/fs-mutate.f test/run-closure-lint.f
\   test/run-closure-lint-test.f
\
\ The lint in test/run-closure-lint.f had no fixtures at all: it ran only over
\ the real declared sets, so its one real finding could not be told apart from
\ a scanner bug by anything except reading the scanner. It was, in fact, right
\ and the declared set was stale -- but that took a full investigation to
\ establish. These cases pin each edge shape the scanner accepts and each shape
\ it must refuse, so the next finding is trustworthy on sight.

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/fs.f
require lib/fs-mutate.f
require test/run-closure-lint.f

package RCLT

$400 constant BODY-CAP

create ROOT-BUF FS-PATH-CAP allot
create A-BUF FS-PATH-CAP allot
create B-BUF FS-PATH-CAP allot
create TXT-BUF FS-PATH-CAP allot
create BODY-BUF BODY-CAP allot

variable ROOT-LEN
variable A-LEN
variable B-LEN
variable TXT-LEN
variable BODY-LEN

: ROOT$ ( -- ptr u8 n )
   ROOT-BUF ROOT-LEN @ ;

: A$ ( -- ptr u8 n )
   A-BUF A-LEN @ ;

: B$ ( -- ptr u8 n )
   B-BUF B-LEN @ ;

: TXT$ ( -- ptr u8 n )
   TXT-BUF TXT-LEN @ ;

: BODY$ ( -- ptr u8 n )
   BODY-BUF BODY-LEN @ ;

\ --- fixture body builder ----------------------------------------------------

: BODY-RESET ( -- )
   0 BODY-LEN ! ;

: BODY+ ( ptr u8 n -- ) {: a:ptr u:n :}
   BODY-LEN @ u + BODY-CAP > if E-STR-CAPACITY throw then
   a BODY-BUF BODY-LEN @ + u BYTE-COPY
   BODY-LEN @ u + BODY-LEN ! ;

: BODY-C ( n -- ) {: c:n :}
   BODY-LEN @ 1 + BODY-CAP > if E-STR-CAPACITY throw then
   c BODY-BUF BODY-LEN @ + c!
   BODY-LEN @ 1 + BODY-LEN ! ;

: BODY-NL ( -- )
   STR-LF BODY-C ;

: BODY-DQ ( -- )
   $22 BODY-C ;

: BODY-SP ( -- )
   $20 BODY-C ;

\ --- fixture files -----------------------------------------------------------

: A! ( -- )                                  \ the member under scan
   A$ BODY$ WRITE-ALL ;

: SETUP ( -- )
   CLEANUP-RESET
   s" habu-closure-lint" TMPDIR-MKDIR {: a:ptr u:n :}
   a ROOT-BUF u BYTE-COPY
   u ROOT-LEN !
   ROOT$ CLEANUP-TREE+
   ROOT$ s" a.f" A-BUF JOIN-PATH A-LEN !
   ROOT$ s" b.f" B-BUF JOIN-PATH B-LEN !
   B$ s" \ fixture b" WRITE-ALL           \ b.f exists on disk for every case
   ROOT$ s" b.txt" TXT-BUF JOIN-PATH TXT-LEN !
   TXT$ s" not a source" WRITE-ALL        \ ...and so does a same-stem non-source
   RUN-CLOSURE:PREPARE ;

\ --- running one case --------------------------------------------------------
\ Every case declares a set, scans it, and asserts the exact finding count.
\ SET-A-ONLY leaves b.f out of the set, so a real edge to it must report;
\ SET-A-AND-B puts it in, so the same edge must not.

: SET-A-ONLY ( -- )
   RUN-CLOSURE:RESET
   RUN-CLOSURE:SET-RESET
   A$ RUN-CLOSURE:SET+ ;

: SET-A-AND-B ( -- )
   SET-A-ONLY
   B$ RUN-CLOSURE:SET+ ;

: FINDINGS ( -- n )
   RUN-CLOSURE:RUN
   RUN-CLOSURE:FINDINGS ;

\ --- the shapes that MUST report ---------------------------------------------

: CASE-REQUIRE ( -- )
   BODY-RESET
   s" require " BODY+  B$ BODY+  BODY-NL
   A!
   s" a plain require edge to a non-member reports" T-LABEL
   SET-A-ONLY FINDINGS 1 T= ;

: CASE-INCLUDE ( -- )
   BODY-RESET
   s" include " BODY+  B$ BODY+  BODY-NL
   A!
   s" an include edge to a non-member reports" T-LABEL
   SET-A-ONLY FINDINGS 1 T= ;

: CASE-INDENTED-REQUIRE ( -- )
   BODY-RESET
   s"    require " BODY+  B$ BODY+  BODY-NL
   A!
   s" leading whitespace does not hide a require edge" T-LABEL
   SET-A-ONLY FINDINGS 1 T= ;

: CASE-LITERAL ( -- )
   BODY-RESET
   s" : W ( -- ptr u8 n ) s" BODY+  BODY-DQ  BODY-SP
   B$ BODY+  BODY-DQ  s"  ;" BODY+  BODY-NL
   A!
   s" an s-quote literal naming a non-member reports" T-LABEL
   SET-A-ONLY FINDINGS 1 T= ;

: CASE-BOTH-SHAPES ( -- )
   BODY-RESET
   s" require " BODY+  B$ BODY+  BODY-NL
   s" : W ( -- ptr u8 n ) s" BODY+  BODY-DQ  BODY-SP
   B$ BODY+  BODY-DQ  s"  ;" BODY+  BODY-NL
   A!
   s" each naming site reports once" T-LABEL
   SET-A-ONLY FINDINGS 2 T= ;

\ --- the shapes that MUST NOT report -----------------------------------------

: CASE-MEMBER ( -- )
   BODY-RESET
   s" require " BODY+  B$ BODY+  BODY-NL
   A!
   s" a require edge to a declared member is clean" T-LABEL
   SET-A-AND-B FINDINGS 0 T= ;

\ The hostile group. A commented-out require line is not an edge: the member
\ does not read that file, so reporting it would force an unrelated file into
\ the phase key. Text is not an edge.
\ This one is held by the require path's own token test -- `\` is the first
\ token, so `require` is never in first position -- which is why the literal
\ case below is the one that needs the comment skip.
: CASE-COMMENTED-REQUIRE ( -- )
   BODY-RESET
   s" \ require " BODY+  B$ BODY+  BODY-NL
   A!
   s" a commented require line is not read as a require" T-LABEL
   SET-A-ONLY FINDINGS 0 T= ;

: CASE-COMMENTED-LITERAL ( -- )
   BODY-RESET
   s" \ see s" BODY+  BODY-DQ  BODY-SP
   B$ BODY+  BODY-DQ  BODY-NL
   A!
   s" an s-quote literal inside a comment is not an edge" T-LABEL
   SET-A-ONLY FINDINGS 0 T= ;

\ The keyword swallowed into a string is not an edge either: the literal is the
\ whole span, and `require <path>` is not a path that exists.
: CASE-REQUIRE-IN-STRING ( -- )
   BODY-RESET
   s" : W ( -- ptr u8 n ) s" BODY+  BODY-DQ  BODY-SP
   s" require " BODY+  B$ BODY+  BODY-DQ  s"  ;" BODY+  BODY-NL
   A!
   s" a require keyword inside a string literal is not an edge" T-LABEL
   SET-A-ONLY FINDINGS 0 T= ;

\ Membership is the DECLARED set, never text found while scanning. Spelling the
\ path in a comment must not admit it, so the real edge below still reports.
: CASE-COMMENT-CANNOT-DECLARE ( -- )
   BODY-RESET
   s" \ member: " BODY+  B$ BODY+  BODY-NL
   s" require " BODY+  B$ BODY+  BODY-NL
   A!
   s" naming a path in a comment does not make it a member" T-LABEL
   SET-A-ONLY FINDINGS 1 T= ;

\ A path that is not on disk is not a source the phase reads.
: CASE-MISSING-TARGET ( -- )
   BODY-RESET
   s" require " BODY+  ROOT$ BODY+  s" /never-written.f" BODY+  BODY-NL
   A!
   s" a require edge to a non-existent path is skipped" T-LABEL
   SET-A-ONLY FINDINGS 0 T= ;

\ `s"` must be a word, not a tail glued to the previous one.
: CASE-GLUED-SQUOTE ( -- )
   BODY-RESET
   s" : W ( -- ptr u8 n ) XXs" BODY+  BODY-DQ  BODY-SP
   B$ BODY+  BODY-DQ  s"  ;" BODY+  BODY-NL
   A!
   s" an s-quote glued to a preceding token is not a literal" T-LABEL
   SET-A-ONLY FINDINGS 0 T= ;

\ A non-source extension is not a source reference. b.txt is written in SETUP,
\ so this case turns on the extension test alone and not on the existence one.
: CASE-NON-SOURCE-EXT ( -- )
   BODY-RESET
   s" require " BODY+  TXT$ BODY+  BODY-NL
   A!
   s" a reference without a source extension is skipped" T-LABEL
   SET-A-ONLY FINDINGS 0 T= ;

\ --- src/ members are keyed but not scanned ----------------------------------
\ src/compiler/binding.f requires lib/prelude.f, lib/errors.f and
\ src/compiler/digest.f, none of which this set declares. It must still be
\ clean, because the src/ skip is what keeps the engine sources out of every
\ phase's scan.

: CASE-SRC-NOT-SCANNED ( -- )
   RUN-CLOSURE:RESET
   RUN-CLOSURE:SET-RESET
   s" src/compiler/binding.f" RUN-CLOSURE:SET+
   s" a src/ member is keyed but not scanned" T-LABEL
   FINDINGS 0 T= ;

\ --- the literal-scan exemption, on the real exempt file ----------------------
\ tools/hook-sites.f is a TABLE of audited hook paths, not a loader: the lint
\ that reads it (CHECKED-BOUNDARY-LINT:FILE) takes whatever path its caller
\ hands it, so the eleven rows are policy data and keying a phase on them would
\ be a false coverage claim. The exemption must therefore cover the literal
\ scan and nothing else.
\
\ One count kills both ways of getting that wrong. Scanned alone, the file has
\ exactly three edges -- its own require lines for lib/errors.f, lib/prelude.f
\ and lib/string.f -- and none of the three is a member here. Drop the
\ exemption and its eleven path rows join them (14); widen it to cover require
\ lines too and they all vanish (0). Only the real scope reads 3.

: CASE-EXEMPT-SCOPE ( -- )
   RUN-CLOSURE:RESET
   RUN-CLOSURE:SET-RESET
   s" tools/hook-sites.f" RUN-CLOSURE:SET+
   s" the exemption covers the literal scan and not require lines" T-LABEL
   FINDINGS 3 T= ;

: CLEANUP ( -- )
   CLEANUP-RUN
   ROOT$ EXISTS? TFALSE ;

: MAIN ( -- )
   T-RESET
   SETUP
   RUN-CLOSURE:REPORT-OFF
   CASE-REQUIRE
   CASE-INCLUDE
   CASE-INDENTED-REQUIRE
   CASE-LITERAL
   CASE-BOTH-SHAPES
   CASE-MEMBER
   CASE-COMMENTED-REQUIRE
   CASE-COMMENTED-LITERAL
   CASE-REQUIRE-IN-STRING
   CASE-COMMENT-CANNOT-DECLARE
   CASE-MISSING-TARGET
   CASE-GLUED-SQUOTE
   CASE-NON-SOURCE-EXT
   CASE-SRC-NOT-SCANNED
   CASE-EXEMPT-SCOPE
   RUN-CLOSURE:REPORT-ON
   CLEANUP
   T-REPORT ;

MAIN

;package
