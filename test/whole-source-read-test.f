\ whole-source-read-test.f - a whole-source reader reads past its size sample.
\
\ check.f's readers (discovery, the lints, --all-errors, the verifier's stopped
\ file), the native source view, diag-origin and the language server's read of
\ a file its packets name each read a source whole through lib/source.f
\ READ-WHOLE-FILE or READ-WHOLE-SAMPLED: the file's size is only the first
\ room, and the read goes on to the end of the file. Each case is a child
\ engine (test/whole-source-read-child.f) whose FILE-SIZE appends blank lines
\ past every first room to the file it grows each time it answers for it, and
\ a require of grown.f to the subject the first time, so every reader meets
\ bytes past the size it sampled and grows to hold them, and each case shows
\ the readers acted on them. In one case the stat refuses with a code that is
\ not about the file, which diag-origin passes on unchanged; in the last the
\ server's file does not exist, which it counts as empty text.
\
\ Run: bin/hb --load test/whole-source-read-test.f

require lib/string.f
require lib/test.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/engine-candidate.f

package WHOLE-SOURCE-READ-TEST
private

\ What a case writes fits IO-CAP, but for diag-origin's standard output: it
\ prints its subject whole, after the child grew it by GROWTH bytes when
\ diag-origin took its size (test/whole-source-read-child.f BLANK-LEN).
$10000 constant IO-CAP
$11000 constant GROWTH
GROWTH IO-CAP + constant OUT-CAP
600000 constant TIMEOUT-MS

create ROOT FS-PATH-CAP allot   variable ROOT-U
create DIR FS-PATH-CAP allot    variable DIR-U
create SUBJECT FS-PATH-CAP allot   variable SUBJECT-U
create GROWN FS-PATH-CAP allot  variable GROWN-U
create GROW FS-PATH-CAP allot   variable GROW-U
create OUT OUT-CAP allot
create ERR IO-CAP allot

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;
: DIR$ ( -- ptr u8 n ) DIR DIR-U @ ;
: SUBJECT$ ( -- ptr u8 n ) SUBJECT SUBJECT-U @ ;
: GROWN$ ( -- ptr u8 n ) GROWN GROWN-U @ ;
: GROW$ ( -- ptr u8 n ) GROW GROW-U @ ;

: SETUP ( -- )
   CLEANUP-RESET
   s" habu-whole-source-read" HB-TMP-MKDIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY  u ROOT-U !
   ROOT$ CLEANUP-TREE+ ;

\ The case's own directory, holding the subject as it starts, which is the
\ file the case grows unless it names another; grown.f is named beside it and
\ written only by a case that needs it.
: CASE-DIR ( ptr u8 n -- ) {: mode:ptr modeu:n :}
   ROOT$ mode modeu DIR JOIN-PATH DIR-U !
   DIR$ MAKE-DIRS
   DIR$ s" grow.f" SUBJECT JOIN-PATH SUBJECT-U !
   DIR$ s" grown.f" GROWN JOIN-PATH GROWN-U !
   DIR$ s" grow.f" GROW JOIN-PATH GROW-U !
   SUBJECT$ S\" \\ grows each time its size is taken\n" WRITE-ALL ;

: RUN ( ptr u8 n -- n n n ) {: mode:ptr modeu:n :}   \ outu erru rc
   ENGINE-CANDIDATE:PATH$ {: engine:ptr engineu:n :}
   PROC-ARGV-ENV-RESET
   s" HB_TMP" >LEN ROOT$ >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   s" --load" >LEN PROC-ARGV+
   s" test/whole-source-read-child.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   mode modeu >LEN PROC-ARGV+
   SUBJECT$ >LEN PROC-ARGV+
   GROW$ >LEN PROC-ARGV+
   engine engineu >LEN s" " >LEN OUT OUT-CAP >LEN ERR IO-CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N
   {: outu:len erru:len rc:n :}
   outu LEN>N erru LEN>N rc ;

\ grown.f declares a cell its definition does not produce, so check.f reports
\ it only if every reader it ran read the subject past its sample, and the
\ check read the require that arrived after the first stat. --verify-only reads
\ the subject whole; a plain check also discovers its closure and runs the
\ lints; --all-errors lexes it again.
: CHECK-CASE ( ptr u8 n ptr u8 n -- )
   {: mode:ptr modeu:n label:ptr labelu:n :}
   mode modeu CASE-DIR
   GROWN$ S\" : GROWN ( -- n ) ;\n" WRITE-ALL
   mode modeu RUN {: outu:n erru:n rc:n :}
   label labelu T-LABEL
   rc 0 T=
   s" ... checks the require that arrived after the stat" T-LABEL
   OUT outu s" check: 70" CONTAINS? TTRUE
   s" ... and reports the mismatch in grown.f" T-LABEL
   ERR erru s\" \"word\":\"grown\"" CONTAINS? TTRUE ;

\ The subject requires dep.f, which defines a word twice and is the file that
\ grows: --verify-only discovers it, stops at the duplicate, and reads it again
\ to report the duplicate where it stands.
: DEP-CASE ( -- )
   s" dep" CASE-DIR
   DIR$ s" dep.f" GROW JOIN-PATH GROW-U !
   SUBJECT$ S\" s\" dep.f\" required\n" WRITE-ALL
   GROWN$ S\" : GROWN ( -- n ) 1 ;\n" WRITE-ALL
   GROW$ S\" : TWICE ( -- n ) 1 ;\n: TWICE ( -- n ) 2 ;\n" WRITE-ALL
   s" dep" RUN {: outu:n erru:n rc:n :}
   s" a verify-only dependency is read past each sampled size" T-LABEL
   rc 0 T=
   s" ... and its stopped duplicate is reported" T-LABEL
   OUT outu s" check: 70" CONTAINS? TTRUE
   ERR erru s" E-DUPLICATE-DEFINITION" CONTAINS? TTRUE ;

\ grown.f is absent, so the view refuses it by name only if it read the require
\ that arrived after the stat.
: VIEW-CASE ( -- )
   s" view" CASE-DIR
   s" view" RUN {: outu:n erru:n rc:n :}
   s" the source view reads its subject past the sampled size" T-LABEL
   rc 0 T=
   s" ... and follows the require that arrived after the stat" T-LABEL
   ERR erru s" cannot read " CONTAINS? TTRUE
   s" ... to grown.f, the file it refuses" T-LABEL
   ERR erru GROWN$ CONTAINS? TTRUE ;

: ORIGIN-CASE ( -- )
   s" origin" CASE-DIR
   s" origin" RUN {: outu:n erru:n rc:n :}
   s" diag-origin reads its subject past the sampled size" T-LABEL
   rc 0 T=
   s" ... and copies the line that arrived after the stat" T-LABEL
   OUT outu S\" s\" grown.f\" required" CONTAINS? TTRUE ;

\ A code the read meets that is not about the file, here E-MEM-MAP from the
\ child's stat, leaves diag-origin unchanged: it is no unreadable file.
: ORIGIN-REFUSED-CASE ( -- )
   s" origin-refused" CASE-DIR
   s" origin-refused" RUN {: outu:n erru:n rc:n :}
   s" diag-origin rethrows a code that is not about the file" T-LABEL
   rc 67 T=
   ERR erru s" uncaught throw code -3201" CONTAINS? TTRUE
   s" ... and does not call the file unreadable" T-LABEL
   ERR erru s" cannot read file" CONTAINS? TFALSE ;

\ The language server publishes a packet about dep.f, the file that grows,
\ besides the subject it has open: bytes 10 to 11, the colon starting dep.f's
\ second line, which the bytes past the sampled size leave where they were.
: LSP-CASE ( -- )
   s" lsp" CASE-DIR
   DIR$ s" dep.f" GROW JOIN-PATH GROW-U !
   GROW$ S\" \\ comment\n: BAD ( -- n ) ;\n" WRITE-ALL
   s" lsp" RUN {: outu:n erru:n rc:n :}
   s" the language server reads a file a packet names past its sampled size" T-LABEL
   rc 0 T=
   ERR erru s" not read" CONTAINS? TFALSE
   s" ... and ranges the packet on that file's second line" T-LABEL
   OUT outu S\" \"range\":{\"start\":{\"line\":1,\"character\":0},\"end\":{\"line\":1,\"character\":1}}" CONTAINS? TTRUE ;

\ The file the packet names does not exist, and it is the first the server
\ reads: it is named on standard error and its text counts as empty.
: LSP-MISSING-CASE ( -- )
   s" lsp-missing" CASE-DIR
   DIR$ s" missing.f" GROW JOIN-PATH GROW-U !
   s" lsp" RUN {: outu:n erru:n rc:n :}
   s" the language server counts a file it cannot read as empty" T-LABEL
   rc 0 T=
   ERR erru s" missing.f: not read: throw " CONTAINS? TTRUE
   s" ... and still publishes the packet, ranged at the start" T-LABEL
   OUT outu S\" \"range\":{\"start\":{\"line\":0,\"character\":0},\"end\":{\"line\":0,\"character\":0}}" CONTAINS? TTRUE ;

public

: WHOLE-SOURCE-READ-TEST-MAIN ( -- )
   T-RESET
   SETUP
   s" check" s" check.f --verify-only reads its subject past each sampled size" CHECK-CASE
   s" json" s" a plain check.f reads its subject past each sampled size" CHECK-CASE
   s" all" s" check.f --all-errors reads its subject past each sampled size" CHECK-CASE
   DEP-CASE
   VIEW-CASE
   ORIGIN-CASE
   ORIGIN-REFUSED-CASE
   LSP-CASE
   LSP-MISSING-CASE
   CLEANUP-RUN
   T-REPORT
   s" whole-source-read-test: ok" type cr ;

;package

WHOLE-SOURCE-READ-TEST:WHOLE-SOURCE-READ-TEST-MAIN
