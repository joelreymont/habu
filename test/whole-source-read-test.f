\ whole-source-read-test.f - a whole-source reader (src/lib/source.f
\ READ-WHOLE-FILE, READ-WHOLE-SAMPLED) reads past its size sample: the size is
\ only the first room. The readers load once behind a FS:FILE-SIZE
\ (test/whole-source-read-child.f) that grows the file past every first room;
\ each case runs in a fork child (src/lib/test/subject.f).

require src/lib/string.f
require src/lib/test.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/test/subject.f
require test/whole-source-read-child.f

package WHOLE-SOURCE-READ-TEST
private

\ Cases fit IO-CAP except diag-origin's stdout: it prints the subject whole,
\ grown by GROWTH bytes when it took the size (whole-source-read-child.f
\ BLANK-LEN).
$10000 constant IO-CAP
$11000 constant GROWTH
GROWTH IO-CAP + constant OUT-CAP
600000 constant TIMEOUT-MS

create ROOT PATH-CAP allot   variable ROOT-U
create DIR PATH-CAP allot    variable DIR-U
create SUBJECT PATH-CAP allot   variable SUBJECT-U
create GROWN PATH-CAP allot  variable GROWN-U
create GROW PATH-CAP allot   variable GROW-U
create OUT OUT-CAP allot
create ERR IO-CAP allot

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;
: DIR$ ( -- ptr u8 n ) DIR DIR-U @ ;
: SUBJECT$ ( -- ptr u8 n ) SUBJECT SUBJECT-U @ ;
: GROWN$ ( -- ptr u8 n ) GROWN GROWN-U @ ;
: GROW$ ( -- ptr u8 n ) GROW GROW-U @ ;

: SETUP ( -- )
   FS:CLEANUP-RESET
   s" habu-whole-source-read" FS:MKTEMP-DIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY  u ROOT-U !
   ROOT$ FS:CLEANUP-TREE+ ;

\ The case grows the subject unless it names another file; grown.f sits beside
\ it, written only by a case that needs it.
: CASE-DIR ( ptr u8 n -- ) {: mode:ptr modeu:n :}
   ROOT$ mode modeu DIR FS:JOIN-PATH DIR-U !
   DIR$ FS:MAKE-DIRS
   DIR$ s" grow.f" SUBJECT FS:JOIN-PATH SUBJECT-U !
   DIR$ s" grown.f" GROWN FS:JOIN-PATH GROWN-U !
   DIR$ s" grow.f" GROW FS:JOIN-PATH GROW-U !
   SUBJECT$ S\" \\ grows each time its size is taken\n" FS:WRITE-ALL ;

: RUN ( ptr u8 n -- n n n ) {: mode:ptr modeu:n :}   \ outu erru rc
   mode modeu SUBJECT$ GROW$ WHOLE-SOURCE-READ-CHILD:CASE!
   s" WHOLE-SOURCE-READ-CHILD:MAIN"
   OUT OUT-CAP >LEN ERR IO-CAP >LEN TIMEOUT-MS >MS SUBJECT:RUN
   PROC-OUTCOME>RC RC>N
   {: outu:len erru:len rc:n :}
   outu LEN>N erru LEN>N rc ;

\ grown.f declares a cell its definition does not produce, so check.f reports it
\ only if every reader read past its sample, including the require that arrived
\ after the first stat.
: CHECK-CASE ( ptr u8 n ptr u8 n -- )
   {: mode:ptr modeu:n label:ptr labelu:n :}
   mode modeu CASE-DIR
   GROWN$ S\" : GROWN ( -- n ) ;\n" FS:WRITE-ALL
   mode modeu RUN {: outu:n erru:n rc:n :}
   label labelu T-LABEL
   rc 0 T=
   s" ... checks the require that arrived after the stat" T-LABEL
   OUT outu s" check: 70" CONTAINS? TTRUE
   s" ... and reports the mismatch in grown.f" T-LABEL
   ERR erru s\" \"word\":\"grown\"" CONTAINS? TTRUE ;

\ dep.f defines a word twice and grows: --verify-only stops at the duplicate and
\ reads dep.f again to report it where it stands.
: DEP-CASE ( -- )
   s" dep" CASE-DIR
   DIR$ s" dep.f" GROW FS:JOIN-PATH GROW-U !
   SUBJECT$ S\" s\" dep.f\" required\n" FS:WRITE-ALL
   GROWN$ S\" : GROWN ( -- n ) 1 ;\n" FS:WRITE-ALL
   GROW$ S\" : TWICE ( -- n ) 1 ;\n: TWICE ( -- n ) 2 ;\n" FS:WRITE-ALL
   s" dep" RUN {: outu:n erru:n rc:n :}
   s" a verify-only dependency is read past each sampled size" T-LABEL
   rc 0 T=
   s" ... and its stopped duplicate is reported" T-LABEL
   OUT outu s" check: 70" CONTAINS? TTRUE
   ERR erru s" E-DUPLICATE-DEFINITION" CONTAINS? TTRUE ;

: ORIGIN-CASE ( -- )
   s" origin" CASE-DIR
   s" origin" RUN {: outu:n erru:n rc:n :}
   s" diag-origin reads its subject past the sampled size" T-LABEL
   rc 0 T=
   s" ... and copies the line that arrived after the stat" T-LABEL
   OUT outu S\" s\" grown.f\" required" CONTAINS? TTRUE ;

\ A code the read meets that is not about the file (E-MEM-MAP from the child's
\ stat) leaves diag-origin unchanged: it is no unreadable file.
: ORIGIN-REFUSED-CASE ( -- )
   s" origin-refused" CASE-DIR
   s" origin-refused" RUN {: outu:n erru:n rc:n :}
   s" diag-origin rethrows a code that is not about the file" T-LABEL
   rc 67 T=
   ERR erru s" uncaught throw code -3201" CONTAINS? TTRUE
   s" ... and does not call the file unreadable" T-LABEL
   ERR erru s" cannot read file" CONTAINS? TFALSE ;

\ The server publishes a packet about dep.f, the growing file, besides its open
\ subject: bytes 10 to 11, the colon starting dep.f's second line, which the
\ bytes past the sampled size leave in place.
: LSP-CASE ( -- )
   s" lsp" CASE-DIR
   DIR$ s" dep.f" GROW FS:JOIN-PATH GROW-U !
   GROW$ S\" \\ comment\n: BAD ( -- n ) ;\n" FS:WRITE-ALL
   s" lsp" RUN {: outu:n erru:n rc:n :}
   s" the language server reads a file a packet names past its sampled size" T-LABEL
   rc 0 T=
   ERR erru s" not read" CONTAINS? TFALSE
   s" ... and ranges the packet on that file's second line" T-LABEL
   OUT outu S\" \"range\":{\"start\":{\"line\":1,\"character\":0},\"end\":{\"line\":1,\"character\":1}}" CONTAINS? TTRUE ;

\ The packet's file does not exist and is the server's first read: it is named
\ on standard error and its text counts as empty.
: LSP-MISSING-CASE ( -- )
   s" lsp-missing" CASE-DIR
   DIR$ s" missing.f" GROW FS:JOIN-PATH GROW-U !
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
   ORIGIN-CASE
   ORIGIN-REFUSED-CASE
   LSP-CASE
   LSP-MISSING-CASE
   FS:CLEANUP-RUN
   T-REPORT
   s" whole-source-read-test: ok" type cr ;

;package

WHOLE-SOURCE-READ-TEST:WHOLE-SOURCE-READ-TEST-MAIN
