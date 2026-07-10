\ trust-lint-test.f - checked fixtures for tools/trust-lint.f.
\ Run: bin/hb --load tools/date.f lib/errors.f lib/string.f lib/test.f
\ lib/memory.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f
\ tools/lint/text.f tools/lint/token.f tools/lint/lib.f tools/argv.f
\ tools/cli-run.f tools/trust-lint-core.f tools/trust-lint-test.f

require tools/date.f
require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f
require tools/argv.f
require tools/cli-run.f
require tools/trust-lint-core.f

8192 constant TLT-CAP
$10000 constant TLT-STR-CAP
$30000 constant TLT-FILE-CAP
10000 constant TLT-TIMEOUT-MS
10 constant TLT-LF
48 constant TLT-ZERO
520 constant TLT-GROW-ROWS

variable TLT-ROOT-U
variable TLT-CASE-U
variable TLT-SRC-U
variable TLT-LIB-U
variable TLT-MAN-U
variable TLT-SRC-TRUST-U
variable TLT-LIB-TRUST-U
variable TLT-LIB-DEF-U
variable TLT-BENCH-U
variable TLT-BENCH-TRUST-U
variable TLT-CORE-SRC-A
variable TLT-CORE-SRC-U
variable TLT-I

create TLT-ROOT-BUF FS-PATH-CAP allot
create TLT-CASE-BUF FS-PATH-CAP allot
create TLT-SRC-BUF FS-PATH-CAP allot
create TLT-LIB-BUF FS-PATH-CAP allot
create TLT-MAN-BUF FS-PATH-CAP allot
create TLT-SRC-TRUST-BUF FS-PATH-CAP allot
create TLT-LIB-TRUST-BUF FS-PATH-CAP allot
create TLT-LIB-DEF-BUF FS-PATH-CAP allot
create TLT-BENCH-BUF FS-PATH-CAP allot
create TLT-BENCH-TRUST-BUF FS-PATH-CAP allot
create TLT-OUT TLT-CAP allot
create TLT-ERR TLT-CAP allot
create TLT-STR-BUF TLT-STR-CAP allot
create TLT-FILE-BUF TLT-FILE-CAP allot
create TLT-LF-BUF 1 allot
TLT-LF TLT-LF-BUF c!

: TLT-CORE-SRC-A-FIELD ( -- ptr ptr u8 )
   TLT-CORE-SRC-A 0 ptr-field ;

: TLT-CORE-SRC-A@ ( -- ptr u8 )
   TLT-CORE-SRC-A-FIELD @ ;

: TLT-CORE-SRC-A! ( ptr u8 -- )
   TLT-CORE-SRC-A-FIELD ! ;

: TLT-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u dst:ptr lenp:ptr :}
   u FS-PATH-CAP > if E-FS-PATH throw then
   a dst u BYTE-COPY
   u lenp ! ;

: TLT-PATH! ( ptr u8 n ptr u8 n ptr u8 ptr n -- ) {: pa:ptr pu na:ptr nu dst:ptr lenp:ptr :}
   pa pu na nu dst JOIN-PATH lenp ! ;

: TLT-ROOT ( -- ptr u8 n )
   TLT-ROOT-BUF TLT-ROOT-U @ ;

: TLT-CASE ( -- ptr u8 n )
   TLT-CASE-BUF TLT-CASE-U @ ;

: TLT-SRC ( -- ptr u8 n )
   TLT-SRC-BUF TLT-SRC-U @ ;

: TLT-LIB ( -- ptr u8 n )
   TLT-LIB-BUF TLT-LIB-U @ ;

: TLT-MAN ( -- ptr u8 n )
   TLT-MAN-BUF TLT-MAN-U @ ;

: TLT-SRC-TRUST ( -- ptr u8 n )
   TLT-SRC-TRUST-BUF TLT-SRC-TRUST-U @ ;

: TLT-LIB-TRUST ( -- ptr u8 n )
   TLT-LIB-TRUST-BUF TLT-LIB-TRUST-U @ ;

: TLT-LIB-DEF ( -- ptr u8 n )
   TLT-LIB-DEF-BUF TLT-LIB-DEF-U @ ;

: TLT-BENCH ( -- ptr u8 n )
   TLT-BENCH-BUF TLT-BENCH-U @ ;

: TLT-BENCH-TRUST ( -- ptr u8 n )
   TLT-BENCH-TRUST-BUF TLT-BENCH-TRUST-U @ ;

: TLT-CORE-SRC! ( ptr u8 n -- ) {: a:ptr u:n :}
   a TLT-CORE-SRC-A!
   u TLT-CORE-SRC-U ! ;

: TLT-CORE-SRC$ ( -- ptr u8 n )
   TLT-CORE-SRC-A@ TLT-CORE-SRC-U @ ;

: TLT-LF+ ( -- )
   TLT-LF SB-APPEND-C ;

: TLT-DQ+ ( -- )
   34 SB-APPEND-C ;

: TLT-U+ ( n -- ) {: n :}
   n 0 < if E-FS-PATH throw then
   n 10 >= if n 10 / RECURSE then
   n 10 mod TLT-ZERO + SB-APPEND-C ;

: TLT-EMPTY$ ( -- ptr u8 n )
   SB-RESET
   SB$ ;

: TLT-HEADER$ ( -- ptr u8 n )
   SB-RESET
   s" | Word | Effect | Reason | Tests | Site | Last audited |" SB-APPEND TLT-LF+
   s" |------|--------|--------|-------|------|--------------|" SB-APPEND TLT-LF+
   SB$ ;

: TLT-BASE-ROW$ ( -- ptr u8 n )
   s" | foo | `n -- n` | fixture | `test/t-fixture.fs` | src/trust.f | 2026-06-13 |" ;

: TLT-BASE-SRC$ ( -- ptr u8 n )
   SB-RESET
   s" s" SB-APPEND TLT-DQ+ s"  foo" SB-APPEND TLT-DQ+
   s"  s" SB-APPEND TLT-DQ+ s"  n -- n" SB-APPEND TLT-DQ+
   s"  TRUST" SB-APPEND TLT-LF+
   SB$ ;

: TLT-LIB-TRUST$ ( -- ptr u8 n )
   SB-RESET
   s" s" SB-APPEND TLT-DQ+ s"  lib-foo" SB-APPEND TLT-DQ+
   s"  s" SB-APPEND TLT-DQ+ s"  -- n" SB-APPEND TLT-DQ+
   s"  TRUST" SB-APPEND TLT-LF+
   SB$ ;

: TLT-LIB-DEF$ ( -- ptr u8 n )
   SB-RESET
   s" TRUSTED: lib-trusted ( n -- n )" SB-APPEND TLT-LF+
   s"   dup ;" SB-APPEND TLT-LF+
   SB$ ;

: TLT-BENCH-TRUST$ ( -- ptr u8 n )
   SB-RESET
   s" TRUSTED: bench-trusted ( -- ptr u8 )" SB-APPEND TLT-LF+
   s"   here ;" SB-APPEND TLT-LF+
   SB$ ;

: TLT-GROW-ROW$ ( n -- ptr u8 n ) {: i:n :}
   SB-RESET
   s" | ROW-" SB-APPEND i TLT-U+
   s"  | `--` | fixture | `test/t-fixture.fs` | bench/row-" SB-APPEND
   i TLT-U+
   s" .f | 2026-06-13 |" SB-APPEND
   SB$ ;

: TLT-OK$ ( n n -- ptr u8 n ) {: sites rows :}
   SB-RESET
   s" trust-lint: " SB-APPEND sites TLT-U+
   s"  TRUST site(s), " SB-APPEND rows TLT-U+
   s"  manifest row(s), 0 finding(s)" SB-APPEND TLT-LF+
   SB$ ;

: TLT-CASE! ( ptr u8 n -- ) {: name:ptr nameu :}
   TLT-ROOT name nameu TLT-CASE-BUF TLT-CASE-U TLT-PATH!
   TLT-CASE MAKE-DIR
   TLT-CASE s" src" TLT-SRC-BUF TLT-SRC-U TLT-PATH!
   TLT-CASE s" lib" TLT-LIB-BUF TLT-LIB-U TLT-PATH!
   TLT-CASE s" TRUSTED.md" TLT-MAN-BUF TLT-MAN-U TLT-PATH!
   TLT-SRC s" trust.f" TLT-SRC-TRUST-BUF TLT-SRC-TRUST-U TLT-PATH!
   TLT-LIB s" trust.f" TLT-LIB-TRUST-BUF TLT-LIB-TRUST-U TLT-PATH!
   TLT-LIB s" trusted-def.f" TLT-LIB-DEF-BUF TLT-LIB-DEF-U TLT-PATH!
   TLT-CASE s" bench" TLT-BENCH-BUF TLT-BENCH-U TLT-PATH!
   TLT-BENCH s" trust.f" TLT-BENCH-TRUST-BUF TLT-BENCH-TRUST-U TLT-PATH! ;

: TLT-WRITE-MAN-HEADER ( -- )
   TLT-MAN TLT-HEADER$ WRITE-ALL ;

: TLT-APPEND-MAN ( ptr u8 n -- )
   TLT-MAN 2swap APPEND-FILE
   TLT-MAN TLT-LF-BUF 1 APPEND-FILE ;

: TLT-WRITE-MAN-ROW ( ptr u8 n -- )
   TLT-WRITE-MAN-HEADER
   TLT-APPEND-MAN ;

: TLT-MAKE-BASE ( ptr u8 n -- )
   TLT-CASE!
   TLT-SRC MAKE-DIR
   TLT-SRC-TRUST TLT-BASE-SRC$ WRITE-ALL
   TLT-BASE-ROW$ TLT-WRITE-MAN-ROW ;

: TLT-ADD-LIB-TRUST ( -- )
   TLT-LIB MAKE-DIRS
   TLT-LIB-TRUST TLT-LIB-TRUST$ WRITE-ALL ;

: TLT-ADD-LIB-SAME-TRUST ( -- )
   TLT-LIB MAKE-DIRS
   TLT-LIB-TRUST TLT-BASE-SRC$ WRITE-ALL ;

: TLT-ADD-LIB-DEF ( -- )
   TLT-LIB MAKE-DIRS
   TLT-LIB-DEF TLT-LIB-DEF$ WRITE-ALL ;

: TLT-ADD-GOOD-LIB-ROWS ( -- )
   s" | lib-foo | `-- n` | fixture | `test/t-lib-fixture.fs` | lib/trust.f | 2026-06-13 |" TLT-APPEND-MAN
   s" | lib-trusted | `n -- n` | fixture | `test/t-lib-fixture.fs` | lib/trusted-def.f | 2026-06-13 |" TLT-APPEND-MAN ;

: TLT-ADD-BENCH-TRUST ( -- )
   TLT-BENCH MAKE-DIRS
   TLT-BENCH-TRUST TLT-BENCH-TRUST$ WRITE-ALL ;

: TLT-ADD-GOOD-BENCH-ROW ( -- )
   s" | bench-trusted | `-- ptr u8` | fixture | `test/t-bench-fixture.fs` | bench/trust.f | 2026-06-13 |" TLT-APPEND-MAN ;

: TLT-APPEND-GROW-ROWS ( n -- ) {: rows:n :}
   0 TLT-I !
   begin TLT-I @ rows < while
      TLT-I @ TLT-GROW-ROW$ TLT-APPEND-MAN
      TLT-I @ 1 + TLT-I !
   repeat ;

: TLT-ARG+ ( ptr u8 n -- )
   >LEN PROC-ARGV+ ;

: TLT-ARGV ( ptr u8 n -- ) {: today:ptr todayu :}
   PROC-ARGV-RESET
   s" tools/trust-lint.f" CLI-TOOLS-LOAD if
      TLT-CASE  >LEN PROC-ARGV+
      today todayu  >LEN PROC-ARGV+
      exit
   then
   s" --load"  >LEN PROC-ARGV+
   s" tools/trust-lint.f"  >LEN PROC-ARGV+
   s" --"  >LEN PROC-ARGV+
   TLT-CASE  >LEN PROC-ARGV+
   today todayu  >LEN PROC-ARGV+ ;

: TLT-SOURCE-ARGV ( ptr u8 n ptr u8 n -- ) {: src:ptr srcu today:ptr todayu :}
   PROC-ARGV-RESET
   s" tools/trust-lint.f" CLI-TOOLS-LOAD if
      s" source-only"  >LEN PROC-ARGV+
      src srcu  >LEN PROC-ARGV+
      TLT-CASE  >LEN PROC-ARGV+
      today todayu  >LEN PROC-ARGV+
      exit
   then
   s" --load"  >LEN PROC-ARGV+
   s" tools/trust-lint.f"  >LEN PROC-ARGV+
   s" --"  >LEN PROC-ARGV+
   s" source-only"  >LEN PROC-ARGV+
   src srcu  >LEN PROC-ARGV+
   TLT-CASE  >LEN PROC-ARGV+
   today todayu  >LEN PROC-ARGV+ ;

: TLT-SOURCE-LIST-ARGV ( ptr u8 n -- ) {: today:ptr todayu :}
   PROC-ARGV-RESET
   s" tools/trust-lint.f" CLI-TOOLS-LOAD if
      s" source-list"  >LEN PROC-ARGV+
      TLT-CASE  >LEN PROC-ARGV+
      today todayu  >LEN PROC-ARGV+
      TLT-SRC-TRUST  >LEN PROC-ARGV+
      TLT-LIB-TRUST  >LEN PROC-ARGV+
      exit
   then
   s" --load"  >LEN PROC-ARGV+
   s" tools/trust-lint.f"  >LEN PROC-ARGV+
   s" --"  >LEN PROC-ARGV+
   s" source-list"  >LEN PROC-ARGV+
   TLT-CASE  >LEN PROC-ARGV+
   today todayu  >LEN PROC-ARGV+
   TLT-SRC-TRUST  >LEN PROC-ARGV+
   TLT-LIB-TRUST  >LEN PROC-ARGV+ ;

: TLT-CAPTURE>N ( len len rc -- n n n ) {: outu erru rc :}
   outu LEN>N erru LEN>N rc RC>N ;

: TLT-RUN-CLI ( ptr u8 n -- n n n )
   TLT-ARGV
   CLI-TOOLS$  >LEN TLT-OUT TLT-CAP >LEN TLT-ERR TLT-CAP >LEN
   TLT-TIMEOUT-MS >MS RUN-ARGV-CAPTURE TLT-CAPTURE>N ;

: TLT-TODAY>N ( ptr u8 n -- n )
   PARSE-YMD MATCH option
     none OF E-FS-PATH throw ENDOF
     some OF ENDOF
   ;MATCH ;

: TLT-CORE-SETUP ( ptr u8 n -- )
   TLT-OUT TLT-CAP LINT-OUT-BUFFER!
   TLT-STR-BUF TLT-STR-CAP TLT-FILE-BUF TLT-FILE-CAP TRUST-LINT-BUFFERS!
   TLT-CASE TRUST-LINT-ROOT!
   TLT-TODAY>N TRUST-LINT-TODAY! ;

: TLT-CORE-FINISH ( n -- n n n ) {: rc:n :}
   LINT-OUT$ nip LINT-OUT-BUFFER-OFF
   0 rc ;

: TLT-RUN ( ptr u8 n -- n n n )
   TLT-CORE-SETUP
   [: TRUST-LINT ;] catch TLT-CORE-FINISH ;

: TLT-RUN-DEFAULT ( -- n n n )
   s" 2026-06-16" TLT-RUN ;

: TLT-RUN-SOURCE ( ptr u8 n -- n n n )
   TLT-CORE-SRC!
   s" 2026-06-16" TLT-CORE-SETUP
   [: TLT-CORE-SRC$ TRUST-LINT-SOURCE-FILE ;] catch TLT-CORE-FINISH ;

: TLT-RUN-SOURCE-LIST ( -- n n n )
   s" 2026-06-16" TLT-CORE-SETUP
   [:
      TRUST-LINT-RESET
      TLT-SRC-TRUST TRUST-LINT-SOURCE+
      TLT-LIB-TRUST TRUST-LINT-SOURCE+
      TRUST-LINT-SOURCES-FINISH
   ;] catch TLT-CORE-FINISH ;

: TLT-EXPECT-OK ( n n -- ) {: sites rows :}
   TLT-RUN-DEFAULT 0 T=
   {: outu:n erru:n :}
   TLT-OUT outu sites rows TLT-OK$ T$=
   TLT-ERR erru TLT-EMPTY$ T$= ;

: TLT-EXPECT-SOURCE-OK ( ptr u8 n n n -- ) {: src:ptr srcu sites rows :}
   src srcu TLT-RUN-SOURCE 0 T=
   {: outu:n erru:n :}
   TLT-OUT outu sites rows TLT-OK$ T$=
   TLT-ERR erru TLT-EMPTY$ T$= ;

: TLT-EXPECT-SOURCE-LIST-OK ( n n -- ) {: sites rows :}
   TLT-RUN-SOURCE-LIST 0 T=
   {: outu:n erru:n :}
   TLT-OUT outu sites rows TLT-OK$ T$=
   TLT-ERR erru TLT-EMPTY$ T$= ;

: TLT-EXPECT-BAD-TODAY ( ptr u8 n ptr u8 n ptr u8 n -- ) {: code:ptr codeu:n today:ptr todayu:n needle:ptr needleu:n :}
   today todayu TLT-RUN 0 T<>
   {: outu:n erru:n :}
   erru 0 T=
   TLT-OUT outu code codeu CONTAINS? TTRUE
   needleu 0 > if TLT-OUT outu needle needleu CONTAINS? TTRUE then ;

: TLT-EXPECT-BAD-TODAY-CLI ( ptr u8 n ptr u8 n ptr u8 n -- ) {: code:ptr codeu:n today:ptr todayu:n needle:ptr needleu:n :}
   today todayu TLT-RUN-CLI 0 T<>
   {: outu:n erru:n :}
   erru 0 T=
   TLT-OUT outu code codeu CONTAINS? TTRUE
   needleu 0 > if TLT-OUT outu needle needleu CONTAINS? TTRUE then ;

: TLT-EXPECT-BAD ( ptr u8 n -- ) {: code:ptr codeu :}
   code codeu s" 2026-06-16" s" " TLT-EXPECT-BAD-TODAY ;

: TLT-EXPECT-BAD-CONTAINS ( ptr u8 n ptr u8 n -- ) {: code:ptr codeu needle:ptr needleu :}
   code codeu s" 2026-06-16" needle needleu TLT-EXPECT-BAD-TODAY ;

: TLT-TEST-GOOD ( -- )
   s" good" TLT-MAKE-BASE
   1 1 TLT-EXPECT-OK ;

: TLT-TEST-GOOD-LIB ( -- )
   s" good-lib" TLT-MAKE-BASE
   TLT-ADD-LIB-TRUST
   TLT-ADD-LIB-DEF
   TLT-ADD-GOOD-LIB-ROWS
   3 3 TLT-EXPECT-OK ;

: TLT-TEST-GOOD-NONROOT-ROW ( -- )
   s" good-nonroot-row" TLT-MAKE-BASE
   TLT-ADD-BENCH-TRUST
   TLT-ADD-GOOD-BENCH-ROW
   1 2 TLT-EXPECT-OK
   TLT-BENCH-TRUST 1 2 TLT-EXPECT-SOURCE-OK ;

: TLT-TEST-UNMANIFESTED-LIB ( -- )
   s" unmanifested-lib" TLT-MAKE-BASE
   TLT-ADD-LIB-TRUST
   s" UNMANIFESTED" s" lib/trust.f:1" TLT-EXPECT-BAD-CONTAINS ;

: TLT-TEST-UNMANIFESTED-TRUSTED ( -- )
   s" unmanifested-trusted-def" TLT-MAKE-BASE
   TLT-ADD-LIB-DEF
   s" UNMANIFESTED" s" lib/trusted-def.f:1" TLT-EXPECT-BAD-CONTAINS ;

: TLT-TEST-STALE-LIB-ROW ( -- )
   s" stale-lib-row" TLT-MAKE-BASE
   s" | lib-gone | `--` | fixture | `test/t-lib-fixture.fs` | lib/missing.f | 2026-06-13 |" TLT-APPEND-MAN
   s" STALE-ROW" s" lib/missing.f" TLT-EXPECT-BAD-CONTAINS ;

\ Same name TRUSTed in a second file with no row of its own: the file key
\ (foo, lib/trust.f) has no manifest row, so the lib site is UNMANIFESTED.
: TLT-TEST-SAME-NAME-UNMANIFESTED ( -- )
   s" same-name-unmanifested" TLT-MAKE-BASE
   TLT-LIB MAKE-DIRS
   TLT-LIB-TRUST TLT-BASE-SRC$ WRITE-ALL
   s" UNMANIFESTED" s" lib/trust.f:1" TLT-EXPECT-BAD-CONTAINS ;

: TLT-TEST-MULTISITE-TRUST ( -- )
   s" multisite-trust" TLT-MAKE-BASE
   TLT-LIB MAKE-DIRS
   TLT-LIB-TRUST TLT-BASE-SRC$ WRITE-ALL
   s" | foo | `n -- n` | fixture | `test/t-fixture.fs` | lib/trust.f | 2026-06-13 |" TLT-APPEND-MAN
   2 2 TLT-EXPECT-OK
   2 2 TLT-EXPECT-SOURCE-LIST-OK ;

: TLT-TEST-DUP-NAME-SITES ( -- )
   s" duplicate-name-sites" TLT-MAKE-BASE
   TLT-ADD-LIB-SAME-TRUST
   s" | foo | `n -- n` | fixture | `test/t-lib-fixture.fs` | lib/trust.f | 2026-06-13 |" TLT-APPEND-MAN
   2 2 TLT-EXPECT-OK
   2 2 TLT-EXPECT-SOURCE-LIST-OK ;

: TLT-TEST-DUP-NAME-MISSING-SITE ( -- )
   s" duplicate-name-missing-site" TLT-CASE!
   TLT-SRC MAKE-DIR
   TLT-SRC-TRUST TLT-BASE-SRC$ WRITE-ALL
   TLT-WRITE-MAN-HEADER
   s" | foo | `n -- n` | fixture | `test/t-fixture.fs` | src/other.f | 2026-06-13 |" TLT-APPEND-MAN
   s" | foo | `n -- n` | fixture | `test/t-lib-fixture.fs` | lib/other.f | 2026-06-13 |" TLT-APPEND-MAN
   s" UNMANIFESTED" s" src/trust.f:1" TLT-EXPECT-BAD-CONTAINS ;

: TLT-TEST-DUP-TRUST ( -- )
   s" duplicate-trust" TLT-MAKE-BASE
   TLT-SRC-TRUST TLT-BASE-SRC$ APPEND-FILE
   s" DUPLICATE-TRUST" TLT-EXPECT-BAD ;

: TLT-TEST-EFFECT-DRIFT ( -- )
   s" effect-drift" TLT-MAKE-BASE
   s" | foo | `n --` | fixture | `test/t-fixture.fs` | src/trust.f | 2026-06-13 |" TLT-WRITE-MAN-ROW
   s" EFFECT-DRIFT" TLT-EXPECT-BAD ;

\ A row keyed on the wrong file matches no scanned site: the source site is
\ UNMANIFESTED and the mislocated row is STALE-ROW - the file-key replacement
\ for the deleted SITE-DRIFT class.
: TLT-TEST-WRONG-FILE-ROW ( -- )
   s" wrong-file-row" TLT-MAKE-BASE
   s" | foo | `n -- n` | fixture | `test/t-fixture.fs` | src/other.f | 2026-06-13 |" TLT-WRITE-MAN-ROW
   s" UNMANIFESTED" s" src/trust.f:1" TLT-EXPECT-BAD-CONTAINS ;

\ The line suffix is informational only: a row pinned to the wrong line still
\ matches by file key, so a 1-line (or 100-line) shift stays green with zero
\ row edits. This is the core anti-toil guarantee of the file-key manifest.
: TLT-TEST-LINE-INFORMATIONAL ( -- )
   s" line-informational" TLT-MAKE-BASE
   s" | foo | `n -- n` | fixture | `test/t-fixture.fs` | src/trust.f:99 | 2026-06-13 |" TLT-WRITE-MAN-ROW
   1 1 TLT-EXPECT-OK ;

: TLT-TEST-UNTESTED ( -- )
   s" untested" TLT-MAKE-BASE
   s" | foo | `n -- n` | fixture | | src/trust.f | 2026-06-13 |" TLT-WRITE-MAN-ROW
   s" UNTESTED" TLT-EXPECT-BAD ;

: TLT-TEST-BAD-AUDIT ( -- )
   s" bad-audit" TLT-MAKE-BASE
   s" | foo | `n -- n` | fixture | `test/t-fixture.fs` | src/trust.f | nope |" TLT-WRITE-MAN-ROW
   s" BAD-AUDIT-DATE" TLT-EXPECT-BAD ;

: TLT-TEST-BAD-CALENDAR-AUDIT ( -- )
   s" bad-calendar-audit" TLT-MAKE-BASE
   s" | foo | `n -- n` | fixture | `test/t-fixture.fs` | src/trust.f | 2026-02-29 |" TLT-WRITE-MAN-ROW
   s" BAD-AUDIT-DATE" TLT-EXPECT-BAD ;

: TLT-TEST-BAD-TODAY ( -- )
   s" bad-today" TLT-MAKE-BASE
   s" BAD-TODAY" s" 2026-02-29" s" " TLT-EXPECT-BAD-TODAY-CLI ;

\ Genuinely-future date: more than one calendar day ahead of UTC today
\ (2026-06-16) exceeds the timezone tolerance and must be rejected.
: TLT-TEST-FUTURE-AUDIT ( -- )
   s" future-audit" TLT-MAKE-BASE
   s" | foo | `n -- n` | fixture | `test/t-fixture.fs` | src/trust.f | 2026-06-18 |" TLT-WRITE-MAN-ROW
   s" FUTURE-AUDIT" TLT-EXPECT-BAD ;

\ Timezone-ahead date: a row authored 'today' in a UTC+ timezone reads one
\ calendar day ahead of UTC today (2026-06-16 -> 2026-06-17); it is legitimate and
\ must NOT be rejected as future.
: TLT-TEST-TZ-AHEAD-AUDIT ( -- )
   s" tz-ahead-audit" TLT-MAKE-BASE
   s" | foo | `n -- n` | fixture | `test/t-fixture.fs` | src/trust.f | 2026-06-17 |" TLT-WRITE-MAN-ROW
   1 1 TLT-EXPECT-OK ;

: TLT-TEST-STALE-AUDIT ( -- )
   s" stale-audit" TLT-MAKE-BASE
   s" STALE-AUDIT" s" 2026-10-01" s" " TLT-EXPECT-BAD-TODAY ;

: TLT-TEST-STALE-ROW ( -- )
   s" stale-row" TLT-MAKE-BASE
   s" | bar | `--` | fixture | `test/t-fixture.fs` | src/trust.f | 2026-06-13 |" TLT-APPEND-MAN
   s" STALE-ROW" TLT-EXPECT-BAD ;

: TLT-TEST-DUP-ROW ( -- )
   s" duplicate-row" TLT-MAKE-BASE
   TLT-BASE-ROW$ TLT-APPEND-MAN
   s" DUPLICATE-ROW" TLT-EXPECT-BAD ;

: TLT-TEST-GROW-MANIFEST ( -- )
   s" grow-manifest" TLT-CASE!
   TLT-WRITE-MAN-HEADER
   TLT-GROW-ROWS TLT-APPEND-GROW-ROWS
   0 TLT-GROW-ROWS TLT-EXPECT-OK ;

: TLT-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-trust-lint" TMPDIR-MKDIR {: a:ptr u :}
   a u TLT-ROOT-BUF TLT-ROOT-U TLT-COPY!
   TLT-ROOT CLEANUP-TREE+ ;

: TLT-MAIN ( -- )
   T-RESET
   TLT-PREPARE
   TLT-TEST-GOOD
   TLT-TEST-GOOD-LIB
   TLT-TEST-GOOD-NONROOT-ROW
   TLT-TEST-UNMANIFESTED-LIB
   TLT-TEST-UNMANIFESTED-TRUSTED
   TLT-TEST-STALE-LIB-ROW
   TLT-TEST-SAME-NAME-UNMANIFESTED
   TLT-TEST-MULTISITE-TRUST
   TLT-TEST-DUP-NAME-SITES
   TLT-TEST-DUP-NAME-MISSING-SITE
   TLT-TEST-DUP-TRUST
   TLT-TEST-EFFECT-DRIFT
   TLT-TEST-WRONG-FILE-ROW
   TLT-TEST-LINE-INFORMATIONAL
   TLT-TEST-UNTESTED
   TLT-TEST-BAD-AUDIT
   TLT-TEST-BAD-CALENDAR-AUDIT
   TLT-TEST-BAD-TODAY
   TLT-TEST-FUTURE-AUDIT
   TLT-TEST-TZ-AHEAD-AUDIT
   TLT-TEST-STALE-AUDIT
   TLT-TEST-STALE-ROW
   TLT-TEST-DUP-ROW
   TLT-TEST-GROW-MANIFEST
   CLEANUP-RUN
   TLT-ROOT EXISTS? TFALSE
   T-REPORT
   s" trust-lint-test: ok" type cr ;

TLT-MAIN
