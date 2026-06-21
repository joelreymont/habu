\ trust-lint-test.f - checked fixtures for tools/trust-lint.f.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f tools/trust-lint-test.f

8192 constant TLT-CAP
10 constant TLT-LF
48 constant TLT-ZERO

variable TLT-ROOT-U
variable TLT-CASE-U
variable TLT-SRC-U
variable TLT-LIB-U
variable TLT-MAN-U
variable TLT-SRC-TRUST-U
variable TLT-LIB-TRUST-U
variable TLT-LIB-DEF-U

create TLT-ROOT-BUF FS-PATH-CAP allot
create TLT-CASE-BUF FS-PATH-CAP allot
create TLT-SRC-BUF FS-PATH-CAP allot
create TLT-LIB-BUF FS-PATH-CAP allot
create TLT-MAN-BUF FS-PATH-CAP allot
create TLT-SRC-TRUST-BUF FS-PATH-CAP allot
create TLT-LIB-TRUST-BUF FS-PATH-CAP allot
create TLT-LIB-DEF-BUF FS-PATH-CAP allot
create TLT-OUT TLT-CAP allot
create TLT-ERR TLT-CAP allot
create TLT-LF-BUF 1 allot
TLT-LF TLT-LF-BUF c!

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
   s" | foo | `n -- n` | fixture | `test/t-fixture.fs` | src/trust.f:1 | 2026-06-13 |" ;

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
   TLT-LIB s" trusted-def.f" TLT-LIB-DEF-BUF TLT-LIB-DEF-U TLT-PATH! ;

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

: TLT-ADD-LIB-DEF ( -- )
   TLT-LIB MAKE-DIRS
   TLT-LIB-DEF TLT-LIB-DEF$ WRITE-ALL ;

: TLT-ADD-GOOD-LIB-ROWS ( -- )
   s" | lib-foo | `-- n` | fixture | `test/t-lib-fixture.fs` | lib/trust.f:1 | 2026-06-13 |" TLT-APPEND-MAN
   s" | lib-trusted | `n -- n` | fixture | `test/t-lib-fixture.fs` | lib/trusted-def.f:1 | 2026-06-13 |" TLT-APPEND-MAN ;

: TLT-ARGV ( ptr u8 n -- ) {: today:ptr todayu :}
   PROC-ARGV-RESET
   s" --load" PROC-ARGV+
   s" tools/date.f" PROC-ARGV+
   s" tools/lint/lib.f" PROC-ARGV+
   s" tools/fs.f" PROC-ARGV+
   s" tools/argv.f" PROC-ARGV+
   s" tools/trust-lint.f" PROC-ARGV+
   s" --" PROC-ARGV+
   TLT-CASE PROC-ARGV+
   today todayu PROC-ARGV+ ;

: TLT-RUN ( ptr u8 n -- n n n )
   TLT-ARGV
   s" bin/hb" TLT-OUT TLT-CAP TLT-ERR TLT-CAP 1000 RUN-ARGV-CAPTURE ;

: TLT-RUN-DEFAULT ( -- n n n )
   s" 2026-06-16" TLT-RUN ;

: TLT-EXPECT-OK ( n n -- ) {: sites rows :}
   TLT-RUN-DEFAULT 0 T=
   {: outu erru :}
   TLT-OUT outu sites rows TLT-OK$ T$=
   TLT-ERR erru TLT-EMPTY$ T$= ;

: TLT-EXPECT-BAD-TODAY ( ptr u8 n ptr u8 n ptr u8 n -- ) {: code:ptr codeu today:ptr todayu needle:ptr needleu :}
   today todayu TLT-RUN 0 T<>
   {: outu erru :}
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
   s" | lib-gone | `--` | fixture | `test/t-lib-fixture.fs` | lib/missing.f:1 | 2026-06-13 |" TLT-APPEND-MAN
   s" STALE-ROW" s" lib/missing.f:1" TLT-EXPECT-BAD-CONTAINS ;

: TLT-TEST-DUP-SRC-LIB ( -- )
   s" duplicate-src-lib" TLT-MAKE-BASE
   TLT-LIB MAKE-DIRS
   TLT-LIB-TRUST TLT-BASE-SRC$ WRITE-ALL
   s" DUPLICATE-TRUST" s" lib/trust.f:1" TLT-EXPECT-BAD-CONTAINS ;

: TLT-TEST-DUP-TRUST ( -- )
   s" duplicate-trust" TLT-MAKE-BASE
   TLT-SRC-TRUST TLT-BASE-SRC$ APPEND-FILE
   s" DUPLICATE-TRUST" TLT-EXPECT-BAD ;

: TLT-TEST-EFFECT-DRIFT ( -- )
   s" effect-drift" TLT-MAKE-BASE
   s" | foo | `n --` | fixture | `test/t-fixture.fs` | src/trust.f:1 | 2026-06-13 |" TLT-WRITE-MAN-ROW
   s" EFFECT-DRIFT" TLT-EXPECT-BAD ;

: TLT-TEST-SITE-DRIFT-PATH ( -- )
   s" site-drift-path" TLT-MAKE-BASE
   s" | foo | `n -- n` | fixture | `test/t-fixture.fs` | src/other.f:1 | 2026-06-13 |" TLT-WRITE-MAN-ROW
   s" SITE-DRIFT" s" src/other.f:1" TLT-EXPECT-BAD-CONTAINS ;

: TLT-TEST-SITE-DRIFT-LINE ( -- )
   s" site-drift-line" TLT-MAKE-BASE
   s" | foo | `n -- n` | fixture | `test/t-fixture.fs` | src/trust.f:2 | 2026-06-13 |" TLT-WRITE-MAN-ROW
   s" SITE-DRIFT" s" src/trust.f:2" TLT-EXPECT-BAD-CONTAINS ;

: TLT-TEST-UNTESTED ( -- )
   s" untested" TLT-MAKE-BASE
   s" | foo | `n -- n` | fixture | | src/trust.f:1 | 2026-06-13 |" TLT-WRITE-MAN-ROW
   s" UNTESTED" TLT-EXPECT-BAD ;

: TLT-TEST-BAD-AUDIT ( -- )
   s" bad-audit" TLT-MAKE-BASE
   s" | foo | `n -- n` | fixture | `test/t-fixture.fs` | src/trust.f:1 | nope |" TLT-WRITE-MAN-ROW
   s" BAD-AUDIT-DATE" TLT-EXPECT-BAD ;

: TLT-TEST-BAD-CALENDAR-AUDIT ( -- )
   s" bad-calendar-audit" TLT-MAKE-BASE
   s" | foo | `n -- n` | fixture | `test/t-fixture.fs` | src/trust.f:1 | 2026-02-29 |" TLT-WRITE-MAN-ROW
   s" BAD-AUDIT-DATE" TLT-EXPECT-BAD ;

: TLT-TEST-BAD-TODAY ( -- )
   s" bad-today" TLT-MAKE-BASE
   s" BAD-TODAY" s" 2026-02-29" s" " TLT-EXPECT-BAD-TODAY ;

: TLT-TEST-FUTURE-AUDIT ( -- )
   s" future-audit" TLT-MAKE-BASE
   s" | foo | `n -- n` | fixture | `test/t-fixture.fs` | src/trust.f:1 | 2026-06-17 |" TLT-WRITE-MAN-ROW
   s" FUTURE-AUDIT" TLT-EXPECT-BAD ;

: TLT-TEST-STALE-AUDIT ( -- )
   s" stale-audit" TLT-MAKE-BASE
   s" STALE-AUDIT" s" 2026-10-01" s" " TLT-EXPECT-BAD-TODAY ;

: TLT-TEST-STALE-ROW ( -- )
   s" stale-row" TLT-MAKE-BASE
   s" | bar | `--` | fixture | `test/t-fixture.fs` | src/trust.f:2 | 2026-06-13 |" TLT-APPEND-MAN
   s" STALE-ROW" TLT-EXPECT-BAD ;

: TLT-TEST-DUP-ROW ( -- )
   s" duplicate-row" TLT-MAKE-BASE
   TLT-BASE-ROW$ TLT-APPEND-MAN
   s" DUPLICATE-ROW" TLT-EXPECT-BAD ;

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
   TLT-TEST-UNMANIFESTED-LIB
   TLT-TEST-UNMANIFESTED-TRUSTED
   TLT-TEST-STALE-LIB-ROW
   TLT-TEST-DUP-SRC-LIB
   TLT-TEST-DUP-TRUST
   TLT-TEST-EFFECT-DRIFT
   TLT-TEST-SITE-DRIFT-PATH
   TLT-TEST-SITE-DRIFT-LINE
   TLT-TEST-UNTESTED
   TLT-TEST-BAD-AUDIT
   TLT-TEST-BAD-CALENDAR-AUDIT
   TLT-TEST-BAD-TODAY
   TLT-TEST-FUTURE-AUDIT
   TLT-TEST-STALE-AUDIT
   TLT-TEST-STALE-ROW
   TLT-TEST-DUP-ROW
   CLEANUP-RUN
   TLT-ROOT EXISTS? TFALSE
   T-REPORT
   s" trust-lint-test: ok" type cr ;

TLT-MAIN
