\ typed-local-diff-lint-test.f - checked fixtures for typed-local diff lint.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/memory.f
\ lib/vector.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f
\ tools/lint/text.f tools/lint/token.f tools/lint/lib.f
\ tools/lint/source-lex.f tools/warm-run.f tools/typed-local-diff-lint-core.f
\ tools/typed-local-diff-lint-test.f

4096 constant TLDT-BUF-CAP
$10000 constant TLDT-LARGE-CAP
1100 constant TLDT-LARGE-LINES
10000 constant TLDT-TIMEOUT-MS

variable TLDT-ROOT-U
variable TLDT-GOOD-U
variable TLDT-BAD-U
variable TLDT-IGNORED-U
variable TLDT-ALLOW-U
variable TLDT-MD-U
variable TLDT-LARGE-U
variable TLDT-LARGE-SRC-U

create TLDT-ROOT-BUF FS-PATH-CAP allot
create TLDT-GOOD-BUF FS-PATH-CAP allot
create TLDT-BAD-BUF FS-PATH-CAP allot
create TLDT-IGNORED-BUF FS-PATH-CAP allot
create TLDT-ALLOW-BUF FS-PATH-CAP allot
create TLDT-MD-BUF FS-PATH-CAP allot
create TLDT-LARGE-BUF FS-PATH-CAP allot
create TLDT-OUT TLDT-BUF-CAP allot
create TLDT-ERR TLDT-BUF-CAP allot
create TLDT-LARGE-SRC TLDT-LARGE-CAP allot

: TLDT-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr lenp:ptr :}
   a dst u BYTE-COPY
   u lenp ! ;

: TLDT-ROOT ( -- ptr u8 n )
   TLDT-ROOT-BUF TLDT-ROOT-U @ ;

: TLDT-GOOD ( -- ptr u8 n )
   TLDT-GOOD-BUF TLDT-GOOD-U @ ;

: TLDT-BAD ( -- ptr u8 n )
   TLDT-BAD-BUF TLDT-BAD-U @ ;

: TLDT-IGNORED ( -- ptr u8 n )
   TLDT-IGNORED-BUF TLDT-IGNORED-U @ ;

: TLDT-ALLOW ( -- ptr u8 n )
   TLDT-ALLOW-BUF TLDT-ALLOW-U @ ;

: TLDT-MD ( -- ptr u8 n )
   TLDT-MD-BUF TLDT-MD-U @ ;

: TLDT-LARGE ( -- ptr u8 n )
   TLDT-LARGE-BUF TLDT-LARGE-U @ ;

: TLDT-LF ( -- )
   10 SB-APPEND-C ;

: TLDT-DQ ( -- )
   34 SB-APPEND-C ;

: TLDT-DIFF-HEAD ( ptr u8 n -- ) {: path:ptr pathu:n :}
   s" diff --git a/" SB-APPEND path pathu SB-APPEND
   s"  b/" SB-APPEND path pathu SB-APPEND TLDT-LF
   s" +++ b/" SB-APPEND path pathu SB-APPEND TLDT-LF
   s" @@ -1,0 +1,3 @@" SB-APPEND TLDT-LF ;

: TLDT-GOOD$ ( -- ptr u8 n )
   SB-RESET
   s" lib/good.f" TLDT-DIFF-HEAD
   s" +: OK ( n -- n ) {: x:n :} x ;" SB-APPEND TLDT-LF
   s" +: TWO ( n n -- n ) {:" SB-APPEND TLDT-LF
   s" +   a:n b:n" SB-APPEND TLDT-LF
   s" +:} a b + ;" SB-APPEND TLDT-LF
   SB$ ;

: TLDT-BAD$ ( -- ptr u8 n )
   SB-RESET
   s" lib/bad.f" TLDT-DIFF-HEAD
   s" +: BAD ( n -- n ) {: x :} x ;" SB-APPEND TLDT-LF
   SB$ ;

: TLDT-IGNORED$ ( -- ptr u8 n )
   SB-RESET
   s" lib/ignored.f" TLDT-DIFF-HEAD
   s" +\\ {: x :} in a line comment" SB-APPEND TLDT-LF
   s" +( {: x :} in a paren comment )" SB-APPEND TLDT-LF
   s" +: STR ( -- ) s" SB-APPEND TLDT-DQ
   s" {: x :}" SB-APPEND TLDT-DQ
   s"  drop ;" SB-APPEND TLDT-LF
   SB$ ;

: TLDT-ALLOW$ ( -- ptr u8 n )
   SB-RESET
   s" lib/allow.f" TLDT-DIFF-HEAD
   s" +: KEEP-ROLE ( ptr u8 n -- ptr u8 ) {: a :} a \\ typed-local-lint: allow-bare-local" SB-APPEND TLDT-LF
   SB$ ;

: TLDT-MD$ ( -- ptr u8 n )
   SB-RESET
   s" docs/note.md" TLDT-DIFF-HEAD
   s" +example {: x :} text" SB-APPEND TLDT-LF
   SB$ ;

: TLDT-LARGE-APPEND ( ptr u8 n -- )
   TLDT-LARGE-SRC TLDT-LARGE-CAP TLDT-LARGE-SRC-U BUF-APPEND ;

: TLDT-LARGE-LF ( -- )
   10 TLDT-LARGE-SRC TLDT-LARGE-CAP TLDT-LARGE-SRC-U BUF-APPEND-C ;

: TLDT-LARGE-DIFF-HEAD ( ptr u8 n -- ) {: path:ptr pathu:n :}
   s" diff --git a/" TLDT-LARGE-APPEND path pathu TLDT-LARGE-APPEND
   s"  b/" TLDT-LARGE-APPEND path pathu TLDT-LARGE-APPEND TLDT-LARGE-LF
   s" +++ b/" TLDT-LARGE-APPEND path pathu TLDT-LARGE-APPEND TLDT-LARGE-LF
   s" @@ -1,0 +1,1100 @@" TLDT-LARGE-APPEND TLDT-LARGE-LF ;

: TLDT-LARGE$ ( -- ptr u8 n )
   TLDT-LARGE-SRC-U BUF-RESET
   s" lib/large.f" TLDT-LARGE-DIFF-HEAD
   0 begin dup TLDT-LARGE-LINES < while
      s" +: OK ( n -- n ) {: x:n :} x ;" TLDT-LARGE-APPEND TLDT-LARGE-LF
      1+
   repeat drop
   TLDT-LARGE-SRC TLDT-LARGE-SRC-U BUF-LEN@ ;

: TLDT-EMPTY$ ( -- ptr u8 n )
   SB-RESET
   SB$ ;

: TLDT-ROOT! ( ptr u8 n -- ) {: root:ptr rootu:n :}
   root rootu TLDT-ROOT-BUF TLDT-ROOT-U TLDT-COPY! ;

: TLDT-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-typed-local-diff-lint" TMPDIR-MKDIR TLDT-ROOT!
   TLDT-ROOT CLEANUP-DIR+
   TLDT-ROOT s" good.diff" TLDT-GOOD-BUF JOIN-PATH TLDT-GOOD-U !
   TLDT-ROOT s" bad.diff" TLDT-BAD-BUF JOIN-PATH TLDT-BAD-U !
   TLDT-ROOT s" ignored.diff" TLDT-IGNORED-BUF JOIN-PATH TLDT-IGNORED-U !
   TLDT-ROOT s" allow.diff" TLDT-ALLOW-BUF JOIN-PATH TLDT-ALLOW-U !
   TLDT-ROOT s" note.diff" TLDT-MD-BUF JOIN-PATH TLDT-MD-U !
   TLDT-ROOT s" large.diff" TLDT-LARGE-BUF JOIN-PATH TLDT-LARGE-U !
   TLDT-GOOD CLEANUP+
   TLDT-BAD CLEANUP+
   TLDT-IGNORED CLEANUP+
   TLDT-ALLOW CLEANUP+
   TLDT-MD CLEANUP+
   TLDT-LARGE CLEANUP+
   TLDT-GOOD TLDT-GOOD$ WRITE-ALL
   TLDT-BAD TLDT-BAD$ WRITE-ALL
   TLDT-IGNORED TLDT-IGNORED$ WRITE-ALL
   TLDT-ALLOW TLDT-ALLOW$ WRITE-ALL
   TLDT-MD TLDT-MD$ WRITE-ALL
   TLDT-LARGE TLDT-LARGE$ WRITE-ALL ;

: TLDT-ARG+ ( ptr u8 n -- )
   >LEN PROC-ARGV+ ;

: TLDT-ARGV-LOAD ( -- )
   PROC-ARGV-RESET
   s" tools/typed-local-diff-lint.f" WR-TOOLS-LOAD IF exit THEN
   s" --load" TLDT-ARG+
   s" lib/errors.f" TLDT-ARG+
   s" lib/string.f" TLDT-ARG+
   s" lib/memory.f" TLDT-ARG+
   s" lib/vector.f" TLDT-ARG+
   s" lib/fs.f" TLDT-ARG+
   s" tools/lint/text.f" TLDT-ARG+
   s" tools/lint/token.f" TLDT-ARG+
   s" tools/lint/lib.f" TLDT-ARG+
   s" tools/lint/source-lex.f" TLDT-ARG+
   s" tools/typed-local-diff-lint-core.f" TLDT-ARG+
   s" tools/argv.f" TLDT-ARG+
   s" tools/typed-local-diff-lint.f" TLDT-ARG+
   s" --" TLDT-ARG+ ;

: TLDT-CAPTURE>N ( len len n n -- n n n n ) {: outu:len erru:len kind:n code:n :}
   outu LEN>N erru LEN>N kind code ;

: TLDT-RUN ( ptr u8 n -- n n n n ) {: path:ptr pathu:n :}
   TLDT-ARGV-LOAD
   path pathu TLDT-ARG+
   WR-TOOLS$ >LEN TLDT-OUT TLDT-BUF-CAP >LEN TLDT-ERR TLDT-BUF-CAP >LEN
   TLDT-TIMEOUT-MS >MS RUN-ARGV-CAPTURE-OUTCOME
   TLDT-CAPTURE>N ;

: TLDT-CORE-SETUP ( -- )
   TYPED-LOCAL-DIFF-LINT-RESET
   TLDT-OUT TLDT-BUF-CAP LINT-OUT-BUFFER! ;

: TLDT-CORE-FINISH ( -- n n n n )
   [: TYPED-LOCAL-DIFF-LINT-FINISH ;] catch {: rc:n :}
   LINT-OUT$ nip LINT-OUT-BUFFER-OFF
   0 PROC-OUTCOME-EXIT rc ;

: TLDT-RUN-CORE ( ptr u8 n -- n n n n )
   TLDT-CORE-SETUP
   TYPED-LOCAL-DIFF-LINT-FILE
   TLDT-CORE-FINISH ;

: TLDT-ASSERT-CLEAN ( n n n n -- ) {: outu:n erru:n kind:n code:n :}
   kind PROC-OUTCOME-EXIT T=
   code 0 T=
   TLDT-OUT outu TLDT-EMPTY$ T$=
   TLDT-ERR erru TLDT-EMPTY$ T$= ;

: TLDT-EXPECT-EXIT ( n n n n n -- n n ) {: outu:n erru:n kind:n code:n expect:n :}
   kind PROC-OUTCOME-EXIT T=
   code expect T=
   outu erru ;

: TLDT-TEST-GOOD ( -- )
   TLDT-GOOD TLDT-RUN-CORE TLDT-ASSERT-CLEAN ;

: TLDT-TEST-LARGE ( -- )
   TLDT-LARGE TLDT-RUN-CORE TLDT-ASSERT-CLEAN ;

: TLDT-TEST-IGNORED ( -- )
   TLDT-IGNORED TLDT-RUN-CORE TLDT-ASSERT-CLEAN ;

: TLDT-TEST-ALLOW ( -- )
   TLDT-ALLOW TLDT-RUN-CORE TLDT-ASSERT-CLEAN ;

: TLDT-TEST-NON-FORTH ( -- )
   TLDT-MD TLDT-RUN-CORE TLDT-ASSERT-CLEAN ;

: TLDT-ASSERT-BAD ( n n -- ) {: outu:n erru:n :}
   erru 0 T=
   TLDT-OUT outu s" E-UNTYPED-LOCAL" CONTAINS? TTRUE
   TLDT-OUT outu s" x" CONTAINS? TTRUE ;

: TLDT-TEST-BAD ( -- )
   TLDT-BAD TLDT-RUN 1 TLDT-EXPECT-EXIT TLDT-ASSERT-BAD ;

: TLDT-MAIN ( -- )
   T-RESET
   TLDT-PREPARE
   TLDT-TEST-GOOD
   TLDT-TEST-LARGE
   TLDT-TEST-IGNORED
   TLDT-TEST-ALLOW
   TLDT-TEST-NON-FORTH
   TLDT-TEST-BAD
   CLEANUP-RUN
   TLDT-ROOT EXISTS? TFALSE
   T-REPORT
   s" typed-local-diff-lint-test: ok" type cr ;

TLDT-MAIN
