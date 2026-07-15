\ typed-local-diff-lint-test.f - checked fixtures for typed-local diff lint.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/memory.f
\ lib/vector.f lib/fs.f lib/fs-mutate.f lib/process.f
\ tools/lint/text.f tools/lint/token.f tools/lint/lib.f
\ tools/lint/source-lex.f tools/lint/diff-frame-write.f tools/typed-local-diff-lint-core.f
\ tools/typed-local-diff-lint-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/vector.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f
require tools/lint/source-lex.f
require tools/lint/diff-frame-write.f
require tools/typed-local-diff-lint-core.f

package TLD-TEST
private

4096 constant BUF-CAP
$10000 constant LARGE-CAP
1100 constant LARGE-LINES

variable ROOT-U
variable GOOD-U
variable BAD-U
variable IGNORED-U
variable ALLOW-U
variable MD-U
variable LARGE-U
variable LARGE-SRC-U

create ROOT-BUF FS-PATH-CAP allot
create GOOD-BUF FS-PATH-CAP allot
create BAD-BUF FS-PATH-CAP allot
create IGNORED-BUF FS-PATH-CAP allot
create ALLOW-BUF FS-PATH-CAP allot
create MD-BUF FS-PATH-CAP allot
create LARGE-BUF FS-PATH-CAP allot
create OUT BUF-CAP allot
create LARGE-SRC LARGE-CAP allot
create FRAME LARGE-CAP 2 * allot
create LF-NAME 128 allot
variable LF-NAME-U

: FRAME-START ( -- )
   FRAME LARGE-CAP 2 *
   s" 0123456789012345678901234567890123456789"
   s" abcdef0123abcdef0123abcdef0123abcdef0123" DIFF-WRITE:START ;

: FRAME-END ( -- ptr u8 n )
   DIFF-WRITE:FINISH ;

: MODIFIED+ ( ptr u8 n ptr u8 n -- )
   {: path:ptr pathu:n raw:ptr rawu:n :}
   DIFF-STATUS:MODIFIED DIFF-FORM:TEXT true false
   true path pathu true path pathu raw rawu DIFF-WRITE:SECTION ;

: PURE-RENAME+ ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: old:ptr oldu:n new:ptr newu:n raw:ptr rawu:n :}
   DIFF-STATUS:RENAMED DIFF-FORM:PURE false false
   true old oldu true new newu raw rawu DIFF-WRITE:SECTION ;

: COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr lenp:ptr :}
   a dst u BYTE-COPY
   u lenp ! ;

: ROOT ( -- ptr u8 n )
   ROOT-BUF ROOT-U @ ;

: GOOD ( -- ptr u8 n )
   GOOD-BUF GOOD-U @ ;

: BAD ( -- ptr u8 n )
   BAD-BUF BAD-U @ ;

: IGNORED ( -- ptr u8 n )
   IGNORED-BUF IGNORED-U @ ;

: ALLOW ( -- ptr u8 n )
   ALLOW-BUF ALLOW-U @ ;

: MD ( -- ptr u8 n )
   MD-BUF MD-U @ ;

: LARGE ( -- ptr u8 n )
   LARGE-BUF LARGE-U @ ;

: LF ( -- )
   10 SB-APPEND-C ;

: DQ ( -- )
   34 SB-APPEND-C ;

: DIFF-HEAD ( ptr u8 n n -- ) {: path:ptr pathu:n count:n :}
   s" diff --git a/" SB-APPEND path pathu SB-APPEND
   s"  b/" SB-APPEND path pathu SB-APPEND LF
   s" index 1234567890..abcdef1234 100644" SB-APPEND LF
   s" --- a/" SB-APPEND path pathu SB-APPEND LF
   s" +++ b/" SB-APPEND path pathu SB-APPEND LF
   s" @@ -0,0 +1," SB-APPEND
   count 1 = if s" 1" else
      count 3 = if s" 3" else s" 4" then
   then SB-APPEND
   s"  @@" SB-APPEND LF ;

: GOOD$ ( -- ptr u8 n )
   FRAME-START
   SB-RESET
   s" lib/good.f" 4 DIFF-HEAD
   s" +: OK ( n -- n ) {: x:n :} x ;" SB-APPEND LF
   s" +: TWO ( n n -- n ) {:" SB-APPEND LF
   s" +   a:n b:n" SB-APPEND LF
   s" +:} a b + ;" SB-APPEND LF
   s" lib/good.f" SB$ MODIFIED+
   FRAME-END ;

: BAD$ ( -- ptr u8 n )
   FRAME-START
   SB-RESET
   s" lib/bad.f" 1 DIFF-HEAD
   s" +: BAD ( n -- n ) {: x :} x ;" SB-APPEND LF
   s" lib/bad.f" SB$ MODIFIED+
   FRAME-END ;

: IGNORED$ ( -- ptr u8 n )
   FRAME-START
   SB-RESET
   s" lib/ignored.f" 3 DIFF-HEAD
   s" +\\ {: x :} in a line comment" SB-APPEND LF
   s" +( {: x :} in a paren comment )" SB-APPEND LF
   s" +: STR ( -- ) s" SB-APPEND DQ
   s" {: x :}" SB-APPEND DQ
   s"  drop ;" SB-APPEND LF
   s" lib/ignored.f" SB$ MODIFIED+
   FRAME-END ;

: ALLOW$ ( -- ptr u8 n )
   FRAME-START
   SB-RESET
   s" lib/allow.f" 1 DIFF-HEAD
   s" +: KEEP-ROLE ( ptr u8 n -- ptr u8 ) {: a :} a \\ typed-local-lint: allow-bare-local" SB-APPEND LF
   s" lib/allow.f" SB$ MODIFIED+
   FRAME-END ;

: MD$ ( -- ptr u8 n )
   FRAME-START
   SB-RESET
   s" docs/note.md" 1 DIFF-HEAD
   s" +example {: x :} text" SB-APPEND LF
   s" docs/note.md" SB$ MODIFIED+
   FRAME-END ;

: LARGE-APPEND ( ptr u8 n -- )
   LARGE-SRC LARGE-CAP LARGE-SRC-U BUF-APPEND ;

: LARGE-LF ( -- )
   10 LARGE-SRC LARGE-CAP LARGE-SRC-U BUF-APPEND-C ;

: LARGE-DIFF-HEAD ( ptr u8 n -- ) {: path:ptr pathu:n :}
   s" diff --git a/" LARGE-APPEND path pathu LARGE-APPEND
   s"  b/" LARGE-APPEND path pathu LARGE-APPEND LARGE-LF
   s" index 1234567890..abcdef1234 100644" LARGE-APPEND LARGE-LF
   s" --- a/" LARGE-APPEND path pathu LARGE-APPEND LARGE-LF
   s" +++ b/" LARGE-APPEND path pathu LARGE-APPEND LARGE-LF
   s" @@ -0,0 +1,1100 @@" LARGE-APPEND LARGE-LF ;

: META$ ( -- ptr u8 n )
   FRAME-START
   SB-RESET
   s" diff --git a/lib/old.f b/docs/old.md" SB-APPEND LF
   s" similarity index 100%" SB-APPEND LF
   s" rename from lib/old.f" SB-APPEND LF
   s" rename to docs/old.md" SB-APPEND LF
   s" lib/old.f" s" docs/old.md" SB$ PURE-RENAME+
   SB-RESET
   s" docs/note.md" 1 DIFF-HEAD
   s" +example {: x :} text" SB-APPEND LF
   s" docs/note.md" SB$ MODIFIED+
   FRAME-END ;

: LF-NAME$ ( -- ptr u8 n )
   SB-RESET
   s" line" SB-APPEND LF
   s" file.f" SB-APPEND
   SB$ LF-NAME LF-NAME-U COPY!
   LF-NAME LF-NAME-U @ ;

: LF-PATH$ ( -- ptr u8 n )
   FRAME-START
   LF-NAME$ {: path:ptr pathu:n :}
   SB-RESET
   path pathu 1 DIFF-HEAD
   s" +: BAD ( n -- n ) {: x :} x ;" SB-APPEND LF
   path pathu SB$ MODIFIED+
   FRAME-END ;

: LARGE$ ( -- ptr u8 n )
   FRAME-START
   LARGE-SRC-U BUF-RESET
   s" lib/large.f" LARGE-DIFF-HEAD
   0 begin dup LARGE-LINES < while
      s" +: OK ( n -- n ) {: x:n :} x ;" LARGE-APPEND LARGE-LF
      1+
   repeat drop
   s" lib/large.f" LARGE-SRC LARGE-SRC-U BUF-LEN@ MODIFIED+
   FRAME-END ;

: EMPTY$ ( -- ptr u8 n )
   SB-RESET
   SB$ ;

: ROOT! ( ptr u8 n -- ) {: root:ptr rootu:n :}
   root rootu ROOT-BUF ROOT-U COPY! ;

: PREPARE ( -- )
   CLEANUP-RESET
   s" habu-typed-local-diff-lint" TMPDIR-MKDIR ROOT!
   ROOT CLEANUP-DIR+
   ROOT s" good.diff" GOOD-BUF JOIN-PATH GOOD-U !
   ROOT s" bad.diff" BAD-BUF JOIN-PATH BAD-U !
   ROOT s" ignored.diff" IGNORED-BUF JOIN-PATH IGNORED-U !
   ROOT s" allow.diff" ALLOW-BUF JOIN-PATH ALLOW-U !
   ROOT s" note.diff" MD-BUF JOIN-PATH MD-U !
   ROOT s" large.diff" LARGE-BUF JOIN-PATH LARGE-U !
   GOOD CLEANUP+
   BAD CLEANUP+
   IGNORED CLEANUP+
   ALLOW CLEANUP+
   MD CLEANUP+
   LARGE CLEANUP+
   GOOD GOOD$ WRITE-ALL
   BAD BAD$ WRITE-ALL
   IGNORED IGNORED$ WRITE-ALL
   ALLOW ALLOW$ WRITE-ALL
   MD MD$ WRITE-ALL
   LARGE LARGE$ WRITE-ALL ;

: CORE-SETUP ( -- )
   TYPED-LOCAL-DIFF:RESET
   OUT BUF-CAP LINT-OUT-BUFFER! ;

: CORE-FINISH ( -- n n n )
   [: TYPED-LOCAL-DIFF:FINISH ;] catch {: rc:n :}
   LINT-OUT$ nip LINT-OUT-BUFFER-OFF
   0 rc ;

: RUN-CORE ( ptr u8 n -- n n n )
   CORE-SETUP
   TYPED-LOCAL-DIFF:FILE
   CORE-FINISH ;

: ASSERT-CLEAN ( n n n -- ) {: outu:n erru:n code:n :}
   code 0 T=
   OUT outu EMPTY$ T$=
   erru 0 T= ;

: EXPECT-EXIT ( n n n n -- n n ) {: outu:n erru:n code:n expect:n :}
   code expect T=
   outu erru ;

: TEST-GOOD ( -- )
   GOOD RUN-CORE ASSERT-CLEAN ;

: TEST-LARGE ( -- )
   LARGE RUN-CORE ASSERT-CLEAN ;

: TEST-IGNORED ( -- )
   IGNORED RUN-CORE ASSERT-CLEAN ;

: TEST-ALLOW ( -- )
   ALLOW RUN-CORE ASSERT-CLEAN ;

: TEST-NON-FORTH ( -- )
   MD RUN-CORE ASSERT-CLEAN ;

: ASSERT-BAD ( n n -- ) {: outu:n erru:n :}
   erru 0 T=
   OUT outu s" E-UNTYPED-LOCAL" CONTAINS? TTRUE
   OUT outu s" x" CONTAINS? TTRUE ;

: TEST-BAD ( -- )
   BAD RUN-CORE 1 EXPECT-EXIT ASSERT-BAD ;

: RUN-META ( -- )
   CORE-SETUP
   META$ TYPED-LOCAL-DIFF:SOURCE
   TYPED-LOCAL-DIFF:FINISH ;

: TEST-META ( -- )
   [: RUN-META ;] catch {: rc:n :}
   LINT-OUT$ nip {: outu:n :}
   LINT-OUT-BUFFER-OFF
   rc 0 T=
   outu 0 T= ;

: RUN-LF-PATH ( -- )
   CORE-SETUP
   LF-PATH$ TYPED-LOCAL-DIFF:SOURCE
   TYPED-LOCAL-DIFF:FINISH ;

: TEST-LF-PATH ( -- )
   [: RUN-LF-PATH ;] catch {: rc:n :}
   LINT-OUT$ nip {: outu:n :}
   LINT-OUT-BUFFER-OFF
   rc 1 T=
   OUT outu s" E-UNTYPED-LOCAL" CONTAINS? TTRUE ;

: MAIN ( -- )
   T-RESET
   PREPARE
   TEST-GOOD
   TEST-LARGE
   TEST-IGNORED
   TEST-ALLOW
   TEST-NON-FORTH
   TEST-BAD
   TEST-META
   TEST-LF-PATH
   CLEANUP-RUN
   ROOT EXISTS? TFALSE
   T-REPORT
   s" typed-local-diff-lint-test: ok" type cr ;

MAIN

;package
