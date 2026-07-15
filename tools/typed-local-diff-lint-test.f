\ typed-local-diff-lint-test.f - checked fixtures for typed-local diff lint.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/memory.f
\ lib/vector.f lib/fs.f lib/fs-mutate.f lib/process.f
\ tools/lint/text.f tools/lint/token.f tools/lint/lib.f
\ tools/lint/source-lex.f tools/typed-local-diff-lint-core.f
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
require tools/typed-local-diff-lint-core.f

\ typed STR:BUF-LEN@ boundary: read the large-source buffer length as a byte-len
\ role, then project it back to the raw n the ( -- ptr u8 n ) accessor returns.
package CAD-NUM
public
: TLDT-BL>RAW ( CAD-NUM:byte-len -- n ) BYTE-LEN>N ;
;package

package TYPED-LOCAL-DIFF-TEST
private

4096 constant TLDT-BUF-CAP
$10000 constant TLDT-LARGE-CAP
1100 constant TLDT-LARGE-LINES

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
   s" index 1234567..abcdef0 100644" SB-APPEND TLDT-LF
   s" --- a/" SB-APPEND path pathu SB-APPEND TLDT-LF
   s" +++ b/" SB-APPEND path pathu SB-APPEND TLDT-LF
;

: TLDT-GOOD$ ( -- ptr u8 n )
   SB-RESET
   s" lib/good.f" TLDT-DIFF-HEAD
   s" @@ -0,0 +1,4 @@" SB-APPEND TLDT-LF
   s" +: OK ( n -- n ) {: x:n :} x ;" SB-APPEND TLDT-LF
   s" +: TWO ( n n -- n ) {:" SB-APPEND TLDT-LF
   s" +   a:n b:n" SB-APPEND TLDT-LF
   s" +:} a b + ;" SB-APPEND TLDT-LF
   SB$ ;

: TLDT-BAD$ ( -- ptr u8 n )
   SB-RESET
   s" lib/bad.f" TLDT-DIFF-HEAD
   s" @@ -0,0 +1 @@" SB-APPEND TLDT-LF
   s" +: BAD ( n -- n ) {: x :} x ;" SB-APPEND TLDT-LF
   SB$ ;

: TLDT-IGNORED$ ( -- ptr u8 n )
   SB-RESET
   s" lib/ignored.f" TLDT-DIFF-HEAD
   s" @@ -0,0 +1,3 @@" SB-APPEND TLDT-LF
   s" +\\ {: x :} in a line comment" SB-APPEND TLDT-LF
   s" +( {: x :} in a paren comment )" SB-APPEND TLDT-LF
   s" +: STR ( -- ) s" SB-APPEND TLDT-DQ
   s" {: x :}" SB-APPEND TLDT-DQ
   s"  drop ;" SB-APPEND TLDT-LF
   SB$ ;

: TLDT-ALLOW$ ( -- ptr u8 n )
   SB-RESET
   s" lib/allow.f" TLDT-DIFF-HEAD
   s" @@ -0,0 +1 @@" SB-APPEND TLDT-LF
   s" +: KEEP-ROLE ( ptr u8 n -- ptr u8 ) {: a :} a \\ typed-local-lint: allow-bare-local" SB-APPEND TLDT-LF
   SB$ ;

: TLDT-MD$ ( -- ptr u8 n )
   SB-RESET
   s" docs/note.md" TLDT-DIFF-HEAD
   s" @@ -0,0 +1 @@" SB-APPEND TLDT-LF
   s" +example {: x :} text" SB-APPEND TLDT-LF
   SB$ ;

: TLDT-LARGE-APPEND ( ptr u8 n -- )
   STR:LENGTH TLDT-LARGE-SRC TLDT-LARGE-CAP STR:LENGTH TLDT-LARGE-SRC-U STR:BUF-APPEND ;

: TLDT-LARGE-LF ( -- )
   10 TLDT-LARGE-SRC TLDT-LARGE-CAP STR:LENGTH TLDT-LARGE-SRC-U STR:BUF-APPEND-C ;

: TLDT-LARGE-DIFF-HEAD ( ptr u8 n -- ) {: path:ptr pathu:n :}
   s" diff --git a/" TLDT-LARGE-APPEND path pathu TLDT-LARGE-APPEND
   s"  b/" TLDT-LARGE-APPEND path pathu TLDT-LARGE-APPEND TLDT-LARGE-LF
   s" index 1234567..abcdef0 100644" TLDT-LARGE-APPEND TLDT-LARGE-LF
   s" --- a/" TLDT-LARGE-APPEND path pathu TLDT-LARGE-APPEND TLDT-LARGE-LF
   s" +++ b/" TLDT-LARGE-APPEND path pathu TLDT-LARGE-APPEND TLDT-LARGE-LF
   s" @@ -0,0 +1,1100 @@" TLDT-LARGE-APPEND TLDT-LARGE-LF ;

: TLDT-LARGE$ ( -- ptr u8 n )
   TLDT-LARGE-SRC-U STR:BUF-RESET
   s" lib/large.f" TLDT-LARGE-DIFF-HEAD
   0 begin dup TLDT-LARGE-LINES < while
      s" +: OK ( n -- n ) {: x:n :} x ;" TLDT-LARGE-APPEND TLDT-LARGE-LF
      1+
   repeat drop
   TLDT-LARGE-SRC TLDT-LARGE-SRC-U STR:BUF-LEN@ CAD-NUM:TLDT-BL>RAW ;

: TLDT-EMPTY$ ( -- ptr u8 n )
   SB-RESET
   SB$ ;

: TLDT-BAD-OUT$ ( -- ptr u8 n )
   SB-RESET
   s" E-UNTYPED-LOCAL lib/bad.f:1:21: `x` needs :type inside {: :}" SB-APPEND
   TLDT-LF
   SB$ ;

: TLDT-CRLF$ ( -- ptr u8 n )
   SB-RESET
   s" diff --git a/a.f b/a.f" SB-APPEND
   13 SB-APPEND-C TLDT-LF
   SB$ ;

: TLDT-TRUNC$ ( -- ptr u8 n )
   SB-RESET
   s" diff --git a/a.f b/a.f" SB-APPEND TLDT-LF
   s" old mode 100644" SB-APPEND TLDT-LF
   s" new mode 100755" SB-APPEND
   SB$ ;

: TLDT-SPOOF$ ( -- ptr u8 n )
   SB-RESET
   s" diff --git a/a.f b/a.f" SB-APPEND TLDT-LF
   s" +++ b/spoof.f" SB-APPEND TLDT-LF
   SB$ ;

: TLDT-CONTROL$ ( -- ptr u8 n )
   SB-RESET
   s" diff --git a/tab" SB-APPEND 9 SB-APPEND-C
   s" x.f b/tab" SB-APPEND 9 SB-APPEND-C
   s" x.f" SB-APPEND TLDT-LF
   SB$ ;

: TLDT-CRLF ( -- )
   TYPED-LOCAL-DIFF:RESET
   TLDT-CRLF$ TYPED-LOCAL-DIFF:SOURCE ;

: TLDT-TRUNC ( -- )
   TYPED-LOCAL-DIFF:RESET
   TLDT-TRUNC$ TYPED-LOCAL-DIFF:SOURCE ;

: TLDT-SPOOF ( -- )
   TYPED-LOCAL-DIFF:RESET
   TLDT-SPOOF$ TYPED-LOCAL-DIFF:SOURCE ;

: TLDT-CONTROL ( -- )
   TYPED-LOCAL-DIFF:RESET
   TLDT-CONTROL$ TYPED-LOCAL-DIFF:SOURCE ;

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

: TLDT-CORE-SETUP ( -- )
   TYPED-LOCAL-DIFF:RESET
   TLDT-OUT TLDT-BUF-CAP LINT-OUT-BUFFER! ;

: TLDT-CORE-FINISH ( -- n n n )
   [: TYPED-LOCAL-DIFF:FINISH ;] catch {: rc:n :}
   LINT-OUT$ nip LINT-OUT-BUFFER-OFF
   0 rc ;

: TLDT-RUN-CORE ( ptr u8 n -- n n n )
   TLDT-CORE-SETUP
   TYPED-LOCAL-DIFF:FILE
   TLDT-CORE-FINISH ;

: TLDT-ASSERT-CLEAN ( n n n -- ) {: outu:n erru:n code:n :}
   code 0 T=
   TLDT-OUT outu TLDT-EMPTY$ T$=
   erru 0 T= ;

: TLDT-EXPECT-EXIT ( n n n n -- n n ) {: outu:n erru:n code:n expect:n :}
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
   TLDT-OUT outu TLDT-BAD-OUT$ T$= ;

: TLDT-TEST-BAD ( -- )
   TLDT-BAD TLDT-RUN-CORE 1 TLDT-EXPECT-EXIT TLDT-ASSERT-BAD ;

: TLDT-TEST-CRLF ( -- )
   [: TLDT-CRLF ;] DIFF:E-SYNTAX TTHROWSQ ;

: TLDT-TEST-MALFORMED ( -- )
   [: TLDT-TRUNC ;] DIFF:E-SYNTAX TTHROWSQ
   [: TLDT-SPOOF ;] DIFF:E-SYNTAX TTHROWSQ
   [: TLDT-CONTROL ;] DIFF:E-SYNTAX TTHROWSQ ;

: TLDT-MAIN ( -- )
   T-RESET
   TLDT-PREPARE
   TLDT-TEST-GOOD
   TLDT-TEST-LARGE
   TLDT-TEST-IGNORED
   TLDT-TEST-ALLOW
   TLDT-TEST-NON-FORTH
   TLDT-TEST-BAD
   TLDT-TEST-CRLF
   TLDT-TEST-MALFORMED
   CLEANUP-RUN
   TLDT-ROOT EXISTS? TFALSE
   T-REPORT
   s" typed-local-diff-lint-test: ok" type cr ;

TLDT-MAIN

;package
