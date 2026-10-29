\ dot-dep-lint-test.f - checked fixtures for dot dependency lint.

require src/lib/errors.f
require src/lib/string.f
require src/lib/test.f
require src/lib/memory.f
require src/lib/vector.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require tools/lint/text.f
require tools/lint/intern.f
require tools/dot-dep-lint-core.f

package DOT-DEP

$1000 constant TEST-OUT-CAP

create TEST-ROOT-BUF PATH-CAP allot
create TEST-DOTS-BUF PATH-CAP allot
create TEST-NEST-DIR-BUF PATH-CAP allot
create TEST-ARCHIVE-DIR-BUF PATH-CAP allot
create TEST-TOP-BUF PATH-CAP allot
create TEST-NESTED-BUF PATH-CAP allot
create TEST-ARCHIVED-BUF PATH-CAP allot
create TEST-UNIQUE-BUF PATH-CAP allot
create TEST-OUT TEST-OUT-CAP allot
create TEST-WANT TEST-OUT-CAP allot   \ the expected report: two paths, more than SB holds

variable TEST-ROOT-U
variable TEST-DOTS-U
variable TEST-NEST-DIR-U
variable TEST-ARCHIVE-DIR-U
variable TEST-TOP-U
variable TEST-NESTED-U
variable TEST-ARCHIVED-U
variable TEST-UNIQUE-U
variable TEST-WANT-U

: TEST-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr lenp:ptr :}
   a dst u BYTE-COPY
   u lenp ! ;

: TEST-ROOT$ ( -- ptr u8 n )
   TEST-ROOT-BUF TEST-ROOT-U @ ;

: TEST-DOTS$ ( -- ptr u8 n )
   TEST-DOTS-BUF TEST-DOTS-U @ ;

: TEST-NEST-DIR$ ( -- ptr u8 n )
   TEST-NEST-DIR-BUF TEST-NEST-DIR-U @ ;

: TEST-ARCHIVE-DIR$ ( -- ptr u8 n )
   TEST-ARCHIVE-DIR-BUF TEST-ARCHIVE-DIR-U @ ;

: TEST-TOP$ ( -- ptr u8 n )
   TEST-TOP-BUF TEST-TOP-U @ ;

: TEST-NESTED$ ( -- ptr u8 n )
   TEST-NESTED-BUF TEST-NESTED-U @ ;

: TEST-ARCHIVED$ ( -- ptr u8 n )
   TEST-ARCHIVED-BUF TEST-ARCHIVED-U @ ;

: TEST-UNIQUE$ ( -- ptr u8 n )
   TEST-UNIQUE-BUF TEST-UNIQUE-U @ ;

: TEST-ROOT-ERRORS ( -- )
   [: s" " ROOT! ;] E-FS-PATH TTHROWSQ ;

: TEST-FRONT-MATTER ( -- )
   s" ---" DDP-FM-MARK? TTRUE
   s"  ---  " DDP-FM-MARK? TTRUE
   s" ----" DDP-FM-MARK? TFALSE
   s" blocks:" DDP-BLOCKS-LINE? TTRUE
   s"   blocks:  " DDP-BLOCKS-LINE? TTRUE
   s" blocker:" DDP-BLOCKS-LINE? TFALSE ;

: TEST-BLOCKERS ( -- )
   s"   - habu-a-12345678" DDP-BLOCKER-LINE? TTRUE
   s" title: nope" DDP-BLOCKER-LINE? TFALSE
   s"   - habu-a-12345678" DDP-BLOCKER$ s" habu-a-12345678" T$= ;

: TEST-PREPARE-DIRS ( -- )
   FS:CLEANUP-RESET
   s" habu-dot-dep-lint" FS:MKTEMP-DIR TEST-ROOT-BUF TEST-ROOT-U TEST-COPY!
   TEST-ROOT$ FS:CLEANUP-TREE+
   TEST-ROOT$ s" .dots" TEST-DOTS-BUF FS:JOIN-PATH TEST-DOTS-U !
   TEST-DOTS$ s" parent" TEST-NEST-DIR-BUF FS:JOIN-PATH TEST-NEST-DIR-U !
   TEST-DOTS$ s" archive" TEST-ARCHIVE-DIR-BUF FS:JOIN-PATH TEST-ARCHIVE-DIR-U !
   TEST-NEST-DIR$ FS:MAKE-DIRS
   TEST-ARCHIVE-DIR$ FS:MAKE-DIRS ;

: TEST-PREPARE-PATHS ( -- )
   TEST-DOTS$ s" habu-duplicate-12345678.md" TEST-TOP-BUF FS:JOIN-PATH TEST-TOP-U !
   TEST-NEST-DIR$ s" habu-duplicate-12345678.md" TEST-NESTED-BUF FS:JOIN-PATH TEST-NESTED-U !
   TEST-ARCHIVE-DIR$ s" habu-duplicate-12345678.md" TEST-ARCHIVED-BUF FS:JOIN-PATH TEST-ARCHIVED-U !
   TEST-DOTS$ s" habu-unique-87654321.md" TEST-UNIQUE-BUF FS:JOIN-PATH TEST-UNIQUE-U ! ;

: TEST-PREPARE-FILES ( -- )
   TEST-TOP$ s" malformed shadow without frontmatter\n" FS:WRITE-ALL
   TEST-NESTED$ s" ---\ntitle: authoritative\n---\n" FS:WRITE-ALL
   TEST-ARCHIVED$ s" malformed archived shadow\n" FS:WRITE-ALL
   TEST-UNIQUE$ s" ---\ntitle: unique\n---\n" FS:WRITE-ALL ;

: TEST-PREPARE ( -- )
   TEST-PREPARE-DIRS
   TEST-PREPARE-PATHS
   TEST-PREPARE-FILES ;

: TEST-WANT+ ( ptr u8 n -- )
   TEST-WANT TEST-OUT-CAP TEST-WANT-U BUF-APPEND ;

: TEST-EXPECTED$ ( -- ptr u8 n )
   TEST-WANT-U BUF-RESET
   s" DOT-DEP-DUPLICATE habu-duplicate-12345678: " TEST-WANT+
   TEST-TOP$ TEST-WANT+
   s" , " TEST-WANT+
   TEST-NESTED$ TEST-WANT+
   S\" \ndot-dep-lint: 3 dot(s), 0 blocker(s), 1 finding(s)\n" TEST-WANT+
   TEST-WANT TEST-WANT-U @ ;

: TEST-DUPLICATE-FIXTURE ( -- )
   TEST-PREPARE
   TEST-OUT TEST-OUT-CAP LINT-OUT-BUFFER!
   [: TEST-DOTS$ LINT-ROOT ;] catch {: rc:n :}
   LINT-OUT$ {: out:ptr outu:n :}
   LINT-OUT-BUFFER-OFF
   rc 1 T=
   out outu TEST-EXPECTED$ T$=
   FS:CLEANUP-RUN ;

: TEST-LIVE-LINT ( -- )
   LINT ;

: TEST-MAIN ( -- )
   T-RESET
   TEST-ROOT-ERRORS
   TEST-FRONT-MATTER
   TEST-BLOCKERS
   TEST-DUPLICATE-FIXTURE
   TEST-LIVE-LINT
   T-REPORT ;

TEST-MAIN

;package
