\ dot-dep-lint-test.f - checked fixtures for dot dependency lint.
\ Load after lib/test.f and tools/dot-dep-lint-core.f.

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/vector.f
require lib/fs.f
require lib/fs-mutate.f
require tools/lint/text.f
require tools/lint/intern.f
require tools/dot-dep-lint-core.f

package DOT-DEP

$1000 constant TEST-OUT-CAP

create TEST-ROOT-BUF FS-PATH-CAP allot
create TEST-DOTS-BUF FS-PATH-CAP allot
create TEST-NEST-DIR-BUF FS-PATH-CAP allot
create TEST-ARCHIVE-DIR-BUF FS-PATH-CAP allot
create TEST-TOP-BUF FS-PATH-CAP allot
create TEST-NESTED-BUF FS-PATH-CAP allot
create TEST-ARCHIVED-BUF FS-PATH-CAP allot
create TEST-UNIQUE-BUF FS-PATH-CAP allot
create TEST-OUT TEST-OUT-CAP allot

variable TEST-ROOT-U
variable TEST-DOTS-U
variable TEST-NEST-DIR-U
variable TEST-ARCHIVE-DIR-U
variable TEST-TOP-U
variable TEST-NESTED-U
variable TEST-ARCHIVED-U
variable TEST-UNIQUE-U

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

: TEST-DOT-IDS ( -- )
   s" .dots/habu-example-12345678.md" DDP-DOT-ID$ s" habu-example-12345678" T$=
   s" .dots/archive/habu-old-87654321.md" DDP-DOT-ID$ s" habu-old-87654321" T$= ;

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
   CLEANUP-RESET
   s" habu-dot-dep-lint" TMPDIR-MKDIR TEST-ROOT-BUF TEST-ROOT-U TEST-COPY!
   TEST-ROOT$ CLEANUP-TREE+
   TEST-ROOT$ s" .dots" TEST-DOTS-BUF JOIN-PATH TEST-DOTS-U !
   TEST-DOTS$ s" parent" TEST-NEST-DIR-BUF JOIN-PATH TEST-NEST-DIR-U !
   TEST-DOTS$ s" archive" TEST-ARCHIVE-DIR-BUF JOIN-PATH TEST-ARCHIVE-DIR-U !
   TEST-NEST-DIR$ MAKE-DIRS
   TEST-ARCHIVE-DIR$ MAKE-DIRS ;

: TEST-PREPARE-PATHS ( -- )
   TEST-DOTS$ s" habu-duplicate-12345678.md" TEST-TOP-BUF JOIN-PATH TEST-TOP-U !
   TEST-NEST-DIR$ s" habu-duplicate-12345678.md" TEST-NESTED-BUF JOIN-PATH TEST-NESTED-U !
   TEST-ARCHIVE-DIR$ s" habu-duplicate-12345678.md" TEST-ARCHIVED-BUF JOIN-PATH TEST-ARCHIVED-U !
   TEST-DOTS$ s" habu-unique-87654321.md" TEST-UNIQUE-BUF JOIN-PATH TEST-UNIQUE-U ! ;

: TEST-PREPARE-FILES ( -- )
   TEST-TOP$ s" malformed shadow without frontmatter\n" WRITE-ALL
   TEST-NESTED$ s" ---\ntitle: authoritative\n---\n" WRITE-ALL
   TEST-ARCHIVED$ s" malformed archived shadow\n" WRITE-ALL
   TEST-UNIQUE$ s" ---\ntitle: unique\n---\n" WRITE-ALL ;

: TEST-PREPARE ( -- )
   TEST-PREPARE-DIRS
   TEST-PREPARE-PATHS
   TEST-PREPARE-FILES ;

: TEST-ROOT-PATHS ( -- )
   TEST-DOTS$ ROOT!
   TEST-TOP$ ROOT-DOT-PATH? TTRUE
   TEST-NESTED$ ROOT-DOT-PATH? TTRUE
   TEST-ARCHIVED$ ROOT-DOT-PATH? TFALSE
   TEST-DOTS$ ROOT-DOT-PATH? TFALSE ;

: TEST-EXPECTED$ ( -- ptr u8 n )
   SB-RESET
   s" DOT-DEP-DUPLICATE habu-duplicate-12345678: " SB-APPEND
   TEST-TOP$ SB-APPEND
   s" , " SB-APPEND
   TEST-NESTED$ SB-APPEND
   10 SB-APPEND-C
   s" dot-dep-lint: 3 dot(s), 0 blocker(s), 1 finding(s)" SB-APPEND
   10 SB-APPEND-C
   SB$ ;

: TEST-DUPLICATE-FIXTURE ( -- )
   TEST-PREPARE
   TEST-ROOT-PATHS
   TEST-OUT TEST-OUT-CAP LINT-OUT-BUFFER!
   [: TEST-DOTS$ LINT-ROOT ;] catch {: rc:n :}
   LINT-OUT$ {: out:ptr outu:n :}
   LINT-OUT-BUFFER-OFF
   rc 1 T=
   out outu TEST-EXPECTED$ T$=
   CLEANUP-RUN ;

: TEST-LIVE-LINT ( -- )
   LINT ;

: TEST-MAIN ( -- )
   T-RESET
   TEST-ROOT-ERRORS
   TEST-DOT-IDS
   TEST-FRONT-MATTER
   TEST-BLOCKERS
   TEST-DUPLICATE-FIXTURE
   TEST-LIVE-LINT
   T-REPORT ;

TEST-MAIN

;package
