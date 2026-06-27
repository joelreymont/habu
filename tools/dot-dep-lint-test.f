\ dot-dep-lint-test.f - checked fixtures for dot dependency lint.
\ Load after lib/test.f and tools/dot-dep-lint-core.f.

: DDLT-DOT-PATHS ( -- )
   s" .dots/habu-example-12345678.md" DDL-DOT-PATH? TTRUE
   s" .dots/archive/habu-example-12345678.md" DDL-DOT-PATH? TTRUE
   s" docs/habu-example-12345678.md" DDL-DOT-PATH? TFALSE
   s" .dots/config" DDL-DOT-PATH? TFALSE ;

: DDLT-DOT-IDS ( -- )
   s" .dots/habu-example-12345678.md" DDL-DOT-ID$ s" habu-example-12345678" T$=
   s" .dots/archive/habu-old-87654321.md" DDL-DOT-ID$ s" habu-old-87654321" T$= ;

: DDLT-FRONT-MATTER ( -- )
   s" ---" DDL-FM-MARK? TTRUE
   s"  ---  " DDL-FM-MARK? TTRUE
   s" ----" DDL-FM-MARK? TFALSE
   s" blocks:" DDL-BLOCKS-LINE? TTRUE
   s"   blocks:  " DDL-BLOCKS-LINE? TTRUE
   s" blocker:" DDL-BLOCKS-LINE? TFALSE ;

: DDLT-BLOCKERS ( -- )
   s"   - habu-a-12345678" DDL-BLOCKER-LINE? TTRUE
   s" title: nope" DDL-BLOCKER-LINE? TFALSE
   s"   - habu-a-12345678" DDL-BLOCKER$ s" habu-a-12345678" T$= ;

: DDLT-LIVE-LINT ( -- )
   DOT-DEP-LINT ;

: DDLT-MAIN ( -- )
   T-RESET
   DDLT-DOT-PATHS
   DDLT-DOT-IDS
   DDLT-FRONT-MATTER
   DDLT-BLOCKERS
   DDLT-LIVE-LINT
   T-REPORT ;

DDLT-MAIN
