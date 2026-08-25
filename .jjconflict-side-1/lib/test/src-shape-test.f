\ src-shape-test.f - tests for the shared source-shape assertion helper.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f lib/fs-mutate.f lib/test/src-shape.f lib/test/src-shape-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/fs.f
require lib/fs-mutate.f
require lib/test/src-shape.f
require lib/test/mapped.f

\ Fixture helpers belong to the file, not the global dictionary; all twelve of
\ these names had zero consumers elsewhere in the tree.
package SHAPE-TEST

create SST-DIR-BUF FS-PATH-CAP allot
variable SST-DIR-U
create SST-PATH-BUF FS-PATH-CAP allot
variable SST-PATH-U

: SST-FIXTURE$ ( -- ptr u8 n )
   s" ALPHA BETA ALPHA GAMMA ALPHA" ;

: SST-DIR$ ( -- ptr u8 n )
   SST-DIR-BUF SST-DIR-U @ ;

: SST-PATH$ ( -- ptr u8 n )
   SST-PATH-BUF SST-PATH-U @ ;

: SST-STORE ( ptr u8 n ptr u8 ptr a -- ) {: a:ptr u:n dst:ptr lenp:ptr :}
   a dst u BYTE-COPY
   u lenp ! ;

: SST-SETUP ( -- )
   s" habu-src-shape" TMPDIR-MKDIR SST-DIR-BUF SST-DIR-U SST-STORE
   SST-DIR$ s" fixture.f" SST-PATH-BUF JOIN-PATH SST-PATH-U !
   SST-PATH$ SST-FIXTURE$ WRITE-ALL ;

\ CHECK-FIT is the fail-closed ceiling guard: it must pass when a source fits and
\ throw E-FS-CAPACITY (after a file/size diagnostic) when it would exceed the cap.
: SST-CHECK-FIT-CASES ( -- )
   [: s" fits.f" 10 100 SHAPE:CHECK-FIT ;] 0 TTHROWSQ
   [: s" big.f" 100 10 SHAPE:CHECK-FIT ;] E-FS-CAPACITY TTHROWSQ ;

\ One span is live per loaded source: every LOAD installs its own mapping and
\ releases the span the previous LOAD owned. Three loads of different sizes, so
\ two spans are abandoned; both must be unmapped (lib/test/mapped.f).
: SST-RELEASE-CASES ( -- )
   SST-PATH$ SHAPE:LOAD
   SHAPE:TEXT drop {: p0:ptr :}
   s" AGENTS.md" SHAPE:LOAD
   SHAPE:TEXT drop {: p1:ptr :}
   p0 p1 <> TTRUE
   p0 MAPPED:LIVE? TFALSE
   p1 MAPPED:LIVE? TTRUE
   s" README.md" SHAPE:LOAD
   SHAPE:TEXT drop {: p2:ptr :}
   p1 p2 <> TTRUE
   p1 MAPPED:LIVE? TFALSE
   p2 MAPPED:LIVE? TTRUE ;

\ LOAD auto-sizes the buffer to the file, then TEXT/HAS?/COUNT read it back.
: SST-LOAD-CASES ( -- )
   SST-PATH$ SHAPE:LOAD
   SHAPE:TEXT SST-FIXTURE$ T$=
   s" ALPHA" SHAPE:MUST-HAVE
   s" OMEGA" SHAPE:MUST-LACK
   s" GAMMA" SHAPE:HAS? TTRUE
   s" OMEGA" SHAPE:HAS? TFALSE
   s" ALPHA" 3 SHAPE:COUNT=
   s" BETA" 1 SHAPE:COUNT=
   s" ZETA" 0 SHAPE:COUNT= ;

: SST-MAIN ( -- )
   T-RESET
   SST-SETUP
   SST-CHECK-FIT-CASES
   SST-LOAD-CASES
   SST-RELEASE-CASES
   T-REPORT
   s" src-shape-test: ok" type cr ;

SST-MAIN

;package
