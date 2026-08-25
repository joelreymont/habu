\ build.f — driver: bake a USER program into a standalone signed engine binary.
\ tools/hb-build.f owns the I/O paths: it writes the bundled source to
\ /tmp/hb-build-src, the user-only check input to /tmp/hb-build-check-src, runs
\ the maker (toolchain + this driver, compiled by bin/hb), and moves
\ /tmp/hb-build-got to the requested output.
\ The toolchain compiling THIS driver is checker-hooked. This driver also
\ pre-verifies user colon definitions with VERIFY:SOURCE-BUF before bundling the source.
\ It does NOT execute top-level user code at build time; the emitted bundle still
\ recompiles/runs the full source at its own startup.

\ Audited driver boundary: generated makers run this source at startup, then
\ VERIFY:SOURCE-BUF checks user colon definitions explicitly.
\ Dissolves with staged fixpoint source checking: habu-staged-fixpoint-src-0b5fc6e6.
0 set-check

package BUILD-DRIVER

: BLD-IN  s" hb-build-src" TMP-PATH ;
: BLD-CHK s" hb-build-check-src" TMP-PATH ;
: BLD-OUT s" hb-build-got" TMP-PATH ;

variable PN

: BLD-FALSE ( -- bool ) 0 0= 0= ;
: BLD-JSON-ARG? ( -- bool )
   ARGC 2 <= IF BLD-FALSE EXIT THEN
   2 ARGV$ dup 1 = IF drop c@ 49 = ELSE 2drop BLD-FALSE THEN ;
: BLD-RUNTIME-ARGS ( -- )
   ARGC 1 > IF 1 ARGV$ DIAG-FILE! THEN
   BLD-JSON-ARG? IF -1 JSON-DIAGS ! THEN ;

: READ-PATH {: a:ptr u :}
   a u 0 MAKER-SOURCE:READ PN ! ;

: READ-CHECK  BLD-CHK READ-PATH ;
: READ-PROG   BLD-IN  READ-PATH ;

: GO ( -- )
   BLD-RUNTIME-ARGS
   READ-CHECK
   MAKER-SOURCE:SOURCE PN @ VERIFY:SOURCE-BUF
   READ-PROG
   MAKER-SOURCE:SOURCE SHK-A !  PN @ SHK-U !  0 SHAKE? !
   0 0= 0= STDIN? !
   MAKER-SOURCE:SOURCE PN @ ENGINE-EMIT:FORTH
   s" hb-prog" BLD-OUT DRV-EMIT-IMAGE ;

\ Process boundary: report uncaught throws instead of exiting silently
\ (driver-io.f DRV-FAIL; exit code stays the throw code when representable,
\ else die maps it to UNCAUGHT-RC).
public

: RUN ( -- )
   [: GO ;] catch
   dup 0 = IF drop EXIT THEN
   DRV-FAIL ;

;package

BUILD-DRIVER:RUN
