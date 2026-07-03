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
0 set-check

: BLD-IN  s" hb-build-src" TMP-PATH ;
: BLD-CHK s" hb-build-check-src" TMP-PATH ;
: BLD-OUT s" hb-build-got" TMP-PATH ;

variable PB  variable PN  variable PFD  variable PRD
$40000 constant PMAX
: BLD-PB@ PB @ ;
s" BLD-PB@" s" -- ptr u8" TRUST

: BLD-FALSE ( -- bool ) 0 0= 0= ;
: BLD-JSON-ARG? ( -- bool )
   ARGC 2 <= IF BLD-FALSE EXIT THEN
   2 ARGV$ dup 1 = IF drop c@ 49 = ELSE 2drop BLD-FALSE THEN ;
: BLD-RUNTIME-ARGS ( -- )
   ARGC 1 > IF 1 ARGV$ DIAG-FILE! THEN
   BLD-JSON-ARG? IF -1 JSON-DIAGS ! THEN ;

: ENSURE-PBUF
   PB @ 0= IF here PB !  PMAX allot THEN ;

: READ-PATH {: a:ptr u :}
   a u PATH0  0 0 open PFD !
   PFD @ 0 < IF s" hb-build: cannot open source" 74 die THEN
   ENSURE-PBUF  0 PN !
   BEGIN                                                 \ read() may return short
     PFD @  BLD-PB@ PN @ +  PMAX PN @ -  read PRD !
     PRD @ 0 >
   WHILE  PN @ PRD @ + PN !  REPEAT
   PRD @ 0 < IF PFD @ close s" hb-build: read failed" 74 die THEN
   PFD @ close
   PN @ 0 > 0= IF s" hb-build: empty source" 74 die THEN
   PN @ PMAX = IF s" hb-build: source exceeds buffer" 74 die THEN ;

: READ-CHECK  BLD-CHK READ-PATH ;
: READ-PROG   BLD-IN  READ-PATH ;

: GO ( -- )
   BLD-RUNTIME-ARGS
   READ-CHECK
   BLD-PB@ PN @ VERIFY:SOURCE-BUF
   READ-PROG
   BLD-PB@ SHK-A !  PN @ SHK-U !  0 SHAKE? !
   0 0= 0= STDIN? !
   BLD-PB@ PN @ EMIT-FORTH
   s" hb-prog" BLD-OUT DRV-EMIT-IMAGE ;

\ Process boundary: report uncaught throws instead of exiting silently with
\ the raw code (driver-io.f DRV-FAIL; exit code stays the throw code).
: BLD-RUN ( -- )
   [: GO ;] catch
   dup 0 = IF drop EXIT THEN
   DRV-FAIL ;

BLD-RUN
