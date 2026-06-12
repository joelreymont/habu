\ build.f — driver: bake a USER program into a standalone signed engine binary.
\ tools/hb-build.sh owns the I/O paths: it writes the program to /tmp/hb-build-src,
\ runs the maker (toolchain + this driver, compiled by bin/hb), and moves
\ /tmp/hb-build-got to the requested output. The emitted binary is the bare
\ engine with the program as its baked source: it runs at startup and exits.
\ The toolchain compiling THIS driver is checker-hooked; the user program is
\ compiled by the emitted engine at its own startup (unchecked there).

: BLD-IN  s" hb-build-src" TMP-PATH ;
: BLD-OUT s" hb-build-got" TMP-PATH ;
variable PB  variable PN  variable PFD  variable PRD
$40000 constant PMAX

: READ-PROG
   BLD-IN PATH0  0 0 open PFD !
   here PB !  PMAX allot  0 PN !
   BEGIN                                                 \ read() may return short
     PFD @  PB @ PN @ +  PMAX PN @ -  read PRD !
     PRD @ 0 >
   WHILE  PN @ PRD @ + PN !  REPEAT
   PFD @ close
   PN @ 0 > 0= IF s" hb-build: empty source" 74 die THEN
   PN @ PMAX = IF s" hb-build: source exceeds buffer" 74 die THEN ;

: GO
   READ-PROG
   0 STDIN? !
   PB @ PN @ EMIT-FORTH
   BUILD-IMAGE
   s" hb-prog" SET-SIGID  CODESIG2
   BLD-OUT PATH0  1537 493 open  dup MBUF MLEN @ write drop  close ;
GO
