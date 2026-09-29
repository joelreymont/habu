\ hb-build-stripped-cells-test.f - checked fixture for tools/hb-build-lib.f:
\ the DATA cells a stripped image refuses or carries - a pointer into memory
\ the build mapped, in an undeclared and in a declared cell, the same
\ allocation taken at run time, and a baked table no claim names.
\ tools/hb-build-test-lib.f lists the other hb-build rows.
\ Run: bin/hb --load tools/hb-build-stripped-cells-test.f

require tools/hb-build-test-lib.f

\ The shared fixture's words are private words of the library's package, so
\ this row reopens it the way tools/hb-build-test-lib.f does.
package HB-BUILD-CLI

: HBT-TABLE-SRC ( -- ptr u8 n )
   HBT-TABLE-SRC-BUF HBT-TABLE-SRC-U @ ;

: HBT-MAPC-SRC ( -- ptr u8 n )
   HBT-MAPC-SRC-BUF HBT-MAPC-SRC-U @ ;

: HBT-MAPD-SRC ( -- ptr u8 n )
   HBT-MAPD-SRC-BUF HBT-MAPD-SRC-U @ ;

\ ... and a baked `create` TABLE that no claim names is refused exactly as
\ before. TPB is src/os/env-base.f's TMP-PATH buffer, the same shape as the
\ carried digit tables and as the claimed path scratch, and TMP-PATH-COPY-SRC
\ spells its address before any other engine data - so a table travels because
\ the list NAMES it, never because it is a table and never because a table in
\ the same file, or in the file next to it, is claimed.
: HBT-TABLE-SRC$ ( -- ptr u8 n )
   S\" : MAIN ( -- ) s\" x\" TMP-PATH-COPY-SRC ;\n" ;

\ A POINTER INTO MEMORY THE BUILD MAPPED, which is the class dot
\ habu-refuse-a-stripped-92290c75 reported from Tender: the program takes a
\ 64 KB buffer AT LOAD TIME and stores the address in a persistent cell, so the
\ image carries an address only the linking process ever held - the same in
\ every run of one image, different in every build under ASLR, and mapped by
\ nobody when the image runs. Tender's stripped server faulted at one of eight
\ such cells before any clone. `variable BUF` is the undeclared cell the span
\ scan meets (src/habu/aot-lib.f AOT-DATA-TEXTPTR-CHECK); the store sits at the
\ top level, which is where a pointer may enter raw storage at all.
: HBT-MAPC-SRC$ ( -- ptr u8 n )
   SB-RESET
   S\" require lib/memory.f\nvariable BUF\nMEM-ALLOC-64K drop BUF !\n" SB-APPEND
   S\" : MAIN ( -- ) s\" mapped\" type cr ;\n" SB-APPEND
   SB$ ;

\ ... and the same store into a DECLARED cell, which the span scan skips BY its
\ declaration: PERSISTED-PTR-VARIABLE registers a DATA row in the engine's
\ address-cell table, so src/habu/aot-closure.f XTD-ROW is the site that has to
\ ask. Measured by disabling the span scan on this tree: with it off this
\ program is still refused and HBT-MAPC-SRC$'s links.
: HBT-MAPD-SRC$ ( -- ptr u8 n )
   SB-RESET
   S\" require lib/memory.f\nPERSISTED-PTR-VARIABLE PBUF\n" SB-APPEND
   S\" MEM-ALLOC-64K drop PBUF !\n: MAIN ( -- ) s\" declared\" type cr ;\n" SB-APPEND
   SB$ ;

\ The pinned line of tools/hb-build-test-lib.f HBT-MAPL-SRC$, the byte the
\ image read back out of its run-time buffer.
: HBT-MAPL-EXPECTED$ ( -- ptr u8 n )
   S\" A\n" ;

\ A PERSISTENT CELL HOLDING A POINTER INTO MEMORY THE BUILD MAPPED is refused by
\ the cell that holds it. The value is left out of the pin because it is an mmap
\ address: it differs in every build, which is the fault itself.
: HBT-STRIPPED-MAPPED-CELL ( -- )
   HBT-MAPC-SRC HBT-MAPC-SRC$ WRITE-ALL
   HBT-MAPC-SRC HBT-RUN-MAKER {: cout:n cerr:n crc:n :}
   crc 0 <> TTRUE
   HBB-ERR-BUF cerr s" holds a pointer into memory the build mapped" CONTAINS? TTRUE
   HBB-ERR-BUF cerr s" word=BUF" CONTAINS? TTRUE
   HBB-ERR-BUF cerr s" data-off=" CONTAINS? TTRUE
   HBB-ERR-BUF cerr s" allocate at run time" CONTAINS? TTRUE ;

\ ... and so is one in a DECLARED cell, at the other site and by its own name.
: HBT-STRIPPED-MAPPED-DECLARED ( -- )
   HBT-MAPD-SRC HBT-MAPD-SRC$ WRITE-ALL
   HBT-MAPD-SRC HBT-RUN-MAKER {: dout:n derr:n drc:n :}
   drc 0 <> TTRUE
   HBB-ERR-BUF derr s" holds a pointer into memory the build mapped" CONTAINS? TTRUE
   HBB-ERR-BUF derr s" word=PBUF" CONTAINS? TTRUE ;

\ ... while the allocation moved into MAIN links, runs and prints its own byte.
: HBT-STRIPPED-MAPPED-LATE ( -- )
   HBT-MAPL-SRC HBT-MAPL-SRC$ WRITE-ALL
   HBT-MAPL-OUT HBT-REMOVE-FILE?
   HBT-MAPL-SRC HBT-MAPL-OUT HBT-HBB-PREPARE-AOT HBT-HBB-BUILD-OUT
   HBT-MAPL-OUT FILE? TTRUE
   HBT-MAPL-OUT >LEN HBT-RUN-OUT HBT-CAPTURE-CAP >LEN HBT-RUN-ERR HBT-CAPTURE-CAP >LEN
   HBT-TIMEOUT-MS >MS RUN-CAPTURE HBT-CAPTURE>N {: outn:n errn:n rcn:n :}
   rcn 0 <> if HBT-RUN-OUT outn type HBT-RUN-ERR errn type then
   rcn 0 T=
   errn 0 T=
   HBT-RUN-OUT outn HBT-MAPL-EXPECTED$ T$= ;

\ ... while a baked create table on no list is refused however much it looks like
\ the carried ones.
: HBT-STRIPPED-UNCARRIED-TABLE ( -- )
   HBT-TABLE-SRC HBT-TABLE-SRC$ WRITE-ALL
   HBT-TABLE-SRC HBT-RUN-MAKER {: tout:n terr:n trc:n :}
   trc 0 <> TTRUE
   HBB-ERR-BUF terr s" outside the restored span" CONTAINS? TTRUE
   HBB-ERR-BUF terr s" caller=TMP-PATH-COPY-SRC" CONTAINS? TTRUE
   HBB-ERR-BUF terr s" target=TPB" CONTAINS? TTRUE ;

\ Public so the driver below runs it with the package CLOSED: the subtests
\ drive real builds, which resolve names in whatever package scope is open.
public
: HBT-STRIPPED-CELLS-MAIN ( -- )
   T-RESET
   HBT-PREPARE
   HBT-STRIPPED-MAPPED-CELL
   HBT-STRIPPED-MAPPED-DECLARED
   HBT-STRIPPED-MAPPED-LATE
   HBT-STRIPPED-UNCARRIED-TABLE
   CLEANUP-RUN
   HBT-ROOT EXISTS? TFALSE
   T-REPORT
   s" hb-build-stripped-cells-test: ok" type cr ;

;package

HB-BUILD-CLI:HBT-STRIPPED-CELLS-MAIN
