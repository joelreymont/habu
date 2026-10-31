\ hb-build-stripped-cache-test.f - tools/hb-build-lib.f: a program that
\ allocates at run time links and runs, two links of it are the same bytes, a
\ refused publication leaves the prior map, and a declared cell caching a baked
\ table's address is mapped to the carried copy, or refused when no claim names
\ the table. tools/hb-build-test-lib.f lists the other hb-build rows.
\ Run: bin/hb --load tools/hb-build-stripped-cache-test.f

require src/lib/string.f
require tools/hb-build-test-lib.f
require tools/image-size-lib.f
require src/lib/engine-candidate.f
require test/aot-builder-engine.f

\ The shared fixture's words are private words of the library's package, so
\ this row reopens it the way tools/hb-build-test-lib.f does.
package HB-BUILD-CLI

create HBT-MAP-PATH PATH-CAP allot
create HBT-MAP-PATH2 PATH-CAP allot
variable HBT-MAP-PATH-U
DYNAMIC-BUFFER HBT-MAP-TEXT u8
create HBT-DONOR-BUF PATH-CAP allot
create HBT-DONOR-OUT-BUF PATH-CAP allot
variable HBT-DONOR-U
variable HBT-DONOR-OUT-U

: HBT-DONOR$ ( -- ptr u8 n ) HBT-DONOR-BUF HBT-DONOR-U @ ;
: HBT-DONOR-OUT$ ( -- ptr u8 n ) HBT-DONOR-OUT-BUF HBT-DONOR-OUT-U @ ;

: HBT-MAP$ ( ptr u8 n -- ptr u8 n )
   {: a:ptr u:n :}
   u 4 + PATH-CAP > if E-FS-CAPACITY throw then
   a HBT-MAP-PATH u BYTE-COPY
   s" .map" drop HBT-MAP-PATH u + 4 BYTE-COPY
   HBT-MAP-PATH u 4 + ;

: HBT-MAP2$ ( ptr u8 n -- ptr u8 n )
   {: a:ptr u:n :}
   u 4 + PATH-CAP > if E-FS-CAPACITY throw then
   a HBT-MAP-PATH2 u BYTE-COPY
   s" .map" drop HBT-MAP-PATH2 u + 4 BYTE-COPY
   HBT-MAP-PATH2 u 4 + ;

: HBT-CODE-ATTR ( ptr u8 n -- )
   {: a:ptr u:n :}
   a u HBT-MAP$ FS:FILE? TTRUE
   a u IMAGE-SIZE:MEASURE
   IMAGE-SIZE:ATTR? TTRUE
   IMAGE-SIZE:ATTR-BYTES IMAGE-SIZE:CODE-BYTES T=
   IMAGE-NAMES:LOADED-BYTES 0 > TTRUE
   IMAGE-NAMES:GENERATED-BYTES 0 > TTRUE ;

: HBT-CODE-MAPPED ( ptr u8 n -- )
   HBT-CODE-ATTR
   IMAGE-NAMES:BAKED-BYTES 0 > TTRUE
   IMAGE-NAMES:BAKED-SOURCE? TTRUE ;

: HBT-MAPL-ATTR ( -- )
   HBT-MAPL-OUT HBT-MAP$ {: path:ptr pathu:n :}
   path pathu FS:FILE-SIZE {: size:n :}
   size HBT-MAP-TEXT-RESERVE
   path pathu 0 HBT-MAP-TEXT BYTE-VIEW size FS:READ-ALL size T=
   0 HBT-MAP-TEXT BYTE-VIEW size {: map:ptr mapu:n :}
   map mapu s" 64K-ALLOC-LEN word baked src/lib/memory.f" CONTAINS? TTRUE
   map mapu s" ALLOC-64K word baked src/lib/memory.f" CONTAINS? TTRUE
   map mapu s" HBT-GLOBAL word loaded -" CONTAINS? TTRUE
   map mapu s" HIDDEN word loaded -" CONTAINS? TTRUE
   map mapu s" PUBLIC word loaded -" CONTAINS? TTRUE
   map mapu s" HBT-ATTR namespace" CONTAINS? TTRUE
   map mapu s" HBT-DEAD " CONTAINS? TFALSE ;

: HBT-MAPL-OUT2 ( -- ptr u8 n )
   HBT-MAPL-OUT2-BUF HBT-MAPL-OUT2-U @ ;

: HBT-PTRC-SRC ( -- ptr u8 n )
   HBT-PTRC-SRC-BUF HBT-PTRC-SRC-U @ ;

: HBT-PTRC-OUT ( -- ptr u8 n )
   HBT-PTRC-OUT-BUF HBT-PTRC-OUT-U @ ;

: HBT-PTRU-SRC ( -- ptr u8 n )
   HBT-PTRU-SRC-BUF HBT-PTRU-SRC-U @ ;

\ A declared cell holding a carried table's address, stored at build time:
\ src/habu/aot-closure.f XTD-ROW maps it to the carried copy (unmapped, the image
\ reads the engine's STR-MAX-I64$ from zero-filled memory and prints nineteen
\ NUL bytes). Line 2 spells the same table in code: equal lines prove both roads
\ give one address. Line 3 caches a pointer into the program's own window data,
\ which the window restores at its captured address and the map leaves alone.
: HBT-PTRC-SRC$ ( -- ptr u8 n )
   SB-RESET
   S\" require src/lib/string.f\ncreate PTRC-OWN 111 c, 107 c,\n" SB-APPEND
   S\" PERSISTED-PTR-VARIABLE PTRC-CACHED\nPERSISTED-PTR-VARIABLE PTRC-MINE\n" SB-APPEND
   S\" STR-MAX-I64$ PTRC-CACHED !\nPTRC-OWN PTRC-MINE !\n" SB-APPEND
   S\" : MAIN ( -- )\n   PTRC-CACHED @ STR-I64-DIGITS type cr\n" SB-APPEND
   S\"    STR-MAX-I64$ STR-I64-DIGITS type cr\n   PTRC-MINE @ 2 type cr ;\n" SB-APPEND
   SB$ ;

: HBT-PTRC-EXPECTED$ ( -- ptr u8 n )
   SB-RESET
   s" 9223372036854775807" SB-APPEND 10 SB-APPEND-C
   s" 9223372036854775807" SB-APPEND 10 SB-APPEND-C
   s" ok" SB-APPEND 10 SB-APPEND-C
   SB$ ;

\ ... and a declared cell holding a baked table NO claim names is refused by the
\ cell that holds it. TPB is the same table tools/hb-build-stripped-cells-test.f
\ HBT-STRIPPED-UNCARRIED-TABLE reaches through TMP-PATH-COPY-SRC, so the two
\ cases differ only in how the address got into the image: spelled in code, or
\ stored in a declared cell at build time.
: HBT-PTRU-SRC$ ( -- ptr u8 n )
   SB-RESET
   S\" PERSISTED-PTR-VARIABLE PTRU-CACHED\nTPB PTRU-CACHED !\n" SB-APPEND
   S\" : MAIN ( -- ) PTRU-CACHED @ 1 type cr ;\n" SB-APPEND
   SB$ ;

\ The pinned line of tools/hb-build-test-lib.f HBT-MAPL-SRC$, the byte the
\ image read back out of its run-time buffer.
: HBT-MAPL-EXPECTED$ ( -- ptr u8 n )
   S\" A\n" ;

\ THE ALLOCATION MOVED INTO MAIN LINKS, RUNS AND PRINTS ITS OWN BYTE: the
\ program tools/hb-build-stripped-cells-test.f HBT-STRIPPED-MAPPED-CELL refuses,
\ written the way that refusal suggests.
: HBT-RUN-MAPL ( -- )
   HBT-MAPL-OUT FS:FILE? TTRUE
   HBT-MAPL-OUT >LEN HBT-RUN-OUT HBT-CAPTURE-CAP >LEN HBT-RUN-ERR HBT-CAPTURE-CAP >LEN
   HBT-TIMEOUT-MS >MS RUN-CAPTURE HBT-CAPTURE>N {: outn:n errn:n rcn:n :}
   rcn 0 <> if HBT-RUN-OUT outn type HBT-RUN-ERR errn type then
   rcn 0 T=
   errn 0 T=
   HBT-RUN-OUT outn HBT-MAPL-EXPECTED$ T$= ;

: HBT-MAPL-SAME-BYTES ( -- )
   HBT-MAPL-OUT2 FS:FILE-SIZE  HBT-MAPL-OUT FS:FILE-SIZE T=
   HBT-MAPL-OUT FS:FILE-SIZE MEM-ALLOC-64K-SPAN {: abuf:ptr acap:n :}
   HBT-MAPL-OUT abuf acap FS:READ-ALL {: au:n :}
   HBT-MAPL-OUT2 FS:FILE-SIZE MEM-ALLOC-64K-SPAN {: bbuf:ptr bcap:n :}
   HBT-MAPL-OUT2 bbuf bcap FS:READ-ALL {: bu:n :}
   abuf au bbuf bu HBT-DIFF-AT -1 T= ;

\ ... and two links of it are the same bytes, the equality a mapped cell breaks
\ (unmapped, a stripped server's builds differed in eight window cells). Every
\ build runs its maker, and that is asserted.
: HBT-STRIPPED-SAME-TWICE ( -- )
   HBT-MAPL-SRC HBT-MAPL-SRC$ FS:WRITE-ALL
   HBT-MAPL-OUT HBT-REMOVE-FILE?
   HBT-MAPL-OUT2 HBT-REMOVE-FILE?
   HBT-MAPL-SRC HBT-MAPL-OUT HBT-HBB-PREPARE-AOT HBT-HBB-BUILD-OUT
   HBT-RUN-MAPL
   HBT-MAPL-OUT HBT-CODE-MAPPED
   HBT-MAPL-ATTR
   HBT-MAPL-SRC HBT-MAPL-OUT2 HBT-HBB-PREPARE-AOT HBT-HBB-BUILD-OUT
   HBB-MAKER-RUN @ 0 <> TTRUE
   HBT-MAPL-OUT2 HBT-CODE-MAPPED
   HBT-MAPL-SAME-BYTES
   HBT-MAPL-OUT HBT-REMOVE-FILE?
   HBT-MAPL-OUT2 HBT-REMOVE-FILE? ;

\ A directory at -o refuses the install's rename (HBB-INSTALL-OUT): the build
\ throws E-BUILD-INSTALL and the map beside -o keeps its prior bytes.
: HBT-STRIPPED-RENAME-FAILS ( -- )
   HBT-ROOT s" install-blocked" HBT-DONOR-OUT-BUF HBT-DONOR-OUT-U HBT-PATH!
   HBT-DONOR-OUT$ FS:MAKE-DIR
   HBT-DONOR-OUT$ HBT-MAP$ s" previous map" FS:WRITE-ALL
   HBT-MAPL-SRC HBT-DONOR-OUT$ HBT-HBB-PREPARE-AOT
   [: HBT-HBB-BUILD-OUT ;] E-BUILD-INSTALL TTHROWSQ
   HBT-DONOR-OUT$ FS:DIR? TTRUE
   HBT-DONOR-OUT$ HBT-MAP$ {: path:ptr pathu:n :}
   path pathu FS:FILE-SIZE {: size:n :}
   size HBT-MAP-TEXT-RESERVE
   path pathu 0 HBT-MAP-TEXT BYTE-VIEW size FS:READ-ALL size T=
   0 HBT-MAP-TEXT BYTE-VIEW size s" previous map" T$=
   path pathu FS:REMOVE-FILE
   HBT-DONOR-OUT$ FS:REMOVE-DIR ;

\ Select a symlink whose target has a map, but whose own sibling map is absent.
\ Only the selected sibling supplies attribution: restoring it must invalidate
\ the no-map build and produce rows attributed to their baked source files.
: HBT-DONOR-MAP-KEY ( -- )
   HBT-ROOT s" donor" HBT-DONOR-BUF HBT-DONOR-U HBT-PATH!
   HBT-ROOT s" donor-out" HBT-DONOR-OUT-BUF HBT-DONOR-OUT-U HBT-PATH!
   HBT-LINKER HBT-MAP$ FS:FILE? TTRUE
   HBT-LINKER HBT-DONOR$ FS:MAKE-SYMLINK
   HBT-DONOR$ HBT-MAP$ FS:FILE? TFALSE
   HBT-MAPL-SRC HBT-MAPL-SRC$ FS:WRITE-ALL
   HBT-MAPL-SRC HBT-DONOR-OUT$ HBT-HBB-PREPARE-AOT
   HBT-DONOR$ HBT-ENGINE!
   HBT-HBB-BUILD-OUT
   HBB-MAKER-RUN @ 0<> TTRUE
   HBT-DONOR-OUT$ HBT-CODE-ATTR
   IMAGE-NAMES:UNKNOWN-BYTES 0 > TTRUE
   IMAGE-NAMES:BAKED-SOURCE? TFALSE
   HBT-LINKER HBT-MAP$ HBT-DONOR$ HBT-MAP2$ FS:COPY-FILE-STREAM
   HBT-MAPL-SRC HBT-DONOR-OUT$ HBT-HBB-PREPARE-AOT
   HBT-DONOR$ HBT-ENGINE!
   HBT-HBB-BUILD-OUT
   HBB-MAKER-RUN @ 0<> TTRUE
   HBT-DONOR-OUT$ HBT-CODE-MAPPED ;

\ ... and a declared cell that holds a carried table's address is mapped to the
\ carried copy, so the image reads the bytes it ships.
: HBT-STRIPPED-CACHED-CARRIED ( -- )
   HBT-PTRC-SRC HBT-PTRC-SRC$ FS:WRITE-ALL
   HBT-PTRC-OUT HBT-REMOVE-FILE?
   HBT-PTRC-SRC HBT-PTRC-OUT HBT-HBB-PREPARE-AOT HBT-HBB-BUILD-OUT
   HBT-PTRC-OUT FS:FILE? TTRUE
   \ The carried DATA table needs no baked word.
   HBT-PTRC-OUT HBT-CODE-ATTR
   HBT-MAPL-OUT2 HBT-MAP$ HBT-PTRC-OUT HBT-MAP2$ FS:COPY-FILE-STREAM
   HBT-PTRC-OUT IMAGE-SIZE:MEASURE
   IMAGE-SIZE:ATTR? TFALSE
   HBT-PTRC-OUT >LEN HBT-RUN-OUT HBT-CAPTURE-CAP >LEN HBT-RUN-ERR HBT-CAPTURE-CAP >LEN
   HBT-TIMEOUT-MS >MS RUN-CAPTURE HBT-CAPTURE>N {: outn:n errn:n rcn:n :}
   rcn 0 <> if HBT-RUN-OUT outn type HBT-RUN-ERR errn type then
   rcn 0 T=
   errn 0 T=
   HBT-RUN-OUT outn HBT-PTRC-EXPECTED$ T$=
   HBT-PTRC-OUT HBT-REMOVE-FILE? ;

\ ... while a declared cell holding a baked table on no list is refused, naming
\ the cell that holds the address rather than a code site.
: HBT-STRIPPED-CACHED-UNOWNED ( -- )
   HBT-PTRU-SRC HBT-PTRU-SRC$ FS:WRITE-ALL
   HBT-PTRU-SRC HBT-RUN-MAKER {: uout:n uerr:n urc:n :}
   urc 70 T=
   HBB-ERR-BUF uerr s" declared data cell holds an engine address outside the restored span" CONTAINS? TTRUE
   HBB-ERR-BUF uerr s" word=PTRU-CACHED" CONTAINS? TTRUE ;

\ Public so the driver below runs it with the package CLOSED: the subtests
\ drive real builds, which resolve names in whatever package scope is open.
public
: HBT-STRIPPED-CACHE-MAIN ( -- )
   T-RESET
   \ Baked attribution needs the donor binary's own map; the preloaded linker
   \ snapshot has different capture coordinates and no map.
   AOT-BUILDER-ENGINE:ENSURE ENGINE-CANDIDATE:PATH$ HBT-KEYED!
   HBT-PREPARE
   HBT-STRIPPED-SAME-TWICE
   HBT-STRIPPED-RENAME-FAILS
   HBT-DONOR-MAP-KEY
   HBT-STRIPPED-CACHED-CARRIED
   HBT-STRIPPED-CACHED-UNOWNED
   FS:CLEANUP-RUN
   HBT-ROOT FS:EXISTS? TFALSE
   T-REPORT
   s" hb-build-stripped-cache-test: ok" type cr ;

;package

HB-BUILD-CLI:HBT-STRIPPED-CACHE-MAIN
