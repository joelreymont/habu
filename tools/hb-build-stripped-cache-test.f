\ hb-build-stripped-cache-test.f - checked fixture for tools/hb-build-lib.f:
\ a program that allocates at run time links and runs, two links of it are
\ the same bytes and so is an object hit's relink of
\ it, and a declared cell caching a baked table's address is mapped to the
\ carried copy, or refused when no claim names the table.
\ tools/hb-build-test-lib.f lists the other hb-build rows.
\ Run: bin/hb --load tools/hb-build-stripped-cache-test.f

require src/lib/string.f
require tools/hb-build-test-lib.f
require tools/image-size-lib.f
require src/lib/engine-candidate.f
require test/aot-builder-engine.f

\ The shared fixture's words are private words of the library's package, so
\ this row reopens it the way tools/hb-build-test-lib.f does.
package HB-BUILD-CLI

create HBT-MAP-PATH FS-PATH-CAP allot
create HBT-MAP-PATH2 FS-PATH-CAP allot
create HBT-OBJ-MAP-PATH FS-PATH-CAP allot
variable HBT-MAP-PATH-U
variable HBT-OBJ-MAP-PATH-U
DYNAMIC-BUFFER HBT-MAP-TEXT u8
create HBT-DONOR-BUF FS-PATH-CAP allot
create HBT-DONOR-OUT-BUF FS-PATH-CAP allot
variable HBT-DONOR-U
variable HBT-DONOR-OUT-U

: HBT-DONOR$ ( -- ptr u8 n ) HBT-DONOR-BUF HBT-DONOR-U @ ;
: HBT-DONOR-OUT$ ( -- ptr u8 n ) HBT-DONOR-OUT-BUF HBT-DONOR-OUT-U @ ;

: HBT-MAP$ ( ptr u8 n -- ptr u8 n )
   {: a:ptr u:n :}
   u 4 + FS-PATH-CAP > if E-FS-CAPACITY throw then
   a HBT-MAP-PATH u BYTE-COPY
   s" .map" drop HBT-MAP-PATH u + 4 BYTE-COPY
   HBT-MAP-PATH u 4 + ;

: HBT-MAP2$ ( ptr u8 n -- ptr u8 n )
   {: a:ptr u:n :}
   u 4 + FS-PATH-CAP > if E-FS-CAPACITY throw then
   a HBT-MAP-PATH2 u BYTE-COPY
   s" .map" drop HBT-MAP-PATH2 u + 4 BYTE-COPY
   HBT-MAP-PATH2 u 4 + ;

: HBT-OBJ-MAP$ ( -- ptr u8 n )
   HBT-OBJ-MAP-PATH HBT-OBJ-MAP-PATH-U @ ;

: HBT-CODE-ATTR ( ptr u8 n -- )
   {: a:ptr u:n :}
   a u HBT-MAP$ FILE? TTRUE
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
   path pathu FILE-SIZE {: size:n :}
   size HBT-MAP-TEXT-RESERVE
   path pathu 0 HBT-MAP-TEXT BYTE-VIEW size READ-ALL size T=
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

\ A DECLARED CELL THAT HOLDS A CARRIED TABLE'S ADDRESS, stored at BUILD time. The
\ cell travels in the window like any other byte, so before src/habu/aot-closure.f
\ XTD-ROW mapped it the image read the ENGINE's STR-MAX-I64$ out of its own
\ zero-filled mapping and printed nineteen NUL bytes (measured on this fixture's
\ program). The second line is the same table spelled in code, which the closure
\ walk has always mapped to the carried copy: equal lines are the proof that both
\ roads answer with one address. The third is a cached pointer into the program's
\ OWN window data, which the window restores at the address it was captured from
\ and which the map therefore leaves alone.
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
   HBT-MAPL-OUT FILE? TTRUE
   HBT-MAPL-OUT >LEN HBT-RUN-OUT HBT-CAPTURE-CAP >LEN HBT-RUN-ERR HBT-CAPTURE-CAP >LEN
   HBT-TIMEOUT-MS >MS RUN-CAPTURE HBT-CAPTURE>N {: outn:n errn:n rcn:n :}
   rcn 0 <> if HBT-RUN-OUT outn type HBT-RUN-ERR errn type then
   rcn 0 T=
   errn 0 T=
   HBT-RUN-OUT outn HBT-MAPL-EXPECTED$ T$= ;

\ The images at HBT-MAPL-OUT and HBT-MAPL-OUT2 are the same length and differ
\ at no offset.
: HBT-MAPL-SAME-BYTES ( -- )
   HBT-MAPL-OUT2 FILE-SIZE  HBT-MAPL-OUT FILE-SIZE T=
   HBT-MAPL-OUT FILE-SIZE MEM-ALLOC-64K-SPAN {: abuf:ptr acap:n :}
   HBT-MAPL-OUT abuf acap READ-ALL {: au:n :}
   HBT-MAPL-OUT2 FILE-SIZE MEM-ALLOC-64K-SPAN {: bbuf:ptr bcap:n :}
   HBT-MAPL-OUT2 bbuf bcap READ-ALL {: bu:n :}
   abuf au bbuf bu HBT-DIFF-AT -1 T= ;

\ The last build's artifact and the object it stored or hit, removed, so the
\ next build of the same source is a link: the resolver misses when the
\ indexed object is gone, and the link rewrites the index and both maps.
: HBT-FORGET-BUILD ( -- )
   HBB-ARTIFACT$ HBT-REMOVE-FILE?
   HBB-OBJECT-PATH$ HBT-REMOVE-FILE? ;

\ ... and TWO LINKS OF IT ARE THE SAME BYTES. This is the equality the mapped
\ cell broke: three stripped builds of Tender's server differed in the middle
\ bytes of eight window cells and nowhere else. With the first link's artifact
\ and object in the cache the second build is no link at all - the cache
\ answers it with a copy of the first, and equal bytes would prove nothing
\ about the linker - so HBT-FORGET-BUILD removes them, and the maker having
\ run is asserted, not assumed. The second link stays for
\ HBT-STRIPPED-OBJECT-RELINK.
: HBT-STRIPPED-SAME-TWICE ( -- )
   HBT-MAPL-SRC HBT-MAPL-SRC$ WRITE-ALL
   HBT-MAPL-OUT HBT-REMOVE-FILE?
   HBT-MAPL-OUT2 HBT-REMOVE-FILE?
   HBT-MAPL-SRC HBT-MAPL-OUT HBT-HBB-PREPARE-AOT HBT-HBB-BUILD-OUT
   HBT-RUN-MAPL
   HBT-MAPL-OUT HBT-CODE-MAPPED
   HBT-MAPL-ATTR
   HBT-MAPL-SRC HBT-MAPL-OUT HBT-HBB-PREPARE-AOT HBT-HBB-BUILD-OUT
   HBB-ARTIFACT-HIT @ 0 <> TTRUE
   HBT-MAPL-OUT HBT-CODE-MAPPED
   HBB-ARTIFACT$ HBT-MAP$ HBT-REMOVE-FILE?
   HBT-MAPL-SRC HBT-MAPL-OUT HBT-HBB-PREPARE-AOT HBT-HBB-BUILD-OUT
   HBB-ARTIFACT-HIT @ 0= TTRUE
   HBB-OBJECT-HIT @ 0 <> TTRUE
   HBT-MAPL-OUT HBT-CODE-MAPPED
   HBB-ARTIFACT$ HBT-MAP$ s" stale" WRITE-ALL
   HBT-MAPL-SRC HBT-MAPL-OUT HBT-HBB-PREPARE-AOT HBT-HBB-BUILD-OUT
   HBB-ARTIFACT-HIT @ 0= TTRUE
   HBB-OBJECT-HIT @ 0 <> TTRUE
   HBT-MAPL-OUT HBT-CODE-MAPPED
   HBT-FORGET-BUILD
   HBT-MAPL-SRC HBT-MAPL-OUT2 HBT-HBB-PREPARE-AOT HBT-HBB-BUILD-OUT
   HBB-MAKER-RUN @ 0 <> TTRUE
   HBT-MAPL-OUT2 HBT-CODE-MAPPED
   HBT-MAPL-SAME-BYTES
   HBT-MAPL-OUT HBT-REMOVE-FILE? ;

\ ... and AN OBJECT HIT WRITES THE BYTES THE MAKER WROTE. The second link above
\ is a fresh build: its maker wrote HBT-MAPL-OUT2 and the build stored the
\ object text in the cache. With its artifact entry gone, the same
\ source misses the artifact and hits the object, and this process relinks it
\ through tools/object-image.f OBJIMG:WRITE. One source, target and engine are
\ one image whichever cache path writes it. A relink signed under an identifier
\ of its own (`hb-obj`, where the maker signs `hb-prog`) was one byte shorter,
\ differing in the code signature and the two header fields that size it.
: HBT-STRIPPED-OBJECT-RENAME-FAILS ( -- )
   HBT-ROOT s" object-blocked" HBT-DONOR-OUT-BUF HBT-DONOR-OUT-U HBT-PATH!
   HBT-DONOR-OUT$ MAKE-DIR
   HBT-DONOR-OUT$ HBT-MAP$ s" previous map" WRITE-ALL
   HBT-MAPL-SRC HBT-DONOR-OUT$ HBT-HBB-PREPARE-AOT
   [: HBT-HBB-BUILD-OUT ;] E-BUILD-INSTALL TTHROWSQ
   HBT-DONOR-OUT$ DIR? TTRUE
   HBT-DONOR-OUT$ HBT-MAP$ {: path:ptr pathu:n :}
   path pathu FILE-SIZE {: size:n :}
   size HBT-MAP-TEXT-RESERVE
   path pathu 0 HBT-MAP-TEXT BYTE-VIEW size READ-ALL size T=
   0 HBT-MAP-TEXT BYTE-VIEW size s" previous map" T$=
   path pathu REMOVE-FILE
   HBT-DONOR-OUT$ REMOVE-DIR ;

: HBT-STRIPPED-OBJECT-RELINK ( -- )
   HBT-REMOVE-ARTIFACT
   \ A refused object-hit publication must leave the prior map untouched.
   HBT-STRIPPED-OBJECT-RENAME-FAILS
   HBT-MAPL-SRC HBT-MAPL-OUT HBT-HBB-PREPARE-AOT HBT-HBB-BUILD-OUT
   HBB-OBJECT-HIT @ 0 <> TTRUE
   HBB-MAKER-RUN @ 0= TTRUE
   HBT-MAPL-OUT HBT-CODE-MAPPED
   HBB-OBJECT-PATH$ HBT-MAP2$ HBT-OBJ-MAP-PATH HBT-OBJ-MAP-PATH-U HBT-COPY!
   HBT-REMOVE-ARTIFACT
   HBT-MAPL-SAME-BYTES
   HBT-MAPL-OUT HBT-REMOVE-FILE?
   HBT-MAPL-OUT2 HBT-REMOVE-FILE? ;

\ A cached object and a map from another valid image must miss the object
\ cache. The maker then publishes a new executable and matching sidecar.
: HBT-STRIPPED-OBJECT-MAP-CORRUPTION ( -- )
   HBT-MAPL-SRC HBT-MAPL-OUT HBT-HBB-PREPARE-AOT
   HBT-PTRC-OUT HBT-MAP$ HBT-OBJ-MAP$ COPY-FILE-STREAM
   HBT-HBB-BUILD-OUT
   HBB-OBJECT-HIT @ 0= TTRUE
   HBB-MAKER-RUN @ 0<> TTRUE
   HBT-MAPL-OUT HBT-CODE-MAPPED ;

\ The donor's matching map changes the attribution of the same executable.
\ Build once without it, then restore it: the second build must select a new
\ cache entry and produce rows attributed to their baked source files.
: HBT-DONOR-MAP-KEY ( -- )
   HBT-ROOT s" donor" HBT-DONOR-BUF HBT-DONOR-U HBT-PATH!
   HBT-ROOT s" donor-out" HBT-DONOR-OUT-BUF HBT-DONOR-OUT-U HBT-PATH!
   HBT-LINKER HBT-DONOR$ COPY-FILE-STREAM
   HBT-DONOR$ CHMOD-X
   HBT-MAPL-SRC HBT-MAPL-SRC$ WRITE-ALL
   HBT-MAPL-SRC HBT-DONOR-OUT$ HBT-HBB-PREPARE-AOT
   HBT-DONOR$ HBT-ENGINE!
   HBT-HBB-BUILD-OUT
   HBB-MAKER-RUN @ 0<> TTRUE
   HBT-LINKER HBT-MAP$ HBT-DONOR$ HBT-MAP2$ COPY-FILE-STREAM
   HBT-MAPL-SRC HBT-DONOR-OUT$ HBT-HBB-PREPARE-AOT
   HBT-DONOR$ HBT-ENGINE!
   HBT-HBB-BUILD-OUT
   HBB-ARTIFACT-HIT @ 0= TTRUE
   HBB-MAKER-RUN @ 0<> TTRUE
   HBT-DONOR-OUT$ HBT-CODE-MAPPED ;

\ ... and a DECLARED CELL that holds a carried table's address is mapped to the
\ carried copy, so the image reads the bytes it ships. The three pinned lines are
\ the cached pointer, the same table spelled in code and a cached pointer into
\ the program's own window data (src/habu/aot-closure.f XTD-ROW).
: HBT-STRIPPED-CACHED-CARRIED ( -- )
   HBT-PTRC-SRC HBT-PTRC-SRC$ WRITE-ALL
   HBT-PTRC-OUT HBT-REMOVE-FILE?
   HBT-PTRC-SRC HBT-PTRC-OUT HBT-HBB-PREPARE-AOT HBT-HBB-BUILD-OUT
   HBT-PTRC-OUT FILE? TTRUE
   \ The carried DATA table needs no baked word.
   HBT-PTRC-OUT HBT-CODE-ATTR
   HBT-STRIPPED-OBJECT-MAP-CORRUPTION
   HBT-MAPL-OUT2 HBT-MAP$ HBT-PTRC-OUT HBT-MAP2$ COPY-FILE-STREAM
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
   HBT-PTRU-SRC HBT-PTRU-SRC$ WRITE-ALL
   HBT-PTRU-SRC HBT-RUN-MAKER {: uout:n uerr:n urc:n :}
   urc 70 T=
   HBB-ERR-BUF uerr s" declared data cell holds an engine address outside the restored span" CONTAINS? TTRUE
   HBB-ERR-BUF uerr s" word=PTRU-CACHED" CONTAINS? TTRUE ;

\ Public so the driver below runs it with the package CLOSED: the subtests
\ drive real builds, which resolve names in whatever package scope is open.
public
: HBT-STRIPPED-CACHE-MAIN ( -- )
   T-RESET
   \ Baked attribution needs the map bound to the actual donor binary. The
   \ preloaded linker snapshot has different capture coordinates and no map.
   AOT-BUILDER-ENGINE:ENSURE ENGINE-CANDIDATE:PATH$ HBT-KEYED!
   HBT-PREPARE
   HBT-STRIPPED-SAME-TWICE
   HBT-STRIPPED-OBJECT-RELINK
   HBT-DONOR-MAP-KEY
   HBT-STRIPPED-CACHED-CARRIED
   HBT-STRIPPED-CACHED-UNOWNED
   CLEANUP-RUN
   HBT-ROOT EXISTS? TFALSE
   T-REPORT
   s" hb-build-stripped-cache-test: ok" type cr ;

;package

HB-BUILD-CLI:HBT-STRIPPED-CACHE-MAIN
