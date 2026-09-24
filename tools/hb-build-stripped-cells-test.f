\ hb-build-stripped-cells-test.f - checked fixture for tools/hb-build-lib.f:
\ the DATA cells a stripped image carries - the mapped and declared cells, the
\ late buffer, the uncarried table, the cached claims - and the CLI's large
\ source. The rest of the stripped window is tools/hb-build-stripped-test.f
\ and the build, CLI and AOT groups are tools/hb-build-test.f; each is a gate
\ row of its own, because one row running all three took 353-355 s of the
\ gate's 360 s child timeout.
\ Run: bin/hb --load tools/hb-build-stripped-cells-test.f

require tools/hb-build-test-lib.f

using BUILD-FIXPOINT                     \ the build tmp root and engine override

\ The shared fixture's words are private words of the library's package, so
\ this half reopens it the way tools/hb-build-test-lib.f does.
package HB-BUILD-CLI

4096 constant HBT-LARGE-CHUNK-U
$40000 1 - constant HBT-LARGE-INPUT-U

create HBT-LARGE-CHUNK HBT-LARGE-CHUNK-U allot

: HBT-LARGE-AOT-SRC$ ( -- ptr u8 n )
   s" variable SLOT 9 SLOT ! : MAIN ( -- ) SLOT @ . cr ;" ;

: HBT-TABLE-SRC ( -- ptr u8 n )
   HBT-TABLE-SRC-BUF HBT-TABLE-SRC-U @ ;

: HBT-TABLE-OUT ( -- ptr u8 n )
   HBT-TABLE-OUT-BUF HBT-TABLE-OUT-U @ ;

: HBT-PTRC-SRC ( -- ptr u8 n )
   HBT-PTRC-SRC-BUF HBT-PTRC-SRC-U @ ;

: HBT-PTRC-OUT ( -- ptr u8 n )
   HBT-PTRC-OUT-BUF HBT-PTRC-OUT-U @ ;

: HBT-PTRU-SRC ( -- ptr u8 n )
   HBT-PTRU-SRC-BUF HBT-PTRU-SRC-U @ ;

: HBT-PTRU-OUT ( -- ptr u8 n )
   HBT-PTRU-OUT-BUF HBT-PTRU-OUT-U @ ;

: HBT-MAPC-SRC ( -- ptr u8 n )
   HBT-MAPC-SRC-BUF HBT-MAPC-SRC-U @ ;

: HBT-MAPC-OUT ( -- ptr u8 n )
   HBT-MAPC-OUT-BUF HBT-MAPC-OUT-U @ ;

: HBT-MAPD-SRC ( -- ptr u8 n )
   HBT-MAPD-SRC-BUF HBT-MAPD-SRC-U @ ;

: HBT-MAPD-OUT ( -- ptr u8 n )
   HBT-MAPD-OUT-BUF HBT-MAPD-OUT-U @ ;

: HBT-MAPL-SRC ( -- ptr u8 n )
   HBT-MAPL-SRC-BUF HBT-MAPL-SRC-U @ ;

: HBT-MAPL-OUT ( -- ptr u8 n )
   HBT-MAPL-OUT-BUF HBT-MAPL-OUT-U @ ;

: HBT-MAPL-OUT2 ( -- ptr u8 n )
   HBT-MAPL-OUT2-BUF HBT-MAPL-OUT2-U @ ;

: HBT-LITB-SRC ( -- ptr u8 n )
   HBT-LITB-SRC-BUF HBT-LITB-SRC-U @ ;

: HBT-LITB-OUT ( -- ptr u8 n )
   HBT-LITB-OUT-BUF HBT-LITB-OUT-U @ ;

: HBT-LITC-SRC ( -- ptr u8 n )
   HBT-LITC-SRC-BUF HBT-LITC-SRC-U @ ;

: HBT-LITC-OUT ( -- ptr u8 n )
   HBT-LITC-OUT-BUF HBT-LITC-OUT-U @ ;

\ ... and a baked `create` TABLE that no claim names is refused exactly as
\ before. TPB is src/os/env-base.f's TMP-PATH buffer, the same shape as the
\ carried digit tables and as the claimed path scratch, and TMP-PATH-COPY-SRC
\ spells its address before any other engine data - so a table travels because
\ the list NAMES it, never because it is a table and never because a table in
\ the same file, or in the file next to it, is claimed.
: HBT-TABLE-SRC$ ( -- ptr u8 n )
   S\" : MAIN ( -- ) s\" x\" TMP-PATH-COPY-SRC ;\n" ;

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
   S\" require lib/string.f\ncreate PTRC-OWN 111 c, 107 c,\n" SB-APPEND
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
\ cell that holds it. TPB is the same table HBT-STRIPPED-UNCARRIED-TABLE reaches
\ through TMP-PATH-COPY-SRC, so the two cases differ only in how the address got
\ into the image: spelled in code, or stored in a declared cell at build time.
: HBT-PTRU-SRC$ ( -- ptr u8 n )
   SB-RESET
   S\" PERSISTED-PTR-VARIABLE PTRU-CACHED\nTPB PTRU-CACHED !\n" SB-APPEND
   S\" : MAIN ( -- ) PTRU-CACHED @ 1 type cr ;\n" SB-APPEND
   SB$ ;

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

\ ... while the SAME allocation inside MAIN links, runs and prints. The cell
\ the image carries is the zero it was captured with and the pointer is taken
\ in the new process, which is what the refusal's suggestion names. The printed
\ byte is read back out of the run-time buffer, so the pinned line proves the
\ image reached its own mapping and not merely that it exited zero.
: HBT-MAPL-SRC$ ( -- ptr u8 n )
   SB-RESET
   S\" require lib/memory.f\nPTR-VARIABLE BUF\n: MAIN ( -- )\n" SB-APPEND
   S\"    MEM-ALLOC-64K drop BUF !\n   BUF @ {: p:ptr :}\n" SB-APPEND
   S\"    65 p c!  p 1 type cr ;\n" SB-APPEND
   SB$ ;

: HBT-MAPL-EXPECTED$ ( -- ptr u8 n )
   S\" A\n" ;

\ A STRING LITERAL'S BODY IS DECLARED TEXT, and the span scan skips its cells by
\ that declaration the way it skips a declared address cell by the engine's
\ address-cell table: src/compiler/native/string.f writes one row per interned
\ body when it places the bytes, and src/habu/aot-closure.f CELL-LITERAL? reads
\ those rows - never the bytes - for every cell src/habu/aot-lib.f SCAN-DATA-CELL
\ is about to refuse. Tender's stripped server is the case: `s" {pm}"` and the
\ 01 byte after it, read as one aligned cell, spelled an address inside the
\ emitted code span and the build refused the word that owns the body (AUTH
\ ROLE-LITERAL$, dot habu-link-int-cells-45328a4d).
\ The child derives its bytes from a live code address; a fixed Linux address
\ does not establish this property in a Mach-O builder with ASLR.
: HBT-LITB-SRC$ ( -- ptr u8 n )
   S\" require test/stripped-literal-subject.f\n' STRIPPED-LITERAL:ANCHOR STRIPPED-LITERAL:BODY-SOURCE evaluate\n" ;

\ MAIN verifies the length and bytes before printing this witness.
: HBT-LITB-EXPECTED$ ( -- ptr u8 n )
   S\" literal\n" ;

\ ... and THE SAME EIGHT BYTES IN A `create` CELL are refused as before. This is
\ the control that keeps the row above honest: raw storage carries no declaration
\ about its contents, so the value is still read as the code pointer it spells,
\ and a build that stopped refusing it would say the pattern had drifted out of
\ the emitted code span - in which case the row above proves nothing and this one
\ goes red instead of both passing on a value nothing classifies.
: HBT-LITC-SRC$ ( -- ptr u8 n )
   S\" require test/stripped-literal-subject.f\n' STRIPPED-LITERAL:ANCHOR STRIPPED-LITERAL:RAW\n: MAIN ( -- ) STRIPPED-LITERAL:VALUE . cr ;\n" ;

: HBT-LARGE-CHUNK! ( -- )
   HBT-LARGE-CHUNK-U 0 ?do 32 HBT-LARGE-CHUNK i + c! loop ;

: HBT-WRITE-LARGE ( ptr u8 n ptr u8 n -- ) {: path:ptr pathu:n src:ptr srcu:n :}
   path pathu src srcu WRITE-ALL
   HBT-LARGE-INPUT-U srcu - {: spaces:n :}
   spaces HBT-LARGE-CHUNK-U / 0 ?do
      path pathu HBT-LARGE-CHUNK HBT-LARGE-CHUNK-U APPEND-FILE
   loop
   spaces HBT-LARGE-CHUNK-U mod {: rem:n :}
   rem 0 > if path pathu HBT-LARGE-CHUNK rem APPEND-FILE then ;

\ The same argv with a BUILD CACHE OF ITS OWN. Two builds of one source under
\ one cache root are one link and one copy: the artifact cache answers the
\ second from the first, so equal bytes would prove nothing about the linker.
: HBT-ARGV-BASE-CACHE ( ptr u8 n -- ) {: a:ptr u :}
   PROC-ARGV-RESET
   PROC-ENV-RESET
   s" HB_TMP" >LEN HBT-TMP >LEN PROC-ENV+
   s" HABU_BUILD_CACHE" >LEN a u >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   s" --load"  >LEN PROC-ARGV+
   s" tools/hb-build.f"  >LEN PROC-ARGV+
   s" --"  >LEN PROC-ARGV+ ;

\ The first byte two spans differ at, or -1 for identical. A pinned -1 names the
\ offset on failure instead of printing two images into the capture.
: HBT-DIFF-AT ( ptr u8 n ptr u8 n -- n ) {: a:ptr au:n b:ptr bu:n :}
   au bu min 0 ?do
      a i + c@  b i + c@ <> if i unloop exit then
   loop
   au bu <> if au bu min exit then
   -1 ;

\ A PERSISTENT CELL HOLDING A POINTER INTO MEMORY THE BUILD MAPPED is refused by
\ the cell that holds it. The value is left out of the pin because it is an mmap
\ address: it differs in every build, which is the fault itself.
: HBT-STRIPPED-MAPPED-CELL ( -- )
   HBT-MAPC-SRC HBT-MAPC-SRC$ WRITE-ALL
   HBT-MAPC-OUT HBT-REMOVE-FILE?
   HBT-ARGV-BASE
   HBT-MAPC-SRC >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   HBT-MAPC-OUT >LEN PROC-ARGV+
   HBT-RUN-HB-BUILD {: cout:n cerr:n crc:n :}
   crc 0 <> TTRUE
   HBT-ERR cerr s" holds a pointer into memory the build mapped" CONTAINS? TTRUE
   HBT-ERR cerr s" word=BUF" CONTAINS? TTRUE
   HBT-ERR cerr s" data-off=" CONTAINS? TTRUE
   HBT-ERR cerr s" allocate at run time" CONTAINS? TTRUE
   HBT-MAPC-OUT FILE? TFALSE ;

\ ... and so is one in a DECLARED cell, at the other site and by its own name.
: HBT-STRIPPED-MAPPED-DECLARED ( -- )
   HBT-MAPD-SRC HBT-MAPD-SRC$ WRITE-ALL
   HBT-MAPD-OUT HBT-REMOVE-FILE?
   HBT-ARGV-BASE
   HBT-MAPD-SRC >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   HBT-MAPD-OUT >LEN PROC-ARGV+
   HBT-RUN-HB-BUILD {: dout:n derr:n drc:n :}
   drc 0 <> TTRUE
   HBT-ERR derr s" holds a pointer into memory the build mapped" CONTAINS? TTRUE
   HBT-ERR derr s" word=PBUF" CONTAINS? TTRUE
   HBT-MAPD-OUT FILE? TFALSE ;

\ ... while the allocation moved into MAIN links, runs and prints its own byte.
: HBT-STRIPPED-MAPPED-LATE ( -- )
   HBT-MAPL-SRC HBT-MAPL-SRC$ WRITE-ALL
   HBT-MAPL-OUT HBT-REMOVE-FILE?
   HBT-ARGV-BASE
   HBT-MAPL-SRC >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   HBT-MAPL-OUT >LEN PROC-ARGV+
   HBT-RUN-HB-BUILD {: lout:n lerr:n lrc:n :}
   lrc 0 <> if HBT-OUT lout type HBT-ERR lerr type then
   lrc 0 T=
   HBT-OUT lout s" hb-build OK" CONTAINS? TTRUE
   HBT-MAPL-OUT FILE? TTRUE
   HBT-MAPL-OUT >LEN HBT-RUN-OUT HBT-CAPTURE-CAP >LEN HBT-RUN-ERR HBT-CAPTURE-CAP >LEN
   HBT-TIMEOUT-MS >MS RUN-CAPTURE HBT-CAPTURE>N {: outn:n errn:n rcn:n :}
   rcn 0 <> if HBT-RUN-OUT outn type HBT-RUN-ERR errn type then
   rcn 0 T=
   errn 0 T=
   HBT-RUN-OUT outn HBT-MAPL-EXPECTED$ T$= ;

\ TWO LINKS OF ONE SOURCE ARE THE SAME BYTES. The second build has a cache root
\ of its own, so the answer is a second link and not a copy of the first. This
\ is the equality the mapped cell broke: three stripped builds of Tender's
\ server differed in the middle bytes of eight window cells and nowhere else.
: HBT-STRIPPED-SAME-TWICE ( -- )
   HBT-MAPL-SRC HBT-MAPL-SRC$ WRITE-ALL
   HBT-MAPL-OUT HBT-REMOVE-FILE?
   HBT-MAPL-OUT2 HBT-REMOVE-FILE?
   HBT-ARGV-BASE
   HBT-MAPL-SRC >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   HBT-MAPL-OUT >LEN PROC-ARGV+
   HBT-RUN-HB-BUILD {: aout:n aerr:n arc:n :}
   arc 0 <> if HBT-OUT aout type HBT-ERR aerr type then
   arc 0 T=
   HBT-TWICE-CACHE HBT-ARGV-BASE-CACHE
   HBT-MAPL-SRC >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   HBT-MAPL-OUT2 >LEN PROC-ARGV+
   HBT-RUN-HB-BUILD {: bout:n berr:n brc:n :}
   brc 0 <> if HBT-OUT bout type HBT-ERR berr type then
   brc 0 T=
   HBT-MAPL-OUT2 FILE-SIZE  HBT-MAPL-OUT FILE-SIZE T=
   HBT-MAPL-OUT FILE-SIZE MEM-ALLOC-64K-SPAN {: abuf:ptr acap:n :}
   HBT-MAPL-OUT abuf acap READ-ALL {: au:n :}
   HBT-MAPL-OUT2 FILE-SIZE MEM-ALLOC-64K-SPAN {: bbuf:ptr bcap:n :}
   HBT-MAPL-OUT2 bbuf bcap READ-ALL {: bu:n :}
   abuf au bbuf bu HBT-DIFF-AT -1 T=
   HBT-MAPL-OUT HBT-REMOVE-FILE?
   HBT-MAPL-OUT2 HBT-REMOVE-FILE? ;

\ ... while a baked create table on no list is refused however much it looks like
\ the carried ones.
: HBT-STRIPPED-UNCARRIED-TABLE ( -- )
   HBT-TABLE-SRC HBT-TABLE-SRC$ WRITE-ALL
   HBT-TABLE-OUT HBT-REMOVE-FILE?
   HBT-ARGV-BASE
   HBT-TABLE-SRC >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   HBT-TABLE-OUT >LEN PROC-ARGV+
   HBT-RUN-HB-BUILD {: tout:n terr:n trc:n :}
   trc 0 <> TTRUE
   HBT-ERR terr s" outside the restored span" CONTAINS? TTRUE
   HBT-ERR terr s" caller=TMP-PATH-COPY-SRC" CONTAINS? TTRUE
   HBT-ERR terr s" target=TPB" CONTAINS? TTRUE
   HBT-TABLE-OUT FILE? TFALSE ;

\ ... and a DECLARED CELL that holds a carried table's address is mapped to the
\ carried copy, so the image reads the bytes it ships. The three pinned lines are
\ the cached pointer, the same table spelled in code and a cached pointer into
\ the program's own window data (src/habu/aot-closure.f XTD-ROW).
: HBT-STRIPPED-CACHED-CARRIED ( -- )
   HBT-PTRC-SRC HBT-PTRC-SRC$ WRITE-ALL
   HBT-PTRC-OUT HBT-REMOVE-FILE?
   HBT-ARGV-BASE
   HBT-PTRC-SRC >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   HBT-PTRC-OUT >LEN PROC-ARGV+
   HBT-RUN-HB-BUILD {: bout:n berr:n brc:n :}
   brc 0 <> if HBT-OUT bout type HBT-ERR berr type then
   brc 0 T=
   HBT-OUT bout s" hb-build OK" CONTAINS? TTRUE
   HBT-PTRC-OUT FILE? TTRUE
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
   HBT-PTRU-OUT HBT-REMOVE-FILE?
   HBT-ARGV-BASE
   HBT-PTRU-SRC >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   HBT-PTRU-OUT >LEN PROC-ARGV+
   HBT-RUN-HB-BUILD {: uout:n uerr:n urc:n :}
   urc 70 T=
   HBT-ERR uerr s" declared data cell holds an engine address outside the restored span" CONTAINS? TTRUE
   HBT-ERR uerr s" word=PTRU-CACHED" CONTAINS? TTRUE
   HBT-PTRU-OUT FILE? TFALSE ;

\ THE CONTROL FIRST, then the body: the refusal is what proves the pattern lands
\ in the emitted code span of a build on this engine, and the link that follows
\ is then a statement about the declaration and not about the value.
: HBT-STRIPPED-LITERAL-BODY ( -- )
   HBT-LITC-SRC HBT-LITC-SRC$ WRITE-ALL
   HBT-LITC-OUT HBT-REMOVE-FILE?
   HBT-ARGV-BASE
   HBT-LITC-SRC >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   HBT-LITC-OUT >LEN PROC-ARGV+
   HBT-RUN-HB-BUILD {: cout:n cerr:n crc:n :}
   crc 70 T=
   HBT-ERR cerr s" holds an undeclared code/dict pointer" CONTAINS? TTRUE
   HBT-ERR cerr s" word=LIT-CELL" CONTAINS? TTRUE
   HBT-LITC-OUT FILE? TFALSE

   HBT-LITB-SRC HBT-LITB-SRC$ WRITE-ALL
   HBT-LITB-OUT HBT-REMOVE-FILE?
   HBT-ARGV-BASE
   HBT-LITB-SRC >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   HBT-LITB-OUT >LEN PROC-ARGV+
   HBT-RUN-HB-BUILD {: bout:n berr:n brc:n :}
   brc 0 <> if HBT-OUT bout type HBT-ERR berr type then
   brc 0 T=
   HBT-OUT bout s" hb-build OK" CONTAINS? TTRUE
   HBT-LITB-OUT FILE? TTRUE
   HBT-LITB-OUT >LEN HBT-RUN-OUT HBT-CAPTURE-CAP >LEN HBT-RUN-ERR HBT-CAPTURE-CAP >LEN
   HBT-TIMEOUT-MS >MS RUN-CAPTURE HBT-CAPTURE>N {: outn:n errn:n rcn:n :}
   rcn 0 <> if HBT-RUN-OUT outn type HBT-RUN-ERR errn type then
   rcn 0 T=
   errn 0 T=
   HBT-RUN-OUT outn HBT-LITB-EXPECTED$ T$=
   HBT-LITB-OUT HBT-REMOVE-FILE? ;

: HBT-CLI-LARGE-SOURCE ( -- )
   HBT-LARGE-CHUNK!
   HBT-REPL-SRC HBT-REPL-SRC$ HBT-WRITE-LARGE
   HBT-REPL-OUT HBT-REMOVE-FILE?
   HBT-ARGV-BASE
   s" --repl" >LEN PROC-ARGV+
   HBT-REPL-SRC >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   HBT-REPL-OUT >LEN PROC-ARGV+
   HBT-RUN-HB-BUILD {: rout:n rerr:n rrc:n :}
   rrc 0 <> if HBT-OUT rout type HBT-ERR rerr type then
   rrc 0 T=
   HBT-OUT rout s" hb-build OK" CONTAINS? TTRUE
   rerr 0 T=
   HBT-TMP BF-TMP!
   s" hb-build-check-src" BF-A$ EXISTS? TFALSE
   s" hb-build-src" BF-A$ EXISTS? TFALSE
   BF-TMP-RESET
   HBT-RUN-REPL

   HBT-AOT-SRC HBT-LARGE-AOT-SRC$ HBT-WRITE-LARGE
   HBT-AOT-OUT HBT-REMOVE-FILE?
   HBT-ARGV-BASE
   HBT-AOT-SRC >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   HBT-AOT-OUT >LEN PROC-ARGV+
   HBT-RUN-HB-BUILD {: aout:n aerr:n arc:n :}
   arc 0 <> if HBT-OUT aout type HBT-ERR aerr type then
   arc 0 T=
   HBT-OUT aout s" hb-build OK" CONTAINS? TTRUE
   aerr 0 T=
   HBT-TMP BF-TMP!
   s" hb-aot-src" BF-A$ EXISTS? TFALSE
   BF-TMP-RESET
   HBT-AOT-OUT FILE? TTRUE
   HBT-AOT-OUT >LEN HBT-RUN-OUT HBT-CAPTURE-CAP >LEN HBT-RUN-ERR HBT-CAPTURE-CAP >LEN
   HBT-TIMEOUT-MS >MS RUN-CAPTURE HBT-CAPTURE>N {: outn:n errn:n rcn:n :}
   rcn 0 T=
   errn 0 T=
   HBT-RUN-OUT outn s" 9" CONTAINS? TTRUE ;

\ Public so the driver below runs it with the package CLOSED: the subtests
\ drive real builds, which resolve names in whatever package scope is open.
public
: HBT-STRIPPED-CELLS-MAIN ( -- )
   T-RESET
   HBT-PREPARE
   HBT-STRIPPED-MAPPED-CELL
   HBT-STRIPPED-MAPPED-DECLARED
   HBT-STRIPPED-MAPPED-LATE
   HBT-STRIPPED-SAME-TWICE
   HBT-STRIPPED-UNCARRIED-TABLE
   HBT-STRIPPED-CACHED-CARRIED
   HBT-STRIPPED-CACHED-UNOWNED
   HBT-STRIPPED-LITERAL-BODY
   HBT-CLI-LARGE-SOURCE
   CLEANUP-RUN
   HBT-ROOT EXISTS? TFALSE
   T-REPORT
   s" hb-build-stripped-cells-test: ok" type cr ;

;package

;using

HB-BUILD-CLI:HBT-STRIPPED-CELLS-MAIN
