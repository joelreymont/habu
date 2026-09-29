\ hb-build-stripped-cache-test.f - checked fixture for tools/hb-build-lib.f:
\ two links of one source under separate cache roots are the same bytes, and a
\ declared cell caching a baked table's address is mapped to the carried copy,
\ or refused when no claim names the table. tools/hb-build-test-lib.f lists
\ the other hb-build rows.
\ Run: bin/hb --load tools/hb-build-stripped-cache-test.f

require tools/hb-build-test-lib.f

\ The shared fixture's words are private words of the library's package, so
\ this row reopens it the way tools/hb-build-test-lib.f does.
package HB-BUILD-CLI

: HBT-MAPL-OUT2 ( -- ptr u8 n )
   HBT-MAPL-OUT2-BUF HBT-MAPL-OUT2-U @ ;

: HBT-PTRC-SRC ( -- ptr u8 n )
   HBT-PTRC-SRC-BUF HBT-PTRC-SRC-U @ ;

: HBT-PTRC-OUT ( -- ptr u8 n )
   HBT-PTRC-OUT-BUF HBT-PTRC-OUT-U @ ;

: HBT-PTRU-SRC ( -- ptr u8 n )
   HBT-PTRU-SRC-BUF HBT-PTRU-SRC-U @ ;

: HBT-PTRU-OUT ( -- ptr u8 n )
   HBT-PTRU-OUT-BUF HBT-PTRU-OUT-U @ ;

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
\ cell that holds it. TPB is the same table tools/hb-build-stripped-cells-test.f
\ HBT-STRIPPED-UNCARRIED-TABLE reaches through TMP-PATH-COPY-SRC, so the two
\ cases differ only in how the address got into the image: spelled in code, or
\ stored in a declared cell at build time.
: HBT-PTRU-SRC$ ( -- ptr u8 n )
   SB-RESET
   S\" PERSISTED-PTR-VARIABLE PTRU-CACHED\nTPB PTRU-CACHED !\n" SB-APPEND
   S\" : MAIN ( -- ) PTRU-CACHED @ 1 type cr ;\n" SB-APPEND
   SB$ ;

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

\ Public so the driver below runs it with the package CLOSED: the subtests
\ drive real builds, which resolve names in whatever package scope is open.
public
: HBT-STRIPPED-CACHE-MAIN ( -- )
   T-RESET
   HBT-PREPARE
   HBT-STRIPPED-SAME-TWICE
   HBT-STRIPPED-CACHED-CARRIED
   HBT-STRIPPED-CACHED-UNOWNED
   CLEANUP-RUN
   HBT-ROOT EXISTS? TFALSE
   T-REPORT
   s" hb-build-stripped-cache-test: ok" type cr ;

;package

HB-BUILD-CLI:HBT-STRIPPED-CACHE-MAIN
