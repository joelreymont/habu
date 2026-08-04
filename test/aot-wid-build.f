\ aot-wid-build.f - build a protected-WID variant of the stdin engine.
\
\ Run as `bin/hb --load test/aot-wid-build.f` with HB_TMP pointing at a private
\ directory; on success it writes an `hb-pwid` engine into that directory. The
\ variant is identical to the shipped `bin/hb` except that its ahead-of-time
\ (AOT) section carries a protected-WID bitmap with two extra wordlist ids set
\ (300 and 8000) on top of whatever the metabuild host itself protects. Nothing
\ in production is touched: the extra bits are set ONLY in this throwaway
\ variant, through the same capture-buffer word the real metabuild uses
\ (aot-capture.f ACAP-PWID-SET), so the shipped engine bakes exactly the band
\ its own build produced.
\
\ How the bits are injected without editing production source: the stdin
\ metabuild driver src/habu/stdin.f ends with a single top-level
\ `STDIN-DRIVER:RUN` call. This builder reads that file, verifies it still ends
\ with that call (and dies with a clear message if the tail ever drifts), drops
\ the call, and appends the same sequence written out at interpret level, with
\ the protected-WID work spliced in between capturing the REPL and emitting the
\ image. CAPTURE-REPL and the capture words are package-private, so the appended
\ text reopens STDIN-DRIVER and AOT-CAPTURE to reach them by their bare names -
\ it never adds a public tail to either. The rest of the build reuses
\ tools/build-fixpoint.f exactly as the normal stdin build does.
\
\ Four modes, selected by environment so one builder serves every case its
\ companion test/aot-wid-suite.f needs:
\
\   (default)          bake the two fixture ids, after checking the capture's own
\                      shape contract on the live host: the band carries
\                      PROT-REG-TAG, the captured buffer is a bit-for-bit image
\                      of the live band, and the table-era conversion leg accepts
\                      an empty registry without setting a bit.
\   HABU_PWID_OOR=N    hand N to ACAP-PWID-SET instead. The capture must refuse
\                      any id at or above the bitmap's bound rather than write
\                      outside the band, so the build dies named and no engine
\                      appears.
\   HABU_PWID_LEGACY_N=N  hand N to ACAP-PWID-LEGACY as a table-era row count.
\                      A count outside [0, PROT-WID-LEGACY-MAX] is not a legacy
\                      registry at all, so the build dies named rather than
\                      walking the read loop out of the band.
\   HABU_AOT_SPAN=N    overwrite the captured AOT DATA span (the sibling
\                      test/aot-data-span-forge.f forge; see SPAN-FORGE-LINE).
\
\ Its companion test/aot-wid-suite.f spawns this builder in a child process and
\ then probes the resulting hb-pwid to prove the protected-WID bitmap is restored
\ at engine startup, before any batch program runs.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/vector.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/process-fork.f
require lib/source.f
require lib/build.f
require lib/codesign.f
require lib/content-key.f
require lib/date.f
require tools/build-fixpoint.f

package AOT-WID-BUILD

: STDIN-SRC-PATH ( -- ptr u8 n ) s" src/habu/stdin.f" ;

$4000 constant SRC-CAP             \ src/habu/stdin.f is ~4 KB
$4000 constant DRV-CAP             \ driver = source (minus GO) + injection

create SRC-BUF SRC-CAP allot   variable SRC-U
create DRV-BUF DRV-CAP allot   variable DRV-U
create DRV-PATH-BUF FS-PATH-CAP allot   variable DRV-PATH-U

: DRV-PATH$ ( -- ptr u8 n )
   DRV-PATH-BUF DRV-PATH-U @ ;

: DRV-PATH! ( -- )                 \ <HB_TMP>/pwid-driver.f
   BF-TMP$ s" pwid-driver.f" DRV-PATH-BUF JOIN-PATH DRV-PATH-U ! ;

: READ-STDIN-SRC ( -- )
   STDIN-SRC-PATH SRC-BUF SRC-CAP READ-ALL SRC-U !
   SRC-U @ 0 <= if s" aot-wid-build: cannot read src/habu/stdin.f" BF-BUILD-RC die then ;

: WS? ( n -- bool ) {: c:n :}      \ whitespace: space, tab, cr, lf
   c 32 = c 9 = or c 13 = or c 10 = or ;

: SRC-LAST ( -- n )                \ index of last non-whitespace byte, or -1
   SRC-U @ 1-
   begin dup 0 >= while
      dup SRC-BUF + c@ WS? 0= if exit then
      1-
   repeat ;

: TAIL-BAD ( -- )
   s" aot-wid-build: src/habu/stdin.f no longer ends with STDIN-DRIVER:RUN" BF-BUILD-RC die ;

: RUN-TAIL$ ( -- ptr u8 n ) s" STDIN-DRIVER:RUN" ;

\ Length of stdin.f to keep: everything up to (not including) the trailing
\ `STDIN-DRIVER:RUN`. Fail closed if the file does not end with that token.
: RUN-KEEP ( -- n )
   SRC-LAST {: l:n :}
   RUN-TAIL$ {: t:ptr tu:n :}
   l 1+ tu < if TAIL-BAD then
   l 1+ tu - SRC-BUF + tu t tu STR= 0= if TAIL-BAD then
   l 1+ tu > if l tu - SRC-BUF + c@ WS? 0= if TAIL-BAD then then
   l 1+ tu - ;

: DRV-RESET ( -- )
   0 DRV-U ! ;

: DRV+ ( ptr u8 n -- ) {: a:ptr u:n :}
   DRV-U @ u + DRV-CAP > if s" aot-wid-build: driver buffer overflow" BF-BUILD-RC die then
   a DRV-BUF DRV-U @ + u BYTE-COPY
   DRV-U @ u + DRV-U ! ;

: DRV-NL ( -- )
   10 DRV-BUF DRV-U @ + c!
   DRV-U @ 1+ DRV-U ! ;

: DRV-LINE ( ptr u8 n -- ) DRV+ DRV-NL ;

\ Optional AOT DATA-span forge (dot habu-guard-aot-data-49de2ee6). The reserve in
\ EM-AOT-RELOC-DATA advances DP by the baked LAOTDATASIZE span read straight from
\ the image; test/aot-data-span-forge.f probes that guard by baking a forged span.
\ When HABU_AOT_SPAN is set to a decimal, that value overwrites the captured span
\ AFTER CAPTURE-REPL and BEFORE ENGINE-EMIT:FORTH, so LAOTDATASIZE carries the forged value
\ (the forge test passes 2*DATA-SIZE, unambiguously past the seed headroom). No env
\ leaves the real capture untouched (the plain protected-WID build path).
: SPAN-FORGE-LINE ( -- )
   s" HABU_AOT_SPAN" GETENV {: v:ptr vu:n :}
   vu 0 > if
      v vu DRV+  s"  AOT-DATA-SIZE !" DRV-LINE
   then ;

\ --- the fixture contract -----------------------------------------------------
\ These two ids are what test/aot-wid-suite.f probes for in the built engine, so
\ any drift here turns that suite red (it is self-checking) - keep the two in
\ step. 300 is above the u8 ceiling the old u32-row table existed to clear; 8000
\ sits high in the band, far above any wordlist a boot allocates, so the suite can
\ also assert that its NEIGHBOUR 8001 came back unprotected - a restore that
\ smeared or mis-shifted the band would set it.
: FIXTURE-A$ ( -- ptr u8 n ) s" 300" ;
: FIXTURE-B$ ( -- ptr u8 n ) s" 8000" ;

\ --- shape and conversion checks emitted into the driver ----------------------
\ These run in the METABUILD HOST, where the live band and the capture buffer both
\ exist, and they use the very words the real capture uses. They are the only
\ place the capture's format contract can be checked against a live band. Those
\ words are private to package AOT-CAPTURE, so the definitions are emitted inside
\ a reopened block of it and stay private too.
: SHAPE-CHECK-DEF ( -- )
   s" package AOT-CAPTURE" DRV-LINE
   s" : PWID-SHAPE-CHECK ( -- )" DRV-LINE
   s"    ACAP-PWID-TAG@ PROT-REG-TAG <> if" DRV-LINE
   S\"       s\" aot-wid-build: metabuild host band carries no bitmap tag\" 74 die then" DRV-LINE
   s"    PROT-BITS-BYTES 0 ?do" DRV-LINE
   s"       AOT-LIVE-DATA PROT-BITS-OFF + i + AOT-A>U8 c@  AOT-PWID-BUF@ i + c@ <> if" DRV-LINE
   S\"          s\" aot-wid-build: capture is not a bit-for-bit image of the live band\" 74 die" DRV-LINE
   s"       then" DRV-LINE
   s"    loop ;" DRV-LINE ;

\ The table-era leg converts u32 rows read from a FIXED live address into bits, so
\ it cannot be handed a fabricated table: on a bitmap-era host that address holds
\ the bitmap, and reading it as rows yields whatever ids the host's own bits happen
\ to spell. What CAN be checked here without forging live memory is the empty
\ table-era registry - count 0, which is exactly the shape every shipped
\ table-era engine carried, and the shape the real changeover fed this leg. It must
\ be accepted (not mistaken for an unknown lineage) and must leave no bit set.
\ Its opposite, a count that is not a legacy registry at all, is the
\ HABU_PWID_LEGACY_N refusal build.
\
\ NOT covered here, and recorded as a gap rather than faked: the row->bit mapping
\ for a NON-empty table. It needs a table-era host, which no longer exists; its
\ only evidence is the one-time changeover measurement (an old-table host and a
\ new-bitmap host both building bin/hb to the same bytes). That is a reason to
\ retire the leg, not to keep it untested - dot habu-retire-the-legacy-31ad57bc.
: LEGACY-CHECK-DEF ( -- )
   s" : PWID-LEGACY-CHECK ( -- )" DRV-LINE
   s"    ACAP-PWID-CLEAR" DRV-LINE
   s"    0 ACAP-PWID-LEGACY" DRV-LINE
   s"    ACAP-PWID-COUNT 0 <> if" DRV-LINE
   S\"       s\" aot-wid-build: empty legacy registry set a bit\" 74 die then" DRV-LINE
   s"    ACAP-PWID-CAPTURE ;" DRV-LINE
   s" ;package" DRV-LINE ;

\ --- the body between CAPTURE-REPL and the image emit --------------------------
: OOR-ENV$ ( -- ptr u8 n )       s" HABU_PWID_OOR" GETENV ;
: LEGACY-ENV$ ( -- ptr u8 n )    s" HABU_PWID_LEGACY_N" GETENV ;

: REFUSE-BODY ( ptr u8 n ptr u8 n -- ) {: v:ptr vu:n w:ptr wu:n :}
   v vu DRV+  s"  " DRV+  w wu DRV-LINE ;

: FIXTURE-BODY ( -- )
   s" PWID-SHAPE-CHECK" DRV-LINE
   s" PWID-LEGACY-CHECK" DRV-LINE
   FIXTURE-A$ DRV+  s"  ACAP-PWID-SET" DRV-LINE
   FIXTURE-B$ DRV+  s"  ACAP-PWID-SET" DRV-LINE ;

: PWID-BODY ( -- )
   OOR-ENV$ {: o:ptr ou:n :}
   ou 0 > if o ou s" ACAP-PWID-SET" REFUSE-BODY exit then
   LEGACY-ENV$ {: l:ptr lu:n :}
   lu 0 > if l lu s" ACAP-PWID-LEGACY" REFUSE-BODY exit then
   FIXTURE-BODY ;

\ CAPTURE-REPL is private to package STDIN-DRIVER, so the appended text reopens
\ that package, ticks it, closes the package and executes the token - reaching the
\ real word by its real name without publishing anything.
: CAPTURE-REPL-LINES ( -- )
   s" package STDIN-DRIVER" DRV-LINE
   s" ' CAPTURE-REPL" DRV-LINE
   s" ;package" DRV-LINE
   s" execute" DRV-LINE ;

\ Append the checks the fixture build needs, then stdin.f's own terminal sequence
\ with the protected-WID work spliced in after CAPTURE-REPL.
: CHECKS-WANTED? ( -- bool )       \ only the plain fixture build carries them
   OOR-ENV$ nip 0 =  LEGACY-ENV$ nip 0 =  and ;

: INJECT ( -- )
   CHECKS-WANTED? if
      SHAPE-CHECK-DEF
      LEGACY-CHECK-DEF
   then
   CAPTURE-REPL-LINES
   s" package AOT-CAPTURE" DRV-LINE
   PWID-BODY
   s" ;package" DRV-LINE
   SPAN-FORGE-LINE
   s" 0 0= STDIN? !" DRV-LINE
   s" HB@ 0 ENGINE-EMIT:FORTH" DRV-LINE
   S\" s\" hb\" STDIN-OUT DRV-EMIT-IMAGE" DRV-LINE
   s" DRV-EXIT-OK" DRV-LINE ;

: GEN-DRIVER ( -- )
   DRV-PATH!
   READ-STDIN-SRC
   RUN-KEEP {: keep:n :}
   DRV-RESET
   SRC-BUF keep DRV+                \ stdin.f minus its terminal driver call
   INJECT                           \ ... plus the bitmap-working fixture
   DRV-PATH$ DRV-BUF DRV-U @ WRITE-ALL ;

: EMIT-PWID-STDIN ( -- )
   s" stage2-src" DRV-PATH$ BF-EMIT-STDIN-RUN-SOURCE ;

\ Build the maker (hb-pwid-mk) from the stage engine, then run the maker to emit
\ the final variant hb-pwid (mirrors BF-BUILD-STDIN-FROM-STAGE with our driver).
: RUN-MAKER ( -- )
   s" stage2-got" s" hb-pwid-mk" BF-RENAME-TMP
   s" hb-pwid-mk" BF-CHMOD-X-TMP
   s" hb-stdin-got" BF-REMOVE-TMP
   s" hb-pwid-mk" BF-RUN-ENV-TMP BF-RC0
   s" hb-stdin-got" BF-EXPECT
   s" hb-stdin-got" s" hb-pwid" BF-RENAME-TMP
   s" hb-pwid" BF-CHMOD-X-TMP
   s" hb-pwid" BF-CODESIGN-VERIFY-TMP ;

public

: BUILD ( -- )
   BF-STAGE-FIXPOINT               \ stage engine at fixpoint (reused if bin/hb already converged)
   GEN-DRIVER
   EMIT-PWID-STDIN
   BF-CERTIFY-STDIN
   BF-RUN-STAGE                    \ stage compiles the injected stdin source -> maker
   RUN-MAKER
   s" aot-wid-build: hb-pwid ready" type cr ;

;package

AOT-WID-BUILD:BUILD
