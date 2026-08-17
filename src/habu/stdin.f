\ stdin.f — internal driver: emit the stdin-program engine used while building
\ the single installed `bin/hb`. The output reads its program from stdin; on a
\ tty it runs the baked repl.f instead, which turns it into the interactive REPL.
\ Swapped in for stage2.f by `bin/hb --load lib/argv.f tools/srclist.f -- stdin`
\ (see tools/build-fixpoint.f).

\ output path — the single knob; the build-fixpoint driver owns the artifact
: STDIN-OUT ( -- ptr u8 n ) s" hb-stdin-got" TMP-PATH ;

\ the REPL + token stepper + breakpoints baked as the engine's LSRC (paths are
\ repo-root relative; run the maker from the repo root)
: REPL-SRC ( -- ptr u8 n ) s" src/habu/repl.f" ;
: WATCH-SRC ( -- ptr u8 n ) s" src/habu/debug-watch.f" ;
: STEP-SRC ( -- ptr u8 n ) s" src/habu/stepper.f" ;
: DBG-SRC ( -- ptr u8 n )  s" src/habu/debug.f" ;
variable HB  variable HL  variable HFD  variable HRD
$20000 constant HMAX
\ HB exposes the raw baked-source buffer cell.
\ Retirement: habu-builder-trust-rows-c5d41af6.
: HB@ ( -- ptr u8 ) HB @ ;
s" HB@" s" -- ptr u8" TRUST

: H+ ( ptr u8 n -- ) {: a:ptr u :}
   HL @ u + HMAX > IF s" hb: repl sources exceed buffer" 74 die THEN
   0 BEGIN dup u < WHILE
      a over + c@  HB@ HL @ + c!
      HL @ 1 + HL !  1 +
   REPEAT drop ;

: HNL ( -- )
   HL @ 1 + HMAX > IF s" hb: repl sources exceed buffer" 74 die THEN
   10 HB@ HL @ + c!  HL @ 1 + HL ! ;

: HLINE ( a u -- )  H+ HNL ;

: RD-1 ( z -- )
   0 0 open HFD !
   HFD @ 0 < IF s" hb: cannot open repl source" 74 die THEN
   BEGIN                                                 \ read() may return short
     HFD @  HB@ HL @ +  HMAX HL @ -  read HRD !
     HRD @ 0 >
   WHILE  HL @ HRD @ + HL !  REPEAT
   HFD @ close
   HRD @ 0 < IF s" hb: repl source read failed" 74 die THEN
   HNL ;

: HFILE ( ptr u8 n -- ) PATH0 RD-1 ;

: HUNKNOWN-TARGET ( -- )
   s" hb: unknown target" 76 die ;

: READ-REPL-TARGET ( -- )
   HB-TARGET-LINUX? IF
      s" src/os/linux/repl-term.f" HFILE
   ELSE HB-TARGET-MACOS? IF
      s" src/os/macos/repl-term.f" HFILE
   ELSE
      HUNKNOWN-TARGET
   THEN
   THEN ;

: READ-REPL ( -- )
   here HB !  HMAX allot  0 HL !
   READ-REPL-TARGET
   REPL-SRC PATH0 RD-1
   WATCH-SRC PATH0 RD-1
   STEP-SRC PATH0 RD-1
   DBG-SRC PATH0 RD-1
   HL @ 2 > 0= IF s" hb: repl/stepper sources missing" 74 die THEN
   HL @ HMAX = IF s" hb: sources exceed buffer" 74 die THEN ;

\ AOT-REPL M2: compile the REPL/token-stepper/breakpoint debugger IN THE METABUILD
\ HOST and capture it (code blob + dict records + call/DATA/CODE relocation tables)
\ so bin/hb seeds it at boot as CODE -- no re-parse, no embedded REPL source. The
\ image writer's EMIT-DICT bakes the emit-builder #PL list (the cold prefix), NOT
\ the host dictionary, so the host-compiled REPL words live ONLY in the AOT blob;
\ EM-SEED-AOT copies the blob, registers the records, name-relocates calls, and
\ relocates DATA + CODE (quotation) literals into bin/hb. The install-tail
\ (INSTALL/BPW-INSTALL/S-INSTALL, the top-level calls at the file tails) becomes
\ the boot-run list: EM-SEED-AOT LFINDs + calls each after the seed, so the engine
\ installs the REPL with ZERO baked source. stage2/maker/snap use other drivers,
\ so their AOT buffers stay empty and EM-SEED-AOT skips.
\ WHAT THE CAPTURED SET IS FOR EVERY OTHER BOOT. The seed runs at the end of the
\ engine prefix on every boot, not only the tty one (dot
\ habu-decide-arm-the-5234727b), so every name compiled here is in the dictionary
\ a piped or `--load` program sees. Adding a file to the four above therefore adds
\ its global names to the engine's contract, and a spelling that some batch
\ program already defines at global scope makes that program die `duplicate
\ definition`. Check a new name against the tree before capturing it.
\ Dynamic host evaluation is source-dependent and cannot carry a static effect.
\ Retirement: habu-builder-trust-rows-c5d41af6.
TRUSTED: EVAL-HOST ( ptr u8 n -- ) evaluate ;    \ compile a source buffer in the host dict
package STDIN-DRIVER
public

\ THE ARTIFACT THIS EMIT BAKES, and the engine whose key it has to carry. Empty
\ is the CAPTURE HOST - today's bin/hb shape, the engine a build with no chain to
\ merge emits - and a declared artifact is the PRODUCT. That is the only thing
\ the two emissions differ by: no mode flag, no second prefix.
\ THE BUILD DRIVER OWNS BOTH PATHS. docs/forth.md is explicit that a build source
\ must not read them out of a stale environment, so they arrive as a call the
\ driver splices into the source it generates (tools/build-fixpoint.f
\ BF-EMIT-STDIN-RUN-SOURCE takes the driver file, which is how the fixtures
\ already reach this driver).
\ THE PRODUCER KEY IS TWO INDEPENDENT READINGS OF ONE FACT: the capture stamped
\ the SHA-256 of the binary it was running into the artifact, and this hashes the
\ binary it is told produced it. A mismatch is the reader's refusal.
$100 constant APATH-CAP           \ src/habu/aot-ident.f's per-path cap, same reason
create ART-P APATH-CAP allot   variable ART-U
create ENG-P APATH-CAP allot   variable ENG-U
create PROD 32 allot

: PATH-COPY ( ptr u8 n ptr u8 -- ) {: a:ptr u:n d:ptr :}
   u APATH-CAP > IF s" hb: artifact path exceeds the driver's buffer" 74 die THEN
   u 0 ?do a i + c@  d i + c!  loop ;

\ ( artifact-path engine-path -- ): the artifact to merge and the engine that
\ produced it. Called with two empty spans, this emit is the capture host.
: ARTIFACT! ( ptr u8 n ptr u8 n -- ) {: a:ptr au:n e:ptr eu:n :}
   a au ART-P PATH-COPY  au ART-U !
   e eu ENG-P PATH-COPY  eu ENG-U ! ;

private

\ THE EMIT'S DATA BASE IS THE DRIVER'S, NOT ITS PARAMETERS'. CAPTURE-REPL latches
\ `here` as the window's DATA base, and an interpret-mode `s"` ALLOTS its bytes
\ there - so a parameter spliced in as a TOP-LEVEL string literal moves that base,
\ every literal into the window, and the residue pad with it. Measured before the
\ fix: two builds of one tree differing only in the length of the artifact path
\ wrote engines differing in 12081 bytes. A spliced parameter therefore belongs
\ inside a colon body, where its literal compiles into code below the window's own
\ base and the seed's canonical CODE-B0 absorbs it (tools/aot-chain-bake.f).
\ DP-MARK is the last thing this file does, so whatever the build driver appends
\ runs between the mark and RUN, and RUN refuses by name if the cursor moved. The
\ mark is mandatory: unmarked, DP0 is 0 and no boot can match it.
variable DP0

: ?DP ( -- )
   here DP0 @ = IF exit THEN
   s" hb: the DATA cursor moved after the driver marked it; this emit would not reproduce"
   74 die ;

\ The window's four coordinates are AOT-ARM's, latched by the two words that
\ name the moments. The widened re-captures in test/aot-wid-build.f start their
\ windows where this one started its own and read the same cells.
: CAPTURE-REPL ( -- )
   READ-REPL                                     \ REPL sources -> HB scratch buffer
   AOT-ARM:WINDOW-OPEN                            \ the engine declines to inline pre-window chains from here on
   AOT-ARM:R0 @ AOT-ARM:D0 @ AOT-CAPTURE:PRELUDE-MARK  \ no prelude: this host compiles only what the target's prefix carries
   HB@ HL @ EVAL-HOST                             \ compile the REPL in the host dictionary
   AOT-ARM:WINDOW-CLOSE
   AOT-ARM:WINDOW$ AOT-CAPTURE:CAPTURE
   s" INSTALL" AOT-CAPTURE:BOOTRUN+                \ repl.f    -> REPL read hook + termios save
   s" BPW-INSTALL" AOT-CAPTURE:BOOTRUN+            \ debug-watch.f -> watch table init
   s" S-INSTALL" AOT-CAPTURE:BOOTRUN+ ;            \ stepper.f -> stepper read hook

\ Append the declared artifact to what CAPTURE-REPL just captured, in the
\ coordinates of that capture, so EMIT-AOT-SEED still bakes one of everything.
\ A declared artifact is mandatory once declared: an engine this build cannot key
\ or read is a refusal, never a quietly unseeded product.
: MERGE-ARTIFACT ( -- )
   ART-U @ 0= IF exit THEN
   ENG-U @ 0= IF
      s" hb: an artifact was declared with no engine to key it against" 74 die
   THEN
   ENG-P ENG-U @ PROD SHA256-FILE 0 <> IF
      s" hb: cannot hash the engine that produced the artifact" 74 die
   THEN
   PROD ART-P ART-U @ AOT-FILE:MERGE ;

;package

package STDIN-DRIVER
public

\ Latch the DATA cursor the capture window will start from.
: DP-MARK ( -- ) here DP0 ! ;

: RUN ( -- )
   ?DP
   CAPTURE-REPL
   MERGE-ARTIFACT
   0 0= STDIN? !
   HB@ 0 ENGINE-EMIT:FORTH                        \ empty LSRC: the REPL is seeded, not re-parsed
   s" hb" STDIN-OUT DRV-EMIT-IMAGE
   DRV-EXIT-OK ;

;package

STDIN-DRIVER:DP-MARK
STDIN-DRIVER:RUN
