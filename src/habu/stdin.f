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
variable REPL-B0  variable REPL-R0  variable REPL-D0
variable REPL-B1  variable REPL-R1  variable REPL-D1

package STDIN-DRIVER

: CAPTURE-REPL ( -- )
   READ-REPL                                     \ REPL sources -> HB scratch buffer
   cp@ REPL-B0 !  ndict@ REPL-R0 !  here REPL-D0 !
   REPL-R0 @ REPL-D0 @ AOT-CAPTURE:PRELUDE-MARK   \ no prelude: this host compiles only what the target's prefix carries
   REPL-B0 @ REPL-D0 @ AOT-ARM:OPEN               \ the engine declines to inline pre-window chains from here on
   HB@ HL @ EVAL-HOST                             \ compile the REPL in the host dictionary
   cp@ REPL-B1 !  ndict@ REPL-R1 !  here REPL-D1 !
   REPL-B0 @ REPL-B1 @  REPL-R0 @ REPL-R1 @  REPL-D0 @ REPL-D1 @  AOT-CAPTURE:CAPTURE
   s" INSTALL" AOT-CAPTURE:BOOTRUN+                \ repl.f    -> REPL read hook + termios save
   s" BPW-INSTALL" AOT-CAPTURE:BOOTRUN+            \ debug-watch.f -> watch table init
   s" S-INSTALL" AOT-CAPTURE:BOOTRUN+ ;            \ stepper.f -> stepper read hook

;package

package STDIN-DRIVER
public

: RUN ( -- )
   CAPTURE-REPL
   0 0= STDIN? !
   HB@ 0 ENGINE-EMIT:FORTH                        \ empty LSRC: the REPL is seeded, not re-parsed
   s" hb" STDIN-OUT DRV-EMIT-IMAGE
   DRV-EXIT-OK ;

;package

STDIN-DRIVER:RUN
