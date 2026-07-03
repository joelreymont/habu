\ stdin.f — internal driver: emit the stdin-program engine used while building
\ the single installed `bin/hb`. The output reads its program from stdin; on a
\ tty it runs the baked repl.f instead, which turns it into the interactive REPL.
\ Swapped in for stage2.f by `bin/hb --load tools/argv.f tools/srclist.f -- stdin`
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

: HB-FIELD ( -- ptr ptr u8 )
   HB 0 ptr-field ;

: HB@ ( -- ptr u8 )
   HB-FIELD @ ;

: HB! ( ptr u8 -- )
   HB-FIELD ! ;

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
   here 0 STRUCT-BYTE+ HB!  HMAX allot  0 HL !
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
TRUSTED: EVAL-HOST ( ptr u8 n -- ) evaluate ;    \ compile a source buffer in the host dict
variable REPL-B0  variable REPL-R0  variable REPL-D0
variable REPL-B1  variable REPL-R1  variable REPL-D1
: CAPTURE-REPL ( -- )
   READ-REPL                                     \ REPL sources -> HB scratch buffer
   cp@ REPL-B0 !  ndict@ REPL-R0 !  here REPL-D0 !
   HB@ HL @ EVAL-HOST                             \ compile the REPL in the host dictionary
   cp@ REPL-B1 !  ndict@ REPL-R1 !  here REPL-D1 !
   REPL-B0 @ REPL-B1 @  REPL-R0 @ REPL-R1 @  REPL-D0 @ REPL-D1 @  ACAP-CAPTURE
   s" INSTALL" ACAP-BOOTRUN+                      \ repl.f    -> REPL read hook + termios save
   s" BPW-INSTALL" ACAP-BOOTRUN+                  \ debug-watch.f -> watch table init
   s" S-INSTALL" ACAP-BOOTRUN+ ;                  \ stepper.f -> stepper read hook

: GO ( -- )
   CAPTURE-REPL
   0 0= STDIN? !
   HB@ 0 EMIT-FORTH                               \ empty LSRC: the REPL is seeded, not re-parsed
   s" hb" STDIN-OUT DRV-EMIT-IMAGE
   DRV-EXIT-OK ;
GO
