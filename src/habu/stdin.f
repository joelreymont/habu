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

: READ-REPL-PREFIX ( -- )
   s" src/core/util.f" HFILE
   HB-TARGET-LINUX? IF
      s" src/os/linux/target.f" HFILE
   ELSE HB-TARGET-MACOS? IF
      s" src/os/macos/target.f" HFILE
   ELSE
      HUNKNOWN-TARGET
   THEN
   THEN
   s" src/core/checker.f" HFILE
   s" src/core/render.f" HFILE
   s" src/habu/layout.f" HFILE
   HB-TARGET-LINUX? IF
      s" src/os/linux/env.f" HFILE
   ELSE HB-TARGET-MACOS? IF
      s" src/os/macos/env.f" HFILE
   ELSE
      HUNKNOWN-TARGET
   THEN
   THEN
   s" src/core/check-hook.f" HFILE
   s" src/core/roles.f" HFILE
   s" src/core/combinators.f" HFILE ;

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
   s" 0 set-check" HLINE
   READ-REPL-PREFIX
   s" 0 set-check" HLINE
   READ-REPL-TARGET
   REPL-SRC PATH0 RD-1
   WATCH-SRC PATH0 RD-1
   STEP-SRC PATH0 RD-1
   DBG-SRC PATH0 RD-1
   s" TRUSTED: HB-CHECK-HOOK ( ptr u8 n -- n ) CHECK! dup -1 <> if 70 throw then ; ' HB-CHECK-HOOK set-check" HLINE
   HL @ 2 > 0= IF s" hb: repl/stepper sources missing" 74 die THEN
   HL @ HMAX = IF s" hb: sources exceed buffer" 74 die THEN ;

: GO ( -- )
   READ-REPL
   0 0= STDIN? !
   HB@ HL @ EMIT-FORTH
   ASM-CODE BUILD-IMAGE
   s" hb" SET-SIGID  CODESIG2
   STDIN-OUT DRV-WRITE-IMAGE
   DRV-EXIT-OK ;
GO
