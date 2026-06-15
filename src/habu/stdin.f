\ stdin.f — internal driver: emit the stdin-program engine used while building
\ the single installed `bin/hb`. The output reads its program from stdin; on a
\ tty it runs the baked repl.f instead, which turns it into the interactive REPL.
\ Swapped in for stage2.f by `srclist.sh stdin` (see tools/build.sh).

\ output path — the single knob; tools/build.sh owns the temporary artifact
: STDIN-OUT s" hb-stdin-got" TMP-PATH ;

\ the REPL bootstrap + token stepper + breakpoints baked as the engine's LSRC (paths are
\ repo-root relative; build.sh cd's there before running the maker)
: REPL-SRC s" src/habu/repl.f" ;
: STEP-SRC s" src/habu/stepper.f" ;
: DBG-SRC  s" src/habu/debug.f" ;
variable HB  variable HL  variable HFD  variable HRD
$10000 constant HMAX

: H+ {: a u :} ( a u -- )
   HL @ u + HMAX > IF s" hb: repl sources exceed buffer" 74 die THEN
   0 BEGIN dup u < WHILE
      a over + c@  HB @ HL @ + c!
      HL @ 1 + HL !  1 +
   REPEAT drop ;

: HNL ( -- )
   HL @ 1 + HMAX > IF s" hb: repl sources exceed buffer" 74 die THEN
   10 HB @ HL @ + c!  HL @ 1 + HL ! ;

: HLINE ( a u -- )  H+ HNL ;

: RD-1 ( z -- )
   0 0 open HFD !
   BEGIN                                                 \ read() may return short
     HFD @  HB @ HL @ +  HMAX HL @ -  read HRD !
     HRD @ 0 >
   WHILE  HL @ HRD @ + HL !  REPEAT
   HFD @ close
   10 HB @ HL @ + c!  HL @ 1 + HL ! ;

: READ-REPL
   here HB !  HMAX allot  0 HL !
   s" 0 set-check" HLINE
   REPL-SRC PATH0 RD-1
   STEP-SRC PATH0 RD-1
   DBG-SRC PATH0 RD-1
   s" : HB-CHECK-HOOK CHECK! ; ' HB-CHECK-HOOK set-check" HLINE
   HL @ 2 > 0= IF s" hb: repl/stepper sources missing" 74 die THEN
   HL @ HMAX = IF s" hb: sources exceed buffer" 74 die THEN ;

: GO
   READ-REPL
   1 STDIN? !
   HB @ HL @ EMIT-FORTH
   BUILD-IMAGE
   s" hb" SET-SIGID  CODESIG2
   STDIN-OUT PATH0  1537 493 open  dup MBUF MLEN @ write drop  close ;
GO
