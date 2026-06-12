\ hbi.f — driver: emit the stdin-program engine. The output binary reads its
\ program from stdin at startup (echo '1 2 + .' | bin/hbi); on a tty it runs
\ the baked repl.f instead, which turns it into the interactive REPL.
\ Swapped in for stage2.f by `srclist.sh hbi` (see tools/build.sh).

\ output path — the single knob; tools/build.sh owns the artifact
: HBI-OUT s" hbi-got" TMP-PATH ;

\ the REPL bootstrap + token stepper baked as the engine's LSRC (paths are
\ repo-root relative; build.sh cd's there before running the maker)
: REPL-SRC s" src/habu/repl.f" ;
: STEP-SRC s" src/habu/stepper.f" ;
variable HB  variable HL  variable HFD  variable HRD
$10000 constant HMAX

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
   REPL-SRC PATH0 RD-1
   STEP-SRC PATH0 RD-1
   HL @ 2 > 0= IF s" hbi: repl/stepper sources missing" 74 die THEN
   HL @ HMAX = IF s" hbi: sources exceed buffer" 74 die THEN ;

: GO
   READ-REPL
   1 STDIN? !
   HB @ HL @ EMIT-FORTH
   BUILD-IMAGE
   s" hbi" SET-SIGID  CODESIG2
   HBI-OUT PATH0  1537 493 open  dup MBUF MLEN @ write drop  close ;
GO
