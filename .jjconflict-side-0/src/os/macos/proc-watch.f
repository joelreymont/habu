\ proc-watch.f -- macOS exact process-lifetime watch primitive emitter.

\ The ARM64 condition-code names are package A64ASM's public surface
\ (src/arch/arm64/asm.f), imported here rather than qualified at each call so the
\ emitters below keep the bodies they had: this file carries no package, so the
\ ownership gate reports a changed global definition in it.
using A64ASM

: BPROCWATCHOPEN ( -- )            \ ( pid -- fd|-1 )
   LBL LBL LBL {: openbad:label regbad:label done:label :}
   9 G-POP
   SP SP 64 SUBI,
   9 SP 0 STR,
   NR-KQUEUE SYS,
   9 C-CS CSET,  9 openbad CBNZ,
   0 SP 56 STR,
   10 EVFILT-PROC-U16 MOVZ,  11 EV-PROC-FLAGS MOVZ,
   11 11 16 LSLI,  10 10 11 ORR,  10 SP 8 STRW,
   10 NOTE-EXIT LIT64,  10 SP 12 STRW,
   10 0 MOVZ,
   10 SP 16 STR,  10 SP 24 STR,  10 SP 32 STR,  10 SP 40 STR,
   0 SP 56 LDR,  1 SP 0 ADDI,  2 1 MOVZ,
   3 0 MOVZ,  4 0 MOVZ,  5 0 MOVZ,  6 0 MOVZ,
   NR-KEVENT64 SYS,
   9 C-CS CSET,  9 regbad CBNZ,
   0 SP 56 LDR,  done B,
   regbad LBL,
   0 SP 56 LDR,  NR-CLOSE SYS,
   0 0 MOVN,  done B,
   openbad LBL,
   0 0 MOVN,
   done LBL,
   SP SP 64 ADDI,
   0 G-PUSH ;

;using
