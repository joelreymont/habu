\ proc-watch.f -- Linux exact process-lifetime watch primitive emitter.
\ Loaded before habu1.f, so the syscall-result push is inlined with local labels
\ rather than habu1.f's shared SYS-PUSH (same carry-checked -1-on-failure logic).

: BPROCWATCHOPEN ( -- )            \ ( pid -- fd|-1 ) pidfd_open(pid, 0)
   LBL LBL {: ok:label done:label :}
   0 G-POP  1 0 MOVZ,
   NR-PIDFD-OPEN SYS,
   9 C-CS CSET,  9 ok CBZ,
      0 0 MOVN,  done B,
   ok LBL,
   done LBL,
   0 G-PUSH ;
