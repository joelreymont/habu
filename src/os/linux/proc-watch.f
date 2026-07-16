\ proc-watch.f -- Linux exact process-lifetime watch primitive emitter.

: BPROCWATCHOPEN ( -- )            \ ( pid -- fd|-1 )
   0 G-POP  1 0 MOVZ,
   NR-PIDFD-OPEN SYS,  SYS-PUSH ;
