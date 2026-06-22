\ drive-foreign-live.f - live model-run boundary for foreign drivers.
\
\ Load after bench/llm/drive-foreign-lib.f and bench/llm/model-run.f.

: DFG-MODEL-ERROR ( -- )
   DS-DIAG-PATH$ MRUN-ERR$ WRITE-ALL
   MRUN-RC @ 0= if DS-DIAG-PATH$ s" model parse failed" WRITE-ALL then
   1 DS-DIAG-COUNT !
   s" error" DFG-LR-OUTCOME ;

: DFG-RUN-MODEL-ROUND ( -- )
   DS-PROMPT-PATH$ DS-PROMPT$ WRITE-ALL
   DS-PROMPT$ MRUN-RUN
   MRUN-OUT$ DS-RAW-PATH$ 2swap WRITE-ALL
   DS-TOKENS @ MRUN-TOKENS @ + DS-TOKENS !
   MRUN-RC @ 0= 0= if DFG-MODEL-ERROR exit then
   MRUN-TEXT$ DFG-EVALUATE-TEXT ;

: DFG-RUN-MODEL ( -- )
   DFG-PREPARE
   DFG-STATE-RESET
   DFG-WALL-SNAPSHOT
   begin DFG-ROUND @ DFG-MAX-ROUNDS < while
      DFG-NEXT-ROUND
      DFG-RUN-MODEL-ROUND
      DFG-DONE? if exit then
      DFG-ROUND @ DFG-MAX-ROUNDS >= if exit then
      DFG-ADD-FEEDBACK
   repeat ;

: DFG-MAIN ( -- )
   DFG-CONFIG
   DFG-RUN-MODEL
   LR-EMIT
   CLEANUP-RUN ;
