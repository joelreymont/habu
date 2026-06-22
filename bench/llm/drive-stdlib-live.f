\ drive-stdlib-live.f - live model-run boundary for stdlib driver.
\
\ Load after bench/llm/drive-stdlib-lib.f and bench/llm/model-run.f.

: DS-MODEL-ERROR ( -- )
   DS-DIAG-PATH$ MRUN-ERR$ WRITE-ALL
   MRUN-RC @ 0= if DS-DIAG-PATH$ s" model parse failed" WRITE-ALL then
   1 DS-DIAG-COUNT !
   DS-CONFIG-LR-COMMON
   s" error" LR-OUTCOME!
   s" rejected" LR-FIRST-CHECKER!
   0 LR-FIRST-PASS !
   0 LR-FIRST-TESTS !
   0 LR-TESTS-PASSED !
   1 LR-DIAG-COUNT ! ;

: DS-RUN-MODEL ( -- )
   DS-PREPARE
   DS-PROMPT$ MRUN-RUN
   MRUN-OUT$ DS-RAW-PATH$ 2swap WRITE-ALL
   MRUN-TOKENS @ DS-TOKENS !
   MRUN-RC @ 0= 0= if DS-MODEL-ERROR exit then
   MRUN-TEXT$ DS-EVALUATE-TEXT ;

: DS-MAIN ( -- )
   DS-CONFIG
   DS-RUN-MODEL
   LR-EMIT
   CLEANUP-RUN ;
