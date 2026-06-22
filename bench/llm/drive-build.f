\ drive-build.f - CLI wrapper for native stdlib build benchmark driver.
\
\ Load after bench/llm/drive-stdlib-live.f and bench/llm/drive-build-lib.f.

: DB-MODEL-ERROR ( -- )
   DS-MODEL-ERROR
   s" habu-stdlib-build" LR-ARM! ;

: DB-RUN-MODEL ( -- )
   DB-PREPARE
   DS-PROMPT$ MRUN-RUN
   MRUN-OUT$ DS-RAW-PATH$ 2swap WRITE-ALL
   MRUN-TOKENS @ DS-TOKENS !
   MRUN-RC @ 0= 0= if DB-MODEL-ERROR exit then
   MRUN-TEXT$ DB-EVALUATE-TEXT ;

: DB-USAGE ( -- )
   s" usage: bench/llm/drive-build.f <id> <name> <sig> <category> <tests> <spec> [maxr]" E-DS-USAGE die ;

: DB-CONFIG ( -- )
   SCRIPT-ARGC 6 < if DB-USAGE then
   SCRIPT-ARGC 7 > if DB-USAGE then
   0 SCRIPT-ARGV$ DS-PARSE-U DS-ID !
   1 SCRIPT-ARGV$ DS-NAME!
   2 SCRIPT-ARGV$ DS-SIG!
   3 SCRIPT-ARGV$ DS-CATEGORY!
   4 SCRIPT-ARGV$ DS-TESTS!
   5 SCRIPT-ARGV$ DS-SPEC!
   SCRIPT-ARGC 6 > if 6 SCRIPT-ARGV$ DS-PARSE-U else 1 then DS-MAX-REPAIRS !
   DS-DEFAULTS
   s" MODEL_REGISTRY" s" bench/llm/models.tsv" DS-ENV$ MR-LOAD
   s" MODEL_ID" GETENV MR-REQUIRE ;

: DB-MAIN ( -- )
   DB-CONFIG
   DB-RUN-MODEL
   LR-EMIT
   CLEANUP-RUN ;

DB-MAIN
