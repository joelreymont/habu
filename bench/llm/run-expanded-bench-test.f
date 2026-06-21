\ run-expanded-bench-test.f - focused native tests for expanded benchmark dispatch.
\
\ Load after lib/errors.f, lib/string.f, lib/test.f, lib/fs.f,
\ lib/fs-mutate.f, lib/process.f, lib/process-argv.f, lib/process-env.f,
\ lib/json-write.f, and bench/llm/fixture-text.f.

120000 constant REBT-TIMEOUT-MS
65536 constant REBT-CAP

create REBT-ROOT FS-PATH-CAP allot
create REBT-HB-TMP FS-PATH-CAP allot
create REBT-MODEL-SRC FS-PATH-CAP allot
create REBT-MODEL-BIN FS-PATH-CAP allot
create REBT-MODELS FS-PATH-CAP allot
create REBT-OUT-PATH FS-PATH-CAP allot
create REBT-REPORT FS-PATH-CAP allot
create REBT-OUT REBT-CAP allot
create REBT-ERR REBT-CAP allot
create REBT-FILE REBT-CAP allot

variable REBT-ROOT-U
variable REBT-HB-TMP-U
variable REBT-MODEL-SRC-U
variable REBT-MODEL-BIN-U
variable REBT-MODELS-U
variable REBT-OUT-PATH-U
variable REBT-REPORT-U
variable REBT-FILE-U

: REBT-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u dst:ptr up:ptr :}
   u FS-PATH-CAP > if E-FS-CAPACITY throw then
   a dst u BYTE-COPY
   u up ! ;

: REBT-ROOT$ ( -- ptr u8 n )
   REBT-ROOT REBT-ROOT-U @ ;

: REBT-HB-TMP$ ( -- ptr u8 n )
   REBT-HB-TMP REBT-HB-TMP-U @ ;

: REBT-MODEL-SRC$ ( -- ptr u8 n )
   REBT-MODEL-SRC REBT-MODEL-SRC-U @ ;

: REBT-MODEL-BIN$ ( -- ptr u8 n )
   REBT-MODEL-BIN REBT-MODEL-BIN-U @ ;

: REBT-MODELS$ ( -- ptr u8 n )
   REBT-MODELS REBT-MODELS-U @ ;

: REBT-OUT-PATH$ ( -- ptr u8 n )
   REBT-OUT-PATH REBT-OUT-PATH-U @ ;

: REBT-REPORT$ ( -- ptr u8 n )
   REBT-REPORT REBT-REPORT-U @ ;

: REBT-JOIN! ( ptr u8 n ptr u8 ptr n -- ) {: name:ptr nameu dst:ptr up:ptr :}
   REBT-ROOT$ name nameu dst JOIN-PATH up ! ;

: REBT-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-run-expanded-test" TMPDIR-MKDIR REBT-ROOT REBT-ROOT-U REBT-COPY!
   REBT-ROOT$ CLEANUP-TREE+
   s" hbtmp" REBT-HB-TMP REBT-HB-TMP-U REBT-JOIN!
   s" model.f" REBT-MODEL-SRC REBT-MODEL-SRC-U REBT-JOIN!
   s" model-bin" REBT-MODEL-BIN REBT-MODEL-BIN-U REBT-JOIN!
   s" models.tsv" REBT-MODELS REBT-MODELS-U REBT-JOIN!
   s" run.jsonl" REBT-OUT-PATH REBT-OUT-PATH-U REBT-JOIN!
   s" report.md" REBT-REPORT REBT-REPORT-U REBT-JOIN!
   REBT-HB-TMP$ MAKE-DIR ;

: REBT-MODEL-SOURCE$ ( -- ptr u8 n )
   BFT-RESET
   s" MAIN" s" --" BFT-SOURCE-DEF
   s" : MAIN ( -- ) here drop ;" BFT-SOURCE-S"
   s"  type cr " BFT+
   BFT-SOURCE-END$ ;

: REBT-WRITE-MODEL ( -- )
   REBT-MODEL-SRC$ REBT-MODEL-SOURCE$ WRITE-ALL ;

: REBT-HB-BUILD-LOADS ( -- )
   s" --load" PROC-ARGV+
   s" lib/errors.f" PROC-ARGV+
   s" lib/string.f" PROC-ARGV+
   s" lib/fs.f" PROC-ARGV+
   s" lib/fs-mutate.f" PROC-ARGV+
   s" lib/process.f" PROC-ARGV+
   s" lib/process-argv.f" PROC-ARGV+
   s" lib/process-env.f" PROC-ARGV+
   s" lib/source.f" PROC-ARGV+
   s" lib/build.f" PROC-ARGV+
   s" tools/build-fixpoint.f" PROC-ARGV+
   s" tools/hb-build-lib.f" PROC-ARGV+
   s" tools/hb-build.f" PROC-ARGV+ ;

: REBT-BUILD-MODEL ( -- )
   PROC-ARGV-ENV-RESET
   s" HB_TMP" REBT-HB-TMP$ PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   REBT-HB-BUILD-LOADS
   s" --" PROC-ARGV+
   s" --strict-signatures" PROC-ARGV+
   REBT-MODEL-SRC$ PROC-ARGV+
   s" -o" PROC-ARGV+
   REBT-MODEL-BIN$ PROC-ARGV+
   s" bin/hb" REBT-OUT REBT-CAP REBT-ERR REBT-CAP REBT-TIMEOUT-MS RUN-ARGV-ENV-CAPTURE
   0 T= 0 T= drop ;

: REBT-MODELS-TEXT$ ( -- ptr u8 n )
   BFT-RESET
   s" id" BFT-TSV-CELL
   s" label" BFT-TSV-CELL
   s" command" BFT-TSV-CELL
   s" args" BFT-TSV-CELL
   s" parser" BFT-TSV-CELL
   s" token_fields" BFT-TSV-CELL
   s" timeout_s" BFT-TSV-LAST
   s" aotfix" BFT-TSV-CELL
   s" AOTFixture" BFT-TSV-CELL
   REBT-MODEL-BIN$ BFT-TSV-CELL
   BFT-TSV-BLANK
   s" raw" BFT-TSV-CELL
   BFT-TSV-BLANK
   s" 10" BFT-TSV-LAST
   BFT$ ;

: REBT-WRITE-MODELS ( -- )
   REBT-MODELS$ REBT-MODELS-TEXT$ WRITE-ALL ;

: REBT-RUN-EXPANDED-LOADS ( -- )
   s" --load" PROC-ARGV+
   s" lib/errors.f" PROC-ARGV+
   s" lib/string.f" PROC-ARGV+
   s" lib/fs.f" PROC-ARGV+
   s" lib/process.f" PROC-ARGV+
   s" lib/process-argv.f" PROC-ARGV+
   s" lib/process-env.f" PROC-ARGV+
   s" lib/time.f" PROC-ARGV+
   s" lib/date.f" PROC-ARGV+
   s" lib/argv.f" PROC-ARGV+
   s" bench/llm/manifest.f" PROC-ARGV+
   s" bench/llm/run-expanded-bench.f" PROC-ARGV+ ;

: REBT-RUN-EXPANDED ( -- )
   PROC-ARGV-ENV-RESET
   s" MODEL_REGISTRY" REBT-MODELS$ PROC-ENV+
   s" MODEL_ID" s" aotfix" PROC-ENV+
   s" BENCH_TASK_IDS" s" 69" PROC-ENV+
   s" BENCH_RESULTS" REBT-REPORT$ PROC-ENV+
   s" BENCH_SEED" s" run-expanded-aot-test" PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   REBT-RUN-EXPANDED-LOADS
   s" --" PROC-ARGV+
   s" 1" PROC-ARGV+
   REBT-OUT-PATH$ PROC-ARGV+
   s" bin/hb" REBT-OUT REBT-CAP REBT-ERR REBT-CAP REBT-TIMEOUT-MS RUN-ARGV-ENV-CAPTURE
   0 T= 0 T= drop ;

: REBT-FILE$ ( ptr u8 n -- ptr u8 n )
   REBT-FILE REBT-CAP READ-ALL REBT-FILE-U !
   REBT-FILE REBT-FILE-U @ ;

: REBT-CONTAINS ( ptr u8 n ptr u8 n -- )
   CONTAINS? TTRUE ;

: REBT-ASSERT-JSONL ( -- )
   REBT-OUT-PATH$ REBT-FILE$ {: a:ptr u :}
   a u s" outcome" REBT-CONTAINS
   a u s" reject" REBT-CONTAINS
   a u s" arm" REBT-CONTAINS
   a u s" habu-aot" REBT-CONTAINS
   a u s" aot_unsupported" REBT-CONTAINS
   a u s" repair_class_stats" REBT-CONTAINS
   a u s" E-AOT-UNSUPPORTED" REBT-CONTAINS
   a u s" here" REBT-CONTAINS ;

: REBT-ASSERT-REPORT ( -- )
   REBT-REPORT$ REBT-FILE$ {: a:ptr u :}
   a u s" category aot-unsupported rows=1" REBT-CONTAINS
   a u s" arm habu-aot rows=1" REBT-CONTAINS ;

: REBT-MAIN ( -- )
   T-RESET
   REBT-PREPARE
   REBT-WRITE-MODEL
   REBT-BUILD-MODEL
   REBT-WRITE-MODELS
   REBT-RUN-EXPANDED
   REBT-ASSERT-JSONL
   REBT-ASSERT-REPORT
   CLEANUP-RUN
   T-REPORT
   s" run-expanded-bench-test: ok" type cr ;

REBT-MAIN
