\ run-expanded-bench-test.f - focused native tests for expanded benchmark dispatch.
\
\ Load after lib/errors.f, lib/string.f, lib/test.f, lib/fs.f,
\ lib/fs-mutate.f, lib/process.f, lib/process-argv.f, lib/process-env.f,
\ lib/json-write.f, and bench/llm/fixture-text.f.

120000 constant REBT-TIMEOUT-MS
65536 constant REBT-CAP
10 constant REBT-DEC
48 constant REBT-ZERO
32 constant REBT-SPACE
32 constant REBT-NUM-CAP

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
create REBT-NUM REBT-NUM-CAP allot

variable REBT-ROOT-U
variable REBT-HB-TMP-U
variable REBT-MODEL-SRC-U
variable REBT-MODEL-BIN-U
variable REBT-MODELS-U
variable REBT-OUT-PATH-U
variable REBT-REPORT-U
variable REBT-FILE-U
variable REBT-NUM-I

: REBT-FORTH-SHELL$ ( -- ptr u8 n )
   SB-RESET
   s" drive-forth" SB-APPEND
   s" .sh" SB-APPEND
   SB$ ;

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

: REBT-U$ ( n -- ptr u8 n ) {: u :}
   REBT-NUM-CAP REBT-NUM-I !
   u 0= if
      REBT-NUM-I @ 1- REBT-NUM-I !
      REBT-ZERO REBT-NUM REBT-NUM-I @ + c!
      REBT-NUM REBT-NUM-I @ + 1
      exit
   then
   u begin dup 0 > while
      dup REBT-DEC mod REBT-ZERO +
      REBT-NUM-I @ 1- REBT-NUM-I !
      REBT-NUM REBT-NUM-I @ + c!
      REBT-DEC /
   repeat drop
   REBT-NUM REBT-NUM-I @ + REBT-NUM-CAP REBT-NUM-I @ - ;

: REBT-U+ ( n -- )
   REBT-U$ BFT+ ;

: REBT-SOURCE-EMIT-C ( n -- )
   REBT-U+
   s"  emit " BFT+ ;

: REBT-SOURCE-EMIT-BYTES ( ptr u8 n -- ) {: a:ptr u :}
   0 begin dup u < while
      dup a + c@ REBT-SOURCE-EMIT-C
      1+
   repeat drop ;

: REBT-MODEL-SOURCE$ ( ptr u8 n -- ptr u8 n )
   BFT-RESET
   s" MAIN" s" --" BFT-SOURCE-DEF
   REBT-SOURCE-EMIT-BYTES
   s"  cr " BFT+
   BFT-SOURCE-END$ ;

: REBT-AOT-CANDIDATE$ ( -- ptr u8 n )
   s" : MAIN ( -- ) here drop ;" ;

: REBT-FORTH-CANDIDATE$ ( -- ptr u8 n )
   s" : SQUARE ( i64 -- i64 ) dup * ;" ;

: REBT-ARRAY-CANDIDATE$ ( -- ptr u8 n )
   s" : ARR-SUM ( ptr a n -- i64 ) >LEN A-SUM ;" ;

: REBT-JS-CANDIDATE$ ( -- ptr u8 n )
   s" function f(a){ return a.reduce((s,x)=>s+x,0); }" ;

: REBT-PY-CANDIDATE$ ( -- ptr u8 n )
   s" def f(a): return sum(a)" ;

: REBT-RUST-CANDIDATE$ ( -- ptr u8 n )
   s" fn f(a: &[i64]) -> i64 { a.iter().sum() }" ;

: REBT-TS-CANDIDATE$ ( -- ptr u8 n )
   s" function f(a: number[]): number { return a.reduce((s,x)=>s+x,0); }" ;

: REBT-WRITE-MODEL ( ptr u8 n -- ) {: a:ptr u :}
   REBT-MODEL-SRC$ a u REBT-MODEL-SOURCE$ WRITE-ALL ;

: REBT-HB-BUILD-LOADS ( -- )
   s" --load"  >LEN PROC-ARGV+
   s" lib/errors.f"  >LEN PROC-ARGV+
   s" lib/string.f"  >LEN PROC-ARGV+
   s" lib/fs.f"  >LEN PROC-ARGV+
   s" lib/fs-mutate.f"  >LEN PROC-ARGV+
   s" lib/process.f"  >LEN PROC-ARGV+
   s" lib/process-argv.f"  >LEN PROC-ARGV+
   s" lib/process-env.f"  >LEN PROC-ARGV+
   s" lib/source.f"  >LEN PROC-ARGV+
   s" lib/build.f"  >LEN PROC-ARGV+
   s" lib/codesign.f"  >LEN PROC-ARGV+
   s" tools/build-fixpoint.f"  >LEN PROC-ARGV+
   s" tools/hb-build-lib.f"  >LEN PROC-ARGV+
   s" tools/hb-build.f"  >LEN PROC-ARGV+ ;

: REBT-BUILD-MODEL ( -- )
   PROC-ARGV-ENV-RESET
   s" HB_TMP" >LEN REBT-HB-TMP$ >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   REBT-HB-BUILD-LOADS
   s" --"  >LEN PROC-ARGV+
   s" --strict-signatures"  >LEN PROC-ARGV+
   REBT-MODEL-SRC$  >LEN PROC-ARGV+
   s" -o"  >LEN PROC-ARGV+
   REBT-MODEL-BIN$  >LEN PROC-ARGV+
   s" bin/hb" >LEN REBT-OUT REBT-CAP >LEN REBT-ERR REBT-CAP >LEN
   REBT-TIMEOUT-MS >MS RUN-ARGV-ENV-CAPTURE {: outu erru rc :}
   rc RC>N 0 T=
   erru LEN>N 0 T=
   outu drop ;

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

: REBT-BAD-MODELS-TEXT$ ( -- ptr u8 n )
   BFT-RESET
   s" id" BFT-TSV-CELL
   s" label" BFT-TSV-CELL
   s" command" BFT-TSV-CELL
   s" args" BFT-TSV-CELL
   s" parser" BFT-TSV-CELL
   s" token_fields" BFT-TSV-CELL
   s" timeout_s" BFT-TSV-LAST
   s" aotfix" BFT-TSV-CELL
   s" BrokenFixture" BFT-TSV-CELL
   s" /bin/echo" BFT-TSV-CELL
   s" --bad-template" BFT-TSV-CELL
   s" raw" BFT-TSV-CELL
   BFT-TSV-BLANK
   s" 10" BFT-TSV-LAST
   BFT$ ;

: REBT-WRITE-MODELS ( -- )
   REBT-MODELS$ REBT-MODELS-TEXT$ WRITE-ALL ;

: REBT-WRITE-BAD-MODELS ( -- )
   REBT-MODELS$ REBT-BAD-MODELS-TEXT$ WRITE-ALL ;

: REBT-RUN-EXPANDED-LOADS ( -- )
   s" --load"  >LEN PROC-ARGV+
   s" lib/errors.f"  >LEN PROC-ARGV+
   s" lib/string.f"  >LEN PROC-ARGV+
   s" lib/fs.f"  >LEN PROC-ARGV+
   s" lib/process.f"  >LEN PROC-ARGV+
   s" lib/process-argv.f"  >LEN PROC-ARGV+
   s" lib/process-env.f"  >LEN PROC-ARGV+
   s" lib/time.f"  >LEN PROC-ARGV+
   s" lib/date.f"  >LEN PROC-ARGV+
   s" lib/argv.f"  >LEN PROC-ARGV+
   s" bench/llm/manifest.f"  >LEN PROC-ARGV+
   s" bench/llm/run-expanded-bench.f"  >LEN PROC-ARGV+ ;

: REBT-RUN-EXPANDED-START ( ptr u8 n ptr u8 n -- ) {: task:ptr tasku seed:ptr seedu :}
   PROC-ARGV-ENV-RESET
   s" MODEL_REGISTRY" >LEN REBT-MODELS$ >LEN PROC-ENV+
   s" MODEL_ID" >LEN s" aotfix" >LEN PROC-ENV+
   s" BENCH_TASK_IDS" >LEN task tasku >LEN PROC-ENV+
   s" BENCH_RESULTS" >LEN REBT-REPORT$ >LEN PROC-ENV+
   s" BENCH_SEED" >LEN seed seedu >LEN PROC-ENV+ ;

: REBT-RUN-EXPANDED-CAPTURE ( -- n n n )
   PROC-ENV-INHERIT-MISSING
   REBT-RUN-EXPANDED-LOADS
   s" --"  >LEN PROC-ARGV+
   s" 1"  >LEN PROC-ARGV+
   REBT-OUT-PATH$  >LEN PROC-ARGV+
   s" bin/hb" >LEN REBT-OUT REBT-CAP >LEN REBT-ERR REBT-CAP >LEN
   REBT-TIMEOUT-MS >MS RUN-ARGV-ENV-CAPTURE {: outu erru rc :}
   outu LEN>N erru LEN>N rc RC>N ;

: REBT-RUN-EXPANDED-SPAWN ( -- )
   REBT-RUN-EXPANDED-CAPTURE {: outu erru rc :}
   rc 0 T=
   erru 0 T=
   outu drop ;

: REBT-RUN-AOT-EXPANDED ( -- )
   s" 69" s" run-expanded-aot-2026-06-21" REBT-RUN-EXPANDED-START
   REBT-RUN-EXPANDED-SPAWN ;

: REBT-RUN-ARRAY-EXPANDED ( -- )
   s" 46" s" run-expanded-array-2026-06-21" REBT-RUN-EXPANDED-START
   s" BENCH_ARRAY_ARMS" >LEN s" habu-stdlib" >LEN PROC-ENV+
   s" PATH" >LEN REBT-ROOT$ >LEN PROC-ENV+
   REBT-RUN-EXPANDED-SPAWN ;

: REBT-RUN-FOREIGN-ARRAY-EXPANDED ( ptr u8 n ptr u8 n -- ) {: arm:ptr armu seed:ptr seedu :}
   s" 46" seed seedu REBT-RUN-EXPANDED-START
   s" BENCH_ARRAY_ARMS" >LEN arm armu >LEN PROC-ENV+
   REBT-RUN-EXPANDED-SPAWN ;

: REBT-RUN-FORTH-EXPANDED ( -- )
   s" 1" s" run-expanded-forth-2026-06-21" REBT-RUN-EXPANDED-START
   s" BENCH_FORTH_MODES" >LEN s" repair raw blind" >LEN PROC-ENV+
   REBT-RUN-EXPANDED-SPAWN ;

: REBT-FILE$ ( ptr u8 n -- ptr u8 n )
   REBT-FILE REBT-CAP READ-ALL REBT-FILE-U !
   REBT-FILE REBT-FILE-U @ ;

: REBT-CONTAINS ( ptr u8 n ptr u8 n -- )
   CONTAINS? TTRUE ;

: REBT-NOT-CONTAINS ( ptr u8 n ptr u8 n -- )
   CONTAINS? 0= TTRUE ;

: REBT-RUN-MISSING-FORTH-ROW ( -- )
   REBT-WRITE-BAD-MODELS
   s" 1" s" run-expanded-missing-row-2026-06-22" REBT-RUN-EXPANDED-START
   s" BENCH_FORTH_MODES" >LEN s" repair" >LEN PROC-ENV+
   REBT-RUN-EXPANDED-CAPTURE {: outu erru rc :}
   rc 0= 0= TTRUE
   outu drop
   REBT-ERR erru s" missing forth result row" REBT-CONTAINS
   REBT-ERR erru s" task_id=1" REBT-CONTAINS
   REBT-ERR erru s" model_id=aotfix" REBT-CONTAINS
   REBT-ERR erru s" arm=habu-forth" REBT-CONTAINS
   REBT-ERR erru s" trial=1" REBT-CONTAINS
   REBT-ERR erru s" child_rc=" REBT-CONTAINS ;

: REBT-ARM-REPORT$ ( ptr u8 n -- ptr u8 n ) {: arm:ptr armu :}
   SB-RESET
   s" arm " SB-APPEND
   arm armu SB-APPEND
   REBT-SPACE SB-APPEND-C
   s" rows=1" SB-APPEND
   SB$ ;

: REBT-ASSERT-AOT-JSONL ( -- )
   REBT-OUT-PATH$ REBT-FILE$ {: a:ptr u :}
   a u s" outcome" REBT-CONTAINS
   a u s" reject" REBT-CONTAINS
   a u s" arm" REBT-CONTAINS
   a u s" habu-aot" REBT-CONTAINS
   a u s" aot_unsupported" REBT-CONTAINS
   a u s" repair_class_stats" REBT-CONTAINS
   a u s" E-AOT-UNSUPPORTED" REBT-CONTAINS
   a u s" here" REBT-CONTAINS ;

: REBT-ASSERT-AOT-REPORT ( -- )
   REBT-REPORT$ REBT-FILE$ {: a:ptr u :}
   a u s" category aot-unsupported rows=1" REBT-CONTAINS
   a u s" arm habu-aot rows=1" REBT-CONTAINS ;

: REBT-ASSERT-ARRAY-JSONL ( -- )
   REBT-OUT-PATH$ REBT-FILE$ {: a:ptr u :}
   a u s" outcome" REBT-CONTAINS
   a u s" pass" REBT-CONTAINS
   a u s" first_pass_checker" REBT-CONTAINS
   a u s" certified" REBT-CONTAINS
   a u s" arm" REBT-CONTAINS
   a u s" habu-stdlib" REBT-CONTAINS
   a u s" task_id" REBT-CONTAINS ;

: REBT-ASSERT-ARRAY-REPORT ( -- )
   REBT-REPORT$ REBT-FILE$ {: a:ptr u :}
   a u s" category arrays rows=1" REBT-CONTAINS
   a u s" arm habu-stdlib rows=1" REBT-CONTAINS ;

: REBT-ASSERT-FOREIGN-ARRAY-JSONL ( ptr u8 n -- ) {: arm:ptr armu :}
   REBT-OUT-PATH$ REBT-FILE$ {: a:ptr u :}
   a u s" outcome" REBT-CONTAINS
   a u s" pass" REBT-CONTAINS
   a u s" first_pass_checker" REBT-CONTAINS
   a u s" certified" REBT-CONTAINS
   a u s" runtime_status" REBT-CONTAINS
   a u s" task_id" REBT-CONTAINS
   a u arm armu REBT-CONTAINS
   a u s" .sh" REBT-NOT-CONTAINS ;

: REBT-ASSERT-FOREIGN-ARRAY-REPORT ( ptr u8 n -- ) {: arm:ptr armu :}
   REBT-REPORT$ REBT-FILE$ {: a:ptr u :}
   a u s" category arrays rows=1" REBT-CONTAINS
   a u arm armu REBT-ARM-REPORT$ REBT-CONTAINS ;

: REBT-ASSERT-FORTH-JSONL ( -- )
   REBT-OUT-PATH$ REBT-FILE$ {: a:ptr u :}
   a u s" outcome" REBT-CONTAINS
   a u s" pass" REBT-CONTAINS
   a u s" habu-forth" REBT-CONTAINS
   a u s" habu-forth-raw" REBT-CONTAINS
   a u s" habu-forth-blind" REBT-CONTAINS
   a u REBT-FORTH-SHELL$ CONTAINS? 0= TTRUE ;

: REBT-ASSERT-FORTH-REPORT ( -- )
   REBT-REPORT$ REBT-FILE$ {: a:ptr u :}
   a u s" arm habu-forth rows=1" REBT-CONTAINS
   a u s" arm habu-forth-raw rows=1" REBT-CONTAINS
   a u s" arm habu-forth-blind rows=1" REBT-CONTAINS ;

: REBT-ASSERT-RUNNER-NO-SHELL ( -- )
   s" bench/llm/run-expanded-bench.f" REBT-FILE$ {: a:ptr u :}
   a u REBT-FORTH-SHELL$ REBT-NOT-CONTAINS
   a u s" drive-js.sh" REBT-NOT-CONTAINS
   a u s" drive-python.sh" REBT-NOT-CONTAINS
   a u s" drive-rust.sh" REBT-NOT-CONTAINS
   a u s" drive-ts.sh" REBT-NOT-CONTAINS
   a u s" RB-RUN-PREPARE" REBT-NOT-CONTAINS
   a u s" RB-RUN-APPEND" REBT-NOT-CONTAINS ;

: REBT-RUN-AOT-CASE ( -- )
   REBT-AOT-CANDIDATE$ REBT-WRITE-MODEL
   REBT-BUILD-MODEL
   REBT-WRITE-MODELS
   REBT-RUN-AOT-EXPANDED
   REBT-ASSERT-AOT-JSONL
   REBT-ASSERT-AOT-REPORT ;

: REBT-RUN-ARRAY-CASE ( -- )
   REBT-ARRAY-CANDIDATE$ REBT-WRITE-MODEL
   REBT-BUILD-MODEL
   REBT-WRITE-MODELS
   REBT-RUN-ARRAY-EXPANDED
   REBT-ASSERT-ARRAY-JSONL
   REBT-ASSERT-ARRAY-REPORT ;

: REBT-RUN-FOREIGN-ARRAY-CASE ( ptr u8 n ptr u8 n ptr u8 n -- ) {: candidate:ptr candidateu arm:ptr armu seed:ptr seedu :}
   candidate candidateu REBT-WRITE-MODEL
   REBT-BUILD-MODEL
   REBT-WRITE-MODELS
   arm armu seed seedu REBT-RUN-FOREIGN-ARRAY-EXPANDED
   arm armu REBT-ASSERT-FOREIGN-ARRAY-JSONL
   arm armu REBT-ASSERT-FOREIGN-ARRAY-REPORT ;

: REBT-RUN-FOREIGN-ARRAY-CASES ( -- )
   REBT-JS-CANDIDATE$ s" js" s" run-expanded-array-js-2026-06-21" REBT-RUN-FOREIGN-ARRAY-CASE
   REBT-PY-CANDIDATE$ s" python" s" run-expanded-array-python-2026-06-21" REBT-RUN-FOREIGN-ARRAY-CASE
   REBT-RUST-CANDIDATE$ s" rust" s" run-expanded-array-rust-2026-06-21" REBT-RUN-FOREIGN-ARRAY-CASE
   REBT-TS-CANDIDATE$ s" ts" s" run-expanded-array-ts-2026-06-21" REBT-RUN-FOREIGN-ARRAY-CASE ;

: REBT-RUN-FORTH-CASE ( -- )
   REBT-FORTH-CANDIDATE$ REBT-WRITE-MODEL
   REBT-BUILD-MODEL
   REBT-WRITE-MODELS
   REBT-RUN-FORTH-EXPANDED
   REBT-ASSERT-FORTH-JSONL
   REBT-ASSERT-FORTH-REPORT
   REBT-ASSERT-RUNNER-NO-SHELL ;

: REBT-MAIN ( -- )
   T-RESET
   REBT-PREPARE
   REBT-RUN-MISSING-FORTH-ROW
   REBT-RUN-AOT-CASE
   REBT-RUN-FORTH-CASE
   REBT-RUN-ARRAY-CASE
   REBT-RUN-FOREIGN-ARRAY-CASES
   CLEANUP-RUN
   T-REPORT
   s" run-expanded-bench-test: ok" type cr ;

REBT-MAIN
