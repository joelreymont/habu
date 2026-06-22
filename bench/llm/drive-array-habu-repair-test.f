\ drive-array-habu-repair-test.f - live repair-loop test for native array driver.
\
\ Load after lib/errors.f, lib/string.f, lib/test.f, lib/fs.f,
\ lib/fs-mutate.f, lib/process.f, lib/process-argv.f, lib/process-env.f,
\ lib/source.f, lib/build.f, tools/build-fixpoint.f, tools/hb-build-lib.f,
\ lib/json-write.f, and bench/llm/fixture-text.f.

120000 constant DART-TIMEOUT-MS
$10000 constant DART-CAP

create DART-ROOT FS-PATH-CAP allot
create DART-HB-TMP FS-PATH-CAP allot
create DART-MODEL-SRC FS-PATH-CAP allot
create DART-MODEL-BIN FS-PATH-CAP allot
create DART-MODELS FS-PATH-CAP allot
create DART-OUT DART-CAP allot
create DART-ERR DART-CAP allot
create DART-FILE DART-CAP allot

variable DART-ROOT-U
variable DART-HB-TMP-U
variable DART-MODEL-SRC-U
variable DART-MODEL-BIN-U
variable DART-MODELS-U
variable DART-OUT-U
variable DART-ERR-U
variable DART-FILE-U
variable DART-RC

: DART-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u dst:ptr up:ptr :}
   u FS-PATH-CAP > if E-FS-CAPACITY throw then
   a dst u BYTE-COPY
   u up ! ;

: DART-ROOT$ ( -- ptr u8 n )
   DART-ROOT DART-ROOT-U @ ;

: DART-HB-TMP$ ( -- ptr u8 n )
   DART-HB-TMP DART-HB-TMP-U @ ;

: DART-MODEL-SRC$ ( -- ptr u8 n )
   DART-MODEL-SRC DART-MODEL-SRC-U @ ;

: DART-MODEL-BIN$ ( -- ptr u8 n )
   DART-MODEL-BIN DART-MODEL-BIN-U @ ;

: DART-MODELS$ ( -- ptr u8 n )
   DART-MODELS DART-MODELS-U @ ;

: DART-JOIN! ( ptr u8 n ptr u8 ptr n -- ) {: name:ptr nameu dst:ptr up:ptr :}
   DART-ROOT$ name nameu dst JOIN-PATH up ! ;

: DART-CAPTURE! ( len len rc -- ) {: outu erru rc :}
   rc RC>N DART-RC !
   erru LEN>N DART-ERR-U !
   outu LEN>N DART-OUT-U ! ;

: DART-DUMP-RUN ( ptr u8 n -- )
   s" FAIL: " type type cr
   s" rc: " type DART-RC @ . cr
   DART-OUT DART-OUT-U @ type
   DART-ERR DART-ERR-U @ type ;

: DART-EXPECT-OK ( ptr u8 n -- )
   DART-RC @ 0 <> if
      DART-DUMP-RUN
      s" drive-array repair subprocess failed" T-EX-FAIL die
   else
      2drop
   then ;

: DART-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-array-repair-test" TMPDIR-MKDIR DART-ROOT DART-ROOT-U DART-COPY!
   DART-ROOT$ CLEANUP-TREE+
   s" hbtmp" DART-HB-TMP DART-HB-TMP-U DART-JOIN!
   s" model.f" DART-MODEL-SRC DART-MODEL-SRC-U DART-JOIN!
   s" model-bin" DART-MODEL-BIN DART-MODEL-BIN-U DART-JOIN!
   s" models.tsv" DART-MODELS DART-MODELS-U DART-JOIN!
   DART-HB-TMP$ MAKE-DIR ;

: DART-APPEND-FILE ( ptr u8 n -- ) {: path:ptr pathu :}
   path pathu DART-FILE DART-CAP READ-ALL DART-FILE-U !
   DART-MODEL-SRC$ DART-FILE DART-FILE-U @ APPEND-FILE ;

: DART-MODEL-MAIN$ ( -- ptr u8 n )
   BFT-RESET
   s" : BAD ( -- ) " BFT+
   s" : ARR-SUM ( ptr a n -- i64 ) >LEN A-SUM dup ;" BFT-SOURCE-S"
   s"  type cr ;" BFT+ BFT-LF+
   s" : GOOD ( -- ) " BFT+
   s" : ARR-SUM ( ptr a n -- i64 ) >LEN A-SUM ;" BFT-SOURCE-S"
   s"  type cr ;" BFT+ BFT-LF+
   s" : MAIN ( -- ) 0 SCRIPT-ARGV$ " BFT+
   s" habu_repair_packet" BFT-SOURCE-S"
   s"  CONTAINS? if GOOD else BAD then ;" BFT+ BFT-LF+
   s" : RUN ( -- ) SCRIPT-ARGC 0 > if MAIN then ;" BFT+ BFT-LF+
   s" RUN" BFT+ BFT-LF+
   BFT$ ;

: DART-WRITE-MODEL ( -- )
   DART-MODEL-SRC$ s" " WRITE-ALL
   s" lib/errors.f" DART-APPEND-FILE
   s" lib/string.f" DART-APPEND-FILE
   s" lib/fs.f" DART-APPEND-FILE
   DART-MODEL-SRC$ DART-MODEL-MAIN$ APPEND-FILE ;

: DART-HB-BUILD-LOADS ( -- )
   s" --load"  >LEN PROC-ARGV+
   s" lib/errors.f"  >LEN PROC-ARGV+
   s" lib/string.f"  >LEN PROC-ARGV+
   s" lib/memory.f"  >LEN PROC-ARGV+
   s" lib/fs.f"  >LEN PROC-ARGV+
   s" lib/fs-mutate.f"  >LEN PROC-ARGV+
   s" lib/process.f"  >LEN PROC-ARGV+
   s" lib/process-argv.f"  >LEN PROC-ARGV+
   s" lib/process-env.f"  >LEN PROC-ARGV+
   s" lib/source.f"  >LEN PROC-ARGV+
   s" lib/build.f"  >LEN PROC-ARGV+
   s" tools/build-fixpoint.f"  >LEN PROC-ARGV+
   s" tools/hb-build-lib.f"  >LEN PROC-ARGV+
   s" tools/hb-build.f"  >LEN PROC-ARGV+ ;

: DART-BUILD-MODEL ( -- )
   PROC-ARGV-ENV-RESET
   s" HB_TMP" >LEN DART-HB-TMP$ >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   DART-HB-BUILD-LOADS
   s" --"  >LEN PROC-ARGV+
   s" --repl"  >LEN PROC-ARGV+
   s" --strict-signatures"  >LEN PROC-ARGV+
   DART-MODEL-SRC$  >LEN PROC-ARGV+
   s" -o"  >LEN PROC-ARGV+
   DART-MODEL-BIN$  >LEN PROC-ARGV+
   s" bin/hb" >LEN DART-OUT DART-CAP >LEN DART-ERR DART-CAP >LEN
   DART-TIMEOUT-MS >MS RUN-ARGV-ENV-CAPTURE
   DART-CAPTURE!
   s" build fixture model" DART-EXPECT-OK
   DART-MODEL-BIN$ FILE? TTRUE ;

: DART-TAB ( -- )
   BFT-TAB+ ;

: DART-WRITE-MODELS ( -- )
   BFT-RESET
   s" id" BFT+ DART-TAB s" label" BFT+ DART-TAB s" command" BFT+ DART-TAB
   s" args" BFT+ DART-TAB s" parser" BFT+ DART-TAB s" token_fields" BFT+ DART-TAB
   s" timeout_s" BFT+ BFT-LF+
   s" fix" BFT+ DART-TAB s" HabuFix" BFT+ DART-TAB DART-MODEL-BIN$ BFT+ DART-TAB
   s" {prompt}" BFT+ DART-TAB s" raw" BFT+ DART-TAB DART-TAB s" 10" BFT+ BFT-LF+
   DART-MODELS$ BFT$ WRITE-ALL ;

: DART-ARRAY-LOADS ( -- )
   s" --load"  >LEN PROC-ARGV+
   s" lib/errors.f"  >LEN PROC-ARGV+
   s" lib/string.f"  >LEN PROC-ARGV+
   s" lib/memory.f"  >LEN PROC-ARGV+
   s" lib/fs.f"  >LEN PROC-ARGV+
   s" lib/fs-mutate.f"  >LEN PROC-ARGV+
   s" lib/process.f"  >LEN PROC-ARGV+
   s" lib/process-argv.f"  >LEN PROC-ARGV+
   s" lib/process-env.f"  >LEN PROC-ARGV+
   s" tools/argv.f"  >LEN PROC-ARGV+
   s" tools/json.f"  >LEN PROC-ARGV+
   s" bench/llm/manifest.f"  >LEN PROC-ARGV+
   s" bench/llm/model.f"  >LEN PROC-ARGV+
   s" bench/llm/parse-resp-lib.f"  >LEN PROC-ARGV+
   s" bench/llm/codex-home.f"  >LEN PROC-ARGV+
   s" bench/llm/model-run.f"  >LEN PROC-ARGV+
   s" bench/llm/vectors.f"  >LEN PROC-ARGV+
   s" lib/json-write.f"  >LEN PROC-ARGV+
   s" src/core/sha256.f"  >LEN PROC-ARGV+
   s" bench/llm/live-row.f"  >LEN PROC-ARGV+
   s" bench/llm/drive-stdlib-lib.f"  >LEN PROC-ARGV+
   s" bench/llm/driver-token-helpers.f"  >LEN PROC-ARGV+
   s" bench/llm/drive-array-habu-lib.f"  >LEN PROC-ARGV+
   s" bench/llm/drive-array-habu.f"  >LEN PROC-ARGV+ ;

: DART-RUN-ARRAY ( -- )
   PROC-ARGV-ENV-RESET
   s" MODEL_REGISTRY" >LEN DART-MODELS$ >LEN PROC-ENV+
   s" MODEL_ID" >LEN s" fix" >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   DART-ARRAY-LOADS
   s" --"  >LEN PROC-ARGV+
   s" 46"  >LEN PROC-ARGV+
   s" ARR-SUM"  >LEN PROC-ARGV+
   s" ptr a n -- i64"  >LEN PROC-ARGV+
   s" Sum the array."  >LEN PROC-ARGV+
   s" as"  >LEN PROC-ARGV+
   s" [3 1 4] -> 8; [5] -> 5; [-2 -3] -> -5"  >LEN PROC-ARGV+
   s" lib"  >LEN PROC-ARGV+
   s" 2"  >LEN PROC-ARGV+
   s" bin/hb" >LEN DART-OUT DART-CAP >LEN DART-ERR DART-CAP >LEN
   DART-TIMEOUT-MS >MS RUN-ARGV-ENV-CAPTURE
   DART-CAPTURE!
   s" run array driver" DART-EXPECT-OK
   DART-ERR DART-ERR-U @ s" " T$= ;

: DART-ROW-HAS ( ptr u8 n -- ) {: needle:ptr needleu :}
   DART-OUT DART-OUT-U @ needle needleu CONTAINS? dup 0= if
      s" missing row needle: " type needle needleu type cr
      s" row: " type DART-OUT DART-OUT-U @ type cr
   then TTRUE ;

: DART-ROW-HAS-JSON ( -- )
   JW$ DART-ROW-HAS ;

: DART-ROW-S ( ptr u8 n ptr u8 n -- )
   JW-RESET JW-FIELD-S DART-ROW-HAS-JSON ;

: DART-ROW-U ( ptr u8 n n -- )
   JW-RESET JW-FIELD-U DART-ROW-HAS-JSON ;

: DART-ROW-B ( ptr u8 n bool -- )
   JW-RESET JW-FIELD-BOOL DART-ROW-HAS-JSON ;

: DART-FALSE ( -- bool )
   0 0= 0= ;

: DART-ASSERT-ROW ( -- )
   s" outcome" s" pass" DART-ROW-S
   s" rounds" 2 DART-ROW-U
   s" attempt" 2 DART-ROW-U
   s" first_pass" DART-FALSE DART-ROW-B
   s" first_pass_checker" s" rejected" DART-ROW-S
   s" tests_passed" 0 0= DART-ROW-B
   s" repair_iterations" 1 DART-ROW-U
   s" checker_iterations" 2 DART-ROW-U
   s" diagnostic_count" 1 DART-ROW-U
   s" runtime_status" s" ok" DART-ROW-S
   s" habu_repair_packet" DART-ROW-HAS
   s" remove_producer" DART-ROW-HAS ;

: DART-MAIN ( -- )
   T-RESET
   DART-PREPARE
   DART-WRITE-MODEL
   DART-BUILD-MODEL
   DART-WRITE-MODELS
   DART-RUN-ARRAY
   DART-ASSERT-ROW
   CLEANUP-RUN
   T-REPORT
   s" drive-array-habu-repair-test: ok" type cr ;

DART-MAIN
