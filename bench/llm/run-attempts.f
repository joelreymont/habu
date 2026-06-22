\ run-attempts.f - checked CLI for schema-1 attempt rows.
\
\ Load after lib/errors.f, lib/string.f, lib/memory.f, lib/json-write.f, tools/json.f,
\ lib/fs.f, lib/fs-mutate.f, lib/process.f, lib/process-argv.f,
\ lib/process-env.f, lib/time.f, lib/date.f, bench/llm/manifest.f,
\ tools/lint/text.f, tools/lint/lib.f, lib/vector.f, tools/lint/source-lex.f,
\ tools/argv.f,
\ bench/llm/forth-task-lines-lib.f, bench/llm/attempt-solutions-lib.f,
\ bench/llm/diagnostic-stats.f, and bench/llm/run-attempts-lib.f.

64 constant RUNA-USAGE-RC
66 constant RUNA-DATAERR-RC
1 constant RUNA-VALIDATOR-RC
120000 constant RUNA-VALIDATE-TIMEOUT-MS
$20000 constant RUNA-VALIDATE-CAP
128 constant RUNA-TEXT-CAP
47 constant RUNA-SLASH

create RUNA-CAND-BUF FS-PATH-CAP allot
create RUNA-OUT-BUF FS-PATH-CAP allot
create RUNA-TMP-BUF FS-PATH-CAP allot
create RUNA-REF-BUF FS-PATH-CAP allot
create RUNA-RUN-BUF RUNA-TEXT-CAP allot
create RUNA-MODEL-BUF RUNA-TEXT-CAP allot
create RUNA-DATE-BUF DATE-LEN allot
create RUNA-LF-BUF 1 allot
create RUNA-VALIDATE-OUT RUNA-VALIDATE-CAP allot
create RUNA-VALIDATE-ERR RUNA-VALIDATE-CAP allot

variable RUNA-CAND-U
variable RUNA-OUT-U
variable RUNA-TMP-U
variable RUNA-REF-U
variable RUNA-RUN-U
variable RUNA-MODEL-U
variable RUNA-LAST

STR-LF RUNA-LF-BUF c!

: RUNA-COPY! ( ptr u8 n ptr u8 ptr n n -- ) {: a:ptr u dst:ptr up:ptr cap :}
   u 0 < if E-FS-PATH throw then
   u cap > if E-FS-CAPACITY throw then
   a dst u BYTE-COPY
   u up ! ;

: RUNA-PATH! ( ptr u8 n ptr u8 ptr n -- )
   FS-PATH-CAP RUNA-COPY! ;

: RUNA-TEXT! ( ptr u8 n ptr u8 ptr n -- )
   RUNA-TEXT-CAP RUNA-COPY! ;

: RUNA-CAND$ ( -- ptr u8 n )
   RUNA-CAND-BUF RUNA-CAND-U @ ;

: RUNA-OUT$ ( -- ptr u8 n )
   RUNA-OUT-BUF RUNA-OUT-U @ ;

: RUNA-TMP$ ( -- ptr u8 n )
   RUNA-TMP-BUF RUNA-TMP-U @ ;

: RUNA-REF$ ( -- ptr u8 n )
   RUNA-REF-BUF RUNA-REF-U @ ;

: RUNA-TASK-LINES$ ( -- ptr u8 n )
   FTL$ ;

: RUNA-RUN$ ( -- ptr u8 n )
   RUNA-RUN-BUF RUNA-RUN-U @ ;

: RUNA-MODEL$ ( -- ptr u8 n )
   RUNA-MODEL-BUF RUNA-MODEL-U @ ;

: RUNA-DEFAULT-RUN$ ( -- ptr u8 n )
   SB-RESET
   s" attempt-" SB-APPEND
   TIME-EPOCH-SECONDS DATE-SECONDS-DAY / RUNA-DATE-BUF DATE-LEN FORMAT-YMD SB-APPEND
   SB$ ;

: RUNA-OUT-ARG$ ( -- ptr u8 n )
   ARGV-POS# 1 > if 1 ARGV-POS$ exit then
   s" bench/llm/results/attempt.jsonl" ;

: RUNA-RUN-ARG$ ( -- ptr u8 n )
   ARGV-POS# 2 > if 2 ARGV-POS$ exit then
   RUNA-DEFAULT-RUN$ ;

: RUNA-MODEL-ARG$ ( -- ptr u8 n )
   ARGV-POS# 3 > if 3 ARGV-POS$ exit then
   s" candidate-dir" ;

: RUNA-USAGE ( -- )
   s" bench/llm/run-attempts.f CANDIDATE_DIR [out.jsonl] [run_id] [model]" ARGV-USAGE! ;

: RUNA-CONFIG ( -- )
   RUNA-USAGE
   ARGV-PARSE
   1 4 ARGV-EXPECT-POS
   0 ARGV-POS$ RUNA-CAND-BUF RUNA-CAND-U RUNA-PATH!
   RUNA-OUT-ARG$ RUNA-OUT-BUF RUNA-OUT-U RUNA-PATH!
   RUNA-RUN-ARG$ RUNA-RUN-BUF RUNA-RUN-U RUNA-TEXT!
   RUNA-MODEL-ARG$ RUNA-MODEL-BUF RUNA-MODEL-U RUNA-TEXT! ;

: RUNA-REQUIRE-CANDIDATES ( -- )
   RUNA-CAND$ DIR? 0= if s" run-attempts: no such candidate dir" RUNA-DATAERR-RC die then ;

: RUNA-JOIN! ( ptr u8 n ptr u8 ptr n -- ) {: name:ptr nameu dst:ptr up:ptr :}
   RUNA-TMP$ name nameu dst JOIN-PATH up ! ;

: RUNA-PREPARE-TEMP ( -- )
   CLEANUP-RESET
   s" habu-attempts" TMPDIR-MKDIR RUNA-TMP-BUF RUNA-TMP-U RUNA-PATH!
   RUNA-TMP$ CLEANUP-TREE+
   s" ref" RUNA-REF-BUF RUNA-REF-U RUNA-JOIN! ;

: RUNA-MATERIALIZE ( -- )
   s" bench/llm/tasks.tsv" FTL-FILE$ 2drop
   s" bench/llm/tasks.tsv" s" bench/llm/solutions.f" RUNA-REF$ AS-EXTRACT-FILES ;

: RUNA-LAST-SLASH ( ptr u8 n -- n ) {: a:ptr u :}
   -1 RUNA-LAST !
   0 begin dup u < while
      dup a + c@ RUNA-SLASH = if dup RUNA-LAST ! then
      1+
   repeat drop
   RUNA-LAST @ ;

: RUNA-ENSURE-PARENT ( ptr u8 n -- ) {: a:ptr u :}
   a u RUNA-LAST-SLASH {: idx :}
   idx 0 < if exit then
   idx 0= if a 1 MAKE-DIRS exit then
   a idx MAKE-DIRS ;

: RUNA-WRITE-RESULTS ( ptr u8 n -- ) {: data:ptr datau :}
   RUNA-OUT$ 2dup RUNA-ENSURE-PARENT
   data datau WRITE-ALL ;

: RUNA-WRITE-FD ( n ptr u8 n -- ) {: fd a:ptr u :}
   u 0= if exit then
   fd a u write u <> if E-FS-IO throw then ;

: RUNA-VALIDATE-ARGS ( -- )
   PROC-ARGV-ENV-RESET
   PROC-ENV-INHERIT-MISSING
   s" --load"  >LEN PROC-ARGV+
   s" lib/errors.f"  >LEN PROC-ARGV+
   s" lib/memory.f"  >LEN PROC-ARGV+
   s" tools/date.f"  >LEN PROC-ARGV+
   s" tools/lint/text.f"  >LEN PROC-ARGV+ s" tools/lint/lib.f"  >LEN PROC-ARGV+
   s" tools/json.f"  >LEN PROC-ARGV+
   s" tools/argv.f"  >LEN PROC-ARGV+
   s" bench/llm/validate-results.f"  >LEN PROC-ARGV+
   s" --"  >LEN PROC-ARGV+
   RUNA-OUT$  >LEN PROC-ARGV+ ;

: RUNA-VALIDATE ( -- )
   RUNA-VALIDATE-ARGS
   s" bin/hb" >LEN RUNA-VALIDATE-OUT RUNA-VALIDATE-CAP >LEN
   RUNA-VALIDATE-ERR RUNA-VALIDATE-CAP >LEN RUNA-VALIDATE-TIMEOUT-MS >MS
   RUN-ARGV-ENV-CAPTURE {: outu erru rc :}
   2 RUNA-VALIDATE-OUT outu LEN>N RUNA-WRITE-FD
   2 RUNA-VALIDATE-ERR erru LEN>N RUNA-WRITE-FD
   rc RC>N 0 <> if s" run-attempts: validator failed" RUNA-VALIDATOR-RC die then ;

: RUNA-WROTE. ( -- )
   2 s" run-attempts: wrote " RUNA-WRITE-FD
   2 RUNA-OUT$ RUNA-WRITE-FD
   2 RUNA-LF-BUF 1 RUNA-WRITE-FD ;

: RUNA-MAIN ( -- )
   RUNA-CONFIG
   RUNA-REQUIRE-CANDIDATES
   RUNA-PREPARE-TEMP
   RUNA-MATERIALIZE
   RUNA-TASK-LINES$ RUNA-REF$ RUNA-CAND$ s" bench/llm/tests.f" RUNA-RUN$ RUNA-MODEL$ RA-RUN-TASKS
   RUNA-WRITE-RESULTS
   RUNA-VALIDATE
   RUNA-WROTE.
   CLEANUP-RUN ;

: RUNA-AUTO ( -- )
   SCRIPT-ARGC 0 > if RUNA-MAIN then ;

RUNA-AUTO
